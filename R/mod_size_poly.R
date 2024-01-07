#' size_poly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_size_poly_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    fluidRow(
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = "This will be updated",
                                    selected = "This will be updated"),
                 hr()
               ),
               actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
      ),
      column(width = 6,
             box(width = NULL, solidHeader = T,
                 fluidPage(
                   checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                      choices = "This will be updated",
                                      selected = "This will be updated"),
                   hr()
                 ),
                 fluidPage(
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = "This will be updated",
                                      selected = "This will be updated")
                 )
             )
      ),
      column(width = 12,
             box(title = "Number of markers after filters",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("filters_poly_out")),
             )
      ),
      column(width = 12,
             box(title = "Map size",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("size_poly_out"), height = 700),
                 p("* Dashed red line correspond to 100 cM"),
             )
      )
    )
  )
}

#' size_poly Server Functions
#' 
#' @importFrom ggrepel geom_text_repel
#'
#' @noRd 
mod_size_poly_server <- function(input, output, session, datas_poly_emp){
  ns <- session$ns
  
  observe({
    
    file_names <- strsplit(names(datas_poly_emp()[[3]]), "_")
    SNPCall_choice <- as.list(unique(sapply(file_names, "[[", 1)))
    names(SNPCall_choice) <- unique(sapply(file_names, "[[", 1))
    
    ErrorProb_choice <- as.list(unique(sapply(file_names, "[[",2)))
    names(ErrorProb_choice) <- unique(sapply(file_names, "[[",2))
    
    CountsFrom_choice <- as.list(unique(sapply(file_names, "[[",3)))
    names(CountsFrom_choice) <- unique(sapply(file_names, "[[",3))
    
    updateCheckboxGroupInput(session, "SNPCall",
                             label="SNP call method",
                             choices = SNPCall_choice,
                             selected=unlist(SNPCall_choice)[1])
    
    updateCheckboxGroupInput(session, "ErrorProb",
                             label="Genotyping method",
                             choices = ErrorProb_choice,
                             selected=unlist(ErrorProb_choice)[1])
    
    updateCheckboxGroupInput(session, "CountsFrom",
                             label="Counts From",
                             choices = CountsFrom_choice,
                             selected=unlist(CountsFrom_choice)[1])
  })
  
  button <- eventReactive(input$go, {
    
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      file_names <- strsplit(names(datas_poly_emp()[[1]]), "_")
      SNPCall_choice <- unique(sapply(file_names, "[[", 1))
      ErrorProb_choice <- unique(sapply(file_names, "[[",2))
      CountsFrom_choice <- unique(sapply(file_names, "[[",3))
      
      SNPCall_choice <- SNPCall_choice[which(SNPCall_choice %in% input$SNPCall)]
      ErrorProb_choice <- ErrorProb_choice[which(ErrorProb_choice %in% input$ErrorProb)]
      CountsFrom_choice <- CountsFrom_choice[which(CountsFrom_choice %in% input$CountsFrom)]
      
      choices <- apply(expand.grid(SNPCall_choice, ErrorProb_choice), 1, paste, collapse="_")
      choices <- apply(expand.grid(choices, CountsFrom_choice), 1, paste, collapse="_")
      
      idx <- which(names(datas_poly_emp()$summaries) %in% choices)
      idx2 <- which(names(datas_poly_emp()$info) %in% choices)
            
      summaries <- datas_poly_emp()$summaries[idx]
      summaries <- do.call(rbind, summaries)
      info <- datas_poly_emp()$info[idx2]
      info <- do.call(rbind, info)
      info$step <- factor(info$step, levels = c("raw", "miss filtered", 
                                                "segr filtered", "p1", "p2"))
      
      summaries$`Map length (cM)` <- as.numeric(summaries$`Map length (cM)`)
      summaries$Total <- as.numeric(summaries$Total)
      summaries_long <- summaries %>% select(`Map length (cM)`, Total, map, data) %>% pivot_longer(cols = 1:2)
      summaries_long$map1 <- sapply(strsplit(summaries_long$map, "[.]"), "[[", 1)
      summaries_long$map1 <- gsub("error", "global error 5%",summaries_long$map1)
      summaries_long$map1 <- gsub("prob", "geno call \n probabilities",summaries_long$map1)
      summaries_long$map2 <- sapply(strsplit(summaries_long$map, "[.]"), "[[", 2)
      summaries_long$map2 <- gsub("p1", "parent 1",summaries_long$map2)
      summaries_long$map2 <- gsub("p2", "parent 2",summaries_long$map2)
      
      incProgress(0.5, detail = paste("Doing part", 2))
      
      list(info, summaries_long)
    })
  })
  
  output$filters_poly_out <- renderPlot({
    button()[[1]] %>% ggplot(aes(x=step, y = n.markers, color = dat, group=dat, label = n.markers)) + 
      geom_point() + geom_line() + geom_text_repel() + theme_bw() +
      labs(x = "", y = "number of markers", color = "dataset") + theme(text = element_text(size = 15))
  })
  
  output$size_poly_out <- renderPlot({
    p1 <- button()[[2]] %>% filter(name != "Total") %>% ggplot(aes(x=value, fill = data, color = data)) + 
      geom_density(alpha=0.1) + facet_grid(map1 + map2~.) + theme_bw() + ggtitle("Map size (cM)") +
      labs(x = "map size (cM)") +
      theme(text = element_text(size = 15)) +
      geom_vline(xintercept = 100, linetype="dotted", 
                 color = "red", linewidth=1.5) 
    
    p2 <- button()[[2]] %>% filter(name == "Total") %>% ggplot(aes(x=value, fill = data, color = data)) + 
      geom_density(alpha=0.1) + facet_grid(map1 + map2~.) + theme_bw() + ggtitle("Number of Markers") + 
      labs(x = "Number of Markers") +
      theme(text = element_text(size = 15)) 
    
    ggarrange(p1, p2,common.legend = TRUE, legend = "bottom")
  })
}

## To be copied in the UI
# mod_size_poly_ui("size_poly_ui_1")

## To be copied in the server
# mod_size_poly_server("size_poly_ui_1")
