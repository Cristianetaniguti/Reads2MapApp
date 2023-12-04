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
    "The graphic show the distribuition of the difference between estimated and simulated distances between each pair markers of the generated maps.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Map size",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("size_poly_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
      ),
      
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = "This will be updated",
                                    selected = "This will be updated"),
                 hr()
               )
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
      )
    )
  )
}

#' size_poly Server Functions
#'
#' @noRd 
mod_size_poly_server <- function(input, output, session, datas_poly_emp){
  ns <- session$ns
  
  observe({
    
    file_names <- strsplit(names(datas_poly_emp()[[4]]), "_")
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
      
      file_names <- strsplit(names(datas_poly_emp()[[4]]), "_")
      SNPCall_choice <- unique(sapply(file_names, "[[", 1))
      ErrorProb_choice <- unique(sapply(file_names, "[[",2))
      CountsFrom_choice <- unique(sapply(file_names, "[[",3))
      
      SNPCall_choice <- SNPCall_choice %in% input$SNPCall
      ErrorProb_choice <- ErrorProb_choice %in% input$ErrorProb
      CountsFrom_choice <- CountsFrom_choice %in% input$CountsFrom
      
      idx <- which(SNPCall_choice & ErrorProb_choice & CountsFrom_choice)
      
      #seqs <- result_list$map[idx]      
      seqs <- datas_poly_emp()$map[idx]
      choosed_files <- file_names[idx]
      
      data <- data.frame()
      for(i in 1:length(seqs)){
        dist <- cumsum(datas_poly_emp()$map[[idx[i]]][[1]]$maps[[1]]$seq.rf)
        
        data_temp <- data.frame(SNPCall = choosed_files[[i]][1], 
                                GenoCall = choosed_files[[i]][2], 
                                CountsFrom = choosed_files[[i]][3],
                                rf = c(0,dist))  
        data <- rbind(data, data_temp)
      }
      
      data_df <- data %>% group_by(GenoCall, SNPCall, CountsFrom) %>%
        summarise(tot_size = round(rf[length(rf)],3),
                  n = n())
      
      data1 <- data %>% mutate(interv.diff = sqrt(c(0,rf[-1] - rf[-length(rf)])^2))
      
      data_n <- data1 %>%  group_by(GenoCall, SNPCall, CountsFrom) %>%
        summarise(n = n())
      
      data2 <- merge(data1, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, - CountsFrom, -rf)
      
      incProgress(0.5, detail = paste("Doing part", 2))
      
      n <- c(`n markers` = "n", `Distance between markers (cM)` = "interv.diff")
      data2$key <- names(n)[match(data2$key, n)]
      
      list(data2, data_df)
    })
  })
  
  output$size_poly_out <- renderPlot({
    ind_size_graph_emp(button()[[1]])
  })
  
  output$size_poly_df_out <- renderDataTable({
    button()[[2]]
  })
}

## To be copied in the UI
# mod_size_poly_ui("size_poly_ui_1")

## To be copied in the server
# mod_size_poly_server("size_poly_ui_1")
