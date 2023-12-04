#' emp_ind_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_ind_size_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic show the distribuition of the difference between estimated and simulated distances between each pair markers of the generated maps.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Map size",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("ind_size_emp_out")),
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

#' emp_ind_size Server Functions
#'
#' @noRd 
mod_emp_ind_size_server <- function(input, output, session, datas_emp){
    ns <- session$ns
    
    observe({
      
      SNPCall_choice <- as.list(unique(datas_emp()[[2]]$SNPCall))
      names(SNPCall_choice) <- unique(datas_emp()[[2]]$SNPCall)
      
      ErrorProb_choice <- as.list(unique(datas_emp()[[2]]$GenoCall))
      names(ErrorProb_choice) <- gsub("default", "_OneMap2.0", unique(datas_emp()[[2]]$GenoCall))
      CountsFrom_choice <- as.list(unique(datas_emp()[[2]]$CountsFrom))
      names(CountsFrom_choice) <- unique(datas_emp()[[2]]$CountsFrom)
      
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
        geno <- input$ErrorProb
    
        data <- datas_emp()[[2]] %>% 
          filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          group_by(CountsFrom, SNPCall, GenoCall) %>%
          mutate(interv.diff = sqrt(c(0,rf[-1] - rf[-length(rf)])^2))
        
        data_df <- data %>% group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise(tot_size = round(rf[length(rf)],3),
                    n = n())
        
        data <- data %>% mutate(interv.diff = sqrt(c(0,rf[-1] - rf[-length(rf)])^2))
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise(n = n())
        
        data<- merge(data, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -mks, -pos, -type, -phases, - CountsFrom, -rf)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        
        n <- c(`n markers` = "n", `Distance between markers (cM)` = "interv.diff")
        data$key <- names(n)[match(data$key, n)]
        
        list(data, data_df)
      })
    })
    
    output$ind_size_emp_out <- renderPlot({
      ind_size_graph_emp(button()[[1]])
    })
}

## To be copied in the UI
# mod_emp_ind_size_ui("emp_ind_size_ui_1")

## To be copied in the server
# mod_emp_ind_size_server("emp_ind_size_ui_1")
