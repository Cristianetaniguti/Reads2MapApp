#' emp_times UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_times_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic show the distribuition of times and number of markers of all families by depth.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Time spent to estimate genetic distances",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("times_emp_out")),
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
               ),
               fluidPage(
                 checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                    choices = "This will be updated",
                                    selected = "This will be updated"),
               )
             )
      ),
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
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

#' emp_times Server Functions
#'
#' @noRd 
mod_emp_times_server <-  function(input, output, session, datas_emp){
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
      
      data_n <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom %in% input$CountsFrom) %>%
        group_by(GenoCall, SNPCall, CountsFrom) %>%
        summarise(n = n())
      
      data <- datas_emp()[[4]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom %in% input$CountsFrom)
      
      data_df<- merge(data, data_n)
      
      data <- data_df %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom)
      
      data$key <- gsub("n", "number of markers", data$key)
      data$key <- gsub("time", "time (seconds)", data$key)
      incProgress(0.5, detail = paste("Doing part", 2))
      list(data, data_df)
    })
  })
  
  output$times_emp_out <- renderPlot({
    times_graph_emp(button()[[1]])
  })
  
  output$times_emp_df_out <- renderDataTable({
    button()[[2]]
  })
}

## To be copied in the UI
# mod_emp_times_ui("emp_times_ui_1")

## To be copied in the server
# mod_emp_times_server("emp_times_ui_1")
