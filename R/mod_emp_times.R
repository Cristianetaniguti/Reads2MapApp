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
                 radioButtons(ns("Global0.05"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
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

#' emp_times Server Functions
#'
#' @noRd 
mod_emp_times_server <-  function(input, output, session, datas_emp){
  ns <- session$ns
  
  observe({

    SNPCall_choice <- as.list(unique(datas_emp()[[2]]$SNPCall))
    names(SNPCall_choice) <- unique(datas_emp()[[2]]$SNPCall)
    methods <- unique(datas_emp()[[2]]$GenoCall)
    methods <- unique(gsub("0.05", "", methods))

    ErrorProb_choice <- as.list(methods)
    names(ErrorProb_choice) <- gsub("default", "_OneMap2.0", methods)
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
      if(input$Global0.05){
        geno <- paste0(input$ErrorProb, 0.05)
        if(any(input$ErrorProb %in% "OneMap_version2"))
          geno[which(input$ErrorProb == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb %in% "gusmap"))
          stop(safeError("Gusmap do not allow to change the error rate.
                           Please, select other option."))
      } else {
        geno <- input$ErrorProb
      }
      
      data_n <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom %in% input$CountsFrom) %>%
        group_by(GenoCall, SNPCall, CountsFrom) %>%
        summarise(n = n())
      
      data_n <- perfumaria(data_n)
      
      data <- datas_emp()[[4]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom %in% input$CountsFrom)
      
      data <- perfumaria(data)
      
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
