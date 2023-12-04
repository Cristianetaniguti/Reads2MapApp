#' emp_breakpoints_count UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_breakpoints_count_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Based on the built map, we can count the number of recombination breakpoints in each individual of progeny using function progeny_haplotypes_counts.
            Choose pipeline and the individuals you want to check the haplotypes.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Breakpoints counts",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 "Estimated number of recombination breakpoints for each individual",
                 hr(),
                 plotOutput(ns("counts_emp_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)))
      ),
      box(solidHeader = T,
          radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                       choices = "This will be updated",
                       selected = "This will be updated"),
      ),
      box(solidHeader = T,
          radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                       choices = "This will be updated",
                       selected = "This will be updated"),
      ),
      box(solidHeader = T,
          radioButtons(ns("CountsFrom"), label = p("Counts from"),
                       choices = "This will be updated",
                       selected = "This will be updated"),
      )
    )
  )
}

#' emp_breakpoints_count Server Functions
#'
#' @noRd 
mod_emp_breakpoints_count_server <- function(input, output, session, datas_emp){
  ns <- session$ns
  
  observe({
    
    SNPCall_choice <- as.list(unique(datas_emp()[[2]]$SNPCall))
    names(SNPCall_choice) <- unique(datas_emp()[[2]]$SNPCall)

    ErrorProb_choice <- as.list(unique(datas_emp()[[2]]$GenoCall))
    names(ErrorProb_choice) <- gsub("default", "_OneMap2.0", unique(datas_emp()[[2]]$GenoCall))
    CountsFrom_choice <- as.list(unique(datas_emp()[[2]]$CountsFrom))
    names(CountsFrom_choice) <- unique(datas_emp()[[2]]$CountsFrom)
    
    updateRadioButtons(session, "SNPCall",
                             label="SNP call method",
                             choices = SNPCall_choice,
                             selected=unlist(SNPCall_choice)[1])
    
    updateRadioButtons(session, "ErrorProb",
                             label="Genotyping method",
                             choices = ErrorProb_choice,
                             selected=unlist(ErrorProb_choice)[1])
    
    updateRadioButtons(session, "CountsFrom",
                             label="Counts From",
                             choices = CountsFrom_choice,
                             selected=unlist(CountsFrom_choice)[1])
  })
  
  button <- eventReactive(input$go, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      stop_bam(input$CountsFrom, input$ErrorProb)

      temp_n <- paste0("map_",input$SNPCall, "_", input$CountsFrom, "_", input$ErrorProb, ".rds")
      
      if(grepl("gusmap", input$ErrorProb)){
        stop(safeError("We do not include in this app support to do it with GUSMap.
                         Please, select other option."))
      } else {
        idx <- which(datas_emp()[[6]] == temp_n)
        data <- datas_emp()[[8]][[idx]]
        inds <- 1:data$data.name$n.ind
        incProgress(0.3, detail = paste("Doing part", 2))
        df <- progeny_haplotypes(data, ind = inds, most_likely = T)
        incProgress(0.6, detail = paste("Doing part", 3))
        progeny_haplotypes_counts(df)
      }
    })
  })
  
  output$counts_emp_out <- renderPlot({
    plot(button())
  })
}

## To be copied in the UI
# mod_emp_breakpoints_count_ui("emp_breakpoints_count_ui_1")

## To be copied in the server
# mod_emp_breakpoints_count_server("emp_breakpoints_count_ui_1")
