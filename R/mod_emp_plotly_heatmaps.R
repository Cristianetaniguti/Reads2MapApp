#' emp_plotly_heatmaps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom plotly plotlyOutput
#' 
mod_emp_plotly_heatmaps_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(title = "Interactive recombination fraction heatmap",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotlyOutput(ns("heatmaps_emp_out"), height = 650),hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
      )
    ),
    fluidRow(
      column(width = 6,
             box(solidHeader = T,
                 width = 6,
                 fluidPage(
                   radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                                choices = "This will be updated",
                                selected = "This will be updated"),
                   hr()
                 ),
                 fluidPage(
                   radioButtons(ns("Global0.05"), label = p("Error rate"),
                                choices = global0.05_choices,
                                selected = "FALSE"),
                   hr()
                 )
             )
      ),
      column(width = 6,
             box(width = 6,solidHeader = T,
                 fluidPage(
                   radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                                choices = "This will be updated",
                                selected = "This will be updated"),
                   hr()
                 ),
                 fluidPage(
                   radioButtons(ns("CountsFrom"), label = p("Counts from"),
                                choices = "This will be updated",
                                selected = "This will be updated")

                 )
             )
      )
    )
  )
}
    
#' emp_plotly_heatmaps Server Functions
#'
#' @importFrom plotly renderPlotly
#' 
#' @noRd 
mod_emp_plotly_heatmaps_server <- function(input, output, session, datas_emp){
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
        if(input$Global0.05){
          if(input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb == "gusmap"){
            stop(safeError("Gusmap do not build plotly heatmaps.
                           Please, select other option."))
          } else {
            geno <- paste0(input$ErrorProb, 0.05)
          }
        } else {
          ifelse(input$ErrorProb == "OneMap_version2", geno <- "default", geno <- input$ErrorProb)
        }
        
        temp_n <- paste0("map_",input$SNPCall, "_", input$CountsFrom, "_", geno, ".RData")
        incProgress(0.25, detail = paste("Doing part", 2))
        idx <- which(datas_emp()[[6]] == temp_n)
        data <- readList(datas_emp()[[8]], index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        incProgress(0.5, detail = paste("Doing part", 3))
        data
      })
    })
    
    output$heatmaps_emp_out <- renderPlotly({
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0.75, detail = paste("Doing part", 4))
        rf_graph_table(button(), inter = T, html.file = tempfile(patter="file",tmpdir = tempdir(),fileext = ".html"),display = F)
      })
    })
}
    
## To be copied in the UI
# mod_emp_plotly_heatmaps_ui("emp_plotly_heatmaps_ui_1")
    
## To be copied in the server
# mod_emp_plotly_heatmaps_server("emp_plotly_heatmaps_ui_1")
