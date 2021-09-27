#' emp_markers_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_markers_type_ui <- function(id){
  ns <- NS(id)
  tagList(
    "These bar plots describes the number of markers of each type according with Wu et. al 2002a that remained in the built maps of each method.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Marker type",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("marker_type_emp_out")), hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = maps_choice,
                                    selected = unlist(maps_choice)),
               )
             )
      ),
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                    choices = SNPCall_choice,
                                    selected = unlist(SNPCall_choice)),
               ),
               
               fluidPage(
                 checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                    choices = CountsFrom_choice,
                                    selected = unlist(CountsFrom_choice))
               )
             )
      )
    ) 
  )
}

#' emp_markers_type Server Functions
#'
#' @noRd 
mod_emp_markers_type_server <- function(input, output, session, datas_emp){
    ns <- session$ns
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          group_by(type, GenoCall, SNPCall, CountsFrom) %>%
          summarise(n = n()) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom,-n)
        perfumaria(data)
      })
    })
    
    output$marker_type_emp_out <- renderPlot({
      marker_type_graph_emp(button())
    })
}

## To be copied in the UI
# mod_emp_markers_type_ui("emp_markers_type_ui_1")

## To be copied in the server
# mod_emp_markers_type_server("emp_markers_type_ui_1")
