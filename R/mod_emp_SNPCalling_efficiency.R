#' emp_SNPCalling_efficiency UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_SNPCalling_efficiency_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The Venn diagrams here are built with marker position compatibility between SNP calling methods. Only informative markers are considered.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Marker position compatibility between data sets",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("SNPCalling_efficiency_emp_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("datasets"), label = p("Choose maximum 4 pipelines"),
                                    choices = "It will be update",
                                    selected = "It will be update"),
                 hr()
               )
             )
      )
    )
  )
}

#' emp_SNPCalling_efficiency Server Functions
#'
#' @noRd 
mod_emp_SNPCalling_efficiency_server <- function(input, output, session, datas_emp){
  ns <- session$ns
  
  observe({
    pip_names <- unique(paste0(datas_emp()[[1]]$SNPCall, "_",datas_emp()[[1]]$CountsFrom, "_",datas_emp()[[1]]$GenoCall))
    pip_names <- pip_names[-c(grep("0.05", pip_names), grep("gusmap", pip_names))]
    pip_choices <- as.list(pip_names)
    
    updateCheckboxGroupInput(session, "datasets",
                             label="Choose maximum 4 pipelines",
                             choices = pip_choices,
                             selected=unlist(pip_choices)[1:2])
  })
  
  button <- eventReactive(input$go, {
    
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      pips <- paste0(datas_emp()[[1]]$SNPCall, "_",datas_emp()[[1]]$CountsFrom, "_",datas_emp()[[1]]$GenoCall)
      idx <- which(pips %in% input$datasets)
      pips <- pips[idx]
      
      data <- datas_emp()[[1]][idx,]
      data <- split(data$mks, pips)
      data
    })
  })
  
  output$SNPCalling_efficiency_emp_out <- renderPlot({
    SNPCalling_efficiency_graph_emp(button())
  })
}

## To be copied in the UI
# mod_emp_SNPCalling_efficiency_ui("emp_SNPCalling_efficiency_ui_1")

## To be copied in the server
# mod_emp_SNPCalling_efficiency_server("emp_SNPCalling_efficiency_ui_1")
