#' simu_markers_type UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_markers_type_ui <- function(id){
  ns <- NS(id)
  tagList(
    "This bar plot describes the number of markers of each type according with Wu et. al 2002a that remained in the built maps of each method.
          If only real markers are considered real and estimated types are shown, if false positive markers are included, the real type would not be available.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Markers types according with Wu et. al 2002",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("marker_type_out"), width = "100%", height = "550px"),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      
      column(width=6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = maps_choice,
                                    selected = unlist(maps_choice)),
                 hr()
               ),
               fluidPage(
                 checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                    choices = SNPCall_choice,
                                    selected = unlist(SNPCall_choice)),
                 hr()
               ))
      ),
      column(width=6,
             box(width=NULL, solidHeader = T,
                 fluidPage(
                   radioButtons(ns("depth"), label = p("Depth"),
                                choices = "This will be updated",
                                selected = "This will be updated"),
                   hr()
                 ),
                 fluidPage(
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = unlist(CountsFrom_choice)),
                   hr()
                 ),
                 
                 fluidPage(
                   
                   radioButtons(ns("fake"), label = p("Allow false positives?"),
                                choices = fake_choices,
                                selected = "without-false")
                 )
             )
      )
    )
  )
}

#' simu_markers_type Server Functions
#'
#' @noRd 
mod_simu_markers_type_server <- function(input, output, session, datas_simu){
  ns <- session$ns
  
  observe({
    depth_choice <- unique(datas_simu()[[7]][[4]])
    
    updateRadioButtons(session, "depth",
                       label="Depth",
                       choices = depth_choice,
                       selected=names(depth_choice)[1])
  })
  
  button <- eventReactive(input$go, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))

      data <- datas_simu()[[2]] %>% filter(GenoCall %in% input$ErrorProb) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom %in% input$CountsFrom) %>%
        filter(depth == input$depth) %>%
        filter(fake == input$fake)

      data <- perfumaria(data)
      
      # Frequencies
      incProgress(0.5, detail = paste("Doing part", 2))
      data1 <- data %>%  group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth, real.mks) %>%
        summarise(n = n()) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n, -real.mks)
      
      n <- c(estimated="type", real="real.type")
      data1$key <- names(n)[match(data1$key, n)]
      
      if(input$fake == "with-false"){
        data1 <- data1[which(data1$key == "estimated"),]
      }
      
      data1
    })
  })
  
  output$marker_type_out <- renderPlot({
    marker_type_graph(button())
  })
}

## To be copied in the UI
# mod_simu_markers_type_ui("simu_markers_type_ui_1")

## To be copied in the server
# mod_simu_markers_type_server("simu_markers_type_ui_1")
