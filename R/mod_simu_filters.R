#' simu_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic here show the total number of markers (n_markers) available for analysis,
            the number of markers filtered by OneMap because of missing data, segregation distortion and redundancy.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Filters",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("filters_out")),  hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = ErrorProb_choice,
                                    selected = unlist(ErrorProb_choice)),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("Global0.05"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
                 hr()
               ),
               
               fluidPage(
                 checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                    choices = SNPCall_choice,
                                    selected = unlist(SNPCall_choice)),
                 hr()
               )
             )
      ),
      column(width = 6,
             box(width = NULL, solidHeader = T,
                 fluidPage(
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = unlist(CountsFrom_choice)),
                   hr()
                 ),
                 fluidPage(
                   checkboxGroupInput(ns("depth"), label = p("Depth"),
                                      choices = "This will be updated",
                                      selected = "This will be updated")
                 )
             )
      )
    )
    
  )
}
    
#' simu_filters Server Functions
#'
#' @noRd 
mod_simu_filters_server <- function(input, output, session, datas_simu){
    ns <- session$ns
    
    observe({
      depth_choice <- unique(datas_simu()[[7]][[4]])
      
      updateCheckboxGroupInput(session, "depth",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
    })
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        
        data <- datas_simu()[[3]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(depth %in% input$depth) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          pivot_longer(cols=c("n_markers", "higher.than.25..missing",
                              "distorted_markers", "redundant_markers",
                              "after_filters"))
        
        n <- c(`Without filters`="n_markers",
               `Missing data`= "higher.than.25..missing",
               `Segregation test`="distorted_markers",
               `Redundants`="redundant_markers",
               `After all filters` = "after_filters")
        
        data$name <- names(n)[match(data$name, n)]
        data$name <- factor(data$name, levels = c("Without filters",
                                                  "Missing data",
                                                  "Segregation test",
                                                  "Redundants",
                                                  "After all filters"))
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$filters_out <- renderPlot({
      filters_graph(button())
    })
}
    
## To be copied in the UI
# mod_simu_filters_ui("simu_filters_ui_1")
    
## To be copied in the server
# mod_simu_filters_server("simu_filters_ui_1")
