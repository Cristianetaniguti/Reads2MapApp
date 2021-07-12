#' simu_phases UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_phases_ui <- function(id){
  ns <- NS(id)
  tagList(
    "These graphic show the percentage of right estimated phases for all estimated families by depths.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Phases",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("phases_out"), width = "100%", height = "550px"),
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
      column(width=6,
             box(
               width=NULL, solidHeader = T,
               fluidPage(
                 checkboxGroupInput(ns("depth"), label = p("Depth"),
                                    choices = "This will be updated",
                                    selected = "This will be updated"),
                 hr()
               ),
               fluidPage(
                 checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                    choices = CountsFrom_choice,
                                    selected = unlist(CountsFrom_choice)),
                 hr()
               )
             )
      )
    )
  )
}
    
#' simu_phases Server Functions
#'
#' @noRd 
mod_simu_phases_server <- function(input, output, session, datas_simu){
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
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          filter(depth %in% input$depth) %>%
          filter(fake == "without-false") %>%
          filter(real.mks == "true marker") # This graphic do not consider multiallelic markers because their were not simulated
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
          summarise(`n markers` = n())
        
        data1 <- data %>% group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
          summarise(`% correct`= 100*sum(est.phases == real.phases)/length(real.phases))
        
        data <- merge(data1, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$phases_out <- renderPlot({
      phases_graph(button())
    })
}
    
## To be copied in the UI
# mod_simu_phases_ui("simu_phases_ui_1")
    
## To be copied in the server
# mod_simu_phases_server("simu_phases_ui_1")
