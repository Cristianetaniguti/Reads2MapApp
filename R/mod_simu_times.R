#' simu_times UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_times_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic show the distribuition of times and number of markers of all families by depth.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Time spent to estimate genetic distances",
                 width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("times_out")),
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
             box(width=NULL, solidHeader = T,
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
    
#' simu_times Server Functions
#'
#' @noRd 
mod_simu_times_server <- function(input, output, session, datas_simu){
    ns <- session$ns
    
    observe({
      depth_choice <- unique(datas_simu()[[7]][[4]])
      
      updateCheckboxGroupInput(session, "depth",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
    })
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Times graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        
        data_n <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          filter(fake == input$fake) %>%
          group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
          summarise(`n markers` = n())
        
        data <- datas_simu()[[4]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(fake == input$fake) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          filter(depth %in% input$depth)
        
        data$time <- data$time/60
        
        data_df <- merge(data, data_n)
        
        data <- data_df %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
        
        data$key <- gsub("time", "minutes", data$key)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$times_out <- renderPlot({
      times_graph(button())
    })
}
    
## To be copied in the UI
# mod_simu_times_ui("simu_times_ui_1")
    
## To be copied in the server
# mod_simu_times_server("simu_times_ui_1")
