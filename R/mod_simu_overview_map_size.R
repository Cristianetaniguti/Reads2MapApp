#' simu_overview_map_size UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_overview_map_size_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Here we show an overview of all simulated familes by depths. Users can choose the descriptive
            statistical to overview the differences between estimated and simulated distances.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title= "Overview map size",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("all_size_out"), width = "100%", height = "550px"),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      
      column(width=6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 radioButtons(ns("stats"), label = p("Statistic"),
                              choices = stats_choice,
                              selected = "mean"),
                 hr()
               ),
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
             )),
      column(width=6,
             box(width = NULL, solidHeader = TRUE,
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
    
#' simu_overview_map_size Server Functions
#'
#' @noRd 
mod_simu_overview_map_size_server <- function(input, output, session, datas_simu){
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
          filter(fake == input$fake) %>%
          group_by(seed,GenoCall, SNPCall, CountsFrom, depth)
        
        data <- perfumaria(data)
        
        # difference between markers
        if(input$fake == "with-false"){
          data <- data %>% mutate("diff" = sqrt(c(0, (rf[-1] - rf[-length(rf)]))^2))
        }
        
        data_n <- data %>%  summarise(`n markers` = n())
        
        data <- switch(input$stats,
                       "euclidean_dist" = summarise(data, D = as.vector((((length(rf)-1)^(-1))*(t(rf-poscM.norm)%*%(rf-poscM.norm)))^{1/2})),
                       "mean" = summarise(data, `mean (cM)` = mean(diff, na.rm=T)),
                       "median" = summarise(data, `median (cM)` = median(diff, na.rm=T)),
                       "var" = summarise(data, `var (cM)` = var(diff, na.rm=T)),
                       "total" = summarise(data, `sum (cM)` = sum(diff, na.rm=T)),
                       "total_size" = summarise(data, `cumsum (cM)` = rf[length(rf)]))
        
        incProgress(0.5, detail = paste("Doing part", 2))
        list(data, data_n)
      })
    })
    
    output$all_size_out <- renderPlot({
      all_size_graph(button()[[1]],button()[[2]],input$stats, input$fake)
    })
}
    
## To be copied in the UI
# mod_simu_overview_map_size_ui("simu_overview_map_size_ui_1")
    
## To be copied in the server
# mod_simu_overview_map_size_server("simu_overview_map_size_ui_1")
