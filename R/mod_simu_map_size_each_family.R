#' simu_map_size_each_family UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_map_size_each_family_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic show the distribuition of the difference between estimated and simulated  distances for each marker of the generated maps.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Map size each family",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("ind_size_out"), width = "100%", height = "550px"),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
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
               #helpText("Select the SNP calling method"),
               fluidPage(
                 checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                    choices = SNPCall_choice,
                                    selected = unlist(SNPCall_choice)),
                 hr()
               )
             )
      ),
      column(width=6,
             box(width = NULL, solidHeader = TRUE,
                 #helpText("Select the family seed"),
                 fluidPage(
                   selectInput(ns("seed"), label = p("Seed"),
                               choices = "It will be updated",
                               selected = "It will be updated"),
                   hr()
                 ),
                 
                 #helpText("Read counts from:"),
                 fluidPage(
                   
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = unlist(CountsFrom_choice)),
                   hr()
                 ),
                 
                 fluidPage(
                   
                   radioButtons(ns("fake"), label = p("Allow false positives?"),
                                choices = fake_choices,
                                selected = "without-false"),
                 )
             )
      ),
      column(width = 12,
             box(title = "Wrong marker types",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 DT::dataTableOutput(ns("wrong_mks_types_out"))
             )
      )
    ) 
  )
}
    
#' simu_map_size_each_family Server Functions
#'
#' @noRd 
mod_simu_map_size_each_family_server <- function(input, output, session, datas_simu){
    ns <- session$ns
    
    observe({
      seeds_choice <- datas_simu()[[7]][[3]]
      
      updateSelectInput(session, "seed",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
    })
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        # see utils.R
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed)]) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed)]) %>%
          filter(fake == input$fake)
        
        data <- perfumaria(data)
        
        # If there are fake markers the interval distances are plotted
        # If there are no fake markers the difference between estimated and simulated distances are plotted
        if(input$fake == "without-false"){
          data <- data %>% mutate("diff (cM)" = sqrt(c(0,(poscM.norm[-1] - poscM.norm[-length(poscM.norm)]) -
                                                         (rf[-1] - rf[-length(rf)]))^2))
        } else {
          data <- data %>% mutate("diff (cM)" = sqrt(c(0, (rf[-1] - rf[-length(rf)]))^2))
        }
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise("n markers" = n())
        
        data <- merge(data, data_n)
        
        list(data, data_n)
      })
    })
    
    output$ind_size_out <- renderPlot({
      ind_size_graph(button()[[1]], button()[[2]])
    })
    
    output$wrong_mks_types_out <- DT::renderDataTable(
      DT::datatable(button()[[1]][which(button()[[1]]$type != button()[[1]]$real.type),
      ],
      options = list(scrollX = TRUE))
    )
}
    
## To be copied in the UI
# mod_simu_map_size_each_family_ui("simu_map_size_each_family_ui_1")
    
## To be copied in the server
# mod_simu_map_size_each_family_server("simu_map_size_each_family_ui_1")
