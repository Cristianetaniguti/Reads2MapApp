#' simu_cM_Mb UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_cM_Mb_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The scatter plots show the relation between genetic (cM) and physical (MB) distance.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title= "cM x Mb",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("cmbymb_out"), width = "100%", height = "1000px"),
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
               )
             )
      ),
      column(width=6,
             box(width = NULL, solidHeader = TRUE,
                 fluidPage(
                   selectInput(ns("seed"), label = p("Seed"),
                               choices = "It will be updated",
                               selected = "It will be updated"),
                   hr()
                 ),
                 
                 fluidPage(
                   
                   radioButtons(ns("CountsFrom"), label = p("Counts from"),
                                choices = CountsFrom_choice,
                                selected = "vcf"),
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

#' simu_cM_Mb Server Functions
#'
#' @noRd 
mod_simu_cM_Mb_server <- function(input, output, session, datas_simu){
  ns <- session$ns
  
  observe({
    seeds_choice <- datas_simu()[[7]][[3]]
    
    updateSelectInput(session, "seed",
                      label="Seed",
                      choices = seeds_choice,
                      selected=unlist(seeds_choice)[1])
  })
  
  button <- eventReactive(input$go, {
    withProgress(message = 'Building left graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      geno <- test_geno(input$Global0.05, input$ErrorProb)
      
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed)]) %>%
        filter(CountsFrom == input$CountsFrom |
                 (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
        filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed)]) %>%
        filter(fake == input$fake)  %>%
        select(pos, rf, poscM.norm, real.mks, SNPCall, GenoCall) %>%
        gather(key, value, -pos, -real.mks, -SNPCall, -GenoCall)
      
      data <- perfumaria(data)
      
      data
    })
  })
  
  output$cmbymb_out <- renderPlot({
    cmbymb(button())
  })
}

## To be copied in the UI
# mod_simu_cM_Mb_ui("simu_cM_Mb_ui_1")

## To be copied in the server
# mod_simu_cM_Mb_server("simu_cM_Mb_ui_1")
