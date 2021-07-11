#' simu_SNPCalling_efficiency UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_SNPCalling_efficiency_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic here show some characteristics of SNP calling. The x axis contain the different
            characteristics indexed according with section `Options`, and y axis shows the number of markers.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "SNP calling efficiency",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("snpcall_out")), hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      column(width = 6,
             box(width = 6, solidHeader = T,
                 fluidPage(
                   checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                      choices = SNPCall_choice,
                                      selected = unlist(SNPCall_choice)),
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

#' simu_SNPCalling_efficiency Server Functions
#'
#' @import tidyr
#' 
#' @noRd 
mod_simu_SNPCalling_efficiency_server <- function(input, output, session, datas_simu){
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
      
      
      data <- datas_simu()[[5]][,-c(8,9)]  %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(depth %in% input$depth)
      
      colnames(data)[4:7] <- c("real", "estimated", "true positives", "false positives")
      data$`false negatives` <- data$real - data$`true positives`
      
      data <- data  %>% pivot_longer(cols=c(4:8))
      
      incProgress(0.5, detail = paste("Doing part", 2))
      # perfurmaria
      snpcall <- c(GATK = "gatk", freebayes = "freebayes")
      data$SNPCall <- names(snpcall)[match(data$SNPCall, snpcall)]
      
      data$name <- factor(data$name, levels = c("real", "estimated", "true positives", "false positives", "false negatives"))
      
      data
    })
  })
  
  output$snpcall_out <- renderPlot({
    avalSNPs_graph(button())
  })
  
}

## To be copied in the UI
# mod_simu_SNPCalling_efficiency_ui("simu_SNPCalling_efficiency_ui_1")

## To be copied in the server
# mod_simu_SNPCalling_efficiency_server("simu_SNPCalling_efficiency_ui_1")
