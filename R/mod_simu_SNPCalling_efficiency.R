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
    "The graphic here show some characteristics of SNP calling provided by VariantEval (GATK):", br(),
     "nEvalVariants - the number of variants in the eval file", br(),
     "novelSites - the number of variants in the eval considered to be novel in comparison to dbsnp (same as novel row of nEvalVariants column)", br(),
     "nVariantsAtComp - the number of variants present in eval that match the location of a variant in the comparison file (same as known row of nEvalVariants)", br(),
     "compRate - nVariantsAtComp divided by nEvalVariants", br(),
     "nConcordant - the number of variants present in eval that exactly match the genotype present in the comparison file", br(),
     "concordantRate - nConcordant divided by nVariantsAtComp",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "SNP calling efficiency - without filters",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("snpcall_out")), hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE))
             ),
             box(title = "SNP calling efficiency - with VCFtools filters",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 uiOutput(ns("plot.ui")), hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
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
#' @import ggVennDiagram
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
      
      # Unfiltered
      data <- datas_simu()[[5]][,-c(4:7)]  %>%
        filter(tolower(SNPCall) %in% input$SNPCall) %>%
        filter(depth %in% input$depth) %>% 
        filter(Novelty == "all")

      data <- data[,-4]
      data <- data  %>% pivot_longer(cols=c(4:9))
      data$counts <- !(data$name %in% c("compRate", "concordantRate"))

      incProgress(0.5, detail = paste("Doing part", 2))

      seeds <- list()
      for(i in 1:length(datas_simu()[[11]])){
        names_files <- names(datas_simu()[[11]][[i]])
        names_files <- gsub("freebaye_", "freebayes_", names_files)
        idx <- which(grepl(paste(input$depth, collapse = "|"), names_files) &
                       grepl(paste(input$SNPCall,collapse = "|"), names_files) &
                       grepl("vcf", names_files))
        
        idx <- c(idx, grep("true", names_files))
        seeds[[i]] <- datas_simu()[[11]][[i]][idx]
      }

      incProgress(0.75, detail = paste("Doing part", 3))
      
      list(data, seeds)
    })
  })
  
  output$snpcall_out <- renderPlot({
    avalSNPs_graph(button()[[1]])
  })
  
  output$snpcall_filt_out <- renderPlot({
    avalSNPs_graph_filt(button()[[2]])
  })
  
  plotHeight <- reactive({
    size <- round(length(button()[[2]])/3, 0)*500
    if(size == 0) size <- 500
    size
  })
  
  output$plot.ui <- renderUI({
    plotOutput(ns("snpcall_filt_out"), height = plotHeight())
  })
  
}

## To be copied in the UI
# mod_simu_SNPCalling_efficiency_ui("simu_SNPCalling_efficiency_ui_1")

## To be copied in the server
# mod_simu_SNPCalling_efficiency_server("simu_SNPCalling_efficiency_ui_1")
