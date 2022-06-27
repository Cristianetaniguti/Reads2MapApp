#' simu_depths_and_genotyping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_depths_and_genotyping_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The same graphic is plotted in left and right to user be able to compare different methods choosing the options below.
            The x and y axis shows the read counts for reference and alternative alleles, respectively.
            The colors of dots varies according with `Genotypes from` section. User can choose to see colors from simulated genotypes, the estimated or a gradient with the error rate used.
            Users can download the left graphic pushing `Download` button.",
    hr(),
    fluidRow(
      column(width = 6,
             box(title = "Depth and genotyping 1",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("disper_depth_out")),
                 hr(),
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE)),
             ),
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                              choices = ErrorProb_choice,
                              selected = "polyrad"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("Global0.05"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("real"), label = p("Display the"),
                              choices = list("simulated genotypes" = "simulated_genotypes",
                                             "estimated genotypes"="estimated_genotypes",
                                             "errors rate" = "estimated_errors"),
                              selected = "estimated_genotypes"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                              choices = SNPCall_choice,
                              selected = "freebayes"),
                 hr()
               ),
               
               fluidPage(
                 selectInput(ns("seed"), label = p("Seed"),
                             choices = "It will be updated",
                             selected ="It will be updated"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("CountsFrom"), label = p("Counts from"),
                              choices = CountsFrom_choice,
                              selected = "vcf"),
                 hr()
               ),
               
               fluidPage(
                 textInput(ns("marker"), label = p("By default, it plots the genotypes of all markers, you can select a specific one defining its ID here. Example: Chr10_1000"),
                           value = "all"),
                 hr(),
                 box(title = "Wrong genotypes",
                     width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                     DT::dataTableOutput(ns("wrong_mks1_out"))
                 )
               )
             )
      ),
      
      column(width = 6,
             box(title = "Depth and genotyping 2",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("disper_depth2_out")),
                 hr(),
                 tableOutput(ns('disper_depth2_cor_out')),
                 actionButton(ns("go2"), "Update",icon("refresh", verify_fa = FALSE)),
             ),
             
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 radioButtons(ns("ErrorProb2"), label = p("Genotyping method"),
                              choices = ErrorProb_choice,
                              selected = "polyrad"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("Global0.05.2"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("real2"), label = p("Display the"),
                              choices = list("simulated genotypes" = "simulated_genotypes",
                                             "estimated genotypes"="estimated_genotypes",
                                             "errors rate" = "estimated_errors"),
                              selected = "estimated_genotypes"),
                 hr()
               ),
               
               #helpText("Select the SNP calling method"),
               fluidPage(
                 radioButtons(ns("SNPCall2"), label = p("SNP calling method"),
                              choices = SNPCall_choice,
                              selected = "freebayes"),
                 hr()
               ),
               
               #helpText("Select the family seed"),
               fluidPage(
                 selectInput(ns("seed2"), label = p("Seed"),
                             choices = "It will be updated",
                             selected = "It will be updated"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("CountsFrom2"), label = p("Counts from"),
                              choices = CountsFrom_choice,
                              selected = "vcf"),
                 hr()
               ),
               
               #helpText("Read counts from:"),
               fluidPage(
                 textInput(ns("marker2"), label = p("By default, it plots the genotypes of all markers, you can select a specific one defining its ID here. Example: Chr10_1000"),
                           value = "all"),
                 hr(),
                 box(title = "Wrong genotypes",
                     width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                     DT::dataTableOutput(ns("wrong_mks2_out"))
                 )
                 # ),
               )
             )
      )
    )
  )
}
    
#' simu_depths_and_genotyping Server Functions
#'
#' @noRd 
mod_simu_depths_and_genotyping_server <- function(input, output, session, datas_simu){
    ns <- session$ns
    
    observe({
      depth_choice <- unique(datas_simu()[[7]][[4]])
      seeds_choice <- datas_simu()[[7]][[3]]

      updateSelectInput(session, "seed",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      
      updateSelectInput(session, "seed2",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
    })
    
    button1 <- eventReactive(input$go1, {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        #see utils.R
        geno <- test_geno_with_gus(input$Global0.05, input$ErrorProb)
        stop_bam(input$CountsFrom, input$ErrorProb)
        
        # The plot with depths does not differentiate fake markers,
        # they receive NA value in the simulated genotype field
        data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed)]) %>%
          filter(CountsFrom == input$CountsFrom) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed)])
        data[,8:9] <- apply(data[,8:9], 2, as.character)
        
        data <- perfumaria(data)
        
        if(input$marker != "all"){
          data <- data %>% filter(mks == input$marker)
          alpha = 1
        } else {
          alpha = 0.3
        }
        
        if(length(which(is.na(data$ref))) > 0)
          data <- data[-which(is.na(data$ref)),]
        
        list(data, alpha)
      })
    })
    
    output$disper_depth_out <- renderPlot({
      errorProb_graph(button1()[[1]], input$real, button1()[[2]])
    })
    
    output$wrong_mks1_out <- DT::renderDataTable({
      if(input$real == "simulated_genotypes"){
        stop(safeError("Simulated genotypes option is set. All genotypes are correct."))
      } else {
        DT::datatable(button1()[[1]][which(button1()[[1]]$gabGT != button1()[[1]]$gt.onemap.alt.ref),
                                     c(1,2,3,18,9,10,17,20)],
                      options = list(scrollX = TRUE))
      }
    })
    
    button2 <- eventReactive(input$go2, {
      withProgress(message = 'Building right graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        #see utils.R
        geno <- test_geno_with_gus(input$Global0.05.2, input$ErrorProb2)
        stop_bam(input$CountsFrom2, input$ErrorProb2)
        
        # The plot with depths does not differentiate fake markers,
        # they receive NA value in the simulated genotype field
        data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall2) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed2)]) %>%
          filter(CountsFrom == input$CountsFrom2) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed2)])
        data[,8:9] <- apply(data[,8:9], 2, as.character)
        
        data <- perfumaria(data)
        
        if(input$marker2 != "all"){
          data <- data %>% filter(mks == input$marker2)
          alpha = 1
        } else {
          alpha = 0.3
        }
        
        if(length(which(is.na(data$ref))) > 0)
          data <- data[-which(is.na(data$ref)),]
        
        list(data,alpha)
      })
    })
    
    output$disper_depth2_out <- renderPlot({
      errorProb_graph(button2()[[1]], input$real2, button2()[[2]])
    })
    
    output$wrong_mks2_out <- DT::renderDataTable(
      if(input$real2 == "simulated_genotypes"){
        stop(safeError("Simulated genotypes option is set. All genotypes are correct."))
      } else {
        DT::datatable(button2()[[1]][which(button2()[[1]]$gabGT != button2()[[1]]$gt.onemap.alt.ref),
                                     c(1,2,3,18,9,10,17,20)],
                      options = list(scrollX = TRUE))
      }
    )
}
    
## To be copied in the UI
# mod_simu_depths_and_genotyping_ui("simu_depths_and_genotyping_ui_1")
    
## To be copied in the server
# mod_simu_depths_and_genotyping_server("simu_depths_and_genotyping_ui_1")
