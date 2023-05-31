#' emp_depth_and_genotyping UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_depth_and_genotyping_ui <- function(id){
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
                 plotOutput(ns("disper_depth_emp_out")),
                 hr(),
                 tableOutput(ns("disper_depth_emp_table")),
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE)),
             ),
             
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 radioButtons(ns("ErrorProb1"), label = p("Genotyping method"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("real1"), label = p("Display the"),
                              choices = list("estimated genotypes"="estimated_genotypes",
                                             "errors rate" = "estimated_errors"),
                              selected = "estimated_genotypes"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("geno_from1"), label = p("Genotypes from"),
                              choices = list("vcf"="vcf",
                                             "onemap" = "onemap"),
                              selected = "onemap"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("Global0.05.1"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("SNPCall1"), label = p("SNP calling method"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
                 hr()
               ),
               
               fluidPage(
                 
                 radioButtons(ns("CountsFrom1"), label = p("Counts from"),
                              choices = "This will be updated",
                              selected = "This will be updated")
               )
             )
      ),
      
      column(width = 6,
             box(title = "Depth and genotyping 2",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("disper_depth2_emp_out")),
                 hr(),
                 tableOutput(ns("disper_depth2_emp_table")),
                 actionButton(ns("go2"), "Update",icon("refresh", verify_fa = FALSE))
             ),
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 radioButtons(ns("ErrorProb2"), label = p("Genotyping method"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("real2"), label = p("Display the"),
                              choices = list("estimated genotypes"="estimated_genotypes",
                                             "estimated errors" = "estimated_errors"),
                              selected = "estimated_genotypes"),
                 hr()
               ),
               
               fluidPage(
                 radioButtons(ns("geno_from2"), label = p("Genotypes from"),
                              choices = list("vcf"="vcf",
                                             "onemap" = "onemap"),
                              selected = "onemap"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("Global0.05.2"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("SNPCall2"), label = p("SNP calling method"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
                 hr()
               ),
               
               fluidPage(
                 
                 radioButtons(ns("CountsFrom2"), label = p("Counts from"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
                 hr()
               )
               #downloadButton(ns('downloadData'), 'Download data')
             )
      )
    )
  )
}

#' emp_depth_and_genotyping Server Functions
#'
#' @noRd 
mod_emp_depth_and_genotyping_server <- function(input, output, session, datas_emp){
    ns <- session$ns
    
    observe({
      
      SNPCall_choice <- as.list(unique(datas_emp()[[1]]$SNPCall))
      names(SNPCall_choice) <- unique(datas_emp()[[1]]$SNPCall)
      methods <- unique(datas_emp()[[1]]$GenoCall)
      methods <- unique(gsub("0.05", "", methods))
      
      ErrorProb_choice <- as.list(methods)
      names(ErrorProb_choice) <- gsub("default", "_OneMap2.0", methods)
      CountsFrom_choice <- as.list(unique(datas_emp()[[1]]$CountsFrom))
      names(CountsFrom_choice) <- unique(datas_emp()[[1]]$CountsFrom)
      
      updateRadioButtons(session, "SNPCall1",
                               label="SNP call method",
                               choices = SNPCall_choice,
                               selected=unlist(SNPCall_choice)[1])
      
      updateRadioButtons(session, "ErrorProb1",
                               label="Genotyping method",
                               choices = ErrorProb_choice,
                               selected=unlist(ErrorProb_choice)[1])
      
      updateRadioButtons(session, "CountsFrom1",
                               label="Counts From",
                               choices = CountsFrom_choice,
                               selected=unlist(CountsFrom_choice)[1])
      
      updateRadioButtons(session, "SNPCall2",
                               label="SNP call method",
                               choices = SNPCall_choice,
                               selected=unlist(SNPCall_choice)[1])
      
      updateRadioButtons(session, "ErrorProb2",
                               label="Genotyping method",
                               choices = ErrorProb_choice,
                               selected=unlist(ErrorProb_choice)[1])
      
      updateRadioButtons(session, "CountsFrom2",
                               label="Counts From",
                               choices = CountsFrom_choice,
                               selected=unlist(CountsFrom_choice)[1])
    })
    
    button1 <- eventReactive(input$go1, {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno_with_gus(input$Global0.05.1, input$ErrorProb1)
        stop_bam(input$CountsFrom1, input$ErrorProb1)

        data <- datas_emp()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall1) %>%
          filter(CountsFrom == input$CountsFrom1)
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$disper_depth_emp_out <- renderPlot({
      errorProb_graph_emp(button1(), input$real1, input$geno_from1)
    })
    
    output$disper_depth_emp_table <- renderTable({
      sum_depth <- button1()$alt + button1()$ref
      
      # Parents
      parents.idx <- which(apply(button1()[,10:13], 1, function(x) all(is.na(x))))
      parents <- sum_depth[parents.idx]
      progeny <- sum_depth[-parents.idx]
      
      if(length(which(is.na(sum_depth)) > 0)){
        parents <- parents[-which(is.na(parents))]
        progeny <- progeny[-which(is.na(progeny))]
      }
      
      data.frame("progeny mean" = mean(progeny), "progeny se" = sd(progeny)/sqrt(length(progeny)),
                 "parents mean" = mean(parents), "parents se" = sd(parents)/sqrt(length(parents)))
    })
    
    button2 <- eventReactive(input$go2, {
      withProgress(message = 'Building right graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))

        geno <- test_geno_with_gus(input$Global0.05.2, input$ErrorProb2)
        stop_bam(input$CountsFrom2, input$ErrorProb2)
        
        data <- datas_emp()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall2) %>%
          filter(CountsFrom == input$CountsFrom2)
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$disper_depth2_emp_out <- renderPlot({
      errorProb_graph_emp(button2(), input$real2, input$geno_from2)
    })
    
    output$disper_depth2_emp_table <- renderTable({
      sum_depth <- button2()$alt + button2()$ref
      data.frame("mean" = mean(sum_depth), "se" = sd(sum_depth)/sqrt(length(sum_depth)))
    })
    
    # output$downloadData <- downloadHandler(
    #   filename = function() { 
    #     paste("alleles_depths-", Sys.Date(), ".png", sep="")
    #   },
    #   content = function(file) {
    #     for(i in 1:length())
    #     graphics[[i]] <- errorProb_graph_emp(button1(), input$real1, input$geno_from1)
    #   })
    
}

## To be copied in the UI
# mod_emp_depth_and_genotyping_ui("emp_depth_and_genotyping_ui_1")

## To be copied in the server
# mod_emp_depth_and_genotyping_server("emp_depth_and_genotyping_ui_1")
