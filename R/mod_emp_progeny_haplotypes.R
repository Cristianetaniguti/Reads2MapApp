#' emp_progeny_haplotypes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_progeny_haplotypes_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Based on the built map, progeny haplotypes can be draw by onemap function progeny_haplotypes. Choose pipeline and the individuals you want to check the haplotypes.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Individual haplotype",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("haplot_emp_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh")))
      ),
      box(solidHeader = T,
          radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                       choices = maps_choice,
                       selected = "updog"),
      ),
      box(solidHeader = T,
          radioButtons(ns("Global0.05"), label = p("Error rate"),
                       choices = global0.05_choices,
                       selected = "FALSE"),
      ),
      box(solidHeader = T,
          radioButtons(ns("Most_likely"), label = p("Choose how to show genotypes probabilities"),
                       choices = list("The most likely genotypes receiveis maximum probabilities" = TRUE,
                                      "Genotypes probabilities are how they were outputted by HMM"= FALSE),
                       selected = "TRUE"),
      ),
      
      box(solidHeader = T,
          radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                       choices = SNPCall_choice,
                       selected = "gatk"),
      ),
      box(solidHeader = T,
          radioButtons(ns("CountsFrom"), label = p("Counts from"),
                       choices = CountsFrom_choice,
                       selected = "vcf"),
      ),
      box(solidHeader = T, collapsible = T,
          checkboxGroupInput(ns("inds"), label = p("Individuals from progeny"),
                             choices = "It will be updated",
                             selected = "It will be updated"),
      ),
    )
  )
}
    
#' emp_progeny_haplotypes Server Functions
#'
#' @noRd 
mod_emp_progeny_haplotypes_server <- function(input, output, session, datas_emp){
    ns <- session$ns
    
    observe({
      inds_choice <- datas_emp()[[7]]
      
      updateCheckboxGroupInput(session, "inds",
                               label="Individuals from progeny",
                               choices = inds_choice,
                               selected=unlist(inds_choice)[1])
    })
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05){
          if( input$ErrorProb == "OneMap_version2"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb == "gusmap"){
            stop(safeError("Gusmap do not allow to change the error rate.
                           Please, select other option."))
          } else {
            geno <- paste0(input$ErrorProb, 0.05)
          }
        } else {
          if( input$ErrorProb == "OneMap_version2"){
            geno <- "default"
          } else {
            geno <- input$ErrorProb
          }
        }
        
        if(input$CountsFrom == "bam" & (input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller")){
          stop(safeError("This option is not available. The SNP callers performs together the SNP
                         and genotype calling using the same read counts, we did not find a way to
                         substitute the depths already used. Please select other option."))
        }
        
        temp_n <- paste0("map_",input$SNPCall, "_", input$CountsFrom, "_", geno, ".RData")
        if(geno == "gusmap"){
          stop(safeError("We do not include in this app support to do it with GUSMap.
                         Please, select other option."))
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- readList(datas_emp()[[8]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          data
        }
      })
    })
    
    output$haplot_emp_out <- renderPlot({
      plot(progeny_haplotypes(button(), ind = as.numeric(input$inds), most_likely = input$Most_likely))
    })
}
    
## To be copied in the UI
# mod_emp_progeny_haplotypes_ui("emp_progeny_haplotypes_ui_1")
    
## To be copied in the server
# mod_emp_progeny_haplotypes_server("emp_progeny_haplotypes_ui_1")
