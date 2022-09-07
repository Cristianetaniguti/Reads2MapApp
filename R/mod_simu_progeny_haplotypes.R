#' simu_progeny_haplotypes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_progeny_haplotypes_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Based on the built map, progeny haplotypes can be draw by onemap function progeny_haplotypes. Choose pipeline and the individuals you want to check the haplotypes.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Estimated progeny haplotypes",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("haplot_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
      ),
      column(width = 12,
             box(title = "Real progeny haplotypes",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("haplot_simu_out")),
                 hr(),
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE))
             )
      ), br(),
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
          radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                       choices = SNPCall_choice,
                       selected = "gatk"),
      ),
      box(solidHeader = T,
          selectInput(ns("seed"), label = p("Seed"),
                      choices = "It will be updated",
                      selected = "It will be updated"),
      ),
      box(solidHeader = T,
          radioButtons(ns("fake"), label = p("Allow false positives"),
                       choices = fake_choices,
                       selected = "without-false"),
      ),
      box(solidHeader = T,
          radioButtons(ns("Most_likely"), label = p("Choose how to show genotypes probabilities"),
                       choices = list("The most likely genotypes receiveis maximum probabilities" = TRUE,
                                      "Genotypes probabilities are how they were outputted by HMM"= FALSE),
                       selected = "TRUE"),
      ),
      box(solidHeader = T,
          radioButtons(ns("CountsFrom"), label = p("Counts from"),
                       choices = CountsFrom_choice,
                       selected = "vcf"),
      ),
      box(solidHeader = T, collapsible = T, collapsed = F,
          checkboxGroupInput(ns("inds"), label = p("Individuals from progeny"),
                             choices = "It will be updated",
                             selected = "It will be updated"),
      )
    )
  )
}

#' simu_progeny_haplotypes Server Functions
#'
#' @import onemap
#' 
#' @noRd 
mod_simu_progeny_haplotypes_server <- function(input, output, session, datas_simu){
    ns <- session$ns
    
    observe({
      seeds_choice <- datas_simu()[[7]][[3]]
      inds_choice <- datas_simu()[[7]][[5]]
      updateCheckboxGroupInput(session, "inds",
                               label="Individuals from progeny",
                               choices = inds_choice,
                               selected=unlist(inds_choice)[1])
      updateSelectInput(session, "seed",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
    })
    
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        
        if(input$CountsFrom == "bam" & (input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller")){
          stop(safeError("This option is not available. The SNP callers performs together the SNP and genotype calling
                         using the same read counts, we did not find a way to substitute the depths already used.
                         Please select other option."))
        }
        
        depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed)])
        seed <- datas_simu()[[7]][[2]][as.numeric(input$seed)]
        if(input$fake == "with-false") fake <- T else fake <- F
        temp_n <- map_name(depth, seed, geno, fake,
                           input$SNPCall, input$CountsFrom, datas_simu()[[8]])
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          stop(safeError("We do not include in this app support to do it with GUSMap.
                         Please, choose other option."))
        } else {
          idx <- which(datas_simu()[[8]] == temp_n)
          data <- readList(datas_simu()[[10]], index = idx)
          if (!is.vector(data[[1]][[1]])) data <- data[[1]][[1]] else data <- data[[1]] # bugfix
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
          idx <- which(rownames(data$data.name$geno) %in% input$inds)
          list(data,idx)
        }
      })
    })
    
    output$haplot_out <- renderPlot({
      plot(progeny_haplotypes(button()[[1]], ind = button()[[2]], most_likely = input$Most_likely))
    })
    
    button1 <- eventReactive(input$go1, {
      withProgress(message = 'Building graphic', value = 0, {
        sub_dat <- subset(datas_simu()[[9]], datas_simu()[[9]][[2]] == datas_simu()[[7]][[1]][as.numeric(input$seed)] &
                            datas_simu()[[9]][[1]] == datas_simu()[[7]][[2]][as.numeric(input$seed)] &
                            datas_simu()[[9]][[3]] %in% input$inds)
        
        sub_dat <- sub_dat[,-c(1,2)]
        class(sub_dat) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
        sub_dat
      })
    })
    
    output$haplot_simu_out <- renderPlot({
      plot(button1())
    })
}

## To be copied in the UI
# mod_simu_progeny_haplotypes_ui("simu_progeny_haplotypes_ui_1")

## To be copied in the server
# mod_simu_progeny_haplotypes_server("simu_progeny_haplotypes_ui_1")
