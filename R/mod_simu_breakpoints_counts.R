#' simu_breakpoints_counts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_breakpoints_counts_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Based on the built map, we can count the number of recombination breakpoints in each individual of progeny using function progeny_haplotypes_counts.
            Choose pipeline and the individuals you want to check the haplotypes.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Estimated number of recombination breakpoints for each individual",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("counts_out"))
             )
      ),
      column(width = 12,
             box(title = "Real number of recombination breakpoints for each individual",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 hr(),
                 plotOutput(ns("counts_simu_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
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
          radioButtons(ns("CountsFrom"), label = p("Counts from"),
                       choices = CountsFrom_choice,
                       selected = "vcf"),
      )
    )
  )
}
    
#' simu_breakpoints_counts Server Functions
#'
#' @import onemap
#' 
#' @noRd 
mod_simu_breakpoints_counts_server <- function(input, output, session, datas_simu){
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
        
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        
        stop_bam(input$CountsFrom, input$ErrorProb)
        
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
          data <- largeList::readList(datas_simu()[[10]], index = idx)
          if (!is.vector(data[[1]][[1]])) data <- data[[1]][[1]] else data <- data[[1]] # bugfix
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
          inds <- 1:data$data.name$n.ind
          df <- progeny_haplotypes(data, ind = inds, most_likely = T)
          data1 <- progeny_haplotypes_counts(df)
          
          ## Correct
          sub_dat <- subset(datas_simu()[[9]], datas_simu()[[9]][[2]] == datas_simu()[[7]][[1]][as.numeric(input$seed)] &
                              datas_simu()[[9]][[1]] == datas_simu()[[7]][[2]][as.numeric(input$seed)])
          
          sub_dat <- sub_dat[,-c(1,2)]
          class(sub_dat) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
          data2 <- progeny_haplotypes_counts(x = sub_dat)
          
          data1$grp <- unique(data2$grp)
          
          list(data1, data2)
        }
      })
    })
    
    output$counts_out <- renderPlot({
      plot(button()[[1]])
    })
    
    output$counts_simu_out <- renderPlot({
      plot(button()[[2]])
    })
}
    
## To be copied in the UI
# mod_simu_breakpoints_counts_ui("simu_breakpoints_counts_ui_1")
    
## To be copied in the server
# mod_simu_breakpoints_counts_server("simu_breakpoints_counts_ui_1")
