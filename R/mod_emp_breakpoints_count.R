#' emp_breakpoints_count UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_breakpoints_count_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Based on the built map, we can count the number of recombination breakpoints in each individual of progeny using function progeny_haplotypes_counts.
            Choose pipeline and the individuals you want to check the haplotypes.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Breakpoints counts",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 "Estimated number of recombination breakpoints for each individual",
                 hr(),
                 plotOutput(ns("counts_emp_out")),
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
          radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                       choices = SNPCall_choice,
                       selected = "gatk"),
      ),
      box(solidHeader = T,
          radioButtons(ns("CountsFrom"), label = p("Counts from"),
                       choices = CountsFrom_choice,
                       selected = "vcf"),
      )
    )
  )
}

#' emp_breakpoints_count Server Functions
#'
#' @noRd 
mod_emp_breakpoints_count_server <- function(input, output, session, datas_emp){
  ns <- session$ns
  button <- eventReactive(input$go, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      geno <- test_geno_emp(input$Global0.05, input$ErrorProb)
      
      stop_bam(input$CountsFrom, input$ErrorProb)

      temp_n <- paste0("map_",input$SNPCall, "_", input$CountsFrom, "_", geno, ".RData")
      
      if(geno == "gusmap"){
        stop(safeError("We do not include in this app support to do it with GUSMap.
                         Please, select other option."))
      } else {
        idx <- which(datas_emp()[[6]] == temp_n)
        data <- readList(datas_emp()[[8]], index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        inds <- 1:data$data.name$n.ind
        incProgress(0.3, detail = paste("Doing part", 2))
        df <- progeny_haplotypes(data, ind = inds, most_likely = T)
        incProgress(0.6, detail = paste("Doing part", 3))
        progeny_haplotypes_counts(df)
      }
    })
  })
  
  output$counts_emp_out <- renderPlot({
    plot(button())
  })
  
  
  output$counts_emp_df_out <- renderTable({
    data.frame(total = sum(button()$counts),
               mean = mean(button()$counts),
               se = sd(button()$counts)/sqrt(length(button()$counts)),
               var = var(button()$counts),
               sd = sd(button()$counts),
               median = median(button()$counts))
  })
}

## To be copied in the UI
# mod_emp_breakpoints_count_ui("emp_breakpoints_count_ui_1")

## To be copied in the server
# mod_emp_breakpoints_count_server("emp_breakpoints_count_ui_1")
