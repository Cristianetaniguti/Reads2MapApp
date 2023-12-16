#' haplo_emp_poly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_haplo_emp_poly_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Based on the built map, progeny haplotypes can be draw by MAPpoly. Choose pipeline and the individual you want to check the haplotypes.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Individual haplotype",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotlyOutput(ns("haplot_emp_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)))
      ),
      box(solidHeader = T,
          radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                       choices = "This will be updated",
                       selected = "This will be updated"),
      ),
      box(solidHeader = T,
          radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                       choices = "This will be updated",
                       selected = "This will be updated"),
      ),
      box(solidHeader = T,
          radioButtons(ns("CountsFrom"), label = p("Counts from"),
                       choices = "This will be updated",
                       selected = "This will be updated"),
      ),
      box(solidHeader = T, collapsible = T,
          checkboxGroupInput(ns("inds"), label = p("Individuals from progeny"),
                             choices = "It will be updated",
                             selected = "It will be updated"),
      ),
    )
  )
}

#' haplo_emp_poly Server Functions
#'
#' @noRd 
mod_haplo_emp_poly_server <- function(input, output, session, datas_poly_emp){
  ns <- session$ns
  
  observe({
    
    file_names <- strsplit(names(datas_poly_emp()[[3]]), "_")
    SNPCall_choice <- as.list(unique(sapply(file_names, "[[", 1)))
    names(SNPCall_choice) <- unique(sapply(file_names, "[[", 1))
    
    ErrorProb_choice <- as.list(unique(sapply(file_names, "[[",2)))
    names(ErrorProb_choice) <- unique(sapply(file_names, "[[",2))
    
    CountsFrom_choice <- as.list(unique(sapply(file_names, "[[",3)))
    names(CountsFrom_choice) <- unique(sapply(file_names, "[[",3))
    
    updateRadioButtons(session, "SNPCall",
                       label="SNP call method",
                       choices = SNPCall_choice,
                       selected=unlist(SNPCall_choice)[1])
    
    updateRadioButtons(session, "ErrorProb",
                       label="Genotyping method",
                       choices = ErrorProb_choice,
                       selected=unlist(ErrorProb_choice)[1])
    
    updateRadioButtons(session, "CountsFrom",
                       label="Counts From",
                       choices = CountsFrom_choice,
                       selected=unlist(CountsFrom_choice)[1])
  })
  
  observe({
    inds_choice <- datas_poly_emp()$dat[[1]]$ind.names
    
    updateRadioButtons(session, "inds",
                       label="Individuals from progeny",
                       choices = inds_choice,
                       selected=unlist(inds_choice)[1])
  })
  
  button <- eventReactive(input$go, {
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))

      idx <- which(grepl(input$CountsFrom, names(datas_poly_emp()$dat)) &
                     grepl(input$SNPCall, names(datas_poly_emp()$dat)) & 
                     grepl(sapply(strsplit(input$ErrorProb, "0"), "[[",1), names(datas_poly_emp()$dat)))
      
      idx2 <- which(grepl(input$CountsFrom, names(datas_poly_emp()$map)) &
                      grepl(input$SNPCall, names(datas_poly_emp()$map)) & 
                      grepl(paste0(input$ErrorProb, "_"), names(datas_poly_emp()$map)))
      
      seq <- datas_poly_emp()$map[[idx2]][[1]]
      dat <<- datas_poly_emp()$dat[[idx]]
      
      print(names(datas_poly_emp()$map)[idx2])
      print(names(datas_poly_emp()$dat)[idx])
      
      genoprob <- calc_genoprob(seq)
      homoprobs <- calc_homologprob(genoprob)
      homoprobs
    })
  })
  
  output$haplot_emp_out <- renderPlotly({
    plot(button(), ind = input$inds)
  })
}

## To be copied in the UI
# mod_haplo_emp_poly_ui("haplo_emp_poly_ui_1")

## To be copied in the server
# mod_haplo_emp_poly_server("haplo_emp_poly_ui_1")
