#' dat_poly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dat_poly_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             box(solidHeader = T,
                 column(width = 6,
                        radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                                     choices = "This will be updated",
                                     selected = "This will be updated")),
                 column(width = 6,
                        radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                                     choices = "This will be updated",
                                     selected = "This will be updated"),
                        radioButtons(ns("CountsFrom"), label = p("Counts from"),
                                     choices = "This will be updated",
                                     selected = "This will be updated"),
                 )
             )
      ), hr(),
      column(width = 12,
             box(title = "Dataset overview",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("dat_out")),hr(),
             ),
             box(title = "Recombination fraction heatmap",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("rf_out")),hr(),
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE)),
             ),
             box(title = "Genetic map",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 actionButton(ns("go2"), "Update",icon("refresh", verify_fa = FALSE)),
                 imageOutput(ns("map_out"), width = "100%", height = "100%"),
             )
      )
    )
  )
}

#' dat_poly Server Functions
#' 
#' @import mappoly
#' @importFrom grDevices dev.off pdf rainbow
#'
#' @noRd 
mod_dat_poly_server <- function(input, output, session, datas_poly_emp){
  ns <- session$ns
  
  observe({
    file_names <- strsplit(names(datas_poly_emp()[[4]]), "_")
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
  
  button1 <- eventReactive(input$go1, {
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      stop_bam(input$CountsFrom, input$ErrorProb)
      
      idx <- which(grepl(input$CountsFrom, names(datas_poly_emp()$mat2)) &
                     grepl(input$SNPCall, names(datas_poly_emp()$mat2)) & 
                     grepl(sapply(strsplit(input$ErrorProb, "0"), "[[",1), names(datas_poly_emp()$mat2)))
      
      idx2 <- which(grepl(input$CountsFrom, names(datas_poly_emp()$map)) &
                      grepl(input$SNPCall, names(datas_poly_emp()$map)) & 
                      grepl(input$ErrorProb, names(datas_poly_emp()$map)))
      
      seq <- datas_poly_emp()$map[[idx2]][[1]]
      mat <- datas_poly_emp()$mat2[[idx]]
      dat <- datas_poly_emp()$dat[[idx]]
      
      list(dat, mat, seq, input$ErrorProb)
    })
  })
  
  output$dat_out <- renderPlot({
    mappoly:::plot.mappoly.data(button1()[[1]])
  })
  
  output$rf_out <- renderPlot({
    mappoly:::plot.mappoly.rf.matrix(button1()[[2]], ord = button1()[[3]]$info$mrk.names)
  })
  
  button2 <- eventReactive(input$go2, {
    withProgress(message = 'Building draw', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      stop_bam(input$CountsFrom, input$ErrorProb)
      
      idx <- which(grepl(input$CountsFrom, names(datas_poly_emp()$map)) &
                     grepl(input$SNPCall, names(datas_poly_emp()$map)) & 
                     grepl(paste0(input$ErrorProb, "_"), names(datas_poly_emp()$map)))
      
      data <- datas_poly_emp()$map[[idx]][[1]]
      
      outfile <- tempfile(pattern="file", tmpdir = tempdir(), fileext = ".png")
      list(data, outfile)
    })
  })
  
  output$map_out <- renderImage({
    
    png(button2()[[2]])
    mappoly:::plot.mappoly.map(button2()[[1]])
    dev.off()
    
    print(button2()[[2]])
    list(src = button2()[[2]],
         contentType = 'image/png')
  }, deleteFile = TRUE)
}

## To be copied in the UI
# mod_dat_poly_ui("dat_poly_ui_1")

## To be copied in the server
# mod_dat_poly_server("dat_poly_ui_1")
