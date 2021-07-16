#' emp_maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_maps_ui <- function(id){
  ns <- NS(id)
  tagList(
    "On the left, it is plotted the heatmap graphic with recombination fraction varying in color according with the
            intensity of the linkage. Higher is the recombination fraction hottest is the color. Markers ordered by genome positions are represented in x
            and y axis. On the right, the linkage group is plotted with distances between markers proportional to the genetic distances. Pressing the
            'Download' button, it will download the RData sequenced resulted from the selected 'Genotype method', 'SNP calling method', 'Error rate' and 'Counts from'.",
    hr(),
    fluidRow(
      column(width = 8,
             box(title = "Recombination fraction heatmap",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("map1_emp_out")),hr(),
                 actionButton(ns("go1"), "Update",icon("refresh")),
             ),
             box(solidHeader = T,
                 radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                              choices = maps_choice,
                              selected = "updog"),
             ),
             box(solidHeader = T,
                 radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                              choices = SNPCall_choice,
                              selected = "gatk"),
             ),
             box(solidHeader = T,
                 radioButtons(ns("Global0.05"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
             ),
             box(solidHeader = T,
                 radioButtons(ns("CountsFrom"), label = p("Counts from"),
                              choices = CountsFrom_choice,
                              selected = "vcf"),
                 div(downloadButton(ns("map_emp_out_down")),style="float:right")
             )
      ),
      column(width = 4,
             box(title = "Genetic map",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 actionButton(ns("go2"), "Update",icon("refresh")),
                 imageOutput(ns("map_emp_out"), width = "100%", height = "100%")
             )
      )
    )
  )
}

#' emp_maps Server Functions
#'
#' @noRd 
mod_emp_maps_server <- function(input, output, session, datas_emp){
  ns <- session$ns
  button2 <- eventReactive(input$go2, {
    withProgress(message = 'Building draw', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      if(input$Global0.05){
        if(input$ErrorProb == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb == "gusmap"){
          stop(safeError("Gusmap do not build plotly heatmaps.
                           Please, select other option."))
        } else {
          geno <- paste0(input$ErrorProb, 0.05)
        }
      } else {
        geno <- input$ErrorProb
      }
      
      if(input$CountsFrom == "bam" & (input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller")){
        stop(safeError("This option is not available.
                         The SNP callers performs together the SNP and genotype calling using
                         the same read counts, we did not find a way to substitute the depths
                         already used. Please select other option."))
      }
      
      incProgress(0.25, detail = paste("Doing part", 2))
      data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom == input$CountsFrom)
      
      incProgress(0.5, detail = paste("Doing part", 3))
      data <-   data.frame(data$mks, data$cm)
      outfile <- tempfile(pattern="file", tmpdir = tempdir(), fileext = ".png")
      list(data, outfile)
    })
  })
  
  output$map_emp_out <- renderImage({
    draw_map2(button2()[[1]], output = button2()[[2]], col.tag = "darkblue", pos = T, id = F)
    
    list(src = button2()[[2]],
         contentType = 'image/png',
         width = 200,
         height = 900)
  }, deleteFile = TRUE)
  
  button1 <- eventReactive(input$go1, {
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05){
        if(input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller"){
          geno <- paste0("default", 0.05)
        } else if (input$ErrorProb == "gusmap"){
          stop(safeError("Gusmap do not allow to change the error rate.
                           Please, select other option."))
        } else {
          geno <- paste0(input$ErrorProb, 0.05)
        }
      } else {
        if(input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller"){
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
      
      incProgress(0.25, detail = paste("Doing part", 2))
      if(geno == "gusmap"){
        data <- datas_emp()[[5]][[temp_n]]
        data$rf_2pt()
        incProgress(0.5, detail = paste("Doing part", 3))
        list(data, geno)
      } else {
        idx <- which(datas_emp()[[6]] == temp_n)
        data <- readList(datas_emp()[[8]], index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        incProgress(0.5, detail = paste("Doing part", 3))
        list(data, geno)
      }
    })
  })
  
  output$map1_emp_out <- renderPlot({
    if(button1()[[2]] == "gusmap"){
      button1()[[1]]$plotChr(mat="rf", parent = "both")
    } else {
      rf_graph_table(button1()[[1]], inter = F, mrk.axis = "none")
    }
  })
  
  ## download
  output$map_emp_out_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RData")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05){
          if(input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb == "gusmap"){
            stop(safeError("Gusmap do not allow to change the error rate.
                             Please, select other option."))
          } else {
            geno <- paste0(input$ErrorProb, 0.05)
          }
        } else {
          if(input$ErrorProb == "OneMap_version2" | input$ErrorProb == "SNPCaller"){
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
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          data <- datas_emp()[[5]][[temp_n]]
          data$rf_2pt()
          incProgress(0.5, detail = paste("Doing part", 3))
          save(data, file=file)
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- readList(datas_emp()[[8]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
          save(data, file=file)
        }
      })
    }
  )
}

## To be copied in the UI
# mod_emp_maps_ui("emp_maps_ui_1")

## To be copied in the server
# mod_emp_maps_server("emp_maps_ui_1")
