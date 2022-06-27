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
                 
                 radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                              choices = SNPCall_choice,
                              selected = "gatk"),
             ),
             box(solidHeader = T,
                 radioButtons(ns("Global0.05"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
                 
                 radioButtons(ns("CountsFrom"), label = p("Counts from"),
                              choices = CountsFrom_choice,
                              selected = "vcf"), hr(),
                 div(downloadButton(ns("map_emp_out_down"), label = "Download sequence"),style="float:right"), br(), br(),
                 div(downloadButton(ns("map_emp_onemap_down"), label = "Download PDF with all onemap heatmaps"),style="float:right")
             )
      ),
      column(width = 4,
             box(title = "Genetic map",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 actionButton(ns("go2"), "Update",icon("refresh")),
                 imageOutput(ns("map_emp_out"), width = "100%", height = "100%"),
             )
      )
    )
  )
}

#' emp_maps Server Functions
#' 
#' @importFrom ggpubr ggarrange
#' @import onemap
#' @import GUSMap
#' @importFrom grDevices dev.off pdf rainbow
#'
#' @noRd 
mod_emp_maps_server <- function(input, output, session, datas_emp){
  ns <- session$ns
  button2 <- eventReactive(input$go2, {
    withProgress(message = 'Building draw', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      if(input$Global0.05){
        if(input$ErrorProb == "SNPCallerdefault"){
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
      
      stop_bam(input$CountsFrom, input$ErrorProb)
      
      incProgress(0.25, detail = paste("Doing part", 2))
      data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom == input$CountsFrom)
      
      incProgress(0.5, detail = paste("Doing part", 3))
      data <-   data.frame(data$mks, data$rf)
      outfile <- tempfile(pattern="file", tmpdir = tempdir(), fileext = ".png")
      print(outfile)
      list(data, outfile)
    })
  })
  
  output$map_emp_out <- renderImage({
    draw_map2(button2()[[1]], output = button2()[[2]], col.tag = "darkblue", pos = T, id = F)
    print(button2()[[2]])
    list(src = button2()[[2]],
         contentType = 'image/png',
         width = 200,
         height = 900)
  }, deleteFile = TRUE)
  
  button1 <- eventReactive(input$go1, {
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      geno <- test_geno_emp(input$Global0.05, input$ErrorProb)
      stop_bam(input$CountsFrom, input$ErrorProb)
      
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
  
  ## download sequence
  output$map_emp_out_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".RData")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno_emp(input$Global0.05, input$ErrorProb)
        stop_bam(input$CountsFrom, input$ErrorProb)
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
  
  # with bug!!
  # ## download all gusmap heatmaps
  # output$map_emp_gusmap_down <- downloadHandler(
  #   filename =  function() {
  #     tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".pdf")
  #   },
  #   # content is a function with argument file. content writes the plot to the device
  #   content = function(file) {
  #     withProgress(message = 'Building heatmap', value = 0, {
  #       incProgress(0, detail = paste("Doing part", 1))
  #       
  #       part_plus <- 1/(length(datas_emp()[[5]]))
  #       part <- 0
  #       part.n <- 2
  #       pdf(file = file)
  #       for(i in 1:length(datas_emp()[[5]])){
  #         part <- part + part_plus
  #         part.n <- part.n + 1
  #         data <- datas_emp()[[5]][[i]]
  #         data$rf_2pt()
  #         data$plotChr(mat="rf", parent = "both")
  #         #text(x=0.9, y=.1,names(datas_emp()[[5]])[i])
  #         incProgress(part, detail = paste("Doing part", part.n))
  #       } 
  #       dev.off()
  #     })
  #   }
  # )
  
  ## download all onemap heatmaps
  output$map_emp_onemap_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        idx.0.05 <- grep("0.05", datas_emp()[[6]])
        idx.rest <- c(1:length(datas_emp()[[6]]))[-idx.0.05]
        
        part_plus <- 1/(length(datas_emp()[[6]][-idx.0.05]))
    
        part <- 0
        part.n <- 2
        p <- list()
        for(i in 1:length(idx.rest)){
          part <- part + part_plus
          part.n <- part.n + 1
          data <- readList(datas_emp()[[8]], index = idx.rest[i])
          data <- data[[1]]
          class(data) <- "sequence"
          name <- strsplit(datas_emp()[[6]][idx.rest[i]], "[.]")
          name <- paste0(name[[1]][1:(length(name[[1]])-1)],collapse = ".")
          
          p[[i]] <- rf_graph_table(data, inter = F, mrk.axis = "none",main = name)
          print(part)
          incProgress(part, detail = paste("Doing part", part.n))
        }
        
        int <- sort(unique(c(seq(0,length(p),6), length(p))))
        
        pdf(file = file, onefile = T)
        for(i in 2:length(int)){
          print(ggarrange(plotlist = p[(int[i-1]+1):int[i]], common.legend = T, ncol = 2, nrow = 3))
        }
        dev.off()
      })
    }
  )
}

## To be copied in the UI
# mod_emp_maps_ui("emp_maps_ui_1")

## To be copied in the server
# mod_emp_maps_server("emp_maps_ui_1")
