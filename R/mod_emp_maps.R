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
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE)),
             ),
             box(solidHeader = T,
                 radioButtons(ns("ErrorProb"), label = p("Genotyping method"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
                 
                 radioButtons(ns("SNPCall"), label = p("SNP calling method"),
                              choices = "This will be updated",
                              selected = "This will be updated"),
             ),
             box(solidHeader = T,
                 radioButtons(ns("CountsFrom"), label = p("Counts from"),
                              choices = "This will be updated",
                              selected = "This will be updated"), hr(),
                 div(downloadButton(ns("map_emp_out_down"), label = "Download sequence"),style="float:right"), br(), br(),
                 div(downloadButton(ns("map_emp_onemap_down"), label = "Download PDF with all onemap heatmaps"),style="float:right")
             )
      ),
      column(width = 4,
             box(title = "Genetic map",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 actionButton(ns("go2"), "Update",icon("refresh", verify_fa = FALSE)),
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
  
  observe({
    if(datas_emp()$software == "onemap"){
      SNPCall_choice <- as.list(unique(datas_emp()[[2]]$SNPCall))
      names(SNPCall_choice) <- unique(datas_emp()[[2]]$SNPCall)
      methods <- unique(datas_emp()[[2]]$GenoCall)
      
      ErrorProb_choice <- as.list(methods)
      names(ErrorProb_choice) <- gsub("default", "_OneMap2.0", methods)
      CountsFrom_choice <- as.list(unique(datas_emp()[[2]]$CountsFrom))
      names(CountsFrom_choice) <- unique(datas_emp()[[2]]$CountsFrom)
    } else {
      file_names <- strsplit(names(result_list[[1]]), "_")
      SNPCall_choice <- as.list(unique(sapply(file_names, "[[", 1)))
      names(SNPCall_choice) <- unique(sapply(file_names, "[[", 1))
      
      ErrorProb_choice <- as.list(unique(sapply(file_names, "[[",2)))
      names(ErrorProb_choice) <- unique(sapply(file_names, "[[",2))
      
      CountsFrom_choice <- as.list(unique(sapply(file_names, "[[",3)))
      names(CountsFrom_choice) <- unique(sapply(file_names, "[[",3))
    }
    
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
  
  button2 <- eventReactive(input$go2, {
    withProgress(message = 'Building draw', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      stop_bam(input$CountsFrom, input$ErrorProb)
      
      if(datas_emp()$software == "onemap"){
        incProgress(0.25, detail = paste("Doing part", 2))
        print(head(datas_emp()[[2]]))

        data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom == input$CountsFrom)
        
        incProgress(0.5, detail = paste("Doing part", 3))
        data <-   data.frame(data$mks, data$rf)
      } else {
        idx <- which(grepl(input$CountsFrom, names(datas_emp()$maps)) &
                       grepl(input$SNPCall, names(datas_emp()$maps)) & 
                       grepl(input$ErrorProb, names(datas_emp()$maps)))
        data <- datas_emp()$maps[[idx]][[1]]
      }
      outfile <- tempfile(pattern="file", tmpdir = tempdir(), fileext = ".png")
      print(outfile)
      list(data, outfile)
    })
  })
  
  output$map_emp_out <- renderImage({
    if(datas_emp()$software == "onemap"){
      draw_map2(button2()[[1]], output = button2()[[2]], col.tag = "darkblue", pos = T, id = F)
    } else {
      png(button2()[[2]])
      mappoly:::plot.mappoly.map(button2()[[1]])
      dev.off()
    }
    print(button2()[[2]])
    list(src = button2()[[2]],
         contentType = 'image/png',
         width = 200,
         height = 900)
  }, deleteFile = TRUE)
  
  button1 <- eventReactive(input$go1, {
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      stop_bam(input$CountsFrom, input$ErrorProb)
      
      if(datas_emp()$software == "onemap"){
        temp_n <- paste0("map_",input$SNPCall, "_", input$CountsFrom, "_", input$ErrorProb, ".rds")
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(input$ErrorProb == "gusmap"){
          list(data = NULL, input$ErrorProb)
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- datas_emp()[[8]][[idx]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
        }
      } else {
        idx <- which(grepl(input$CountsFrom, names(result_list$mat2)) &
                       grepl(input$SNPCall, names(result_list$mat2)) & 
                       grepl(input$ErrorProb, names(result_list$mat2)))
        
        idx2 <- which(grepl(input$CountsFrom, names(result_list$maps)) &
                       grepl(input$SNPCall, names(result_list$maps)) & 
                       grepl(input$ErrorProb, names(result_list$maps)))
        
        seq <- result_list$map_error[[idx2]][[1]]
        data <- result_list$mat2[[idx]]
      }
      list(data, input$ErrorProb, seq)
    })
  })
  
  output$map1_emp_out <- renderPlot({
    if(datas_emp()$software == "onemap"){
      if(button1()[[2]] == "gusmap"){
        stop("The app no longer support GUSMap recombination fraction data, once the package is not available in CRAN anymore. 
           Please, install the package from GitHub and visualize the recombination fraction matrix using its functions and the file gusmap_RDatas.RData contained in the Reads2Map tar.gz results.")
      } else {
        rf_graph_table(button1()[[1]], inter = F, mrk.axis = "none")
      }
    } else {
      mappoly:::plot.mappoly.rf.matrix(button1()[[1]], ord = button1()[[3]])
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
        
        stop_bam(input$CountsFrom, input$ErrorProb)
        temp_n <- paste0("map_",input$SNPCall, "_", input$CountsFrom, "_", input$ErrorProb, ".RData")
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(input$ErrorProb == "gusmap"){
          data <- NULL
          save(data, file=file)
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- datas_emp()[[8]][[idx]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
          save(data, file=file)
        }
      })
    }
  )
  
  ## download all onemap heatmaps
  output$map_emp_onemap_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        idx.0.05 <- grep("0.", datas_emp()[[6]])
        idx.rest <- c(1:length(datas_emp()[[6]]))[-idx.0.05]
        
        part_plus <- 1/(length(datas_emp()[[6]][-idx.0.05]))
        
        part <- 0
        part.n <- 2
        p <- list()
        for(i in 1:length(idx.rest)){
          part <- part + part_plus
          part.n <- part.n + 1
          data <- datas_emp()[[8]][[idx]]
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
