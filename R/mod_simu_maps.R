#' simu_maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_maps_ui <- function(id){
  ns <- NS(id)
  tagList(
    "On the left, it is plotted the heatmap graphic with recombination fraction varying in color according with the
            intensity of the linkage. Higher is the recombination fraction hottest is the color. Markers ordered by genome positions are represented in x
            and y axis. On the right, the linkage group is plotted with distances between markers proportional to the genetic distances.",
    hr(),
    fluidRow(
      column(width = 8,
             box(title = "Recombination fraction heatmap",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("map_out")),
                 hr(),
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE)),
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
                 radioButtons(ns("CountsFrom"), label = p("Counts from"),
                              choices = CountsFrom_choice,
                              selected = "vcf"),
                 div(downloadButton(ns("map1_out_down")),style="float:right"),
             ),
      ),
      
      column(width = 4,
             box(title = "Genetic map",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
                 hr(),
                 imageOutput(ns("map1_out"), width = "100%", height = "100%")
             )
      )
    )
  )
}
    
#' simu_maps Server Functions
#'
#' @import onemap
#' @import GUSMap
#' 
#' @noRd 
mod_simu_maps_server <- function(input, output, session, datas_simu){
    ns <- session$ns
    
    observe({
      seeds_choice <- datas_simu()[[7]][[3]]
      
      updateSelectInput(session, "seed",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
    })
    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building draw', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        stop_bam(input$CountsFrom, input$ErrorProb)
        
        incProgress(0.25, detail = paste("Doing part", 2))
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed)]) %>%
          filter(CountsFrom == input$CountsFrom) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed)]) %>%
          filter(fake == input$fake)
        
        temp_path <- tempdir()
        filename <- paste0(temp_path, "/", data$SNPCall, "_", data$CountsFrom, "_", geno, ".png")
        filename <- unique(filename)
        
        incProgress(0.5, detail = paste("Doing part", 3))
        if(input$fake == "with-false"){
          false_mks <- as.character(data$mk.name[data$real.mks == "false positive"])
          data <- data.frame(data$mk.name, data$rf)
        } else {
          false_mks = NULL
          data <- data.frame(data$mk.name, data$rf)
        }
        list(data,filename, false_mks)
      })
    })
    
    output$map1_out <- renderImage({
      if(input$fake == "with-false"){
        draw_map2(button()[[1]], output = button()[[2]],
                  tag = button()[[3]], col.tag = "darkblue",
                  pos = T, id = F)
      } else {
        draw_map2(button()[[1]], output = button()[[2]])
      }
      
      list(src = button()[[2]],
           contentType = 'image/png',
           width = 200,
           height = 900)
    }, deleteFile = TRUE) # change here
    
    button1 <- eventReactive(input$go1, {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05, input$ErrorProb)
        stop_bam(input$CountsFrom, input$ErrorProb)
        
        depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed)])
        seed <- datas_simu()[[7]][[2]][as.numeric(input$seed)]
        if(input$fake == "with-false") fake <- T else fake <- F
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          temp_n <- map_name_gus(geno, seed, depth, fake,
                                 snpcall = input$SNPCall, countsfrom = input$CountsFrom,
                                 data_names = names(datas_simu()[[6]]))
          data <- datas_simu()[[6]][[temp_n]]
          data$rf_2pt()
          incProgress(0.5, detail = paste("Doing part", 3))
        } else {
          temp_n <- map_name(depth, seed, geno, fake,
                             snpcall = input$SNPCall, countsfrom = input$CountsFrom,
                             data_names = datas_simu()[[8]])
          idx <- which(datas_simu()[[8]] == temp_n)
          data <- largeList::readList(datas_simu()[[10]], index = idx)
          if (!is.vector(data[[1]][[1]])) data <- data[[1]][[1]] else data <- data[[1]] # bugfix
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
        }
        list(data, geno)
      })
    })
    
    output$map_out <- renderPlot({
      if(button1()[[2]] == "gusmap"){
        button1()[[1]]$plotChr(mat="rf", parent = "both")
      } else {
        rf_graph_table(button1()[[1]], inter = F, mrk.axis = "none")
      }
    })
    
    ## download
    output$map1_out_down <- downloadHandler(
      filename =  function() {
        tempfile("file",tmpdir = tempdir(), fileext = ".RData")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Loading data', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05, input$ErrorProb)
          stop_bam(input$CountsFrom, input$ErrorProb)
          
          depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed)])
          seed <- datas_simu()[[7]][[2]][as.numeric(input$seed)]
          if(input$fake == "with-false") fake <- T else fake <- F
          
          incProgress(0.25, detail = paste("Doing part", 2))
          if(geno == "gusmap"){
            temp_n <- map_name_gus(geno, fake,
                                   snpcall = input$SNPCall, countsfrom = input$CountsFrom,
                                   data_names = names(datas_simu()[[6]]))
            data <- datas_simu()[[6]][[temp_n]]
            data$rf_2pt()
            incProgress(0.5, detail = paste("Doing part", 3))
            save(data, file = file)
          } else {
            temp_n <- map_name(depth, seed, geno, fake,
                               snpcall = input$SNPCall, countsfrom = input$CountsFrom,
                               data_names = datas_simu()[[8]])
            idx <- which(datas_simu()[[8]] == temp_n)
            data <- largeList::readList(datas_simu()[[10]], index = idx)
            if (!is.vector(data[[1]][[1]])) data <- data[[1]][[1]] else data <- data[[1]] # bugfix
            class(data) <- "sequence"
            incProgress(0.5, detail = paste("Doing part", 3))
            save(data, file = file)
          }
        })
      }
    )
}
    
## To be copied in the UI
# mod_simu_maps_ui("simu_maps_ui_1")
    
## To be copied in the server
# mod_simu_maps_server("simu_maps_ui_1")
