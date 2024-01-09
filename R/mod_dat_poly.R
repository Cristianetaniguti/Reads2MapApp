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
                 div(downloadButton(ns("dats_down"), label = "Download as PDF image"),style="float:right"),
                 actionButton(ns("go1"), "Update",icon("refresh", verify_fa = FALSE)),
                 plotOutput(ns("dat_out"))
             ),
             box(title = "Recombination fraction heatmap",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 div(downloadButton(ns("rf_down"), label = "Download as PDF image"),style="float:right"),
                 plotOutput(ns("rf_out"), width = "600px", height = "600px"),hr(),
             ),
             box(title = "Select markers",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 sliderInput(ns("interval"), label = "Select map size interval", min = 10, max = 200, value = c(90,120)), br(),
                 radioButtons(ns("prob"), label = h3("Radio buttons"),
                              choices = list("5% global error" = "error", "probabilities" = 2), 
                              selected = "error"),
                 actionButton(ns("go2"), "Update",icon("refresh", verify_fa = FALSE)), br(),
                 textOutput(ns("selected")), br(),
                 div(downloadButton(ns("selected_down"), 
                                    label = "Download the list of selected markers IDs"),style="float:right")
             ),
             box(title = "Build map",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 p("Building the map will take a while, make sure you selected your best pipeline and interval."), br(),
                 actionButton(ns("go3"), "Update",icon("refresh", verify_fa = FALSE)),
                 plotOutput(ns("built_map"),height = "600px"), br(),
                 plotOutput(ns("rf_built_map"), width = "600px", height = "600px"),
                 div(downloadButton(ns("datas_down"), 
                                    label = "Download a list object with \n selected dataset (first list level) 
                                    \n and built map (second list level)"),style="float:right")
             ), hr()
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
    file_names <- strsplit(names(datas_poly_emp()[[1]]), "_")
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
      
      if(input$SNPCall %in% c("stacks", "tassel") & grepl("SNPCaller", input$ErrorProb)) 
        stop("STACKs and TASSEL do not provide polyploid genotype calls.")
      
      if(!(input$SNPCall %in% c("gatk")) & input$ErrorProb %in% "SNPCaller") 
        stop("MAPpoly current version only reads genotype probabilities from GATK SNPCaller vcf.")
      
      idx <- which(grepl(input$CountsFrom, names(datas_poly_emp()$mat2)) &
                     grepl(input$SNPCall, names(datas_poly_emp()$mat2)) & 
                     grepl(sapply(strsplit(input$ErrorProb, "0"), "[[",1), names(datas_poly_emp()$mat2)))
      
      idx1 <- which(grepl(input$CountsFrom, names(datas_poly_emp()$dat)) &
                      grepl(input$SNPCall, names(datas_poly_emp()$dat)) & 
                      grepl(sapply(strsplit(input$ErrorProb, "0"), "[[",1), names(datas_poly_emp()$dat)))
      
      idx2 <- which(grepl(input$CountsFrom, names(datas_poly_emp()$maps)) &
                      grepl(input$SNPCall, names(datas_poly_emp()$maps)) & 
                      grepl(paste0(input$ErrorProb, "_"), names(datas_poly_emp()$maps)))
      
      cat(paste("map:", names(datas_poly_emp()$maps)[idx2], "\n"))
      seq <- datas_poly_emp()$map[[idx2]]
      
      cat(idx, "\n")
      cat(paste("mat:", names(datas_poly_emp()$mat2)[idx], "\n"))
      mat <- datas_poly_emp()$mat2[[idx]]
      
      cat(paste("dat:", names(datas_poly_emp()$dat)[idx1], "\n"))
      dat <<- datas_poly_emp()$dat[[idx1]]
      
      summary <- datas_poly_emp()$summaries[[which(names(datas_poly_emp()$summaries) %in% names(datas_poly_emp()$dat)[idx1])]]
      info <- datas_poly_emp()$info[[which(names(datas_poly_emp()$info) %in% names(datas_poly_emp()$dat)[idx1])]]
      
      list(dat, mat, seq, summary, info, input$ErrorProb)
    })
  })
  
  output$dat_out <- renderPlot({
    mappoly:::plot.mappoly.data(button1()[[1]])
  })
  
  output$rf_out <- renderPlot({
    mappoly:::plot.mappoly.rf.matrix(button1()[[2]], ord = button1()[[3]]$info$mrk.names)
  })
  
  button2 <- eventReactive(input$go2, {
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      df <- button1()[[4]]
      df$`Map length (cM)` <- as.numeric(button1()[[4]]$`Map length (cM)`)
      summary_sub <- df %>% filter(map == paste0(input$prob, ".p1"))
      idx.p1 <- which(summary_sub$`Map length (cM)` >= input$interval[1] & 
                        summary_sub$`Map length (cM)` <= input$interval[2])
      
      summary_sub <- df %>% filter(map == paste0(input$prob, ".p2"))
      idx.p2 <- which(summary_sub$`Map length (cM)` >= input$interval[1] & 
                        summary_sub$`Map length (cM)` <= input$interval[2])
      
      incProgress(0.5, detail = paste("Doing part", 2))
      if(input$prob == "error"){
        selec.p1 <- button1()[[3]]$map.err.p1[idx.p1]
        selec.p1 <- unlist(sapply(selec.p1, function(x) x$info$mrk.names))
        selec.p1 <- unique(selec.p1)
        
        selec.p2 <- button1()[[3]]$map.err.p2[idx.p2]
        selec.p2 <- unlist(sapply(selec.p2, function(x) x$info$mrk.names))
        selec.p2 <- unique(selec.p2)
        select.mks <- unique(c(selec.p1, selec.p2))
        
      } else {
        selec.p1 <- button1()[[3]]$map.prob.p1[idx.p1]
        selec.p1 <- unlist(sapply(selec.p1, function(x) x$info$mrk.names))
        selec.p1 <- unique(selec.p1)
        
        selec.p2 <- button1()[[3]]$map.prob.p2[idx.p2]
        selec.p2 <- unlist(sapply(selec.p2, function(x) x$info$mrk.names))
        selec.p2 <- unique(selec.p2)
        select.mks <- unique(selec.p1, selec.p2)
      }
      
      pos <- button1()[[1]]$genome.pos[match(select.mks, button1()[[1]]$mrk.names)]
      select.mks <- select.mks[order(pos)]
      select.mks
    })
  })
  
  output$selected <- renderText({ 
    paste0("Number of selected markers:", 
           length(button2()))
  })
  
  output$selected_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".csv")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      
      datas_lst <- data.frame(selected_markers = button2())
      write.csv(datas_lst, file = file)
      
    }
  )
  
  build_map <- eventReactive(input$go3, {
    withProgress(message = 'Building map', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      dat <<- button1()[[1]]
      seq_dat <- make_seq_mappoly(dat, button2())
      
      tpt <- est_pairwise_rf(seq_dat, ncpus = 1)
      incProgress(0.3, detail = paste("Doing part", 2))
      
      map <- est_rf_hmm_sequential(input.seq = seq_dat,
                                   start.set = 5,
                                   thres.twopt = 10,
                                   thres.hmm = 10,
                                   extend.tail = 30,
                                   info.tail = TRUE,
                                   twopt = tpt,
                                   phase.number.limit = 10,
                                   reestimate.single.ph.configuration = TRUE,
                                   tol = 10e-2,
                                   tol.final = 10e-4, 
                                   verbose = FALSE)
      incProgress(0.8, detail = paste("Doing part", 4))
      
      map <- filter_map_at_hmm_thres(map, thres.hmm = 0.0001)
      map2 <- est_full_hmm_with_global_error(map, error = 0.05, tol = 10e-3)
      map3 <- split_and_rephase(map2, gap.threshold = 20, size.rem.cluster = 3, twopt = tpt)
      map.final <- est_full_hmm_with_global_error(map3, error = 0.05, tol = 10e-4)
      map.final
    })
  })
  
  output$built_map <- renderPlot({
    plot(build_map())  
  })
  
  output$rf_built_map <- renderPlot({
    mappoly:::plot.mappoly.rf.matrix(button1()[[2]], ord = build_map()$info$mrk.names)
  })
  
  
  ## download all 
  output$datas_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".rds")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      
      dat <- button1()[[1]]
      datas_lst <- list(dat, build_map())
      saveRDS(datas_lst, file = file)
      
    }
  )
  
  ## download all 
  output$dats_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        pdf(file = file, onefile = T)
        for(i in 1:length(datas_poly_emp()$dat)){
          
          mappoly:::plot.mappoly.data(datas_poly_emp()$dat[[i]])
          mtext(text = names(datas_poly_emp()$dat)[i], side = 4)
        }
        dev.off()
      })
    }
  )
  
  
  ## download all onemap heatmaps
  output$rf_down <- downloadHandler(
    filename =  function() {
      tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        pdf(file = file, onefile = T)
        for(i in 1:length(datas_poly_emp()$mat2)){
          idx <- match(names(datas_poly_emp()$mat2)[i], gsub("0.05", "", names(datas_poly_emp()$map)))[1]
          mappoly:::plot.mappoly.rf.matrix(datas_poly_emp()$mat2[[i]], 
                                           ord = datas_poly_emp()$map[[idx]][[1]]$info$mrk.names)
          mtext(text = names(datas_poly_emp()$mat2)[i], side = 1)
        }
        dev.off()
      })
    }
  )
}

## To be copied in the UI
# mod_dat_poly_ui("dat_poly_ui_1")

## To be copied in the server
# mod_dat_poly_server("dat_poly_ui_1")
