#' simu_genotype_probabilities UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_genotype_probabilities_ui <- function(id){
  ns <- NS(id)
  tagList(
    "Genotype probabilities",
    fluidRow(
      column(width=6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = maps_choice,
                                    selected = unlist(maps_choice)),
                 hr()
               ),
               #helpText("Select the SNP calling method"),
               fluidPage(
                 checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                    choices = SNPCall_choice,
                                    selected = unlist(SNPCall_choice)),
               )
             )
      ),
      column(width=6,
             box(width = NULL, solidHeader = TRUE,
                 #helpText("Select the family seed"),
                 fluidPage(
                   checkboxGroupInput(ns("depth"), label = p("Depth"),
                                      choices = "It will be updated",
                                      selected = "It will be updated"),
                   hr()
                 ),
                 
                 #helpText("Read counts from:"),
                 fluidPage(
                   
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = unlist(CountsFrom_choice)),
                   hr(),
                   actionButton(ns("go"), "Update",icon("refresh")),
                   # ),
                 )
             )
      ), br(),
      column(width=12,
             box(width=NULL, solidHeader = TRUE,
                 tags$strong("Progeny genotypes"), br(),
                 "We can evaluate the estimated progenies genotypes concordance by comparing the agreement between
                 real and estimated heterozygous, reference allele homozygous (homozygous-ref), and alternative
                 allele homozygous (homozygous-alt). For that, we can use conditional probabilities: P (Estimate =
                 E|M ethod = M ∩ Real = R). It returns the probability of an estimated genotype given a method and
                 a real genotype. The methods are the combination of each SNP caller, genotype caller, and reads count
                 source. We expect that a good method results in high probabilities for the same estimated and real genotypes
                 (e.g. P (E = heterozygous|M ∩ R = hetetozygous)) and low probabilities when they are different
                 (e.g. P (E = heterozygous|M ∩ S = homozygous − alt))."),
             hr(),
             
             box(title = "Progeny  error and conditional genotypes probabilities",
                 width = NULL, solidHeader = TRUE, collapsible = TRUE, status="primary",
                 plotOutput(ns("probs_prog_out"), width = "100%", height = "100%"),
             )
      ),
      column(width = 12,
             box(width=NULL, solidHeader = TRUE,
                 tags$strong("Parents genotypes"), br(),
                 "To test the capabilities of software correctly estimating the parents’ genotypes, we can use the
                 same conditional probability, but, instead of measure the similarities between individuals genotypes, we
                 test the combination of both parents genotypes. To do that we perform the conditional probabilities
                 analysis between the marker types (e.g. P (E = B3.7|M ∩ R = B3.7))"),
             hr(),
             box(title = "Parents conditional genotypes probabilities - only biallelic markers.",
                 width = NULL, solidHeader = TRUE, collapsible = TRUE, status="primary",
                 plotOutput(ns("probs_pare_bi_out"), width = "100%", height = "100%"),
             ),
             box(title = "Parents conditional genotypes probabilities - only multiallelic markers.",
                 width = NULL, solidHeader = TRUE, collapsible = TRUE, status="primary",
                 plotOutput(ns("probs_pare_multi_out"), width = "100%", height = "100%"),
             )
      )
    )
  )
}
    
#' simu_genotype_probabilities Server Functions
#'
#' @noRd 
mod_simu_genotype_probabilities_server <- function(input, output, session, datas_simu){
    ns <- session$ns
 
    observe({
      depth_choice <- unique(datas_simu()[[7]][[4]])
      seeds_choice <- datas_simu()[[7]][[3]]
      
      updateCheckboxGroupInput(session, "depth",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
    })

    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        ## parents
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% input$ErrorProb) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          filter(depth %in% input$depth) %>%
          filter(fake == "without-false")
        
        data <- perfumaria(data)
        
        probs_plot <- list()
        data$method <- paste0(data$seed,"_", data$depth,"_",
                              data$SNPCall, "_",data$CountsFrom,"_",
                              data$GenoCall)
        
        data <- mutate_if(data, is.character, as.factor)
        
        real.mks.sele <-c("B3.7", "D1.10", "D2.15")
        for(i in 1:2){
          if(i == 1)
            data_prob <- data %>% filter(type %in% real.mks.sele)
          else data_prob <- data %>% filter(!(type %in% real.mks.sele))
          
          data_prob <- droplevels(data_prob)
          if(dim(data_prob)[1] == 0){
            probs_plot[[i]] <- 0
          } else {
            probs <- prob_f(data=data_prob, method = "method", estimate = "type", gabarito = "real.type")
            
            probs_df <- plyr::adply(probs, c(1,2,3))
            probs_df$X3 <- as.character(probs_df$X3)
            
            split_meth <- strsplit(probs_df$X3, "_")
            
            probs_plot[[i]] <- data.frame(est = probs_df$X1, simu = probs_df$X2,
                                          seed = sapply(split_meth, "[",1),
                                          depth = sapply(split_meth, "[",2),
                                          SNPCall = sapply(split_meth, "[",3),
                                          CountsFrom = sapply(split_meth, "[",4),
                                          GenoCall = sapply(split_meth, "[",5),
                                          prob = probs_df$V1)
            
            probs_plot[[i]]$GenoCall <- as.character(probs_plot[[i]]$GenoCall)
            probs_plot[[i]]$GenoCall[probs_plot[[i]]$GenoCall == "SNPCallerdefault"] <- "OneMap_version2"
          }
        }
        
        incProgress(0.25, detail = paste("Doing part", 2))
        # progeny
        data <- datas_simu()[[1]] %>% filter(GenoCall %in% input$ErrorProb) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom) %>%
          filter(depth %in% input$depth)
        
        data <- perfumaria(data)
        
        data$method <- paste0(data$seed,"_", data$depth,"_",
                              data$SNPCall, "_",data$CountsFrom,"_",
                              data$GenoCall)
        
        data <- mutate_if(data, is.character, as.factor)
        
        probs <- prob_f(data=data, method = "method", estimate = "gt.onemap.alt.ref", gabarito = "gabGT")
        
        probs_df <- plyr::adply(probs, c(1,2,3))
        probs_df$X3 <- as.character(probs_df$X3)
        
        split_meth <- strsplit(probs_df$X3, "_")
        
        data_plot <- data.frame(est = probs_df$X1, simu = probs_df$X2,
                                seed = sapply(split_meth, "[",1),
                                depth = sapply(split_meth, "[",2),
                                SNPCall = sapply(split_meth, "[",3),
                                CountsFrom = sapply(split_meth, "[",4),
                                GenoCall = sapply(split_meth, "[",5),
                                prob = probs_df$V1)
        
        probs_error <- prob_error(data=data, method = "method",
                                  estimate = "gt.onemap.alt.ref", gabarito = "gabGT", error = "errors")
        
        test <- plyr::adply(probs_error, c(1,2,3))
        test$X3 <- as.character(test$X3)
        
        split_meth <- strsplit(test$X3, "_")
        
        data_plot_error <- data.frame(est = test$X1, simu = test$X2,
                                      seed = sapply(split_meth, "[",1),
                                      depth = sapply(split_meth, "[",2),
                                      SNPCall = sapply(split_meth, "[",3),
                                      CountsFrom = sapply(split_meth, "[",4),
                                      GenoCall = sapply(split_meth, "[",5),
                                      error = test$V1)
        
        data_tot <- inner_join(data_plot, data_plot_error)
        incProgress(0.5, detail = paste("Doing part", 3))
        list(probs_plot[[1]], probs_plot[[2]], data_tot)
      })
    })
    
    output$probs_pare_bi_out <- renderPlot({
      marker_type_probs(data_plot_par = button()[[1]])
    }, width = 1152, height = 1536)
    
    output$probs_pare_multi_out <- renderPlot({
      marker_type_probs_multi(button()[[2]])
    }, width = 1152, height = 1536)
    
    output$probs_prog_out <- renderPlot({
      geno_probs(button()[[3]])
    }, width = 1152, height = 1536)
}
    
## To be copied in the UI
# mod_simu_genotype_probabilities_ui("simu_genotype_probabilities_ui_1")
    
## To be copied in the server
# mod_simu_genotype_probabilities_server("simu_genotype_probabilities_ui_1")
