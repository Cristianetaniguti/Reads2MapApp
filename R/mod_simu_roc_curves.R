#' simu_roc_curves UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_simu_roc_curves_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel="stylesheet",
                href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css",
                integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
                crossorigin="anonymous"),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
      HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
      HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
    ),
    helpText("To summarize each genotype caller model predictive power we used
              receiver operating characteristic (ROC) curves.
              It plots the sensitivity ($\\frac{true\\ positives}{true\\ positives + false\\ negatives}$)
              in the vertical axis versus (1 - specificity) ($\\frac{false\ positives}{false\\ positives + true\\ negatives}$)
              on the horizontal axis for all possible thresholds $\\pi_0$ in a logistic regression (Berkson 1944):"),
    helpText("$ logit[\\pi(x)] = log[\\frac{\\pi(x)}{1-\\pi(x)}] = \\alpha + \\beta x$"),
    helpText("or"),
    helpText("$ \\pi(x) = \\frac{e^{\\alpha + \\beta x}}{1 + e^{\\alpha + \\beta x}}$"),
    helpText("
              where x is the error rate of the genotypes and the binary response variable is if they were called
              correctly or wrongly. The formula implies that $\\pi(x)$ changes as an S-shaped function of $x$.
              Parameter $\\beta$ determines the rate of increase or decrease of the S-shaped curve for $\\pi(x)$ (Sloane and Morgan 1996).
              Higher is the sensitivity value for each (1 - specificity), better is the predictive power of the selected
              error rate threshold. In other words, better this particular error rate threshold can differentiate wrong
              and correct genotypes. The best threshold would be the one more close to the left superior corner of the graphic.
              Therefore, the better the predictive power, the higher is the ROC curve. Because of this, the area under
              the curve provides a single value that summarizes predictive power. The greater the area, the better
              the predictive power of the outputted error rate from the genotype call model used (Bradley 1997)."),
    hr(),
    fluidRow(
      column(width = 12,
             box(title= "ROC curves",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("roc_out"), width = "100%", height = "500px"),
                 hr(),
                 "Best thresholds:",
                 tableOutput(ns("thr_out")), hr(),
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
      ),
      
      column(width=6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = ErrorProb_choice_unique,
                                    selected = unlist(ErrorProb_choice_unique)),
                 hr()
               ),
               fluidPage(
                 radioButtons(ns("Global0.05"), label = p("Error rate"),
                              choices = global0.05_choices,
                              selected = "FALSE"),
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
                 fluidPage(
                   checkboxGroupInput(ns("depth"), label = p("Depth"),
                                      choices = "It will be updated",
                                      selected = "It will be updated"),
                   hr()
                 ),
                 
                 fluidPage(
                   
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = unlist(CountsFrom_choice)),
                   hr()
                 ),
                 
                 fluidPage(
                   
                   radioButtons(ns("fake"), label = p("Allow false positives?"),
                                choices = fake_choices,
                                selected = "without-false")
                 )
             )
      )
    )
  )
}
    
#' simu_roc_curves Server Functions
#'
#' @noRd 
mod_simu_roc_curves_server <- function(input, output, session, datas_simu){
    ns <- session$ns
 
    observe({
      depth_choice <- unique(datas_simu()[[7]][[4]])
      
      updateCheckboxGroupInput(session, "depth",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
    })

    
    button <- eventReactive(input$go, {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        genocall <- test_geno(global = input$Global0.05, error = input$ErrorProb)
        snpcall <- input$SNPCall
        countsfrom <- input$CountsFrom
        depth <- input$depth
        
        methods <- expand.grid(genocall=genocall, snpcall=snpcall,
                               countsfrom = countsfrom, depth = depth)
        
        methods <- methods %>% filter(!(genocall == "SNPCaller" & countsfrom == "bam")) %>%
          mutate_if(is.factor, as.character)
        
        data <- datas_simu()[[1]]
        data$id <- "non"
        data[which(data$gabGT == data$gt.onemap.alt.ref),]$id <- "correct"
        data[which(data$gabGT != data$gt.onemap.alt.ref),]$id <- "wrong"
        
        incProgress(0.25, detail = paste("Doing part", 2))
        tot_df <- subset_df <- data.frame()
        for(i in 1:dim(methods)[1]){
          data1 <- data %>% filter(GenoCall == methods$genocall[i] &
                                   SNPCall == methods$snpcall[i] &
                                   CountsFrom == methods$countsfrom[i] &
                                   depth == methods$depth[i] &
                                     gt.onemap.alt.ref != "missing")
          
          if(all(data1$id == "correct")){
            df <- data.frame(threshold = Inf, specificity = 1, sensitivity=1, precision =1)
            best <- data.frame(threshold =Inf)
          } else if (all(data1$id == "wrong")){
            df <- data.frame(threshold = -Inf, specificity = 0, sensitivity=0, precision =0)
            best <- data.frame(threshold =-Inf)
          } else {
            method <- pROC::roc(response = data1$id, predictor = data1$errors)
            
            best <- pROC::coords(method, "best", ret="threshold", transpose = FALSE)
            
            df <- pROC::coords(method, "all", ret=c("threshold", "specificity", "sensitivity",
                                                    "precision"), transpose = FALSE)
          }
          df$SNPCall <- methods$snpcall[i]
          df$GenoCall <- methods$genocall[i]
          df$CountsFrom <- methods$countsfrom[i]
          df$depth <- methods$depth[i]
          df$best_tr <- best$threshold
          
          n <- 250 # Only a subset of dots
          vec <- 1:dim(df)[1]
          subs <- round(seq(min(vec),max(vec),(max(vec)-min(vec))/(n-1)), 0)
          subs_df <- df[order(df$sensitivity),]
          subs_df <- subs_df[subs,]
          
          tot_df <- rbind(tot_df, df)
          subset_df <- rbind(subset_df, subs_df)
        }
        
        incProgress(0.5, detail = paste("Doing part", 3))
        subset_df <- perfumaria(subset_df)
        subset_df <- subset_df[-which(subset_df$threshold == Inf | subset_df$threshold == -Inf),]
        
        tot_df <- perfumaria(tot_df)
        latex_df <- tot_df %>% group_by(GenoCall,SNPCall, CountsFrom, depth) %>%
          summarise(best = unique(best_tr)) %>% pivot_wider(names_from = GenoCall, values_from = best)
        
        latex_df <- latex_df[order(latex_df$SNPCall),]
        latex_df$`freebayes/GATK`[is.na(latex_df$`freebayes/GATK`)] <- "not applicable"
        latex_df <- data.frame(latex_df)
        latex_df[latex_df == Inf] <- "All genoypes correct"
        latex_df[latex_df == -Inf] <- "All genoypes wrong"
        
        incProgress(0.75, detail = paste("Doing part", 4))
        list(subset_df, latex_df)
      })
    })
    
    output$roc_out <- renderPlot({
      roc_graph(button()[[1]])
    })
    
    output$thr_out <- renderTable(
      button()[[2]]
    )
}
    
## To be copied in the UI
# mod_simu_roc_curves_ui("simu_roc_curves_ui_1")
    
## To be copied in the server
# mod_simu_roc_curves_server("simu_roc_curves_ui_1")
