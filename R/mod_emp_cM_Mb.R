#' emp_cM_Mb UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_cM_Mb_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic here show the relation between the position of marker in centimorgan and their position in reference genome (Mb).",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "cM x Mb",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("cmxmb_emp_out")), hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = maps_choice,
                                    selected = names(maps_choice)),
                 hr()
               )
             )
      ),
      column(width = 6,
             box(width = NULL, solidHeader = T,
                 #helpText("Select the SNP calling method"),
                 fluidPage(
                   checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                      choices = SNPCall_choice,
                                      selected = unlist(SNPCall_choice)),
                   hr()
                 ),
                 fluidPage(
                   radioButtons(ns("Global0.05"), label = p("Error rate"),
                                choices = global0.05_choices,
                                selected = "FALSE"),
                   hr()
                 ),
                 fluidPage(
                   
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = unlist(CountsFrom_choice))
                 )
             )
      )
    )
  )
}
    
#' emp_cM_Mb Server Functions
#'
#' @noRd 
mod_emp_cM_Mb_server <- function(input, output, session, datas_emp){
    ns <- session$ns
    button <- eventReactive(input$go, {
      
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        if(input$Global0.05){
          geno <- paste0(input$ErrorProb, 0.05)
          if(any(input$ErrorProb %in% "OneMap_version2"))
            geno[which(input$ErrorProb == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb %in% "gusmap"))
            stop(safeError("Gusmap do not allow to change the error rate.
                           Please, select other option."))
        } else {
          geno <- input$ErrorProb
        }
        
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom)
        data$pos <- data$pos/1000
        perfumaria(data)
      })
    })
    
    output$cmxmb_emp_out <- renderPlot({
      ggplot(button(), aes(x=rf, y=pos)) +
        geom_point(size=2, shape=20) + facet_grid(SNPCall + CountsFrom~GenoCall, scales = "free") +
        xlab("cM") + ylab("Mb") + theme_bw()
    })
}
    
## To be copied in the UI
# mod_emp_cM_Mb_ui("emp_cM_Mb_ui_1")
    
## To be copied in the server
# mod_emp_cM_Mb_server("emp_cM_Mb_ui_1")
