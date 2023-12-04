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
                 actionButton(ns("go"), "Update",icon("refresh", verify_fa = FALSE)),
             )
      ),
      
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = "This will be updated",
                                    selected = "This will be updated"),
                 hr()
               )
             )
      ),
      column(width = 6,
             box(width = NULL, solidHeader = T,
                 #helpText("Select the SNP calling method"),
                 fluidPage(
                   checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                      choices = "This will be updated",
                                      selected = "This will be updated"),
                   hr()
                 ),
                 fluidPage(
                   
                   checkboxGroupInput(ns("CountsFrom"), label = p("Counts from"),
                                      choices = "This will be updated",
                                      selected = "This will be updated")
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
    
    observe({
      
      SNPCall_choice <- as.list(unique(datas_emp()[[2]]$SNPCall))
      names(SNPCall_choice) <- unique(datas_emp()[[2]]$SNPCall)
      
      ErrorProb_choice <- as.list(unique(datas_emp()[[2]]$GenoCall))
      names(ErrorProb_choice) <- gsub("default", "_OneMap2.0", unique(datas_emp()[[2]]$GenoCall))
      CountsFrom_choice <- as.list(unique(datas_emp()[[2]]$CountsFrom))
      names(CountsFrom_choice) <- unique(datas_emp()[[2]]$CountsFrom)
      
      updateCheckboxGroupInput(session, "SNPCall",
                               label="SNP call method",
                               choices = SNPCall_choice,
                               selected=unlist(SNPCall_choice)[1])
      
      updateCheckboxGroupInput(session, "ErrorProb",
                               label="Genotyping method",
                               choices = ErrorProb_choice,
                               selected=unlist(ErrorProb_choice)[1])
      
      updateCheckboxGroupInput(session, "CountsFrom",
                               label="Counts From",
                               choices = CountsFrom_choice,
                               selected=unlist(CountsFrom_choice)[1])
    })
    
    button <- eventReactive(input$go, {
      
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb) %>%
          filter(SNPCall %in% input$SNPCall) %>%
          filter(CountsFrom %in% input$CountsFrom)
        data$pos <- data$pos/1000
        data
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
