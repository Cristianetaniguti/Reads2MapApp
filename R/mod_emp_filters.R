#' emp_filters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_emp_filters_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The graphic here show the total number of markers available for analysis,
            the number of markers filtered by OneMap because of missing data, segregation distortion and redundancy.",
    hr(),
    fluidRow(
      column(width = 12,
             box(title = "Filters",
                 width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                 plotOutput(ns("filters_emp_out")),
                 hr(),
                 actionButton(ns("go"), "Update",icon("refresh")),
             )
      ),
      
      column(width = 6,
             box(
               width = NULL, solidHeader = TRUE,
               fluidPage(
                 checkboxGroupInput(ns("ErrorProb"), label = p("Genotyping method"),
                                    choices = ErrorProb_choice,
                                    selected = unlist(ErrorProb_choice))
               )
             )
      ),
      column(width = 6,
             box(width = NULL, solidHeader = T,
                 fluidPage(
                   checkboxGroupInput(ns("SNPCall"), label = p("SNP calling method"),
                                      choices = SNPCall_choice,
                                      selected = unlist(SNPCall_choice)),
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

#' emp_filters Server Functions
#'
#' @noRd 
mod_emp_filters_server <- function(input, output, session, datas_emp){
  ns <- session$ns
  
  button <- eventReactive(input$go, {
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))

      data <- datas_emp()[[3]] %>% filter(GenoCall %in% input$ErrorProb) %>%
        filter(SNPCall %in% input$SNPCall) %>%
        filter(CountsFrom %in% input$CountsFrom)  %>%
        gather(key, value, -CountsFrom, -GenoCall, -SNPCall) %>%
        filter(key %in% c("n_markers", "miss", "distorted_markers",
                          "redundant_markers",  "selected_chr_no_dist"))
      
      new_names <- c(`Without filters` = "n_markers", `Missing data` = "miss",
                     `Segregation test` = "distorted_markers", Redundants = "redundant_markers",
                     `After all filters`= "selected_chr_no_dist")
      data$key <- names(new_names)[match(data$key, new_names)]
      data$key <- factor(data$key, levels = c("Without filters",
                                              "Missing data",
                                              "Segregation test",
                                              "Redundants",
                                              "After all filters"))
      
      perfumaria(data)
      
    })
  })
  
  output$filters_emp_out <- renderPlot({
    filters_graph_emp(button())
  })
  
  output$filters_emp_df_out <- renderDataTable({
    button()
  }) 
}

## To be copied in the UI
# mod_emp_filters_ui("emp_filters_ui_1")

## To be copied in the server
# mod_emp_filters_server("emp_filters_ui_1")
