#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      "This shiny app build several graphics using results from Reads2Map workflows. 
            If you run the", tags$b("SimulatedReads2Map.wdl"),"and/or", tags$b("EmpiricalReads2Map.wdl"), 
      "workflows for a diploid specie you can upload the outputted tar.gz data in", tags$b("SimulatedReads2Map"), "and/or",
      tags$b("EmpiricalReads2Map"), "sections. If you run the", tags$b("EmpiricalReads2Map.wdl"),"for a polyploid specie,
      upload the outputted tar.gz file in the", tags$b("Polyploid EmpiricalReads2Map"),". If you don't have your own results yet,
            you can explore the App features with the example subsets.",
      hr(),
      fluidPage(
        column(width = 6,
               box(width = 12, solidHeader = TRUE, collapsible = FALSE, status="primary", title = "SimulatedReads2Map",
                   fluidPage(
                     tags$h4(tags$b("Upload SimulatesReads2Map results:")),
                     # Copy the line below to make a file upload manager
                     "If you have more than one depth value, submit all them together.", br(),
                     fileInput(ns("simulatedreads"), label = h6("File: SimulatedReads2Map_<depth>.tar.gz"), multiple = T),
                   ),
                   
                   fluidPage(
                     # Copy the line below to make a select box 
                     "Available results for example subsets:",
                     selectInput(ns("example_simu"), label = h4(tags$b("SimulatedReads2Map.wdl results")), 
                                 choices = list(
                                   "Toy sample diploid with multiallelics" = "toy_sample_multi"),
                                 selected = "toy_sample_multi"),
                   )
               )
        ),
        column(width = 6,
               box(width = 12, solidHeader = TRUE, collapsible = FALSE, status="primary", title= "EmpiricalReads2Map",
                   fluidPage(
                     
                     tags$h4(tags$b("Upload EmpiricalReads2Map results for diploids:")),
                     # Copy the line below to make a file upload manager
                     fileInput(ns("empiricalreads"), label = h6("File: EmpiricalReads2Map.tar.gz"), multiple = T, accept = ".tar.gz"),
                   ),
                   fluidPage(
                     # Copy the line below to make a select box 
                     "Available results for example subsets:",
                     selectInput(ns("example_emp"), label = h4(tags$b("EmpiricalReads2Map.wdl results for diploids")), 
                                 choices = list(
                                   "Toy sample diploids" = "toy_sample_diplo",
                                   "Toy sample polyploids" = "toy_sample_poly"), 
                                 selected = "toy_sample_diplo"),
                   )
               )
        ),
        column(width = 12,
               box(width = 12, solidHeader = TRUE, collapsible = FALSE, status="primary", title= "Polyploid EmpiricalReads2Map",
                   fluidPage(
                     
                     tags$h4(tags$b("Upload EmpiricalReads2Map results for polyploids:")),
                     # Copy the line below to make a file upload manager
                     fileInput(ns("empiricalpolyreads"), label = h6("File: EmpiricalReads2Map.tar.gz"), multiple = T, accept = ".tar.gz"),
                   ),
                   fluidPage(
                     # Copy the line below to make a select box 
                     "Available results for example subsets:",
                     selectInput(ns("example_poly_emp"), label = h4(tags$b("EmpiricalReads2Map.wdl results for polyploids")), 
                                 choices = list(
                                   "Toy sample polyploids" = "toy_sample_poly"), 
                                 selected = "toy_sample_poly"),
                   )
               )
        )
      )
    )
  )
}

#' upload Server Functions
#'
#' @noRd 
mod_upload_server <- function(input, output, session){
  ns <- session$ns
  ##################################################
  # Simulations
  ##################################################
  
  # Input tables
  output$simulated_datasets <- DT::renderDataTable({
    DT::datatable(simulated_datasets, options = list(scrollX = TRUE))
  })
  
  output$empirical_datasets <- DT::renderDataTable({
    DT::datatable(empirical_datasets, options = list(scrollX = TRUE))
  })
  
  return(
    list(
      datas_simu = reactive({prepare_datas_simu(x=input$simulatedreads, example_simu =input$example_simu)}),
      datas_emp = reactive({prepare_datas_emp(input$empiricalreads, input$example_emp)}),
      datas_poly_emp = reactive({prepare_poly_datas_emp(input$empiricalpolyreads, input$example_poly_emp)})
    )
  )
}

## To be copied in the UI
# mod_upload_ui("upload_ui_1")

## To be copied in the server
# mod_upload_server("upload_ui_1")
