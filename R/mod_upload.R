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
      "workflows you can upload the outputted data in", tags$b("Upload SimulatedReads2Map outputs"), "and/or",
      tags$b("Upload EmpiricalReads2Map outputs"), "sections. If you don't have your own results yet,
            you can explore the ones generated with the datasets described in the tables bellow.
            Select the available dataset results in", tags$b("SimulatedReads2Map.wdl results"),"and/or", 
      tags$b("EmpiricalReads2Map.wdl results"),".",
      hr(),
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
                   "See description of each dataset in the tables bellow.",
                   selectInput(ns("example_simu"), label = h4(tags$b("SimulatedReads2Map.wdl results")), 
                               choices = list("P. tremula 38cM of chromosome 10" = "bi",
                                              "P. tremula 38cM of chromosome 10 - geno probs" = "bi_pl",
                                              "P. tremula 38cM of chromosome 10 - with segregation distortion" = "bi_dev",
                                              "P. tremula 38cM of chromosome 10 - with multiallelics" = "multi",
                                              "P. tremula 38cM of chromosome 10 - with multiallelics and segregation distortion" = "multi_dev",
                                              "Toy sample without multiallelics" = "toy_sample_bi",
                                              "Toy sample with multiallelics" = "toy_sample_multi"),
                               selected = "toy_sample_multi"),
                 )
             )
      ),
      column(width = 6,
             box(width = 12, solidHeader = TRUE, collapsible = FALSE, status="primary", title= "EmpiricalReads2Map",
                 fluidPage(
                   
                   tags$h4(tags$b("Upload EmpiricalReads2Map results:")),
                   # Copy the line below to make a file upload manager
                   "If you have more than one depth value, submit all them together.", br(),
                   fileInput(ns("empiricalreads"), label = h6("File: EmpiricalReads2Map_<depth>.tar.gz"), multiple = T, accept = ".tar.gz"),
                 ),
                 fluidPage(
                   # Copy the line below to make a select box 
                   "See description of each dataset in the tables bellow.",
                   selectInput(ns("example_emp"), label = h4(tags$b("EmpiricalReads2Map.wdl results")), 
                               choices = list(
                               "Roses Chr01	" = "rose",
                               # "P. tremula - with multiallelics	" = "populus_multi8.5"),
                               # "P. tremula - with contaminants; without multiallelics" = "populus_cont",
                               # "P. tremula - without multiallelics	" = "populus",
                               # "P. tremula - with multiallics" = "populus_multi",
                               # #"Eucalyptus - without multiallelics" = "eucalyptus",
                               # "Toy sample without multiallelics " = "toy_sample",
                                "Toy sample with multiallelics" = "toy_sample_multi"), 
                               selected = "toy_sample_multi"),
                 )
             )
      ),
      column(width = 12,
             "Here we describe some of the main characteristics of each dataset available in this server. 
                     It is possible to access all other arguments used in the analysis in the metadata produced by the workflows.", hr(),
             box(width = NULL,  solidHeader = TRUE, 
                 collapsible = FALSE, status="primary", title= "SimulatedReads2Map results available",
                 DT::dataTableOutput(ns("simulated_datasets")),
             ), 
             hr(),
             box(width = NULL,  solidHeader = TRUE, 
                 collapsible = FALSE, status="primary", title= "EmpiricalReads2Map results available",
                 DT::dataTableOutput(ns("empirical_datasets")),
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
        datas_emp = reactive({prepare_datas_emp(input$empiricalreads, input$example_emp)})
      )
    )
}

## To be copied in the UI
# mod_upload_ui("upload_ui_1")

## To be copied in the server
# mod_upload_server("upload_ui_1")
