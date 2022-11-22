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
                   selectInput(ns("example_simu"), label = h4(tags$b("SimulatedReads2Map.wdl results for P. tremula 38cM of chromosome 10")), 
                               choices = list("Biallelics GQ" = "bi",
                                              "Biallelics GQ with segregation distortion" = "bi_dev",
                                              "Biallelics filtered GQ" = "bi_filt_gq",
                                              "Biallelics GQ filtered with segregation distortion" = "bi_filt_gq_dev",
                                              "Biallelics GQ and non-informative filtered" = "bi_filt_gq_noninfo",
                                              "Biallelics GQ and non-informative filtered with segregation distortion" = "bi_filt_gq_noninfo_dev",
                                              "Biallelics PL filtered" = "bi_pl_filt",
                                              "Biallelics + multiallelics GQ" = "multi",
                                              "Biallelics + multiallelics GQ with segregation distortion" = "multi_dev",
                                              "Biallelics + multiallelics filtered GQ" = "multi_filt_gq", 
                                              "Biallelics + multiallelics filtered GQ with segregation distortion" = "multi_filt_gq_dev", 
                                              "Biallelics + multiallelics filtered GQ and non-informative" = "multi_filt_gq_noninfo",
                                              "Biallelics + multiallelics filtered GQ and non-informative with segregation distortion" = "multi_filt_gq_noninfo_dev",=======
                                              "Biallelics + multiallelics, GQ and non-informative filtered, with segregation distortion, and GT missing replaced" = "multi_filt_gq_noninfo_dev_replaced",
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
                                 "Roses 37% Chr01	- biallelics filt GQ and noninfo" = "rose_biallelics_filt_GQ_noninfo",
                                 "Roses 37% Chr01	- multiallelics filt GQ and noninfo" = "rose_multiallelics_filt_GQ_noninfo",
                                 "Roses 37% Chr01	- biallelics  GQ" = "rose_biallelics_GQ",
                                 "Roses 37% Chr01	- multiallelics GQ" = "rose_multiallelics_GQ",
                                 "Roses 37% Chr01	- biallelics filt GQ" = "rose_biallelics_filt_GQ",
                                 "Roses 37% Chr01	- multiallelics filt GQ" = "rose_multiallelics_filt_GQ",
                                 "P. tremula 37% Chr10 - biallelics GQ" = "populus_biallelics_GQ",
                                 "P. tremula 37% Chr10 - biallelics filt GQ" = "populus_biallelics_filt_GQ",
                                 "P. tremula 37% Chr10 - biallelics filt GQ and noninfo" = "populus_biallelics_filt_GQ_noninfo",
                                 "P. tremula 37% Chr10 - multiallelics GQ" = "populus_multiallelics_GQ",
                                 "P. tremula 37% Chr10 - multiallelics filt GQ" = "populus_multiallelics_filt_GQ",
                                 "P. tremula 37% Chr10 - multiallelics filt GQ and noninfo" = "populus_multiallelics_filt_GQ_noninfo",
                                 "P. tremula 37% Chr10 with 6 contaminants - biallelics GQ" = "populus_biallelics_GQ_cont",
                                 "P. tremula 37% Chr10 with 6 contaminants - biallelics filt GQ" = "populus_biallelics_filt_GQ_cont",
                                 "P. tremula 37% Chr10 with 6 contaminants - biallelics filt GQ and noninfo" = "populus_biallelics_filt_GQ_noninfo_cont",
                                 "P. tremula 37% Chr10 with 6 contaminants - multiallelics GQ" = "populus_multiallelics_GQ_cont",
                                 "P. tremula 37% Chr10 with 6 contaminants - multiallelics filt GQ" = "populus_multiallelics_filt_GQ_cont",
                                 "P. tremula 37% Chr10 with 6 contaminants - multiallelics filt GQ and noninfo" = "populus_multiallelics_filt_GQ_noninfo_cont",=======
                                 "Toy sample with multiallelics" = "toy_sample_multi"), 
                               selected = "toy_sample_multi"),
                 )
             )
      ),
      column(width = 12,
             fluidPage(
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
