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
                   "To not overload our server, we limited the upload size to 500MB.", 
                   "If your results have larger size, please run the app locally using:", br(),
                   tags$code("runGitHub('Cristianetaniguti/Reads2MapApp')"), br(), br(),
                   # Copy the line below to make a file upload manager
                   "If you have more than one depth value, submit all them together.", br(),
                   fileInput(ns("simulatedreads"), label = h6("File: SimulatedReads2Map_<depth>.tar.gz"), multiple = T),
                 ),
                 
                 fluidPage(
                   # Copy the line below to make a select box 
                   "See description of each dataset in the tables bellow.",
                   selectInput(ns("example_simu"), label = h4(tags$b("SimulatedReads2Map.wdl results")), 
                               choices = list("P. tremula 20cM of chromosome 10 - without multiallelics" = "populus_200_bi_radinitio20",
                                              "P. tremula 20cM of chromosome 10 - with multiallelics" = "populus_200_multi_radinitio20",
                                              "P. tremula 37cM of chromosome 10 - without multiallelics" = "populus_200_bi_radinitio37",
                                              "P. tremula 37cM of chromosome 10 - with multiallelics" = "populus_200_multi_radinitio37",
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
                   "To not overload our server, we limited the upload size to 500MB.", 
                   "If your results have larger size, please run the app locally using:", br(),
                   tags$code("runGitHub('Cristianetaniguti/Reads2MapApp')"), br(), br(),
                   # Copy the line below to make a file upload manager
                   "If you have more than one depth value, submit all them together.", br(),
                   fileInput(ns("empiricalreads"), label = h6("File: EmpiricalReads2Map_<depth>.tar.gz"), multiple = T, accept = ".tar.gz"),
                 ),
                 fluidPage(
                   # Copy the line below to make a select box 
                   "See description of each dataset in the tables bellow.",
                   selectInput(ns("example_emp"), label = h4(tags$b("EmpiricalReads2Map.wdl results")), 
                               choices = list(
                                 "P. tremula - without multiallelics	" = "populus_bi8.5",
                                 "P. tremula - with multiallelics	" = "populus_multi8.5"),
                               # "P. tremula - with contaminants; without multiallelics" = "populus_cont",
                               # "P. tremula - without multiallelics	" = "populus",
                               # "P. tremula - with multiallics" = "populus_multi",
                               # #"Eucalyptus - without multiallelics" = "eucalyptus",
                               # "Toy sample without multiallelics " = "toy_sample",
                               # "Toy sample with multiallelics" = "toy_sample_multi"), 
                               selected = "populus_multi8.5"),
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
    # Simulations - rearranging data
    ##################################################
    
    # Update choices of seed and depth according with the dataset chosen
    # observe({
    #   seeds_choice <- datas_simu()[[7]][[3]] 
    #   depth_choice <- unique(datas_simu()[[7]][[4]])
    #   inds_choice <- datas_simu()[[7]][[5]]
    #   
    #   updateSelectInput(session, "seed1",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   
    #   updateSelectInput(session, "seed2",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   
    #   updateSelectInput(session, "seed3",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   updateSelectInput(session, "seed_cmbymb",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   updateSelectInput(session, "seed11",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   updateSelectInput(session, "seed12",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   updateCheckboxGroupInput(session, "inds12",
    #                            label="Individuals from progeny",
    #                            choices = inds_choice,
    #                            selected=unlist(inds_choice)[1])
    #   
    #   updateSelectInput(session, "seed13",
    #                     label="Seed",
    #                     choices = seeds_choice,
    #                     selected=unlist(seeds_choice)[1])
    #   
    #   updateCheckboxGroupInput(session, "depth4",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateCheckboxGroupInput(session, "depth_probs",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateCheckboxGroupInput(session, "depth_roc",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateRadioButtons(session, "depth10",
    #                      label="Depth",
    #                      choices = depth_choice,
    #                      selected=names(depth_choice)[1])
    #   
    #   updateCheckboxGroupInput(session, "depth8",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateCheckboxGroupInput(session, "depth9",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateCheckboxGroupInput(session, "depth5",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateCheckboxGroupInput(session, "depth7",
    #                            label="Depth",
    #                            choices = depth_choice,
    #                            selected=unlist(depth_choice))
    #   
    #   updateSelectInput(session, "overview_depth",
    #                     label="Choose depth for the plot",
    #                     choices = depth_choice,
    #                     selected=unlist(depth_choice)[1])
    # })
    
    ####################################################################
    # Empirical - - rearranging data
    ####################################################################
    
    # observe({
    #   inds_choice <- datas_emp()[[7]]
    #   
    #   updateCheckboxGroupInput(session, "inds12_emp",
    #                            label="Individuals from progeny",
    #                            choices = inds_choice,
    #                            selected=unlist(inds_choice)[1])
    # })
    
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
