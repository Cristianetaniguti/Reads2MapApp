##'
##'@import shiny
##'@import onemap
##'@import shinydashboard
##'@import largeList
##'@import dplyr
##'@import tidyr
##'@import GUSMap
##'@import patchwork
##'@importFrom plotly plotlyOutput renderPlotly ggplotly
##'@import ggplot2
##'@import ggpubr
##'@import RColorBrewer
##'@import vroom
##'@importFrom htmlwidgets saveWidget
##'@importFrom irr kappa2 kendall agree
##'
##'@export
Reads2MapApp <- function(...) {
  ## Permanently choices - If add more softwares in workflow comparision this part requires update
  overview_emp_choices <- list("n_markers", "redundants", "n_markers_map", "filt_mks",
                               "map_size", "time", "breakpoints", "mean_break", "se_break")
  
  names(overview_emp_choices) <- c("Informative markers in VCF", "Redundant markers", 
                                   "Mapped markers", "Filtered markers", "Map size (cM)", 
                                   "Time (s)","Total breakpoints", "Mean breakpoints", 
                                   "Standard error breakpoints")
  
  overview_choices <- list("geno", "phases", "marker", "noninfo", "mapsize",
                           "nmarker", "conco_break", "corr_break", "time")
  names(overview_choices) <- c("Kappa's coefficient for genotypes",
                               "Kappa's coefficient for phases",
                               "Kappa's coefficient for marker types",
                               "Percentage of noninformative markers", 
                               "Map size (cM)",
                               "Number markers in map",
                               "Kendall's coefficient of concordance for breakpoints",
                               "Kendall's coefficient of correlation for breakpoints",
                               "Time spent (s)")
  
  ErrorProb_choice <- list("OneMap_version2" = "OneMap_version2",
                           "polyRAD"="polyrad",
                           "freebayes/GATK"="SNPCaller",
                           "SuperMASSA"="supermassa",
                           "updog"="updog")
  
  global0.05_choices <- list("global error of 0.05"=TRUE,
                             "variable error" = FALSE)
  
  maps_choice <- list("GUSMap" = "gusmap",
                      "OneMap_version2" = "OneMap_version2",
                      "polyRAD"="polyrad",
                      "freebayes/GATK"="SNPCaller",
                      "SuperMASSA"="supermassa",
                      "updog"="updog")
  SNPCall_choice <- list("freebayes"="freebayes",
                         "GATK" = "gatk")
  CountsFrom_choice <- list("BAM"="bam",
                            "VCF"="vcf")
  
  stats_choice <- list("euclidean_dist", "mean", "median", "var", "total", "total_size")
  names(stats_choice) <- c("Euclidean distance (D)", "mean", "median", "var", "total", "total size")
  
  fake_choices <- list("with-false", "without-false")
  names(fake_choices) <- c("yes", "no")
  
  
  ########
  #  UI  #
  ########
  
  ## Header
  header <- dashboardHeader(
    title = "Reads2MapApp",
    # Logo in the header
    # tags$li(class = "dropdown",
    #         tags$a(href="https://statgen-esalq.github.io/", target="_blank", 
    #                tags$img(height = "20px", alt="SNAP Logo", src="logo_fundo_azul.png")
    #         )
    # ),
    titleWidth = 250)
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("About", tabName = "about", icon = icon("lightbulb")),
      #menuItem("Parallel map", icon = icon("dot-circle"), tabName = "parallel"), 
      menuItem("Upload data", icon = icon("upload"), tabName= "upload"),
      menuItem("Simulation results", icon = icon("dot-circle"), tabName= "simulations",
               menuSubItem("SNP calling efficiency", icon = icon("circle"), tabName = "snpcall"),
               #menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage"),
               menuSubItem("Filters", icon = icon("circle"), tabName = "filters"),
               menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type"),
               menuSubItem("Times", icon = icon("circle"), tabName = "times"),
               menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth"),
               menuSubItem("Genotype probabilities", icon = icon("circle"), tabName = "probs"),
               menuSubItem("Map size each family", icon = icon("circle"), tabName = "ind_size"),
               menuSubItem("Overview map size", icon = icon("circle"), tabName = "all_size"),
               menuSubItem("Phases", icon = icon("circle"), tabName = "phases"),
               menuSubItem("Maps", icon = icon("circle"), tabName = "map"),
               menuSubItem("Progeny haplotypes", icon = icon("circle"), tabName = "haplo"),
               menuSubItem("Breakpoints count", icon = icon("circle"), tabName = "counts"),
               menuSubItem("cM x Mb", icon = icon("circle"), tabName = "cmxmb")),
      #menuSubItem("Overview", icon = icon("circle"), tabName = "overview")),
      
      menuItem("Empirical data results", icon = icon("dot-circle" ), tabName = "empirical",
               #menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage_emp"),
               menuSubItem("Filters", icon = icon("circle"), tabName = "filters_emp"),
               menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type_emp"),
               menuSubItem("Times", icon = icon("circle"), tabName = "times_emp"),
               menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth_emp"),
               menuSubItem("Map size", icon = icon("circle"), tabName = "ind_size_emp"),
               menuSubItem("Plotly heatmaps", icon = icon("circle"), tabName = "heatmaps_emp"),
               menuSubItem("Maps", icon = icon("circle"), tabName = "map_emp"),
               menuSubItem("Progeny haplotypes", icon = icon("circle"), tabName = "haplo_emp"),
               menuSubItem("Breakpoints count", icon = icon("circle"), tabName = "counts_emp"),
               menuSubItem("cM x Mb", icon = icon("circle"), tabName = "cmxmb_emp")),
      menuItem("Workflow tasks times", icon = icon("circle"), tabName = "wf_times"),
      tags$li(class = "dropdown",
              tags$a(href="https://statgen-esalq.github.io/", target="_blank", 
                     tags$img(height = "60px", alt="Logo", src="logo_fundo_azul.png")
              ))
    )
  )
  
  body <- dashboardBody(
    # Also add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #003350;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #003350;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #003350;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #003350;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color:  #003350;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color:  #003350;
                              color: #ffffff;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #cc662f;
                              color: #000000;
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #003350;
         }
                              
        .box.box-solid.box-primary>.box-header {
          color:#fff;
          background:#cc662f
                            }
        
        .box.box-solid.box-primary{
        border-bottom-color:#cc662f;
        border-left-color:#cc662f;
        border-right-color:#cc662f;
        border-top-color:#cc662f;
        }
                              '))),
    
    tabItems(
      ##########################################################
      tabItem(tabName = "about",
              includeMarkdown(system.file("vignettes", "about.Rmd", package = "Reads2MapApp"))
      ),
      ####################################################################################
      # tabItem(tabName = "parallel", # Se eu deixo isso funcional o menu deixa de ser dinamico
      #         includeHTML("docs/parallel.html")
      #         #includeMarkdown("docs/parallel.Rmd")
      # ),
      ##########################################################
      # Upload data
      ##########################################################
      tabItem(tabName = "upload",
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
                           fileInput("simulatedreads", label = h6("File: SimulatedReads2Map_<depth>.tar.gz"), multiple = T),
                         ),
                         
                         fluidPage(
                           # Copy the line below to make a select box 
                           "See description of each dataset in the tables bellow.",
                           selectInput("example_simu", label = h4(tags$b("SimulatedReads2Map.wdl results")), 
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
                           fileInput("empiricalreads", label = h6("File: EmpiricalReads2Map_<depth>.tar.gz"), multiple = T, accept = ".tar.gz"),
                         ),
                         fluidPage(
                           # Copy the line below to make a select box 
                           "See description of each dataset in the tables bellow.",
                           selectInput("example_emp", label = h4(tags$b("EmpiricalReads2Map.wdl results")), 
                                       choices = list("P. tremula - with contaminants; without multiallelics" = "populus_cont",
                                                      "P. tremula - without multiallelics	" = "populus",
                                                      "P. tremula - with multiallics" = "populus_multi",
                                                      "Eucalyptus - without multiallelics" = "eucalyptus",
                                                      "Toy sample without multiallelics " = "toy_sample",
                                                      "Toy sample with multiallelics" = "toy_sample_multi"), 
                                       selected = "toy_sample"),
                         )
                     )
              ),
              includeMarkdown(system.file("vignettes", "datasets.Rmd", package = "Reads2MapApp"))
      ),
      ##########################################################
      # Simulations
      ##########################################################
      tabItem(tabName = "disper_depth",
              "The same graphic is plotted in left and right to user be able to compare different methods choosing the options below.
            The x and y axis shows the read counts for reference and alternative alleles, respectively. 
            The colors of dots varies according with `Genotypes from` section. User can choose to see colors from simulated genotypes, the estimated or a gradient with the error rate used. 
            Users can download the left graphic pushing `Download` button.",
              hr(),
              fluidRow(
                column(width = 6,
                       box(title = "Depth and genotyping 1",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("disper_depth_out"),
                           hr(),
                           actionButton("go1", "Update",icon("refresh")),
                       ),
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           radioButtons("ErrorProb1", label = p("Genotyping method"),
                                        choices = ErrorProb_choice,
                                        selected = "polyrad"),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.1", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("real1", label = p("Display the"),
                                        choices = list("simulated genotypes" = "simulated_genotypes", 
                                                       "estimated genotypes"="estimated_genotypes",
                                                       "errors rate" = "estimated_errors"),
                                        selected = "estimated_genotypes"),
                           hr()
                         ),
                         
                         #helpText("Select the SNP calling method"),
                         fluidPage(
                           radioButtons("SNPCall1", label = p("SNP calling method"),
                                        choices = SNPCall_choice,
                                        selected = "freebayes"),
                           hr()
                         ),
                         
                         #helpText("Select the family seed"),
                         fluidPage(
                           selectInput("seed1", label = p("Seed"),
                                       choices = "It will be updated",
                                       selected ="It will be updated"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("CountsFrom1", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr()
                         ),
                         
                         #helpText("Read counts from:"),
                         fluidPage(
                           textInput("marker1", label = p("By default, it plots the genotypes of all markers, you can select a specific one defining its ID here:"),
                                     value = "Ex: Chr10_1000"),
                           hr(),
                           div(downloadButton("disper_depth_out_down"),style="float:right")
                           # ),
                         )
                       )
                ),
                
                column(width = 6,
                       box(title = "Depth and genotyping 2",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("disper_depth2_out"), 
                           hr(),
                           tableOutput('disper_depth2_cor_out'),
                           actionButton("go2", "Update",icon("refresh")),
                       ),
                       
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           radioButtons("ErrorProb2", label = p("Genotyping method"),
                                        choices = ErrorProb_choice,
                                        selected = "polyrad"),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.2", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("real2", label = p("Display the"),
                                        choices = list("simulated genotypes" = "simulated_genotypes", 
                                                       "estimated genotypes"="estimated_genotypes",
                                                       "errors rate" = "estimated_errors"),
                                        selected = "estimated_genotypes"),
                           hr()
                         ),
                         
                         #helpText("Select the SNP calling method"),
                         fluidPage(
                           radioButtons("SNPCall2", label = p("SNP calling method"),
                                        choices = SNPCall_choice,
                                        selected = "freebayes"),
                           hr()
                         ),
                         
                         #helpText("Select the family seed"),
                         fluidPage(
                           selectInput("seed2", label = p("Seed"),
                                       choices = "It will be updated",
                                       selected = "It will be updated"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("CountsFrom2", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr()
                         ),
                         
                         #helpText("Read counts from:"),
                         fluidPage(
                           textInput("marker2", label = p("By default, it plots the genotypes of all markers, you can select a specific one defining its ID here:"),
                                     value = "Ex: Chr10_1000"),
                           hr()
                           # ),
                         )
                       )
                )
              )
      ),
      ##########################################################
      tabItem(tabName = "probs",
              "Genotype probabilities",
              fluidRow(
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb_probs", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         #helpText("Select the SNP calling method"),
                         fluidPage(
                           checkboxGroupInput("SNPCall_probs", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = unlist(SNPCall_choice)),
                         )
                       )
                ),
                column(width=6,
                       box(width = NULL, solidHeader = TRUE,
                           #helpText("Select the family seed"),
                           fluidPage(
                             checkboxGroupInput("depth_probs", label = p("Depth"),
                                                choices = "It will be updated",
                                                selected = "It will be updated"),
                             hr()
                           ),
                           
                           #helpText("Read counts from:"),
                           fluidPage(
                             
                             checkboxGroupInput("CountsFrom_probs", label = p("Counts from"),
                                                choices = CountsFrom_choice,
                                                selected = "vcf"),
                             hr(),
                             actionButton("go_probs", "Update",icon("refresh")),
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
                           plotOutput("probs_prog_out", width = "100%", height = "100%"),
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
                           plotOutput("probs_pare_bi_out", width = "100%", height = "100%"),
                       ),
                       box(title = "Parents conditional genotypes probabilities - only multiallelic markers.",
                           width = NULL, solidHeader = TRUE, collapsible = TRUE, status="primary",
                           plotOutput("probs_pare_multi_out", width = "100%", height = "100%"),
                       ),
                )
              )
      ),
      ##########################################################
      tabItem(tabName = "cmxmb",
              "The scatter plots show the relation between genetic (cM) and physical (MB) distance.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title= "cM x Mb",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("cmbymb_out"),
                           hr(),
                           actionButton("go_cmbymb", "Update",icon("refresh")),
                       )
                ),
                
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb_cmbymb", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05_cmbymb", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ),
                         #helpText("Select the SNP calling method"),
                         fluidPage(
                           checkboxGroupInput("SNPCall_cmbymb", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = unlist(SNPCall_choice)),
                         )
                       )
                ),
                column(width=6,
                       box(width = NULL, solidHeader = TRUE,
                           #helpText("Select the family seed"),
                           fluidPage(
                             selectInput("seed_cmbymb", label = p("Seed"),
                                         choices = "It will be updated",
                                         selected = "It will be updated"),
                             hr()
                           ),
                           
                           #helpText("Read counts from:"),
                           fluidPage(
                             
                             radioButtons("CountsFrom_cmbymb", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr()
                           ),
                           
                           fluidPage(
                             
                             radioButtons("fake_cmbymb", label = p("Allow false positives?"),
                                          choices = fake_choices,
                                          selected = "without-false"),
                             hr(),
                             div(downloadButton("cmbymb_out_down"),style="float:right")
                             # ),
                           )
                       )
                )
              )
      ),
      ##########################################################
      tabItem(tabName = "ind_size",
              "The graphic show the distribuition of the difference between estimated and simulated  distances for each marker of the generated maps.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Map size each family",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("ind_size_out"),
                           hr(),
                           actionButton("go3", "Update",icon("refresh")),
                       )
                ),
                
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb3", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.3", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ),
                         #helpText("Select the SNP calling method"),
                         fluidPage(
                           checkboxGroupInput("SNPCall3", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = unlist(SNPCall_choice)),
                           hr()
                         )
                       )
                ),
                column(width=6,
                       box(width = NULL, solidHeader = TRUE,
                           #helpText("Select the family seed"),
                           fluidPage(
                             selectInput("seed3", label = p("Seed"),
                                         choices = "It will be updated",
                                         selected = "It will be updated"),
                             hr()
                           ),
                           
                           #helpText("Read counts from:"),
                           fluidPage(
                             
                             radioButtons("CountsFrom3", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr()
                           ),
                           
                           fluidPage(
                             
                             radioButtons("fake1", label = p("Allow false positives?"),
                                          choices = fake_choices,
                                          selected = "without-false"),
                             hr(),
                             div(downloadButton("ind_size_out_down"),style="float:right")
                             # ),
                           )
                       )
                )
              )
      ),
      ######################################################################
      tabItem(tabName = "all_size",
              "Here we show an overview of all simulated familes by depths. Users can choose the descriptive 
            statistical to overview the differences between estimated and simulated distances.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title= "Overview map size",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("all_size_out"),
                           hr(),
                           dataTableOutput("all_size_df_out"),
                           actionButton("go4", "Update",icon("refresh")),
                       )
                ),
                
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           radioButtons("stats1", label = p("Statistic"),
                                        choices = stats_choice,
                                        selected = "mean"),
                           hr()
                         ),
                         fluidPage(
                           checkboxGroupInput("ErrorProb4", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.4", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         fluidPage(
                           checkboxGroupInput("SNPCall4", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                           hr()
                         )
                       )),
                column(width=6,
                       box(width = NULL, solidHeader = TRUE,
                           fluidPage(
                             checkboxGroupInput("depth4", label = p("Depth"),
                                                choices = "This will be updated",
                                                selected = "This will be updated"),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("CountsFrom4", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr()
                           ),
                           
                           fluidPage(
                             
                             radioButtons("fake2", label = p("Allow false positives?"),
                                          choices = fake_choices,
                                          selected = "without-false"),
                             hr(),
                             div(downloadButton("all_size_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      ###################################################################################
      tabItem(tabName = "marker_type",
              "These bar plots describes the number of markers of each type according with Wu et. al 2002a that remained in the built maps of each method.
            The upper graphics show the simulated marker types (real.type) and below they show the estimated (type). ",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Markers type",
                           width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("marker_type_out"),
                           hr(),
                           actionButton("go5", "Update",icon("refresh")),
                       )
                ),
                
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb10", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         fluidPage(
                           checkboxGroupInput("SNPCall10", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                           hr()
                         ))
                ),
                column(width=6,
                       box(width=NULL, solidHeader = T,
                           fluidPage(
                             radioButtons("depth10", label = p("Depth"),
                                          choices = "This will be updated",
                                          selected = "This will be updated"),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("CountsFrom10", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr()
                           ),
                           
                           fluidPage(
                             
                             radioButtons("fake3", label = p("Allow false positives?"),
                                          choices = fake_choices,
                                          selected = "without-false"),
                             hr(),
                             div(downloadButton("marker_type_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      ###################################################################################
      tabItem(tabName = "phases",
              "These graphic show the percentage of right estimated phases for all estimated families by depths.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Phases",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("phases_out"),
                           hr(),
                           actionButton("go6", "Update",icon("refresh")),
                       )
                ),
                
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb8", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.8", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         fluidPage(
                           checkboxGroupInput("SNPCall8", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                           hr()
                         )
                       )
                ),
                column(width=6,
                       box(
                         width=NULL, solidHeader = T,
                         fluidPage(
                           checkboxGroupInput("depth8", label = p("Depth"),
                                              choices = "This will be updated",
                                              selected = "This will be updated"),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("CountsFrom8", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr()
                         ),
                         div(downloadButton("phases_out_down"),style="float:right")
                         
                       )
                )
              )
      ),
      ###################################################################################
      tabItem(tabName = "times",
              "The graphic show the distribuition of times and number of markers of all families by depth.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Time spent to estimate genetic distances",
                           width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("times_out"),
                           hr(),
                           actionButton("go7", "Update",icon("refresh")),
                       )
                ),
                
                column(width=6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb9", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.9", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         fluidPage(
                           checkboxGroupInput("SNPCall9", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                           hr()
                         )
                       )
                ),
                column(width=6,
                       box(width=NULL, solidHeader = T,
                           fluidPage(
                             checkboxGroupInput("depth9", label = p("Depth"),
                                                choices = "This will be updated",
                                                selected = "This will be updated"),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("CountsFrom9", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr()
                           ),
                           
                           fluidPage(
                             
                             radioButtons("fake5", label = p("Allow false positives?"),
                                          choices = fake_choices,
                                          selected = "without-false"),
                             hr(),
                             
                             div(downloadButton("times_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      ####################################################################################
      tabItem(tabName = "snpcall",
              "The graphic here show some characteristics of SNP calling. The x axis contain the different 
            characteristics indexed according with section `Options`, and y axis shows the number of markers.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "SNP calling efficiency",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("snpcall_out"), hr(),
                           actionButton("go9", "Update",icon("refresh")),
                       )
                ),
                column(width = 6,
                       box(width = 6, solidHeader = T,
                           fluidPage(
                             checkboxGroupInput("SNPCall6", label = p("SNP calling method"),
                                                choices = SNPCall_choice,
                                                selected = names(SNPCall_choice)),
                             hr()
                           ),
                           fluidPage(
                             checkboxGroupInput("depth6", label = p("Depth"),
                                                choices = "This will be updated",
                                                selected = "This will be updated"),
                             hr(),
                             div(downloadButton("snpcall_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      ################################################################################3
      tabItem(tabName = "filters",
              "The graphic here show the total number of markers (n_markers) available for analysis, 
            the number of markers filtered by OneMap because of missing data, segregation distortion and redundancy.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Filters",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("filters_out"),  hr(),
                           actionButton("go10", "Update",icon("refresh")),
                       )
                ),
                
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb7", label = p("Genotyping method"),
                                              choices = ErrorProb_choice,
                                              selected = unlist(ErrorProb_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.7", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         
                         fluidPage(
                           checkboxGroupInput("SNPCall7", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                           hr()
                         )
                       )
                ),
                column(width = 6,
                       box(width = NULL, solidHeader = T,
                           fluidPage(
                             checkboxGroupInput("CountsFrom7", label = p("Counts from"),
                                                choices = CountsFrom_choice,
                                                selected = "vcf"),
                             hr()
                           ),
                           fluidPage(
                             selectInput("depth7", label = p("Depth"),
                                         choices = "This will be updated",
                                         selected = "This will be updated"),
                             hr(),
                             div(downloadButton("filters_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      
      ################################################################################3
      tabItem(tabName = "map",
              "On the left, it is plotted the heatmap graphic with recombination fraction varying in color according with the 
            intensity of the linkage. Higher is the recombination fraction hottest is the color. Markers ordered by genome positions are represented in x 
            and y axis. On the right, the linkage group is plotted with distances between markers proportional to the genetic distances.",
              hr(),
              fluidRow(
                column(width = 8,
                       box(title = "Recombination fraction heatmap",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("map_out"),
                           hr(),
                           actionButton("go11.1", "Update",icon("refresh")),
                       ), br(),
                       box(solidHeader = T,
                           radioButtons("ErrorProb11", label = p("Genotyping method"),
                                        choices = maps_choice,
                                        selected = "updog"),
                       ),
                       box(solidHeader = T,
                           radioButtons("Global0.05.11", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                       ), 
                       
                       box(solidHeader = T,
                           radioButtons("SNPCall11", label = p("SNP calling method"),
                                        choices = SNPCall_choice,
                                        selected = "gatk"),
                       ),
                       box(solidHeader = T,
                           selectInput("seed11", label = p("Seed"),
                                       choices = "It will be updated",
                                       selected = "It will be updated"),
                       ),
                       box(solidHeader = T,
                           radioButtons("fake11", label = p("Allow false positives"),
                                        choices = fake_choices,
                                        selected = "without-false"),
                       ),
                       box(solidHeader = T,
                           radioButtons("CountsFrom11", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           div(downloadButton("map1_out_down"),style="float:right"),
                       ),
                       
                       
                ),
                
                column(width = 4,
                       box(title = "Genetic map", 
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           actionButton("go11", "Update",icon("refresh")),
                           hr(),
                           imageOutput("map1_out", width = "100%", height = "100%")
                       )
                )
              )
      ),
      ################################################################################3
      tabItem(tabName = "haplo",
              "Based on the built map, progeny haplotypes can be draw by onemap function progeny_haplotypes. Choose pipeline and the individuals you want to check the haplotypes.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Estimated progeny haplotypes",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("haplot_out"),
                           hr(),
                           actionButton("go12", "Update",icon("refresh")),
                       )
                ),
                column(width = 12,
                       box(title = "Real progeny haplotypes",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("haplot_simu_out"),
                           hr(),
                           actionButton("go12.1", "Update",icon("refresh"))
                       )
                ), br(),
                box(solidHeader = T,
                    radioButtons("ErrorProb12", label = p("Genotyping method"),
                                 choices = maps_choice,
                                 selected = "updog"),
                ),
                box(solidHeader = T,
                    radioButtons("Global0.05.12", label = p("Error rate"),
                                 choices = global0.05_choices,
                                 selected = "FALSE"),
                ), 
                
                box(solidHeader = T,
                    radioButtons("SNPCall12", label = p("SNP calling method"),
                                 choices = SNPCall_choice,
                                 selected = "gatk"),
                ),
                box(solidHeader = T,
                    selectInput("seed12", label = p("Seed"),
                                choices = "It will be updated",
                                selected = "It will be updated"),
                ),
                box(solidHeader = T,
                    radioButtons("fake12", label = p("Allow false positives"),
                                 choices = fake_choices,
                                 selected = "without-false"),
                ),
                box(solidHeader = T,
                    radioButtons("Most_likely12", label = p("Choose how to show genotypes probabilities"),
                                 choices = list("The most likely genotypes receiveis maximum probabilities" = TRUE,
                                                "Genotypes probabilities are how they were outputted by HMM"= FALSE),
                                 selected = "TRUE"),
                ), 
                box(solidHeader = T,
                    radioButtons("CountsFrom12", label = p("Counts from"),
                                 choices = CountsFrom_choice,
                                 selected = "vcf"),
                ),
                box(solidHeader = T, collapsible = T, collapsed = T,
                    checkboxGroupInput("inds12", label = p("Individuals from progeny"),
                                       choices = "It will be updated",
                                       selected = "It will be updated"),
                ),
                div(downloadButton("haplot_out_down"),style="float:right")
              )
              
      ),
      ################################################################################3
      tabItem(tabName = "counts",
              "Based on the built map, we can count the number of recombination breakpoints in each individual of progeny using function progeny_haplotypes_counts. 
            Choose pipeline and the individuals you want to check the haplotypes.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Estimated number of recombination breakpoints for each individual",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("counts_out")
                       )
                ),
                column(width = 12,
                       box(title = "Real number of recombination breakpoints for each individual",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           hr(),
                           plotOutput("counts_simu_out"),
                           hr(),
                           actionButton("go13", "Update",icon("refresh")),
                       )
                ),
                
                box(solidHeader = T,
                    radioButtons("ErrorProb13", label = p("Genotyping method"),
                                 choices = maps_choice,
                                 selected = "updog"),
                ),
                box(solidHeader = T,
                    radioButtons("Global0.05.13", label = p("Error rate"),
                                 choices = global0.05_choices,
                                 selected = "FALSE"),
                ), 
                
                box(solidHeader = T,
                    radioButtons("SNPCall13", label = p("SNP calling method"),
                                 choices = SNPCall_choice,
                                 selected = "gatk"),
                ),
                box(solidHeader = T,
                    selectInput("seed13", label = p("Seed"),
                                choices = "It will be updated",
                                selected = "It will be updated"),
                ),
                box(solidHeader = T,
                    radioButtons("fake13", label = p("Allow false positives"),
                                 choices = fake_choices,
                                 selected = "without-false"),
                ),
                box(solidHeader = T,
                    radioButtons("CountsFrom13", label = p("Counts from"),
                                 choices = CountsFrom_choice,
                                 selected = "vcf"),
                ),
                div(downloadButton("counts_out_down"),style="float:right")
              )
      ),
      ################################################################################
      tabItem(tabName = "overview",
              "The table here highlights the main aspects of the built maps.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(
                         width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                         actionButton("go31", "Update",icon("refresh")),
                         hr(),
                         dataTableOutput("overview_out"),
                         hr(),
                         div(downloadButton("overview_down"),style="float:right")
                       )
                )
              ),
              fluidRow(
                column(width = 8.5,
                       box(solidHeader = T,
                           checkboxGroupInput("over_columns", label = p("Table columns"),
                                              choices = overview_choices,
                                              selected = c("geno", "conco_break")),
                       )
                ),
                column(width=6,
                       box(solidHeader = T,
                           radioButtons("display_overview", label = p("Display"),
                                        choices = list("all data" = "full", "Mean and SE" = "mean_se"),
                                        selected = "mean_se")
                       ),
                       box(solidHeader = T,
                           checkboxGroupInput("value", label = p("Display which value"),
                                              choices = list("Coefficient value" = "value", "p-value" = "p-value"),
                                              selected = "value"),
                       ),  
                       box(solidHeader = T,
                           selectInput("overview_depth", label = p("Choose depth for the plot"),
                                       choices = "This will be updated",
                                       selected = "This will be updated")
                       )
                )
              ),
              fluidRow(
                column(width = 12, 
                       box(
                         width = NULL, height = 900,
                         plotOutput("overview_plot_out"),
                         hr(),
                         div(downloadButton("overview_plot_down"),style="float:right")
                       )
                )
              ),
      ),
      ##########################################################
      # Empirical
      ##########################################################
      tabItem(tabName = "disper_depth_emp",
              "The same graphic is plotted in left and right to user be able to compare different methods choosing the options below.
            The x and y axis shows the read counts for reference and alternative alleles, respectively. 
            The colors of dots varies according with `Genotypes from` section. User can choose to see colors from simulated genotypes, the estimated or a gradient with the error rate used. 
            Users can download the left graphic pushing `Download` button.",
              hr(),
              fluidRow(
                column(width = 6,
                       box(title = "Depth and genotyping 1",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("disper_depth_emp_out"),   
                           hr(),
                           tableOutput("disper_depth_emp_table"),
                           actionButton("go16", "Update",icon("refresh")),
                       ),
                       
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           radioButtons("ErrorProb1_emp", label = p("Genotyping method"),
                                        choices = ErrorProb_choice,
                                        selected = "polyrad"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("real1_emp", label = p("Display the"),
                                        choices = list("estimated genotypes"="estimated_genotypes",
                                                       "errors rate" = "estimated_errors"),
                                        selected = "estimated_genotypes"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("geno_from1_emp", label = p("Genotypes from"),
                                        choices = list("vcf"="vcf",
                                                       "onemap" = "onemap"),
                                        selected = "onemap"),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.1_emp", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         fluidPage(
                           radioButtons("SNPCall1_emp", label = p("SNP calling method"),
                                        choices = SNPCall_choice,
                                        selected = "freebayes"),
                           hr()
                         ),
                         
                         #helpText("Read counts from:"),
                         fluidPage(
                           
                           radioButtons("CountsFrom1_emp", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr(),
                           div(downloadButton("disper_depth_emp_out_down"),style="float:right")
                           # ),
                         )
                       )
                ),
                
                column(width = 6,
                       box(title = "Depth and genotyping 2",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("disper_depth2_emp_out"),
                           hr(),
                           tableOutput("disper_depth2_emp_table"),
                           actionButton("go17", "Update",icon("refresh"))
                       ),
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           radioButtons("ErrorProb2_emp", label = p("Genotyping method"),
                                        choices = ErrorProb_choice,
                                        selected = "polyrad"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("real2_emp", label = p("Display the"),
                                        choices = list("estimated genotypes"="estimated_genotypes",
                                                       "estimated errors" = "estimated_errors"),
                                        selected = "estimated_genotypes"),
                           hr()
                         ),
                         
                         fluidPage(
                           radioButtons("geno_from2_emp", label = p("Genotypes from"),
                                        choices = list("vcf"="vcf",
                                                       "onemap" = "onemap"),
                                        selected = "onemap"),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.2_emp", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         fluidPage(
                           radioButtons("SNPCall2_emp", label = p("SNP calling method"),
                                        choices = SNPCall_choice,
                                        selected = "freebayes"),
                           hr()
                         ),
                         
                         
                         #helpText("Read counts from:"),
                         fluidPage(
                           
                           radioButtons("CountsFrom2_emp", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr()
                           # ),
                         )
                       )
                )
              )
      ),
      ##########################################################
      tabItem(tabName = "ind_size_emp",
              "The graphic show the distribuition of the difference between estimated and simulated distances between each pair markers of the generated maps.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Map size",
                           width = NULL,solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("ind_size_emp_out"),
                           hr(),
                           actionButton("go18", "Update",icon("refresh")),
                       )
                ),
                
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb3_emp", label = p("Genotyping method"),
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
                             checkboxGroupInput("SNPCall3_emp", label = p("SNP calling method"),
                                                choices = SNPCall_choice,
                                                selected = unlist(SNPCall_choice)),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("Global0.05.3_emp", label = p("Error rate"),
                                          choices = global0.05_choices,
                                          selected = "FALSE"),
                             hr()
                           ), 
                           fluidPage(
                             
                             radioButtons("CountsFrom3_emp", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr(),
                             div(downloadButton("ind_size_out_emp_down"),style="float:right")
                             # ),
                           )
                       )
                )
              )
      ),
      ###################################################################################
      tabItem(tabName = "marker_type_emp",
              "These bar plots describes the number of markers of each type according with Wu et. al 2002a that remained in the built maps of each method.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Marker type",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("marker_type_emp_out"), hr(),
                           actionButton("go19", "Update",icon("refresh")),
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb10_emp", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                         )
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("SNPCall10_emp", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                         ),
                         
                         fluidPage(
                           radioButtons("CountsFrom10_emp", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr(),
                           div(downloadButton("marker_type_emp_out_down"),style="float:right")
                         )
                       )
                )
              )
      ),
      ###################################################################################
      tabItem(tabName = "times_emp",
              "The graphic show the distribuition of times and number of markers of all families by depth.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Time spent to estimate genetic distances",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("times_emp_out"),
                           hr(),
                           actionButton("go20", "Update",icon("refresh")),
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb9_emp", label = p("Genotyping method"),
                                              choices = maps_choice,
                                              selected = names(maps_choice)),
                         ),
                         fluidPage(
                           checkboxGroupInput("SNPCall9_emp", label = p("SNP calling method"),
                                              choices = SNPCall_choice,
                                              selected = names(SNPCall_choice)),
                         )
                       )
                ),
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           radioButtons("Global0.05.9_emp", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         ), 
                         fluidPage(
                           radioButtons("CountsFrom9_emp", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr(),
                           div(downloadButton("times_emp_out_down"),style="float:right")
                         )
                       )
                )
              )
      ),
      ################################################################################3
      tabItem(tabName = "filters_emp",
              "The graphic here show the total number of markers (n_markers) available for analysis, 
            the number of markers filtered by OneMap because of missing data, segregation distortion and redundancy.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Fitlers",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("filters_emp_out"),
                           hr(),
                           actionButton("go22", "Update",icon("refresh")),
                       )
                ),
                
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb7_emp", label = p("Genotyping method"),
                                              choices = ErrorProb_choice,
                                              selected = unlist(ErrorProb_choice)),
                           hr()
                         ),
                         fluidPage(
                           radioButtons("Global0.05.7_emp", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                           hr()
                         )
                       )
                ),
                column(width = 6,
                       box(width = NULL, solidHeader = T,
                           fluidPage(
                             checkboxGroupInput("SNPCall7_emp", label = p("SNP calling method"),
                                                choices = SNPCall_choice,
                                                selected = names(SNPCall_choice)),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("CountsFrom7_emp", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr(),
                             div(downloadButton("filters_emp_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      ################################################################################3
      tabItem(tabName = "heatmaps_emp",
              fluidRow(
                column(width = 12,
                       box(title = "Interactive recombination fraction heatmap",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotlyOutput("heatmaps_emp_out", height = 650),hr(),
                           actionButton("go23", "Update",icon("refresh")),
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(solidHeader = T,
                           width = 6, 
                           fluidPage(
                             radioButtons("ErrorProb8_emp", label = p("Genotyping method"),
                                          choices = ErrorProb_choice,
                                          selected = "polyrad"),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("Global0.05.8_emp", label = p("Error rate"),
                                          choices = global0.05_choices,
                                          selected = "FALSE"),
                             hr()
                           )
                       ) 
                ),
                column(width = 6,
                       box(width = 6,solidHeader = T,
                           fluidPage( 
                             radioButtons("SNPCall8_emp", label = p("SNP calling method"),
                                          choices = SNPCall_choice,
                                          selected = "freebayes"),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("CountsFrom8_emp", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr(),
                             div(downloadButton("heatmaps_emp_out_down"),style="float:right")
                           )
                       )
                )
              )
      ),
      ################################################################################3
      tabItem(tabName = "map_emp",
              "On the left, it is plotted the heatmap graphic with recombination fraction varying in color according with the 
            intensity of the linkage. Higher is the recombination fraction hottest is the color. Markers ordered by genome positions are represented in x 
            and y axis. On the right, the linkage group is plotted with distances between markers proportional to the genetic distances. Pressing the 
            'Download' button, it will download the RData sequenced resulted from the selected 'Genotype method', 'SNP calling method', 'Error rate' and 'Counts from'.",
              hr(),
              fluidRow(
                column(width = 8,
                       box(title = "Recombination fraction heatmap",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("map1_emp_out"),hr(),
                           actionButton("go25", "Update",icon("refresh")), 
                       ),
                       box(solidHeader = T,
                           radioButtons("ErrorProb11_emp", label = p("Genotyping method"),
                                        choices = maps_choice,
                                        selected = "updog"),
                       ),
                       box(solidHeader = T,
                           radioButtons("SNPCall11_emp", label = p("SNP calling method"),
                                        choices = SNPCall_choice,
                                        selected = "gatk"),
                       ),
                       box(solidHeader = T,
                           radioButtons("Global0.05.11_emp", label = p("Error rate"),
                                        choices = global0.05_choices,
                                        selected = "FALSE"),
                       ), 
                       box(solidHeader = T,
                           radioButtons("CountsFrom11_emp", label = p("Counts from"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           div(downloadButton("map_emp_out_down"),style="float:right")
                       )
                ),
                column(width = 4,
                       box(title = "Genetic map",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           actionButton("go24", "Update",icon("refresh")),
                           imageOutput("map_emp_out", width = "100%", height = "100%")
                       )
                )
              )
      ),
      ################################################################################
      tabItem(tabName = "haplo_emp",
              "Based on the built map, progeny haplotypes can be draw by onemap function progeny_haplotypes. Choose pipeline and the individuals you want to check the haplotypes.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Individual haplotype",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("haplot_emp_out"),
                           hr(),
                           actionButton("go26", "Update",icon("refresh")))
                ),
                box(solidHeader = T,
                    radioButtons("ErrorProb12_emp", label = p("Genotyping method"),
                                 choices = maps_choice,
                                 selected = "updog"),
                ),
                box(solidHeader = T,
                    radioButtons("Global0.05.12_emp", label = p("Error rate"),
                                 choices = global0.05_choices,
                                 selected = "FALSE"),
                ), 
                box(solidHeader = T,
                    radioButtons("Most_likely12_emp", label = p("Choose how to show genotypes probabilities"),
                                 choices = list("The most likely genotypes receiveis maximum probabilities" = TRUE,
                                                "Genotypes probabilities are how they were outputted by HMM"= FALSE),
                                 selected = "TRUE"),
                ), 
                
                box(solidHeader = T,
                    radioButtons("SNPCall12_emp", label = p("SNP calling method"),
                                 choices = SNPCall_choice,
                                 selected = "gatk"),
                ),
                box(solidHeader = T,
                    radioButtons("CountsFrom12_emp", label = p("Counts from"),
                                 choices = CountsFrom_choice,
                                 selected = "vcf"),
                ),
                box(solidHeader = T, collapsible = T,
                    checkboxGroupInput("inds12_emp", label = p("Individuals from progeny"),
                                       choices = "It will be updated",
                                       selected = "It will be updated"),
                ),
                div(downloadButton("haplot_emp_out_down"),style="float:right")
              )
              
      ),
      ################################################################################3
      tabItem(tabName = "counts_emp",
              "Based on the built map, we can count the number of recombination breakpoints in each individual of progeny using function progeny_haplotypes_counts. 
            Choose pipeline and the individuals you want to check the haplotypes.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Breakpoints counts",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           "Estimated number of recombination breakpoints for each individual",
                           hr(),
                           plotOutput("counts_emp_out"),
                           hr(),
                           actionButton("go27", "Update",icon("refresh")))
                ),
                box(solidHeader = T,
                    radioButtons("ErrorProb13_emp", label = p("Genotyping method"),
                                 choices = maps_choice,
                                 selected = "updog"),
                ),
                box(solidHeader = T,
                    radioButtons("Global0.05.13_emp", label = p("Error rate"),
                                 choices = global0.05_choices,
                                 selected = "FALSE"),
                ), 
                
                box(solidHeader = T,
                    radioButtons("SNPCall13_emp", label = p("SNP calling method"),
                                 choices = SNPCall_choice,
                                 selected = "gatk"),
                ),
                box(solidHeader = T,
                    radioButtons("CountsFrom13_emp", label = p("Counts from"),
                                 choices = CountsFrom_choice,
                                 selected = "vcf"),
                ),
                div(downloadButton("counts_emp_out_down"),style="float:right")
              )
      ),
      ################################################################################3
      tabItem(tabName = "cmxmb_emp",
              "The graphic here show the relation between the position of marker in centimorgan and their position in reference genome (Mb).",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "cM x Mb",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           plotOutput("cmxmb_emp_out"), hr(),
                           actionButton("go29", "Update",icon("refresh")),
                       )
                ),
                
                column(width = 6,
                       box(
                         width = NULL, solidHeader = TRUE,
                         fluidPage(
                           checkboxGroupInput("ErrorProb14_emp", label = p("Genotyping method"),
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
                             checkboxGroupInput("SNPCall14_emp", label = p("SNP calling method"),
                                                choices = SNPCall_choice,
                                                selected = unlist(SNPCall_choice)),
                             hr()
                           ),
                           fluidPage(
                             radioButtons("Global0.05.14_emp", label = p("Error rate"),
                                          choices = global0.05_choices,
                                          selected = "FALSE"),
                             hr()
                           ), 
                           fluidPage(
                             
                             radioButtons("CountsFrom14_emp", label = p("Counts from"),
                                          choices = CountsFrom_choice,
                                          selected = "vcf"),
                             hr(),
                             div(downloadButton("cmxmb_emp_down"),style="float:right")
                             # ),
                           )
                       )
                )
              )
      ),
      
      ################################################################################3
      tabItem(tabName = "overview_emp",
              "The table here highlights the main aspects of the built maps.",
              hr(),
              fluidRow(
                column(width = 12,
                       box(title = "Overview",
                           width = NULL, solidHeader = TRUE, collapsible = FALSE, status="primary",
                           actionButton("go30", "Update",icon("refresh")),
                           hr(),
                           div(downloadButton("overview_emp_down"),style="float:right")
                       )
                ),
                column(width = 12,
                       box(solidHeader = T,
                           checkboxGroupInput("over_emp_columns", label = p("Table columns"),
                                              choices = overview_emp_choices,
                                              selected = c("n_markers", "redundants")),
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(solidHeader = T,
                           width = 12,
                           plotOutput("overview_emp_plot_out"),
                           hr(),
                           div(downloadButton("overview_emp_plot_down"),style="float:right")
                       )
                )
              )
      ),
      ##############################################################################
      # Workflow times
      ##############################################################################
      tabItem(tabName = "wf_times",
              "Upload the log file generated by the cromwell + WDL workflow. It can be from simulations or empirical. We can upload multiple files.",
              hr(),
              fluidPage(
                box(solidHeader = T,
                    tags$h4(tags$b("Upload workflow log file")), br(),
                    # Copy the line below to make a file upload manager
                    fileInput("wflog", label = h6("slurm_<depth>.out"), multiple = T),
                    #tags$strong("This option is not avaible in this server. Please use:"), br(),
                    #tags$code("runGitHub('Cristianetaniguti/Reads2MapApp')")
                ),
                box(solidHeader = T,
                    # Copy the line below to make a select box 
                    selectInput("example_wf", label = h4(tags$b("Examples")), 
                                choices = list("P.tremula empirical SNP calling" = "populus_snpcalling",
                                               "P.tremula empirical maps with contaminant" = "populus_emp_cont",
                                               "P.tremula empirical maps without contaminant" = "populus_emp",
                                               "Eucalyptus empirical SNP calling" = "eucalyptus_snpcalling",
                                               "Eucalyptus empirical maps" = "eucalyptus_map",
                                               "P. tremula simulation 37cM of chromosome 10 -  5 families pop size 200 depth 10" = "populus_simu_fam5_pop200_depth10",
                                               "P. tremula simulation 37cM of chromosome 10 -  5 families pop size 200 depth 20" = "populus_simu_fam5_pop200_depth20"
                                ), 
                                selected = "populus_emp"),
                )
              ),
              box(solidHeader = T, collapsible = FALSE, status="primary",
                  title = "Time spent by each WDL task",
                  width = NULL,
                  actionButton("go28", "Update",icon("refresh")),
                  div(downloadButton("wf_out_down"),style="float:right"),
                  hr(),
                  plotlyOutput("wf_times_out", width = "100%", height = "100%"),
              )
      )
    )
  )
  
  ui <- dashboardPage(header, sidebar, body)
  
  ## Define server logic required to draw a histogram ----
  server <- function(input, output,session) {
    options(shiny.maxRequestSize=500*1024^2)
    ##################################################
    # Simulations - rearranging data
    ##################################################
    prepare_datas_simu <- function(x){
      # This function makes adjustments in the input tar.gz file to be processed inside the app
      # It returns six data objects and the app options in a list format
      withProgress(message = 'Reading simulations data', value = 0, {
        if(!is.null(x)){
          data.gz <- x[,4]
          path = "data/"
        } else { ######## Only the toy_sample in the package - the rest in server
          if(input$example_simu == "populus_200_bi_radinitio20"){
            data.gz <- c("inst/ext/simulations/RADinitio20/SimulatedReads_results_depth10pop200_bi_up.tar.gz",
                         "inst/ext/simulations/RADinitio20/SimulatedReads_results_depth20pop200_bi_up.tar.gz")
          } else if(input$example_simu == "populus_200_multi_radinitio20"){
            data.gz <- c("inst/ext/simulations/RADinitio20/SimulatedReads_results_depth10pop200_multi_up.tar.gz",
                         "inst/ext/simulations/RADinitio20/SimulatedReads_results_depth20pop200_multi_up.tar.gz")
          } else if(input$example_simu == "populus_200_bi_radinitio37"){
            data.gz <- c("inst/ext/simulations/RADinitio37/biallelics/SimulatedReads_results_depth10.tar.gz",
                         "inst/ext/simulations/RADinitio37/biallelics/SimulatedReads_results_depth20.tar.gz")
          } else if(input$example_simu == "populus_200_multi_radinitio37"){
            data.gz <- c("inst/ext/simulations/RADinitio37/multiallelics/SimulatedReads_results_depth10.tar.gz",
                         "inst/ext/simulations/RADinitio37/multiallelics/SimulatedReads_results_depth20.tar.gz")
          } else if(input$example_simu == "toy_sample_bi"){
            data.gz <- c("inst/ext/toy_sample_simu/biallelics/SimulatedReads_results_depth10.tar.gz",
                         "inst/ext/toy_sample_simu/biallelics/SimulatedReads_results_depth20.tar.gz")
          } else if(input$example_simu == "toy_sample_multi"){
            data.gz <- c("inst/ext/toy_sample_simu/multiallelics/SimulatedReads_results_depth10.tar.gz",
                         "inst/ext/toy_sample_simu/multiallelics/SimulatedReads_results_depth20.tar.gz")
          }
        }
        path_dir <- tempdir()
        list_files <- list()
        incProgress(0, detail = paste("Doing part", 1))
        for(i in 1:length(data.gz)){
          untar(data.gz[i], exdir = path_dir)
          list_files[[i]] <- untar(data.gz[i], list = T)
        }
        
        incProgress(0.25, detail = paste("Doing part", 2))
        list_files <- lapply(list_files, function(x) paste0(path_dir,"/", x))
        for_rm <- sapply(list_files, "[", 1)
        list_files <- lapply(list_files, "[", -1)
        list_files <- lapply(list_files, sort)
        
        # Data
        datas <- list()
        for(i in 1:length(list_files[[1]])){
          datas[[i]] <- sapply(list_files, "[", i)
        }
        
        ## Tables
        data1_depths_geno_prob <- data2_maps <- data3_filters <- vector()
        data4_times <- data5_SNPCall_efficiency <- simu_haplo <- vector()
        data6 <- names_rdatas  <- list()
        #seeds <- depths <- seeds_choices <- depths_choices <- vector()
        
        temp_name <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".llo")
        incProgress(0.5, detail = paste("Doing part", 3))
        for(i in 1:length(datas)){
          for(j in 1:length(datas[[i]])){
            if(all(grepl("gusmap_RDatas.RData", datas[[i]]))){
              temp1 <- load(datas[[i]][[j]])
              temp1 <- base::get(temp1)
              data6 <- c(data6, temp1)
            } else if(all(grepl("sequences.llo", datas[[i]]))){
              temp1 <- readList(datas[[i]][[j]])
              if(j == 1){
                saveList(temp1, file = temp_name, append = F, compress = T)
                inds <- rownames(temp1[[1]]$data.name$geno)
              } else {
                saveList(temp1, file = temp_name, append = T, compress = T)
              }
            } else if(all(grepl("names.tsv.gz", datas[[i]]))){
              temp1 <-  vroom(datas[[i]][[j]], delim = "\t")
              names_rdatas <- c(names_rdatas, temp1)
            } else {
              temp1 <-  vroom(datas[[i]][[j]], delim = "\t")
              name_temp <- unlist(strsplit(datas[[i]][[j]], "/"))
              name_temp <- unlist(strsplit(name_temp[length(name_temp)], "[.]"))[1]
              assign(name_temp, rbind(base::get(name_temp), temp1))
            }
          }
        }
        
        incProgress(0.75, detail = paste("Doing part", 4))
        temp <- unique(paste0(data2_maps$depth, "_", data2_maps$seed))
        seeds <- sapply(strsplit(temp, "_"), "[", 2)
        depths <- sapply(strsplit(temp, "_"), "[", 1)
        depths_choices <- as.list(depths)
        names(depths_choices) <- depths
        seed_depth <- unique(paste("Depth",data2_maps$depth, "seed", data2_maps$seed))
        temp_names <- seed_depth
        seeds_choices <- as.list(1:length(seed_depth))
        names(seeds_choices) <- temp_names
        inds_choices <- sort(inds)
        names(inds_choices) <- sort(inds)
        names_rdatas <- unlist(names_rdatas)
        names_rdatas <- names_rdatas[-grep("gusmap", names_rdatas)]
        
        data2_maps$fake[data2_maps$fake == "TRUE"] <- "with-false"
        data2_maps$fake[data2_maps$fake == "FALSE"] <- "without-false"
        
        data4_times$fake[data4_times$fake == "TRUE"] <- "with-false"
        data4_times$fake[data4_times$fake == "FALSE"] <- "without-false"
        
        result_list <- list("data1"=data1_depths_geno_prob, 
                            "data2"=data2_maps, 
                            "data3"=data3_filters, 
                            "data4"=data4_times, 
                            "data5"=data5_SNPCall_efficiency, 
                            "data6"=data6, 
                            "choices"=list(depths, seeds, seeds_choices, depths_choices, inds_choices),
                            "names"=names_rdatas, 
                            "haplo"=simu_haplo,
                            "sequence.llo"=temp_name)
        
        system(paste("rm -r", paste(for_rm, collapse = " ")))
      })
      result_list
    }
    
    datas_simu <- reactive({prepare_datas_simu(input$simulatedreads)})
    
    # Update choices of seed and depth according with the dataset chosen
    observe({
      seeds_choice <- datas_simu()[[7]][[3]] 
      depth_choice <- datas_simu()[[7]][[4]]
      inds_choice <- datas_simu()[[7]][[5]]
      
      updateSelectInput(session, "seed1",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      
      updateSelectInput(session, "seed2",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      
      updateSelectInput(session, "seed3",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      updateSelectInput(session, "seed_cmbymb",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      updateSelectInput(session, "seed11",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      updateSelectInput(session, "seed12",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      updateCheckboxGroupInput(session, "inds12",
                               label="Individuals from progeny",
                               choices = inds_choice,
                               selected=unlist(inds_choice)[1])
      
      updateSelectInput(session, "seed13",
                        label="Seed",
                        choices = seeds_choice,
                        selected=unlist(seeds_choice)[1])
      
      updateCheckboxGroupInput(session, "depth4",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
      
      updateCheckboxGroupInput(session, "depth_probs",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
      
      updateRadioButtons(session, "depth10",
                         label="Depth",
                         choices = depth_choice,
                         selected=names(depth_choice)[1])
      
      updateCheckboxGroupInput(session, "depth8",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
      
      updateCheckboxGroupInput(session, "depth9",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
      
      updateCheckboxGroupInput(session, "depth5",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
      
      updateCheckboxGroupInput(session, "depth6",
                               label="Depth",
                               choices = depth_choice,
                               selected=unlist(depth_choice))
      
      updateSelectInput(session, "depth7",
                        label="Depth",
                        choices = depth_choice,
                        selected=unlist(depth_choice)[1])
      
      updateSelectInput(session, "overview_depth",
                        label="Choose depth for the plot",
                        choices = depth_choice,
                        selected=unlist(depth_choice)[1])
    })
    
    ####################################################################
    # Empirical - - rearranging data
    ####################################################################
    
    prepare_datas_emp <- function(x){
      # This function makes adjustments in the input tar.gz file to be processed inside the app
      # It returns six data objects and the app options in a list format
      withProgress(message = 'Reading empirical data', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(!is.null(x)){
          data.gz <- x[,4]
          path = "data/"
        } else { ######## Available examples
          if(input$example_emp == "populus"){
            data.gz <- "inst/ext/empirical/populus/multiallelics/EmpiricalReads_results.tar.gz"
          } else  if(input$example_emp == "populus_multi"){
            data.gz <- "inst/ext/populus/multiallelics/without_contaminants/EmpiricalReads_results.tar.gz"
          } else  if(input$example_emp == "populus_cont"){
            data.gz <- "inst/ext/populus/biallelics/with_contaminants/EmpiricalReads_results.tar.gz"
          } else if(input$example_emp == "eucalyptus"){
            data.gz <- "inst/ext/eucalyptus/biallelics/EmpiricalReads_results.tar.gz"
          } else if(input$example_emp == "toy_sample"){
            data.gz <- "inst/ext/toy_sample_emp/temp/EmpiricalReads_results.tar.gz"
          } else if(input$example_emp == "toy_sample_multi"){
            data.gz <- "inst/ext/toy_sample_emp/multiallelics/EmpiricalReads_results.tar.gz"
          }
        }
        
        path_dir <- tempdir()
        list_files <- list()
        for(i in 1:length(data.gz)){
          untar(data.gz[i], exdir = path_dir)
          list_files[[i]] <- untar(data.gz[i], list = T)
        }
        
        incProgress(0.5, detail = paste("Doing part", 2))
        list_files <- lapply(list_files, function(x) paste0(path_dir,"/", x))
        list_files <- lapply(list_files, "[", -1)
        
        # Data
        datas <- list()
        for(i in 1:length(list_files[[1]])){
          datas[[i]] <- sapply(list_files, "[", i)
        }
        
        for_rm <- sapply(list_files, "[", -grep("sequences",datas))
        
        temp_dat <- readList(datas[[grep("sequences",datas)]], index = 1)
        inds <- rownames(temp_dat[[1]]$data.name$geno)
        inds_list <- as.list(1:length(inds))
        names(inds_list) <- paste0(inds, " (", inds, ")")
        
        data5 <- load(datas[[grep("gusmap_RDatas.RData", datas)]])
        data5 <- base::get(data5)
        
        ## Tables
        idx <- grep("multi_names", datas)
        if(length(idx) > 0){
          multi_names <- load(datas[[idx]])
          multi_names <- base::get(multi_names)
        } else { multi_names = 0}
        
        names_rdatas <- readRDS(datas[[grep("names.rds", datas)]])
        names_rdatas <- names_rdatas[-grep("gusmap", names_rdatas)]
        result_list <- list("data1" = readRDS(datas[[grep("data1_depths_geno_prob.rds", datas)]]), 
                            "data2" = readRDS(datas[[grep("data2_maps.rds", datas)]]), 
                            "data3" = readRDS(datas[[grep("data3_filters.rds", datas)]]), 
                            "data4" = readRDS(datas[[grep("data4_times.rd", datas)]]), 
                            "data5" = data5, 
                            "names" = names_rdatas, 
                            "ind_names" = inds_list,
                            "sequence.llo" = datas[[grep("sequences",datas)]],
                            "multi_names" = multi_names)
        
        system(paste("rm -r", paste(for_rm, collapse = " ")))
      })
      result_list
    }
    
    datas_emp <- reactive({prepare_datas_emp(input$empiricalreads)})
    
    observe({
      inds_choice <- datas_emp()[[7]]
      
      updateCheckboxGroupInput(session, "inds12_emp",
                               label="Individuals from progeny",
                               choices = inds_choice,
                               selected=unlist(inds_choice)[1])
    })
    ##################################################################
    # Simulations
    ##################################################################
    
    #######################
    # cM by MB
    ######################
    button_cmbymb <- eventReactive(input$go_cmbymb, {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        geno <- test_geno(input$Global0.05_cmbymb, input$ErrorProb_cmbymb)
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall_cmbymb) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed_cmbymb)]) %>%
          filter(CountsFrom == input$CountsFrom_cmbymb | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed_cmbymb)]) %>%
          filter(fake == input$fake_cmbymb)  %>% 
          select(pos, rf, poscM.norm, real.mks, SNPCall, GenoCall) %>%
          gather(key, value, -pos, -real.mks, -SNPCall, -GenoCall)
        
        data <- perfumaria(data)
        
        data
      })
    })
    
    output$cmbymb_out <- renderPlot({
      cmbymb(button_cmbymb())
    })
    
    #######################
    # Depth and genotyping
    #######################
    button1 <- eventReactive(input$go1, {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        #see utils.R
        geno <- test_geno_with_gus(input$Global0.05.1, input$ErrorProb1)
        stop_bam(input$CountsFrom1, input$ErrorProb1)
        
        # The plot with depths does not differentiate fake markers, 
        # they receive NA value in the simulated genotype field 
        data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall1) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed1)]) %>%
          filter(CountsFrom == input$CountsFrom1) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed1)])
        data[,8:9] <- apply(data[,8:9], 2, as.character)
        
        data <- perfumaria(data)
        
        if(input$marker1 != "Ex: Chr10_1000"){
          data <- data %>% filter(mks == input$marker1)
          alpha = 1
        } else {
          alpha = 0.3
        }
        
        if(length(which(is.na(data$ref))) > 0)
          data <- data[-which(is.na(data$ref)),]
        
        list(data, alpha)
      })
    })
    
    output$disper_depth_out <- renderPlot({
      errorProb_graph(button1()[[1]], input$real1, button1()[[2]])
    })
    
    ## download
    output$disper_depth_out_down <- downloadHandler(
      filename =  function() {
        paste("snp_genoype_call.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building left graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          #see utils.R
          geno <- test_geno_with_gus(input$Global0.05.1, input$ErrorProb1)
          stop_bam(input$CountsFrom1, input$ErrorProb1)
          
          # The plot with depths does not differentiate fake markers, 
          # they receive NA value in the simulated genotype field 
          data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
            filter(SNPCall == input$SNPCall1) %>%
            filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed1)]) %>%
            filter(CountsFrom == input$CountsFrom1) %>%
            filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed1)])
          data[,8:9] <- apply(data[,8:9], 2, as.character)
          
          data <- perfumaria(data)
          
          if(length(which(is.na(data$ref))) > 0)
            data <- data[-which(is.na(data$ref)),]
          
          incProgress(0.5, detail = paste("Doing part", 2))
          p <- errorProb_graph(data, input$real1)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    
    button2 <- eventReactive(input$go2, {
      withProgress(message = 'Building right graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        #see utils.R
        geno <- test_geno_with_gus(input$Global0.05.2, input$ErrorProb2)
        stop_bam(input$CountsFrom2, input$ErrorProb2)
        
        # The plot with depths does not differentiate fake markers, 
        # they receive NA value in the simulated genotype field 
        data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall2) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed2)]) %>%
          filter(CountsFrom == input$CountsFrom2) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed2)])
        data[,8:9] <- apply(data[,8:9], 2, as.character)
        
        data <- perfumaria(data)
        
        if(input$marker2 != "Ex: Chr10_1000"){
          data <- data %>% filter(mks == input$marker2)
          alpha = 1
        } else {
          alpha = 0.3
        }
        
        if(length(which(is.na(data$ref))) > 0)
          data <- data[-which(is.na(data$ref)),]
        
        list(data,alpha)
      })
    })
    
    output$disper_depth2_out <- renderPlot({
      errorProb_graph(button2()[[1]], input$real2, button2()[[2]])
    })
    
    #######################
    # Map size each family
    #######################
    button3 <- eventReactive(input$go3, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        # see utils.R
        geno <- test_geno(input$Global0.05.3, input$ErrorProb3)
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall3) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed3)]) %>%
          filter(CountsFrom == input$CountsFrom3 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed3)]) %>%
          filter(fake == input$fake1) 
        
        data <- perfumaria(data)
        
        # If there are fake markers the interval distances are plotted
        # If there are no fake markers the difference between estimated and simulated distances are plotted 
        if(input$fake1 == "without-false"){
          data <- data %>% mutate("diff (cM)" = sqrt(c(0,(poscM.norm[-1] - poscM.norm[-length(poscM.norm)]) -
                                                         (rf[-1] - rf[-length(rf)]))^2))
        } else {
          data <- data %>% mutate("diff (cM)" = sqrt(c(0, (rf[-1] - rf[-length(rf)]))^2))
        }
        
        data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
          summarise("n markers" = n()) 
        
        data <- merge(data, data_n) 
        
        list(data, data_n)
      })
    })
    
    output$ind_size_out <- renderPlot({
      ind_size_graph(button3()[[1]], button3()[[2]])
    })
    
    ## download
    output$ind_size_out_down <- downloadHandler(
      filename =  function() {
        paste("ind_size.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          # see utils.R
          geno <- test_geno(input$Global0.05.3, input$ErrorProb3)
          
          data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall3) %>%
            filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed3)]) %>%
            filter(CountsFrom == input$CountsFrom3 | 
                     (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
            filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed3)]) %>%
            filter(fake == input$fake1) 
          
          data <- perfumaria(data)
          
          # If there are fake markers the interval distances are plotted
          # If there are no fake markers the difference between estimated and simulated distances are plotted 
          if(input$fake1 == "with-false"){
            data <- data %>% mutate("diff" = sqrt(c(0, (rf[-1] - rf[-length(rf)]))^2))
          }
          
          data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
            summarise("n markers" = n()) 
          
          data <- merge(data, data_n) 
          
          incProgress(0.5, detail = paste("Doing part", 2))
          p <- ind_size_graph(data, data_n)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    
    ####################
    # Overview map size
    ####################
    button4 <- eventReactive(input$go4, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.4, input$ErrorProb4)
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall4) %>%
          filter(CountsFrom == input$CountsFrom4 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(depth %in% input$depth4) %>%
          filter(fake == input$fake2) %>%
          group_by(seed,GenoCall, SNPCall, CountsFrom, depth) 
        
        data <- perfumaria(data)
        
        # difference between markers
        if(input$fake2 == "with-false"){
          data <- data %>% mutate("diff" = sqrt(c(0, (rf[-1] - rf[-length(rf)]))^2))
        }
        
        data_n <- data %>%  summarise(`n markers` = n()) 
        
        data <- switch(input$stats1,
                       "euclidean_dist" = summarise(data, D = as.vector((((length(rf)-1)^(-1))*(t(rf-poscM.norm)%*%(rf-poscM.norm)))^{1/2})),
                       "mean" = summarise(data, `mean (cM)` = mean(diff, na.rm=T)),
                       "median" = summarise(data, `median (cM)` = median(diff, na.rm=T)),
                       "var" = summarise(data, `var (cM)` = var(diff, na.rm=T)),
                       "total" = summarise(data, `sum (cM)` = sum(diff, na.rm=T)),
                       "total_size" = summarise(data, `cumsum (cM)` = rf[length(rf)]))
        
        incProgress(0.5, detail = paste("Doing part", 2))
        list(data, data_n)
      })
    })
    
    output$all_size_out <- renderPlot({
      all_size_graph(button4()[[1]],button4()[[2]],input$stats1, input$fake2)
    })
    
    ## download
    output$all_size_out_down <- downloadHandler(
      filename =  function() {
        paste("all_size.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05.4, input$ErrorProb4)
          
          data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall4) %>%
            filter(CountsFrom == input$CountsFrom4 | 
                     (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
            filter(depth %in% input$depth4) %>%
            filter(fake == input$fake2) %>%
            group_by(seed,GenoCall, SNPCall, CountsFrom, depth) 
          
          data <- perfumaria(data)
          
          # difference between markers
          if(input$fake2 == "with-false"){
            data <- data %>% mutate("diff" = sqrt(c(0, (rf[-1] - rf[-length(rf)]))^2))
          }
          
          data_n <- data %>%  summarise(`n markers` = n()) 
          
          data <- switch(input$stats1,
                         "euclidean_dist" = summarise(data, D = as.vector((((length(rf)-1)^(-1))*(t(rf-poscM.norm)%*%(rf-poscM.norm)))^{1/2})),
                         "mean" = summarise(data, `mean (cM)` = mean(diff, na.rm=T)),
                         "median" = summarise(data, `median (cM)` = median(diff, na.rm=T)),
                         "var" = summarise(data, `var (cM)` = var(diff, na.rm=T)),
                         "total" = summarise(data, `sum (cM)` = sum(diff, na.rm=T)),
                         "total_size" = summarise(data, `cumsum (cM)` = rf[length(rf)]))
          
          p <- all_size_graph(data, data_n)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    
    #######################
    # Marker types
    #######################
    button5 <- eventReactive(input$go5, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        data <- datas_simu[[2]] %>% filter(GenoCall %in% input$ErrorProb10) %>%
          filter(SNPCall %in% input$SNPCall10) %>%
          filter(CountsFrom == input$CountsFrom10) %>%
          filter(depth == input$depth10) %>%
          filter(fake == input$fake3)
        
        data <- perfumaria(data)
        
        # Frequencies
        incProgress(0.5, detail = paste("Doing part", 2))
        data1 <- data %>%  group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth, real.mks) %>%
          summarise(n = n()) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n, -real.mks)
        
        n <- c(estimated="type", real="real.type")
        data1$key <- names(n)[match(data1$key, n)]
        
        if(input$fake3 == "with-false"){
          data1 <- data1[which(data1$key == "estimated"),]
        }
        
        list(data1)
      })
    })
    
    output$marker_type_out <- renderPlot({
      marker_type_graph(button5()[[1]])
    })
    
    ## download
    output$marker_type_out_down <- downloadHandler(
      filename =  function() {
        paste("marker_type.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          data <- datas_simu()[[2]] %>% filter(GenoCall %in% input$ErrorProb10) %>%
            filter(SNPCall %in% input$SNPCall10) %>%
            filter(CountsFrom == input$CountsFrom10) %>%
            filter(depth == input$depth10) %>%
            filter(fake == input$fake3)
          
          data <- perfumaria(data)
          
          # Frequencies
          incProgress(0.5, detail = paste("Doing part", 2))
          data1 <- data %>%  group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth, real.mks) %>%
            summarise(n = n()) %>%
            gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n, -real.mks)
          
          n <- c(estimated="type", real="real.type")
          data1$key <- names(n)[match(data1$key, n)]
          
          if(input$fake3 == "with-false"){
            data1 <- data1[which(data1$key == "estimated"),]
          }
          
          p <- marker_type_graph(data1)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    ##########################
    # Genotypes probabilities
    ##########################
    button_probs <- eventReactive(input$go_probs, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        ## parents
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% input$ErrorProb_probs) %>%
          filter(SNPCall %in% input$SNPCall_probs) %>%
          filter(CountsFrom %in% input$CountsFrom_probs) %>%
          filter(depth %in% input$depth_probs) %>%
          filter(fake == "without-false")
        
        data <- perfumaria(data)
        
        probs_plot <- list()
        data$method <- paste0(data$seed,"_", data$depth,"_", 
                              data$SNPCall, "_",data$CountsFrom,"_",
                              data$GenoCall)
        
        data <- mutate_if(data, is.character, as.factor)
        
        real.mks.sele <-c("true marker", "multiallelic")
        for(i in 1:2){
          data_prob <- data %>% filter(real.mks == real.mks.sele[i])
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
            probs_plot[[i]]$GenoCall[probs_plot[[i]]$GenoCall == "OneMap"] <- "OneMap_version2"
          }
        }
        
        incProgress(0.25, detail = paste("Doing part", 2))
        # progeny
        data <- datas_simu()[[1]] %>% filter(GenoCall %in% input$ErrorProb_probs) %>%
          filter(SNPCall %in% input$SNPCall_probs) %>%
          filter(CountsFrom %in% input$CountsFrom_probs) %>%
          filter(depth %in% input$depth_probs) 
        
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
      marker_type_probs(button_probs()[[1]])
    }, width = 1152, height = 1536)
    
    output$probs_pare_multi_out <- renderPlot({
      marker_type_probs(button_probs()[[2]])
    }, width = 1152, height = 1536)
    
    output$probs_prog_out <- renderPlot({
      geno_probs(button_probs()[[3]])
    }, width = 1152, height = 1536)
    
    #######################
    # Phases
    #######################
    button6 <- eventReactive(input$go6, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        geno <- test_geno(input$Global0.05.8, input$ErrorProb8)
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall8) %>%
          filter(CountsFrom == input$CountsFrom8 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(depth %in% input$depth8) %>%
          filter(fake == "without-false") %>%
          filter(real.mks == "true marker") # This graphic do not consider multiallelic markers because their were not simulated
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
          summarise(`n markers` = n()) 
        
        data1 <- data %>% group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
          summarise(`% correct`= 100*sum(est.phases == real.phases)/length(real.phases))
        
        
        data <- merge(data1, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$phases_out <- renderPlot({
      phases_graph(button6())
    })
    
    ## download
    output$phases_out_down <- downloadHandler(
      filename =  function() {
        paste("phases.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05.8, input$ErrorProb8)
          
          data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall8) %>%
            filter(CountsFrom == input$CountsFrom8 | 
                     (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
            filter(depth == input$depth8) %>%
            filter(fake == "without-false") %>%
            filter(real.mks == "true marker") # This graphic do not consider multiallelic markers because their were not simulated
          
          data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
            summarise(`n markers` = n()) 
          
          data1 <- data %>% group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
            summarise(`% correct`= 100*sum(est.phases == real.phases)/length(real.phases))
          
          data <- merge(data1, data_n) %>%
            gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
          
          data$depth <- paste0("depth ", as.character(data$depth))
          incProgress(0.5, detail = paste("Doing part", 2))
          data <- perfumaria(data)
          
          p <- phases_graph(data)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    ####################### 
    # Times
    #######################
    button7 <- eventReactive(input$go7, {
      withProgress(message = 'Times graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.9, input$ErrorProb9)
        
        data_n <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall9) %>%
          filter(CountsFrom == input$CountsFrom9 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(fake == input$fake5) %>%
          group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
          summarise(`n markers` = n()) 
        
        data <- datas_simu()[[4]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall9) %>%
          filter(fake == input$fake5) %>%
          filter(CountsFrom == input$CountsFrom9 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(depth %in% input$depth9)  
        
        data$times <- data$time/60
        
        data_df <- merge(data, data_n) 
        
        data <- data_df %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
        
        data$key <- gsub("time", "minutes", data$key)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        data <- perfumaria(data)
        
        data
      })
    })
    
    output$times_out <- renderPlot({
      times_graph(button7())
    })
    
    ## download
    output$times_out_down <- downloadHandler(
      filename =  function() {
        paste("times.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        incProgress(0, detail = paste("Doing part", 1))
        geno <- test_geno(input$Global0.05.9, input$ErrorProb9)
        
        data_n <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall9) %>%
          filter(CountsFrom == input$CountsFrom9 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(fake == input$fake5) %>%
          group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
          summarise(`n markers` = n()) 
        
        data <- datas_simu()[[4]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall9) %>%
          filter(fake == input$fake5) %>%
          filter(CountsFrom == input$CountsFrom9 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          filter(depth %in% input$depth9)  
        
        data_df <- merge(data, data_n) 
        
        data <- data_df %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
        
        data$key <- gsub("time", "seconds", data$key)
        
        perfumaria(data)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        
        p <- times_graph(data)      
        p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                       legend.text = element_text(size=17),
                       axis.title=element_text(size=17),
                       axis.text = element_text(size=17), 
                       strip.text = element_text(size=17))
        ggsave(file, p, width = 400, height = 200, units="mm")
      } 
    )
    ########################
    # SNP calling efficiency
    ########################
    button9 <- eventReactive(input$go9, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        data <- datas_simu()[[5]][,-c(8,9)]  %>%
          filter(SNPCall %in% input$SNPCall6) %>%
          filter(depth %in% input$depth6)  
        
        colnames(data)[4:7] <- c("real", "estimated", "true positives", "false positives")
        data$`false negatives` <- data$real - data$`true positives`
        
        data <- data  %>% pivot_longer(cols=c(4:8))
        
        incProgress(0.5, detail = paste("Doing part", 2))
        # perfurmaria
        snpcall <- c(GATK = "gatk", freebayes = "freebayes")
        data$SNPCall <- names(snpcall)[match(data$SNPCall, snpcall)]
        
        data$name <- factor(data$name, levels = c("real", "estimated", "true positives", "false positives", "false negatives"))
        
        data
      })
    })
    
    output$snpcall_out <- renderPlot({
      avalSNPs_graph(button9())
    })
    
    ## download
    output$snpcall_out_down <- downloadHandler(
      filename =  function() {
        paste("snpcall.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        data <- datas_simu()[[5]][,-c(8,9)]  %>%
          filter(SNPCall %in% input$SNPCall6) %>%
          filter(depth %in% input$depth6)  
        
        colnames(data)[4:7] <- c("real", "estimated", "true positives", "false positives")
        data$`false negatives` <- data$real - data$`true positives`
        
        data <- data  %>% pivot_longer(cols=c(4:8))
        
        incProgress(0.5, detail = paste("Doing part", 2))
        # perfurmaria
        snpcall <- c(GATK = "gatk", freebayes = "freebayes")
        data$SNPCall <- names(snpcall)[match(data$SNPCall, snpcall)]
        
        data$name <- factor(data$name, levels = c("real", "estimated", "true positives", "false positives", "false negatives"))
        
        p <- avalSNPs_graph(data)
        p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                       legend.text = element_text(size=17),
                       axis.title=element_text(size=17),
                       axis.text = element_text(size=17), 
                       strip.text = element_text(size=17))
        ggsave(file, p, width = 400, height = 200, units="mm")
      } 
    )
    #######################
    # Filters
    #######################
    button10 <- eventReactive(input$go10, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.7, input$ErrorProb7)
        
        data <- datas_simu()[[3]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall7) %>%
          filter(depth == input$depth7) %>%
          filter(CountsFrom %in% input$CountsFrom7 | 
                   (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
          pivot_longer(cols=1:5)
        
        n <- c(`Without filters`="n_markers",
               `Missing data`= "higher.than.25..missing",
               `Segregation test`="distorted_markers",
               `Redundants`="redundant_markers",
               `After all filters` = "after_filters")
        
        data$name <- names(n)[match(data$name, n)]
        data$name <- factor(data$name, levels = c("Without filters",
                                                  "Missing data",
                                                  "Segregation test",
                                                  "Redundants",
                                                  "After all filters"))
        incProgress(0.5, detail = paste("Doing part", 2))
        perfumaria(data)
      })
    })
    
    output$filters_out <- renderPlot({
      filters_graph(button10())
    })  
    
    ## download
    output$filters_out_down <- downloadHandler(
      filename =  function() {
        paste("filters.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05.7, input$ErrorProb7)
          
          data <- datas_simu()[[3]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall7) %>%
            filter(depth == input$depth7) %>%
            filter(CountsFrom == input$CountsFrom7 | 
                     (CountsFrom == "vcf" & GenoCall %in% c("SNPCaller", "OneMap_version2", "SNPCaller0.05"))) %>%
            pivot_longer(cols=1:5)
          
          n <- c(`Without filters`="n_markers",
                 `Missing data`= "higher.than.25..missing",
                 `Segregation test`="distorted_markers",
                 `Redundants`="redundant_markers",
                 `After all filters` = "after_filters")
          
          data$name <- names(n)[match(data$name, n)]
          data$name <- factor(data$name, levels = c("Without filters",
                                                    "Missing data",
                                                    "Segregation test",
                                                    "Redundants",
                                                    "After all filters"))
          incProgress(0.5, detail = paste("Doing part", 2))
          data <- perfumaria(data)
          p <- filters_graph(data)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    #######################
    # Maps
    #######################
    button11 <- eventReactive(input$go11, {
      withProgress(message = 'Building draw', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.11, input$ErrorProb11)
        
        incProgress(0.25, detail = paste("Doing part", 2))
        
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall11) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed11)]) %>%
          filter(CountsFrom == input$CountsFrom11) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed11)]) %>%
          filter(fake == input$fake11)
        
        filename <- paste0(data$SNPCall, "_", data$CountsFrom, "_", geno, ".png")
        filename <- unique(filename)
        
        incProgress(0.5, detail = paste("Doing part", 3))
        if(input$fake11 == "with-false"){
          false_mks <- as.character(data$mk.name[data$real.mks == "false positive"])
          data <- data.frame(data$mk.name, data$rf)
        } else {
          data <- data.frame(data$mk.name, data$rf)
        }
        list(data,filename)
      })
    })
    
    output$map1_out <- renderImage({
      if(input$fake11 == "with-false"){
        draw_map2(button11()[[1]], output = button11()[[2]], 
                  tag = false_mks, col.tag = "darkblue", 
                  pos = T, id = F)  
      } else {
        draw_map2(button11()[[1]], output = button11()[[2]])
      }
      
      list(src = button11()[[2]],
           contentType = 'image/png',
           width = 200,
           height = 900)
    }, deleteFile = TRUE) # change here
    
    button11.1 <- eventReactive(input$go11.1, {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.11, input$ErrorProb11)
        stop_bam(input$CountsFrom11, input$ErrorProb11)
        
        depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed11)])
        seed <- datas_simu()[[7]][[2]][as.numeric(input$seed11)]
        if(input$fake11 == "with-false") fake <- T else fake <- F
        temp_n <- paste0("map_",seed, "_", depth, "_", input$SNPCall11, "_", 
                         input$CountsFrom11, "_", geno, "_", fake)
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          data <- datas_simu()[[6]][[temp_n]]
          data$rf_2pt()
          incProgress(0.5, detail = paste("Doing part", 3))
        } else {
          idx <- which(datas_simu()[[8]] == temp_n)
          data <- readList(datas_simu()[[10]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
        }
        list(data, geno)
      })
    })
    
    output$map_out <- renderPlot({
      if(button11.1()[[2]] == "gusmap"){
        button11.1()[[1]]$plotChr(mat="rf", parent = "both")
      } else {
        rf_graph_table(button11.1()[[1]], inter = F, mrk.axis = "none")
      }
    })
    
    ## download
    output$map1_out_down <- downloadHandler(
      filename =  function() {
        paste("map.RData")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Loading data', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05.11, input$ErrorProb11)
          
          if(input$CountsFrom11 == "bam" & (input$ErrorProb11 == "OneMap_version2" | input$ErrorProb11 == "SNPCaller")){
            stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
          }
          
          depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed11)])
          seed <- datas_simu()[[7]][[2]][as.numeric(input$seed11)]
          if(input$fake11 == "with-false") fake <- T else fake <- F
          temp_n <- paste0("map_",seed, "_", depth, "_", input$SNPCall11, "_", 
                           input$CountsFrom11, "_", geno, "_", fake)
          
          if(geno == "gusmap"){
            data <- datas_simu()[[6]][[temp_n]]
          } else {
            idx <- which(datas_simu()[[8]] == temp_n)
            data <- readList(datas_simu()[[10]], index = idx)
            class(data) <- "sequence"
          }
          incProgress(0.5, detail = paste("Doing part", 2))
          outfile <- paste0(temp_n, ".RData")
          save(data, file = outfile)
        })
      } 
    )
    
    #######################
    # Progeny haplotypes
    #######################
    button12 <- eventReactive(input$go12, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.12, input$ErrorProb12)
        
        if(input$CountsFrom12 == "bam" & (input$ErrorProb12 == "OneMap_version2" | input$ErrorProb12 == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        
        depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed12)])
        seed <- datas_simu()[[7]][[2]][as.numeric(input$seed12)]
        if(input$fake12 == "with-false") fake <- T else fake <- F
        temp_n <- paste0("map_",seed, "_", depth, "_", input$SNPCall12, "_", 
                         input$CountsFrom12, "_", geno, "_", fake)
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          stop("We do not include in this app support to do it with GUSMap. Please, choose other option.")
        } else {
          idx <- which(datas_simu()[[8]] == temp_n)
          data <- readList(datas_simu()[[10]], index = idx)
          data <- data[[1]][[1]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
          idx <- which(rownames(data$data.name$geno) %in% input$inds12)
          list(data,idx)
        }
      })
    })
    
    output$haplot_out <- renderPlot({
      plot(progeny_haplotypes(button12()[[1]], ind = button12()[[2]], most_likely = input$Most_likely12))
    })
    
    button12.1 <- eventReactive(input$go12.1, {
      withProgress(message = 'Building graphic', value = 0, {
        sub_dat <- subset(datas_simu()[[9]], datas_simu()[[9]][[2]] == datas_simu()[[7]][[1]][as.numeric(input$seed12)] & 
                            datas_simu()[[9]][[1]] == datas_simu()[[7]][[2]][as.numeric(input$seed12)] &
                            datas_simu()[[9]][[3]] %in% input$inds12)
        
        sub_dat <- sub_dat[,-c(1,2)]
        class(sub_dat) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
        sub_dat
      })
    })
    
    output$haplot_simu_out <- renderPlot({
      plot(button12.1())
    })
    
    ## download
    output$haplot_out_down <- downloadHandler(
      filename =  function() {
        paste("haplotypes.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05.12, input$ErrorProb12)
          
          if(input$CountsFrom12 == "bam" & (input$ErrorProb12 == "OneMap_version2" | input$ErrorProb12 == "SNPCaller")){
            stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
          }
          
          depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed12)])
          seed <- datas_simu()[[7]][[2]][as.numeric(input$seed12)]
          if(input$fake12 == "with-false") fake <- T else fake <- F
          temp_n <- paste0("map_",seed, "_", depth, "_", input$SNPCall12, "_", 
                           input$CountsFrom12, "_", geno, "_", fake)
          
          incProgress(0.25, detail = paste("Doing part", 2))
          if(geno == "gusmap"){
            stop("We do not include in this app support to do it with GUSMap. Please, choose other option.")
          } else {
            idx <- which(datas_simu()[[8]] == temp_n)
            data <- readList(datas_simu()[[10]], index = idx)
            data <- data[[1]]
            class(data) <- "sequence"
            incProgress(0.5, detail = paste("Doing part", 3))
            p <-  plot(progeny_haplotypes(data, ind = as.numeric(input$inds12), most_likely = input$Most_likely12), position = "split")
            ggsave(file, p, width = 400, height = 200, units="mm")
          }
        })
      } 
    )
    #######################
    # Breakpoints count 
    #######################
    button13 <- eventReactive(input$go13, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        geno <- test_geno(input$Global0.05.13, input$ErrorProb13)
        
        stop_bam(input$CountsFrom13, input$ErrorProb13)
        
        depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed13)])
        seed <- datas_simu()[[7]][[2]][as.numeric(input$seed13)]
        if(input$fake13 == "with-false") fake <- T else fake <- F
        temp_n <- paste0("map_",seed, "_", depth, "_", input$SNPCall13, "_", 
                         input$CountsFrom13, "_", geno, "_", fake)
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          stop("We do not include in this app support to do it with GUSMap. Please, choose other option.")
        } else {
          idx <- which(datas_simu()[[8]] == temp_n)
          data <- readList(datas_simu()[[10]], index = idx)
          data <- data[[1]][[1]]
          incProgress(0.5, detail = paste("Doing part", 3))
          inds <- 1:data$data.name$n.ind
          df <- progeny_haplotypes(data, ind = inds, most_likely = T)
          data1 <- progeny_haplotypes_counts(df)
          
          ## Correct
          sub_dat <- subset(datas_simu()[[9]], datas_simu()[[9]][[2]] == datas_simu()[[7]][[1]][as.numeric(input$seed13)] & 
                              datas_simu()[[9]][[1]] == datas_simu()[[7]][[2]][as.numeric(input$seed13)])
          
          sub_dat <- sub_dat[,-c(1,2)]
          class(sub_dat) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
          data2 <- progeny_haplotypes_counts(x = sub_dat)
          
          data1$grp <- unique(data2$grp)
          
          list(data1, data2)
        }
      })
    })
    
    output$counts_out <- renderPlot({
      plot(button13()[[1]])
    })
    
    output$counts_simu_out <- renderPlot({
      plot(button13()[[2]])
    })
    
    ## download
    output$counts_out_down <- downloadHandler(
      filename =  function() {
        paste("haplotypes.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          geno <- test_geno(input$Global0.05.13, input$ErrorProb13)
          
          stop_bam(input$CountsFrom13, input$ErrorProb13)
          
          depth <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed13)])
          seed <- datas_simu()[[7]][[2]][as.numeric(input$seed13)]
          if(input$fake13 == "with-false") fake <- T else fake <- F
          temp_n <- paste0("map_",seed, "_", depth, "_", input$SNPCall13, "_", 
                           input$CountsFrom13, "_", geno, "_", fake)
          
          incProgress(0.25, detail = paste("Doing part", 2))
          if(geno == "gusmap"){
            stop("We do not include in this app support to do it with GUSMap. Please, choose other option.")
          } else {
            idx <- which(datas_simu()[[8]] == temp_n)
            data <- readList(datas_simu()[[10]], index = idx)
            data <- data[[1]][[1]]
            incProgress(0.5, detail = paste("Doing part", 3))
            inds <- 1:data$data.name$n.ind
            df <- progeny_haplotypes(data, ind = inds, most_likely = T)
            p <- plot(progeny_haplotypes_counts(df), n.graphics = 5, ncol = 5)
            ggsave(file, p, width = 400, height = 200, units="mm")
          }
        })
      } 
    )
    #######################
    # Overview
    #######################
    # button31 <- eventReactive(input$go31, {
    #   
    #   withProgress(message = 'Building graphic', value = 0, {
    #     incProgress(0, detail = paste("Doing part", 1))
    #     
    #     data <- datas_simu()[[1]]
    #     data[,8:9] <- apply(datas_simu()[[1]][,8:9], 2, as.character)
    #     incProgress(0.1, detail = paste("Doing part", 2))
    # 
    #     # Why are there NAs in ref and alt of polyrad?
    #     if(length(which(is.na(data$ref))) > 0)
    #       data <- data[-which(is.na(data$ref)),]
    #     incProgress(0.15, detail = paste("Doing part", 3))
    #     data$gab <- as.numeric(factor(data$gabGT, levels=c("homozygote-ref", "heterozygote", "homozygote-alt"))) 
    #     data$est <- as.numeric(factor(data$methGT, levels=c("homozygote-ref", "heterozygote", "homozygote-alt", "missing")))
    #     
    #     data <- data %>% group_by(SNPCall, GenoCall, CountsFrom, depth, seed) %>%
    #       summarise(kappa_phases = agree_coefs(cbind(gab,est), method = "kappa")) %>% ungroup()
    #     
    #     genotype <- cbind(data[,1:5], data$kappa_phases[,1:2])
    #     colnames(genotype)[6:7] <- c("Value", "Kappa's coefficient for genotypes")
    #     
    #     data <- datas_simu()[[2]][-which(datas_simu()[[2]]$fake =="with-false"),]
    #     data1 <- data %>% group_by(GenoCall, SNPCall, CountsFrom, depth, seed) %>%
    #       summarise(kappa_phases= agree_coefs(cbind(est.phases, real.phases), method = "kappa"),
    #                 kappa_markers= agree_coefs(cbind(type, real.type), method = "kappa"),
    #                 non_informative = (sum(real.type=="non-informative")/length(real.type))*100,
    #                 map_size = rf[length(rf)],
    #                 n_markers = n()) %>% 
    #       ungroup()
    #     incProgress(0.2, detail = paste("Doing part", 4))
    #     data1 <- cbind(data1[,1:5], 
    #                    data1$kappa_phases[,1:2], 
    #                    data1$kappa_markers[,2], 
    #                    data1$non_informative,
    #                    data1$map_size,
    #                    data1$n_markers)
    #     
    #     colnames(data1)[6:11] <- c("Value", "Kappa's coefficient for phases",
    #                                "Kappa's coefficient for marker types",
    #                                "Percentage of non-informative markers", 
    #                                "Map size (cM)",
    #                                "Number markers in map")
    #     
    #     df_overview <- merge(genotype, data1)
    #     
    #     only_true <- seq(1,length(datas_simu()[[8]]),2)
    #     part<-0.7/length(datas_simu()[[8]][only_true])
    #     parts <- seq(0.2,0.9, part)[-1]
    #     
    #     df_breakpoints <- data.frame()
    #     z <- 1
    #     for(i in only_true){
    #       #incProgress(0.2+parts[z], detail = paste("Doing part", 4+z))
    #       z <- z + 1
    #       ID <- unlist(strsplit(datas_simu()[[8]][i], "_"))
    #       data <- readList(datas_simu()[[10]], index = i)
    #       data <- data[[1]]
    #       class(data) <- "sequence"
    #       inds <- 1:data$data.name$n.ind
    #       df <- progeny_haplotypes(data, ind = inds, most_likely = T)
    #       counts <- progeny_haplotypes_counts(df)
    #       
    #       df_correct <- datas_simu()[[9]] %>% filter(seed == ID[1]) %>%
    #         filter(depth == ID[2]) 
    #       
    #       sub_dat <- df_correct[,-c(1,2)]
    #       class(sub_dat) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
    #       data2 <- progeny_haplotypes_counts(x = sub_dat)
    #       
    #       counts$grp <- unique(data2$grp)
    #       temp_list <- merge(counts, data2, by = c("ind", "grp", "homologs"))
    #       
    #       m <- cbind(est = temp_list[,4], gab = temp_list[,5])
    #       
    #       data3 <- agree_coefs(m, method = c("kendall.concor","kendall.corr"))
    #       
    #       breakpoints <- data.frame(seed = ID[1], depth = ID[2], SNPCall = ID[4], 
    #                                 CountsFrom = ID[5], GenoCall = ID[6], data3[,c(1,2,3)])
    #       
    #       df_breakpoints <- rbind(df_breakpoints, breakpoints)
    #     }
    #     
    #     incProgress(0.95, detail = paste("Doing last part"))
    #     colnames(df_breakpoints)[6:8] <- c("Value", "Kendall's coefficient of concordance for breakpoints",
    #                                        "Kendall's coefficient of correlation for breakpoints")
    #     
    #     df_breakpoints$GenoCall <- as.character(df_breakpoints$GenoCall)
    #     df_breakpoints$GenoCall[df_breakpoints$GenoCall == "default0.05"] <- "SNPCaller0.05"
    #     df_breakpoints$GenoCall[df_breakpoints$GenoCall == "default"] <- "OneMap_version2"
    #     
    #     df_overview <- merge(df_overview, df_breakpoints)
    #     
    #     
    #     time <- datas_simu()[[4]] %>% filter(fake == "without-false")
    #     time <- time[,-6] 
    #     
    #     df_overview <- merge(df_overview, time)
    #     df_overview
    #   })
    # })
    # 
    # output$overview_plot_out <- renderPlot({
    #   df_overview <- button31()
    #   over_graph <- overview_graph(df_overview, input$overview_depth)
    #   annotate_figure(over_graph,
    #                   top = text_grob("Overview", face = "bold", size = 14))
    # }, width = 1000, height = 670)
    # 
    # 
    # ## download
    # output$overview_plot_down <- downloadHandler(
    #   filename =  function() {
    #     paste("overview.eps")
    #   },
    #   # content is a function with argument file. content writes the plot to the device
    #   content = function(file) {
    #     df_overview <- button31()
    #     p <- overview_graph(df_overview, depth = input$overview_depth)
    #     ggsave(p, filename = file, width = 220, height = 220, units = "mm")
    #   }
    # )    
    # 
    # 
    # output$overview_out <- renderDataTable({
    #   df_overview <- button31()
    #   choices <- c("geno", "phases", "marker", "noninfo", "mapsize",
    #                "nmarker", "conco_break", "corr_break", "time")
    #   
    #   if (input$display_overview == "full") {
    #     colnames(df_overview)[1:3] <- c("SNP caller","Genotype caller","Read counts from")
    #     
    #     idx_columns <- which(choices %in% input$over_columns)
    #     idx_columns <- idx_columns + 6
    #     data_display <- df_overview[, c(1:6, idx_columns)]
    #     
    #   } else {
    #     colnames(df_overview)[7:15] <-  c("geno", "phases", "marker", "noninfo", "mapsize",
    #                                       "nmarker", "conco_break", "corr_break", "time")
    #     df_overview$time <- as.numeric(as.character(df_overview$time))
    #     # Include statistics 
    #     data_display <- df_overview %>% group_by(depth, SNPCall, GenoCall, CountsFrom, Value) %>%
    #       summarise(geno_mean = mean(geno),
    #                 geno_se = sd(geno)/sqrt(length(geno)),
    #                 phases_mean = mean(phases),
    #                 phases_se = sd(phases)/sqrt(length(phases)),
    #                 marker_mean = mean(marker),
    #                 marker_se = sd(marker)/sqrt(length(marker)),
    #                 noninfo_mean = mean(noninfo),
    #                 noninfo_se = sd(noninfo)/sqrt(length(noninfo)),
    #                 mapsize_mean = mean(mapsize),
    #                 mapsize_se = sd(mapsize)/sqrt(length(mapsize)),
    #                 nmarker_mean = mean(nmarker),
    #                 nmarker_se = sd(nmarker)/sqrt(length(nmarker)),
    #                 conco_break_mean = mean(conco_break),
    #                 conco_break_se = sd(conco_break)/sqrt(length(conco_break)),
    #                 corr_break_mean = mean(corr_break),
    #                 corr_break_se = sd(corr_break)/sqrt(length(corr_break)),
    #                 time_mean = mean(time),
    #                 time_se = sd(time)/sqrt(length(time))) %>% ungroup()
    #     
    #     colnames(data_display)[6:23] <- c("Kappa's coefficient for genotypes mean",
    #                                       "Kappa's coefficient for genotypes se",
    #                                       "Kappa's coefficient for phases mean",
    #                                       "Kappa's coefficient for phases se",
    #                                       "Kappa's coefficient for marker types mean",
    #                                       "Kappa's coefficient for marker types se",
    #                                       "Percentage of non-informative markers mean", 
    #                                       "Percentage of non-informative markers se", 
    #                                       "Map size (cM) mean",
    #                                       "Map size (cM) se",
    #                                       "Number markers in map mean",
    #                                       "Number markers in map se",
    #                                       "Kendall's coefficient of concordance for breakpoints mean",
    #                                       "Kendall's coefficient of concordance for breakpoints se",
    #                                       "Kendall's coefficient of correlation for breakpoints mean",
    #                                       "Kendall's coefficient of correlation for breakpoints se",
    #                                       "Time spent (s) mean",
    #                                       "Time spent (s) se")
    #     choices <- rep(choices, each=2)
    #     idx_columns <- which(choices %in% input$over_columns)
    #     idx_columns <- idx_columns + 5
    #     data_display <- data_display[, c(1:5, idx_columns)]
    #     colnames(data_display)[2:4] <- c("SNP caller","Genotype caller","Read counts from")
    #   }
    #   
    #   idx <- c("value", "p-value") %in% input$value
    #   if(all(idx)){
    #     df_sele <- data_display
    #   } else if(idx[1] == TRUE){
    #     df_sele <- data_display %>% filter(Value == "value")
    #   } else {
    #     df_sele <- data_display %>% filter(Value == "p-value")
    #   }
    #   df_sele
    # })
    # 
    # ## download
    # output$overview_down <- downloadHandler(
    #   filename =  function() {
    #     paste("overview.rds")
    #   },
    #   # content is a function with argument file. content writes the plot to the device
    #   content = function(file) {
    #     df_overview <- button31()
    #     choices <- c("geno", "phases", "marker", "noninfo", "mapsize",
    #                  "nmarker", "conco_break", "corr_break", "time")
    #     
    #     if (input$display_overview == "full") {
    #       colnames(df_overview)[1:3] <- c("SNP caller","Genotype caller","Read counts from")
    #       
    #       idx_columns <- which(choices %in% input$over_columns)
    #       idx_columns <- idx_columns + 6
    #       data_display <- df_overview[, c(1:6, idx_columns)]
    #       
    #     } else {
    #       colnames(df_overview)[7:15] <-  c("geno", "phases", "marker", "noninfo", "mapsize",
    #                                         "nmarker", "conco_break", "corr_break", "time")
    #       df_overview$time <- as.numeric(as.character(df_overview$time))
    #       # Include statistics 
    #       data_display <- df_overview %>% group_by(depth, SNPCall, GenoCall, CountsFrom, Value) %>%
    #         summarise(geno_mean = mean(geno),
    #                   geno_se = sd(geno)/sqrt(length(geno)),
    #                   phases_mean = mean(phases),
    #                   phases_se = sd(phases)/sqrt(length(phases)),
    #                   marker_mean = mean(marker),
    #                   marker_se = sd(marker)/sqrt(length(marker)),
    #                   noninfo_mean = mean(noninfo),
    #                   noninfo_se = sd(noninfo)/sqrt(length(noninfo)),
    #                   mapsize_mean = mean(mapsize),
    #                   mapsize_se = sd(mapsize)/sqrt(length(mapsize)),
    #                   nmarker_mean = mean(nmarker),
    #                   nmarker_se = sd(nmarker)/sqrt(length(nmarker)),
    #                   conco_break_mean = mean(conco_break),
    #                   conco_break_se = sd(conco_break)/sqrt(length(conco_break)),
    #                   corr_break_mean = mean(corr_break),
    #                   corr_break_se = sd(corr_break)/sqrt(length(corr_break)),
    #                   time_mean = mean(time),
    #                   time_se = sd(time)/sqrt(length(time))) %>% ungroup()
    #       
    #       colnames(data_display)[6:23] <- c("Kappa's coefficient for genotypes mean",
    #                                         "Kappa's coefficient for genotypes se",
    #                                         "Kappa's coefficient for phases mean",
    #                                         "Kappa's coefficient for phases se",
    #                                         "Kappa's coefficient for marker types mean",
    #                                         "Kappa's coefficient for marker types se",
    #                                         "Percentage of non-informative markers mean", 
    #                                         "Percentage of non-informative markers se", 
    #                                         "Map size (cM) mean",
    #                                         "Map size (cM) se",
    #                                         "Number markers in map mean",
    #                                         "Number markers in map se",
    #                                         "Kendall's coefficient of concordance for breakpoints mean",
    #                                         "Kendall's coefficient of concordance for breakpoints se",
    #                                         "Kendall's coefficient of correlation for breakpoints mean",
    #                                         "Kendall's coefficient of correlation for breakpoints se",
    #                                         "Time spent (s) mean",
    #                                         "Time spent (s) se")
    #       choices <- rep(choices, each=2)
    #       idx_columns <- which(choices %in% input$over_columns)
    #       idx_columns <- idx_columns + 5
    #       data_display <- data_display[, c(1:5, idx_columns)]
    #       colnames(data_display)[2:4] <- c("SNP caller","Genotype caller","Read counts from")
    #     }
    #     
    #     idx <- c("value", "p-value") %in% input$value
    #     if(all(idx)){
    #       df_sele <- data_display
    #     } else if(idx[1] == TRUE){
    #       df_sele <- data_display %>% filter(Value == "value")
    #     } else {
    #       df_sele <- data_display %>% filter(Value == "p-value")
    #     }
    #     saveRDS(df_sele, file = file)
    #   } 
    # )    
    
    ##################################################################
    # Empirical 
    ##################################################################
    
    ################
    # disper_depth
    ################
    button16 <- eventReactive(input$go16, {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.1_emp){
          if(input$ErrorProb1_emp == "OneMap_version2"){
            geno <- paste0("SNPCaller", 0.05)
          } else {
            geno <- paste0(input$ErrorProb1_emp, 0.05)
          }
        } else {
          geno <- input$ErrorProb1_emp
        }
        
        if(input$CountsFrom1_emp == "bam" & (input$ErrorProb1_emp == "OneMap_version2" | input$ErrorProb1_emp == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        data <- datas_emp()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall1_emp) %>%
          filter(CountsFrom == input$CountsFrom1_emp)
        incProgress(0.5, detail = paste("Doing part", 2))
        data
      })
    })
    
    output$disper_depth_emp_out <- renderPlot({
      errorProb_graph_emp(button16(), input$real1_emp, input$geno_from1_emp)
    })
    
    output$disper_depth_emp_table <- renderTable({
      sum_depth <- button16()$alt + button16()$ref 
      data.frame("mean" = mean(sum_depth), "se" = sd(sum_depth)/sqrt(length(sum_depth)))
    })
    
    ## download
    output$disper_depth_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("depths.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building left graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          if(input$Global0.05.1_emp){
            if( input$ErrorProb1_emp == "OneMap_version2"){
              geno <- paste0("SNPCaller", 0.05)
            } else {
              geno <- paste0(input$ErrorProb1_emp, 0.05)
            }
          } else {
            geno <- input$ErrorProb1_emp
          }
          
          if(input$CountsFrom1_emp == "bam" & (input$ErrorProb1_emp == "OneMap_version2" | input$ErrorProb1_emp == "SNPCaller")){
            stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
          }
          data <- datas_emp()[[1]] %>% filter(GenoCall == geno) %>%
            filter(SNPCall == input$SNPCall1_emp) %>%
            filter(CountsFrom == input$CountsFrom1_emp)
          incProgress(0.5, detail = paste("Doing part", 2))
          p <- errorProb_graph_emp(data, input$real1_emp, input$geno_from1_emp)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    
    button17 <- eventReactive(input$go17, {
      withProgress(message = 'Building right graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.2_emp){
          if( input$ErrorProb2_emp == "OneMap_version2"){
            geno <- paste0("SNPCaller", 0.05)
          } else {
            geno <- paste0(input$ErrorProb2_emp, 0.05)
          }
        } else {
          geno <- input$ErrorProb2_emp
        }
        
        if(input$CountsFrom2_emp == "bam" & (input$ErrorProb2_emp == "OneMap_version2" | input$ErrorProb2_emp == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        data <- datas_emp()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall2_emp) %>%
          filter(CountsFrom == input$CountsFrom2_emp)
        incProgress(0.5, detail = paste("Doing part", 2))
        data
      })
    })
    
    output$disper_depth2_emp_out <- renderPlot({
      errorProb_graph_emp(button17(), input$real2_emp, input$geno_from2_emp)
    })
    
    output$disper_depth2_emp_table <- renderTable({
      sum_depth <- button17()$alt + button17()$ref 
      data.frame("mean" = mean(sum_depth), "se" = sd(sum_depth)/sqrt(length(sum_depth)))
    })
    
    ##################################################################
    
    ###########
    # ind_size
    ###########
    button18 <- eventReactive(input$go18, {
      
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        if(input$Global0.05.3_emp){
          geno <- paste0(input$ErrorProb3_emp, 0.05)
          if(any(input$ErrorProb3_emp %in% "OneMap_version2"))
            geno[which(input$ErrorProb3_emp == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb3_emp %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb3_emp
        }
        
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall3_emp) %>%
          filter(CountsFrom == input$CountsFrom3_emp) %>%
          mutate(interv.diff = sqrt(c(0,cm[-1] - cm[-length(cm)])^2))
        
        data_df <- data %>% group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise(tot_size = round(cm[length(cm)],3),
                    n = n())
        
        data <- data %>% mutate(interv.diff = sqrt(c(0,cm[-1] - cm[-length(cm)])^2))
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise(n = n()) 
        
        data<- merge(data, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -mks, -pos, -mk.type, -phase, - CountsFrom, -cm)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        data$key <- gsub("interv.diff", "diff (cM)", data$key)
        data$key <- gsub("n", "n markers", data$key)
        list(data, data_df)
      })
    })
    
    output$ind_size_emp_out <- renderPlot({
      ind_size_graph_emp(button18()[[1]])
    })
    
    output$ind_size_emp_df_out <- renderDataTable({
      button18()[[2]]
    })
    
    ## download
    output$ind_size_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("ind_size.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          if(input$Global0.05.3_emp){
            geno <- paste0(input$ErrorProb3_emp, 0.05)
            if(any(input$ErrorProb3_emp %in% "OneMap_version2"))
              geno[which(input$ErrorProb3_emp == "OneMap_version2")] <- "SNPCaller0.05"
            if(any(input$ErrorProb3_emp %in% "gusmap"))
              stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- input$ErrorProb3_emp
          }
          
          data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall3_emp) %>%
            filter(CountsFrom == input$CountsFrom3_emp) %>%
            mutate(interv.diff = sqrt(c(0,cm[-1] - cm[-length(cm)])^2))
          
          data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
            summarise(n = n()) 
          
          data<- merge(data, data_n) %>%
            gather(key, value, -GenoCall, -SNPCall, -mks, -pos, -mk.type, -phase, - CountsFrom, -cm)
          
          data$key <- gsub("interv.diff", "diff (cM)", data$key)
          data$key <- gsub("n", "n markers", data$key)
          incProgress(0.5, detail = paste("Doing part", 2))
          p <- ind_size_graph_emp(data)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    #######################################################################
    
    #############
    # marker_type
    #############
    button19 <- eventReactive(input$go19, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb10_emp) %>%
          filter(SNPCall %in% input$SNPCall10_emp) %>%
          filter(CountsFrom == input$CountsFrom10_emp) %>%
          group_by(mk.type, GenoCall, SNPCall, CountsFrom) %>%
          summarise(n = n()) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom,-n)
        data
      })
    })
    
    
    output$marker_type_emp_out <- renderPlot({
      marker_type_graph_emp(button19())
    })
    
    ## download
    output$marker_type_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("marker_type_emp.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb10_emp) %>%
            filter(SNPCall %in% input$SNPCall10_emp) %>%
            filter(CountsFrom == input$CountsFrom10_emp) %>%
            group_by(mk.type, GenoCall, SNPCall, CountsFrom) %>%
            summarise(n = n()) %>%
            gather(key, value, -GenoCall, -SNPCall, -CountsFrom,-n)
          incProgress(0.5, detail = paste("Doing part", 2))
          p <- marker_type_graph(data)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    
    #######################################################################
    
    #########
    # times
    #########
    button20 <- eventReactive(input$go20, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.9_emp){
          geno <- paste0(input$ErrorProb9_emp, 0.05)
          if(any(input$ErrorProb9_emp %in% "OneMap_version2"))
            geno[which(input$ErrorProb9_emp == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb9_emp %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb9_emp
        }
        
        data_n <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall9_emp) %>%
          filter(CountsFrom == input$CountsFrom9_emp) %>%
          group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise(n = n()) 
        
        data <- datas_emp()[[4]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall9_emp) %>%
          filter(CountsFrom == input$CountsFrom9_emp)
        
        data_df<- merge(data, data_n) 
        
        data <- data_df %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom)
        
        data$key <- gsub("n", "number of markers", data$key)
        data$key <- gsub("time", "time (seconds)", data$key)
        incProgress(0.5, detail = paste("Doing part", 2))
        list(data, data_df)
      })
    })
    
    output$times_emp_out <- renderPlot({
      times_graph_emp(button20()[[1]])
    })
    
    output$times_emp_df_out <- renderDataTable({
      button20()[[2]]
    })
    
    ## download
    output$times_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("times.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          if(input$Global0.05.9_emp){
            geno <- paste0(input$ErrorProb9_emp, 0.05)
            if(any(input$ErrorProb9_emp %in% "OneMap_version2"))
              geno[which(input$ErrorProb9_emp == "OneMap_version2")] <- "SNPCaller0.05"
            if(any(input$ErrorProb9_emp %in% "gusmap"))
              stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- input$ErrorProb9_emp
          }
          
          data_n <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall9_emp) %>%
            filter(CountsFrom == input$CountsFrom9_emp) %>%
            group_by(GenoCall, SNPCall, CountsFrom) %>%
            summarise(n = n()) 
          
          data <- datas_emp()[[4]] %>% filter(GenoCall %in% input$ErrorProb9_emp) %>%
            filter(SNPCall %in% input$SNPCall9_emp) %>%
            filter(CountsFrom == input$CountsFrom9_emp)
          
          data<- merge(data, data_n) %>%
            gather(key, value, -GenoCall, -SNPCall, -CountsFrom)
          
          data$key <- gsub("n", "number of markers", data$key)
          data$key <- gsub("time", "time (seconds)", data$key)
          incProgress(0.5, detail = paste("Doing part", 2))
          
          p <- times_graph_emp(data)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    
    ##########
    # filters
    ##########
    button22 <- eventReactive(input$go22, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        choosed <- input$ErrorProb7_emp
        if(input$Global0.05.7_emp){
          geno <- paste0(choosed, 0.05)
          if(any(choosed %in% "OneMap_version2") & !any(choosed %in% "SNPCaller") ){
            geno[which(choosed %in% "OneMap_version2")] <- paste0("SNPCaller", 0.05)
          } else if(any(choosed %in% "OneMap_version2") & any(choosed %in% "SNPCaller")){
            geno <- geno[-which(choosed %in% "OneMap_version2")]
          } else if (any(choosed %in% "gusmap")){
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } 
        } else {
          geno <- choosed
        }
        datas_emp()[[3]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall7_emp) %>%
          filter(CountsFrom == input$CountsFrom7_emp)  %>%
          gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
        
      })
    })
    
    output$filters_emp_out <- renderPlot({
      filters_graph_emp(button22())
    })
    
    output$filters_emp_df_out <- renderDataTable({
      button22()
    })
    
    ## download
    output$filters_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("filters.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          
          choosed <- input$ErrorProb7_emp
          if(input$Global0.05.7_emp){
            geno <- paste0(choosed, 0.05)
            if(any(choosed %in% "OneMap_version2") & !any(choosed %in% "SNPCaller") ){
              geno[which(choosed %in% "OneMap_version2")] <- paste0("SNPCaller", 0.05)
            } else if(any(choosed %in% "OneMap_version2") & any(choosed %in% "SNPCaller")){
              geno <- geno[-which(choosed %in% "OneMap_version2")]
            } else if (any(choosed %in% "gusmap")){
              stop("Gusmap do not allow to change the error rate. Please, select other option.")
            } 
          } else {
            geno <- choosed
          }
          data <- datas_emp()[[3]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall7_emp) %>%
            filter(CountsFrom == input$CountsFrom7_emp) %>%
            gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
          incProgress(0.5, detail = paste("Doing part", 2))
          
          p <- filters_graph_emp(data)
          p <- p + theme(legend.title=element_text(size=20, hjust=0.5),
                         legend.text = element_text(size=17),
                         axis.title=element_text(size=17),
                         axis.text = element_text(size=17), 
                         strip.text = element_text(size=17))
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    ##################################################################
    
    ###########
    # heatmaps
    ###########
    button23 <- eventReactive(input$go23, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.8_emp){
          if(input$ErrorProb8_emp == "OneMap_version2" | input$ErrorProb8_emp == "SNPCaller"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb8_emp == "gusmap"){
            stop("Gusmap do not build plotly heatmaps. Please, select other option.")
          } else {
            geno <- paste0(input$ErrorProb8_emp, 0.05)
          }
        } else {
          ifelse(input$ErrorProb8_emp == "OneMap_version2", geno <- "default", geno <- input$ErrorProb8_emp)
        }
        
        temp_n <- paste0("map_",input$SNPCall8_emp, "_", input$CountsFrom8_emp, "_", geno, ".RData")
        incProgress(0.25, detail = paste("Doing part", 2))
        idx <- which(datas_emp()[[6]] == temp_n)
        data <- readList(datas_emp()[[8]], index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        incProgress(0.5, detail = paste("Doing part", 3))
        data
      })
    })
    
    output$heatmaps_emp_out <- renderPlotly({
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0.75, detail = paste("Doing part", 4))
        rf_graph_table(button23(), inter = T, html.file = tempfile(patter="file", fileext = ".html"),display = F) 
      })
    })
    
    ## download
    output$heatmaps_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("heatmap.html")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          
          if(input$Global0.05.8_emp){
            if(input$ErrorProb8_emp == "OneMap_version2"){
              geno <- paste0("SNPCaller", 0.05)
            } else if (input$ErrorProb8_emp == "gusmap"){
              stop("Gusmap do not build plotly heatmaps. Please, select other option.")
            } else {
              geno <- paste0(input$ErrorProb8_emp, 0.05)
            }
          } else {
            ifelse(input$ErrorProb8_emp == "OneMap_version2", geno <- "default", geno <- input$ErrorProb8_emp)
          }
          
          if(input$CountsFrom8_emp == "bam" & (input$ErrorProb8_emp == "OneMap_version2" | input$ErrorProb8_emp == "SNPCaller")){
            stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
          }
          
          temp_n <- paste0("map_",input$SNPCall8_emp, "_", input$CountsFrom8_emp, "_", geno, ".RData")
          
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- readList(datas_emp()[[8]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 2))
          p <- rf_graph_table(data, inter = T, display = F, html.file = file) 
          saveWidget(p, file)
        })
      } 
    )
    
    ################################################################# 
    
    ############
    # Maps
    ############
    button24 <- eventReactive(input$go24, {
      withProgress(message = 'Building draw', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        if(input$Global0.05.11_emp){
          if(input$ErrorProb11_emp == "OneMap_version2"){
            geno <- paste0("SNPCaller", 0.05)
          } else if (input$ErrorProb11_emp == "gusmap"){
            stop("Gusmap do not build plotly heatmaps. Please, select other option.")
          } else {
            geno <- paste0(input$ErrorProb11_emp, 0.05)
          }
        } else {
          geno <- input$ErrorProb11_emp
        }
        
        if(input$CountsFrom11_emp == "bam" & (input$ErrorProb11_emp == "OneMap_version2" | input$ErrorProb11_emp == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        
        incProgress(0.25, detail = paste("Doing part", 2))
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall11_emp) %>%
          filter(CountsFrom == input$CountsFrom11_emp)
        
        incProgress(0.5, detail = paste("Doing part", 3))
        data <-   data.frame(data$mks, data$cm)
        outfile <- tempfile(pattern="file", fileext = ".png")
        list(data, outfile)
      })
    })
    
    output$map_emp_out <- renderImage({
      draw_map2(button24()[[1]], output = button24()[[2]], col.tag = "darkblue", pos = T, id = F)  
      
      list(src = button24()[[2]],
           contentType = 'image/png',
           width = 200,
           height = 900)
    }, deleteFile = TRUE)
    
    button25 <- eventReactive(input$go25, {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.11_emp){
          if(input$ErrorProb11_emp == "OneMap_version2" | input$ErrorProb11_emp == "SNPCaller"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb11_emp == "gusmap"){
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- paste0(input$ErrorProb11_emp, 0.05)
          }
        } else {
          if(input$ErrorProb11_emp == "OneMap_version2" | input$ErrorProb11_emp == "SNPCaller"){
            geno <- "default"
          } else {
            geno <- input$ErrorProb11_emp
          }
        }
        
        if(input$CountsFrom11_emp == "bam" & (input$ErrorProb11_emp == "OneMap_version2" | input$ErrorProb11_emp == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        
        temp_n <- paste0("map_",input$SNPCall11_emp, "_", input$CountsFrom11_emp, "_", geno, ".RData")
        
        incProgress(0.25, detail = paste("Doing part", 2))
        if(geno == "gusmap"){
          data <- datas_emp()[[5]][[temp_n]]
          data$rf_2pt()
          incProgress(0.5, detail = paste("Doing part", 3))
          list(data, geno)
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- readList(datas_emp()[[8]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          incProgress(0.5, detail = paste("Doing part", 3))
          list(data, geno)
        }
      })
    })
    
    output$map1_emp_out <- renderPlot({
      if(button25()[[2]] == "gusmap"){
        button25()[[1]]$plotChr(mat="rf", parent = "both")
      } else {
        rf_graph_table(button25()[[1]], inter = F, mrk.axis = "none")
      }
    })
    
    ## download
    output$map_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("map.RData")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building heatmap', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          if(input$Global0.05.11_emp){
            if( input$ErrorProb11_emp == "OneMap_version2"){
              geno <- paste0("default", 0.05)
            } else if (input$ErrorProb11_emp == "gusmap"){
              stop("Gusmap do not allow to change the error rate. Please, select other option.")
            } else {
              geno <- paste0(input$ErrorProb11_emp, 0.05)
            }
          } else {
            if( input$ErrorProb11_emp == "OneMap_version2"){
              geno <- "default"
            } else {
              geno <- input$ErrorProb11_emp
            }
          }
          
          if(input$CountsFrom11_emp == "bam" & (input$ErrorProb11_emp == "OneMap_version2" | input$ErrorProb11_emp == "SNPCaller")){
            stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
          }
          
          temp_n <- paste0("map_",input$SNPCall11_emp, "_", input$CountsFrom11_emp, "_", geno, ".RData")
          if(geno == "gusmap"){
            data <- datas_emp()[[5]][[temp_n]]
            save(data, file=file)
          } else {
            idx <- which(datas_emp()[[6]] == temp_n)
            data <- readList(datas_emp()[[8]], index = idx)
            data <- data[[1]]
            class(data) <- "sequence"
            save(data, file=file)
          }
        })
      }
    )
    
    #################################
    button26 <- eventReactive(input$go26, {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.12_emp){
          if( input$ErrorProb12_emp == "OneMap_version2"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb12_emp == "gusmap"){
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- paste0(input$ErrorProb12_emp, 0.05)
          }
        } else {
          if( input$ErrorProb12_emp == "OneMap_version2"){
            geno <- "default"
          } else {
            geno <- input$ErrorProb12_emp
          }
        }
        
        if(input$CountsFrom12_emp == "bam" & (input$ErrorProb12_emp == "OneMap_version2" | input$ErrorProb12_emp == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        
        temp_n <- paste0("map_",input$SNPCall12_emp, "_", input$CountsFrom12_emp, "_", geno, ".RData")
        if(geno == "gusmap"){
          stop("We do not include in this app support to do it with GUSMap. Please, select other option.")
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- readList(datas_emp()[[8]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          data
        }
      })
    })
    
    output$haplot_emp_out <- renderPlot({
      plot(progeny_haplotypes(button26(), ind = as.numeric(input$inds12_emp), most_likely = input$Most_likely12_emp))
    })
    
    ## download
    output$haplo_out_down <- downloadHandler(
      filename =  function() {
        paste("haplotypes.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building heatmap', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          if(input$Global0.05.12_emp){
            if( input$ErrorProb12_emp == "OneMap_version2"){
              geno <- paste0("default", 0.05)
            } else if (input$ErrorProb12_emp == "gusmap"){
              stop("Gusmap do not allow to change the error rate. Please, select other option.")
            } else {
              geno <- paste0(input$ErrorProb12_emp, 0.05)
            }
          } else {
            if( input$ErrorProb12_emp == "OneMap_version2"){
              geno <- "default"
            } else {
              geno <- input$ErrorProb12_emp
            }
          }
          
          if(input$CountsFrom12_emp == "bam" & (input$ErrorProb12_emp == "OneMap_version2" | input$ErrorProb12_emp == "SNPCaller")){
            stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
          }
          
          temp_n <- paste0("map_",input$SNPCall12_emp, "_", input$CountsFrom12_emp, "_", geno, ".RData")
          if(geno == "gusmap"){
            stop("We do not include in this app support to do it with GUSMap. Please, select other option.")
          } else {
            idx <- which(datas_emp()[[6]] == temp_n)
            data <- readList(datas_emp()[[8]], index = idx)
            data <- data[[1]]
            class(data) <- "sequence"
            p <- plot(progeny_haplotypes(data, ind = as.numeric(input$inds12_emp), most_likely = input$Most_likely12_emp))
            ggsave(file, p, width = 400, height = 200, units="mm")
          }
        })
      } 
    )
    #################################
    button27 <- eventReactive(input$go27, {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.13_emp){
          if(input$ErrorProb13_emp == "OneMap_version2" | input$ErrorProb13_emp == "SNPCaller"){
            geno <- paste0("default", 0.05)
          } else if (input$ErrorProb13_emp == "gusmap"){
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- paste0(input$ErrorProb13_emp, 0.05)
          }
        } else {
          if( input$ErrorProb13_emp == "OneMap_version2"){
            geno <- "default"
          } else {
            geno <- input$ErrorProb13_emp
          }
        }
        
        if(input$CountsFrom13_emp == "bam" & (input$ErrorProb13_emp == "OneMap_version2" | input$ErrorProb13_emp == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        
        temp_n <- paste0("map_",input$SNPCall13_emp, "_", input$CountsFrom13_emp, "_", geno, ".RData")
        if(geno == "gusmap"){
          stop("We do not include in this app support to do it with GUSMap. Please, select other option.")
        } else {
          idx <- which(datas_emp()[[6]] == temp_n)
          data <- readList(datas_emp()[[8]], index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          inds <- 1:data$data.name$n.ind
          incProgress(0.3, detail = paste("Doing part", 2))
          df <- progeny_haplotypes(data, ind = inds, most_likely = T)
          incProgress(0.6, detail = paste("Doing part", 3))
          progeny_haplotypes_counts(df)
        }
      })
    })
    
    output$counts_emp_out <- renderPlot({
      plot(button27())
    })
    
    
    output$counts_emp_df_out <- renderTable({
      data.frame(total = sum(button27()$counts),
                 mean = mean(button27()$counts),
                 se = sd(button27()$counts)/sqrt(length(button27()$counts)),
                 var = var(button27()$counts),
                 sd = sd(button27()$counts),
                 median = median(button27()$counts))
    })
    
    ##################################################################
    button29 <- eventReactive(input$go29, {
      
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        if(input$Global0.05.14_emp){
          geno <- paste0(input$ErrorProb14_emp, 0.05)
          if(any(input$ErrorProb14_emp %in% "OneMap_version2"))
            geno[which(input$ErrorProb14_emp == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb14_emp %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb14_emp
        }
        
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall14_emp) %>%
          filter(CountsFrom == input$CountsFrom14_emp) 
        data$pos <- data$pos/1000 
        data
      })
    })
    
    output$cmxmb_emp_out <- renderPlot({
      ggplot(button29(), aes(x=cm, y=pos)) +
        geom_point(size=2, shape=20) + facet_grid(SNPCall~GenoCall, scales = "free") +
        xlab("cM") + ylab("Mb")
    })
    
    ## download
    output$cmxmb_emp_out_down <- downloadHandler(
      filename =  function() {
        paste("cmxmb.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          
          if(input$Global0.05.14_emp){
            geno <- paste0(input$ErrorProb14_emp, 0.05)
            if(any(input$ErrorProb14_emp %in% "OneMap_version2"))
              geno[which(input$ErrorProb14_emp == "OneMap_version2")] <- "SNPCaller0.05"
            if(any(input$ErrorProb14_emp %in% "gusmap"))
              stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- input$ErrorProb14_emp
          }
          
          data <- datas_emp()[[2]] %>% filter(GenoCall %in% geno) %>%
            filter(SNPCall %in% input$SNPCall14_emp) %>%
            filter(CountsFrom == input$CountsFrom14_emp) 
          data$pos <- data$pos/1000 
          
          p <-  ggplot(data, aes(x=cm, y=pos)) +
            geom_point(size=2, shape=20) + facet_grid(SNPCall~GenoCall) +
            xlab("cM") + ylab("Mb")
          
          ggsave(file, p, width = 400, height = 200, units="mm")
        })
      } 
    )
    #################################
    button30 <- eventReactive(input$go30, {
      
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        map_size <- datas_emp()[[2]] %>% group_by(CountsFrom, SNPCall, GenoCall) %>%
          summarise(map_size = cm[length(cm)])
        
        n_markers <- datas_emp()[[2]] %>% group_by(CountsFrom, SNPCall, GenoCall) %>%
          summarise(n_markers_map = n())
        
        markers <- datas_emp()[[3]][,c(1,2,3,5,9)]
        
        markers <- merge(markers, n_markers)
        markers <- cbind(markers, filtered = (markers$n_markers - markers$n_markers_map - markers$redundant_markers))
        
        df_overview <- merge(markers, map_size)
        incProgress(0.1, detail = paste("Doing part", 2))
        
        df_overview <- merge(df_overview, datas_emp()[[4]])
        df_breakpoints <- data.frame()
        
        part<-0.7/length(datas_emp()[[6]])
        parts <- seq(0.1,0.8, part)[-1]
        
        for(i in 1:length(datas_emp()[[6]])){
          #incProgress(0.1+parts[i], detail = paste("Doing part", 2+i))
          ID <- gsub(pattern= ".RData", replacement = "", unlist(strsplit(datas_emp()[[6]][i], "_")))[-1]
          data <- readList(datas_emp()[[8]], index = i)
          data <- data[[1]]
          class(data) <- "sequence"
          inds <- 1:data$data.name$n.ind
          df <- progeny_haplotypes(data, ind = inds, most_likely = T)
          counts <- progeny_haplotypes_counts(df)
          
          breakpoints <- data.frame(SNPCall = ID[1], CountsFrom = ID[2], GenoCall = ID[3], 
                                    Total_break = sum(counts$counts), 
                                    mean_break = mean(counts$counts), 
                                    se = sd(counts$counts)/sqrt(length(counts$counts)))
          
          df_breakpoints <- rbind(df_breakpoints, breakpoints)
        }
        
        incProgress(0.9, detail = paste("Doing last part"))
        df_breakpoints$GenoCall <- as.character(df_breakpoints$GenoCall)
        df_breakpoints$GenoCall[df_breakpoints$GenoCall == "default0.05"] <- "SNPCaller0.05" # Bug - fix in wdl
        df_breakpoints$GenoCall[df_breakpoints$GenoCall == "default"] <- "OneMap_version2" # Bug - fix in wdl
        
        df_overview <- merge(df_overview, df_breakpoints)
        colnames(df_overview) <- c("Read counts from","SNP caller","Genotype caller",
                                   "Informative markers in VCF", "Redundant markers", 
                                   "Mapped markers", "Filtered markers", "Map size (cM)", 
                                   "Time (s)","Total breakpoints", "Mean breakpoints", 
                                   "Standard error breakpoints")
        df_overview
      })
    })
    
    output$overview_emp_out <- renderDataTable({
      data_display <- button30()
      choices <- c("n_markers", "redundants", "n_markers_map", "filt_mks",
                   "map_size", "time", "breakpoints", "mean_break", "se_break")
      
      idx_columns <- choices %in% input$over_emp_columns
      idx_columns <- c(T,T,T,idx_columns)
      data_display1 <- data_display[,idx_columns, with=FALSE]
      data_display1
    })
    
    output$overview_emp_plot_out <- renderPlot({
      data_display <- button30()
      overview_graph_emp(data_display)
    }, width = 900, height = 570)
    
    ## download
    output$overview_emp_plot_down <- downloadHandler(
      filename =  function() {
        paste("overview_emp.eps")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        data_display <- button30()
        p <- overview_graph_emp(data_display)
        ggsave(p, filename = file)
      } 
    )    
    
    ## download
    output$overview_emp_down <- downloadHandler(
      filename =  function() {
        paste("overview_emp.rds")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        data_display <- button30()
        choices <- c("n_markers", "redundants", "n_markers_map", "filt_mks",
                     "map_size", "time", "breakpoints", "mean_break", "se_break")
        
        idx_columns <- choices %in% input$over_emp_columns
        idx_columns <- c(T,T,T,idx_columns)
        data_display1 <- data_display[,idx_columns, with=FALSE]
        saveRDS(data_display1, file = file)
      } 
    )    
    
    #################################
    button28 <- eventReactive(input$go28, {
      withProgress(message = 'Building heatmap', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        # Empiricals
        ## Populus snpcalling
        if(input$example_wf=="populus_snpcalling"){
          paste(system.file("ext","populus/biallelics/slurm-67440032_snpcalling_popu.out", package = "Reads2MapApp"))
          
          ## Populus maps
        } else if(input$example_wf=="populus_emp_cont"){
          paste(system.file("ext","populus/biallelics/with_contaminants/maps_emp_populus.log", package = "Reads2MapApp"))
        } else if(input$example_wf=="populus_emp"){
          paste(system.file("ext","populus/biallelics/without_contaminants/populus_rmind.log", package = "Reads2MapApp"))
          
          ## Eucalyptus snpcalling
        } else if(input$example_wf=="eucalyptus_snpcalling"){
          paste(system.file("ext","eucalyptus/biallelics/slurm-67472436_snpcalling_euc.out", package = "Reads2MapApp"))
          
          ## Eucalyptus maps
        } else if(input$example_wf=="eucalyptus_map"){
          paste(system.file("ext","eucalyptus/biallelics/euc.log", package = "Reads2MapApp"))
          
          # Simulations
        } else if(input$example_wf=="populus_simu_fam5_pop200_depth10"){
          paste(system.file("ext","simulations/popsize50/biallelics/slurm-67454631_depth20.out", package = "Reads2MapApp"))
        } else if(input$example_wf=="populus_simu_fam5_pop200_depth20"){
          paste(system.file("ext","simulations/popsize150/biallelics/slurm-67465833_depth10.out", package = "Reads2MapApp"))
        } 
      })
    })
    
    output$wf_times_out <- renderPlotly({
      workflow_times(button28(), interactive=TRUE)
    })
    
    ## download
    output$wf_out_down <- downloadHandler(
      filename =  function() {
        paste("wf_times.html")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        withProgress(message = 'Building graphic', value = 0, {
          incProgress(0, detail = paste("Doing part", 1))
          if(!is.null(input$wflog)){
            sele_file <- input$wflog
          } else {
            if(input$example_wf=="populus_map"){
              sele_file <- paste(system.file("ext","populus/biallelics/slurm-67440032_snpcalling_popu.log", package = "Reads2MapApp"))
              
              ## Populus maps
            } else if(input$example_wf=="populus_emp_cont"){
              sele_file <- paste(system.file("ext","populus/biallelics/with_contaminants/maps_emp_populus.log", package = "Reads2MapApp"))
            } else if(input$example_wf=="populus_emp"){
              sele_file <- paste(system.file("ext","populus/biallelics/without_contaminants/populus_rmind.log", package = "Reads2MapApp"))
              
              ## Eucalyptus snpcalling
            } else if(input$example_wf=="eucalyptus_snpcalling"){
              sele_file <- paste(system.file("ext","populus/biallelics/slurm-67472436_snpcalling_euc.log", package = "Reads2MapApp"))
              
              ## Eucalyptus maps
            } else if(input$example_wf=="eucalyptus_map"){
              sele_file <- paste(system.file("ext","eucalyptus/biallelics/euc.log", package = "Reads2MapApp"))
              
              # Simulations
              ## pop size 50
            } else if(input$example_wf=="populus_simu_5fam_pop200_depth10"){
              sele_file <- paste(system.file("ext","simulations/biallelics/slurm-67454631_depth20.out", package = "Reads2MapApp"))
            } else if(input$example_wf=="populus_simu_5fam_pop200_depth20"){
              sele_file <- paste(system.file("ext","simulations/biallelics/slurm-67465833_depth10.out", package = "Reads2MapApp"))
            } 
          }
          p <-workflow_times(sele_file, interactive = T)
          saveWidget(p, file = file)
        })
      } 
    )
  }
  
  # Create Shiny app ----
  shinyApp(ui = ui, server = server)
}