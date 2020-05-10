
# Packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(onemap)
library(GUSMap)
library(largeList)

# Functions
## Simulations
source("graphics_simu.R")
## Empirical
source("graphics_emp.R")

## Permanently choices - If add more softwares in workflow comparision this part requires update
ErrorProb_choice <- list("OneMap_version2" = "OneMap_version2",
                         "polyrad"="polyrad",
                         "SNPCaller"="SNPCaller",
                         "supermassa"="supermassa",
                         "updog"="updog")

global0.05_choices <- list("global error of 0.05"=TRUE,
                           "variable error" = FALSE)

maps_choice <- list("gusmap" = "gusmap",
                    "OneMap_version2" = "OneMap_version2",
                    "polyrad"="polyrad",
                    "SNPCaller"="SNPCaller",
                    "supermassa"="supermassa",
                    "updog"="updog")
SNPCall_choice <- list("freebayes"="freebayes",
                       "gatk" = "gatk")
CountsFrom_choice <- list("bam"="bam",
                          "vcf"="vcf")

stats_choice <- list("mean", "median", "var", "total")
names(stats_choice) <- c("mean", "median", "var", "total")
avalSNPs_choice <- list("(1)", "(2)", "(3)", "(4)", "(5)")
names(avalSNPs_choice) <- c("number of simulated SNPs (1)", 
                            "number of SNPs identified (2)", 
                            "number of correctly identified SNPs (3)",
                            "number of correctly identified reference allele (4)",
                            "number of correctly identified alternative allele (5)")
fake_choices <- list("with-false", "without-false")
names(fake_choices) <- c("yes", "no")


########
#  UI  #
########

## Header
header <- dashboardHeader(
  title = "Error workflow results",
  titleWidth = 250)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("About", tabName = "about", icon = icon("lightbulb")),
    #menuItem("Parallel map", icon = icon("dot-circle"), tabName = "parallel"), 
    menuItem("Upload data", icon = icon("upload"), tabName= "upload"),
    menuItem("Simulation results", icon = icon("dot-circle"), tabName= "simulations",
             menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth"),
             menuSubItem("Map size each family", icon = icon("circle"), tabName = "ind_size"),
             menuSubItem("Overview map size", icon = icon("circle"), tabName = "all_size"),
             menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type"),
             menuSubItem("Phases", icon = icon("circle"), tabName = "phases"),
             menuSubItem("Times", icon = icon("circle"), tabName = "times"),
             menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage"),
             menuSubItem("SNP calling efficiency", icon = icon("circle"), tabName = "snpcall"),
             menuSubItem("Filters", icon = icon("circle"), tabName = "filters"),
             menuSubItem("Maps", icon = icon("circle"), tabName = "map")),
    
    menuItem("Empirical data results", icon = icon("dot-circle" ), tabName = "empirical",
             menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth_emp"),
             menuSubItem("Map size", icon = icon("circle"), tabName = "ind_size_emp"),
             menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type_emp"),
             menuSubItem("Times", icon = icon("circle"), tabName = "times_emp"),
             menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage_emp"),
             menuSubItem("Filters", icon = icon("circle"), tabName = "filters_emp"),
             menuSubItem("Plotly heatmaps", icon = icon("circle"), tabName = "heatmaps_emp"),
             menuSubItem("Maps", icon = icon("circle"), tabName = "map_emp"),
             menuSubItem("Progeny haplotypes", icon = icon("circle"), tabName = "haplo_emp"))
  )
)

body <- dashboardBody(
  tabItems(
    ##########################################################
    tabItem(tabName = "about",
            includeMarkdown("about.Rmd")
    ),
    ####################################################################################
    # tabItem(tabName = "parallel", # Se eu deixo isso funcional o menu deixa de ser dinamico
    #         includeHTML("parallel.html")
    #         #includeMarkdown("parallel.Rmd")
    # ),
    ##########################################################
    # Upload data
    ##########################################################
    tabItem(tabName = "upload",
            "This shiny app build several graphics as results from OneMap workflows. 
            If you run the", tags$b("SimulatedReads.wdl"),"and/or EmpiricalReads.wdl workflows you 
            can upload the outputted data in", tags$b("Upload SimulatedReads outputs"), "and/or",
            tags$b("Upload EmpiricalReads outputs"), "sections. If you don't have your own results yet,
            you can explore the generate to populus dataset (Bioproject X), eucalyptus and acca
            all these results is better explained in the paper. Select the available 
            example results in", tags$b("SimulatedReads.wdl example results"),"and/or", 
            tags$b("EmpiricalReads.wdl example results"),".",
            hr(),
            column(width = 6,
                   box(width = 12,
                       fluidPage(
                         
                         tags$h4(tags$b("Upload SimulatesReads results:")),
                         "If you have more than one depth value, submit all files in the same window.",
                         # Copy the line below to make a file upload manager
                         fileInput("simulatedreads", label = h6("SimulatedReads_<depth>.tar.gz"), multiple = T),
                         
                       ),
                       
                       fluidPage(
                         # Copy the line below to make a select box 
                         selectInput("example_simu", label = h4(tags$b("SimulatedReads.wdl example results")), 
                                     choices = list("Populus chromosome 10" = "populus",
                                                    "Eucalyptus chromosome 10" = "eucalyptus", 
                                                    "Acca chromosome 10" = "acca"), 
                                     selected = "Populus chromosome 10"),
                       ),
                       verbatimTextOutput("simulatedreads_out")
                   )
            ),
            column(width = 6,
                   box(width = 12,
                       fluidPage(
                         
                         tags$h4(tags$b("Upload EmpiricalReads results:")),
                         "If you have more than one depth value, submit all files in the same window.",
                         # Copy the line below to make a file upload manager
                         fileInput("empiricalreads", label = h6("EmpiricalReads_<depth>.tar.gz"), multiple = T),
                       ),
                       fluidPage(
                         # Copy the line below to make a select box 
                         selectInput("example_emp", label = h4(tags$b("EmpiricalReads.wdl example results")), 
                                     choices = list("Populus chromosome 10" = 1, 
                                                    "Eucalyptus chromosome 10" = 2, 
                                                    "Acca chromosome 10" = 3), 
                                     selected = 1),
                       )
                   )
            )
    ),
    ##########################################################
    # Simulations
    ##########################################################
    tabItem(tabName = "disper_depth",
            "The same graphic is plotted in left and right to user be able to compare different methods choosing the options below.
            The x and y axis shows the read counts for reference and alternative alleles, respectively. 
            The colors of dots varies according with `Genotypes` section. User can choose to see colors from simulated genotypes, the estimated or a gradient with the error rate used. 
            Users can download the left graphic pushing `Download` button.",
            hr(),
            fluidRow(
              column(width = 6,
                     box(
                       width = NULL,
                       plotOutput("disper_depth_out")
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
                       
                       #helpText("Read counts from:"),
                       fluidPage(
                         
                         radioButtons("CountsFrom1", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr(),
                         div(downloadButton("disper_depth_out_down"),style="float:right")
                         # ),
                       )
                     )
              ),
              
              column(width = 6,
                     box(
                       width = NULL,
                       plotOutput("disper_depth2_out")
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
                       
                       #helpText("Read counts from:"),
                       fluidPage(
                         
                         radioButtons("CountsFrom2", label = p("Counts from"),
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
    tabItem(tabName = "ind_size",
            "The boxplots show the distribuition of the difference between estimated and simulated  distances for each marker of the generated maps.",
            hr(),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("ind_size_out")
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
                     box(
                       width = NULL,
                       plotOutput("all_size_out")
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
                     box(
                       width = NULL,
                       plotOutput("marker_type_out")
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
            "These boxplots show the percentage of right estimated phases for all estimated families by depths.",
            hr(),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("phases_out")
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
            "The boxplots show the distribuition of times and number of markers of all families by depth. We decide to show 
            both together because increase of number of markers also increase time needed to build the map.",
            hr(),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("times_out")
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
    ###################################################################################
    tabItem(tabName = "coverage",
            "This barplots show the percentage of the chromosome covered by the map built. It depends of the 
            chromosome size information in base pair (bp). The chromosome we used in our example has 1019956 bp (toy sample) or 22862796 bp in chromosome 10 of populus.",
            hr(),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("coverage_out")
                     )
              ),
              column(width=6,
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb5", label = p("Genotyping method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.5", label = p("Error rate"),
                                      choices = global0.05_choices,
                                      selected = "FALSE"),
                         hr()
                       ), 
                       fluidPage(
                         checkboxGroupInput("SNPCall5", label = p("SNP calling method"),
                                            choices = SNPCall_choice,
                                            selected = names(SNPCall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("depth5", label = p("Depth"),
                                            choices = "This will be updated",
                                            selected = "This will be updated"),
                         hr()
                       )
                     )
              ),
              column(width=6,
                     box(width = NULL, solidHeader = T,
                         fluidPage(
                           radioButtons("CountsFrom5", label = p("Counts from:"),
                                        choices = CountsFrom_choice,
                                        selected = "vcf"),
                           hr()
                         ),
                         fluidPage(
                           
                           radioButtons("fake6", label = p("Allow false positives?"),
                                        choices = fake_choices,
                                        selected = "without-false"),
                           hr()
                         ),
                         
                         fluidPage(
                           numericInput("chr_size1", label = p("Chromosome size"), value = "complete here"),
                           
                           hr(),
                           div(downloadButton("coverage_out_down"),style="float:right")
                         )
                     )
              )
            )
    ),
    ####################################################################################
    tabItem(tabName = "snpcall",
            "The boxplots here show some characteristics of SNP calling. The x axis contain the different 
            characteristics indexed according with section `Options`, and y axis shows the number of markers.",
            hr(),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("snpcall_out")
                     )
              ),
              column(width = 6,
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("avalSNPs1", label = p("Options"),
                                            choices = avalSNPs_choice,
                                            selected = unlist(avalSNPs_choice)),
                         hr()
                       )
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
            "The boxplots here show the total number of markers (n_markers) available for analysis, 
            the number of markers filtered by OneMap because of segregation distortion and redundancy.",
            hr(),
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("filters_out")
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
                           radioButtons("CountsFrom7", label = p("Counts from"),
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
            intensity of the linkage. Higher is the recombination fraction hottest is the color. Markers are represented in x 
            and y axis. On the right, the linkage group is plotted with distances between markers proportional to the genetic distances.",
            hr(),
            fluidRow(
              column(width = 8,
                     box(
                       width = NULL,
                       plotOutput("map_out"),
                       hr(),
                       box(solidHeader = T,
                           radioButtons("ErrorProb11", label = p("Genotyping method"),
                                        choices = maps_choice,
                                        selected = "updog"),
                       ),
                       fluidPage(
                         radioButtons("Global0.05.11", label = p("Error rate"),
                                      choices = global0.05_choices,
                                      selected = "FALSE"),
                         hr()
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
                           div(downloadButton("map1_out_down"),style="float:right")
                       )
                     )
              ),
              column(width = 4,
                     imageOutput("map1_out")
              )
            )
    ),
    
    ##########################################################
    # Empirical
    ##########################################################
    tabItem(tabName = "disper_depth_emp",
            fluidRow(
              column(width = 6,
                     box(
                       width = NULL,
                       plotOutput("disper_depth_emp_out")
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
                     box(
                       width = NULL,
                       plotOutput("disper_depth2_emp_out")
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
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("ind_size_emp_out")
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
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("marker_type_emp_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb10_emp", label = p("Genotyping method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPCall10_emp", label = p("SNP calling method"),
                                            choices = SNPCall_choice,
                                            selected = names(SNPCall_choice)),
                         hr()
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
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("times_emp_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb9_emp", label = p("Genotyping method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPCall9_emp", label = p("SNP calling method"),
                                            choices = SNPCall_choice,
                                            selected = names(SNPCall_choice)),
                         hr()
                       ),
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
    ###################################################################################
    tabItem(tabName = "coverage_emp",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("coverage_emp_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb5_emp", label = p("Genotyping method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPCall5_emp", label = p("SNP calling method"),
                                            choices = SNPCall_choice,
                                            selected = names(SNPCall_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom5_emp", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr()
                       ),
                       
                       fluidPage(
                         numericInput("chr_size", label = p("Chromosome size"), value = "complete here"),
                         
                         hr(),
                         div(downloadButton("coverage_emp_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ################################################################################3
    tabItem(tabName = "filters_emp",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("filters_emp_out")
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
                       ), 
                       fluidPage(
                         checkboxGroupInput("SNPCall7_emp", label = p("SNP calling method"),
                                            choices = SNPCall_choice,
                                            selected = names(SNPCall_choice)),
                         hr()
                       )
                     )
              ),
              column(width = 6,
                     box(width = NULL, solidHeader = T,
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
              box(
                width = NULL,
                plotlyOutput("heatmaps_emp_out", height = 650)
              )
            ),
            fluidRow(
              column(width = 6,
                     box(
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
                     box(width = 6,
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
            fluidRow(
              column(width = 8,
                     box(
                       width = NULL,
                       plotOutput("map1_emp_out"),
                       box(
                         radioButtons("ErrorProb11_emp", label = p("Genotyping method"),
                                      choices = maps_choice,
                                      selected = "updog"),
                       ),
                       box(
                         radioButtons("SNPCall11_emp", label = p("SNP calling method"),
                                      choices = SNPCall_choice,
                                      selected = "gatk"),
                       ),
                       box(
                         radioButtons("Global0.05.11_emp", label = p("Error rate"),
                                      choices = global0.05_choices,
                                      selected = "FALSE"),
                         hr()
                       ), 
                       box(
                         radioButtons("CountsFrom11_emp", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         div(downloadButton("map_emp_out_down"),style="float:right")
                       )
                     )
              ),
              column(width = 4,
                     imageOutput("map_emp_out")
              )
            )
    )
    ############################################################################
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")

## Define server logic required to draw a histogram ----
server <- function(input, output,session) {
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
      } else { ######## Available examples
        if(input$example_simu == "populus"){
          data.gz <- c("data/ig_toy_sample/SimulatedReads_results_depth10.tar.gz","data/ig_toy_sample/SimulatedReads_results_depth20.tar.gz")
        } else if(input$example_simu == "eucalyptus"){
          data.gz <- c("data/eucalyptus/SimulatedReads_10.tar.gz", "data/eucalyptus/SimulatedReads_20.tar.gz", "data/eucalyptus/SimulatedReads_100.tar.gz" )
        } else if(input$example_simu == "acca"){
          data.gz <- c("data/acca/SimulatedReads_10.tar.gz", "data/acca/SimulatedReads_20.tar.gz", "data/acca/SimulatedReads_100.tar.gz" )
        }
        path <- unlist(strsplit(data.gz[1], "/"))
        path <- paste0(paste0(path[-length(path)], collapse = "/"), "/")
      }
      list_files <- list()
      incProgress(0, detail = paste("Doing part", 1))
      for(i in 1:length(data.gz)){
        untar(data.gz[i], exdir = path)
        list_files[[i]] <- untar(data.gz[i], list = T)
      }
      
      incProgress(0.25, detail = paste("Doing part", 2))
      list_files <- lapply(list_files, function(x) paste0(path, x))
      for_rm <- sapply(list_files, "[", 1)
      list_files <- lapply(list_files, "[", -1)
      
      # Data
      datas <- list()
      for(i in 1:9){
        datas[[i]] <- sapply(list_files, "[", i)
      }
      
      ## Tables
      data1 <- data2 <- data3 <- data4 <- data5 <- vector()
      data6 <- names_rdatas <- list()
      seeds <- depths <- seeds_choices <- depths_choices <- vector()
      
      incProgress(0.5, detail = paste("Doing part", 3))
      for(i in 1:length(datas)){
        for(j in 1:length(datas[[i]])){
          if(all(grepl("gusmap_RDatas.RData", datas[[i]]))){
            temp <- load(datas[[i]][[j]])
            temp <- get(temp)
            data6 <- c(data6, temp)
          } else if(all(grepl("sequences.llo", datas[[i]]))){
            temp <- readList(datas[[i]][[j]])
            if(j == 1){
              saveList(temp, file = "data/temp_file/sequences.llo", append = F, compress = T)
            } else {
              saveList(temp, file = "data/temp_file/sequences.llo", append = T, compress = T)
            }
          } else if(all(grepl("choices.RData", datas[[i]]))){
            temp <- load(datas[[i]][[j]])
            temp <- get(temp)
            depths <- c(depths, temp[[1]])
            seeds <- c(seeds, temp[[2]])
            seeds_choices <- c(seeds_choices, temp[[3]])
            depths_choices <- c(depths_choices, temp[[4]])
          } else if(all(grepl("names.rds", datas[[i]]))){
            temp <-  readRDS(datas[[i]][[j]])
            names_rdatas <- c(names_rdatas, temp)
          } else {
            temp <-  readRDS(datas[[i]][[j]])
            name_temp <- unlist(strsplit(datas[[i]][[j]], "/"))
            name_temp <- unlist(strsplit(name_temp[length(name_temp)], "[.]"))[1]
            assign(name_temp, rbind(get(name_temp), temp))
          }
        }
      }
      
      incProgress(0.75, detail = paste("Doing part", 4))
      temp_names <- names(seeds_choices)
      seeds_choices <- as.list(1:length(seeds_choices))
      names(seeds_choices) <- temp_names
      
      names_rdatas <- unlist(names_rdatas)
      names_rdatas <- names_rdatas[-grep("gusmap", names_rdatas)]
      result_list <- list("data1" = data1, "data2"= data2, 
                          "data3"=data3, "data4"=data4, "data5"=data5, "data6"=data6, 
                          "choices" = list(depths, seeds, seeds_choices, depths_choices),
                          "names" = names_rdatas)
      
      system(paste("rm -r", paste(for_rm, collapse = " ")))
    })
    return(result_list)
  }
  
  datas_simu <- reactive({prepare_datas_simu(input$simulatedreads)})
  
  # Update choices of seed and depth according with the dateset choosed
  observe({
    seeds_choice <- datas_simu()[[7]][[3]] 
    depth_choice <- datas_simu()[[7]][[4]]
    
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
    
    
    updateSelectInput(session, "seed11",
                      label="Seed",
                      choices = seeds_choice,
                      selected=unlist(seeds_choice)[1])
    
    updateCheckboxGroupInput(session, "depth4",
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
        if(input$example_simu == "populus"){
          data.gz <- c("data/ig_populus_emp/EmpiricalReads_results.tar.gz")
        } else if(input$example_simu == "eucalyptus"){
          data.gz <- c("data/ig_eucalyptus_emp/EmpiricalReads_results.tar.gz")
        } else if(input$example_simu == "acca"){
          data.gz <- c("data/ig_acca_emp/EmpiricalReads_results.tar.gz")
        }
        path <- unlist(strsplit(data.gz[1], "/"))
        path <- paste0(paste0(path[-length(path)], collapse = "/"), "/")
      }
      list_files <- list()
      for(i in 1:length(data.gz)){
        untar(data.gz[i], exdir = path)
        list_files[[i]] <- untar(data.gz[i], list = T)
      }
      
      incProgress(0.5, detail = paste("Doing part", 2))
      list_files <- lapply(list_files, function(x) paste0(path, x))
      for_rm <- sapply(list_files, "[", 1)
      list_files <- lapply(list_files, "[", -1)
      
      # Data
      datas <- list()
      for(i in 1:7){
        datas[[i]] <- sapply(list_files, "[", i)
      }
      
      system(paste("mv", datas[[grep("sequences",datas)]], "data/temp_file/"))
      
      data5 <- load(datas[[grep("gusmap_RDatas.RData", datas)]])
      data5 <- get(data5)
      
      ## Tables
      names_rdatas <- readRDS(datas[[grep("names.rds", datas)]])
      names_rdatas <- names_rdatas[-grep("gusmap", names_rdatas)]
      result_list <- list("data1" = readRDS(datas[[grep("data1_depths_geno_prob.rds", datas)]]), 
                          "data2" = readRDS(datas[[grep("data2_maps.rds", datas)]]), 
                          "data3" = readRDS(datas[[grep("data3_filters.rds", datas)]]), 
                          "data4" = readRDS(datas[[grep("data4_times.rd", datas)]]), 
                          "data5" = data5, 
                          "names" = names_rdatas)
      
      system(paste("rm -r", paste(for_rm, collapse = " ")))
    })
    return(result_list)
  }
  
  datas_emp <- reactive({prepare_datas_emp(input$empiricalreads)})
  
  ##################################################################
  # Simulations
  ##################################################################
  
  output$disper_depth_out <- renderPlot({
    withProgress(message = 'Building left graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.1){
        if( input$ErrorProb1 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else {
          geno <- paste0(input$ErrorProb1, 0.05)
        }
      } else {
        geno <- input$ErrorProb1
      }
      
      if(input$CountsFrom1 == "bam" & (input$ErrorProb1 == "OneMap_version2" | input$ErrorProb1 == "SNPCaller")){
        stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
      }
      
      data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
        filter(SNPCall == input$SNPCall1) %>%
        filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed1)]) %>%
        filter(CountsFrom == input$CountsFrom1) %>%
        filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed1)])
      incProgress(0.5, detail = paste("Doing part", 2))
      errorProb_graph(data, input$real1)
    })
  })
  ## download
  output$disper_depth_out_down <- downloadHandler(
    filename =  function() {
      paste("snp_genoype_call.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building left graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.1){
          if( input$ErrorProb1 == "OneMap_version2"){
            geno <- paste0("SNPCaller", 0.05)
          } else {
            geno <- paste0(input$ErrorProb1, 0.05)
          }
        } else {
          geno <- input$ErrorProb1
        }
        
        if(input$CountsFrom1 == "bam" & (input$ErrorProb1 == "OneMap_version2" | input$ErrorProb1 == "SNPCaller")){
          stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
        }
        
        data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
          filter(SNPCall == input$SNPCall1) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed1)]) %>%
          filter(CountsFrom == input$CountsFrom1) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed1)])
        incProgress(0.5, detail = paste("Doing part", 2))
        p <- errorProb_graph(data, input$real1)
        ggsave(file, p)
      })
    } 
  )
  
  output$disper_depth2_out <- renderPlot({
    withProgress(message = 'Building right graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.2){
        if( input$ErrorProb2 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else {
          geno <- paste0(input$ErrorProb2, 0.05)
        }
      } else {
        geno <- input$ErrorProb2
      }
      data <- datas_simu()[[1]] %>% filter(GenoCall == geno) %>%
        filter(SNPCall == input$SNPCall2) %>%
        filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed2)]) %>%
        filter(CountsFrom == input$CountsFrom2) %>%
        filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed2)])
      incProgress(0.5, detail = paste("Doing part", 2))
      errorProb_graph(data, input$real2)
    })
  })
  
  ##################################################################
  output$ind_size_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.3){
        geno <- paste0(input$ErrorProb3, 0.05)
        if(any(input$ErrorProb3 %in% "OneMap_version2"))
          geno[which(input$ErrorProb3 == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb3 %in% "gusmap"))
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- input$ErrorProb3
      }
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall3) %>%
        filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed3)]) %>%
        filter(CountsFrom == input$CountsFrom3) %>%
        filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed3)]) %>%
        filter(fake == input$fake1)
      
      data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
        summarise(n = n()) 
      
      data <- data[,c(11,12, 10, 17)]
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -real.mks,)
      
      data$key <- gsub("n", "n markers", data$key)
      data$key <- gsub("diff", "diff (cM)", data$key)
      incProgress(0.5, detail = paste("Doing part", 2))
      ind_size_graph(data)
    })
  })
  
  ## download
  output$ind_size_out_down <- downloadHandler(
    filename =  function() {
      paste("ind_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.3){
          geno <- paste0(input$ErrorProb3, 0.05)
          if(any(input$ErrorProb3 %in% "OneMap_version2"))
            geno[which(input$ErrorProb3 == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb3 %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb3
        }
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall3) %>%
          filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed3)]) %>%
          filter(CountsFrom == input$CountsFrom3) %>%
          filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed3)]) %>%
          filter(fake == input$fake1)
        data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
          summarise(n = n()) 
        
        data <- data[,c(11,12, 10, 17)]
        
        data<- merge(data, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -real.mks,)
        
        data$key <- gsub("n", "n markers", data$key)
        data$key <- gsub("diff", "diff (cM)", data$key)
        incProgress(0.5, detail = paste("Doing part", 2))
        p <- ind_size_graph(data)
        ggsave(file, p)
      })
    } 
  )
  ##################################################################
  output$all_size_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.4){
        geno <- paste0(input$ErrorProb4, 0.05)
        if(any(input$ErrorProb4 %in% "OneMap_version2"))
          geno[which(input$ErrorProb4 == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb4 %in% "gusmap"))
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- input$ErrorProb4
      }
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall4) %>%
        filter(CountsFrom == input$CountsFrom4) %>%
        filter(depth %in% input$depth4) %>%
        filter(fake == input$fake2) %>%
        group_by(seed,GenoCall, SNPCall, CountsFrom, depth)
      
      data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
        summarise(n = n()) 
      
      data <- switch(input$stats1,
                     "mean" = summarise(data, value = mean(diff, na.rm=T)),
                     "median" = summarise(data, value = median(diff, na.rm=T)),
                     "var" = summarise(data, value = var(diff, na.rm=T)),
                     "total" = summarise(data, value = sum(diff, na.rm=T)))
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
      
      data$key <- gsub("n", "n markers", data$key)
      data$key <- gsub("value", "diff (cM)", data$key)
      incProgress(0.5, detail = paste("Doing part", 2))
      all_size_graph(data, input$stats1)
    })
  })
  
  ## download
  output$all_size_out_down <- downloadHandler(
    filename =  function() {
      paste("all_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.4){
          geno <- paste0(input$ErrorProb4, 0.05)
          if(any(input$ErrorProb4 %in% "OneMap_version2"))
            geno[which(input$ErrorProb4 == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb4 %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb4
        }
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall4) %>%
          filter(CountsFrom == input$CountsFrom4) %>%
          filter(depth %in% input$depth4) %>%
          filter(fake == input$fake2) %>%
          group_by(seed,GenoCall, SNPCall, CountsFrom, depth)
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
          summarise(n = n()) 
        
        data <- switch(input$stats1,
                       "mean" = summarise(data, value = mean(diff, na.rm=T)),
                       "median" = summarise(data, value = median(diff, na.rm=T)),
                       "var" = summarise(data, value = var(diff, na.rm=T)),
                       "total" = summarise(data, value = sum(diff, na.rm=T)))
        
        data<- merge(data, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
        
        data$key <- gsub("n", "n markers", data$key)
        data$key <- gsub("value", "diff (cM)", data$key)
        incProgress(0.5, detail = paste("Doing part", 2))
        
        p <- all_size_graph(data, input$stat1)
        ggsave(file, p)
      })
    } 
  )
  #######################################################################
  output$marker_type_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% input$ErrorProb10) %>%
        filter(SNPCall %in% input$SNPCall10) %>%
        filter(CountsFrom == input$CountsFrom10) %>%
        filter(depth == input$depth10) %>%
        filter(fake == input$fake3) %>%
        group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth) %>%
        summarise(n = n()) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n)
      incProgress(0.5, detail = paste("Doing part", 2))
      marker_type_graph(data)
    })
  })
  
  ## download
  output$marker_type_out_down <- downloadHandler(
    filename =  function() {
      paste("marker_type.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% input$ErrorProb10) %>%
          filter(SNPCall %in% input$SNPCall10) %>%
          filter(CountsFrom == input$CountsFrom10) %>%
          filter(depth == input$depth10) %>%
          filter(fake == input$fake3) %>%
          group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth) %>%
          summarise(n = n()) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n)
        incProgress(0.5, detail = paste("Doing part", 2))
        
        p <- marker_type_graph(data)
        ggsave(file, p)
      })
    } 
  )
  #######################################################################
  output$phases_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.8){
        geno <- paste0(input$ErrorProb8, 0.05)
        if(any(input$ErrorProb8 %in% "OneMap_version2"))
          geno[which(input$ErrorProb8 == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb8 %in% "gusmap"))
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- input$ErrorProb8
      }
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall8) %>%
        filter(CountsFrom == input$CountsFrom8) %>%
        filter(depth == input$depth8) %>%
        filter(fake == "without-false") 
      
      data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
        summarise(n = n()) 
      
      data <- data %>% group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
        summarise(value= 100*sum(est.phases == real.phases)/length(real.phases))
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
      
      data$key <- gsub("n", "n markers", data$key)
      data$key <- gsub("value", "% correct", data$key)
      
      incProgress(0.5, detail = paste("Doing part", 2))
      phases_graph(data)
    })
  })
  ## download
  output$phases_out_down <- downloadHandler(
    filename =  function() {
      paste("phases.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.8){
          geno <- paste0(input$ErrorProb8, 0.05)
          if(any(input$ErrorProb8 %in% "OneMap_version2"))
            geno[which(input$ErrorProb8 == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb8 %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb8
        }
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall8) %>%
          filter(CountsFrom == input$CountsFrom8) %>%
          filter(depth == input$depth8) %>%
          filter(fake == "without-false") 
        
        data_n <- data %>%  group_by(GenoCall, SNPCall, seed, depth) %>%
          summarise(n = n()) 
        
        data <- data %>% group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
          summarise(value= 100*sum(est.phases == real.phases)/length(real.phases))
        
        data<- merge(data, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -seed, -depth)
        
        data$key <- gsub("n", "n markers", data$key)
        data$key <- gsub("value", "% correct", data$key)
        
        incProgress(0.5, detail = paste("Doing part", 2))
        
        p <- phases_graph(data)
        ggsave(file, p)
      })
    } 
  )
  #######################################################################
  output$times_out <- renderPlot({
    withProgress(message = 'Times graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.9){
        geno <- paste0(input$ErrorProb9, 0.05)
        if(any(input$ErrorProb9 %in% "OneMap_version2"))
          geno[which(input$ErrorProb9 == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb9 %in% "gusmap"))
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- input$ErrorProb9
      }
      
      data_n <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall9) %>%
        filter(CountsFrom == input$CountsFrom9) %>%
        filter(fake == input$fake5) %>%
        group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
        summarise(n = n()) 
      
      data <- datas_simu()[[4]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall9) %>%
        filter(fake == input$fake5) %>%
        filter(CountsFrom == input$CountsFrom9)
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
      
      data$key <- gsub("n", "n markers", data$key)
      data$key <- gsub("time", "seconds", data$key)
      data$depth <- paste0("depth ", as.character(data$depth))
      
      incProgress(0.5, detail = paste("Doing part", 2))
      times_graph(data)
    })
  })
  
  ## download
  output$times_out_down <- downloadHandler(
    filename =  function() {
      paste("times.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.9){
        geno <- paste0(input$ErrorProb9, 0.05)
        if(any(input$ErrorProb9 %in% "OneMap_version2"))
          geno[which(input$ErrorProb9 == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb9 %in% "gusmap"))
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- input$ErrorProb9
      }
      
      data_n <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall9) %>%
        filter(CountsFrom == input$CountsFrom9) %>%
        filter(fake == input$fake5) %>%
        group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
        summarise(n = n()) 
      
      data <- datas_simu()[[4]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall9) %>%
        filter(fake == input$fake5) %>%
        filter(CountsFrom == input$CountsFrom9)
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
      
      data$key <- gsub("n", "n markers", data$key)
      data$key <- gsub("time", "seconds", data$key)
      data$depth <- paste0("depth ", as.character(data$depth))
      
      incProgress(0.5, detail = paste("Doing part", 2))
      
      p <- times_graph(data)      
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$coverage_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.5){
        geno <- paste0(input$ErrorProb5, 0.05)
        if(any(input$ErrorProb5 %in% "OneMap_version2"))
          geno[which(input$ErrorProb5 == "OneMap_version2")] <- "SNPCaller0.05"
        if(any(input$ErrorProb5 %in% "gusmap"))
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- input$ErrorProb5
      }
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall5) %>%
        filter(fake == input$fake6) %>%
        filter(depth == input$depth5) %>%
        filter(CountsFrom == input$CountsFrom5) %>%
        group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
        summarise(max = max(pos), min = min(pos))
      
      data$coverage <- ((data$max - data$min)/input$chr_size1)*100
      incProgress(0, detail = paste("Doing part", 1))
      coverage_graph(data)
    })
  })
  
  ## download
  output$coverage_out_down <- downloadHandler(
    filename =  function() {
      paste("coverage_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        if(input$Global0.05.5){
          geno <- paste0(input$ErrorProb5, 0.05)
          if(any(input$ErrorProb5 %in% "OneMap_version2"))
            geno[which(input$ErrorProb5 == "OneMap_version2")] <- "SNPCaller0.05"
          if(any(input$ErrorProb5 %in% "gusmap"))
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- input$ErrorProb5
        }
        data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall5) %>%
          filter(fake == input$fake6) %>%
          filter(depth == input$depth5) %>%
          filter(CountsFrom == input$CountsFrom5) %>%
          group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
          summarise(max = max(pos), min = min(pos))
        
        data$coverage <- ((data$max - data$min)/input$chr_size1)*100
        incProgress(0, detail = paste("Doing part", 1))
        
        p <- coverage_graph(data)
        ggsave(file, p)
        
      })
    } 
  )
  ##########################################################################
  output$snpcall_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      data <- datas_simu()[[5]] %>% filter(key %in% input$avalSNPs1) %>%
        filter(SNPCall %in% input$SNPCall6) %>%
        filter(depth %in% input$depth6)
      
      incProgress(0.5, detail = paste("Doing part", 2))
      avalSNPs_graph(data)
    })
  })
  
  ## download
  output$snpcall_out_down <- downloadHandler(
    filename =  function() {
      paste("snpcall.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- datas_simu()[[5]] %>% filter(key %in% input$avalSNPs1) %>%
        filter(SNPCall %in% input$SNPCall6) %>%
        filter(depth %in% input$depth6)
      
      p <- avalSNPs_graph(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$filters_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      choosed <- input$ErrorProb7
      if(input$Global0.05.7){
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
      data <- datas_simu()[[3]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall7) %>%
        filter(depth == input$depth7) %>%
        filter(CountsFrom == input$CountsFrom7)
      
      data$key <- factor(data$key, levels = c("n_markers", "mis_markers", "distorted_markers", "redundant_markers"))
      incProgress(0.5, detail = paste("Doing part", 2))
      filters_graph(data)
    })
  })  
  
  ## download
  output$filters_out_down <- downloadHandler(
    filename =  function() {
      paste("filters.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        choosed <- input$ErrorProb7
        if(input$Global0.05.7){
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
        data <- datas_simu()[[3]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall7) %>%
          filter(depth == input$depth7) %>%
          filter(CountsFrom == input$CountsFrom7)
        
        data$key <- factor(data$key, levels = c("n_markers", "mis_markers", "distorted_markers", "redundant_markers"))
        incProgress(0.5, detail = paste("Doing part", 2))     
        p <- filters_graph(data)
        ggsave(file, p)
      })
    } 
  )
  
  #################################
  output$map1_out <- renderImage({
    withProgress(message = 'Building draw', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      if(input$Global0.05.11){
        if(input$ErrorProb11 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb11 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb11, 0.05)
        }
      } else {
        geno <- input$ErrorProb11
      }
      incProgress(0.25, detail = paste("Doing part", 2))
      data <- datas_simu()[[2]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall11) %>%
        filter(seed == datas_simu()[[7]][[2]][as.numeric(input$seed11)]) %>%
        filter(CountsFrom == input$CountsFrom11) %>%
        filter(depth == datas_simu()[[7]][[1]][as.numeric(input$seed11)]) %>%
        filter(fake == input$fake11)
      
      incProgress(0.5, detail = paste("Doing part", 3))
      if(input$fake11 == "with-false"){
        false_mks <- as.character(data$mk.name[data$real.mks == "false positives"])
        data <-   data.frame(data$mk.name, data$rf)
        outfile <- paste0("temp.", sample(10000,1),".png")
        draw_map2(data, output = outfile, tag = false_mks, col.tag = "darkblue", pos = T, id = F)  
      } else {
        data <-   data.frame(data$mk.name, data$rf)
        outfile <- paste0("temp.", sample(10000,1),".png")
        draw_map2(data, output = outfile)
      }
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 900)
    })
  }, deleteFile = TRUE)
  
  output$map_out <- renderPlot({
    withProgress(message = 'Building heatmap', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      if(input$Global0.05.11){
        if( input$ErrorProb11 == "OneMap_version2"){
          geno <- paste0("default", 0.05)
        } else if (input$ErrorProb11 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb11, 0.05)
        }
      } else {
        if( input$ErrorProb11 == "OneMap_version2"){
          geno <- "default"
        } else {
          geno <- input$ErrorProb11
        }
      }
      
      if(input$CountsFrom11 == "bam" & (input$ErrorProb11 == "OneMap_version2" | input$ErrorProb11 == "SNPCaller")){
        stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
      }
      
      temp_n <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed11)])
      if(input$fake11 == "with-false") fake <- T else fake <- F
      temp_n <- paste0(datas_simu()[[7]][[2]][as.numeric(input$seed11)], "_",temp_n, 
                       "_map_",input$SNPCall11, "_", input$CountsFrom11, "_", geno, "_", fake)
      
      incProgress(0.25, detail = paste("Doing part", 2))
      if(geno == "gusmap"){
        data <- datas_simu()[[6]][[temp_n]]
        data$rf_2pt()
        incProgress(0.5, detail = paste("Doing part", 3))
        data$plotChr(mat="rf", parent = "both")
      } else {
        idx <- which(datas_simu()[[8]] == temp_n)
        data <- readList("data/temp_file/sequences.llo", index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        incProgress(0.5, detail = paste("Doing part", 3))
        rf_graph_table(data, inter = F, mrk.axis = "none")
      }
    })
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
        if(input$Global0.05.11){
          if( input$ErrorProb11 == "OneMap_version2"){
            geno <- paste0("SNPCaller", 0.05)
          } else if (input$ErrorProb11 == "gusmap"){
            stop("Gusmap do not allow to change the error rate. Please, select other option.")
          } else {
            geno <- paste0(input$ErrorProb11, 0.05)
          }
        } else {
          geno <- input$ErrorProb11
        }
        temp_n <- paste0(datas_simu()[[7]][[1]][as.numeric(input$seed11)])
        if(input$fake11 == "with-false") fake <- T else fake <- F
        temp_n <- paste0(datas_simu()[[7]][[2]][as.numeric(input$seed11)], "_",temp_n, 
                         "_map_",input$SNPCall11, "_", input$CountsFrom11, "_", geno, "_", fake)
        
        if(geno == "gusmap"){
          data <- datas_simu()[[6]][[temp_n]]
        } else {
          idx <- which(datas_simu()[[8]] == temp_n)
          data <- readList("data/temp_file/sequences.llo", index = idx)
          class(data) <- "sequence"
        }
        incProgress(0.5, detail = paste("Doing part", 2))
        outfile <- paste0(temp_n, ".RData")
        save(data, file = outfile)
      })
    } 
  )
  
  
  ##################################################################
  # Empirical 
  ##################################################################
  
  output$disper_depth_emp_out <- renderPlot({
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
      errorProb_graph_emp(data, input$real1_emp, input$geno_from1_emp)
    })
  })
  
  ## download
  output$disper_depth_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("depths.pdf")
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
        ggsave(file, p)
      })
    } 
  )
  
  output$disper_depth2_emp_out <- renderPlot({
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
      errorProb_graph_emp(data, input$real2_emp, input$geno_from2_emp)
    })
  })
  
  ##################################################################
  output$ind_size_emp_out <- renderPlot({
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
        filter(CountsFrom == input$CountsFrom3_emp) 
      
      data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
        summarise(n = n()) 
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -mks, -pos, -mk.type, -phase, - CountsFrom)
      
      data$key <- gsub("n", "n markers", data$key)
      data$key <- gsub("cm", "centimorgan", data$key)
      incProgress(0.5, detail = paste("Doing part", 2))
      ind_size_graph_emp(data)
    })
  })
  
  ## download
  output$ind_size_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("ind_size.pdf")
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
          filter(CountsFrom == input$CountsFrom3_emp) 
        
        data_n <- data %>%  group_by(GenoCall, SNPCall) %>%
          summarise(n = n()) 
        
        data<- merge(data, data_n) %>%
          gather(key, value, -GenoCall, -SNPCall, -mks, -pos, -mk.type, -phase, - CountsFrom)
        
        data$key <- gsub("n", "n markers", data$key)
        data$key <- gsub("cm", "centimorgan", data$key)
        incProgress(0.5, detail = paste("Doing part", 2))
        p <- ind_size_graph_emp(data)
        ggsave(file, p)
      })
    } 
  )
  #######################################################################
  output$marker_type_emp_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb10_emp) %>%
        filter(SNPCall %in% input$SNPCall10_emp) %>%
        filter(CountsFrom == input$CountsFrom10_emp) %>%
        group_by(mk.type, GenoCall, SNPCall, CountsFrom) %>%
        summarise(n = n()) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom,-n)
      incProgress(0.5, detail = paste("Doing part", 2))
      marker_type_graph_emp(data)
    })
  })
  
  ## download
  output$marker_type_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("marker_type_emp.pdf")
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
        ggsave(file, p)
      })
    } 
  )
  
  #######################################################################
  output$times_emp_out <- renderPlot({
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
      times_graph_emp(data)
    })
  })
  
  ## download
  output$times_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("times.pdf")
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
        ggsave(file, p)
      })
    } 
  )
  ####################################################################### 
  output$coverage_emp_out <- renderPlot({
    withProgress(message = 'Building graphic', value = 0, {
      incProgress(0, detail = paste("Doing part", 1))
      
      data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb5_emp) %>%
        filter(SNPCall %in% input$SNPCall5_emp) %>%
        filter(CountsFrom %in% input$CountsFrom5_emp) %>%
        group_by(GenoCall, SNPCall, CountsFrom) %>%
        summarise(max = max(pos), min = min(pos)) 
      
      data$coverage <- ((data$max - data$min)/input$chr_size)*100
      incProgress(0.5, detail = paste("Doing part", 2))
      
      coverage_graph_emp(data)
    })
  })
  
  ## download
  output$coverage_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("coverage_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      withProgress(message = 'Building graphic', value = 0, {
        incProgress(0, detail = paste("Doing part", 1))
        
        data <- datas_emp()[[2]] %>% filter(GenoCall %in% input$ErrorProb5_emp) %>%
          filter(SNPCall %in% input$SNPCall5_emp) %>%
          filter(CountsFrom %in% input$CountsFrom5_emp) %>%
          group_by(GenoCall, SNPCall, CountsFrom) %>%
          summarise(max = max(pos), min = min(pos)) 
        
        data$coverage <- ((data$max - data$min)/input$chr_size)*100
        incProgress(0.5, detail = paste("Doing part", 2))
        p <- coverage_graph_emp(data)
        ggsave(file, p)
      })
    } 
  )
  ##################################################################
  output$filters_emp_out <- renderPlot({
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
      data <- result_list[[3]] %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall7_emp) %>%
        filter(CountsFrom == input$CountsFrom7_emp) %>%
        gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
      incProgress(0.5, detail = paste("Doing part", 2))
      filters_graph_emp(data)
    })
  })
  
  ## download
  output$filters_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("filters.pdf")
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
        data <- result_list[[3]] %>% filter(GenoCall %in% geno) %>%
          filter(SNPCall %in% input$SNPCall7_emp) %>%
          filter(CountsFrom == input$CountsFrom7_emp) %>%
          gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
        incProgress(0.5, detail = paste("Doing part", 2))
        
        p <- filters_graph_emp(data)
        ggsave(file, p)
      })
    } 
  )
  ##################################################################
  output$heatmaps_emp_out <- renderPlotly({
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
      
      temp_n <- paste0("map_",input$SNPCall8_emp, "_", input$CountsFrom8_emp, "_", geno, ".RData")
      
      idx <- which(datas_emp()[[6]] == temp_n)
      data <- readList("data/temp_file/sequences_emp.llo", index = idx)
      data <- data[[1]]
      class(data) <- "sequence"
      incProgress(0.5, detail = paste("Doing part", 2))
      rf_graph_table(data, inter = T, html.file = "data/temp_file/temp.html",display = F) 
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
        data <- readList("data/temp_file/sequences_emp.llo", index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        incProgress(0.5, detail = paste("Doing part", 2))
        p <- rf_graph_table(data, inter = T, display = F, html.file = file) 
        htmlwidgets::saveWidget(p, file)
      })
    } 
  )
  
  ################################################################# 
  output$map_emp_out <- renderImage({
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
      outfile <- paste0("data/temp_file/temp.", sample(10000,1),".png")
      draw_map2(data, output = outfile, col.tag = "darkblue", pos = T, id = F)  
      
      list(src = outfile,
           contentType = 'image/png',
           width = 400,
           height = 900)
    })
  }, deleteFile = TRUE)
  
  output$map1_emp_out <- renderPlot({
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
      
      incProgress(0.25, detail = paste("Doing part", 2))
      if(geno == "gusmap"){
        data <- datas_emp()[[5]][[temp_n]]
        data$rf_2pt()
        incProgress(0.5, detail = paste("Doing part", 3))
        data$plotChr(mat="rf", parent = "both")
      } else {
        idx <- which(datas_emp()[[6]] == temp_n)
        data <- readList("data/temp_file/sequences_emp.llo", index = idx)
        data <- data[[1]]
        class(data) <- "sequence"
        incProgress(0.5, detail = paste("Doing part", 3))
        rf_graph_table(data, inter = F, mrk.axis = "none")
      }
    })
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
          data <- readList("data/temp_file/sequences_emp.llo", index = idx)
          data <- data[[1]]
          class(data) <- "sequence"
          save(data, file=file)
        }
      })
    }
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)