
# Packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(onemap)
library(GUSMap)

# Functions
## Simulations
source("graphics.R")
## Empirical
source("graphics_emp.R")


##################################################
# Simulations
##################################################

# Data
datas <- list()
datas[[1]] <- Sys.glob("data/data1*")
datas[[2]] <- Sys.glob("data/data2*")
datas[[3]] <- Sys.glob("data/data3*")
datas[[4]] <- Sys.glob("data/data4*")
datas[[5]] <- Sys.glob("data/data5*")
datas[[6]] <- Sys.glob("data/data6*")

# Joint all depths
data1 <- data2 <- data3 <- data4 <- data5 <- data6 <- vector()
for(i in 1:length(datas)){
  for(j in 1:length(datas[[i]])){
    temp <-  readRDS(datas[[i]][[j]])
    assign(paste0("data",i), rbind(get(paste0("data",i)), temp))
  }
}

#load("data/results1.RData")

# For errors
data1$errors <- apply(data1[,12:15], 1, function(x) 1 - x[which.max(x)])

# The onemap genotype codification do not diferenciate the homozyotes for each parent
data1$gabGT[data1$gabGT == 3 | data1$gabGT == 1] <- "homozygous"
data1$gabGT[data1$gabGT == 2] <- "heterozygote"
data1$methGT[data1$methGT == "3" | data1$methGT == "1"] <- "homozygous"
data1$methGT[data1$methGT == "2"] <- "heterozygote"
data1$methGT[data1$methGT == "0"] <- "missing"

data1$methGT <- factor(data1$methGT, labels = c("missing", "homozygous", "heterozygote"), levels = c("missing", "homozygous", "heterozygote"))
data1$gabGT <- factor(data1$gabGT, labels = c("missing", "homozygous", "heterozygote"), levels = c("missing", "homozygous", "heterozygote"))

#####
data3 <- as.data.frame(data3)
data3$coverage <- as.numeric(as.character(data3$coverage))

data5 <- data5[,-7] ## Ajustar
names(data5) <- c("depth", "seed", "SNPcall", "(1)", "(2)", "(3)", "(4)", "(5)") ## Ajustars
data5 <- gather(data5, key,value,-SNPcall, -depth)

data4$GenoCall <- factor(data4$GenoCall, labels = c("polyrad", "updog", "supermassa", "GQ"))
data4 <- gather(data4,key,value, -CountsFrom, -seed, -depth, -SNPcall, -GenoCall)
data4$key <- factor(data4$key, levels = c("n_markers", "redundant_markers", "distorted_markers"))

# Defining the options
cout <- table(data1$seed, data1$depth)
depthNames <- colnames(cout)
seeds <- seedsNames <-  depths <- vector()
for(i in 1:length(depthNames)){
  temp <- names(which(table(data1$seed, data1$depth)[,i] != 0))
  seeds <- c(seeds, temp)
  depths <- c(depths, rep(depthNames[i], length(temp)))
  seedsNames <- c(seedsNames, paste0("Depth ", depthNames[i], " seed ", temp))
}

seeds_choice <- as.list(1:length(seedsNames))
names(seeds_choice) <- as.character(seedsNames)
ErrorProb_choice <- as.list(levels(data1$ErrorProb))
names(ErrorProb_choice) <- as.character(unique(data1$ErrorProb))
GenotypeCall_choice <- ErrorProb_choice[-1]
names(GenotypeCall_choice)[1] <- "genotype calling software = snp calling method"
maps_choice <- as.list(levels(data2$ErrorProb))
names(maps_choice) <- as.character(unique(data2$ErrorProb))
SNPcall_choice <- as.list(levels(data1$SNPcall))
names(SNPcall_choice) <- as.character(unique(data1$SNPcall))
CountsFrom_choice <- as.list(unique(data1$CountsFrom))
names(CountsFrom_choice) <- as.character(unique(data1$CountsFrom))
depth_choice <- as.list(unique(data1$depth))
names(depth_choice) <- as.character(unique(data1$depth))
stats_choice <- list("mean", "median", "var", "total")
names(stats_choice) <- c("mean", "median", "var", "total")
avalSNPs_choice <- list("(1)", "(2)", "(3)", "(4)", "(5)")
names(avalSNPs_choice) <- c("number of simulated SNPs (1)", 
                            "number of SNPs identified (2)", 
                            "number of correctly identified SNPs (3)",
                            "number of correctly identified reference allele (4)",
                            "number of correctly identified alternative allele (5)")

####################################################################
# Empirical
####################################################################

data_map <- readRDS("data/data_map.rds")
data_depths <- readRDS("data/data_depths.rds")
data_times <- readRDS("data/data_times.rds")
data_filters <- readRDS("data/data_filters.rds")
load("data/data_RDatas.RData")

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
    
    menuItem("Simulation results", icon = icon("dot-circle"), tabName= "simulations",
             menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth"),
             menuSubItem("Map size each family", icon = icon("circle"), tabName = "ind_size"),
             menuSubItem("Overview map size", icon = icon("circle"), tabName = "all_size"),
             menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type"),
             menuSubItem("Phases", icon = icon("circle"), tabName = "phases"),
             menuSubItem("Times", icon = icon("circle"), tabName = "times"),
             menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage"),
             menuSubItem("SNP calling efficiency", icon = icon("circle"), tabName = "snpcall"),
             menuSubItem("Filters", icon = icon("circle"), tabName = "filters")),
    
    menuItem("Empirical data results", icon = icon("dot-circle" ), tabName = "empirical",
             menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth_emp"),
             menuSubItem("Map size", icon = icon("circle"), tabName = "ind_size_emp"),
             menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type_emp"),
             menuSubItem("Times", icon = icon("circle"), tabName = "times_emp"),
             menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage_emp"),
             menuSubItem("Filters", icon = icon("circle"), tabName = "filters_emp"),
             menuSubItem("Heatmaps", icon = icon("circle"), tabName = "heatmaps_emp"),
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
    # Simulations
    ##########################################################
    tabItem(tabName = "disper_depth",
            fluidRow(
              column(width = 6,
                     box(
                       width = NULL,
                       plotOutput("disper_depth_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         radioButtons("ErrorProb1", label = p("Genotype method"),
                                      choices = GenotypeCall_choice,
                                      selected = "polyrad"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("real1", label = p("Genotypes"),
                                      choices = list("real_genotypes" = "real_genotypes", 
                                                     "estimated_genotypes"="estimated_genotypes",
                                                     "estimated_errors" = "estimated_errors"),
                                      selected = "estimated_genotypes"),
                         hr()
                       ),
                       
                       #helpText("Select the SNP calling method"),
                       fluidPage(
                         radioButtons("SNPcall1", label = p("SNP calling method"),
                                      choices = SNPcall_choice,
                                      selected = "freebayes"),
                         hr()
                       ),
                       
                       #helpText("Select the family seed"),
                       fluidPage(
                         
                         selectInput("seed1", label = p("Seed"),
                                     choices = seeds_choice,
                                     selected = names(seeds_choice)[1]),
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
                         radioButtons("ErrorProb2", label = p("Genotype method"),
                                      choices = GenotypeCall_choice,
                                      selected = "polyrad"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("real2", label = p("Genotypes"),
                                      choices = list("real_genotypes" = "real_genotypes", 
                                                     "estimated_genotypes"="estimated_genotypes",
                                                     "estimated_errors" = "estimated_errors"),
                                      selected = "estimated_genotypes"),
                         hr()
                       ),
                       
                       #helpText("Select the SNP calling method"),
                       fluidPage(
                         radioButtons("SNPcall2", label = p("SNP calling method"),
                                      choices = SNPcall_choice,
                                      selected = "freebayes"),
                         hr()
                       ),
                       
                       #helpText("Select the family seed"),
                       fluidPage(
                         
                         selectInput("seed2", label = p("Seed"),
                                     choices = seeds_choice,
                                     selected = names(seeds_choice)[1]),
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
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("ind_size_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb3", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       
                       #helpText("Select the SNP calling method"),
                       fluidPage(
                         checkboxGroupInput("SNPcall3", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = unlist(SNPcall_choice)),
                         hr()
                       ),
                       
                       #helpText("Select the family seed"),
                       fluidPage(
                         
                         selectInput("seed3", label = p("Seed"),
                                     choices = seeds_choice,
                                     selected = names(seeds_choice)[1]),
                         hr()
                       ),
                       
                       #helpText("Read counts from:"),
                       fluidPage(
                         
                         radioButtons("CountsFrom3", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
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
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("all_size_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         radioButtons("stats1", label = p("Statistic"),
                                      choices = stats_choice,
                                      selected = "mean"),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("ErrorProb4", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall4", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("depth4", label = p("Depth"),
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom4", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr(),
                         div(downloadButton("all_size_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ###################################################################################
    tabItem(tabName = "marker_type",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("marker_type_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb10", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall10", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("depth10", label = p("Depth"),
                                      choices = depth_choice,
                                      selected = depth_choice[[1]]),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom10", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr(),
                         div(downloadButton("marker_type_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ###################################################################################
    tabItem(tabName = "phases",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("phases_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb8", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall8", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("depth8", label = p("Depth"),
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom8", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr(),
                         div(downloadButton("phases_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ###################################################################################
    tabItem(tabName = "times",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("times_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb9", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall9", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("depth9", label = p("Depth"),
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom9", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr(),
                         div(downloadButton("times_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ###################################################################################
    tabItem(tabName = "coverage",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("coverage_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb5", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall5", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("depth5", label = p("Depth"),
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
                         hr(),
                         div(downloadButton("coverage_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ####################################################################################
    tabItem(tabName = "snpcall",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("snpcall_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("avalSNPs1", label = p("Options"),
                                            choices = avalSNPs_choice,
                                            selected = unlist(avalSNPs_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall6", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("depth6", label = p("Depth"),
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
                         hr(),
                         div(downloadButton("snpcall_out_down"),style="float:right")
                       )
                     )
              )
            )
    ),
    ################################################################################3
    tabItem(tabName = "filters",
            fluidRow(
              column(width = 12,
                     box(
                       width = NULL,
                       plotOutput("filters_out")
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb7", label = p("Genotype method"),
                                            choices = GenotypeCall_choice,
                                            selected = unlist(GenotypeCall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall7", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom7", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr()
                       ),
                       fluidPage(
                         selectInput("depth7", label = p("Depth"),
                                     choices = depth_choice,
                                     selected = depth_choice[[1]]),
                         hr(),
                         div(downloadButton("filters_out_down"),style="float:right")
                       )
                     )
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
                         radioButtons("ErrorProb1_emp", label = p("Genotype method"),
                                      choices = GenotypeCall_choice,
                                      selected = "polyrad"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("real1_emp", label = p("Display the:"),
                                      choices = list("estimated_genotypes"="estimated_genotypes",
                                                     "estimated_errors" = "estimated_errors"),
                                      selected = "estimated_genotypes"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("geno_from1_emp", label = p("Genotypes"),
                                      choices = list("vcf"="vcf",
                                                     "onemap" = "onemap"),
                                      selected = "onemap"),
                         hr()
                       ),
                       #helpText("Select the SNP calling method"),
                       fluidPage(
                         radioButtons("SNPcall1_emp", label = p("SNP calling method"),
                                      choices = SNPcall_choice,
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
                         radioButtons("ErrorProb2_emp", label = p("Genotype method"),
                                      choices = GenotypeCall_choice,
                                      selected = "polyrad"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("real2_emp", label = p("Genotypes"),
                                      choices = list("estimated_genotypes"="estimated_genotypes",
                                                     "estimated_errors" = "estimated_errors"),
                                      selected = "estimated_genotypes"),
                         hr()
                       ),
                       
                       #helpText("Select the SNP calling method"),
                       fluidPage(
                         radioButtons("SNPcall2_emp", label = p("SNP calling method"),
                                      choices = SNPcall_choice,
                                      selected = "freebayes"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("geno_from2_emp", label = p("Genotypes"),
                                      choices = list("vcf"="vcf",
                                                     "onemap" = "onemap"),
                                      selected = "onemap"),
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
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb3_emp", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       
                       #helpText("Select the SNP calling method"),
                       fluidPage(
                         checkboxGroupInput("SNPcall3_emp", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = unlist(SNPcall_choice)),
                         hr()
                       ),
                       
                       #helpText("Read counts from:"),
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
                         checkboxGroupInput("ErrorProb10_emp", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall10_emp", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
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
                         checkboxGroupInput("ErrorProb9_emp", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall9_emp", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
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
                         checkboxGroupInput("ErrorProb5_emp", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall5_emp", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
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
                     ),
                     box(
                       width = NULL, solidHeader = TRUE,
                       fluidPage(
                         checkboxGroupInput("ErrorProb7_emp", label = p("Genotype method"),
                                            choices = GenotypeCall_choice,
                                            selected = unlist(GenotypeCall_choice)),
                         hr()
                       ),
                       fluidPage(
                         checkboxGroupInput("SNPcall7_emp", label = p("SNP calling method"),
                                            choices = SNPcall_choice,
                                            selected = names(SNPcall_choice)),
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
              box(
                width = NULL,
                plotlyOutput("heatmaps_emp_out", height = 650)
              )
            ),
            fluidRow(
              box(
                width = 4, 
                fluidPage(
                  radioButtons("ErrorProb8_emp", label = p("Genotype method"),
                               choices = GenotypeCall_choice,
                               selected = "polyrad"),
                  hr()
                )
              ),
              
              #helpText("Select the SNP calling method"),
              box(width = 4,
                  fluidPage( # Tenho q acertar isso aqui
                    radioButtons("SNPcall8_emp", label = p("SNP calling method"),
                                 choices = SNPcall_choice,
                                 selected = "freebayes"),
                    hr()
                  )
              ),
              
              #helpText("Read counts from:"),
              box(width = 4,
                  fluidPage(
                    radioButtons("CountsFrom8_emp", label = p("Counts from"),
                                 choices = CountsFrom_choice,
                                 selected = "vcf"),
                    hr(),
                    div(downloadButton("heatmaps_emp_out_down"),style="float:right")
                    # ),
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
                         radioButtons("ErrorProb11_emp", label = p("Genotype method"),
                                      choices = maps_choice,
                                      selected = "updog"),
                       ),
                       box(
                         radioButtons("SNPcall11_emp", label = p("SNP calling method"),
                                      choices = SNPcall_choice,
                                      selected = "gatk"),
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
server <- function(input, output) {
  ##################################################################
  # Simulations
  ##################################################################
  
  output$disper_depth_out <- renderPlot({
    data <- data1 %>% filter(ErrorProb == input$ErrorProb1) %>%
      filter(SNPcall == input$SNPcall1) %>%
      filter(seed == seeds[as.numeric(input$seed1)]) %>%
      filter(CountsFrom == input$CountsFrom1) %>%
      filter(depth == depths[as.numeric(input$seed1)])
    errorProb_graph(data, input$real1)
  })
  
  ## download
  output$disper_depth_out_down <- downloadHandler(
    filename =  function() {
      paste("snp_genoype_call.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data1 %>% filter(ErrorProb == input$ErrorProb1) %>%
        filter(SNPcall == input$SNPcall1) %>%
        filter(seed == seeds[as.numeric(input$seed1)]) %>%
        filter(CountsFrom == input$CountsFrom1) %>%
        filter(depth == depths[as.numeric(input$seed1)])
      p <- errorProb_graph(data, input$real1)
      ggsave(file, p)
    } 
  )
  
  output$disper_depth2_out <- renderPlot({
    data <- data1 %>% filter(ErrorProb == input$ErrorProb2) %>%
      filter(SNPcall == input$SNPcall2) %>%
      filter(seed == seeds[as.numeric(input$seed2)]) %>%
      filter(CountsFrom == input$CountsFrom2) %>%
      filter(depth == depths[as.numeric(input$seed2)])
    errorProb_graph(data, input$real2)
  })
  
  ##################################################################
  output$ind_size_out <- renderPlot({
    data <- data2 %>% filter(ErrorProb %in% input$ErrorProb3) %>%
      filter(SNPcall %in% input$SNPcall3) %>%
      filter(seed == seeds[as.numeric(input$seed3)]) %>%
      filter(CountsFrom == input$CountsFrom3) %>%
      filter(depth == depths[as.numeric(input$seed3)])
    ind_size_graph(data)
  })
  
  ## download
  output$ind_size_out_down <- downloadHandler(
    filename =  function() {
      paste("ind_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data2 %>% filter(ErrorProb %in% input$ErrorProb3) %>%
        filter(SNPcall %in% input$SNPcall3) %>%
        filter(seed == seeds[as.numeric(input$seed3)]) %>%
        filter(CountsFrom == input$CountsFrom3) %>%
        filter(depth == depths[as.numeric(input$seed3)])
      p <- ind_size_graph(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$all_size_out <- renderPlot({
    data <- data2 %>% filter(ErrorProb %in% input$ErrorProb4) %>%
      filter(SNPcall %in% input$SNPcall4) %>%
      filter(CountsFrom == input$CountsFrom4) %>%
      filter(depth == input$depth4) %>%
      group_by(seed,ErrorProb, SNPcall, CountsFrom, depth)
    
    data <- switch(input$stats1,
                   "mean" = summarise(data, value = mean(rf, na.rm=T)),
                   "median" = summarise(data, value = median(rf, na.rm=T)),
                   "var" = summarise(data, value = var(rf, na.rm=T)),
                   "total" = summarise(data, value = sum(rf, na.rm=T)))
    
    all_size_graph(data, input$stats1)
  })
  
  ## download
  output$all_size_out_down <- downloadHandler(
    filename =  function() {
      paste("all_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data2 %>% filter(ErrorProb %in% input$ErrorProb4) %>%
        filter(SNPcall %in% input$SNPcall4) %>%
        filter(CountsFrom == input$CountsFrom4) %>%
        filter(depth==input$depth4) %>%
        group_by(seed,ErrorProb, SNPcall, CountsFrom, depth)
      
      data <- switch(input$stats1,
                     "mean" = summarise(data, value = mean(rf, na.rm=T)),
                     "median" = summarise(data, value = median(rf, na.rm=T)),
                     "var" = summarise(data, value = var(rf, na.rm=T)),
                     "total" = summarise(data, value = sum(rf, na.rm=T)))
      
      p <- all_size_graph(data, input$stat1)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$marker_type_out <- renderPlot({
    data <- data2 %>% filter(ErrorProb %in% input$ErrorProb10) %>%
      filter(SNPcall %in% input$SNPcall10) %>%
      filter(CountsFrom == input$CountsFrom10) %>%
      filter(depth == input$depth10) %>%
      group_by(type, real.type, ErrorProb, SNPcall, CountsFrom, depth) %>%
      summarise(n = n()) %>%
      gather(key, value, -ErrorProb, -SNPcall, -CountsFrom, -depth,-n)
    
    marker_type_graph(data)
  })
  
  ## download
  output$phases_out_down <- downloadHandler(
    filename =  function() {
      paste("phases.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data2 %>% filter(ErrorProb %in% input$ErrorProb10) %>%
        filter(SNPcall %in% input$SNPcall10) %>%
        filter(CountsFrom == input$CountsFrom10) %>%
        filter(depth == input$depth10) %>%
        group_by(type, real.type, ErrorProb, SNPcall, CountsFrom, depth) %>%
        summarise(n = n()) %>%
        gather(key, value, -ErrorProb, -SNPcall, -CountsFrom, -depth,-n)
      
      p <- marker_type_graph(data)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$phases_out <- renderPlot({
    data <- data2 %>% filter(ErrorProb %in% input$ErrorProb8) %>%
      filter(SNPcall %in% input$SNPcall8) %>%
      filter(CountsFrom == input$CountsFrom8) %>%
      filter(depth == input$depth8) %>%
      group_by(seed,ErrorProb, SNPcall, CountsFrom, depth) %>%
      summarise(value= 100*sum(est.phases == real.phases)/length(real.phases))
    
    phases_graph(data)
  })
  
  ## download
  output$phases_out_down <- downloadHandler(
    filename =  function() {
      paste("phases.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data2 %>% filter(ErrorProb %in% input$ErrorProb8) %>%
        filter(SNPcall %in% input$SNPcall8) %>%
        filter(depth == input$depth8) %>%
        group_by(seed,ErrorProb, SNPcall, CountsFrom, depth) %>%
        summarise(value= 100*sum(est.phases == real.phases)/length(real.phases))
      
      p <- phases_graph(data)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$times_out <- renderPlot({
    data <- data6 %>% filter(Genocall %in% input$ErrorProb9) %>%
      filter(SNPcall %in% input$SNPcall9) %>%
      filter(CountsFrom == input$CountsFrom9)
    
    times_graph(data)
  })
  
  ## download
  output$phases_out_down <- downloadHandler(
    filename =  function() {
      paste("phases.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data6 %>% filter(Genocall %in% input$ErrorProb9) %>%
        filter(SNPcall %in% input$SNPcall9) %>%
        filter(CountsFrom == input$CountsFrom9) %>%
        filter(depth == input$depth9) 
      
      p <- times_graph(data)      
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$coverage_out <- renderPlot({
    data <- data3 %>% filter(ErrorProb %in% input$ErrorProb5) %>%
      filter(SNPcall %in% input$SNPcall5) %>%
      filter(depth == input$depth5)
    
    coverage_graph(data)
  })
  
  ## download
  output$coverage_out_down <- downloadHandler(
    filename =  function() {
      paste("coverage_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data3 %>% filter(ErrorProb %in% input$ErrorProb5) %>%
        filter(SNPcall %in% input$SNPcall5) %>%
        filter(depth == input$depth5)
      
      p <- coverage_graph(data)
      ggsave(file, p)
    } 
  )
  ##########################################################################
  output$snpcall_out <- renderPlot({
    data <- data5 %>% filter(key %in% input$avalSNPs1) %>%
      filter(SNPcall %in% input$SNPcall6) %>%
      filter(depth == input$depth6)
    
    avalSNPs_graph(data)
  })
  
  ## download
  output$snpcall_out_down <- downloadHandler(
    filename =  function() {
      paste("snpcall.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data5 %>% filter(key %in% input$avalSNPs1) %>%
        filter(SNPcall %in% input$SNPcall6) %>%
        filter(depth == input$depth6)
      
      p <- avalSNPs_graph(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$filters_out <- renderPlot({
    data <- data4 %>% filter(GenoCall %in% input$ErrorProb7) %>%
      filter(SNPcall %in% input$SNPcall7) %>%
      filter(depth == input$depth7) %>%
      filter(CountsFrom == input$CountsFrom7)
    
    filters_graph(data)
  })
  
  ## download
  output$filters_out_down <- downloadHandler(
    filename =  function() {
      paste("filters.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data4 %>% filter(GenoCall %in% input$ErrorProb7) %>%
        filter(SNPcall %in% input$SNPcall7) %>%
        filter(depth == input$depth7) %>%
        filter(CountsFrom == input$CountsFrom7)
      
      p <- filters_graph(data)
      ggsave(file, p)
    } 
  )
  
  ##################################################################
  # Empirical 
  ##################################################################
  
  output$disper_depth_emp_out <- renderPlot({
    
    data <- data_depths %>% filter(GenoCall == input$ErrorProb1_emp) %>%
      filter(SNPCall == input$SNPcall1_emp) %>%
      filter(CountsFrom == input$CountsFrom1_emp)
    errorProb_graph_emp(data, input$real1_emp, input$geno_from1_emp)
  })
  
  ## download
  output$disper_depth_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("depths.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_depths %>% filter(GenoCall == input$ErrorProb1_emp) %>%
        filter(SNPCall == input$SNPcall1_emp) %>%
        filter(CountsFrom == input$CountsFrom1_emp)
      p <- errorProb_graph_emp(data, input$real1_emp, input$geno_from1_emp)
      ggsave(file, p)
    } 
  )
  
  output$disper_depth2_emp_out <- renderPlot({
    data <- data_depths %>% filter(GenoCall == input$ErrorProb2_emp) %>%
      filter(SNPCall == input$SNPcall2_emp) %>%
      filter(CountsFrom == input$CountsFrom2_emp)
    errorProb_graph_emp(data, input$real2_emp, input$geno_from2_emp)
  })
  
  ##################################################################
  output$ind_size_emp_out <- renderPlot({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb3_emp) %>%
      filter(SNPCall %in% input$SNPcall3_emp) %>%
      filter(CountsFrom == input$CountsFrom3_emp) 
    ind_size_graph_emp(data)
  })
  
  ## download
  output$ind_size_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("ind_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_map %>% filter(GenoCall %in% input$ErrorProb3_emp) %>%
        filter(SNPCall %in% input$SNPcall3_emp) %>%
        filter(CountsFrom == input$CountsFrom3_emp) 
      p <- ind_size_graph_emp(data)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$marker_type_emp_out <- renderPlot({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb10_emp) %>%
      filter(SNPCall %in% input$SNPcall10_emp) %>%
      filter(CountsFrom == input$CountsFrom10_emp) %>%
      group_by(type, GenoCall, SNPCall, CountsFrom) %>%
      summarise(n = n()) %>%
      gather(key, value, -GenoCall, -SNPCall, -CountsFrom,-n)
    
    marker_type_graph_emp(data)
  })
  
  ## download
  output$phases_out_down <- downloadHandler(
    filename =  function() {
      paste("phases.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data2 %>% filter(ErrorProb %in% input$ErrorProb10) %>%
        filter(SNPcall %in% input$SNPcall10) %>%
        filter(CountsFrom == input$CountsFrom10) %>%
        filter(depth == input$depth10) %>%
        group_by(type, real.type, ErrorProb, SNPcall, CountsFrom, depth) %>%
        summarise(n = n()) %>%
        gather(key, value, -ErrorProb, -SNPcall, -CountsFrom, -depth,-n)
      
      p <- marker_type_graph(data)
      ggsave(file, p)
    } 
  )
  
  #######################################################################
  output$times_emp_out <- renderPlot({
    
    data_n <- data_map %>% filter(GenoCall %in% input$ErrorProb9_emp) %>%
      filter(SNPCall %in% input$SNPcall9_emp) %>%
      filter(CountsFrom == input$CountsFrom9_emp) %>%
      group_by(GenoCall, SNPCall, CountsFrom) %>%
      summarise(n = n()) 
    
    
    data <- data_times %>% filter(GenoCall %in% input$ErrorProb9_emp) %>%
      filter(SNPCall %in% input$SNPcall9_emp) %>%
      filter(CountsFrom == input$CountsFrom9_emp)
    
    data<- merge(data, data_n) %>%
      gather(key, value, -GenoCall, -SNPCall, -CountsFrom)
    
    data$key <- gsub("n", "number of markers", data$key)
    data$key <- gsub("time_par.3.", "time (seconds)", data$key)
    
    times_graph_emp(data)
  })
  
  ## download
  output$times_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("times.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_times %>% filter(GenoCall %in% input$ErrorProb9_emp) %>%
        filter(SNPCall %in% input$SNPcall9_emp) %>%
        filter(CountsFrom == input$CountsFrom9_emp)
      
      p <- times_graph_emp(data)
      
      ggsave(file, p)
    } 
  )
  ####################################################################### 
  output$coverage_emp_out <- renderPlot({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb5_emp) %>%
      filter(SNPCall %in% input$SNPcall5_emp) %>%
      filter(CountsFrom %in% input$CountsFrom5_emp) %>%
      group_by(GenoCall, SNPCall, CountsFrom) %>%
      summarise(max = max(pos), min = min(pos)) 
    
    data$coverage <- ((data$max - data$min)/input$chr_size)*100
    coverage_graph_emp(data)
  })
  
  ## download
  output$coverage_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("coverage_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_map %>% filter(GenoCall %in% input$ErrorProb5_emp) %>%
        filter(SNPCall %in% input$SNPcall5_emp) %>%
        filter(CountsFrom %in% input$CountsFrom5_emp) %>%
        group_by(GenoCall, SNPCall, CountsFrom) %>%
        summarise(max = max(pos), min = min(pos)) 
      
      data$coverage <- ((data$max - data$min)/input$chr_size)*100
      p <- coverage_graph_emp(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$filters_emp_out <- renderPlot({
    data <- data_filters %>% filter(GenoCall %in% input$ErrorProb7_emp) %>%
      filter(SNPCall %in% input$SNPcall7_emp) %>%
      filter(CountsFrom == input$CountsFrom7_emp) %>%
      gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
    
    filters_graph_emp(data)
  })
  
  ## download
  output$filters_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("filters.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_filters %>% filter(GenoCall %in% input$ErrorProb7_emp) %>%
        filter(SNPCall %in% input$SNPcall7_emp) %>%
        filter(CountsFrom == input$CountsFrom7_emp) %>%
        gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
      
      p <- filters_graph_emp(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$heatmaps_emp_out <- renderPlotly({
    data <- data_RDatas[[paste0(input$SNPcall8_emp, "_", input$CountsFrom8_emp, "_", input$ErrorProb8_emp)]]
    rf_graph_table(data, inter = T, html.file = "temp.html",display = F) 
  })
  
  ## download
  output$heatmaps_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("heatmap.html")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_RDatas[[paste0(input$SNPcall8_emp, "_", input$CountsFrom8_emp, "_", input$ErrorProb8_emp)]]
      outfile <- paste0("heatmap.", sample(10000,1),".html")
      p <- rf_graph_table(data, inter = T, display = F) 
      htmlwidgets::saveWidget(p, file = outfile)
    } 
  )
  
  ################################################################# 
  output$map_emp_out <- renderImage({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb11_emp) %>%
      filter(SNPCall %in% input$SNPcall11_emp) %>%
      filter(CountsFrom == input$CountsFrom11_emp) 
    
    data <-   data.frame(data$mks, data$rf)
      
    outfile <- paste0("temp.", sample(10000,1),".png")
    draw_map2(data, output = outfile)
    
    list(src = outfile,
         contentType = 'image/png',
         width = 300,
         height = 800)
  }, deleteFile = TRUE)
  
  output$map1_emp_out <- renderPlot({
    data <- data_RDatas[paste0(input$SNPcall11_emp, "_", input$CountsFrom11_emp, "_", input$ErrorProb11_emp)]
    if(input$ErrorProb11_emp == "gusmap"){
      data[[1]]$rf_2pt()
      data[[1]]$plotChr(mat="rf", parent = "both")
    } else {
      rf_graph_table(data[[1]], inter = F)
    }
  })
  
  ## download
  output$map_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("map.png")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_RDatas[[paste0(input$SNPcall9_emp, "_", input$CountsFrom9_emp, "_", input$ErrorProb9_emp)]]
      outfile <- paste0("temp.", sample(10000,1),".png")
      draw_map2(data, output = outfile)
    } 
  )
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)