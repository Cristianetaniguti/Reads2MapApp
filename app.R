
# Packages

library(shiny)
library(shinydashboard)
library(tidyverse)
library(onemap)
library(GUSMap)

# Functions
## Simulations
source("graphics_simu.R")
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
data1 <- data2 <- data3 <- data4 <- data5 <- rdata_n <- vector()
data6 <- list()

for(i in 1:length(datas)){
  for(j in 1:length(datas[[i]])){
    if(i == 6){
      temp <- load(datas[[i]][[j]])
      temp <- get(temp)
      temp_n <- names(Rdatas)[1]
      temp_n <- paste0(lapply(strsplit(temp_n, "_"), "[", 1:2)[[1]], collapse = "_")
      rdata_n <- c(rdata_n, temp_n)
      data6[[j]] <- Rdatas
    } else {
      temp <-  readRDS(datas[[i]][[j]])
      assign(paste0("data",i), rbind(get(paste0("data",i)), temp))
    }
  }
}

names(data6) <- rdata_n

# For errors
data1$errors <- apply(data1[,10:13], 1, function(x) if(all(x==1)) NA else 1 - x[which.max(x)])
data1$ref <- as.numeric(data1$ref)
# The onemap genotype codification do not diferenciate the homozyotes for each parent
data1$gabGT[data1$gabGT == 3 | data1$gabGT == 1] <- "homozygous"
data1$gabGT[data1$gabGT == 2] <- "heterozygote"
data1$methGT[data1$methGT == "3" | data1$methGT == "1"] <- "homozygous"
data1$methGT[data1$methGT == "2"] <- "heterozygote"
data1$methGT[data1$methGT == "0"] <- "missing"

data1$methGT <- factor(data1$methGT, labels = c("missing", "homozygous", "heterozygote"), levels = c("missing", "homozygous", "heterozygote"))
data1$gabGT <- factor(data1$gabGT, labels = c("missing", "homozygous", "heterozygote"), levels = c("missing", "homozygous", "heterozygote"))

temp <- levels(data1$GenoCall)
temp[1:2] <- c("OneMap_version2", "SNPCaller0.05")
data1$GenoCall <- as.character(data1$GenoCall)
data1$GenoCall[data1$GenoCall == "default"] <- "OneMap_version2"
data1$GenoCall[data1$GenoCall == "default0.05"] <- "SNPCaller0.05"
data1$GenoCall <- factor(data1$GenoCall, labels = temp, levels = temp)

####
data2 <- data2[,-3]
colnames(data2) <- c("seed", "depth", "mk.name", "pos" , "rf" , "type", "real.type", "est.phases", "real.phases", "real.mks", "SNPCall", "GenoCall", "CountsFrom",
                     "poscM", "poscM.norm", "diff")

data2$fake[which(data2$real.mks ==99)] <- "without-false"
data2$fake[which(data2$real.mks < 2)] <- "with-false"

data2$real.mks[which(data2$real.mks == 99 | data2$real.mks == 1)] <- "true markers"
data2$real.mks[which(data2$real.mks == 0)] <- "false positives"

temp <- levels(data2$GenoCall)
temp[1:2] <- c("OneMap_version2", "SNPCaller0.05")
data2$GenoCall <- as.character(data2$GenoCall)
data2$GenoCall[data2$GenoCall == "default"] <- "OneMap_version2"
data2$GenoCall[data2$GenoCall == "default0.05"] <- "SNPCaller0.05"
data2$GenoCall <- factor(data2$GenoCall, labels = temp, levels = temp)


###
data4 <- as.data.frame(data4)
data4$fake <- as.character(data4$fake)
data4$fake[which(data4$fake =="FALSE")] <- "without-false"
data4$fake[which(data4$fake =="TRUE")] <- "with-false"
data4$fake <- as.factor(data4$fake)

#####
data5 <- data5[,-7] ## Ajustar
names(data5) <- c("depth", "seed", "SNPCall", "(1)", "(2)", "(3)", "(4)", "(5)") ## Ajustars
data5 <- gather(data5, key,value,-SNPCall, -depth)

colnames(data3) <- c("seed", "depth", "n_markers", "distorted_markers", "redundant_markers", "SNPCall", "GenoCall", "CountsFrom")

data3$GenoCall <- factor(data3$GenoCall)
data3 <- gather(data3,key,value, -CountsFrom, -seed, -depth, -SNPCall, -GenoCall)
data3$key <- factor(data3$key)

# Defining the options
cout <- table(data1$seed, data1$depth)
depthNames <- colnames(cout)
depths <- seeds <- seedsNames <- vector()
for(i in 1:length(depthNames)){
  for(j in 1:nrow(cout)){
    if(cout[j,i] > 0){
    temp <- rownames(cout)[j]
    seedsNames <- c(seedsNames, paste0("Depth ", depthNames[i], " seed ", temp))
    depths <- c(depths, depthNames[i])
    seeds <- c(seeds, temp)
    }
  }
}

seeds_choice <- as.list(1:length(seedsNames))
names(seeds_choice) <- as.character(seedsNames)
ErrorProb_choice <- as.list(sort(levels(data1$GenoCall)))
names(ErrorProb_choice) <- as.character(sort(levels(data1$GenoCall)))
ErrorProb_choice <- ErrorProb_choice[-grep("0.05", ErrorProb_choice)]
global0.05_choices <- list("Global error of 0.05" = T, "Variable error" = FALSE)
maps_choice <- as.list(sort(levels(data2$GenoCall)))
names(maps_choice) <- as.character(sort(levels(data2$GenoCall)))
maps_choice <- maps_choice[-grep("0.05", maps_choice)]
SNPCall_choice <- as.list(sort(levels(data1$SNPCall)))
names(SNPCall_choice) <- as.character(sort(levels(data1$SNPCall)))
CountsFrom_choice <- as.list(as.character(sort(levels(data1$CountsFrom))))
names(CountsFrom_choice) <- as.character(sort(levels(data1$CountsFrom)))
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
fake_choices <- list("with-false", "without-false")
names(fake_choices) <- c("yes", "no")

####################################################################
# Empirical
####################################################################

data_map <- readRDS("data/data_map.rds")
data_map$GenoCall <- factor(data_map$GenoCall, levels = sort(levels(data_map$GenoCall)))
data_depths <- readRDS("data/data_depths.rds")
data_depths$GenoCall <- factor(data_depths$GenoCall, levels = sort(levels(data_depths$GenoCall)))
data_times <- readRDS("data/data_times.rds")
data_times$GenoCall <- factor(data_times$GenoCall, levels= sort(levels(data_times$GenoCall)))
data_filters <- readRDS("data/data_filters.rds")
data_filters$GenoCall <- factor(data_filters$GenoCall, levels= sort(levels(data_filters$GenoCall)))
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
             menuSubItem("Filters", icon = icon("circle"), tabName = "filters"),
             menuSubItem("Maps", icon = icon("circle"), tabName = "map")),
    
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
                         radioButtons("ErrorProb1", label = p("Genotype method"),
                                      choices = ErrorProb_choice,
                                      selected = "polyrad"),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.1", label = p("Global error of 0.05"),
                                            choices = global0.05_choices,
                                            selected = "FALSE"),
                         hr()
                       ),
                       
                       fluidPage(
                         radioButtons("real1", label = p("Genotypes"),
                                      choices = list("Simulated genotypes" = "simulated_genotypes", 
                                                     "Estimated genotypes"="estimated_genotypes",
                                                     "Errors rate" = "estimated_errors"),
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
                                      choices = ErrorProb_choice,
                                      selected = "polyrad"),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.2", label = p("Error type"),
                                      choices = global0.05_choices,
                                      selected = "FALSE"),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("real2", label = p("Genotypes"),
                                      choices = list("Simulated genotypes" = "simulated_genotypes", 
                                                    "Estimated genotypes"="estimated_genotypes",
                                                    "Errors rate" = "estimated_errors"),
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
                         checkboxGroupInput("ErrorProb3", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.3", label = p("Error type"),
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
                                       choices = seeds_choice,
                                       selected = names(seeds_choice)[1]),
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
                         checkboxGroupInput("ErrorProb4", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.4", label = p("Error type"),
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
                                              choices = depth_choice,
                                              selected = unlist(depth_choice)),
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
                         checkboxGroupInput("ErrorProb10", label = p("Genotype method"),
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
                                        choices = depth_choice,
                                        selected = depth_choice[[1]]),
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
                         checkboxGroupInput("ErrorProb8", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.8", label = p("Error type"),
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
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("CountsFrom8", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         hr()
                       ),
                       
                       fluidPage(
                         
                         radioButtons("fake4", label = p("Allow false positives?"),
                                      choices = fake_choices,
                                      selected = "without-false"),
                         hr(),
                         div(downloadButton("phases_out_down"),style="float:right")
                       )
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
                         checkboxGroupInput("ErrorProb9", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.9", label = p("Error type"),
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
                                              choices = depth_choice,
                                              selected = unlist(depth_choice)),
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
                         checkboxGroupInput("ErrorProb5", label = p("Genotype method"),
                                            choices = maps_choice,
                                            selected = names(maps_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.5", label = p("Error type"),
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
                                            choices = depth_choice,
                                            selected = unlist(depth_choice)),
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
                         checkboxGroupInput("ErrorProb7", label = p("Genotype method"),
                                            choices = ErrorProb_choice,
                                            selected = unlist(ErrorProb_choice)),
                         hr()
                       ),
                       fluidPage(
                         radioButtons("Global0.05.7", label = p("Error type"),
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
                                       choices = depth_choice,
                                       selected = depth_choice[[1]]),
                           hr(),
                           div(downloadButton("filters_out_down"),style="float:right")
                         )
                     )
              )
            )
    ),
    
    ################################################################################3
    tabItem(tabName = "map",
            "Here, at left, it is plotted the heatmap graphic with recombination fraction varying in color according with the 
            intensity of the linkage. Higher is the recombination fraction hottest is the color. Markers are represented in x 
            and y axis. On the right, the linkage group is plotted with distances between markers proportional to the genetic distances.",
            hr(),
            fluidRow(
              column(width = 8,
                     box(
                       width = NULL,
                       plotOutput("map_out"),
                       box(solidHeader = T,
                           radioButtons("ErrorProb11", label = p("Genotype method"),
                                        choices = maps_choice,
                                        selected = "updog"),
                       ),
                       fluidPage(
                         radioButtons("Global0.05.11", label = p("Error type"),
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
                                       choices = seeds_choice,
                                       selected = names(seeds_choice)[1]),
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
                         radioButtons("ErrorProb1_emp", label = p("Genotype method"),
                                      choices = ErrorProb_choice,
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
                         radioButtons("ErrorProb2_emp", label = p("Genotype method"),
                                      choices = ErrorProb_choice,
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
                       
                       fluidPage(
                         radioButtons("geno_from2_emp", label = p("Genotypes"),
                                      choices = list("vcf"="vcf",
                                                     "onemap" = "onemap"),
                                      selected = "onemap"),
                         hr()
                       ),
                       
                       #helpText("Select the SNP calling method"),
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
                         checkboxGroupInput("ErrorProb3_emp", label = p("Genotype method"),
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
                         )
                         ,
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
                         checkboxGroupInput("ErrorProb9_emp", label = p("Genotype method"),
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
                         checkboxGroupInput("ErrorProb7_emp", label = p("Genotype method"),
                                            choices = ErrorProb_choice,
                                            selected = unlist(ErrorProb_choice)),
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
              box(
                width = 4, 
                fluidPage(
                  radioButtons("ErrorProb8_emp", label = p("Genotype method"),
                               choices = ErrorProb_choice,
                               selected = "polyrad"),
                  hr()
                )
              ),
              
              #helpText("Select the SNP calling method"),
              box(width = 4,
                  fluidPage( # Tenho q acertar isso aqui
                    radioButtons("SNPCall8_emp", label = p("SNP calling method"),
                                 choices = SNPCall_choice,
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
                         radioButtons("SNPCall11_emp", label = p("SNP calling method"),
                                      choices = SNPCall_choice,
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
    
    data <- data1 %>% filter(GenoCall == geno) %>%
      filter(SNPCall == input$SNPCall1) %>%
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
      if(input$Global0.05.1){
        if( input$ErrorProb1 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else {
          geno <- paste0(input$ErrorProb1, 0.05)
        }
      } else {
        geno <- input$ErrorProb1
      }
      data <- data1 %>% filter(GenoCall == geno) %>%
        filter(SNPCall == input$SNPCall1) %>%
        filter(seed == seeds[as.numeric(input$seed1)]) %>%
        filter(CountsFrom == input$CountsFrom1) %>%
        filter(depth == depths[as.numeric(input$seed1)])
      p <- errorProb_graph(data, input$real1)
      ggsave(file, p)
    } 
  )
  
  output$disper_depth2_out <- renderPlot({
    
    if(input$Global0.05.2){
      if( input$ErrorProb2 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else {
        geno <- paste0(input$ErrorProb2, 0.05)
      }
    } else {
      geno <- input$ErrorProb2
    }
    data <- data1 %>% filter(GenoCall == geno) %>%
      filter(SNPCall == input$SNPCall2) %>%
      filter(seed == seeds[as.numeric(input$seed2)]) %>%
      filter(CountsFrom == input$CountsFrom2) %>%
      filter(depth == depths[as.numeric(input$seed2)])
    errorProb_graph(data, input$real2)
  })
  
  ##################################################################
  output$ind_size_out <- renderPlot({
    if(input$Global0.05.3){
      if( input$ErrorProb3 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else if (input$ErrorProb3 == "gusmap"){
        stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- paste0(input$ErrorProb3, 0.05)
      }
    } else {
      geno <- input$ErrorProb3
    }
    data <- data2 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall3) %>%
      filter(seed == seeds[as.numeric(input$seed3)]) %>%
      filter(CountsFrom == input$CountsFrom3) %>%
      filter(depth == depths[as.numeric(input$seed3)]) %>%
      filter(fake == input$fake1)
    ind_size_graph(data)
  })
  
  ## download
  output$ind_size_out_down <- downloadHandler(
    filename =  function() {
      paste("ind_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$Global0.05.3){
        if( input$ErrorProb3 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb3 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb3, 0.05)
        }
      } else {
        geno <- input$ErrorProb3
      }
      data <- data2 %>% filter(ErrorProb %in% geno) %>%
        filter(SNPCall %in% input$SNPCall3) %>%
        filter(seed == seeds[as.numeric(input$seed3)]) %>%
        filter(CountsFrom == input$CountsFrom3) %>%
        filter(depth == depths[as.numeric(input$seed3)]) %>%
        filter(fake == input$fake1)
      p <- ind_size_graph(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$all_size_out <- renderPlot({
    if(input$Global0.05.4){
      if( input$ErrorProb4 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else if (input$ErrorProb4 == "gusmap"){
        stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- paste0(input$ErrorProb4, 0.05)
      }
    } else {
      geno <- input$ErrorProb4
    }
    data <- data2 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall4) %>%
      filter(CountsFrom == input$CountsFrom4) %>%
      filter(depth == input$depth4) %>%
      filter(fake == input$fake2) %>%
      group_by(seed,GenoCall, SNPCall, CountsFrom, depth)
    
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
      if(input$Global0.05.4){
        if( input$ErrorProb4 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb4 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb4, 0.05)
        }
      } else {
        geno <- input$ErrorProb4
      }
      data <- data2 %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall4) %>%
        filter(CountsFrom == input$CountsFrom4) %>%
        filter(depth==input$depth4) %>%
        filter(fake == input$fake2) %>%
        group_by(seed,GenoCall, SNPCall, CountsFrom, depth)
      
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
    data <- data2 %>% filter(GenoCall %in% input$ErrorProb10) %>%
      filter(SNPCall %in% input$SNPCall10) %>%
      filter(CountsFrom == input$CountsFrom10) %>%
      filter(depth == input$depth10) %>%
      filter(fake == input$fake3) %>%
      group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth) %>%
      summarise(n = n()) %>%
      gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n)
    
    marker_type_graph(data)
  })
  
  ## download
  output$marker_type_out_down <- downloadHandler(
    filename =  function() {
      paste("marker_type.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data2 %>% filter(GenoCall %in% input$ErrorProb10) %>%
        filter(SNPCall %in% input$SNPCall10) %>%
        filter(CountsFrom == input$CountsFrom10) %>%
        filter(depth == input$depth10) %>%
        filter(fake == input$fake3) %>%
        group_by(type, real.type, GenoCall, SNPCall, CountsFrom, depth) %>%
        summarise(n = n()) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -depth,-n)
      
      p <- marker_type_graph(data)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$phases_out <- renderPlot({
    if(input$Global0.05.8){
      if( input$ErrorProb8 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else if (input$ErrorProb8 == "gusmap"){
        stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- paste0(input$ErrorProb8, 0.05)
      }
    } else {
      geno <- input$ErrorProb8
    }
    data <- data2 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall8) %>%
      filter(CountsFrom == input$CountsFrom8) %>%
      filter(depth == input$depth8) %>%
      filter(fake == input$fake4) %>%
      group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
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
      if(input$Global0.05.8){
        if( input$ErrorProb8 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb8 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb8, 0.05)
        }
      } else {
        geno <- input$ErrorProb8
      }
      data <- data2 %>% filter(GenoCall %in% input$ErrorProb8) %>%
        filter(SNPCall %in% input$SNPCall8) %>%
        filter(depth == input$depth8) %>%
        filter(fake == input$fake4) %>%
        group_by(seed,GenoCall, SNPCall, CountsFrom, depth) %>%
        summarise(value= 100*sum(est.phases == real.phases)/length(real.phases))
      
      p <- phases_graph(data)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$times_out <- renderPlot({
    if(input$Global0.05.9){
      if( input$ErrorProb9 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else if (input$ErrorProb8 == "gusmap"){
        stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- paste0(input$ErrorProb9, 0.05)
      }
    } else {
      geno <- input$ErrorProb9
    }
    
    data_n <- data2 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall9) %>%
      filter(CountsFrom == input$CountsFrom9) %>%
      filter(fake == input$fake5) %>%
      group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
      summarise(n = n()) 
    
    data <- data4 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall9) %>%
      filter(fake == input$fake5) %>%
      filter(CountsFrom == input$CountsFrom9)
    
    data<- merge(data, data_n) %>%
      gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
    
    data$key <- gsub("n", "number of markers", data$key)
    data$key <- gsub("times", "time (seconds)", data$key)
    
    times_graph(data)
  })
  
  ## download
  output$times_out_down <- downloadHandler(
    filename =  function() {
      paste("times.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$Global0.05.9){
        if( input$ErrorProb9 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb8 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb9, 0.05)
        }
      } else {
        geno <- input$ErrorProb9
      }
      
      data_n <- data2 %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall9) %>%
        filter(CountsFrom == input$CountsFrom9) %>%
        filter(fake == input$fake5) %>%
        group_by(GenoCall, SNPCall, CountsFrom, fake, seed, depth) %>%
        summarise(n = n()) 
      
      data <- data4 %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall9) %>%
        filter(fake == input$fake5) %>%
        filter(CountsFrom == input$CountsFrom9)
      
      data<- merge(data, data_n) %>%
        gather(key, value, -GenoCall, -SNPCall, -CountsFrom, -fake, -seed, -depth)
      
      data$key <- gsub("n", "number of markers", data$key)
      data$key <- gsub("times", "time (seconds)", data$key)
      
      p <- times_graph(data)      
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$coverage_out <- renderPlot({
    if(input$Global0.05.9){
      if( input$ErrorProb5 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else if (input$ErrorProb5 == "gusmap"){
        stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- paste0(input$ErrorProb5, 0.05)
      }
    } else {
      geno <- input$ErrorProb5
    }
    
    data <- data2 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall5) %>%
      filter(fake == input$fake6) %>%
      filter(depth == input$depth5) %>%
      filter(CountsFrom == input$CountsFrom5) %>%
      group_by(GenoCall, SNPCall, CountsFrom, fake, depth) %>%
      summarise(max = max(pos), min = min(pos))
    
    data$coverage <- ((data$max - data$min)/input$chr_size1)*100
    
    coverage_graph(data)
  })
  
  ## download
  output$coverage_out_down <- downloadHandler(
    filename =  function() {
      paste("coverage_size.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$Global0.05.9){
        if( input$ErrorProb5 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb5 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb5, 0.05)
        }
      } else {
        geno <- input$ErrorProb5
      }
      data <- data2 %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall5) %>%
        filter(fake == input$fake6) %>%
        filter(depth == input$depth5) %>%
        filter(CountsFrom == input$CountsFrom5) %>%
        group_by(GenoCall, SNPCall, CountsFrom, fake, depth) %>%
        summarise(max = max(pos), min = min(pos))
      
      data$coverage <- ((data$max - data$min)/input$chr_size1)*100
      
      p <- coverage_graph(data)
      ggsave(file, p)
    } 
  )
  ##########################################################################
  output$snpcall_out <- renderPlot({
    data <- data5 %>% filter(key %in% input$avalSNPs1) %>%
      filter(SNPCall %in% input$SNPCall6) %>%
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
        filter(SNPCall %in% input$SNPCall6) %>%
        filter(depth == input$depth6)
      
      p <- avalSNPs_graph(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$filters_out <- renderPlot({
    if(input$Global0.05.7){
      if( input$ErrorProb7 == "OneMap_version2"){
        geno <- paste0("SNPCaller", 0.05)
      } else if (input$ErrorProb7 == "gusmap"){
        stop("Gusmap do not allow to change the error rate. Please, select other option.")
      } else {
        geno <- paste0(input$ErrorProb7, 0.05)
      }
    } else {
      geno <- input$ErrorProb7
    }
    data <- data3 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall7) %>%
      filter(depth == input$depth7) %>%
      filter(CountsFrom == input$CountsFrom7)
    levels(data$key)
    data$key <- factor(data$key, levels = c("n_markers", "distorted_markers", "redundant_markers"))
    filters_graph(data)
  })
  
  ## download
  output$filters_out_down <- downloadHandler(
    filename =  function() {
      paste("filters.pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      if(input$Global0.05.7){
        if( input$ErrorProb7 == "OneMap_version2"){
          geno <- paste0("SNPCaller", 0.05)
        } else if (input$ErrorProb7 == "gusmap"){
          stop("Gusmap do not allow to change the error rate. Please, select other option.")
        } else {
          geno <- paste0(input$ErrorProb7, 0.05)
        }
      } else {
        geno <- input$ErrorProb7
      }
      data <- data3 %>% filter(GenoCall %in% geno) %>%
        filter(SNPCall %in% input$SNPCall7) %>%
        filter(depth == input$depth7) %>%
        filter(CountsFrom == input$CountsFrom7)
      
      p <- filters_graph(data)
      ggsave(file, p)
    } 
  )
  
  #################################
  output$map1_out <- renderImage({
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
    data <- data2 %>% filter(GenoCall %in% geno) %>%
      filter(SNPCall %in% input$SNPCall11) %>%
      filter(seed == seeds[as.numeric(input$seed11)]) %>%
      filter(CountsFrom == input$CountsFrom11) %>%
      filter(depth == depths[as.numeric(input$seed11)]) %>%
      filter(fake == input$fake11)
    
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
  }, deleteFile = TRUE)
  
  output$map_out <- renderPlot({
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
    temp_n <- paste0(seeds[as.numeric(input$seed11)], "_",depths[as.numeric(input$seed11)])
    data <- data6[[temp_n]]
    if(input$fake11 == "with-false") fake <- T else fake <- F
    temp_n <- paste0(temp_n, "_map_",input$SNPCall11, "_", input$CountsFrom11, "_", geno, "_", fake)
    data <- data[[temp_n]]
    
    if(geno == "gusmap"){
      data$rf_2pt()
      data$plotChr(mat="rf", parent = "both")
    } else {
      rf_graph_table(data, inter = F)
    }
  })
  
  ## download
  output$map1_out_down <- downloadHandler(
    filename =  function() {
      paste("map.RData")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
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
      temp_n <- paste0(seeds[as.numeric(input$seed11)], "_",depths[as.numeric(input$seed11)])
      data <- data6[[temp_n]]
      if(input$fake11 == "with-false") fake <- T else fake <- F
      temp_n <- paste0(temp_n, "_map_",input$SNPCall11, "_", input$CountsFrom11, "_", geno, "_", fake)
      data <- data[[temp_n]]
      
      outfile <- paste0(temp_n, ".RData")
      save(data, file = outfile)
    } 
  )
  
  
  ##################################################################
  # Empirical 
  ##################################################################
  
  output$disper_depth_emp_out <- renderPlot({
    
    data <- data_depths %>% filter(GenoCall == input$ErrorProb1_emp) %>%
      filter(SNPCall == input$SNPCall1_emp) %>%
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
        filter(SNPCall == input$SNPCall1_emp) %>%
        filter(CountsFrom == input$CountsFrom1_emp)
      p <- errorProb_graph_emp(data, input$real1_emp, input$geno_from1_emp)
      ggsave(file, p)
    } 
  )
  
  output$disper_depth2_emp_out <- renderPlot({
    data <- data_depths %>% filter(GenoCall == input$ErrorProb2_emp) %>%
      filter(SNPCall == input$SNPCall2_emp) %>%
      filter(CountsFrom == input$CountsFrom2_emp)
    errorProb_graph_emp(data, input$real2_emp, input$geno_from2_emp)
  })
  
  ##################################################################
  output$ind_size_emp_out <- renderPlot({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb3_emp) %>%
      filter(SNPCall %in% input$SNPCall3_emp) %>%
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
        filter(SNPCall %in% input$SNPCall3_emp) %>%
        filter(CountsFrom == input$CountsFrom3_emp) 
      p <- ind_size_graph_emp(data)
      ggsave(file, p)
    } 
  )
  #######################################################################
  output$marker_type_emp_out <- renderPlot({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb10_emp) %>%
      filter(SNPCall %in% input$SNPCall10_emp) %>%
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
        filter(SNPCall %in% input$SNPCall10) %>%
        filter(CountsFrom == input$CountsFrom10) %>%
        filter(depth == input$depth10) %>%
        group_by(type, real.type, ErrorProb, SNPCall, CountsFrom, depth) %>%
        summarise(n = n()) %>%
        gather(key, value, -ErrorProb, -SNPCall, -CountsFrom, -depth,-n)
      
      p <- marker_type_graph(data)
      ggsave(file, p)
    } 
  )
  
  #######################################################################
  output$times_emp_out <- renderPlot({
    
    data_n <- data_map %>% filter(GenoCall %in% input$ErrorProb9_emp) %>%
      filter(SNPCall %in% input$SNPCall9_emp) %>%
      filter(CountsFrom == input$CountsFrom9_emp) %>%
      group_by(GenoCall, SNPCall, CountsFrom) %>%
      summarise(n = n()) 
    
    data <- data_times %>% filter(GenoCall %in% input$ErrorProb9_emp) %>%
      filter(SNPCall %in% input$SNPCall9_emp) %>%
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
        filter(SNPCall %in% input$SNPCall9_emp) %>%
        filter(CountsFrom == input$CountsFrom9_emp)
      
      p <- times_graph_emp(data)
      
      ggsave(file, p)
    } 
  )
  ####################################################################### 
  output$coverage_emp_out <- renderPlot({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb5_emp) %>%
      filter(SNPCall %in% input$SNPCall5_emp) %>%
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
        filter(SNPCall %in% input$SNPCall5_emp) %>%
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
      filter(SNPCall %in% input$SNPCall7_emp) %>%
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
        filter(SNPCall %in% input$SNPCall7_emp) %>%
        filter(CountsFrom == input$CountsFrom7_emp) %>%
        gather(key, value, -CountsFrom, -GenoCall, -SNPCall)
      
      p <- filters_graph_emp(data)
      ggsave(file, p)
    } 
  )
  ##################################################################
  output$heatmaps_emp_out <- renderPlotly({
    data <- data_RDatas[[paste0(input$SNPCall8_emp, "_", input$CountsFrom8_emp, "_", input$ErrorProb8_emp)]]
    rf_graph_table(data, inter = T, html.file = "temp.html",display = F) 
  })
  
  ## download
  output$heatmaps_emp_out_down <- downloadHandler(
    filename =  function() {
      paste("heatmap.html")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      data <- data_RDatas[[paste0(input$SNPCall8_emp, "_", input$CountsFrom8_emp, "_", input$ErrorProb8_emp)]]
      outfile <- paste0("heatmap.", sample(10000,1),".html")
      p <- rf_graph_table(data, inter = T, display = F) 
      htmlwidgets::saveWidget(p, file = outfile)
    } 
  )
  
  ################################################################# 
  output$map_emp_out <- renderImage({
    data <- data_map %>% filter(GenoCall %in% input$ErrorProb11_emp) %>%
      filter(SNPCall %in% input$SNPCall11_emp) %>%
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
    data <- data_RDatas[paste0(input$SNPCall11_emp, "_", input$CountsFrom11_emp, "_", input$ErrorProb11_emp)]
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
      data <- data_RDatas[[paste0(input$SNPCall9_emp, "_", input$CountsFrom9_emp, "_", input$ErrorProb9_emp)]]
      outfile <- paste0("temp.", sample(10000,1),".png")
      draw_map2(data, output = outfile)
    } 
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)