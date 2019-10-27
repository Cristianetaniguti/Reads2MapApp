
# Packages

library(shiny)
library(shinydashboard)
library(tidyverse)

# Functions
source("graphics.R")


# Data

data1 <- readRDS("data/data1_tot.rds")
data2 <- readRDS("data/data2_tot.rds")
data3 <- readRDS("data/data3_tot.rds")
data4 <- readRDS("data/data4_tot.rds")
data5 <- readRDS("data/data5_tot.rds")
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
seeds_choice <- as.list(unique(data1$seed))
names(seeds_choice) <- as.character(unique(data1$seed))
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

########
#  UI  #
########

## Header
header <- dashboardHeader(
  title = "Error workflow results",
  titleWidth = 250)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("lightbulb")),
    menuItem("Depth and genotyping", icon = icon("chart-line"), tabName = "disper_depth"),
    menuItem("Map size each family", icon = icon("chart-bar"), tabName = "ind_size"),
    menuItem("Overview map size", icon = icon("chart-bar"), tabName = "all_size"),
    menuItem("Phases", icon = icon("chart-bar"), tabName = "phases"),
    menuItem("Coverage", icon = icon("chart-bar"), tabName = "coverage"),
    menuItem("SNP calling efficiency", icon = icon("chart-bar"), tabName = "snpcall"),
    menuItem("Filters", icon = icon("chart-bar"), tabName = "filters"),
    menuItem("Parallel map", icon = icon("chart-bar"), tabName = "parallel")
  )
)

body <- dashboardBody(
  tabItems(
    ##########################################################
    tabItem(tabName = "about",
            includeMarkdown("about.Rmd")
    ),
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
                                     selected = seeds_choice[[1]]),
                         hr()
                       ),
                       
                       fluidPage(
                         
                         selectInput("depth1", label = p("Depth"),
                                     choices = depth_choice,
                                     selected = depth_choice[[1]]),
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
                                     selected = seeds_choice[[1]]),
                         hr()
                       ),
                       
                       fluidPage(
                         
                         selectInput("depth2", label = p("Depth"),
                                     choices = depth_choice,
                                     selected = depth_choice[[1]]),
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
                                     selected = seeds_choice[[1]]),
                         hr()
                       ),
                       
                       fluidPage(
                         
                         checkboxGroupInput("depth3", label = p("Depth"),
                                     choices = depth_choice,
                                     selected = unlist(depth_choice)),
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
    ####################################################################################
    tabItem(tabName = "parallel",
            includeHTML("parallel.html")
    )
    ####################################################################################
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")

## Define server logic required to draw a histogram ----
server <- function(input, output) {
  ##################################################################
  output$disper_depth_out <- renderPlot({
    data <- data1 %>% filter(ErrorProb == input$ErrorProb1) %>%
      filter(SNPcall == input$SNPcall1) %>%
      filter(seed == input$seed1) %>%
      filter(CountsFrom == input$CountsFrom1) %>%
      filter(depth == input$depth1)
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
        filter(seed == input$seed1) %>%
        filter(CountsFrom == input$CountsFrom1) %>%
        filter(depth == input$depth1)
      p <- errorProb_graph(data, input$real1)
      ggsave(file, p)
    } 
  )
  
  output$disper_depth2_out <- renderPlot({
    data <- data1 %>% filter(ErrorProb == input$ErrorProb2) %>%
      filter(SNPcall == input$SNPcall2) %>%
      filter(seed == input$seed2) %>%
      filter(CountsFrom == input$CountsFrom2) %>%
      filter(depth == input$depth2)
    errorProb_graph(data, input$real2)
  })
  
  ##################################################################
  output$ind_size_out <- renderPlot({
    data <- data2 %>% filter(ErrorProb %in% input$ErrorProb3) %>%
      filter(SNPcall %in% input$SNPcall3) %>%
      filter(seed == input$seed3) %>%
      filter(CountsFrom == input$CountsFrom3) %>%
      filter(depth == input$depth3)
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
        filter(seed == input$seed3) %>%
        filter(CountsFrom == input$CountsFrom3) %>%
        filter(depth == input$depth3)
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
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)