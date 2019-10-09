
# Packages

library(shiny)
library(shinydashboard)
library(tidyverse)

# Functions
source("graphics.R")


# Data

data1 <- readRDS("data/data1_depths_geno_prob.rds")
data2 <- readRDS("data/data2_maps.rds")
data3 <- readRDS("data/data3_coverage.rds")
data4 <- readRDS("data/data4_filters.rds")
data5 <- readRDS("data/data5_SNPcall_efficiency.rds")

seeds_choice <- as.list(unique(data1$seed))
names(seeds_choice) <- as.character(unique(data1$seed))
ErrorProb_choice <- as.list(levels(data1$ErrorProb))
names(ErrorProb_choice) <- as.character(unique(data1$ErrorProb))
SNPcall_choice <- as.list(levels(data1$SNPcall))
names(SNPcall_choice) <- as.character(unique(data1$SNPcall))
CountsFrom_choice <- as.list(levels(data1$CountsFrom))
names(CountsFrom_choice) <- as.character(unique(data1$CountsFrom))

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
    menuItem("Depth and genotyping", icon = icon("chart-line"), tabName = "graph1"),
    menuItem("Map size each family", icon = icon("chart-bar"), tabName = "graph2"),
    menuItem("Overview map size", icon = icon("chart-bar"), tabName = "graph3"),
    menuItem("Coverage", icon = icon("chart-bar"), tabName = "graph4"),
    menuItem("SNP calling efficiency", icon = icon("chart-bar"), tabName = "graph5"),
    menuItem("Error probabilities", icon = icon("chart-bar"), tabName = "graph6"),
    
    menuItem("Personal website", icon = icon("venus"), href= "http://cristianetaniguti.github.io/"),
    menuItem("Laboratory website", icon = icon("users"), href="http://augustogarcia.me/statgen-esalq/")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "graph1",
            fluidRow(
              column(width = 6,
                     box(
                       title = "Graph1", width = NULL, status = "primary",
                       plotOutput("graph1_out")
                     ),
                     box(
                       title = "Widgets", width = NULL, solidHeader = TRUE, status = "primary",
                       fluidPage(
                         radioButtons("ErrorProb1", label = p("Genotype method"),
                                      choices = ErrorProb_choice,
                                      selected = "polyrad"),
                         
                         
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
                       
                       #helpText("Read counts from:"),
                       fluidPage(
                         
                         radioButtons("CountsFrom1", label = p("Counts from"),
                                      choices = CountsFrom_choice,
                                      selected = "vcf"),
                         
                         hr()
                         # ),
                       )
                     )
              ),
              
              column(width = 6,
                     box(
                       title = "Graph2", status = "warning", width = NULL,
                       plotOutput("graph2_out")
                     ),
                     box(
                       title = "Widgets", width = NULL, solidHeader = TRUE, status = "warning",
                             fluidPage(
                               radioButtons("ErrorProb2", label = p("Genotype method"),
                                                  choices = ErrorProb_choice,
                                                  selected = "polyrad"),


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
    

    
    tabItem(tabName = "graph2",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "graph3",
            h2("Widgets tab content")
    ),
    tabItem(tabName = "graph4",
            h2("Dashboard tab content")
    ),
    
    tabItem(tabName = "graph5",
            h2("Widgets tab content")
    ),
    
    tabItem(tabName = "graph5",
            h2("Widgets tab content")
    )
  )
)


ui <- dashboardPage(header, sidebar, body)




#   sidebarLayout(
#     sidebarPanel(
#       helpText("This app shows the results of reads simulations"),
#       
#       img(src = "LogoFundoTransp.jpg", height = 70, width = 200, align="center"),
#       br(),
#       br(),
#       a("Lab homepage", href = "http://augustogarcia.me/statgen-esalq/"),
#       br(),
#       a("Cristiane homepage", href = "http://cristianetaniguti.github.io/"),
#       hr(),
#       
#       helpText("Select the genotyping method"),
#       fluidPage(
#         radioButtons("ErrorProb", label = h3("Genotype method"), 
#                            choices = list("Freebayes" = "GQ", "GATK" = "GQ", 
#                                           "polyrad" = "polyrad", "updog" = "updog",
#                                           "supermassa" = "supermassa"),
#                            selected = "polyrad"),
#         
#         
#         hr()
#       ),
#       
#       # helpText("An overview of the SNP calling efficiency"),
#       # fluidPage(
#       #   checkboxGroupInput("SNPcall_ef", label = h3("SNP calling efficiency"), 
#       #                      choices = list("simulated SNPs" = 1, "identified SNP" = 2, 
#       #                                     "SNPs ok" = 2, "fake SNPs" = 3,
#       #                                     "ref allele ok" = 4, "alt allele ok"= 5),
#       #                      selected = 1),
#       #   
#       #   
#       #   hr()
#       # ),
#       # 
#       helpText("Select the SNP calling method"),
#       fluidPage(
#         radioButtons("SNPcall", label = h3("SNP calling method"), 
#                            choices = list("Freebayes" = "freebayes", "GATK" = "gatk"),
#                            selected = "freebayes"),
#         
#         
#         hr()
#       ),
#       
#       helpText("Select the family seed"),
#       fluidPage(
# 
#         selectInput("seed", label = h3("Seed"), 
#                     choices = seeds_choice, 
#                     selected = seeds_choice[[1]]),
#         
#         hr()
#       ),
#       
#       helpText("Read counts from:"),
#       fluidPage(
# 
#         radioButtons("CountsFrom", label = h3("Counts from"),
#                      choices = list("vcf" = "vcf", "bam" = "bam"), 
#                      selected = "vcf"),
#         
#         hr()
#       # ),
#       
#       # helpText("Choose the statistics to an overview of the recombination fractions"),
#       # fluidPage(
#       #   
#       #   radioButtons("stat", label = h3("Overview"),
#       #                choices = list("median" = 1, "mean" = 2,
#       #                               "variance"= 3, "Total size" = 3), 
#       #                selected = 1),
#       #   
#       #   hr()
#       )
#     ),
#     mainPanel(
#       plotOutput("ErrorProb_out")
#     )
#   )
# )
# 
# # Define server logic required to draw a histogram ----
server <- function(input, output) {

    output$graph1_out <- renderPlot({
      data <- data1 %>% filter(ErrorProb == input$ErrorProb1) %>%
              filter(SNPcall == input$SNPcall1) %>%
              filter(seed == input$seed1) %>%
              filter(CountsFrom == input$CountsFrom1)
      errorProb_graph(data)
    })

    output$graph2_out <- renderPlot({
      data <- data1 %>% filter(ErrorProb == input$ErrorProb2) %>%
        filter(SNPcall == input$SNPcall2) %>%
        filter(seed == input$seed2) %>%
        filter(CountsFrom == input$CountsFrom2)
      errorProb_graph(data)
    })


}

# Create Shiny app ----
shinyApp(ui = ui, server = server)