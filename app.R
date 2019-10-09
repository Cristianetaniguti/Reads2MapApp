library(shiny)
library(tidyverse)

source("graphics.R")
data1 <- readRDS("data/data1_depths_geno_prob.rds")
data2 <- readRDS("data/data2_maps.rds")
data3 <- readRDS("data/data3_coverage.rds")
data4 <- readRDS("data/data4_filters.rds")
data5 <- readRDS("data/data5_SNPcall_efficiency.rds")

seeds_choice <- as.list(unique(data4$seed))
names(seeds_choice) <- as.character(unique(data4$seed))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  titlePanel("Simulations results"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This app shows the results of reads simulations"),
      
      img(src = "LogoFundoTransp.jpg", height = 70, width = 200, align="center"),
      br(),
      br(),
      a("Lab homepage", href = "http://augustogarcia.me/statgen-esalq/"),
      br(),
      a("Cristiane homepage", href = "http://cristianetaniguti.github.io/"),
      hr(),
      
      helpText("Select the genotyping method"),
      fluidPage(
        radioButtons("ErrorProb", label = h3("Genotype method"), 
                           choices = list("Freebayes" = "GQ", "GATK" = "GQ", 
                                          "polyrad" = "polyrad", "updog" = "updog",
                                          "supermassa" = "supermassa"),
                           selected = "polyrad"),
        
        
        hr()
      ),
      
      # helpText("An overview of the SNP calling efficiency"),
      # fluidPage(
      #   checkboxGroupInput("SNPcall_ef", label = h3("SNP calling efficiency"), 
      #                      choices = list("simulated SNPs" = 1, "identified SNP" = 2, 
      #                                     "SNPs ok" = 2, "fake SNPs" = 3,
      #                                     "ref allele ok" = 4, "alt allele ok"= 5),
      #                      selected = 1),
      #   
      #   
      #   hr()
      # ),
      # 
      helpText("Select the SNP calling method"),
      fluidPage(
        radioButtons("SNPcall", label = h3("SNP calling method"), 
                           choices = list("Freebayes" = "freebayes", "GATK" = "gatk"),
                           selected = "freebayes"),
        
        
        hr()
      ),
      
      helpText("Select the family seed"),
      fluidPage(

        selectInput("seed", label = h3("Seed"), 
                    choices = seeds_choice, 
                    selected = seeds_choice[[1]]),
        
        hr()
      ),
      
      helpText("Read counts from:"),
      fluidPage(

        radioButtons("CountsFrom", label = h3("Counts from"),
                     choices = list("vcf" = "vcf", "bam" = "bam"), 
                     selected = "vcf"),
        
        hr()
      # ),
      
      # helpText("Choose the statistics to an overview of the recombination fractions"),
      # fluidPage(
      #   
      #   radioButtons("stat", label = h3("Overview"),
      #                choices = list("median" = 1, "mean" = 2,
      #                               "variance"= 3, "Total size" = 3), 
      #                selected = 1),
      #   
      #   hr()
      )
    ),
    mainPanel(
      plotOutput("ErrorProb_out")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$ErrorProb_out <- renderPlot({ 
    data <- data1 %>% filter(ErrorProb == input$ErrorProb) %>% 
            filter(SNPcall == input$SNPcall) %>%
            filter(seed == input$seed) %>%
            filter(CountsFrom == input$CountsFrom)
    errorProb_graph(data)
  })
  

  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)