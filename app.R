# Load all R scripts and functions
pkgload::load_all()
# Launch the application
shiny::shinyApp(OneMapWorkflowsApp(), server = OneMapWorkflowsApp) 