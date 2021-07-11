#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinymanager
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    dashboardPage(
      dashboardHeader(title = "Reads2Map App"),
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("About", tabName = "about", icon = icon("lightbulb")),
          #menuItem("Parallel map", icon = icon("dot-circle"), tabName = "parallel"), 
          menuItem("Upload data", icon = icon("upload"), tabName= "upload"),
          menuItem("SimulatedReads2Map results", icon = icon("dot-circle"), tabName= "simulations",
                   menuSubItem("SNP calling efficiency", icon = icon("circle"), tabName = "snpcall"),
                   #menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage"),
                   menuSubItem("Filters", icon = icon("circle"), tabName = "filters"),
                   menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type"),
                   menuSubItem("Times", icon = icon("circle"), tabName = "times"),
                   menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth"),
                   menuSubItem("Genotype probabilities", icon = icon("circle"), tabName = "probs"),
                   menuSubItem("ROC curves", icon = icon("circle"), tabName = "roc"),
                   menuSubItem("Map size each family", icon = icon("circle"), tabName = "ind_size"),
                   menuSubItem("Overview map size", icon = icon("circle"), tabName = "all_size"),
                   menuSubItem("Phases", icon = icon("circle"), tabName = "phases"),
                   menuSubItem("Maps", icon = icon("circle"), tabName = "map"),
                   menuSubItem("Progeny haplotypes", icon = icon("circle"), tabName = "haplo"),
                   menuSubItem("Breakpoints count", icon = icon("circle"), tabName = "counts"),
                   menuSubItem("cM x Mb", icon = icon("circle"), tabName = "cmxmb")),
          #menuSubItem("Overview", icon = icon("circle"), tabName = "overview")),
          
          menuItem("EmpiricalReads2Map results", icon = icon("dot-circle" ), tabName = "empirical",
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
      ),
      
      dashboardBody(
        # Lab colors
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
        
        .box.box-solid.box-info>.box-header {
        color:#fff;
        background:#003350
        }
        
        .box.box-solid.box-info{
        border-bottom-color:#003350;
        border-left-color:#003350;
        border-right-color:#003350;
        border-top-color:#003350;
        }
                              '))),
        
        tabItems(
          # First tab content
          tabItem(tabName = "about",
                  includeMarkdown(system.file("ext", "about.Rmd", package = "Reads2MapApp"))
          ),
          
          tabItem(tabName = "upload",
                  mod_upload_ui("upload_ui_1")
          ),
          # simulations
          tabItem(tabName = "snpcall",
                  mod_simu_SNPCalling_efficiency_ui("simu_SNPCalling_efficiency_ui_1")
          ),
          tabItem(tabName = "filters",
                  mod_simu_filters_ui("simu_filters_ui_1")
          ),
          tabItem(tabName = "marker_type",
                  mod_simu_markers_type_ui("simu_markers_type_ui_1")
          ),
          tabItem(tabName = "times",
          ),
          tabItem(tabName = "disper_depth",
          ),
          tabItem(tabName = "probs",
          ),
          tabItem(tabName = "roc",
          ),
          tabItem(tabName = "ind_size",
          ),
          tabItem(tabName = "all_size",
          ),
          tabItem(tabName = "phases",
          ),
          tabItem(tabName = "map",
          ),
          tabItem(tabName = "haplo",
          ),
          tabItem(tabName = "counts",
          ),
          tabItem(tabName = "cmxmb",
          ),
          # Empirical
          tabItem(tabName = "filters_emp",
          ),
          tabItem(tabName = "marker_type_emp",
          ),
          tabItem(tabName = "times_emp",
          ),
          tabItem(tabName = "disper_depth_emp",
          ),
          tabItem(tabName = "ind_size_emp",
          ),
          tabItem(tabName = "map_emp",
          ),
          tabItem(tabName = "haplo_emp",
          ),
          tabItem(tabName = "counts_emp",
          ),
          tabItem(tabName = "cmxmb_emp",
          ),
          # workflow times
          tabItem(tabName = "wf_times",
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Reads2MapApp'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}