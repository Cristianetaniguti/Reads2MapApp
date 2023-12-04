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
          menuItem("SimulatedReads2Map", icon = icon("dot-circle"), tabName= "simulations",
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
                   menuSubItem("cM x Mb", icon = icon("circle"), tabName = "cmxmb")
          ),
          #menuSubItem("Overview", icon = icon("circle"), tabName = "overview")),
          
          menuItem("EmpiricalReads2Map", icon = icon("dot-circle" ), tabName = "empirical",
                   #menuSubItem("Coverage", icon = icon("circle"), tabName = "coverage_emp"),
                   menuSubItem("SNP calling efficiency", icon = icon("circle"), tabName = "snpcall_emp"),
                   menuSubItem("Filters", icon = icon("circle"), tabName = "filters_emp"),
                   menuSubItem("Markers type", icon = icon("circle"), tabName = "marker_type_emp"),
                   menuSubItem("Times", icon = icon("circle"), tabName = "times_emp"),
                   menuSubItem("Depth and genotyping", icon = icon("circle"), tabName = "disper_depth_emp"),
                   menuSubItem("Map size", icon = icon("circle"), tabName = "ind_size_emp"),
                   menuSubItem("Plotly heatmaps", icon = icon("circle"), tabName = "heatmaps_emp"),
                   menuSubItem("Maps", icon = icon("circle"), tabName = "map_emp"),
                   menuSubItem("Progeny haplotypes", icon = icon("circle"), tabName = "haplo_emp"),
                   menuSubItem("Breakpoints count", icon = icon("circle"), tabName = "counts_emp"),
                   menuSubItem("cM x Mb", icon = icon("circle"), tabName = "cmxmb_emp")
          ),
          menuItem("Polyploid EmpiricalReads2Map", icon = icon("dot-circle" ), tabName = "poly_empirical",
                   menuSubItem("Data set overview", icon = icon("circle"), tabName = "dat_poly"),
                   menuSubItem("Map size", icon = icon("circle"), tabName = "size_poly"),
                   menuSubItem("Progeny haplotypes", icon = icon("circle"), tabName = "haplo_emp_poly")
          ),
          #menuItem("Workflow tasks times", icon = icon("circle"), tabName = "wf_times"),
          tags$li(class = "dropdown",
                  tags$a(href="https://statgen-esalq.github.io/", target="_blank", 
                         tags$img(height = "55px", alt="Logo", src="www/logo_fundo_azul.png")
                  ),
                  tags$a(href="https://www.polyploids.org/", target="_blank", 
                         tags$img(height = "55px", alt="Logo", src="www/logo_white.png"))
          )
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
                  mod_simu_times_ui("simu_times_ui_1")
          ),
          tabItem(tabName = "disper_depth",
                  mod_simu_depths_and_genotyping_ui("simu_depths_and_genotyping_ui_1")
          ),
          tabItem(tabName = "probs",
                  mod_simu_genotype_probabilities_ui("simu_genotype_probabilities_ui_1")
          ),
          tabItem(tabName = "roc",
                  mod_simu_roc_curves_ui("simu_roc_curves_ui_1")
          ),
          tabItem(tabName = "ind_size",
                  mod_simu_map_size_each_family_ui("simu_map_size_each_family_ui_1")
          ),
          tabItem(tabName = "all_size",
                  mod_simu_overview_map_size_ui("simu_overview_map_size_ui_1")
          ),
          tabItem(tabName = "phases",
                  mod_simu_phases_ui("simu_phases_ui_1")
          ),
          tabItem(tabName = "map",
                  mod_simu_maps_ui("simu_maps_ui_1")
          ),
          tabItem(tabName = "haplo",
                  mod_simu_progeny_haplotypes_ui("simu_progeny_haplotypes_ui_1")
          ),
          tabItem(tabName = "counts",
                  mod_simu_breakpoints_counts_ui("simu_breakpoints_counts_ui_1")
          ),
          tabItem(tabName = "cmxmb",
                  mod_simu_cM_Mb_ui("simu_cM_Mb_ui_1")
          ),
          # Empirical
          tabItem(tabName = "snpcall_emp",
                  mod_emp_SNPCalling_efficiency_ui("emp_SNPCalling_efficiency_ui_1")
          ),
          tabItem(tabName = "filters_emp",
                  mod_emp_filters_ui("emp_filters_ui_1")
          ),
          tabItem(tabName = "marker_type_emp",
                  mod_emp_markers_type_ui("emp_markers_type_ui_1")
          ),
          tabItem(tabName = "times_emp",
                  mod_emp_times_ui("emp_times_ui_1")
          ),
          tabItem(tabName = "disper_depth_emp",
                  mod_emp_depth_and_genotyping_ui("emp_depth_and_genotyping_ui_1")
          ),
          tabItem(tabName = "ind_size_emp",
                  mod_emp_ind_size_ui("emp_ind_size_ui_1")
          ),
          tabItem(tabName = "heatmaps_emp",
                  mod_emp_plotly_heatmaps_ui("emp_plotly_heatmaps_ui_1")
          ),
          tabItem(tabName = "map_emp",
                  mod_emp_maps_ui("emp_maps_ui_1")
          ),
          tabItem(tabName = "haplo_emp",
                  mod_emp_progeny_haplotypes_ui("emp_progeny_haplotypes_ui_1")
          ),
          tabItem(tabName = "counts_emp",
                  mod_emp_breakpoints_count_ui("emp_breakpoints_count_ui_1")
          ),
          tabItem(tabName = "cmxmb_emp",
                  mod_emp_cM_Mb_ui("emp_cM_Mb_ui_1")
          ),
          # Polyploids
          tabItem(tabName = "dat_poly",
                  mod_dat_poly_ui("dat_poly_ui_1")
          ),
          tabItem(tabName = "size_poly",
                  mod_size_poly_ui("size_poly_ui_1")
          ),
          tabItem(tabName = "haplo_emp_poly",
                  mod_haplo_emp_poly_ui("haplo_emp_poly_ui_1")
          ),
          # workflow times
          tabItem(tabName = "wf_times",
                  mod_workflow_tasks_times_ui("workflow_tasks_times_ui_1")
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