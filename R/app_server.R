#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  ####################
  # Upload size
  options(shiny.maxRequestSize=200*1024^2)
  
  #####################
  # Credentials
  ####################
  passwords <- c("Visitor" = "MuitoMapa123")

  credentials <- data.frame(
    user =     names(passwords), # mandatory
    password = as.vector(passwords), # mandatory
    start = c(rep("2021-02-28", length(passwords))), # optinal (all others)
    expire = c(rep(NA,length(passwords))),
    admin = c(TRUE, rep(FALSE, length(passwords)-1)),
    comment = "Simple and secure authentification mechanism
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
  )

  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  ## Start modules
  datas <- callModule(mod_upload_server,"upload_ui_1")
  
  # simulations
  callModule(mod_simu_SNPCalling_efficiency_server,
             "simu_SNPCalling_efficiency_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_filters_server,
             "simu_filters_ui_1",
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_markers_type_server, 
             "simu_markers_type_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_times_server,
             "simu_times_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_depths_and_genotyping_server, 
             "simu_depths_and_genotyping_ui_1",
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_genotype_probabilities_server,
             "simu_genotype_probabilities_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_roc_curves_server,
             "simu_roc_curves_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_map_size_each_family_server,
             "simu_map_size_each_family_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_overview_map_size_server,
             "simu_overview_map_size_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_phases_server,
             "simu_phases_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_maps_server,
             "simu_maps_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_progeny_haplotypes_server, 
             "simu_progeny_haplotypes_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_breakpoints_counts_server, 
             "simu_breakpoints_counts_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_simu_cM_Mb_server, 
             "simu_cM_Mb_ui_1", 
             datas_simu = datas$datas_simu)
  
  callModule(mod_emp_SNPCalling_efficiency_server, 
             "emp_SNPCalling_efficiency_ui_1", 
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_filters_server, 
             "emp_filters_ui_1", 
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_markers_type_server, 
             "emp_markers_type_ui_1", 
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_times_server,
             "emp_times_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_depth_and_genotyping_server,
             "emp_depth_and_genotyping_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_maps_server,
             "emp_maps_ui_1", 
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_ind_size_server, 
             "emp_ind_size_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_plotly_heatmaps_server,
             "emp_plotly_heatmaps_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_progeny_haplotypes_server, 
             "emp_progeny_haplotypes_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_breakpoints_count_server, 
             "emp_breakpoints_count_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_emp_cM_Mb_server,"emp_cM_Mb_ui_1",
             datas_emp = datas$datas_emp)
  
  callModule(mod_dat_poly_server,"dat_poly_ui_1",
             datas_poly_emp = datas$datas_poly_emp)
  
  callModule(mod_size_poly_server,"size_poly_ui_1",
             datas_poly_emp = datas$datas_poly_emp)
 
  callModule(mod_haplo_emp_poly_server,"haplo_emp_poly_ui_1",
             datas_poly_emp = datas$datas_poly_emp)
  
  # callModule(mod_workflow_tasks_times_server, 
  #            "workflow_tasks_times_ui_1")
  
}