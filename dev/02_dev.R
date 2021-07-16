# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "shiny" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinymanager" )
usethis::use_package( "dplyr" )
usethis::use_package( "tidyr" )
usethis::use_package( "onemap" )
usethis::use_package( "GUSMap" )
usethis::use_package( "largeList" )
usethis::use_package( "ggpubr" )
usethis::use_package( "plotly" )
usethis::use_package( "ggplot2" )
usethis::use_package( "pkgload" )
usethis::use_package( "vroom" )
usethis::use_package( "RColorBrewer" )
usethis::use_package( "htmlwidgets" )
usethis::use_package( "pROC" )
usethis::use_package( "DT" )
usethis::use_package( "attempt" )
usethis::use_package( "glue" )
usethis::use_package( "golem" )
usethis::use_package( "htmltools" )

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "upload" )

golem::add_module( name = "simu_SNPCalling_efficiency" )
golem::add_module( name = "simu_filters" )
golem::add_module( name = "simu_markers_type" )
golem::add_module( name = "simu_times" )
golem::add_module( name = "simu_depths_and_genotyping" )
golem::add_module( name = "simu_genotype_probabilities" )
golem::add_module( name = "simu_roc_curves" )
golem::add_module( name = "simu_map_size_each_family" )
golem::add_module( name = "simu_overview_map_size" )
golem::add_module( name = "simu_phases" )
golem::add_module( name = "simu_maps" )
golem::add_module( name = "simu_progeny_haplotypes" )
golem::add_module( name = "simu_breakpoints_counts" )
golem::add_module( name = "simu_cM_Mb" )

golem::add_module( name = "emp_filters" )
golem::add_module( name = "emp_markers_type" )
golem::add_module( name = "emp_times" )
golem::add_module( name = "emp_depth_and_genotyping" )
golem::add_module( name = "emp_ind_size" )
golem::add_module( name = "emp_plotly_heatmaps" )
golem::add_module( name = "emp_maps" )
golem::add_module( name = "emp_progeny_haplotypes" )
golem::add_module( name = "emp_breakpoints_count" )
golem::add_module( name = "emp_cM_Mb" )

golem::add_module( name = "workflow_tasks_times" )



## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("Reads2MapApp")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")