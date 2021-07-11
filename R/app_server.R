#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  #####################
  # Credentials
  ####################
  passwords <- c(Cris = "DanaBrisa%2021", # Author
                 Marcelo = "z8*R@3X1",  # committee
                 Alexandre = "Lw2c5UQ&", # committee
                 Felipe = "I8DP8#g7", # committee
                 Estela = "@Ts0xJ@0", # committee
                 Benicio = "%uH5XA64", # committee
                 Augusto = "uTm93C#C", # supervisor
                 Gabriel = "swb$sS66", # lab member
                 Wellingson = "a*9F13cP", # lab member
                 Camila = "^5T*22Hh", # lab member
                 Lucas = "E5!YY1dR",  # co-author
                 Thiago = "8Qgp!t13", # co-author
                 Veri = "jP!K5FE0",  # design appraiser
                 Guilherme = "3UKy%2ED", # lab member
                 Leticia = "4v#@8fSR", # lab member
                 Getulio = "^i%S03!zJ5Gm",
                 Oscar   ="fzPI6$3%y") # lab member
  
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
             datas_simu=datas$datas_simu)
  
  callModule(mod_simu_filters_server,
             "simu_filters_ui_1",
             datas_simu=datas$datas_simu)
  
  callModule(mod_simu_markers_type_server, 
             "simu_markers_type_ui_1", 
             datas_simu=datas$datas_simu)
  
}