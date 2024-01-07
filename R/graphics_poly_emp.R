#' Read empirical input polyploid data
#' 
#' @param x inputted dataset
#' @param example_emp selected example id
#' 
#' @importFrom utils read.table untar
#' @import vroom
prepare_poly_datas_emp <- function(x = NULL, example_emp = NULL){
  # This function makes adjustments in the input tar.gz file to be processed inside the app
  # It returns six data objects and the app options in a list format
  if(!is.null(x)){
    data.gz <- x[,4]
    path = "data/"
  } else if(is.null(example_emp)){
    cat("Wait credentials\n")
    data.gz <- "Wait"
  } else if(example_emp== "none" ){
    cat("Wait credentials\n")
    data.gz <- "Wait"
  } else { ######## Available examples
    if(example_emp == "toy_sample_poly"){
      data.gz <- system.file("ext", "toy_sample_emp/polyploid/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
    }
  }
  
  if(data.gz == "Wait"){
    cat("Waiting...\n")
  } else {
    path_dir <- tempdir()
    list_files <- list()
    for(i in 1:length(data.gz)){
      untar(data.gz[i], exdir = path_dir)
      list_files[[i]] <- untar(data.gz[i], list = T)
    }
    
    list_files <- lapply(list_files, function(x) {
      y <- file.path(path_dir, x,sep="")
      y <- substring(y,1, nchar(y))
      return(y)
    })
    list_files <- lapply(list_files, "[", -1)
    
    # Data
    datas <- list()
    for(i in 1:length(list_files[[1]])){
      datas[[i]] <- sapply(list_files, "[", i)
    }
    
    software <- "mappoly"
    datas <- unlist(datas)
    list_items <- c("dat", "mat2", "maps", "summaries", "info")
    result_list <- list()
    for(j in 1:length(list_items)){
      files <- datas[grep(list_items[j], datas)]
      if(length(files) > 0){
        temp_item <- list()
        for(i in 1:length(files)){
          temp_item[[i]] <- readRDS(files[i])
        }
      } else temp_item <- NULL
      names(temp_item) <- sapply(strsplit(basename(files), "_"), function(x) paste0(x[1:3], collapse = "_"))
      result_list[[j]] <- temp_item
    }
    names(result_list) <- list_items
    result_list$software <- software
    result_list1 <- result_list
    
    for(i in 1:5){
     idx <- which(sapply(result_list[[i]], is.list))
     if(length(result_list[[i]]) > length(idx)) result_list[[i]][-idx] <- NULL
    }
    
    return(result_list)
  }
}
