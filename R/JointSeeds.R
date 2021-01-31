########################################################
# Joint simulations with same depth and different seed
#
# Written by Cristiane Taniguti
#
########################################################

#'Joint simulations with same depth and different seed
#'
#'@param ... tar.gz files outputted from onemap_workflows
#'@param out_name output file name without the extension definition
#'
#'@import largeList
#'@import dplyr
#'
#'@examples
#' 
# joint_same_depth("depth10_file1/SimulatedReads_results_depth10.tar.gz",
#                  "depth10_file2/SimulatedReads_results_depth10.tar.gz",
#                  out_name = "SimulatedReads_results_depth10_joint")
#'
#'@export
joint_same_depth <- function(..., out_name = "SimulatedReads_results_joint"){
  data.gz <- c(...)
  
  dir.create(out_name)
  
  # Organizing files
  list_files <- list()
  path_dir <- vector()
  for(i in 1:length(data.gz)){
    path_dir <- c(path_dir,tempfile())
    dir.create(path_dir[i])
    untar(data.gz[i], exdir = path_dir[i])
    list_files[[i]] <- untar(data.gz[i], list = T)
    list_files[[i]] <- paste0(path_dir[i], "/", list_files[[i]][-1])
  }
  
  list_files <- lapply(list_files, sort)
  
  datas <- list()
  for(i in 1:length(list_files[[1]])){
    datas[[i]] <- sapply(list_files, "[", i)
  }
  
  # Firsts
  rdatas <- list()
  mk <- 1
  for(j in c(1,7,8)){
    rdatas[[mk]] <- base::get(load(datas[[j]][1]))
    mk <- mk + 1
  }
  
  mk <- 1
  datas_rds <- list()
  for(j in c(2:6, 9, 11)){
    datas_rds[[mk]] <- readRDS(datas[[j]][1])
    mk <- mk + 1
  }
  
  largelist_temp <- readList(datas[[10]][1])
  saveList(largelist_temp, file= paste0(out_name,"/sequences.llo"))
  
  
  if(length(unique(lengths(datas))) > 1) stop("Something is wrong.\n")
  
  for(i in 2:unique(lengths(datas))){
    # choices
    temp <- base::get(load(datas[[1]][i]))
    rdatas[[1]][[2]] <- c(rdatas[[1]][[2]], temp[[2]])
    temp[[3]][1] <- i
    rdatas[[1]][[3]] <- c(rdatas[[1]][[3]], temp[[3]])
    
    # gusmaps
    temp <- base::get(load(datas[[7]][i]))
    rdatas[[2]] <- c(rdatas[[2]], temp)
    
    # multi_names
    temp <- base::get(load(datas[[8]][i]))
    rdatas[[3]] <- c(rdatas[[3]], temp)
    
    # datas rds
    mk <- 1
    for(j in c(2:6, 9, 11)){
      datas_rds[[mk]] <- rbind(datas_rds[[mk]], readRDS(datas[[j]][i]))
      mk <- mk +1
    }
    
    # large list
    temp <- readList(file= datas[[10]][i])
    saveList(temp, file= paste0(out_name,"/sequences.llo"), append = T, compress = T)
  }
  
  # save
  names_file <- c("choices.RData", "gusmap_RDatas.RData", "multi_names.RData")
  for(mk in 1:3){
    temp <- rdatas[[mk]]
    save(temp, file = paste0(out_name, "/",names_file[mk]))
  }
  
  names_files <- c(paste0("data",1:5, ".rds"), "names.rds","simu_haplo.rds")
  for(mk in 1:7){
    saveRDS(datas_rds[[mk]], file = paste0(out_name,"/",names_files[mk]))
  }
  
  system(paste0("tar -czvf ", out_name,".tar.gz ", out_name))
}

