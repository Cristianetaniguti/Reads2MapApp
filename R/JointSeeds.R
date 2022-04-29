########################################################
# Joint simulations with same depth and different seed
#
# Written by Cristiane Taniguti
#
########################################################

# Deprecated

#'Joint simulations with same depth and different seed
#'
#'@param ... tar.gz files outputted from onemap_workflows
#'@param out_name output file name without the extension definition
#'
#'@import largeList
#'@import dplyr
#'@import vroom
#'
#'@examples
#' 
# joint_same_depth("depth10_file1",
#                  "depth10_file2",
#                  out_name = "SimulatedReads_results_depth10_joint")
#'
#'@export
joint_same_depth <- function(..., out_name = "SimulatedReads_results_joint"){
  data.gz <- c(...)

  # Organizing files
  list_files <- list()
  for(i in 1:length(data.gz)){
    list_files[[i]] <- paste0(data.gz[i],"/",list.files(data.gz[i]))
  }
  
  list_files <- lapply(list_files, sort)
  
  datas <- list()
  for(i in 1:length(list_files[[1]])){
    datas[[i]] <- sapply(list_files, "[", i)
  }
  
  datas <- datas[c(1,3,4,6,5,7,8,9,2)] # Parei aqui -- nao tem o simu_haplo, plots e positions
  
  Rdata_lst <- data_lst <- datas_up <- list()
  for(j in 1:length(datas)){
    if(j == 6){
      for(i in 1:length(datas[[j]])){
        temp <- readList(datas[[j]][i])
        if(i == 1){
          saveList(temp, file="sequences.llo", append = F, compress = T)
        } else {
          saveList(temp, file="sequences.llo", append = T, compress = T)
        }
      }
    } else  if(j == 7){
      for(i in 1:length(datas[[j]])){
        temp <- load(datas[[j]][i])
        Rdata_lst[[i]] <- get(temp)
      }
      Rdatas <- do.call(c, Rdata_lst)
      save(Rdatas, file = "gusmap_RDatas.RData")
    } else {
      for(i in 1:length(datas[[j]])){
        data_lst[[i]] <- vroom(datas[[j]][i], delim = "\t")
      }
      if(j == 8){
        dat <- do.call(c, data_lst)
      } else   dat <- do.call(rbind, data_lst)
      datas_up[[j]] <- dat
    }
  }
  
  vroom_write(datas_up[[1]], "data1_depths_geno_prob.tsv.gz")
  vroom_write(datas_up[[2]], "data2_maps.tsv.gz")
  vroom_write(datas_up[[3]], "data3_filters.tsv.gz")
  vroom_write(datas_up[[5]], "data4_times.tsv.gz")
  vroom_write(datas_up[[4]], "data5_SNPCall_efficiency.tsv.gz")
  vroom_write(datas_up[[9]], "simu_haplo.tsv.gz")
  vroom_write(datas_up[[10]], "data10_counts.tsv.gz")
  
  data.names <- as.data.frame(datas_up[[8]])
  print(data.names)
  vroom_write(data.names, "names.tsv.gz")
  
  # system("mkdir SimulatedReads_results_depth~{depth}")
  # system("mv gusmap_RDatas.RData sequences.llo data1_depths_geno_prob.tsv.gz \
  #           data2_maps.tsv.gz data3_filters.tsv.gz data4_times.tsv.gz data5_SNPCall_efficiency.tsv.gz data10_counts.tsv.gz \
  #           simu_haplo.tsv.gz  names.tsv.gz plots positions SimulatedReads_results_depth~{depth}")
  # system("tar -czvf SimulatedReads_results_depth~{depth}.tar.gz SimulatedReads_results_depth~{depth}")
  
}

