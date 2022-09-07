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


# library(tidyverse)
# library(largeList)
# library(vroom)
# 
# datas <- list()
# 
# datas[[1]] <- c("sha02/data1_depths_geno_prob.tsv.gz;sha04/data1_depths_geno_prob.tsv.gz;sh00/data1_depths_geno_prob.tsv.gz;sh01/data1_depths_geno_prob.tsv.gz")
# datas[[2]] <- c("sha02/data2_maps.tsv.gz;sha04/data2_maps.tsv.gz;sh00/data2_maps.tsv.gz;sh01/data2_maps.tsv.gz")
# datas[[3]] <- c("sha02/data3_filters.tsv.gz;sha04/data3_filters.tsv.gz;sh00/data3_filters.tsv.gz;sh01/data3_filters.tsv.gz")
# datas[[4]] <- c("sha02/data4_times.tsv.gz;sha04/data4_times.tsv.gz;sh00/data4_times.tsv.gz;sh01/data4_times.tsv.gz")
# datas[[5]] <- c("sha02/data5_SNPCall_efficiency.tsv.gz;sha04/data5_SNPCall_efficiency.tsv.gz;sh00/data5_SNPCall_efficiency.tsv.gz;sh01/data5_SNPCall_efficiency.tsv.gz")
# datas[[6]] <- c("sha02/data6_RDatas.llo;sha04/data6_RDatas.llo;sh00/data6_RDatas.llo;sh01/data6_RDatas.llo")
# datas[[7]] <- c("sha02/gusmap_RDatas.RData;sha04/gusmap_RDatas.RData;sh01/gusmap_RDatas.RData;sh01/gusmap_RDatas.RData")
# datas[[8]] <- c("sha02/names.tsv.gz;sha04/names.tsv.gz;sh00/names.tsv.gz;sh01/names.tsv.gz")
# datas[[9]] <- c("sha02/101_20_haplo_simu.tsv.gz;sha04/51_20_haplo_simu.tsv.gz;sh00/42_20_haplo_simu.tsv.gz;sh01/30_20_haplo_simu.tsv.gz")
# datas[[10]] <- c("sha02/data10_CountVariants.tsv.gz;sha04/data10_CountVariants.tsv.gz;sh00/data10_CountVariants.tsv.gz;sh01/data10_CountVariants.tsv.gz")
# 
# datas <- lapply(datas, function(x) unlist(strsplit(x, ";")))
# 
# Rdata_lst <- data_lst <- datas_up <- list()
# for(j in 1:length(datas)){
#   if(j == 6){
#     for(i in 1:length(datas[[j]])){
#       temp <- readList(datas[[j]][i])
#       if(i == 1){
#         saveList(temp, file="sequences.llo", append = F, compress = T)
#       } else {
#         saveList(temp, file="sequences.llo", append = T, compress = T)
#       }
#     }
#   } else  if(j == 7){
#     for(i in 1:length(datas[[j]])){
#       temp <- load(datas[[j]][i])
#       Rdata_lst[[i]] <- get(temp)
#     }
#     Rdatas <- do.call(c, Rdata_lst)
#     save(Rdatas, file = "gusmap_RDatas.RData")
#   } else {
#     for(i in 1:length(datas[[j]])){
#       data_lst[[i]] <- vroom(datas[[j]][i], delim = "\t")
#     }
#     if(j == 8){
#       dat <- do.call(c, data_lst)
#     } else   dat <- do.call(rbind, data_lst)
#     datas_up[[j]] <- dat
#   }
# }
# 
# vroom_write(datas_up[[1]], "data1_depths_geno_prob.tsv.gz")
# vroom_write(datas_up[[2]], "data2_maps.tsv.gz")
# vroom_write(datas_up[[3]], "data3_filters.tsv.gz")
# vroom_write(datas_up[[5]], "data4_times.tsv.gz")
# vroom_write(datas_up[[4]], "data5_SNPCall_efficiency.tsv.gz")
# vroom_write(datas_up[[9]], "simu_haplo.tsv.gz")
# vroom_write(datas_up[[10]], "data10_counts.tsv.gz")
# 
# data.names <- as.data.frame(datas_up[[8]])
# vroom_write(data.names, "names.tsv.gz")
# 
# system("mkdir SimulatedReads_results_depth20")
# system("mv gusmap_RDatas.RData sequences.llo data1_depths_geno_prob.tsv.gz \
#             data2_maps.tsv.gz data3_filters.tsv.gz data4_times.tsv.gz data5_SNPCall_efficiency.tsv.gz data10_counts.tsv.gz \
#             simu_haplo.tsv.gz  names.tsv.gz  sha04/20_51_positions.tar.gz sha02/20_101_positions.tar.gz sh00/20_42_positions.tar.gz sh01/20_30_positions.tar.gz SimulatedReads_results_depth20")
# system("tar -czvf SimulatedReads_results_depth20.tar.gz SimulatedReads_results_depth20")
