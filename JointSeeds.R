# Here we joint simulations with same depth and different seed

# Packages and Functions 

library(tidyverse)
library(largeList)
source("/home/cristiane/github/errors_workflow/.dockerfiles/onemap/functions_simu.R")


# From reads_simu

datas <- list()
datas[[1]] <- c(paste0("part", 1:3, "/data1_depths_geno_prob.rds"))
datas[[2]] <- c(paste0("part", 1:3, "/data2_maps.rds"))
datas[[3]] <- c(paste0("part", 1:3, "/data3_filters.rds"))
datas[[4]] <- c(paste0("part", 1:3, "/data4_times.rds"))
datas[[5]] <- c(paste0("part", 1:3, "/data5_SNPcall_efficiency.rds"))
datas[[6]] <- c(paste0("part", 1:3, "/data6_RDatas.llo"))
datas[[7]] <- c(paste0("part", 1:3, "/gusmap_RDatas.RData"))
datas[[8]] <- c(paste0("part", 1:3, "/names.rds"))
datas[[9]] <- c("part1/29_5_haplo_simu.rds",
                       "part2/87_5_haplo_simu.rds",
                       "part3/64_5_haplo_simu.rds")


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
      data_lst[[i]] <- readRDS(datas[[j]][i])
    }
    if(j == 8){
      dat <- do.call(c, data_lst)
    } else   dat <- do.call(rbind, data_lst)
    datas_up[[j]] <- dat
  }
}

result_list <- adapt2app(data = datas_up)

saveRDS(result_list[[1]], file="data1.rds")
saveRDS(result_list[[2]], file="data2.rds")
saveRDS(result_list[[3]], file="data3.rds")
saveRDS(result_list[[4]], file="data4.rds")
saveRDS(result_list[[5]], file="data5.rds")
saveRDS(datas_up[[9]], file="simu_haplo.rds")

choices <- result_list[[6]]
save(choices, file = "choices.RData")
saveRDS(datas_up[[8]], file = "names.rds")

system("mkdir SimulatedReads_results_depth5_multi")
system("mv gusmap_RDatas.RData sequences.llo data1.rds data2.rds data3.rds data4.rds data5.rds simu_haplo.rds choices.RData names.rds SimulatedReads_results_depth5_multi")
system("tar -czvf SimulatedReads_results_depth5_multi3rep.tar.gz SimulatedReads_results_depth5_multi")


# from SimulationReads

path <- "SimulatedReads_results_depth5_multi"
path1 <- "part4"
path_joint <- "SimulatedReads_results_depth5_multi4rep"

system(paste0("mkdir ", path_joint))

load(paste0(path,"/choices.RData"))
choices1 <- choices
load(paste0(path1,"/choices.RData"))

choices[[2]] <- c(choices1[[2]], choices[[2]])
choices[[3]] <- c(choices1[[3]], choices[[3]])
choices[[3]][length(choices[[3]])] <- length(choices[[3]])

save(choices, file=paste0(path_joint,"/choices.RData"))

for(i in 1:5){
    temp <- readRDS(paste0(path,"/data",i,".rds"))
    temp1 <- readRDS(paste0(path1,"/data",i,".rds"))

    data1 <- rbind(temp, temp1)
    saveRDS(data1, file=paste0(paste0(path_joint,"/data",i,".rds")))
}


temp <- readRDS(paste0(path,"/names.rds"))
temp1 <- readRDS(paste0(path1,"/names.rds"))

data1 <- c(temp, temp1)
saveRDS(data1, file=paste0(paste0(path_joint,"/names.rds")))


system(paste0("cp ",path, "/sequences.llo ",path_joint))
temp <- readList(file=paste0(path1,"/sequences.llo"))
saveList(temp, file= paste0(path_joint,"/sequences.llo"), append = T, compress = T)

temp <- readRDS(paste0(path,"/simu_haplo.rds"))
temp1 <- readRDS(paste0(path1,"/simu_haplo.rds"))

data1 <- rbind(temp, temp1)
saveRDS(data1, file=paste0(path_joint,"/simu_haplo.rds"))

load(paste0(path,"/gusmap_RDatas.RData"))
temp <- Rdatas
load(paste0(path1,"/gusmap_RDatas.RData"))
Rdatas <- c(temp,Rdatas)
save(Rdatas, file=paste0(path_joint,"/gusmap_RDatas.RData"))

system(paste0("tar -czvf ", path_joint,".tar.gz ", path_joint))
