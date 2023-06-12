# Packages
library(vroom)
library(tidyverse)
library(vcfR)
library(tidymodels)
library(ggpubr)
library(ggbreak) 

## For depth
data.gz <- c(system.file("ext", "simu_results/depth10/biallelics/SimulatedReads_results_depth10_seed8085.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics/SimulatedReads_results_depth20_seed8082.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics/SimulatedReads_results_depth10_seed8084.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_dev/SimulatedReads_results_depth20_seed8080.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_dev/SimulatedReads_results_depth10_seed8081.tar.gz", package = "Reads2MapApp"))

list_files <- list()
for(i in 1:length(data.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files[[i]] <- untar(data.gz[i], list = T)
  list_files[[i]] <- file.path(path_dir, list_files[[i]])
}

for_rm <- sapply(list_files, "[", 1)
list_files <- lapply(list_files, "[", -1)
list_files <- lapply(list_files, sort)

# Data
datas <- list()
for(i in 1:length(list_files[[1]])){
  datas[[i]] <- sapply(list_files, "[", i)
}

## Tables
data1_depths_geno_prob  <- vector()

for(i in 1:length(datas)){
  for(j in 1:length(datas[[i]])){
    if(all(grepl("data1_depths_geno_prob", datas[[i]]))){
      temp1 <-  vroom(datas[[i]][[j]], delim = "\t")
      if(length(which(temp1$mk.type %in% c("A.1", "A.2", "D1.9", "D2.14"))) > 0)
        temp1 <- temp1[-which(temp1$mk.type %in% c("A.1", "A.2", "D1.9", "D2.14")),]
      if(grepl("dev",data.gz[j])) temp1_segr <- "dev" else temp1_segr <- "nondev" 
      if(grepl("biallelics",data.gz[j])) temp1_bi <- "bi" else temp1_bi <- "multi" 
      temp1 <- cbind(segr = temp1_segr, bi = temp1_bi, temp1)
      name_temp <- unlist(strsplit(datas[[i]][[j]], "/"))
      name_temp <- unlist(strsplit(name_temp[length(name_temp)], "[.]"))[1]
      assign(name_temp, rbind(base::get(name_temp), temp1))
    }
  }
}

data1_depths_geno_prob$for_depth <- paste0(data1_depths_geno_prob$seed, "_", 
                                           data1_depths_geno_prob$depth, "_",
                                           data1_depths_geno_prob$bi, "_",
                                           data1_depths_geno_prob$segr, "_",
                                           "GQ_",
                                           data1_depths_geno_prob$CountsFrom, "_",
                                           data1_depths_geno_prob$GenoCall, "_",
                                           data1_depths_geno_prob$SNPCall, "_",
                                           data1_depths_geno_prob$mks)

data1_depths_geno_prob <- data.frame(data1_depths_geno_prob)
data1_depths_geno_prob <- perfumaria(data1_depths_geno_prob)

colors <- c("gray", "#009E73", "#0072B2", "#D55E00", "#F0E442", "#E69F00")
names(colors) <-  c("missing", 
                    "True=homozygous | Est=homozygous", "True=heterozygous | Est=heterozygous", 
                    "True=homozygous | Est=heterozygous", "True=heterozygous | Est=homozygous",
                    "True=homozygous-alt/ref | Est=homozygous-ref/alt")

data1_depths_geno_prob$pop <- "progeny"
data1_depths_geno_prob$pop[data1_depths_geno_prob$ind %in% c("P1", "P2")] <- "parents"

data1_depths_geno_prob$alt <- as.numeric(data1_depths_geno_prob$alt)
data1_depths_geno_prob$gt.onemap.alt.ref <- as.factor(data1_depths_geno_prob$gt.onemap.alt.ref)

# True=homozygous | Est=heterozygous
# True=homozygous | Est=homozygous
# True=heterozygous | Est=homozygous
# True=heterozygous | Est=heterozygous

data1_depths_geno_prob$graph.color <- NA
data1_depths_geno_prob$graph.color[which(data1_depths_geno_prob$gabGT == data1_depths_geno_prob$gt.onemap.alt.ref & grepl("homozygous", data1_depths_geno_prob$gabGT))] <- "True=homozygous | Est=homozygous"
data1_depths_geno_prob$graph.color[which(grepl("heterozygous", data1_depths_geno_prob$gt.onemap.alt.ref) & grepl("homozygous", data1_depths_geno_prob$gabGT))] <- "True=homozygous | Est=heterozygous"
data1_depths_geno_prob$graph.color[which(data1_depths_geno_prob$gabGT == data1_depths_geno_prob$gt.onemap.alt.ref & grepl("heterozygous", data1_depths_geno_prob$gabGT))] <- "True=heterozygous | Est=heterozygous"
data1_depths_geno_prob$graph.color[which(grepl("homozygous", data1_depths_geno_prob$gt.onemap.alt.ref) & grepl("heterozygous", data1_depths_geno_prob$gabGT))] <- "True=heterozygous | Est=homozygous"
data1_depths_geno_prob$graph.color[which((data1_depths_geno_prob$gabGT == "homozygous-ref" & data1_depths_geno_prob$gt.onemap.alt.ref == "homozygous-alt") | data1_depths_geno_prob$gabGT == "homozygous-alt" & data1_depths_geno_prob$gt.onemap.alt.ref == "homozygous-ref")] <- "True=homozygous-alt/ref | Est=homozygous-ref/alt"
data1_depths_geno_prob$graph.color[which(data1_depths_geno_prob$gt.onemap.alt.ref == "missing")] <- "missing"

################
# Dataset list
data.gz <- c(system.file("ext", "simu_results/depth10/biallelics/SimulatedReads_results_depth10_seed8085.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_filt_GQ/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_filt_GQ/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_filt_GQ_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_filt_GQ_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8296.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8297.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20_seed8295.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10_seed8294.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics/SimulatedReads_results_depth20_seed8082.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics/SimulatedReads_results_depth10_seed8084.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_dev/SimulatedReads_results_depth20_seed8080.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_dev/SimulatedReads_results_depth10_seed8081.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_filt_GQ/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8299.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8300.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20_seed8303.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo_dev_replaced/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev_replaced/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))

data.gz[31] <- "/home/cris/R/x86_64-pc-linux-gnu-library/4.1/Reads2MapApp/ext/simu_results/depth20/multiallelics_filt_GQ_noninfo_dev_replaced/SimulatedReads_results_depth20.tar.gz"

# Big dataset
df_total.polyrad <- df_total.updog <- df_total.supermassa <- df_total.SNPCaller <- data.frame()
for(i in 1:length(data.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- file.path(path_dir, list_files)
  path.file <- list_files[grep("data2_maps", list_files)]
  df <- vroom(path.file, num_threads = 15)
  df <- df %>% filter(fake == "without-false") # Only dataset without false-positive
  df.polyrad <- df %>% filter(GenoCall == "polyrad")
  df.updog <- df %>% filter(GenoCall == "updog")
  df.supermassa <- df %>% filter(GenoCall == "supermassa")
  df.SNPCaller <- df %>% filter(GenoCall == "SNPCaller")
  if(grepl("multiallelics", data.gz[i])) dataset <- "multi" else dataset <- "bi"
  if(grepl("_dev", data.gz[i])) dataset2 <- "dev" else dataset2 <- "nondev"
  if(grepl("replaced", data.gz[i])) dataset3 <- "filt_GQ_noninfo_replaced" else 
    if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else 
      if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else 
        dataset3 <- "GQ"     
  df.polyrad <- cbind(data=dataset, segr = dataset2, filt = dataset3, df.polyrad)
  df.updog <- cbind(data=dataset, segr = dataset2, filt = dataset3, df.updog)
  df.supermassa <- cbind(data=dataset, segr = dataset2, filt = dataset3, df.supermassa)
  df.SNPCaller <- cbind(data=dataset, segr = dataset2, filt = dataset3, df.SNPCaller)
  df_total.polyrad <- rbind(df_total.polyrad, df.polyrad)
  df_total.updog <- rbind(df_total.updog, df.updog)
  df_total.supermassa <- rbind(df_total.supermassa, df.supermassa)
  df_total.SNPCaller <- rbind(df_total.SNPCaller, df.SNPCaller)
}

vroom_write(df_total.polyrad, file = "data2_polyrad.tsv.gz", num_threads = 5)
vroom_write(df_total.updog, file = "data2_updog.tsv.gz", num_threads = 5)
vroom_write(df_total.supermassa, file = "data2_supermassa.tsv.gz", num_threads = 5)
vroom_write(df_total.SNPCaller, file = "data2_SNPCaller.tsv.gz", num_threads = 5)

software_lab <- c("polyRAD", "updog", "SuperMASSA", "freebayes/GATK")
software <- c("polyrad", "updog", "supermassa", "SNPCaller")

df_wrong_depth <- p_wrong <- bi_total <- multi_total  <- list()
for(i in 1:length(software)){
  df_total <- vroom(paste0("data2_",software[i],".tsv.gz"))
  df_total$lab <- NA
  df_total$wrong <- NA
  df_total$lab[which(df_total$type == "B3.7" &
                       df_total$real.type == "B3.7")] <- "True=B3.7\nEst=B3.7"
  df_total$lab[which(df_total$type == "B3.7" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=B3.7"
  df_total$lab[which(df_total$type == "B3.7" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=B3.7"
  df_total$lab[which(df_total$type == "D1.10" &
                       df_total$real.type == "B3.7")] <- "True=B3.7\nEst=D1.10"
  df_total$lab[which(df_total$type == "D1.10" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=D1.10"
  df_total$lab[which(df_total$type == "D1.10" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=D1.10"
  df_total$lab[which(df_total$type == "D2.15" &
                       df_total$real.type == "B3.7")] <- "True=B3.7\nEst=D2.15"
  df_total$lab[which(df_total$type == "D2.15" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=D2.15"
  df_total$lab[which(df_total$type == "D2.15" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=D2.15"
  df_total$lab[which(df_total$type == "B3.7" &
                       df_total$real.type == "non-informative")] <- "True=non-informative\nEst=B3.7"
  df_total$lab[which(df_total$type == "D1.10" &
                       df_total$real.type == "non-informative")] <- "True=non-informative\nEst=D1.10"
  df_total$lab[which(df_total$type == "D2.15" &
                       df_total$real.type == "non-informative")] <- "True=non-informative\nEst=D2.15"
  
  df_total$lab[which(df_total$type == "A.1" &
                       df_total$real.type == "B3.7")] <- "True=B3.7\nEst=A.1"
  df_total$lab[which(df_total$type == "A.1" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=A.1"
  df_total$lab[which(df_total$type == "A.1" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=A.1"
  
  df_total$lab[which(df_total$type == "A.2" &
                       df_total$real.type == "B3.7")] <- "True=B3.7\nEst=A.2"
  df_total$lab[which(df_total$type == "D1.9" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=D1.9"
  df_total$lab[which(df_total$type == "D2.14" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=D2.14"
  df_total$lab[which(df_total$type == "A.1" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=A.1"
  df_total$lab[which(df_total$type == "A.2" &
                       df_total$real.type == "D1.10")] <- "True=D1.10\nEst=A.2"
  df_total$lab[which(df_total$type == "A.1" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=A.1"
  df_total$lab[which(df_total$type == "A.2" &
                       df_total$real.type == "D2.15")] <- "True=D2.15\nEst=A.2"
  df_total$lab[which(df_total$type == "D1.9" &
                       df_total$real.type == "non-informative")] <- "True=non-informative\nEst=B3.7"
  df_total$lab[which(df_total$type == "D2.14" &
                       df_total$real.type == "non-informative")] <- "True=non-informative\nEst=D1.10"
  df_total$lab[which(df_total$type == "A.1" &
                       df_total$real.type == "non-informative")] <- "True=non-informative\nEst=A.1"
  
  # Save wrong markers to get depth
  df_total$for_depth <- paste0(df_total$seed, "_", 
                               df_total$depth, "_",
                               df_total$data, "_",
                               df_total$segr, "_",
                               df_total$filt, "_",
                               df_total$CountsFrom, "_",
                               df_total$GenoCall, "_",
                               df_total$SNPCall, "_",
                               df_total$mk.name)
  # data1 <- data1_depths_geno_prob
  # data1$lab <- df_total$lab[match(data1$for_depth, df_total$for_depth)]
  # if(length(which(is.na(data1$lab))) > 0)
  #   data1 <- data1[-which(is.na(data1$lab)),]
  # 
  # data1$wrong <- NA
  # data1$wrong[which(data1$lab %in% c("True=B3.7\nEst=B3.7","True=D1.10\nEst=D1.10" , "True=D2.15\nEst=D2.15"))] <- "correct"
  # data1$wrong[which(!(data1$lab %in% c("True=B3.7\nEst=B3.7","True=D1.10\nEst=D1.10" , "True=D2.15\nEst=D2.15")))] <- "wrong"
  # 
  # alpha <- 1
  # 
  # sub_data <- list()
  # temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 10" & CountsFrom == "BAM" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[1]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 10" & CountsFrom == "VCF" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[2]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 20" & CountsFrom == "BAM" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[3]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 20" & CountsFrom == "VCF" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[4]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 10" & CountsFrom == "BAM" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[5]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 10" & CountsFrom == "VCF" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[6]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 20" & CountsFrom == "BAM" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[7]] <- temp 
  # temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 20" & CountsFrom == "VCF" & wrong == "wrong")
  # if(dim(temp)[1] > 0) sub_data[[8]] <- temp 
  # 
  # sub_data <- sub_data[-which(sapply(sub_data, is.null))]
  # 
  # p <- list()
  # for(j in 1:length(sub_data)){
  #   if(!is.null(sub_data[[j]])){
  #     p[[j]] <- sub_data[[j]] %>% ggplot(aes(x=ref, y=alt, color=graph.color)) + 
  #       geom_point(aes(shape = pop, size = pop, alpha = pop)) +
  #       scale_size_manual(values = c(2.5,1)) + 
  #       scale_color_manual(values = colors) +
  #       scale_shape_manual(values=c(3, 19)) +
  #       scale_alpha_manual(values=c(1, 0.5)) +
  #       labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  #       facet_grid(lab ~ paste(SNPCall,"\n", CountsFrom, "\n", depth), scales = "free") +
  #       guides(colour = guide_legend(override.aes = list(alpha = 1), nrow=3), 
  #              shape = guide_legend(override.aes = list(alpha = 1), ncol = 1),
  #              size ="none", alpha ="none")  + theme_bw() + 
  #       theme(legend.position = "right", legend.title = element_blank(), 
  #             axis.text.y =  element_text(size=6),
  #             axis.title = element_text(size=8),
  #             legend.text = element_text(size=6),
  #             strip.text = element_text(size = 6),
  #             axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
  #             title = element_text(size = 8)) 
  #   }
  # }
  # 
  # p_joint <- ggarrange(plotlist = p, ncol = length(p), common.legend = T, legend = "bottom") 
  # ggsave(p_joint, filename = paste0("wrong_by_depth_",gsub("/", "_", software_lab[i]), ".png"), width = 170, height = 225, units = "mm")
  
  #####
  
  df_total$dataset <- paste0(df_total$CountsFrom, "_", df_total$segr)
  
  change.name <- c(GQ = "no extra filters",
                   filt_GQ = "genotype prob > 0.8",
                   filt_GQ_noninfo = "+ only informative markers",
                   filt_GQ_noninfo_replaced = "+ missing replaced")
  
  df_total$filt <- change.name[match(df_total$filt, names(change.name))]
  
  df_total$filt <- factor(df_total$filt, levels =  c("no extra filters", 
                                                     "genotype prob > 0.8", 
                                                     "+ only informative markers",
                                                     "+ missing replaced"))
  
  multi <- df_total[which(df_total$type %in% c("A.1", "A.2", "D1.9", "D2.14")),]
  bi <- df_total[-which(df_total$type %in% c("A.1", "A.2", "D1.9", "D2.14")),]
  
  bi <- bi %>% group_by(dataset,seed, depth, SNPCall, GenoCall, lab, filt) %>%  summarise(cnt = n()) %>%
    group_by(dataset, seed, depth, SNPCall, GenoCall, filt) %>% mutate(perc = round((cnt / sum(cnt))*100, 2)) %>%
    group_by(dataset, depth, SNPCall, GenoCall, lab, filt) %>% summarise(mean_count = mean(cnt),
                                                                         sd_count = sd(cnt),
                                                                         mean_perc = mean(perc), sd_perc = sd(perc))
  
  multi <- multi %>% group_by(dataset, seed, depth, SNPCall, GenoCall, lab, filt) %>%  summarise(cnt = n()) %>%
    group_by(dataset, seed, depth, SNPCall, GenoCall, filt) %>% mutate(perc = round((cnt / sum(cnt))*100, 2)) %>%
    group_by(dataset, depth, SNPCall, GenoCall, lab, filt) %>% summarise(mean_count = mean(cnt),
                                                                         sd_count = sd(cnt),
                                                                         mean_perc = mean(perc), 
                                                                         sd_perc = sd(perc))
  
  bi$wrong <- NA
  bi$wrong[which(bi$lab %in% c("True=B3.7\nEst=B3.7","True=D1.10\nEst=D1.10" , "True=D2.15\nEst=D2.15"))] <- "correct"
  bi$wrong[which(!(bi$lab %in% c("True=B3.7\nEst=B3.7","True=D1.10\nEst=D1.10" , "True=D2.15\nEst=D2.15")))] <- "wrong"
  
  #bi$lab  <- gsub("\n", "|", bi$lab)
  #multi$lab <- gsub("\n", "|", multi$lab)
  
  legend_text <- c(vcf_nondev = "Without segregation distortion/ Counts from VCF",
                   bam_nondev = "Without segregation distortion/ Counts from BAM",
                   vcf_dev = "With segregation distortion/ Counts from VCF",
                   bam_dev = "With segregation distortion/ Counts from BAM")
  
  # Color blind palette
  colors.p <- c("#009E73", "#0072B2", "#D55E00", "#CC79A7")
  names(colors.p) <- legend_text
  
  bi$dataset <- legend_text[match(bi$dataset, names(legend_text))]
  multi$dataset <- legend_text[match(multi$dataset, names(legend_text))]
  
  bi <- perfumaria(bi)
  multi <- perfumaria(multi)
  
  bi_total <- rbind(bi_total, bi)
  multi_total <- rbind(multi_total, multi)
}

save(bi_total, file = "bi_total.RData")
save(multi_total, file = "multi_total.RData")

temp <- bi_total 

temp$filt2 <- factor(paste0(bi_total$filt, "\n", bi_total$depth), 
                     levels = c("no extra filters\nDepth 10",
                                "genotype prob > 0.8\nDepth 10",
                                "+ only informative markers\nDepth 10",
                                "+ missing replaced\nDepth 10",
                                "no extra filters\nDepth 20",
                                "genotype prob > 0.8\nDepth 20",
                                "+ only informative markers\nDepth 20",
                                "+ missing replaced\nDepth 20"))

temp$filt3 <- factor(paste0(temp$filt, "\n", temp$GenoCall), 
                     levels = unique(paste0(temp$filt, "\n", temp$GenoCall)))

temp$CountsFrom <- temp$dataset
temp$CountsFrom[grep("BAM", temp$dataset)] <- "BAM"
temp$CountsFrom[grep("VCF", temp$dataset)] <- "VCF"

temp$lab2 <- temp$lab

temp$lab2[which(temp$lab2 %in% c("True=B3.7\nEst=D1.10", "True=B3.7\nEst=D2.15"))] <- "True=B\nEst=D"
temp$lab2[which(temp$lab2 %in% c("True=non-informative\nEst=D2.15", "True=non-informative\nEst=D1.10"))] <- "True=non-informative\nEst=D"

# 2D
bi_correct_paper <- temp %>% filter(wrong == "correct" & grepl("With segr", dataset) &
                                      GenoCall != "freebayes/GATK" &
                                      CountsFrom == "VCF") %>% 
  ggplot(aes(x=filt, y = mean_count, group = lab, color = lab, fill = lab)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.8)) +
  scale_color_manual(values = my_colors) +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.8)) +
  facet_grid(depth + GenoCall ~ SNPCall) + 
  theme_bw() +
  ylab("mean # of markers") + xlab("Filters") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1)) + ggtitle("Biallelics - correct")

ggsave(bi_correct_paper, filename = paste0("marker_type_bi_correct_paper.png"), width = 120, height = 200, units = "mm")

bi_correct_suppl <- temp %>% filter(wrong == "correct" & grepl("With segr", dataset) &
                                      GenoCall == "freebayes/GATK" &
                                      CountsFrom == "VCF") %>% 
  ggplot(aes(x=filt, y = mean_count, group = lab, color = lab, fill = lab)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.8)) +
  scale_color_manual(values = my_colors) +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.8)) +
  facet_grid(depth ~ SNPCall) + 
  theme_bw() +
  ylab("mean # of markers") +  xlab("Filters") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1)) + ggtitle("Biallelics - correct")

ggsave(bi_correct_suppl, filename = paste0("marker_type_bi_correct_suppl.png"), width = 120, height = 150, units = "mm")

count_wrong_correct <- temp %>% filter(grepl("With segr", dataset) &
                                         GenoCall != "freebayes/GATK" &
                                         SNPCall == "GATK" &
                                         CountsFrom == "VCF") %>% group_by(depth, SNPCall, GenoCall, filt, CountsFrom, wrong) %>%
  summarize(correct = sum(mean_count))

count_wrong_correct$lab <- paste0(count_wrong_correct$wrong, ": ", round(count_wrong_correct$correct,2))
count_wrong_correct$x <- NA
count_wrong_correct$x[which(count_wrong_correct$filt == "no extra filters")] <- 1
count_wrong_correct$x[which(count_wrong_correct$filt == "genotype prob > 0.8")] <- 2
count_wrong_correct$x[which(count_wrong_correct$filt == "+ only informative markers")] <- 3
count_wrong_correct$x[which(count_wrong_correct$filt == "+ missing replaced")] <- 4

count_wrong_correct$y <- NA
count_wrong_correct$y[which(count_wrong_correct$wrong == "correct")] <- 7
count_wrong_correct$y[which(count_wrong_correct$wrong == "wrong")] <- 6

temp2 <- temp %>% filter(wrong == "wrong" & grepl("With segr", dataset) &
                           GenoCall != "freebayes/GATK" &
                           CountsFrom == "VCF") %>% droplevels() %>%
  group_by(dataset, depth, SNPCall, GenoCall, filt, lab2) %>% summarise(mean_count2 = mean(mean_count),
                                                                        sd_count2 = mean(sd_count)) %>% ungroup() %>% 
  complete(SNPCall, GenoCall, depth, lab2, filt)

temp2$mean_count2[is.na(temp2$mean_count2)] <- 0

bi_wrong1 <- temp2 %>%  
  ggplot(aes(x=filt, y = mean_count2, group = lab2, color = lab2, fill = lab2)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) +
  scale_color_manual(values = my_colors) +
  geom_errorbar(
    aes(ymin = mean_count2-sd_count2, ymax = mean_count2+sd_count2),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.5)) +
  geom_text(data= count_wrong_correct, aes(x,y, label = lab), 
            inherit.aes = FALSE, show.legend = FALSE, size = 2.5) + 
  facet_grid(depth + GenoCall ~ SNPCall) + 
  theme_bw() +
  ylab("mean # of wrong markers") +  xlab("Filters") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 20, hjust=1)) + ggtitle("Biallelics") +
  scale_y_continuous(expand = expansion(mult = c(NULL, 0.1)))

ggsave(bi_wrong1, filename = paste0("marker_type_bi_wrong_paper.png"),  width = 120, height = 200, units = "mm")

count_wrong_correct <- temp %>% filter(grepl("With segr", dataset) &
                                         GenoCall == "freebayes/GATK" &
                                         SNPCall == "GATK" &
                                         CountsFrom == "VCF") %>% group_by(depth, SNPCall, GenoCall, filt, CountsFrom, wrong) %>%
  summarize(correct = sum(mean_count))

count_wrong_correct$lab <- paste0(count_wrong_correct$wrong, ": ", round(count_wrong_correct$correct,2))
count_wrong_correct$x <- NA
count_wrong_correct$x[which(count_wrong_correct$filt == "no extra filters")] <- 1
count_wrong_correct$x[which(count_wrong_correct$filt == "genotype prob > 0.8")] <- 2
count_wrong_correct$x[which(count_wrong_correct$filt == "+ only informative markers")] <- 3
count_wrong_correct$x[which(count_wrong_correct$filt == "+ missing replaced")] <- 4

count_wrong_correct$y <- NA
count_wrong_correct$y[which(count_wrong_correct$wrong == "correct")] <- 5
count_wrong_correct$y[which(count_wrong_correct$wrong == "wrong")] <- 4.5


temp2 <- temp %>% filter(wrong == "wrong" & grepl("With segr", dataset) &
                           GenoCall == "freebayes/GATK" &
                           CountsFrom == "VCF") %>% droplevels() %>%
  group_by(dataset, depth, SNPCall, GenoCall, filt, CountsFrom, lab2) %>% summarise(mean_count2 = mean(mean_count),
                                                                                    sd_count2 = mean(sd_count)) %>% ungroup() %>% 
  complete(SNPCall, CountsFrom, GenoCall, depth, lab2, filt)

temp2$mean_count2[is.na(temp2$mean_count2)] <- 0

bi_wrong2 <- temp2 %>% 
  ggplot(aes(x=filt, y = mean_count2, group = lab2, color = lab2, fill = lab2)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) +
  scale_color_manual(values = my_colors) +
  geom_errorbar(
    aes(ymin = mean_count2-sd_count2, ymax = mean_count2+sd_count2),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.5)) +
  geom_text(data= count_wrong_correct, aes(x,y, label = lab), 
            inherit.aes = FALSE, show.legend = FALSE, size = 2.5) + 
  facet_grid(depth  ~ SNPCall) + 
  theme_bw() +
  ylab("mean # of wrong markers") +  xlab("Filters") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 20, hjust=1)) + ggtitle("Biallelics")  +
  scale_y_continuous(expand = expansion(mult = c(NULL, 0.1)))

ggsave(bi_wrong2, filename = paste0("marker_type_bi_wrong_suppl.png"),  width = 120, height = 170, units = "mm")

temp <- multi_total

temp$CountsFrom <- temp$dataset
temp$CountsFrom[grep("BAM", temp$dataset)] <- "BAM"
temp$CountsFrom[grep("VCF", temp$dataset)] <- "VCF"

temp$lab2 <- temp$lab
temp$lab2[which(temp$lab2 %in% c("True=B3.7\nEst=A.1", "True=B3.7\nEst=A.2"))] <- "True=B\nEst=A"
temp$lab2[which(temp$lab2 %in% c("True=D1.10\nEst=A.1", "True=D1.10\nEst=A.2", "True=D2.15\nEst=A.1", "True=D2.15\nEst=A.2"))] <- "True=D\nEst=A"
unique(temp$lab2)

temp2 <- temp %>% filter(grepl("With segr", dataset) &
                           GenoCall != "freebayes/GATK" &
                           CountsFrom == "VCF") %>% 
  droplevels() %>%
  group_by(dataset, depth, SNPCall, GenoCall, filt, CountsFrom, lab2) %>% 
  summarise(mean_count2 = mean(mean_count),
            sd_count2 = mean(sd_count)) %>% ungroup() %>% 
  complete(SNPCall, CountsFrom, GenoCall, depth, lab2, filt)

temp2$mean_count2[is.na(temp2$mean_count2)] <- 0

multi_plot1 <- temp2 %>% 
  ggplot(aes(x=filt, y = mean_count2, group = lab2, color = lab2, fill = lab2)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) +
  scale_color_manual(values = my_colors) +
  geom_errorbar(
    aes(ymin = mean_count2-sd_count2, ymax = mean_count2+sd_count2),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.5)) +
  facet_grid(depth + GenoCall ~ SNPCall) + 
  theme_bw() +
  ylab("mean # of markers") +  xlab("Filters") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 20, hjust=1)) + ggtitle("Multiallelics")

ggsave(multi_plot1, filename = paste0("marker_type_multi_paper.png"),  width = 130, height = 200, units = "mm")

temp2 <- temp %>% filter(grepl("With segr", dataset) &
                           GenoCall == "freebayes/GATK") %>% 
  droplevels() %>%
  group_by(dataset, depth, SNPCall, GenoCall, filt, CountsFrom, lab2) %>% 
  summarise(mean_count2 = mean(mean_count),
            sd_count2 = mean(sd_count)) %>% ungroup() %>% 
  complete(SNPCall, CountsFrom, GenoCall, depth, lab2, filt)

temp2$mean_count2[is.na(temp2$mean_count2)] <- 0

multi_plot2 <- temp2 %>% 
  ggplot(aes(x=filt, y = mean_count2, group = lab2, color = lab2, fill = lab2)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.5)) +
  scale_color_manual(values = my_colors) +
  geom_errorbar(
    aes(ymin = mean_count2-sd_count2, ymax = mean_count2+sd_count2),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.5)) +
  facet_grid(depth ~ SNPCall) + 
  theme_bw() +
  ylab("mean # of markers") +  xlab("Filters") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(angle = 20, hjust=1)) + ggtitle("Multiallelics")

ggsave(multi_plot2, filename = paste0("marker_type_multi_suppl.png"),  width = 140, height = 170, units = "mm")

### Map size

df_total <- data.frame()
for(i in 1:length(data.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- file.path(path_dir, list_files)
  path.file <- list_files[grep("data2_maps", list_files)]
  df <- vroom(path.file, num_threads = 15)
  df <- df %>% filter(fake == "without-false") # Only dataset without false-positive
  if(grepl("multiallelics", data.gz[i])) dataset <- "multi" else dataset <- "bi"
  if(grepl("_dev", data.gz[i])) dataset2 <- "dev" else dataset2 <- "nondev"
  if(grepl("replaced", data.gz[i])) dataset3 <- "filt_GQ_noninfo_replaced" else 
    if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else 
      if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else 
        dataset3 <- "GQ"
  df_temp <- cbind(data=dataset, segr = dataset2, filt = dataset3, df)
  df_total <- rbind(df_total, df_temp)
}

#df_up <- df_total
df_total <- df_up

change.name <- c(GQ = "no extra filters",
                 filt_GQ = "genotype prob > 0.8",
                 filt_GQ_noninfo = "+ only informative markers",
                 filt_GQ_noninfo_replaced = "+ missing replaced")

df_total$filt <- change.name[match(df_total$filt, names(change.name))]

df_total$filt <- factor(df_total$filt, levels =  c("no extra filters", 
                                                   "genotype prob > 0.8", 
                                                   "+ only informative markers",
                                                   "+ missing replaced"))

df_total <- perfumaria(df_total)

df_total$GenoCall <- factor(df_total$GenoCall, levels =  c("polyRAD",
                                                           "polyRAD (5%)",
                                                           "updog",
                                                           "updog (5%)",
                                                           "SuperMASSA",
                                                           "SuperMASSA (5%)",
                                                           "freebayes/GATK",
                                                           "freebayes/GATK (5%)",
                                                           "freebayes/GATK (0.001%)", "GUSMap"))

df_total$dataset <- paste0(df_total$CountsFrom, "_", df_total$data, "_", df_total$segr)

# d_df <- df_total %>% group_by(dataset, depth, SNPCall, GenoCall, seed, filt) %>% 
#   summarise(D = as.vector((((length(rf)-1)^(-1))*(t(rf-poscM.norm)%*%(rf-poscM.norm)))^{1/2}),
#             n_markers = n()) %>%
#   group_by(dataset, depth, SNPCall, GenoCall, filt) %>% summarise(mean_d = mean(D), 
#                                                                   sd_d = sd(D), 
#                                                                   se_d = sd(D)/sqrt(length(D)),
#                                                                   mean_n = mean(n_markers),
#                                                                   se_n = sd(n_markers)/sqrt(length(n_markers)))

d_df <- df_total %>% group_by(dataset, depth, SNPCall, GenoCall, seed, filt) %>%
  summarise(D = as.vector((((length(rf)-1)^(-1))*(t(rf-poscM.norm)%*%(rf-poscM.norm)))^{1/2}),
            n_markers = n(),
            total_size = rf[length(rf)]) %>%
  group_by(dataset, depth, SNPCall, GenoCall, filt) %>% summarise(d = D,
                                                                  total_size = total_size,
                                                                  mean_n = mean(n_markers),
                                                                  se_n = sd(n_markers)/sqrt(length(n_markers)))

d_df$mean_n[which(duplicated(paste0(d_df$mean_n, d_df$se_n)) & duplicated(paste0(d_df$dataset, d_df$depth, d_df$SNPCall, d_df$GenoCall, d_df$filt)))] <- NA

legend_text <- c(VCF_bi_nondev = "Without segregation distortion\n Only biallelics/ Counts from VCF",
                 BAM_bi_nondev = "Without segregation distortion\n Only biallelics/ Counts from BAM",
                 VCF_multi_nondev = "Without segregation distortion\n With multiallelics/ Counts from VCF",
                 BAM_multi_nondev = "Without segregation distortion\n With multiallelics/ Counts from BAM",
                 VCF_bi_dev = "With segregation distortion\n Only biallelics/ Counts from VCF",
                 BAM_bi_dev = "With segregation distortion\n Only biallelics/ Counts from BAM",
                 VCF_multi_dev = "With segregation distortion\n With multiallelics/ Counts from VCF",
                 BAM_multi_dev = "With segregation distortion\n With multiallelics/ Counts from BAM")

# Color blind palette
colors.p <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(colors.p) <- legend_text

d_df$dataset <- legend_text[match(d_df$dataset, names(legend_text))]

d_plot <- list()
d_plot[[1]] <- d_df %>% filter(GenoCall != "GUSMap" & depth == "Depth 10") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/70, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  # geom_errorbar(
  #   aes(ymin = mean_n-se_n, ymax = mean_n+se_n),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  geom_point(aes(y = log(d, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the Euclidean distance"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*70, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) + theme_bw() +
  geom_hline(yintercept = -Inf, color= "blue") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

d_plot[[2]] <- d_df %>% filter(GenoCall != "GUSMap" & depth == "Depth 20" ) %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/50, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  geom_point(aes(y = log(d, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  # geom_errorbar(
  #   aes(ymin = mean_d-se_d, ymax = mean_d+se_d),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the Euclidean distance"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*50, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = -Inf, color= "blue") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 


ggsave(d_plot[[1]], filename = "log10_d_size1.png", width = 170, height = 225, units = "mm")
ggsave(d_plot[[2]], filename = "log10_d_size2.png", width = 170, height = 225, units = "mm")

d_plot[[1]] <- d_df %>% filter(GenoCall != "GUSMap" & depth == "Depth 10") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/50, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  # geom_errorbar(
  #   aes(ymin = mean_n-se_n, ymax = mean_n+se_n),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  geom_point(aes(y = log(total_size, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*50, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = log(38, base = 10), color= "red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

d_plot[[2]] <- d_df %>% filter(GenoCall != "GUSMap" & depth == "Depth 20" ) %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/50, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  # geom_errorbar(
  #   aes(ymin = mean_d-se_d, ymax = mean_d+se_d),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*50, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = log(38, base = 10), color= "red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

ggsave(d_plot[[1]], filename = "log10_total_size1.png", width = 170, height = 225, units = "mm")
ggsave(d_plot[[2]], filename = "log10_total_size2.png", width = 170, height = 225, units = "mm")

d_plot[[1]] <- d_df %>% filter(filt == "no extra filters" & depth == "Depth 10") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/40, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  # geom_errorbar(
  #   aes(ymin = mean_n-se_n, ymax = mean_n+se_n),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  geom_point(aes(y = log(total_size, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*40, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = log(38, base = 10), color= "red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

d_plot[[2]] <- d_df %>% filter(filt == "no extra filters" & depth == "Depth 20" ) %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/30, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  # geom_errorbar(
  #   aes(ymin = mean_d-se_d, ymax = mean_d+se_d),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*30, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = log(38, base = 10), color= "red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

ggsave(d_plot[[1]], filename = "log10_total_size_gusmap1.png", width = 170, height = 120, units = "mm")
ggsave(d_plot[[2]], filename = "log10_total_size_gusmap2.png", width = 170, height = 120, units = "mm")

d_plot <- list()
d_plot[[1]] <- d_df %>% filter(filt == "no extra filters" & depth == "Depth 10") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/30, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  # geom_errorbar(
  #   aes(ymin = mean_n-se_n, ymax = mean_n+se_n),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  geom_point(aes(y = log(d, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the Euclidean distance"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*30, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = -Inf, color= "blue") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

d_plot[[2]] <- d_df %>% filter(filt == "no extra filters" & depth == "Depth 20" ) %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n/30, fill = dataset), alpha = 0.15, position="dodge", stat="identity") + 
  geom_point(aes(y = log(d, base = 10)), size = 1, position=position_dodge(width=0.9)) +
  # geom_errorbar(
  #   aes(ymin = mean_d-se_d, ymax = mean_d+se_d),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the Euclidean distance"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*30, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) +  theme_bw() +
  geom_hline(yintercept = -Inf, color= "blue") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

ggsave(d_plot[[1]], filename = "log10_d_size_gusmap1.png", width = 170, height = 120, units = "mm")
ggsave(d_plot[[2]], filename = "log10_d_size_gusmap2.png", width = 170, height = 120, units = "mm")


### Subsets
df_sub <- df_total %>% group_by(dataset, depth, SNPCall, GenoCall, seed, filt) %>%
  summarise(D = as.vector((((length(rf)-1)^(-1))*(t(rf-poscM.norm)%*%(rf-poscM.norm)))^{1/2}),
            n_markers = n(),
            n_multi_markers = length(which(type %in% c("A.2", "A.1", "D2.14", "D1.9"))),
            total_size = rf[length(rf)])

legend_text <- c(VCF_bi_nondev = "Without segregation distortion\n Only biallelics/ Counts from VCF",
                 BAM_bi_nondev = "Without segregation distortion\n Only biallelics/ Counts from BAM",
                 VCF_multi_nondev = "Without segregation distortion\n With multiallelics/ Counts from VCF",
                 BAM_multi_nondev = "Without segregation distortion\n With multiallelics/ Counts from BAM",
                 VCF_bi_dev = "With segregation distortion\n Only biallelics/ Counts from VCF",
                 BAM_bi_dev = "With segregation distortion\n Only biallelics/ Counts from BAM",
                 VCF_multi_dev = "With segregation distortion\n With multiallelics/ Counts from VCF",
                 BAM_multi_dev = "With segregation distortion\n With multiallelics/ Counts from BAM")

df_sub$dataset <- legend_text[match(df_sub$dataset, names(legend_text))]

# Comparison with GUSMap

df_sub$countsfrom <- df_sub$dataset
df_sub$countsfrom[grep("BAM", df_sub$dataset)] <- "BAM"
df_sub$countsfrom[grep("VCF", df_sub$dataset)] <- "VCF"

save(df_sub , file = "df_sub.RData")

my_shapes <- c(0, 1, 2, 5, 6, 7, 10, 14)

gus_paper <- df_sub %>% group_by(dataset, depth, SNPCall, GenoCall, filt, countsfrom) %>% 
  summarise(mean_D = mean(D),
            sd_D = sd(D),
            mean_markers = mean(n_markers),
            sd_markers = sd(n_markers)) %>%
  filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) &
           #!(GenoCall == "freebayes/GATK" & countsfrom == "VCF" & SNPCall == "freebayes" & depth == "Depth 10") &
           filt == "+ only informative markers" &
           grepl("biallelics", dataset) &
           grepl("With segr", dataset)) %>%
  ggplot(aes(x = mean_markers, y = log(mean_D, base = 10), color = GenoCall, shape = GenoCall)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymax = log(mean_D, base = 10) + log(sd_D,base = 10), ymin = log(mean_D, base = 10) - log(sd_D, base = 10))) + 
  geom_errorbarh(aes(xmax = mean_markers + sd_markers, xmin = mean_markers - sd_markers)) +
  facet_grid(depth + countsfrom ~ SNPCall) +
  scale_color_manual(values = colors) + 
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlab("mean # of markers") + ylab(expression("Log"[10]*" of mean Euclidean distance")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware")

ggsave(gus_paper, filename = "gus_paper.png", width = 170, height = 120, units = "mm")

gus_suppl <- df_sub %>% group_by(dataset, depth, SNPCall, GenoCall, filt,countsfrom) %>% 
  summarise(mean_D = mean(D),
            sd_D = sd(D),
            mean_markers = mean(n_markers),
            sd_markers = sd(n_markers)) %>%
  filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "GUSMap")) &
           filt == "+ only informative markers" &
           grepl("biallelics", dataset) &
           grepl("With segr", dataset)) %>%
  ggplot(aes(x = mean_markers, y = log(mean_D, base = 10), color = GenoCall, shape = GenoCall)) +
  geom_errorbar(aes(ymax = log(mean_D, base = 10) + log(sd_D,base = 10), ymin = log(mean_D, base = 10) - log(sd_D, base = 10))) + 
  geom_errorbarh(aes(xmax = mean_markers + sd_markers, xmin = mean_markers - sd_markers)) +
  geom_point(size = 3) + 
  facet_grid(depth + countsfrom~ SNPCall) +
  scale_color_manual(values = colors) + theme_bw() +
  scale_shape_manual(values = my_shapes) +
  xlab("mean # of markers") + ylab(expression("Log"[10]*" of mean Euclidean distance")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware")

ggsave(gus_suppl, filename = "gus_suppl.png", width = 170, height = 120, units = "mm")

#
load("df_long2.RData") # from breakpoint_count

change.name <- c("no extra filters" = "no extra filters",
                 "genotype prob > 0.8" = "genotype prob > 0.8",
                 "genotype prob > 0.8 \n only informative markers" = "+ only informative markers",
                 "genotype prob > 0.8 \n only informative markers \n missing replaced" = "+ missing replaced")

df_long2$filt <- change.name[match(df_long2$filt, names(change.name))]

df_long2$filt <- factor(df_long2$filt, levels =  c("no extra filters", 
                                                   "genotype prob > 0.8", 
                                                   "+ only informative markers",
                                                   "+ missing replaced"))

total_wrong <- df_long2 %>% group_by(dataset, filt, GenoCall, SNPCall, depth, seed) %>% summarise(total_wrong = sum(sqrt(as.numeric(wrong_breakpoints)^2)))

idx <- match(paste0(total_wrong$dataset, total_wrong$depth, total_wrong$SNPCall, total_wrong$GenoCall, total_wrong$filt, total_wrong$seed),
             paste0(df_sub$dataset, df_sub$depth, df_sub$SNPCall, df_sub$GenoCall, df_sub$filt, df_sub$seed))

merge_df <- cbind(total_wrong, df_sub[idx, c(7,8,9,10)])
merge_df$GenoCall <- as.character(merge_df$GenoCall)

means <- merge_df %>% group_by(dataset, GenoCall, filt, SNPCall, depth) %>% 
  summarise(mean_wrong = mean(total_wrong), 
            mean_mks = mean(n_markers), 
            mean_multi_mks = mean(n_multi_markers),
            mean_d = mean(D), 
            median_d = median(D),
            mean_size = mean(total_size),
            sd_wrong = sd(total_wrong),
            sd_mks = sd(n_markers),
            sd_multi_mks = sd(n_multi_markers),
            sd_d = sd(D),
            sd_size = sd(total_size)) %>% 
  mutate(min_mks = mean_mks-sd_mks, max_mks = mean_mks + sd_mks, 
         min_d = mean_d - sd_d, max_d = mean_d + sd_d,
         min_size = mean_size - sd_size, max_size = mean_size + sd_size)

means$min_d[which(means$min_d < 0)] <- 0


# Correlation size and breakpoints - All data set
merge_df$GenoCall
merge_df$global <- merge_df$GenoCall
merge_df$global[grep("5%",merge_df$global)] <- "5% error rate"
merge_df$global[grep("0.001%",merge_df$global)] <- "0.001% error rate"
merge_df$global[-c(grep("5%",merge_df$GenoCall), grep("0.001%",merge_df$GenoCall))] <- "relative error"

merge_df <- merge_df %>% mutate(difference = total_wrong - total_size)

save(merge_df, file = "merge_df.RData")

p_paper_corr <- merge_df  %>% 
  ggplot(aes(x = log(total_wrong, base = 10), y = log(D, base = 10))) +
  geom_point(alpha = 0.2)  + theme_bw() +
  facet_grid(.~global) +
  xlab(expression("Log"[10]*" of the number of wrong recombination breakpoints")) + 
  ylab(expression("Log"[10]*" of the Euclidean distance")) +
  annotate("rect", xmin = 1, xmax = 2, ymin = -Inf, ymax = 0,
           alpha = .2, color = "red", fill = "red")

ggsave(p_paper_corr, filename = "corr_size_wrong_all.png", width = 170, height = 120, units = "mm")

### Multiallelics x biallelics

means$multi <- means$dataset
means$multi[grep("multiallelics", means$dataset)] <- "biallelics_multiallelics"
means$multi[grep("biallelics", means$dataset)] <- "biallelics"

multi <- means %>% filter(multi == "biallelics_multiallelics")
bi <- means %>% filter(multi == "biallelics")

colnames(multi)[6:22] <- paste0(colnames(multi)[6:22], "_multi")

colnames(bi)[6:22] <- paste0(colnames(bi)[6:22], "_bi")

means_wider <- merge(multi, bi, by = c(2:5))

save(means_wider, file = "means_wider.RData")

p_paper1 <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) & 
                                     (filt %in% c("+ only informative markers")) & 
                                     SNPCall == "freebayes" &
                                     grepl("VCF", dataset.x) &
                                     grepl("VCF", dataset.y) &
                                     grepl("With s", dataset.x) &
                                     grepl("With s", dataset.y)) %>% 
  ggplot(aes(x = log(mean_d_multi, base = 10), y = log(mean_d_bi, base = 10), color = GenoCall, shape = GenoCall)) + 
  geom_abline(color = "black")+
  geom_errorbar(aes(ymax = log(max_d_bi,base = 10), ymin = log(min_d_bi, base = 10))) + 
  geom_errorbarh(aes(xmax = log(max_d_multi,base = 10), xmin =  log(min_d_multi,base = 10))) +
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(-1,4)) + ylim(-1, 4) + 
  facet_grid(depth~SNPCall)  + 
  xlab(expression(atop("Log"[10]*" of mean Euclidean distance","(with multiallelic markers)"))) + 
  ylab(expression("Log"[10]*" of mean Euclidean distance (without multiallelic markers)")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + ggtitle("A")

p_paper2 <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) & 
                                     (filt %in% c("+ only informative markers")) & 
                                     SNPCall == "freebayes" &
                                     grepl("VCF", dataset.x) &
                                     grepl("VCF", dataset.y) &
                                     grepl("With s", dataset.x) &
                                     grepl("With s", dataset.y)) %>% 
  ggplot(aes(x = GenoCall, y = mean_multi_mks_multi, fill = GenoCall, shape = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values = colors) + 
  facet_grid(depth~SNPCall) +
  theme_bw() + ylab("# of multiallelic markers") + 
  theme(axis.text.x = element_text(angle = 60, hjust=1), axis.title.x = element_blank()) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") +
  ggtitle("B")

p_paper <- ggarrange(p_paper1 ,p_paper2, ncol = 2, common.legend = TRUE)

ggsave(p_paper, filename = "bixmulti_paper.png", width = 170, height = 150, units = "mm")

p_supple1 <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                      (filt %in% c("+ only informative markers")) & 
                                      SNPCall == "freebayes" &
                                      grepl("VCF", dataset.x) &
                                      grepl("VCF", dataset.y) &
                                      grepl("With s", dataset.x) &
                                      grepl("With s", dataset.y)) %>% 
  ggplot(aes(x = log(mean_d_multi, base = 10), y = log(mean_d_bi, base = 10), color = GenoCall, shape = GenoCall)) + 
  geom_abline(color = "black")+
  geom_errorbar(aes(ymax = log(max_d_bi,base = 10), ymin = log(min_d_bi, base = 10))) + 
  geom_errorbarh(aes(xmax = log(max_d_multi,base = 10), xmin =  log(min_d_multi,base = 10))) +
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(-1,4)) + ylim(-1, 4) + 
  facet_grid(depth~SNPCall)  + 
  xlab(expression(atop("Log"[10]*" of mean Euclidean distance", "(with multiallelic markers)"))) + 
  ylab(expression("Log"[10]*" of mean Euclidean distance (without multiallelic markers)")) +
  labs(color = "Genotype call \nsofware", shape = "Genotype call \nsofware") + ggtitle("A")

p_supple2 <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                      (filt %in% c("+ only informative markers")) & 
                                      SNPCall == "freebayes" &
                                      grepl("VCF", dataset.x) &
                                      grepl("VCF", dataset.y) &
                                      grepl("With s", dataset.x) &
                                      grepl("With s", dataset.y)) %>% 
  ggplot(aes(x = GenoCall, y = mean_multi_mks_multi, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values = colors) + 
  facet_grid(depth~SNPCall) +
  theme_bw() + ylab("# of multiallelic markers") + 
  theme(axis.text.x = element_text(angle = 60, hjust=1), axis.title.x = element_blank()) +
  labs(color = "Genotype call \nsoftware") + ggtitle("B")

p_supple <- ggarrange(p_supple1 , p_supple2, ncol = 2, common.legend = TRUE)

ggsave(p_supple, filename = "bixmulti_suppl.png", width = 170, height = 120, units = "mm")

### Effect of segregation distortion 

means$dev <- means$dataset
means$dev[grep("With segr", means$dataset)] <- "with"
means$dev[grep("Without segr", means$dataset)] <- "without"

dev <- means %>% filter(dev == "with")
nondev <- means %>% filter(dev == "without")

colnames(dev)[6:22] <- paste0(colnames(dev)[6:22], "_dev")

colnames(nondev)[6:22] <- paste0(colnames(nondev)[6:22], "_nondev")

means_wider <- merge(dev, nondev, by = c(2:5))

save(means_wider, file = "means_wider.RData")

p_paper <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) & 
                                    (filt %in% c("+ only informative markers")) & 
                                    grepl("VCF", dataset.x) &
                                    grepl("VCF", dataset.y) &
                                    grepl("biallelics", dataset.x) &
                                    grepl("biallelics", dataset.y)) %>% 
  ggplot(aes(x = log(mean_d_dev, base = 10), y = log(mean_d_nondev, base = 10), color = GenoCall, shape = GenoCall)) + geom_abline(color = "black")+
  geom_errorbar(aes(ymax = log(max_d_nondev,base = 10), ymin = log(min_d_nondev, base = 10))) + 
  geom_errorbarh(aes(xmax = log(max_d_dev,base = 10), xmin =  log(min_d_dev,base = 10))) +
  geom_point(size =3 ) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(-1,4)) + ylim(-1, 4) + 
  facet_grid(depth~SNPCall)  + 
  xlab(expression(atop("Log"[10]*" of the mean Euclidean distance","(with segregation distortion)"))) + 
  ylab(expression(atop("Log"[10]*" of the mean Euclidean distance","(without segregation distortion)"))) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") 

ggsave(p_paper, filename = "devxnondev_paper.png", width = 170, height = 120, units = "mm")

p_supple <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                     (filt %in% c("+ only informative markers")) & 
                                     grepl("VCF", dataset.x) &
                                     grepl("VCF", dataset.y) &
                                     grepl("biallelics", dataset.x) &
                                     grepl("biallelics", dataset.y)) %>% 
  ggplot(aes(x = log(mean_d_dev, base = 10), y = log(mean_d_nondev, base = 10), color = GenoCall, shape = GenoCall)) + geom_abline(color = "black")+
  geom_errorbar(aes(ymax = log(max_d_nondev,base = 10), ymin = log(min_d_nondev, base = 10))) + 
  geom_errorbarh(aes(xmax = log(max_d_dev,base = 10), xmin =  log(min_d_dev,base = 10))) +
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(-1,4)) + ylim(-1, 4) + 
  facet_grid(depth~SNPCall)  + 
  xlab(expression(atop("Log"[10]*" of the mean Euclidean distance","(with segregation distortion)"))) + 
  ylab(expression(atop("Log"[10]*" of the mean Euclidean distance","(without segregation distortion)"))) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware")

ggsave(p_supple, filename = "devxnondev_supple.png", width = 170, height = 120, units = "mm")

### Effect of filters

means$countsfrom <- means$dataset
means$countsfrom[grep("VCF", means$dataset)] <- "VCF"
means$countsfrom[grep("BAM", means$dataset)] <- "BAM"
means$SNPCall[grep("freebayes", means$SNPCall)] <- "freebayes"

means$filt <- factor(means$filt, levels = c("no extra filters", 
                                            "genotype prob > 0.8", 
                                            "+ only informative markers",
                                            "+ missing replaced"))

colors <- c("#332288", "#88CCEE", "#117733", "#44AA99", "#882255", "#CC6677")

save(means, file = "means.RData")

filters_supple1 <- means %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                      grepl("multiallelics", dataset) &
                                      grepl("With s", dataset)) %>% 
  ggplot(aes(x = filt, y = log(median_d, base = 10),  group = GenoCall, shape = GenoCall)) +
  geom_line(aes(color = GenoCall)) + 
  geom_point(aes(color = GenoCall)) + 
  scale_color_manual(values = colors) + 
  scale_shape_manual(values = my_shapes) +
  facet_grid(depth + countsfrom ~ SNPCall) +
  theme_bw() + 
  xlab(expression("Filters")) + 
  ylab(expression("Log"[10]*" of the mean Euclidean distance")) +
  labs(color = "Genotype call\nsoftware", shape = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("A") 


filters_supple2 <- means %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                      grepl("multiallelics", dataset) &
                                      grepl("With s", dataset)) %>% 
  ggplot(aes(x = filt, y = mean_mks, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymax = max_mks, ymin = min_mks), position=position_dodge2()) + 
  #geom_point(aes(color = GenoCall), alpha = 0.8) +
  scale_fill_manual(values = colors) + 
  facet_grid(depth + countsfrom ~ SNPCall) +
  theme_bw() +
  xlab(expression("Filters")) + 
  ylab(expression("Mean number markers")) +
  labs(color = "Genotype call\nsoftware", shape = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("B") 

filters_supple <- ggarrange(filters_supple1, filters_supple2, common.legend = TRUE)

ggsave(filters_supple, filename = "filters_supple.png", width = 170, height = 120, units = "mm")

filters_paper1 <- means %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                     grepl("multiallelics", dataset) &
                                     grepl("With s", dataset)) %>% 
  ggplot(aes(x = filt, y = log(mean_d, base = 10),  group = GenoCall, shape = GenoCall)) +
  geom_line(aes(color = GenoCall)) + 
  geom_point(aes(color = GenoCall)) +
  scale_color_manual(values = colors) + 
  scale_shape_manual(values = my_shapes) +
  facet_grid(depth + countsfrom ~ SNPCall) +
  theme_bw() +
  xlab(expression("Filters")) + 
  ylab(expression("Log"[10]*" of the mean Euclidean distance")) +
  labs(color = "Genotype call\nsoftware", shape = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("A") 
#guides(shape = guide_legend(override.aes = list(size = 5)))

filters_paper2 <- means %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                     grepl("multiallelics", dataset) &
                                     grepl("With s", dataset)) %>% 
  ggplot(aes(x = filt, y = mean_mks, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_errorbar(aes(ymax = max_mks, ymin = min_mks), position=position_dodge2()) + 
  #geom_point(aes(color = GenoCall), alpha = 0.8) +
  scale_fill_manual(values = colors) + 
  facet_grid(depth + countsfrom ~ SNPCall) +
  theme_bw() +
  xlab(expression("Filters")) + 
  ylab(expression("Mean number markers")) +
  labs(color = "Genotype call\nsoftware", shape = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("B") 

filters_paper <- ggarrange(filters_paper1, filters_paper2, common.legend = TRUE)

ggsave(filters_paper, filename = "filters_paper.png", width = 190, height = 225, units = "mm")

# GenoCall effect

means$GenoCall<- gsub("freebayes/GATK", "fb/GATK", means$GenoCall) 
means$SNPCall <- gsub("freebayes", "freebayes (fb)", means$SNPCall)
means$GenoCall <- gsub("SuperMASSA", "SMASSA", means$GenoCall)

save(means, file = "means.RData")

p_paper <- means %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SMASSA", "SMASSA (5%)")) & 
                              grepl("multiallelics", dataset) &
                              !(filt %in% c("genotype prob > 0.8", "no extra filters", "+ only informative markers")) & 
                              grepl("VCF", dataset) &
                              grepl("With s", dataset)) %>% 
  ggplot(aes(x = mean_mks, y = log(mean_d, base = 10), color =  mean_wrong, label = GenoCall)) + 
  geom_errorbar(aes(ymax = log(max_d,base = 10), ymin = log(min_d, base = 10))) + 
  geom_errorbarh(aes(xmax = max_mks, xmin =  min_mks)) +
  ggrepel::geom_label_repel(color = 'black',
                            size = 2, box.padding = 0.8, min.segment.length = 0.3,
                            segment.linetype = 3) +
  geom_point() +
  scale_color_gradient(low = "blue",  high = "red") +
  scale_fill_gradient(low = "blue",  high = "red") +
  theme_bw() +
  facet_grid(depth~SNPCall)  + 
  xlab("Mean number of markers") + ylab(expression("Log"[10]*" of the mean Euclidean distance")) +
  labs(color = "Mean number \nof wrong \nrecombination \nbreakpoint", shape = "Genotype call software") +
  guides(shape = guide_legend(override.aes = list(size = 2))) 

ggsave(p_paper, filename = "size_paper_multi.png", width = 170, height = 120, units = "mm")

means$GenoCall <- gsub("SuperMASSA", "SMASSA", means$GenoCall)

p_supple <- means %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SMASSA", "SMASSA (5%)", "updog", "updog (5%)")) & 
                               grepl("multiallelics", dataset) &
                               !(filt %in% c("genotype prob > 0.8", "no extra filters", "+ only informative markers")) & 
                               grepl("VCF", dataset) &
                               grepl("With s", dataset)) %>% 
  ggplot(aes(x = mean_mks, y = log(mean_d, base = 10), color =  mean_wrong, label = GenoCall)) + 
  geom_errorbar(aes(ymax = log(max_d,base = 10), ymin = log(min_d, base = 10))) + 
  geom_errorbarh(aes(xmax = max_mks, xmin =  min_mks)) +
  ggrepel::geom_label_repel(color = 'black',
                            size = 2, box.padding = 1, segment.linetype = 3, nudge_x = -0.8, nudge_y = 0.6) +
  geom_point() + scale_shape_manual(values = c(15, 16, 17, 22,24))+
  scale_color_gradient(low = "blue",  high = "red") +
  scale_fill_gradient(low = "blue",  high = "red") +
  theme_bw() +
  facet_grid(depth~SNPCall)  + 
  xlab("Mean number of markers") + ylab(expression("Log"[10]*" of the mean Euclidean distance")) +
  labs(color = "Mean number \nof wrong \nrecombination \nbreakpoint", shape = "Genotype call software") +
  guides(shape = guide_legend(override.aes = list(size = 2))) 

ggsave(p_supple, filename = "size_suppl_multi.png", width = 170, height = 120, units = "mm")

### Empirical
data.gz <- c("/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_GQ/EmpiricalReads_results_bugfix.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_GQ/EmpiricalReads_results_bugfix.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ/EmpiricalReads_results_bugfix.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_bugfix.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_GQ/EmpiricalReads_results_cont_bugfix.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_GQ/EmpiricalReads_results_cont_bugfix.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results_bugfix.tar.gz")

df_total <- data.frame()
for(i in 1:length(data.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- file.path(path_dir, list_files)
  path.file <- list_files[grep("data2_maps", list_files)]
  df <- vroom(path.file, num_threads = 15)
  if(grepl("rose", data.gz[i])) dataset2 <- "rose" else if(grepl("_cont", data.gz[i])) dataset2 <- "aspen_cont" else dataset2 <- "aspen"
  if(grepl("multiallelics", data.gz[i])) dataset <- "multi" else dataset <- "bi"
  if(grepl("replaced", data.gz[i])) dataset3 <- "filt_GQ_noninfo_replaced" else 
    if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else 
      if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else 
        dataset3 <- "GQ"
  df_temp <- cbind(data=dataset, emp = dataset2, filt = dataset3, df)
  df_total <- rbind(df_total, df_temp)
}

change.name <- c(GQ = "no extra filters",
                 filt_GQ = "genotype prob > 0.8",
                 filt_GQ_noninfo = "+ only informative markers",
                 filt_GQ_noninfo_replaced = "+ missing replaced")

df_total$filt <- change.name[match(df_total$filt, names(change.name))]

df_total$filt <- factor(df_total$filt, levels =  c("no extra filters", 
                                                   "genotype prob > 0.8", 
                                                   "+ only informative markers",
                                                   "+ missing replaced"))

df_total <- perfumaria(df_total)

df_total$GenoCall <- factor(df_total$GenoCall, levels =  c("polyRAD",
                                                           "polyRAD (5%)",
                                                           "updog",
                                                           "updog (5%)",
                                                           "SuperMASSA",
                                                           "SuperMASSA (5%)",
                                                           "freebayes/GATK",
                                                           "freebayes/GATK (5%)",
                                                           "freebayes/GATK (0.001%)", "GUSMap"))

df_total$dataset <- paste0(df_total$CountsFrom, "_", df_total$data)

data_plot <- df_total %>% group_by(dataset, filt, SNPCall, GenoCall, emp) %>% 
  summarise(mean_diff = mean(diff(rf)), 
            se_diff = sd(diff(rf))/sqrt(length(diff(rf))),
            n_mks = n(),
            total_size = rf[length(rf)],
            n_multi_mks = length(which(type %in% c("D1.9", "A.2", "D2.14", "A.1"))))

data_plot2 <- df_total %>% group_by(dataset, filt, SNPCall, GenoCall, emp) %>% summarise(diff = diff(rf), 
                                                                                         n_mks = n())

data_types <- df_total  %>% group_by(dataset, data, CountsFrom, filt, SNPCall, GenoCall, emp, type) %>% summarise(n_mks = n())

legend_text <- c(VCF_bi = "Only biallelics/ Counts from VCF",
                 BAM_bi = "Only biallelics/ Counts from BAM",
                 VCF_multi = "With multiallelics/ Counts from VCF",
                 BAM_multi = "With multiallelics/ Counts from BAM")

legend_text2 <- c(VCF_bi = "Only biallelics\nCounts from VCF",
                  BAM_bi = "Only biallelics\nCounts from BAM",
                  VCF_multi = "With multiallelics\nCounts from VCF",
                  BAM_multi = "With multiallelics\nCounts from BAM")

colors.p <- c("#E69F00", "#56B4E9", "#009E73", "#D55E00")
names(colors.p) <- legend_text

data_plot$dataset <- legend_text[match(data_plot$dataset, names(legend_text))]
data_plot2$dataset <- legend_text[match(data_plot2$dataset, names(legend_text))]
data_types$dataset <- legend_text2[match(data_types$dataset, names(legend_text2))]

### Marker types
save(data_types, file = "data_types.RData")

size_plot1 <- data_types %>% filter(GenoCall != "GUSMap" & 
                                      emp == "rose" &
                                      data == "multi" &
                                      grepl("VCF", dataset) &
                                      grepl("multiallelics", dataset) &
                                      !(grepl("%", GenoCall))) %>% 
  ggplot(aes(x=filt,y = n_mks, fill = type)) + 
  geom_bar(stat= "identity", position = position_dodge()) + 
  geom_text(aes(label=n_mks), position = position_dodge(width= 0.9), vjust = -0.3, size = 2) +
  scale_y_continuous(expand = c(.1,.1)) +
  scale_fill_manual(values = my_colors) + 
  facet_grid(GenoCall ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "right", legend.title = element_blank())  + 
  ylab("Number of markers") + xlab("Filters") +
  ggtitle("Rose")

ggsave(size_plot1, filename = "marker_type_rose.png", width = 170, height = 225, units = "mm")

size_plot1 <- data_types %>% filter(GenoCall != "GUSMap" & 
                                      emp == "aspen" &
                                      data == "multi" &
                                      grepl("BAM", dataset) &
                                      grepl("multiallelics", dataset) &
                                      !(grepl("%", GenoCall))) %>% 
  ggplot(aes(x=filt, y = n_mks, fill = type)) + 
  geom_bar(stat= "identity", position = position_dodge()) + 
  geom_text(aes(label=n_mks), position = position_dodge(width= 0.9), vjust = -0.3, size = 2) +
  scale_y_continuous(expand = c(.1,.1)) +
  scale_fill_manual(values = my_colors) + 
  facet_grid(GenoCall ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "right")  + 
  ylab("Number of markers") + xlab("Filters") +
  ggtitle("Aspen")

ggsave(size_plot1, filename = "marker_type_aspen.png", width = 170, height = 225, units = "mm")

## Without gusmap
load("counts_emp.RData")
counts_all <- x2 %>% group_by(dataset, dataset2, multi, filt, file, GenoCall, SNPCall, CountsFrom) %>% 
  summarise(total_counts = sum(counts), 
            var_counts = var(counts),
            max_counts = max(counts) - min(counts))

temp <- x2[which(x2$file == "map_freebayes_vcf_SNPCaller.RData" & x2$filt == "+ missing replaced" & x2$multi == "multi" & grepl("VCF", x2$dataset2)),]
colnames(counts_all)[1:2] <- c("emp", "dataset") 
merge_emp <- merge(data_plot, counts_all, by =  c("emp", "dataset", "filt", "GenoCall", "SNPCall"))

# Correlation size x breakpoints - All data set 
# p_paper_corr <- merge_emp  %>% 
#   ggplot(aes(x = log(total_counts, base = 10), y = log(total_size, base = 10))) + 
#   geom_point(alpha = 0.3) + scale_color_viridis_d() + theme_bw() + geom_hline(aes(yintercept = log(38, base = 10)), color = "red") +
#   xlab(expression("Log"[10]*" of the number of recombination breakpoint")) + ylab(expression("Log"[10]*" of the total map size")) +
#   xlim(c(0,5)) + ylim(c(0,5)) 
# 
# ggsave(p_paper_corr, filename = "corr_size_wrong_all_emp.png", width = 170, height = 120, units = "mm")


merge_emp$emp <- gsub("rose", "rose (~94x)", merge_emp$emp)
merge_emp$emp <- gsub("aspen", "aspen (~6x)", merge_emp$emp)

# Comparision with GUSMap

gus_comp <- data_plot
gus_comp$emp <- gsub("rose", "rose (~94x)", gus_comp$emp)
gus_comp$emp <- gsub("aspen", "aspen (~6x)", gus_comp$emp)

gus_comp$CountsFrom <- gus_comp$dataset
gus_comp$CountsFrom[grepl("VCF", gus_comp$CountsFrom)] <- "VCF"
gus_comp$CountsFrom[grepl("BAM", gus_comp$CountsFrom)] <- "BAM"

save(gus_comp, file = "gus_comp.RData")

gus_paper <- gus_comp %>% filter(emp %in% c("aspen (~6x)", "rose (~94x)") &
                                   !(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) &
                                   filt == "+ missing replaced" &
                                   grepl("biallelics", dataset)) %>%
  ggplot(aes(x = n_mks, y = log(total_size, base = 10), color = GenoCall, shape = GenoCall)) +
  geom_point(size = 3) + 
  facet_grid(emp + CountsFrom~ SNPCall) + geom_hline(yintercept = log(38, base = 10), color = "red") +
  scale_color_manual(values = colors) + theme_bw() +
  scale_shape_manual(values = my_shapes) +
  xlab("# of markers") + ylab(expression("Log"[10]*" of map size")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware")

ggsave(gus_paper, filename = "gus_paper_emp.png", width = 170, height = 120, units = "mm")

gus_suppl <- gus_comp %>% filter(emp %in% c("aspen (~6x)", "rose (~94x)") &
                                   (GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "GUSMap")) &
                                   filt == "+ missing replaced" &
                                   grepl("biallelics", dataset)) %>%
  ggplot(aes(x = n_mks, y = log(total_size, base = 10), color = GenoCall, shape = GenoCall)) +
  geom_point(size = 3) + 
  facet_grid(emp + CountsFrom~ SNPCall) + geom_hline(yintercept = log(38, base = 10), color = "red") +
  scale_color_manual(values = colors) + theme_bw() +
  scale_shape_manual(values = my_shapes) +
  xlab("# of markers") + ylab(expression("Log"[10]*" of map size")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware")

ggsave(gus_suppl, filename = "gus_suppl_emp.png", width = 170, height = 120, units = "mm")

# Filters effect
colors <- c("#332288", "#88CCEE", "#117733", "#44AA99", "#882255", "#CC6677")
save(merge_emp, file = "merge_emp.RData")
filters_supple1 <- merge_emp %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                          grepl("biallelics", dataset) & # multiallelics have issues with freebayes 
                                          emp %in% c("rose (~94x)", "aspen (~6x)")) %>% 
  ggplot(aes(x = filt, y = log(total_size, base = 10),  group = GenoCall, shape = GenoCall)) +
  geom_line(aes(color = GenoCall)) + 
  geom_point(aes(color = GenoCall)) + 
  scale_color_manual(values = colors) + 
  scale_shape_manual(values = my_shapes) +
  facet_grid(emp + CountsFrom ~ SNPCall) +
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color = "red") +
  xlab(expression("Filters")) + 
  ylab(expression("Log"[10]*" of map size")) +
  labs(color = "Genotype call\nsoftware", shape = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("A")

filters_supple2 <- merge_emp %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                          grepl("biallelics", dataset) &
                                          emp %in% c("rose (~94x)", "aspen (~6x)")) %>% 
  ggplot(aes(x = filt, y = n_mks, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_errorbar(aes(ymax = max_mks, ymin = min_mks), position=position_dodge2()) + 
  #geom_point(aes(color = GenoCall), alpha = 0.8) +
  scale_fill_manual(values = colors) + 
  facet_grid(emp + CountsFrom ~ SNPCall) +
  theme_bw() +
  xlab(expression("Filters")) + 
  ylab(expression("Mean number markers")) +
  labs(color = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("B")

filters_supple <- ggarrange(filters_supple1, filters_supple2, common.legend = TRUE)

ggsave(filters_supple, filename = "filters_supple_emp.png", width = 170, height = 120, units = "mm")

filters_paper1 <- merge_emp %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                         grepl("multiallelics", dataset) &
                                         emp %in% c("rose (~94x)", "aspen (~6x)")) %>% 
  ggplot(aes(x = filt, y = log(total_size, base = 10),  group = GenoCall, shape = GenoCall)) +
  geom_line(aes(color = GenoCall)) + 
  geom_point(aes(color = GenoCall)) + 
  scale_color_manual(values = colors) + 
  scale_shape_manual(values = my_shapes) +
  facet_grid(emp + CountsFrom ~ SNPCall) +
  theme_bw() + geom_hline(yintercept = log(38, base = 10), color = "red") + 
  xlab(expression("Filters")) + 
  ylab(expression("Log"[10]*" of map size")) +
  labs(color = "Genotype call\nsoftware", shape = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("A")

filters_paper2 <- merge_emp %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                         grepl("multiallelics", dataset) &
                                         emp %in% c("rose (~94x)", "aspen (~6x)")) %>% 
  ggplot(aes(x = filt, y = n_mks, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) +
  #geom_errorbar(aes(ymax = max_mks, ymin = min_mks), position=position_dodge2()) + 
  #geom_point(aes(color = GenoCall), alpha = 0.8) +
  scale_fill_manual(values = colors) + 
  facet_grid(emp + CountsFrom ~ SNPCall) +
  theme_bw() +
  xlab(expression("Filters")) + 
  ylab(expression("Mean number markers")) +
  labs(color = "Genotype call\nsoftware") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + ggtitle("B")

filters_paper <- ggarrange(filters_paper1, filters_paper2, common.legend = TRUE)

ggsave(filters_paper, filename = "filters_paper_emp.png", width = 170, height = 225, units = "mm")

# Multiallelics effect

multi <- merge_emp %>% filter(multi == "multi")
bi <- merge_emp %>% filter(multi == "bi")

colnames(multi)[c(2,6:11, 14:16)] <- paste0(colnames(multi)[c(2,6:11, 14:16)], "_multi")

colnames(bi)[c(2,6:11, 14:16)] <- paste0(colnames(bi)[c(2,6:11, 14:16)], "_bi")

means_wider <- merge(multi, bi)
save(means_wider, file = "means_wider_emp.RData")

p_paper1 <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "freebayes/GATK")) & 
                                     (filt %in% c("+ missing replaced")) & 
                                     SNPCall == "freebayes" &
                                     CountsFrom == "VCF") %>% 
  ggplot(aes(x = log(total_size_multi, base = 10), y = log(total_size_bi, base = 10), color = GenoCall, shape = GenoCall)) + 
  geom_abline(color = "black") +
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(0,5)) + ylim(0, 5) + 
  facet_grid(emp~SNPCall)  + 
  xlab(expression("Log"[10]*" of map size (with multiallelic markers)")) + 
  ylab(expression("Log"[10]*" of map size (without multiallelic markers)")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") + ggtitle("A")

p_paper2 <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "freebayes/GATK")) & 
                                     (filt %in% c("+ missing replaced")) & 
                                     SNPCall == "freebayes" &
                                     CountsFrom == "VCF") %>% 
  ggplot(aes(x = GenoCall, y = n_multi_mks_multi, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values = colors) + 
  facet_grid(emp~SNPCall) +
  theme_bw() + ylab("# of multiallelic markers") + 
  theme(axis.text.x = element_text(angle = 60, hjust=1), axis.title.x = element_blank()) +
  labs(color = "Genotype call \nsoftware") + ggtitle("B")

p_paper <- ggarrange(p_paper1, p_paper2, ncol = 2, common.legend = TRUE)

ggsave(p_paper, filename = "bixmulti_paper_emp.png", width = 170, height = 120, units = "mm")

p_supple1 <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                      (filt %in% c("+ missing replaced")) & 
                                      SNPCall == "freebayes" &
                                      CountsFrom == "VCF") %>% 
  ggplot(aes(x = log(total_size_multi, base = 10), y = log(total_size_bi, base = 10), color = GenoCall, shape = GenoCall)) + 
  geom_abline(color = "black")+
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(0,5)) + ylim(0, 5) + 
  facet_grid(emp~SNPCall)  + 
  xlab(expression("Log"[10]*" of map size (with multiallelic markers)")) + 
  ylab(expression("Log"[10]*" of map size (without multiallelic markers)")) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware")  + ggtitle("A")

p_supple2 <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                      (filt %in% c("+ missing replaced")) & 
                                      SNPCall == "freebayes" &
                                      CountsFrom == "VCF") %>% 
  ggplot(aes(x = GenoCall, y = n_multi_mks_multi, fill = GenoCall)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values = colors) + 
  facet_grid(emp~SNPCall) +
  theme_bw() + ylab("# of multiallelic markers") + 
  theme(axis.text.x = element_text(angle = 60, hjust=1), axis.title.x = element_blank()) +
  labs(color = "Genotype call \nsoftware")  + ggtitle("B")

p_supple <- ggarrange(p_supple1, p_supple2, ncol = 2, common.legend = TRUE)

ggsave(p_supple, filename = "bixmulti_suppl_emp.png", width = 170, height = 120, units = "mm")

# Contaminant effects

cont <- merge_emp %>% filter(emp == "aspen (~6x)_cont")
noncont <- merge_emp %>% filter(emp == "aspen (~6x)")

colnames(cont)[c(1,6:10, 14:16)] <- paste0(colnames(cont)[c(1,6:10, 14:16)], "_cont")

colnames(noncont)[c(1,6:10, 14:16)] <- paste0(colnames(noncont)[c(1,6:10, 14:16)], "_noncont")

means_wider <- merge(cont, noncont)
save(means_wider, file = "means_wider.RData")

p_paper1 <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) & 
                                     #!(GenoCall == "freebayes/GATK" & SNPCall == "freebayes") &
                                     (filt %in% c("+ only informative markers")) & 
                                     multi == "multi" &
                                     CountsFrom == "VCF") %>% 
  ggplot(aes(x = log(total_size_cont, base = 10), y = log(total_size_noncont, base = 10), color = GenoCall, shape = GenoCall)) + geom_abline(color = "black")+
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(0,5)) + ylim(0, 5) + 
  facet_grid(.~SNPCall)  + 
  xlab(expression("Log"[10]*" of map size (with contaminant samples)")) + 
  ylab(expression(atop("Log"[10]*" of map size", "(without contaminant samples)"))) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + ggtitle("A")

p_paper2 <- means_wider %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) & 
                                     #!(GenoCall == "freebayes/GATK" & SNPCall == "freebayes") &
                                     (filt %in% c("+ only informative markers")) & 
                                     multi == "multi" &
                                     CountsFrom == "VCF") %>% 
  ggplot(aes(x = log(max_counts_cont, base = 10), y = log(max_counts_noncont, base = 10), color = GenoCall, shape = GenoCall)) + geom_abline(color = "black")+
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(0,3)) + ylim(0, 3) + 
  facet_grid(.~SNPCall)  + 
  xlab(expression("Log"[10]*" of break points counts range (with contaminant samples)")) + 
  ylab(expression(atop("Log"[10]*" of break points counts range","(without contaminant samples)"))) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + ggtitle("B")

p_paper <- ggarrange(p_paper1, p_paper2, nrow = 2, common.legend = TRUE)

ggsave(p_paper, filename = "noncontxcont_paper_emp.png", width = 170, height = 150, units = "mm")

p_suppl1 <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                     (filt %in% c("+ only informative markers")) & 
                                     multi == "multi" &
                                     CountsFrom == "VCF") %>% 
  ggplot(aes(x = log(total_size_cont, base = 10), y = log(total_size_noncont, base = 10), color = GenoCall, shape = GenoCall)) + 
  geom_abline(color = "black")+
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(0,4)) + ylim(0, 4) + 
  facet_grid(.~SNPCall)  + 
  xlab(expression("Log"[10]*" of map size (with contaminant samples)")) + 
  ylab(expression(atop("Log"[10]*" of map size", "(without contaminant samples)"))) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))+ ggtitle("A")

p_suppl2 <- means_wider %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                     (filt %in% c("+ only informative markers")) & 
                                     multi == "multi" &
                                     CountsFrom == "VCF") %>% 
  ggplot(aes(x = log(max_counts_cont, base = 10), y = log(max_counts_noncont, base = 10), color = GenoCall, shape = GenoCall)) + geom_abline(color = "black")+
  geom_point(size = 3) + 
  scale_color_manual(values = colors) +
  scale_shape_manual(values = my_shapes) +
  theme_bw() +
  xlim(c(0,2.5)) + ylim(0, 2.5) + 
  facet_grid(.~SNPCall)  + 
  xlab(expression("Log"[10]*" of break points counts range (with contaminant samples)")) + 
  ylab(expression(atop("Log"[10]*" of break points counts range","(without contaminant samples)"))) +
  labs(color = "Genotype call \nsoftware", shape = "Genotype call \nsoftware") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + ggtitle("B")

p_suppl <- ggarrange(p_suppl1, p_suppl2, nrow = 2, common.legend = TRUE)

ggsave(p_suppl, filename = "noncontxcont_suppl_emp.png", width = 170, height = 150, units = "mm")

# Comparing GenoCall

merge_emp$GenoCall <- gsub("freebayes/GATK", "fb/GATK", merge_emp$GenoCall) 
merge_emp$SNPCall <- gsub("freebayes", "freebayes (fb)", merge_emp$SNPCall)

save(merge_emp, file = "merge_emp.RData")

p_paper <- merge_emp %>% filter(!(GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)")) & 
                                  #!(GenoCall == "fb/GATK" & SNPCall == "freebayes (fb)") & # here
                                  emp %in% c("rose (~94x)", "aspen (~6x)") &
                                  !(filt %in% c("genotype prob > 0.8", "no extra filters", "+ only informative markers")) & 
                                  grepl("VCF", dataset) &
                                  grepl("multiallelics", dataset)) %>% 
  ggplot(aes(x = n_mks, y = log(total_size, base = 10), color =  max_counts, label = GenoCall)) + 
  ggrepel::geom_label_repel(color = 'black',
                            size = 2, box.padding = 1, min.segment.length = 0, segment.linetype = 3) +
  geom_point() + 
  scale_color_gradient(low = "blue",  high = "red") +
  scale_fill_gradient(low = "blue",  high = "red") +
  theme_bw() + geom_hline(yintercept = log(38, base = 10), color = "red") +
  facet_grid(emp~SNPCall)  + 
  xlab("Mean number of markers") + ylab(expression("Log"[10]*" of map size")) +
  labs(color = "Break point \ncounts range") 

ggsave(p_paper, filename = "size_paper_multi_emp.png", width = 170, height = 120, units = "mm")

p_supple <- merge_emp %>% filter((GenoCall %in% c("polyRAD", "polyRAD (5%)","SuperMASSA", "SuperMASSA (5%)", "updog", "updog (5%)")) & 
                                   emp %in% c("rose (~94x)", "aspen (~6x)") &
                                   !(filt %in% c("genotype prob > 0.8", "no extra filters", "+ only informative markers")) & 
                                   grepl("VCF", dataset) &
                                   grepl("multiallelics", dataset)) %>% 
  ggplot(aes(x = n_mks, y = log(total_size, base = 10), color =  max_counts, label = GenoCall)) + 
  ggrepel::geom_label_repel(color = 'black',
                            size = 2, box.padding = 1, min.segment.length = 0, segment.linetype = 3, nudge_x = 0.6, nudge_y = 0.2) +
  geom_point() + 
  scale_color_gradient(low = "blue",  high = "red") +
  scale_fill_gradient(low = "blue",  high = "red") +
  theme_bw() + geom_hline(yintercept = log(38, base = 10), color = "red") +
  facet_grid(emp~SNPCall)  + 
  xlab("Mean number of markers") + ylab(expression("Log"[10]*" of map size")) +
  labs(color = "Break point \ncounts range") 

ggsave(p_supple, filename = "size_supple_multi_emp.png", width = 170, height = 120, units = "mm")


# Old graphics
total_size_plot1 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "rose") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/400, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 2, position=position_dodge(width=0.9)) +
  #scale_y_break(c(20, 85)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*400, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=9),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=2)) + ggtitle("Rose")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot2 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "aspen") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/550, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 2, position=position_dodge(width=0.9)) +
  #scale_y_break(c(4000, 5500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*550, name="# of markers"),
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=9),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=2)) + ggtitle("Aspen")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot3 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "aspen_cont") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/550, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 2, position=position_dodge(width=0.9)) +
  #scale_y_break(c(10000, 65500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*550, name="# of markers"),
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=9),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=2)) + ggtitle("Aspen with contaminants")


emp_list <- list(total_size_plot1, total_size_plot2, total_size_plot3)

save(emp_list, file = "empirical_map_size.RData")

ggsave(total_size_plot1, filename = "log10_total_size_emp_plot1.png", width = 170, height = 225, units = "mm")
ggsave(total_size_plot2, filename = "log10_total_size_emp_plot2.png", width = 170, height = 225, units = "mm")
ggsave(total_size_plot3, filename = "log10_total_size_emp_plot3.png", width = 170, height = 225, units = "mm")


## With gusmap
total_size_plot1 <- data_plot %>% filter(emp == "rose"  & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/350, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 2, position=position_dodge(width=0.9)) +
  #scale_y_break(c(20, 85)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*350, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=9),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=2)) + ggtitle("Rose")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot2 <- data_plot %>% filter(emp == "aspen"  & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/550, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 2, position=position_dodge(width=0.9)) +
  #scale_y_break(c(4000, 5500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*550, name="# of markers"),
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=9),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=2)) + ggtitle("Aspen")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot3 <- data_plot %>% filter(emp == "aspen_cont"  & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/550, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = log(total_size, base = 10)), size = 2, position=position_dodge(width=0.9)) +
  #scale_y_break(c(10000, 65500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = expression("Log"[10]*" of the total size (cM)"),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*550, name="# of markers"),
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = log(38, base = 10), color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=9),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=2)) + ggtitle("Aspen with contaminants")

ggsave(total_size_plot1, filename = "log_total_size_emp_plot_gusmap1.png", width = 170, height = 112.5, units = "mm")
ggsave(total_size_plot2, filename = "log_total_size_emp_plot_gusmap2.png", width = 170, height = 120, units = "mm")
ggsave(total_size_plot3, filename = "log_total_size_emp_plot_gusmap3.png", width = 170, height = 112.5, units = "mm")

