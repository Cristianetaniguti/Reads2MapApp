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
             system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))

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
  if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else dataset3 <- "GQ"
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

i <- 1

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
  data1 <- data1_depths_geno_prob
  data1$lab <- df_total$lab[match(data1$for_depth, df_total$for_depth)]
  if(length(which(is.na(data1$lab))) > 0)
    data1 <- data1[-which(is.na(data1$lab)),]
  
  data1$wrong <- NA
  data1$wrong[which(data1$lab %in% c("True=B3.7\nEst=B3.7","True=D1.10\nEst=D1.10" , "True=D2.15\nEst=D2.15"))] <- "correct"
  data1$wrong[which(!(data1$lab %in% c("True=B3.7\nEst=B3.7","True=D1.10\nEst=D1.10" , "True=D2.15\nEst=D2.15")))] <- "wrong"
  
  alpha <- 1
  
  sub_data <- list()
  temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 10" & CountsFrom == "BAM" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[1]] <- temp 
  temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 10" & CountsFrom == "VCF" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[2]] <- temp 
  temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 20" & CountsFrom == "BAM" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[3]] <- temp 
  temp <- data1  %>% filter(SNPCall == "freebayes" & depth == "Depth 20" & CountsFrom == "VCF" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[4]] <- temp 
  temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 10" & CountsFrom == "BAM" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[5]] <- temp 
  temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 10" & CountsFrom == "VCF" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[6]] <- temp 
  temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 20" & CountsFrom == "BAM" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[7]] <- temp 
  temp <- data1  %>% filter(SNPCall == "GATK" & depth == "Depth 20" & CountsFrom == "VCF" & wrong == "wrong")
  if(dim(temp)[1] > 0) sub_data[[8]] <- temp 
  
  sub_data <- sub_data[-which(sapply(sub_data, is.null))]
  
  p <- list()
  for(j in 1:length(sub_data)){
    if(!is.null(sub_data[[j]])){
      p[[j]] <- sub_data[[j]] %>% ggplot(aes(x=ref, y=alt, color=graph.color)) + 
        geom_point(aes(shape = pop, size = pop, alpha = pop)) +
        scale_size_manual(values = c(2.5,1)) + 
        scale_color_manual(values = colors) +
        scale_shape_manual(values=c(3, 19)) +
        scale_alpha_manual(values=c(1, 0.5)) +
        labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
        facet_grid(lab ~ paste(SNPCall,"\n", CountsFrom, "\n", depth), scales = "free") +
        guides(colour = guide_legend(override.aes = list(alpha = 1), nrow=3), 
               shape = guide_legend(override.aes = list(alpha = 1), ncol = 1),
               size ="none", alpha ="none")  + theme_bw() + 
        theme(legend.position = "right", legend.title = element_blank(), 
              axis.text.y =  element_text(size=6),
              axis.title = element_text(size=8),
              legend.text = element_text(size=6),
              strip.text = element_text(size = 6),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
              title = element_text(size = 8)) 
    }
  }
  
  p_joint <- ggarrange(plotlist = p, ncol = length(p), common.legend = T, legend = "bottom") 
  ggsave(p_joint, filename = paste0("wrong_by_depth_",gsub("/", "_", software_lab[i]), ".png"), width = 170, height = 225, units = "mm")
  
  #####
  
  df_total$dataset <- paste0(df_total$CountsFrom, "_", df_total$segr)
  
  change.name <- c(GQ = "no extra filters",
                   filt_GQ = "genotype prob > 0.8",
                   filt_GQ_noninfo = "genotype prob > 0.8 \n only informative markers")
  
  df_total$filt <- change.name[match(df_total$filt, names(change.name))]
  
  df_total$filt <- factor(df_total$filt, levels =  c("no extra filters", 
                                                     "genotype prob > 0.8", 
                                                     "genotype prob > 0.8 \n only informative markers"))
  
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

temp <- bi_total 

temp$filt2 <- factor(paste0(bi_total$filt, "\n", bi_total$depth), 
                     levels = c("no extra filters\nDepth 10",
                                "genotype prob > 0.8\nDepth 10",
                                "genotype prob > 0.8 \n only informative markers\nDepth 10",
                                "no extra filters\nDepth 20",
                                "genotype prob > 0.8\nDepth 20",
                                "genotype prob > 0.8 \n only informative markers\nDepth 20"))

temp$filt3 <- factor(paste0(temp$filt, "\n", temp$GenoCall), 
                     levels = unique(paste0(temp$filt, "\n", temp$GenoCall)))

# 2D
bi_correct <- temp %>% filter(wrong == "correct") %>% ggplot(aes(x=lab, y = mean_count, color = dataset)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.8))  + coord_flip() +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "dashed",
    position=position_dodge(width=0.8)) +
  scale_color_manual(values = colors.p) + 
  facet_grid(filt2 ~ SNPCall + GenoCall) + 
  theme_bw() +
  ylab("mean # of marker") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        title = element_text(size=7),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow=2)) + ggtitle("Biallelics - correct")

bi_wrong1 <- temp %>% filter(depth == "Depth 10" & wrong == "wrong") %>% ggplot(aes(x=lab, y = mean_count, color = dataset)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.8))  + coord_flip() +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "dashed",
    position=position_dodge(width=0.8)) +
  scale_color_manual(values = colors.p) + 
  facet_grid(filt2 ~ SNPCall + GenoCall) + 
  theme_bw() +
  ylab("mean # of marker") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        title = element_text(size=7),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow=2)) + ggtitle("Biallelics - wrong")

bi_wrong2 <- temp %>% filter(depth == "Depth 20" & wrong == "wrong") %>% ggplot(aes(x=lab, y = mean_count, color = dataset)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.8))  + coord_flip() +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "dashed",
    position=position_dodge(width=0.8)) +
  scale_color_manual(values = colors.p) + 
  facet_grid(filt2 ~ SNPCall + GenoCall) + 
  theme_bw() +
  ylab("mean # of marker") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        title = element_text(size=7),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow=2)) + ggtitle("Biallelics - wrong")

ggsave(bi_correct, filename = paste0("marker_type_bi_correct.png"), width = 170, height = 225, units = "mm")
ggsave(bi_wrong1, filename = paste0("marker_type_bi_wrong1.png"),  width = 170, height = 225, units = "mm")
ggsave(bi_wrong2, filename = paste0("marker_type_bi_wrong2.png"),  width = 170, height = 225, units = "mm")


temp <- multi_total 

temp$filt2 <- factor(paste0(multi_total$filt, "\n", multi_total$depth), 
                     levels = c("no extra filters\nDepth 10",
                                "genotype prob > 0.8\nDepth 10",
                                "genotype prob > 0.8 \n only informative markers\nDepth 10",
                                "no extra filters\nDepth 20",
                                "genotype prob > 0.8\nDepth 20",
                                "genotype prob > 0.8 \n only informative markers\nDepth 20"))

temp$filt3 <- factor(paste0(temp$filt, "\n", temp$GenoCall), 
                     levels = unique(paste0(temp$filt, "\n", temp$GenoCall)))


multi_plot1 <- temp %>% filter(depth == "Depth 10") %>% ggplot(aes(x=lab, y = mean_count, color = dataset)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.7))  + coord_flip() +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "dashed",
    position=position_dodge(width=0.7)) +
  scale_color_manual(values = colors.p) + 
  facet_grid(filt2 ~ SNPCall + GenoCall) + 
  theme_bw() +
  ylab("mean # of marker") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.y = element_text(size=4.5),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        title = element_text(size=7),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow=2)) + ggtitle("Multiallelics")

multi_plot2 <- temp %>% filter(depth == "Depth 20") %>% ggplot(aes(x=lab, y = mean_count, color = dataset)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.7))  + coord_flip() +
  geom_errorbar(
    aes(ymin = mean_count-sd_count, ymax = mean_count+sd_count),
    width = 0.1,
    linetype = "dashed",
    position=position_dodge(width=0.7)) +
  scale_color_manual(values = colors.p) + 
  facet_grid(filt2 ~ SNPCall + GenoCall) + 
  theme_bw() +
  ylab("mean # of marker") +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.y = element_text(size=4.5),
        axis.text.x = element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        title = element_text(size=7),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 6)) +
  guides(color = guide_legend(nrow=2)) + ggtitle("Multiallelics")


ggsave(multi_plot1, filename = paste0("marker_type_multi1.png"), width = 170, height = 225, units = "mm")
ggsave(multi_plot2, filename = paste0("marker_type_multi2.png"), width = 170, height = 225, units = "mm")


# 3D graphics
# df_temp <- bi %>% filter(wrong == "wrong", SNPCall == "GATK", depth == "Depth 10")
# 
# change.name <- c(GQ = 1, #"no extra filters", 
#                  filt_GQ = 2, #"genotype prob > 0.8", 
#                  filt_GQ_noninfo = 3) #"genotype prob > 0.8 & only informative markers")
# 
# df_temp$filt <- change.name[match(df_temp$filt, names(change.name))]
# 
# plot_ly(df_temp, x= ~lab, 
#         y=~filt, 
#         z=~mean_perc, 
#         color=~dataset, 
#         error_z = ~list(array = sd_perc,
#                         color = dataset))



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
  if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else dataset3 <- "GQ"
  df_temp <- cbind(data=dataset, segr = dataset2, filt = dataset3, df)
  df_total <- rbind(df_total, df_temp)
}

df_total <- df_up

change.name <- c(GQ = "no extra filters",
                 filt_GQ = "genotype prob > 0.8",
                 filt_GQ_noninfo = "genotype prob > 0.8 \n only informative markers")

df_total$filt <- change.name[match(df_total$filt, names(change.name))]

df_total$filt <- factor(df_total$filt, levels =  c("no extra filters", 
                                                   "genotype prob > 0.8", 
                                                   "genotype prob > 0.8 \n only informative markers"))

df_total <- perfumaria(df_total)

df_total$GenoCall <- factor(df_total$GenoCall, levels =  c("polyRAD",
                                                           "polyRAD (5%)",
                                                           "updog",
                                                           "updog (5%)",
                                                           "SuperMASSA",
                                                           "SuperMASSA (5%)",
                                                           "freebayes/GATK",
                                                           "freebayes/GATK (5%)",
                                                           "OneMap_version2", "GUSMap"))

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
  geom_bar(aes(y = mean_n*1, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  # geom_errorbar(
  #   aes(ymin = mean_n-se_n, ymax = mean_n+se_n),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  geom_point(aes(y = total_size), size = 1.5, position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./1, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color= "red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

d_plot[[2]] <- d_df %>% filter(GenoCall != "GUSMap" & depth == "Depth 20" ) %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = mean_n*10, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1.5, position=position_dodge(width=0.9)) +
  # geom_errorbar(
  #   aes(ymin = mean_d-se_d, ymax = mean_d+se_d),
  #   width = 0.1,
  #   linetype = "dashed",
  #   position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Euclidean distance (D)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./10, name="Mean # of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color= "red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) 

ggsave(d_plot[[1]], filename = "total_size1.png", width = 170, height = 225, units = "mm")
ggsave(d_plot[[2]], filename = "total_size2.png", width = 170, height = 225, units = "mm")

ggsave(d_plot[[1]], filename = "d_plot_gusmap1.png", width = 170, height = 120, units = "mm")
ggsave(d_plot[[2]], filename = "d_plot_gusmap2.png", width = 170, height = 120, units = "mm")


### Empirical
data.gz <- c(system.file("ext", "rose/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/biallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"))

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
  if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else dataset3 <- "GQ"
  df_temp <- cbind(data=dataset, emp = dataset2, filt = dataset3, df)
  df_total <- rbind(df_total, df_temp)
}

change.name <- c(GQ = "no extra filters",
                 filt_GQ = "genotype prob > 0.8",
                 filt_GQ_noninfo = "genotype prob > 0.8 \n only informative markers")

df_total$filt <- change.name[match(df_total$filt, names(change.name))]

df_total$filt <- factor(df_total$filt, levels =  c("no extra filters", 
                                                   "genotype prob > 0.8", 
                                                   "genotype prob > 0.8 \n only informative markers"))

df_total <- perfumaria(df_total)

df_total$GenoCall <- factor(df_total$GenoCall, levels =  c("polyRAD",
                                                           "polyRAD (5%)",
                                                           "updog",
                                                           "updog (5%)",
                                                           "SuperMASSA",
                                                           "SuperMASSA (5%)",
                                                           "freebayes/GATK",
                                                           "freebayes/GATK (5%)",
                                                           "OneMap_version2", "GUSMap"))

df_total$dataset <- paste0(df_total$CountsFrom, "_", df_total$data)

data_plot <- df_total %>% group_by(dataset, filt, SNPCall, GenoCall, emp) %>% summarise(mean_diff = mean(diff(rf)), 
                                                                                        se_diff = sd(diff(rf))/sqrt(length(diff(rf))),
                                                                                        n_mks = n(),
                                                                                        total_size = rf[length(rf)])

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

colors.p <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442")
names(colors.p) <- legend_text

data_plot$dataset <- legend_text[match(data_plot$dataset, names(legend_text))]
data_plot2$dataset <- legend_text[match(data_plot2$dataset, names(legend_text))]
data_types$dataset <- legend_text2[match(data_types$dataset, names(legend_text2))]

### Marker types
size_plot1 <- data_types %>% filter(GenoCall != "GUSMap" & emp == "rose") %>% ggplot(aes(x=type, color = dataset)) + 
  geom_bar(aes(y = n_mks, fill = dataset), stat= "identity") + 
  facet_grid(GenoCall ~ SNPCall + filt) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        legend.position = "right", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text.y =  element_text(size = 5),
        strip.text.x =  element_text(size = 4.8))  + ylab("Number of markers") +
  guides(fill = guide_legend(nrow=4, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Rose")

ggsave(size_plot1, filename = "marker_type_rose.png", width = 170, height = 225, units = "mm")

size_plot1 <- data_types %>% filter(GenoCall != "GUSMap" & emp == "aspen") %>% ggplot(aes(x=type, color = dataset)) + 
  geom_bar(aes(y = n_mks, fill = dataset), stat= "identity") + 
  facet_grid(GenoCall ~ SNPCall + filt) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        legend.position = "right", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text.y =  element_text(size = 5),
        strip.text.x =  element_text(size = 4.8))  + ylab("Number of markers") +
  guides(fill = guide_legend(nrow=4, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Aspen")

ggsave(size_plot1, filename = "marker_type_aspen.png", width = 170, height = 225, units = "mm")

size_plot1 <- data_types %>% filter(GenoCall != "GUSMap" & emp == "aspen_cont") %>% ggplot(aes(x=type, color = dataset)) + 
  geom_bar(aes(y = n_mks, fill = dataset), stat= "identity") + 
  facet_grid(GenoCall ~ SNPCall + filt) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        legend.position = "right", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text.y =  element_text(size = 5),
        strip.text.x =  element_text(size = 4.8))  + ylab("Number of markers") +
  guides(fill = guide_legend(nrow=4, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Aspen with contaminants")

ggsave(size_plot1, filename = "marker_type_aspen_cont.png", width = 170, height = 225, units = "mm")


#### Without GUSMap
size_plot1 <- data_plot2 %>% filter(GenoCall != "GUSMap" & emp == "rose") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_boxplot(aes(y = diff, fill= dataset, alpha=0.5), position = position_dodge(width = 1), outlier.size = 0.8) +
  geom_bar(aes(y = n_mks/1.3, fill = dataset), 
           alpha = 0.001, stat= "identity", position = position_dodge(width = 1)) + 
  #scale_y_break(c(25, 110)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1.3, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 7))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Rose")

ggsave(size_plot1, filename = "dist_rose.png", width = 170, height = 225, units = "mm")

size_plot1 <- data_plot2 %>% filter(filt == "no extra filters" & emp == "rose") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_boxplot(aes(y = diff, fill= dataset, alpha=0.5), position = position_dodge(width = 1), outlier.size = 0.8) +
  geom_bar(aes(y = n_mks/1.3, fill = dataset), 
           alpha = 0.001, stat= "identity", position = position_dodge(width = 1)) + 
  #scale_y_break(c(25, 110)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1.3, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 7))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Rose")

ggsave(size_plot1, filename = "dist_gusmap_rose.png", width = 170, height = 100, units = "mm")


size_plot1 <- data_plot2 %>% filter(GenoCall != "GUSMap" & emp == "aspen") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_boxplot(aes(y = diff, fill= dataset, alpha=0.5), position = position_dodge(width = 1), outlier.size = 0.8) +
  geom_bar(aes(y = n_mks/3, fill = dataset), 
           alpha = 0.001, stat= "identity", position = position_dodge(width = 1)) + 
  #scale_y_break(c(25, 110)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*3, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 7))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Aspen")

ggsave(size_plot1, filename = "dist_aspen.png", width = 170, height = 225, units = "mm")

size_plot1 <- data_plot2 %>% filter(filt == "no extra filters" & emp == "aspen") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_boxplot(aes(y = diff), position = position_dodge(width = 1), outlier.size = 0.8) +
  geom_bar(aes(y = n_mks/1.5, fill = dataset), 
           alpha = 0.001, stat= "identity", position = position_dodge(width = 1)) + 
  #scale_y_break(c(25, 110)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1.5, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 7))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Aspen")

ggsave(size_plot1, filename = "dist_gusmap_aspen.png", width = 170, height = 100, units = "mm")

size_plot1 <- data_plot2 %>% filter(GenoCall != "GUSMap" & emp == "aspen_cont") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_boxplot(aes(y = diff, fill= dataset, alpha=0.5), position = position_dodge(width = 1), outlier.size = 0.8) +
  geom_bar(aes(y = n_mks/3, fill = dataset), 
           alpha = 0.001, stat= "identity", position = position_dodge(width = 1)) + 
  #scale_y_break(c(25, 110)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*3, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 7))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Aspen with contaminants")

ggsave(size_plot1, filename = "dist_aspen_cont.png", width = 170, height = 225, units = "mm")

size_plot1 <- data_plot2 %>% filter(filt == "no extra filters" & emp == "aspen_cont") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_boxplot(aes(y = diff), position = position_dodge(width = 1), outlier.size = 0.8) +
  geom_bar(aes(y = n_mks/1, fill = dataset), 
           alpha = 0.001, stat= "identity", position = position_dodge(width = 1)) + 
  #scale_y_break(c(25, 110)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 7))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5), alpha = "none") + ggtitle("Aspen with contaminants")

ggsave(size_plot1, filename = "dist_gusmap_aspen_cont.png", width = 170, height = 100, units = "mm")


## mean diff
size_plot1 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "rose") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/25, fill = dataset), alpha = 0.3, stat= "identity", position = position_dodge()) + 
  geom_point(aes(y = mean_diff), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(25, 110)) +
  geom_errorbar(
    aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*25, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Rose")

#data_plot_temp <- data_plot[which(data_plot$total_size < 2000),]
size_plot2 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "aspen") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/10, fill = dataset), alpha = 0.3, stat= "identity", position = position_dodge()) + 
  geom_point(aes(y = mean_diff), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(40, 85)) +
  geom_errorbar(
    aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="# of markers"),
    limits = c(0, 125)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen")

size_plot3 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "aspen_cont") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/10, fill = dataset), alpha = 0.3, stat= "identity", position = position_dodge()) + 
  geom_point(aes(y = mean_diff), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(20, 85)) +
  geom_errorbar(
    aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="# of markers"),
    limits = c(0, 125)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen with contaminants")

ggsave(size_plot1, filename = "size_emp_plot1.png", width = 170, height = 112.5, units = "mm")
ggsave(size_plot2, filename = "size_emp_plot2.png", width = 170, height = 112.5, units = "mm")
ggsave(size_plot3, filename = "size_emp_plot3.png", width = 170, height = 112.5, units = "mm")

#### With GUSMap
#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
size_plot1 <- data_plot %>% filter(emp == "rose" & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/5, fill = dataset), alpha = 0.3, stat= "identity", position = position_dodge()) + 
  geom_point(aes(y = mean_diff), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(25, 110)) +
  geom_errorbar(
    aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*5, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall, scales = "free_y") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Rose")

#data_plot_temp <- data_plot[which(data_plot$total_size < 2000),]
size_plot2 <- data_plot %>% filter(emp == "aspen" & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/10, fill = dataset), alpha = 0.3, stat= "identity", position = position_dodge()) + 
  geom_point(aes(y = mean_diff), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(40, 85)) +
  geom_errorbar(
    aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="# of markers"),
    limits = c(0, 125)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen")

size_plot3 <- data_plot %>% filter(emp == "aspen_cont" & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks/10, fill = dataset), alpha = 0.3, stat= "identity", position = position_dodge()) + 
  geom_point(aes(y = mean_diff), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(20, 85)) +
  geom_errorbar(
    aes(ymin = mean_diff-se_diff, ymax = mean_diff+se_diff),
    width = 0.1,
    linetype = "solid",
    position=position_dodge(width=0.9)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean distance between adjacent markers (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="# of markers"),
    limits = c(0, 125)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + 
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen with contaminants")


ggsave(size_plot1, filename = "size_emp_plot1_gusmap.png", width = 170, height = 80, units = "mm")
ggsave(size_plot2, filename = "size_emp_plot2_gusmap.png", width = 170, height = 80, units = "mm")
ggsave(size_plot3, filename = "size_emp_plot3_gusmap.png", width = 170, height = 80, units = "mm")

## Without gusmap

total_size_plot1 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "rose") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks*2, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(20, 85)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./2, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 6))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Rose")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot2 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "aspen") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks*25, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(4000, 5500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./25, name="# of markers"),
    limits = c(0, 92000)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 6))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot3 <- data_plot %>% filter(GenoCall != "GUSMap" & emp == "aspen_cont") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks*25, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(10000, 65500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./25, name="# of markers"),
    limits = c(0, 92000)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=7),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 6))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen with contaminants")


ggsave(total_size_plot1, filename = "total_size_emp_plot1.png", width = 170, height = 225, units = "mm")
ggsave(total_size_plot2, filename = "total_size_emp_plot2.png", width = 170, height = 225, units = "mm")
ggsave(total_size_plot3, filename = "total_size_emp_plot3.png", width = 170, height = 225, units = "mm")


## With gusmap
total_size_plot1 <- data_plot %>% filter(emp == "rose"  & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks*5, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(20, 85)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./5, name="# of markers")
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Rose")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot2 <- data_plot %>% filter(emp == "aspen"  & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks*25, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(4000, 5500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./25, name="# of markers"),
    limits = c(0, 93000)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen")

#data_plot_temp <- data_plot[which(data_plot$total_size < 1000),]
total_size_plot3 <- data_plot %>% filter(emp == "aspen_cont"  & filt == "no extra filters") %>% ggplot(aes(x=GenoCall, color = dataset)) + 
  geom_bar(aes(y = n_mks*25, fill = dataset), alpha = 0.3, position="stack", stat="identity") + 
  geom_point(aes(y = total_size), size = 1, position=position_dodge(width=0.9)) +
  #scale_y_break(c(10000, 65500)) +
  scale_color_manual(values = colors.p) +
  scale_fill_manual(values = colors.p) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Total size (cM)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./25, name="# of markers"),
    limits = c(0, 93000)
  ) +
  facet_grid(filt ~ SNPCall) + 
  theme_bw()  + geom_hline(yintercept = 38, color="red") +
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        axis.text =  element_text(size=6),
        axis.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5))  +
  guides(color = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + ggtitle("Aspen with contaminants")


ggsave(total_size_plot1, filename = "total_size_emp_plot_gusmap1.png", width = 170, height = 112.5, units = "mm")
ggsave(total_size_plot2, filename = "total_size_emp_plot_gusmap2.png", width = 170, height = 120, units = "mm")
ggsave(total_size_plot3, filename = "total_size_emp_plot_gusmap3.png", width = 170, height = 112.5, units = "mm")
