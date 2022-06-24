# Packages
library(vroom)
library(tidyverse)
library(vcfR)
library(tidymodels)

# Dataset list

data.gz <- c(system.file("ext", "simu_results/depth10/biallelics/SimulatedReads_results_depth10_seed8085.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/biallelics_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth10/biallelics_filt_GQ/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth20/biallelics_filt_GQ/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8296.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8297.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20_seed8295.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             #system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10_seed8294.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics/SimulatedReads_results_depth20_seed8082.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics/SimulatedReads_results_depth10_seed8084.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/multiallelics_dev/SimulatedReads_results_depth20_seed8080.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth10/multiallelics_dev/SimulatedReads_results_depth10_seed8081.tar.gz", package = "Reads2MapApp"))
#system.file("ext", "simu_results/depth20/multiallelics_filt_GQ/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth10/multiallelics_filt_GQ/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8299.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8300.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20_seed8303.tar.gz", package = "Reads2MapApp"),
#system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))

# ROC or Precision recall curves

# Big dataset
df_total.polyrad <- df_total.updog <- df_total.supermassa <- df_total.SNPCaller <- data.frame()
for(i in 1:length(data.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- file.path(path_dir, list_files)
  path.file <- list_files[grep("data1_depths", list_files)]
  df <- vroom(path.file, num_threads = 15)
  df.polyrad <- df %>% filter(GenoCall == "polyrad")
  df.updog <- df %>% filter(GenoCall == "updog")
  df.supermassa <- df %>% filter(GenoCall == "supermassa")
  df.SNPCaller <- df %>% filter(GenoCall == "SNPCaller")
  if(grepl("multiallelics", data.gz[i])) dataset <- "multi" else dataset <- "bi"
  if(grepl("_dev", data.gz[i])) dataset2 <- "dev" else dataset2 <- "nondev"
  df.polyrad <- cbind(data=dataset, segr = dataset2, df.polyrad)
  df.updog <- cbind(data=dataset, segr = dataset2, df.updog)
  df.supermassa <- cbind(data=dataset, segr = dataset2, df.supermassa)
  df.SNPCaller <- cbind(data=dataset, segr = dataset2, df.SNPCaller)
  df_total.polyrad <- rbind(df_total.polyrad, df.polyrad)
  df_total.updog <- rbind(df_total.updog, df.updog)
  df_total.supermassa <- rbind(df_total.supermassa, df.supermassa)
  df_total.SNPCaller <- rbind(df_total.SNPCaller, df.SNPCaller)
}

vroom_write(df_total.polyrad, file = "data1_polyrad.tsv.gz", num_threads = 5)
vroom_write(df_total.updog, file = "data1_updog.tsv.gz", num_threads = 5)
vroom_write(df_total.supermassa, file = "data1_supermassa.tsv.gz", num_threads = 5)
vroom_write(df_total.SNPCaller, file = "data1_SNPCaller.tsv.gz", num_threads = 5)

# polyrad

softwares <- c(polyRAD = "data1_polyrad.tsv.gz",
               updog = "data1_updog.tsv.gz",
               SuperMASSA = "data1_supermassa.tsv.gz",
               SNPCaller = "data1_SNPCaller.tsv.gz")

df_total <- p <- list()
for(z in 1:length(softwares)){
  print(paste("doing", softwares[z]))
  data <- vroom(softwares[z], num_threads = 6)
  
  methods <- expand.grid(snpcall= unique(data$SNPCall),
                         countsfrom = unique(data$CountsFrom), 
                         depth = unique(data$depth),
                         data = unique(data$data),
                         segr = unique(data$segr))
  
  methods <- as.data.frame(apply(methods, 2, as.character))
  data$id <- "non"
  
  # Not comparing alt x ref only het x homo
  data$gabGT[which(data$gabGT == "homozygous-ref" | data$gabGT == "homozygous-alt")] <- "homozygous"
  data$gt.onemap.alt.ref[which(data$gt.onemap.alt.ref == "homozygous-ref" | 
                                 data$gt.onemap.alt.ref == "homozygous-alt" |
                                 data$gt.onemap.alt.ref == "homozygous-alt == ref")] <- "homozygous"
  
  data[which(data$gabGT == data$gt.onemap.alt.ref),]$id <- "correct"
  data[which(data$gabGT != data$gt.onemap.alt.ref),]$id <- "wrong"
  
  if(length(which(data$gt.onemap.alt.ref == "homozygous-alt == ref" | is.na(data$gt.onemap.alt.ref))) > 0){
    data_test <- data[-which(data$gt.onemap.alt.ref == "homozygous-alt == ref" | is.na(data$gt.onemap.alt.ref)),]
  } else data_test <- data
  data_test$gt.onemap.alt.ref <- as.factor(data_test$gt.onemap.alt.ref) 
  data_test$gabGT <- as.factor(data_test$gabGT) 
  
  df_total[[z]] <- data.frame()
  for(i in 1:dim(methods)[1]){
    data1 <- data_test %>% filter(SNPCall == methods$snpcall[i] &
                                    CountsFrom == methods$countsfrom[i] &
                                    depth == methods$depth[i] &
                                    data == methods$data[i] &
                                    segr == methods$segr[i] &
                                    gt.onemap.alt.ref != "missing")
    
    data1$gabGT <- droplevels(data1$gabGT)
    data1$gt.onemap.alt.ref <- droplevels(data1$gt.onemap.alt.ref)
    acc <- accuracy(data1, truth = gabGT, estimate = gt.onemap.alt.ref)
    
    if(all(data1$id == "correct") | all(data1$id == "wrong")){
      forplot <- data.frame(threshold = NA,
                            specificity = NA,
                            sensitivity = NA,
                            precision = NA,
                            recall = NA,
                            auc = NA, 
                            accuracy = acc$.estimate, 
                            perc_wrong = table(data1$id)[2]/sum(table(data1$id))*100,
                            method = paste0(methods[i,c(2,4,5)], collapse = "_"),
                            snpcall = methods$snpcall[i],
                            depth = methods$depth[i],
                            countsfrom = methods$countsfrom[i])
    } else {
      # Precision recall
      # temp_df <- data.frame(id = data1$id, errors = data1$errors)
      
      # temp_df$id <- as.factor(temp_df$id)
      # if(any(is.na(temp_df$errors)))
      #   temp_df <- temp_df[-which(is.na(temp_df$errors)),]
      # df <- pr_curve(temp_df, "id", "errors")
      # auc <- pr_auc(temp_df , "id", "errors")
      # roc <- roc_curve(temp_df, "id", "errors")
      # roc_auc <- roc_auc(temp_df, "id", "errors")
      # 
      # forplot_pr <- data.frame(Recall = df$recall, 
      #                       Precision = df$precision,
      #                       Threshold_pr = df$.threshold,
      #                       auc = auc, 
      #                       perc_wrong = acc, 
      #                       method = paste0(methods[i,c(2,4,5)], collapse = "_"),
      #                       snpcall = methods$snpcall[i],
      #                       depth = methods$depth[i],
      #                       countsfrom = methods$countsfrom[i])
      # 
      # forplot_roc <- data.frame(Specificity = roc$specificity,
      #                       Sensitivity = roc$sensitivity,
      #                       Threshold_roc = roc$.threshold,
      #                       auc = roc_auc, 
      #                       perc_wrong = acc, 
      #                       method = paste0(methods[i,c(2,4,5)], collapse = "_"),
      #                       snpcall = methods$snpcall[i],
      #                       depth = methods$depth[i],
      #                       countsfrom = methods$countsfrom[i])
      
      method <- pROC::roc(response = data1$id, predictor = data1$errors)
      
      auc <- pROC::auc(method)
      
      df <- pROC::coords(method, "all", ret=c("threshold", "specificity", "sensitivity",
                                              "precision", "recall"), transpose = FALSE)
      head(df)
      df$auc <- auc
      df$accuracy <- acc$.estimate
      df$perc_wrong <- table(data1$id)[2]/sum(table(data1$id))*100
      df$method <- paste0(methods[i,c(2,4,5)], collapse = "_")
      df$snpcall <- methods$snpcall[i]
      df$depth <- methods$depth[i]
      df$countsfrom <- methods$countsfrom[i]
    }
    df_total[[z]] <- rbind(df_total[[z]], df)
  }
}

#save(df_total, file = "roc_df_list.RData")

load("roc_df_list.RData")

legend_text <- c(vcf_bi_nondev = "1 - Without segregation \n     distortion \n     Only biallelics \n     Counts from VCF \n",
                 bam_bi_nondev = "2 - Without segregation \n     distortion \n     Only biallelics \n     Counts from BAM \n",
                 vcf_multi_nondev = "3 - Without segregation \n     distortion \n     With multiallelics \n     Counts from VCF \n",
                 bam_multi_nondev = "4 - Without segregation \n     distortion \n     With multiallelics \n     Counts from BAM \n",
                 vcf_bi_dev = "5 - With segregation \n     distortion \n     Only biallelics \n     Counts from VCF \n",
                 bam_bi_dev = "6 - With segregation \n     distortion \n     Only biallelics \n     Counts from BAM \n",
                 vcf_multi_dev = "7 - With segregation \n     distortion \n     With multiallelics \n     Counts from VCF \n",
                 bam_multi_dev = "8 - With segregation \n     distortion \n     With multiallelics \n     Counts from BAM \n")

# Color blind palette
colors.p <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(colors.p) <- legend_text

for(i in 1:length(df_total)){
  df_total[[i]] <- cbind(df_total[[i]], software = names(softwares)[i])
}

df_total <- do.call(rbind, df_total)

df_total$snpcall <- gsub("gatk", "GATK", df_total$snpcall)

df_total$method <- legend_text[match(df_total$method, names(legend_text))]

numb <- sapply(strsplit(df_total$method, " -"), "[",1)

df_total$lab <- paste0("AUC = ", round(df_total$auc,2),", % wrong = ", round(df_total$perc_wrong,2)," (", numb,")")

labels_df <- df_total %>% group_by(snpcall, depth, lab, software) %>% summarise(unique(method))

n_lab <- labels_df %>% group_by(snpcall, depth, software) %>% summarise(n = n())

x <- y <- vector()
for(i in 1:dim(n_lab)[1]){
  x <- c(x, rep(0.6,n_lab$n[i]))
  y <- c(y, c(0.05,0.13,0.21,0.29,0.37,0.45,0.53,0.61)[1:n_lab$n[i]])
}

labels_df <- labels_df[order(labels_df$snpcall, labels_df$depth, labels_df$software),]
labels_df <- cbind(labels_df, x= x, y=y)

labels_df10 <- labels_df %>% filter(depth == 10)
labels_df20 <- labels_df %>% filter(depth == 20)

p10 <- df_total %>% filter(depth == 10) %>% ggplot(aes(x=1-specificity, y=sensitivity, color = method)) +
  geom_line()  +
  scale_color_manual(values = colors.p) +
  geom_text(data= labels_df10, aes(x,y, label = lab, color = `unique(method)`), inherit.aes = FALSE, show.legend = FALSE, size = 2.3) +
  facet_grid(software ~ snpcall) +
  theme_bw() +
  theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(colour = guide_legend(nrow=8, override.aes = list(size=2)))

p20 <- df_total %>% filter(depth == 20) %>% ggplot(aes(x=1-specificity, y=sensitivity, color = method)) +
  geom_line()  +
  scale_color_manual(values = colors.p) +
  geom_text(data= labels_df20, aes(x,y, label = lab, color = `unique(method)`), inherit.aes = FALSE, show.legend = FALSE, size = 2.3) +
  facet_grid(software ~ snpcall) +
  theme_bw() +
  theme(legend.position = "right", legend.title = element_blank(), legend.text = element_text(size=8)) +
  guides(colour = guide_legend(nrow=8, override.aes = list(size=2))) 

ggsave(p10, filename = "roc10.png", width = 170, height = 225, units = "mm")
ggsave(p20, filename = "roc20.png", width = 170, height = 225, units = "mm")
ggsave(p10, filename = "roc10.svg", width = 170, height = 225, units = "mm")
ggsave(p20, filename = "roc20.svg", width = 170, height = 225, units = "mm")

rec <- p <- list()
for(z in 1:length(softwares)){
  df_total[[z]]$method <- legend_text[match(df_total[[z]]$method, names(legend_text))]
  
  numb <- sapply(strsplit(df_total[[z]]$method, " -"), "[",1)
  
  df_total[[z]]$lab <- paste0("AUC = ", round(df_total[[z]]$auc,2),", % wrong = ", round(df_total[[z]]$perc_wrong,2)," (", numb,")")
  
  labels_df <- df_total[[z]] %>% group_by(snpcall, depth, lab) %>% summarise(unique(method))
  
  if(names(softwares)[z] != "SNPCaller"){
    labels_df <- cbind(x = rep(0.7,32), y = rep(c(0.05,0.13,0.21,0.29,0.37,0.45,0.53,0.61), 4),labels_df)
  } else 
    labels_df <- cbind(x = rep(0.7,16), y = rep(c(0.05,0.13,0.21,0.29), 4),labels_df)

  p[[z]] <- ggplot(data = df_total[[z]], aes(x=1-specificity, y=sensitivity, color = method)) +
    geom_point() +
    scale_color_manual(values = colors.p) +
    geom_line()  +
    geom_text(data= labels_df, aes(x,y, label = lab, color = `unique(method)`), inherit.aes = FALSE, show.legend = FALSE, size = 2.5) +
    facet_grid(depth ~ snpcall) +
    theme_bw() +
    ggtitle(names(softwares)[z]) +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    guides(colour = guide_legend(nrow=8))
  
  rec[[z]] <- ggplot(data = df_total[[z]], aes(x=precision, y=recall, color = method)) +
    geom_point() +
    scale_color_manual(values = colors.p) +
    geom_line()  +
    geom_text(data= labels_df, aes(x,y, label = lab, color = `unique(method)`), inherit.aes = FALSE, show.legend = FALSE, size = 2.5) + 
    facet_grid(depth ~ snpcall) + 
    theme_bw() +
    geom_abline(intercept = 0, slope = 1) + ggtitle(names(softwares)[z]) +
    theme(legend.position = "bottom", legend.title = element_blank())
}

library(ggpubr)

p_list1 <- ggarrange(plotlist = p[1:2], common.legend = TRUE, ncol = 1, legend = "bottom")
p_list2 <- ggarrange(plotlist = p[3:4], common.legend = TRUE, ncol = 1, legend = "bottom")

rec_list <- ggarrange(plotlist = rec, common.legend = TRUE)

ggsave(p_list1, filename = "roc1.png", width = 170, height = 225, units = "mm")
ggsave(p_list2, filename = "roc2.png", width = 170, height = 225, units = "mm")
