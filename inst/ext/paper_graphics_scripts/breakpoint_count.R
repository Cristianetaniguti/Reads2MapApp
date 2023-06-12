# breakpoint counts
library(largeList)
library(vroom)
library(onemap)
library(tidyverse)

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

data.gz[31] <- "C:/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/simu_results/depth20/multiallelics_filt_GQ_noninfo_dev_replaced/SimulatedReads_results_depth20.tar.gz"

# Big dataset
counts.total <- data.frame()
for(i in 1:length(data.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- file.path(path_dir, list_files)
  path.file <- list_files[grep("sequences", list_files)]
  temp1 <- readList(path.file)
  files.names <-  vroom(list_files[grep("names", list_files)], delim = "\t", num_threads = 5)
  files.names <- as.vector(as.matrix(files.names))
  files.names <- files.names[-grep("gusmap", files.names)]
  
  simu <- list_files[grep("simu_haplo", list_files)]
  simu.df <- vroom(simu, num_threads = 5)
  simu.df$id <- paste0(simu.df$seed, "_", simu.df$depth)
  simu.list <- split(simu.df, simu.df$id)
  prog.simu <- list()
  seed <- vector()
  for(j in 1:length(simu.list)){
    prog.simu.temp <- as.data.frame(simu.list[[j]][,3:10])
    class(prog.simu.temp) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
    prog.simu[[j]] <- progeny_haplotypes_counts2(prog.simu.temp)
  }
  
  # class(prog.simu.temp) <- "data.frame"
  # prog.simu.temp.ind <- prog.simu.temp %>% filter(ind %in% c("F1_001", "F1_002", "F1_003", "F1_004", "F1_005"))
  # class(prog.simu.temp.ind) <- c("onemap_progeny_haplotypes", "outcross", "data.frame", "most.likely")
  # plot(prog.simu.temp.ind)
  # progeny_haplotypes_counts2(prog.simu.temp.ind)
  
  names(prog.simu) <- names(simu.list)
  
  names(temp1) <- files.names
  temp1 <- temp1[grep("FALSE", files.names)]
  files.names <- files.names[grep("FALSE", files.names)]
  for(j in 1:length(temp1)){
    data <- temp1[[j]]
    files.names[j]
    class(data) <- "sequence"
    
    #df <- progeny_haplotypes(data, ind = 1:5, most_likely = T)
    df <- progeny_haplotypes(data, ind = "all", most_likely = T)
    #plot(df)
    
    counts.est <- progeny_haplotypes_counts(df)
    #counts.est2 <- progeny_haplotypes_counts(df2)
    
    # Difference est - simu
    id <- paste0(sapply(strsplit(gsub("map_", "", names(temp1)[j]), "_"), "[[", 1), "_",
                 sapply(strsplit(gsub("map_", "", names(temp1)[j]), "_"), "[[", 2))
    counts.simu <- prog.simu[[match(id, names(prog.simu))]]
    
    names(counts.simu)[4] <- "counts.simu"
    
    counts.diff <- full_join(counts.simu, counts.est, by = c("ind", "homolog"))
    counts.diff$counts[which(is.na(counts.diff$counts))] <- 0
    counts.diff$counts.simu[which(is.na(counts.diff$counts.simu))] <- 0
    counts.diff <- counts.diff %>% group_by(ind) %>% summarise(homolog.sum = sum(counts), count_simu_sum = sum(counts.simu))
    counts.diff$diff <- counts.diff$homolog.sum - counts.diff$count_simu_sum
    neg.error <- sum(counts.diff$diff[which(counts.diff$diff < 0)])
    pos.error <- sum(counts.diff$diff[which(counts.diff$diff > 0)])
    perc.error <- (sum(counts.diff$diff != 0)/length(counts.diff$diff))*100
    
    # counts.diff <- full_join(counts.simu, counts.est, by = c("ind", "homolog"))
    # counts.diff$counts[which(is.na(counts.diff$counts))] <- 0
    # counts.diff$counts.simu[which(is.na(counts.diff$counts.simu))] <- 0
    # counts.diff <- counts.diff %>% group_by(ind) %>% summarise(homolog.sum = sum(counts), count_simu_sum = sum(counts.simu))
    # counts.diff$diff <- counts.diff$homolog.sum - counts.diff$count_simu_sum
    # neg.error <- sum(counts.diff$diff[which(counts.diff$diff < 0)])
    # pos.error <- sum(counts.diff$diff[which(counts.diff$diff > 0)])
    # perc.error <- (sum(counts.diff$diff != 0)/length(counts.diff$diff))*100
    
    if(grepl("multiallelics", data.gz[i])) dataset <- "multi" else dataset <- "bi"
    if(grepl("_dev", data.gz[i])) dataset2 <- "dev" else dataset2 <- "nondev"
    if(grepl("replaced", data.gz[i])) dataset3 <- "filt_GQ_noninfo_replaced" else 
      if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else 
        if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else 
          dataset3 <- "GQ"    
    total_size = cumsum(c(0,kosambi(data$seq.rf)))
    total_size = total_size[length(total_size)]
    counts <- cbind(multi = dataset, segr = dataset2, filt = dataset3, name = files.names[j], neg.error, pos.error, perc.error, total_size = total_size)
    
    counts.total <- rbind(counts.total, counts)
  }
}

#save(counts.total, file = "counts.total_HMM_parents_replaced.RData")
load("counts.total_HMM_parents_replaced.RData")
#load("../counts.total_bugfix.RData")

without_false <- counts.total[grep("FALSE", counts.total$name),]
without_false$seed <- sapply(strsplit(without_false$name, "_"), "[[",2)
without_false$depth <- sapply(strsplit(without_false$name, "_"), "[[",3)
without_false$SNPCall <- sapply(strsplit(without_false$name, "_"), "[[",4)
without_false$CountsFrom <- sapply(strsplit(without_false$name, "_"), "[[",5)
without_false$GenoCall <- sapply(strsplit(without_false$name, "_"), "[[",6)

df <- without_false %>% group_by(depth, SNPCall, CountsFrom, GenoCall, multi, segr, filt) %>% 
  summarise(mean.neg = mean(as.numeric(neg.error), na.rm = T), mean.pos = mean(as.numeric(pos.error), na.rm = T),
            sd.neg = sd(neg.error), sd.pos = sd(pos.error), mean_perc = mean(as.numeric(perc.error)))

df_long1 <- pivot_longer(df, cols = c(8,9))
df_long2 <- pivot_longer(df, cols = c(10,11), names_to =  "names2", values_to = "sd")

df_long <- merge(df_long1, df_long2, by = c(1,2,3,4,5,6,7,10))

df_long <- df_long[-which(df_long$name == "mean.neg" & df_long$names2 == "sd.pos"),]
df_long <- df_long[-which(df_long$name == "mean.pos" & df_long$names2 == "sd.neg"),]

df_long$mean_perc[which(df_long$name == "mean.neg")] <- NA

df_long <- perfumaria(df_long)

df_long$mean_perc <- paste0(round(df_long$mean_perc, 1), "%")
df_long$mean_perc[which(df_long$mean_perc == "NA%")] <- ""

df_long2 <- pivot_longer(without_false, cols = c(5,6), names_to = "signal", values_to = "wrong_breakpoints")

change.name <- c(GQ = "no extra filters",
                 filt_GQ = "genotype prob > 0.8",
                 filt_GQ_noninfo = "genotype prob > 0.8 \n only informative markers",
                 filt_GQ_noninfo_replaced = "genotype prob > 0.8 \n only informative markers \n missing replaced")

df_long2$filt <- change.name[match(df_long2$filt, names(change.name))]

df_long2$filt <- factor(df_long2$filt, levels =  c("no extra filters", 
                                                   "genotype prob > 0.8", 
                                                   "genotype prob > 0.8 \n only informative markers",
                                                   "genotype prob > 0.8 \n only informative markers \n missing replaced"))

df_long2 <- perfumaria(df_long2)

df_long2$GenoCall <- factor(df_long2$GenoCall, levels =  c("polyRAD",
                                                           "polyRAD (5%)",
                                                           "updog",
                                                           "updog (5%)",
                                                           "SuperMASSA",
                                                           "SuperMASSA (5%)",
                                                           "freebayes/GATK",
                                                           "freebayes/GATK (5%)",
                                                           "freebayes/GATK (0.001%)", "GUSMap"))

legend_text <- c(VCF_bi_nondev = "Without segregation distortion\n Only biallelics/ Counts from VCF",
                 BAM_bi_nondev = "Without segregation distortion\n Only biallelics/ Counts from BAM",
                 VCF_multi_nondev = "Without segregation distortion\n With multiallelics/ Counts from VCF",
                 BAM_multi_nondev = "Without segregation distortion\n With multiallelics/ Counts from BAM",
                 VCF_bi_dev = "With segregation distortion\n Only biallelics/ Counts from VCF",
                 BAM_bi_dev = "With segregation distortion\n Only biallelics/ Counts from BAM",
                 VCF_multi_dev = "With segregation distortion\n With multiallelics/ Counts from VCF",
                 BAM_multi_dev = "With segregation distortion\n With multiallelics/ Counts from BAM")

df_long2$dataset <- paste0(df_long2$CountsFrom, "_", df_long2$multi, "_", df_long2$segr)
df_long2$dataset <- legend_text[match(df_long2$dataset, names(legend_text))]

colors.p <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#colors.p <- c("#a9a9a9","#800000", "#f58231", "#ffe119", "#000075", "#4363d8", "#000000", "#dcbeff")
names(colors.p) <- legend_text

find_zeros <- df_long2 %>% group_by(multi, seed, depth, segr, filt, name) %>% summarise(zeros = sum(sqrt(as.numeric(wrong_breakpoints)^2)))

df_mean <- df_long2  %>% group_by(multi, segr, depth, filt, SNPCall, GenoCall, CountsFrom, dataset, signal) %>% summarise(mean_wrong = mean(as.numeric(wrong_breakpoints)))

find_zeros <- df_mean %>% group_by(multi, depth, segr, filt,  SNPCall, GenoCall, CountsFrom, dataset) %>% summarise(zeros = sum(sqrt(as.numeric(mean_wrong)^2)))

idx <- paste0(find_zeros$name[which(round(find_zeros$zeros,4) == 0)],
              find_zeros$multi[which(round(find_zeros$zeros,4) == 0)],
              find_zeros$filt[which(round(find_zeros$zeros,4) == 0)])

df_long2[which(paste0(df_long2$name,df_long2$multi,df_long2$filt) %in% idx),]
df_long2$signal[which(paste0(df_long2$name,df_long2$multi,df_long2$filt) %in% idx)] <- "zero"


zeros <- paste0(
  df_mean$multi[which(round(find_zeros$zeros,0) < 0)],
  df_mean$segr[which(round(find_zeros$zeros,0) < 0)],
  df_mean$depth[which(round(find_zeros$zeros,0) < 0)],
  df_mean$filt[which(round(find_zeros$zeros,0) < 0)],
  df_mean$SNPCall[which(round(find_zeros$zeros,0) < 0)],
  df_mean$GenoCall[which(round(find_zeros$zeros,0) < 0)],
  df_mean$CountsFrom[which(round(find_zeros$zeros,0) < 0)])

temp <- paste0(df_mean$multi,
               df_mean$segr,
               df_mean$depth,
               df_mean$filt,
               df_mean$SNPCall,
               df_mean$GenoCall,
               df_mean$CountsFrom)

if(length(which(temp %in% zeros)) > 0){
  df_mean[which(temp %in% zeros),]
  df_mean$mean_wrong[which(temp %in% zeros)] <- 0
  df_mean$signal[which(temp %in% zeros)] <- "zero"
}

save(df_long2, file = "df_long2.RData")

df_long2$dataset

p1 <- df_long2  %>% filter(depth == "Depth 20") %>% 
  ggplot(aes(x=GenoCall,y= as.numeric(wrong_breakpoints), color = dataset, shape = signal)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.9)) + 
  scale_shape_manual(values = c(6, 17, 19), labels = c("Underestimated breaks", "Overestimated breaks", "all correct")) +
  facet_grid(filt ~ SNPCall) + theme_bw() + geom_hline(yintercept=Inf, color = "black") + 
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  + 
  guides(color = guide_legend(nrow=4), shape = guide_legend(nrow=2)) + ylab("Number of wrong breakpoints")

p1 <- df_mean  %>% filter(depth == "Depth 20") %>% 
  ggplot(aes(x=GenoCall,y= log(sqrt(round(as.numeric(mean_wrong),0)^2), base = 10), color = dataset, shape = signal)) + 
  geom_point(size = 2, position=position_dodge(width=0.9)) + 
  scale_color_manual(values = colors.p) +
  scale_shape_manual(values = c(6, 17, 15), labels = c("Underestimated breaks", "Overestimated breaks", "all correct")) +
  facet_grid(filt ~ SNPCall) + theme_minimal() +  geom_hline(yintercept=-Inf, color = "blue") + 
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4), shape = guide_legend(nrow=2)) + ylab(expression("Log"[10]*" of mean number of wrong breakpoints"))

p1

ggsave(p1, filename = "log10_haplo20.png", width = 170, height = 225, units = "mm")


p <- df_long2  %>% filter(depth == "Depth 10") %>% ggplot(aes(x=GenoCall,y=as.numeric(perc.error), color = dataset)) + 
  geom_point(size = 1.5, position=position_dodge(width=0.9)) + 
  facet_grid(filt ~ SNPCall) + theme_bw() + geom_hline(yintercept=0, color = "black") + 
  theme(axis.text.x = element_text(angle = 30, hjust=1),
        legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=8),
        axis.text = element_text(size=6),
        axis.title.x = element_blank())  +
  guides(color = guide_legend(nrow=4)) + ylab(expression("Log"[10]*" of mean number of wrong breakpoints"))

ggsave(p, filename = "perc_wrong_haplo10_n.png", width = 170, height = 225, units = "mm")


# Empirical
data.gz <- c(system.file("ext", "rose/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/biallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/biallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "rose/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
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
             system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/biallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"))

data.gz <- c("/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/biallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/rose/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_GQ/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_GQ/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/biallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz",
             "/mnt/c/Users/Rose_Lab/Documents/Cris_temp/Reads2MapApp/inst/ext/populus/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz")

max_cores <- 10
# fix -Inf issue
tofix <- list()
idx <- c(9,12,13,14,15,18, 22)
for(i in idx){
  cat(i, "i \n")
  path_dir <- dirname(data.gz[i])
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- paste0(path_dir, "/", list_files)
  path.file <- list_files[grep("sequences", list_files)]
  temp1 <- readList(path.file)
  files.names <-  vroom(list_files[grep("names", list_files)], delim = "\t", num_threads = max_cores)
  files.names <- as.vector(as.matrix(files.names))
  files.names <- files.names[-grep("gusmap", files.names)]
  map_report <- vroom(list_files[grep("data2_maps", list_files)], delim = "\t", num_threads = max_cores)

  tofix[[i]] <- which(sapply(temp1, "[[",4) == -Inf)
  if(length(tofix[[i]]) > 0){
    temp2 <- temp1
    for(z in 1:length(tofix[[i]])){
      map1 <- temp1[tofix[[i]]][[z]]
      twopts <- map1$twopt
      input.seq <- make_seq(twopts, map1$seq.num)

      split.names <- unlist(strsplit(files.names[[tofix[[i]][[z]]]], "_"))
      result <- create_map_report_emp(input.seq,
                                      CountsFrom = split.names[3],
                                      SNPCall = split.names[2],
                                      GenoCall = gsub("0", "", sapply(strsplit(split.names[4], "[.]"), "[[", 1)),
                                      max_cores = 4)
      temp2[tofix[[i]]][[z]] <- result[[1]]
      map_report_temp <- map_report[-which(map_report$CountsFrom == split.names[3] &
                                       map_report$SNPCall == split.names[2] &
                                       map_report$GenoCall == gsub("0", "", sapply(strsplit(split.names[4], "[.]"), "[[", 1))),]
      map_report <- rbind(map_report_temp, result[[2]])
    }
    vroom_write(map_report, paste0(path_dir,"/EmpiricalReads_results/data2_maps.tsv.gz"), num_threads = max_cores)
    saveList(temp2, file = paste0(path_dir, "/EmpiricalReads_results/sequences_emp.llo"), append=FALSE, compress=TRUE)
    if(grepl("_cont.tar.gz", data.gz[i])) name.file <- "/EmpiricalReads_results_cont_bugfix.tar.gz " else name.file <- "/EmpiricalReads_results_bugfix.tar.gz "
    system(paste0("tar -czvf ", path_dir,name.file, path_dir,"/EmpiricalReads_results"))
  }
  system(paste0("rm -r ", path_dir,"/EmpiricalReads_results"))
}

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

prog_counts_total <- data.frame()
for(i in 1:length(data.gz)){
  cat(i, "i \n")
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data.gz[i], exdir = path_dir)
  list_files <- untar(data.gz[i], list = T)
  list_files <- file.path(path_dir, list_files)
  path.file <- list_files[grep("sequences", list_files)]
  temp1 <- readList(path.file)
  files.names <-  vroom(list_files[grep("names", list_files)], delim = "\t", num_threads = 5)
  files.names <- as.vector(as.matrix(files.names))
  files.names <- files.names[-grep("gusmap", files.names)]
  
  if(grepl("rose/", data.gz[i])) dataset2 <- "rose" else if(grepl("_cont", data.gz[i])) dataset2 <- "aspen_cont" else dataset2 <- "aspen"
  cat(dataset2)
  if(grepl("multiallelics", data.gz[i])) dataset <- "multi" else dataset <- "bi"
  if(grepl("replaced", data.gz[i])) dataset3 <- "filt_GQ_noninfo_replaced" else 
    if(grepl("_filt_GQ_noninfo", data.gz[i])) dataset3 <- "filt_GQ_noninfo" else 
      if(grepl("_filt_GQ", data.gz[i])) dataset3 <- "filt_GQ" else 
        dataset3 <- "GQ"  
  for(j in 1:length(temp1)){
    cat(j, "j \n")
    data <- temp1[[j]]
    files.names[j]
    class(data) <- "sequence"
    prog <- progeny_haplotypes(data, ind = "all", most_likely = TRUE)
    prog_counts <- progeny_haplotypes_counts(x = prog)
    prog_counts <- cbind(dataset = dataset2, multi = dataset, filt = dataset3, file = files.names[[j]], prog_counts)
    prog_counts_total <- rbind(prog_counts_total, prog_counts)
  }
}

#save(prog_counts_total, file = "prog_counts_total.RData")

#load("prog_counts_total.RData")
x <- prog_counts_total

change.name <- c(GQ = "no extra filters",
                 filt_GQ = "genotype prob > 0.8",
                 filt_GQ_noninfo = "+ only informative markers",
                 filt_GQ_noninfo_replaced = "+ missing replaced")

x$filt <- change.name[match(x$filt, names(change.name))]

x$filt <- factor(x$filt, levels =  c("no extra filters", 
                                     "genotype prob > 0.8", 
                                     "+ only informative markers",
                                     "+ missing replaced"))

x$GenoCall <- gsub(".RData","",sapply(strsplit(x$file, "_"), "[[", 4))
x$SNPCall <- sapply(strsplit(x$file, "_"), "[[", 2)
x$CountsFrom <- sapply(strsplit(x$file, "_"), "[[", 3)

x <- perfumaria(x)

x$GenoCall <- factor(x$GenoCall, levels =  c("polyRAD",
                                             "polyRAD (5%)",
                                             "updog",
                                             "updog (5%)",
                                             "SuperMASSA",
                                             "SuperMASSA (5%)",
                                             "freebayes/GATK",
                                             "freebayes/GATK (5%)",
                                             "freebayes/GATK (0.001%)", "GUSMap"))

legend_text <- c(VCF_bi = "Only biallelics/ Counts from VCF",
                 BAM_bi = "Only biallelics/ Counts from BAM",
                 VCF_multi = "With multiallelics/ Counts from VCF",
                 BAM_multi = "With multiallelics/ Counts from BAM")

x$dataset2 <- paste0(x$CountsFrom, "_", x$multi)
x$dataset2 <- legend_text[match(x$dataset2, names(legend_text))]

colors.p <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(colors.p) <- legend_text

library(RColorBrewer)

p <- list()
n.ind <- length(unique(x$ind))
nb.cols <- n.ind
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)
set.seed(20)
mycolors <- sample(mycolors)

x2 <- x %>% ungroup %>% group_by(ind, grp, dataset, dataset2 , multi, filt, file, GenoCall, SNPCall, CountsFrom) %>%
  summarise(counts = sum(counts))

save(x2, file = "counts_emp.RData")

p <- x2 %>% filter(SNPCall == "GATK" & 
                     GenoCall == "polyRAD (5%)" & 
                     dataset %in%  c("aspen_cont", "aspen") & 
                     dataset2 == "Only biallelics/ Counts from VCF") %>% 
  ggplot(aes(x=ind, y=counts)) +
  geom_bar(stat="identity") + coord_flip() +
  facet_grid(. ~ dataset, 
             labeller = labeller(dataset = c("aspen" = "Without contaminants", "aspen_cont" = "With contaminants")))+
  theme_bw() +
  theme(legend.position = "right", legend.title = element_blank(), 
        axis.text.y =  element_text(size=2.5),
        axis.title = element_text(size=8),
        legend.text = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(size = 5),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))  +
  labs(fill="groups")

ggsave(p, filename = "breakcounts_emp_polyrad0.05_bi_vcf_gatk.png", width = 170, height = 112.5, units = "mm")

software <- as.character(unique(x2$GenoCall))

# Aspen issue

p_aspen <- x2 %>% filter( GenoCall == "freebayes/GATK" & 
                            dataset %in%  c("aspen") &
                            filt == "+ only informative markers" &
                            SNPCall == "freebayes" &
                            dataset2 == "With multiallelics/ Counts from VCF")  %>% 
  ggplot(aes(x=ind, y=counts, fill = dataset2)) +
  scale_fill_manual(values = c("#009E73")) +
  geom_bar(stat="identity") + coord_flip() +
  facet_grid(. ~ SNPCall + GenoCall+ dataset, 
             labeller = labeller(dataset = c("aspen" = "Without contaminants", "aspen_cont" = "With contaminants")))+
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.y =  element_text(size=2.5),
        axis.title = element_text(size=8),
        legend.text = element_text(size=8),
        strip.text = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        title = element_text(size = 8))  +
  guides(fill = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + 
  ylab("breakpoint counts")

# Aspen
p_aspen <- x2 %>% filter( GenoCall == "updog" & 
                            dataset %in%  c("aspen_cont", "aspen") &
                            filt == "+ only informative markers" &
                            SNPCall == "freebayes" &
                            dataset2 == "With multiallelics/ Counts from BAM")  %>% 
  ggplot(aes(x=ind, y=counts, fill = dataset2)) +
  scale_fill_manual(values = c("#009E73")) +
  geom_bar(stat="identity") + coord_flip() +
  facet_grid(. ~ SNPCall + GenoCall+ dataset, 
             labeller = labeller(dataset = c("aspen" = "Without contaminants", "aspen_cont" = "With contaminants")))+
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.y =  element_text(size=2.5),
        axis.title = element_text(size=8),
        legend.text = element_text(size=8),
        strip.text = element_text(size = 7),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7),
        title = element_text(size = 8))  +
  guides(fill = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + 
  ylab("breakpoint counts")

ggsave(p_aspen, filename = "breakcounts_aspen1.png", width = 100, height = 225, units = "mm")


p_rose <- x2 %>% filter( GenoCall == "freebayes/GATK (5%)" & 
                           dataset %in%  c("rose") &
                           filt == "genotype prob > 0.8" &
                           SNPCall == "GATK" &
                           dataset2 == "Only biallelics/ Counts from VCF")  %>% 
  ggplot(aes(x=ind, y=counts, fill = dataset2)) +
  scale_fill_manual(values = c("#E69F00")) +
  geom_bar(stat="identity") + coord_flip() +
  facet_grid(. ~ SNPCall + GenoCall)+
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.y =  element_text(size=2.5),
        axis.title = element_text(size=8),
        legend.text = element_text(size=8),
        strip.text = element_text(size = 6),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
        title = element_text(size = 8))  +
  guides(fill = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + 
  ylab("breakpoint counts")

ggsave(p_rose, filename = "breakcounts_rose3.png", width = 60, height = 225, units = "mm")


p <- list()
for(i in 1:length(software)){
  p[[i]] <- x2 %>% filter( GenoCall == software[i] & 
                             dataset %in%  c("aspen_cont", "aspen"))  %>% 
    ggplot(aes(x=ind, y=counts, fill = dataset2)) +
    geom_bar(stat="identity") + coord_flip() +
    facet_grid(. ~ SNPCall + dataset, 
               labeller = labeller(dataset = c("aspen" = "Without contaminants", "aspen_cont" = "With contaminants")))+
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), 
          axis.text.y =  element_text(size=2.5),
          axis.title = element_text(size=8),
          legend.text = element_text(size=8),
          strip.text = element_text(size = 4.5),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
          title = element_text(size = 8))  +
    guides(fill = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + 
    ggtitle(software[i]) + ylab("breakpoint counts")
}

p_joint <- ggarrange(plotlist = p[1:2], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_aspen1.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[3:4], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_aspen2.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[5:6], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_aspen3.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[7:8], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_aspen4.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[9], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_emp5.png", width = 170, height = 112.5, units = "mm")


# Rose

p <- list()
for(i in 1:length(software)){
  p[[i]] <- x2 %>% filter( GenoCall == software[i] & 
                             dataset %in%  c("rose"))  %>% 
    ggplot(aes(x=ind, y=counts, fill = dataset2)) +
    geom_bar(stat="identity") + coord_flip() +
    facet_grid(. ~ SNPCall + dataset, 
               labeller = labeller(dataset = c("rose" = "Roses")))+
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), 
          axis.text.y =  element_text(size=2.5),
          axis.title = element_text(size=8),
          axis.title.y = element_blank(),
          legend.text = element_text(size=8),
          strip.text = element_text(size = 4.5),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5),
          title = element_text(size = 8))  +
    guides(fill = guide_legend(nrow=2, keywidth = 0.5, keyheight = 0.5)) + 
    ggtitle(software[i]) + ylab("breakpoint count")
  
}

p_joint <- ggarrange(plotlist = p[1:2], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_rose1.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[3:4], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_rose2.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[5:6], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_rose3.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[7:8], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_rose4.png", width = 170, height = 112.5, units = "mm")

p_joint <- ggarrange(plotlist = p[9], common.legend = T, ncol = 2, nrow = 1)
ggsave(p_joint, filename = "breakcounts_rose5.png", width = 170, height = 112.5, units = "mm")

