library(tidyverse)
library(vroom)
library(largeList)
library(onemap)
library(ggpubr)

data.gz <- c(system.file("ext", "rose/multiallelics_filt_GQ_noninfo_replaced/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp"))

i <- 1
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

# Roses selected pipes
rose_datas <- rf_graph <- list()
prog_counts_tot <- data.frame()
idx <- c(grep("map_gatk_vcf_SNPCaller0.05.RData", files.names),
         #grep("map_gatk_vcf_SNPCaller.RData", files.names),
         grep("map_gatk_vcf_polyrad0.05.RData", files.names),
         grep("map_freebayes_vcf_SNPCaller0.05.RData", files.names))

data.names <- c("GATK 5%", "GATK-polyRAD 5%", "freebayes 5%")

for(i in 1:length(idx)){
  data <- temp1[[idx[i]]]
  files.names[idx[i]]
  class(data) <- "sequence"
  rose_datas[[i]] <- data
  rf_graph[[i]] <- rf_graph_table(data, mrk.axis = "none") + ggtitle(data.names[i])
  
  prog <- progeny_haplotypes(data, ind = "all", most_likely = TRUE)
  prog_counts <- progeny_haplotypes_counts(x = prog)
  
  prog_counts_temp <- cbind(prog_counts, pipe = data.names[i])
  prog_counts_tot <- rbind(prog_counts_tot, prog_counts_temp)
}

save(rf_graph, file = "rf_graph_rose.RData")
save(rose_datas, file = "rose_datas.RData")
save(prog_counts_tot, file = "prog_counts_tot_rose.RData")

p1 <- ggarrange(plotlist = rf_graph, common.legend = T, ncol = 1)
draw_map2(rose_datas, output = "rose_maps.png", group.names = data.names, cex.label = 3, space = 2.8)

p3 <- prog_counts_tot %>% 
  ggplot(aes(x=ind, y=counts)) +
  geom_bar(stat="identity") + coord_flip() +
  theme_bw() +
  theme(legend.position = "right", 
        legend.title = element_blank(), 
        axis.text.y =  element_text(size=2.5),
        legend.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  labs(fill="groups") + facet_grid(.~pipe)

ggsave(p1, filename = "roses_pipe1.png", width = 100, height = 160, units = "mm")
ggsave(p3, filename = "roses_pipe2.png", width = 140, height = 225, units = "mm")


# Aspen

i <- 2
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

rose_datas <- rf_graph <- list()
prog_counts_tot <- data.frame()
idx <- c(grep("map_gatk_vcf_SNPCaller0.05.RData", files.names),
         #grep("map_gatk_vcf_SNPCaller.RData", files.names),
         grep("map_gatk_vcf_updog.RData", files.names),
         grep("map_freebayes_vcf_polyrad0.05.RData", files.names))

data.names <- c("GATK 5%", "GATK\nupdog", "freebayes\npolyrad 5%")

for(i in 1:length(idx)){
  data <- temp1[[idx[i]]]
  files.names[idx[i]]
  class(data) <- "sequence"
  rose_datas[[i]] <- data
  rf_graph[[i]] <- rf_graph_table(data, mrk.axis = "none") + ggtitle(data.names[i])
  
  prog <- progeny_haplotypes(data, ind = "all", most_likely = TRUE)
  prog_counts <- progeny_haplotypes_counts(x = prog)
  
  prog_counts_temp <- cbind(prog_counts, pipe = data.names[i])
  prog_counts_tot <- rbind(prog_counts_tot, prog_counts_temp)
}

i <- 3
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

idx <- c(grep("map_gatk_vcf_SNPCaller0.05.RData", files.names),
         #grep("map_gatk_vcf_SNPCaller.RData", files.names),
         grep("map_gatk_vcf_updog.RData", files.names),
         grep("map_freebayes_vcf_polyrad0.05.RData", files.names))

data.names <- c("GATK 5%\n(ct)", "GATK\nupdog (ct)", "freebayes\npolyrad 5% (ct)")

for(i in 1:length(idx)){
  data <- temp1[[idx[i]]]
  files.names[idx[i]]
  class(data) <- "sequence"
  rose_datas[[length(idx)+i]] <- data
  rf_graph[[length(idx)+i]] <- rf_graph_table(data, mrk.axis = "none") + ggtitle(data.names[i])
  
  prog <- progeny_haplotypes(data, ind = "all", most_likely = TRUE)
  prog_counts <- progeny_haplotypes_counts(x = prog)
  
  prog_counts_temp <- cbind(prog_counts, pipe = data.names[i])
  prog_counts_tot <- rbind(prog_counts_tot, prog_counts_temp)
}

save(rf_graph, file = "rf_graph_apen.RData")
save(rose_datas, file = "aspen_datas.RData")
save(prog_counts_tot, file = "prog_counts_tot_aspen.RData")

p1 <- ggarrange(plotlist = rf_graph[1:3], common.legend = T, ncol = 1)
ggsave(p1, filename = "aspen_pipe1.png", width = 100, height = 160, units = "mm")

p2 <- ggarrange(plotlist = rf_graph[4:6], common.legend = T, ncol = 1)
ggsave(p2, filename = "aspen_pipe2.png", width = 100, height = 160, units = "mm")

p <- ggarrange(p1, p2, ncol = 2, common.legend = T)
ggsave(p, filename = "aspen_pipe2.png", width = 170, height = 225, units = "mm")


rose_datas_sort <- rose_datas[c(1,4,2,5,3,6)]
data.names <- c("GATK 5%", "GATK-updog", "freebayes-polyrad 5%","GATK 5% (ct)", "GATK-updog (ct)", "freebayes-polyrad 5% (ct)")
data.names <- data.names[c(1,4,2,5,3,6)]
draw_map2(rose_datas_sort, output = "aspen_maps1.png", group.names = data.names, cex.label = 3, space = 4.2)


p3 <- prog_counts_tot %>% 
  ggplot(aes(x=ind, y=counts)) +
  geom_bar(stat="identity") + coord_flip() +
  theme_bw() +
  theme(legend.position = "right", 
        legend.title = element_blank(), 
        axis.text.y =  element_text(size=2.5),
        legend.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  labs(fill="groups") + facet_grid(.~pipe)

ggsave(p3, filename = "aspen_pipe3.png", width = 170, height = 225, units = "mm")
