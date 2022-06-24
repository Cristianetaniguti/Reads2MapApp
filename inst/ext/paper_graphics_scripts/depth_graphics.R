data.gz <- c(system.file("ext", "simu_results/depth10/biallelics/SimulatedReads_results_depth10_seed8085.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))


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
      name_temp <- unlist(strsplit(datas[[i]][[j]], "/"))
      name_temp <- unlist(strsplit(name_temp[length(name_temp)], "[.]"))[1]
      assign(name_temp, rbind(base::get(name_temp), temp1))
    }
  }
}

data <- data.frame(data1_depths_geno_prob)
data <- data[which(data$seed %in% c(63,77)),]
data <- perfumaria(data)
if(dim(data)[1] == 0) stop(safeError("This marker does not exists in this dataset."))

colors <- c("gray", "#009E73", "#0072B2", "#D55E00", "#F0E442", "#E69F00")
names(colors) <-  c("missing", 
                    "Est=homozygous | True=homozygous", "Est=heterozygous | True=heterozygous", 
                    "Est=heterozygous | True=homozygous", "Est=homozygous | True=heterozygous",
                    "Est=homozygous-ref/alt | True=homozygous-alt/ref")

data$pop <- "progeny"
data$pop[data$ind %in% c("P1", "P2")] <- "parents"

data$alt <- as.numeric(data$alt)
data$gt.onemap.alt.ref <- as.factor(data$gt.onemap.alt.ref)

# True=homozygous | Est=heterozygous
# True=homozygous | Est=homozygous
# True=heterozygous | Est=homozygous
# True=heterozygous | Est=heterozygous

data$graph.color <- NA
data$graph.color[which(data$gabGT == data$gt.onemap.alt.ref & grepl("homozygous", data$gabGT))] <- "Est=homozygous | True=homozygous"
data$graph.color[which(grepl("heterozygous", data$gt.onemap.alt.ref) & grepl("homozygous", data$gabGT))] <- "Est=heterozygous | True=homozygous"
data$graph.color[which(data$gabGT == data$gt.onemap.alt.ref & grepl("heterozygous", data$gabGT))] <- "Est=heterozygous | True=heterozygous"
data$graph.color[which(grepl("homozygous", data$gt.onemap.alt.ref) & grepl("heterozygous", data$gabGT))] <- "Est=homozygous | True=heterozygous"
data$graph.color[which((data$gabGT == "homozygous-ref" & data$gt.onemap.alt.ref == "homozygous-alt") | data$gabGT == "homozygous-alt" & data$gt.onemap.alt.ref == "homozygous-ref")] <- "Est=homozygous-ref/alt | True=homozygous-alt/ref"
data$graph.color[which(data$gt.onemap.alt.ref == "missing")] <- "missing"

labels_df <- data %>% group_by(SNPCall, depth, CountsFrom, GenoCall, graph.color, pop) %>% 
  summarise(cnt = n()) %>%  group_by(SNPCall, depth, CountsFrom, GenoCall, pop) %>%
  mutate(freq = round((cnt / sum(cnt))*100, 2)) %>% pivot_wider(names_from = pop, values_from = freq) 

parents <- labels_df[,-c(6,8)]
progeny <- labels_df[,-c(6,7)]
parents <- parents[-which(is.na(parents$parents)),]
progeny <- progeny[-which(is.na(progeny$progeny)),]

labels_df <- full_join(parents, progeny)

labels_df$parents[which(is.na(labels_df$parents))] <- 0
labels_df$progeny[which(is.na(labels_df$progeny))] <- 0

labels_df$perc <- paste0(labels_df$parents, "% ",labels_df$progeny, "%")

labels_df <- labels_df[order(labels_df$depth, labels_df$SNPCall, labels_df$CountsFrom,  labels_df$GenoCall,  labels_df$graph.color),]
pos <- labels_df %>% group_by(depth, SNPCall,  CountsFrom, GenoCall) %>% summarise(cnt = n()) 
pos <- pos$cnt

max.y <- max(data$alt[which(data$pop == "progeny")], na.rm = T)
max.x <- max(data$ref[which(data$pop == "progeny")], na.rm = T)

pos.y <- list()
for(i in 1:length(pos))
  pos.y[[i]] <- seq(max.y-max.y/12, 1, -max.y/10)[1:max(pos[i])]

labels_df <- cbind(x = max.x - max.x/2.3 , y = unlist(pos.y),labels_df)

df <- data.frame(
  x = c(max.x - max.x/2),
  y = c(max.y),
  text = c("parents  progeny")
)

alpha <- 0.1
software <- c("polyRAD", "updog", "SuperMASSA", "freebayes/GATK")
p <- p_wrong <- list()
for(i in 1:length(software)){
  labels_df_temp <- labels_df %>% filter(GenoCall == software[i])
  
  data_temp <- data %>% filter(GenoCall == software[i])
  subset_data <- data_temp[sample(1:dim(data_temp)[1], dim(data_temp)[1]*0.25),]
  
  p[[i]] <- subset_data %>% filter(pop == "progeny") %>% ggplot(aes(x=ref, y=alt, color=graph.color)) + 
    geom_point(alpha = alpha, size = 1) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values=c(3, 19)) +
    labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
    geom_text(data= labels_df_temp, aes(x,y, label = perc, color = graph.color), 
              inherit.aes = FALSE, show.legend = FALSE, size = 2.2, fontface = "bold") + 
    geom_text(data = df, aes(x,y, label=text), 
              inherit.aes = FALSE, show.legend = FALSE, size = 2.2, fontface = "bold") + 
    facet_grid(depth + CountsFrom ~ SNPCall , scales = "free") +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2), nrow=3), 
           shape = guide_legend(override.aes = list(alpha = 1)))  + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust=1),
          legend.position = "bottom", legend.title = element_blank(), 
          axis.text =  element_text(size=6),
          axis.title = element_text(size=8),
          legend.text = element_text(size=6),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 7))  +
    ggtitle(software[i])
}

p_joint1 <- ggarrange(plotlist = p[1:2], common.legend = TRUE, ncol = 2)
p_joint2 <- ggarrange(plotlist = p[3:4], common.legend = TRUE, ncol = 2)

ggsave(p_joint1, filename = "depth_simu_joint1.png", width = 170, height = 225, units = "mm")
ggsave(p_joint2, filename = "depth_simu_joint2.png", width = 170, height = 225, units = "mm")
ggsave(p[[4]], filename = "depth_simu_joint3.png", width = 85, height = 112.5, units = "mm")

##############################
# With segregation distortion

data.gz <- c(system.file("ext", "simu_results/depth10/biallelics_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
             system.file("ext", "simu_results/depth20/biallelics_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))


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
      name_temp <- unlist(strsplit(datas[[i]][[j]], "/"))
      name_temp <- unlist(strsplit(name_temp[length(name_temp)], "[.]"))[1]
      assign(name_temp, rbind(base::get(name_temp), temp1))
    }
  }
}

data <- data.frame(data1_depths_geno_prob)
data <- data[which(data$seed %in% c(16,67)),]
data <- perfumaria(data)
if(dim(data)[1] == 0) stop(safeError("This marker does not exists in this dataset."))

colors <- c("gray", "#009E73", "#0072B2", "#D55E00", "#F0E442", "#E69F00")
names(colors) <-  c("missing", 
                    "Est=homozygous | True=homozygous", "Est=heterozygous | True=heterozygous", 
                    "Est=heterozygous | True=homozygous", "Est=homozygous | True=heterozygous",
                    "Est=homozygous-ref/alt | True=homozygous-alt/ref")

data$pop <- "progeny"
data$pop[data$ind %in% c("P1", "P2")] <- "parents"

data$alt <- as.numeric(data$alt)
data$gt.onemap.alt.ref <- as.factor(data$gt.onemap.alt.ref)

# True=homozygous | Est=heterozygous
# True=homozygous | Est=homozygous
# True=heterozygous | Est=homozygous
# True=heterozygous | Est=heterozygous

data$graph.color <- NA
data$graph.color[which(data$gabGT == data$gt.onemap.alt.ref & grepl("homozygous", data$gabGT))] <- "Est=homozygous | True=homozygous"
data$graph.color[which(grepl("heterozygous", data$gt.onemap.alt.ref) & grepl("homozygous", data$gabGT))] <- "Est=heterozygous | True=homozygous"
data$graph.color[which(data$gabGT == data$gt.onemap.alt.ref & grepl("heterozygous", data$gabGT))] <- "Est=heterozygous | True=heterozygous"
data$graph.color[which(grepl("homozygous", data$gt.onemap.alt.ref) & grepl("heterozygous", data$gabGT))] <- "Est=homozygous | True=heterozygous"
data$graph.color[which((data$gabGT == "homozygous-ref" & data$gt.onemap.alt.ref == "homozygous-alt") | data$gabGT == "homozygous-alt" & data$gt.onemap.alt.ref == "homozygous-ref")] <- "Est=homozygous-ref/alt | True=homozygous-alt/ref"
data$graph.color[which(data$gt.onemap.alt.ref == "missing")] <- "missing"

labels_df <- data %>% group_by(SNPCall, depth, CountsFrom, GenoCall, graph.color, pop) %>% 
  summarise(cnt = n()) %>%  group_by(SNPCall, depth, CountsFrom, GenoCall, pop) %>%
  mutate(freq = round((cnt / sum(cnt))*100, 2)) %>% pivot_wider(names_from = pop, values_from = freq) 

parents <- labels_df[,-c(6,8)]
progeny <- labels_df[,-c(6,7)]
parents <- parents[-which(is.na(parents$parents)),]
progeny <- progeny[-which(is.na(progeny$progeny)),]

labels_df <- full_join(parents, progeny)

labels_df$parents[which(is.na(labels_df$parents))] <- 0
labels_df$progeny[which(is.na(labels_df$progeny))] <- 0

labels_df$perc <- paste0(labels_df$parents, "% ",labels_df$progeny, "%")

labels_df <- labels_df[order(labels_df$depth, labels_df$SNPCall, labels_df$CountsFrom,  labels_df$GenoCall,  labels_df$graph.color),]
pos <- labels_df %>% group_by(depth, SNPCall,  CountsFrom, GenoCall) %>% summarise(cnt = n()) 
pos <- pos$cnt

max.y <- max(data$alt[which(data$pop == "progeny")], na.rm = T)
max.x <- max(data$ref[which(data$pop == "progeny")], na.rm = T)

pos.y <- list()
for(i in 1:length(pos))
  pos.y[[i]] <- seq(max.y-max.y/12, 1, -max.y/10)[1:max(pos[i])]

labels_df <- cbind(x = max.x - max.x/2.3 , y = unlist(pos.y),labels_df)

df <- data.frame(
  x = c(max.x - max.x/2),
  y = c(max.y),
  text = c("parents  progeny")
)

alpha <- 0.1
software <- c("polyRAD", "updog", "SuperMASSA", "freebayes/GATK")
p <- p_wrong <- list()
for(i in 1:length(software)){
  labels_df_temp <- labels_df %>% filter(GenoCall == software[i])
  
  data_temp <- data %>% filter(GenoCall == software[i])
  subset_data <- data_temp[sample(1:dim(data_temp)[1], dim(data_temp)[1]*0.25),]
  
  p[[i]] <- subset_data %>% filter(pop == "progeny") %>% ggplot(aes(x=ref, y=alt, color=graph.color)) + 
    geom_point(alpha = alpha, size = 1) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values=c(3, 19)) +
    labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
    geom_text(data= labels_df_temp, aes(x,y, label = perc, color = graph.color), 
              inherit.aes = FALSE, show.legend = FALSE, size = 2.2, fontface = "bold") + 
    geom_text(data = df, aes(x,y, label=text), 
              inherit.aes = FALSE, show.legend = FALSE, size = 2.2, fontface = "bold") + 
    facet_grid(depth + CountsFrom ~ SNPCall , scales = "free") +
    guides(colour = guide_legend(override.aes = list(alpha = 1, size = 2), nrow=3), 
           shape = guide_legend(override.aes = list(alpha = 1)))  + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust=1),
          legend.position = "bottom", legend.title = element_blank(), 
          axis.text =  element_text(size=6),
          axis.title = element_text(size=8),
          legend.text = element_text(size=6),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 7))  +
    ggtitle(software[i])
}

p_joint1 <- ggarrange(plotlist = p[1:2], common.legend = TRUE, ncol = 2)
p_joint2 <- ggarrange(plotlist = p[3:4], common.legend = TRUE, ncol = 2)

ggsave(p_joint1, filename = "depth_simu_joint1_dev.png", width = 170, height = 225, units = "mm")
ggsave(p_joint2, filename = "depth_simu_joint2_dev.png", width = 170, height = 225, units = "mm")
ggsave(p[[4]], filename = "depth_simu_joint3_dev.png", width = 85, height = 112.5, units = "mm")


## Empirical

data.gz <- system.file("ext", "rose/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")

path_dir <- tempdir()
list_files <- list()
for(i in 1:length(data.gz)){
  untar(data.gz[i], exdir = path_dir)
  list_files[[i]] <- untar(data.gz[i], list = T)
}

list_files <- lapply(list_files, function(x) file.path(path_dir, x,sep="")) 
list_files <- lapply(list_files, "[", -1)

# Data
datas <- list()
for(i in 1:length(list_files[[1]])){
  datas[[i]] <- sapply(list_files, "[", i)
}

data1 = vroom(datas[[grep("data1_depths_geno_prob.tsv.gz", datas)]])

# Mean depth

parents <- data1[which(data1$ind %in% c("J14-3_s1+s2", "PH_s1+s2+s3")),]
progeny <- data1[which(!(data1$ind %in% c("J14-3_s1+s2", "PH_s1+s2+s3"))),]

summary(parents$alt + parents$ref)
summary(progeny$alt + progeny$ref)


data.gz <- system.file("ext", "populus/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")

path_dir <- tempdir()
list_files <- list()
for(i in 1:length(data.gz)){
  untar(data.gz[i], exdir = path_dir)
  list_files[[i]] <- untar(data.gz[i], list = T)
}

list_files <- lapply(list_files, function(x) file.path(path_dir, x,sep="")) 
list_files <- lapply(list_files, "[", -1)

# Data
datas <- list()
for(i in 1:length(list_files[[1]])){
  datas[[i]] <- sapply(list_files, "[", i)
}

data2 = vroom(datas[[grep("data1_depths_geno_prob.tsv.gz", datas)]])

parents <- data2[which(data2$ind %in% c("PT_M", "PT_F")),]
progeny <- data2[which(!(data2$ind %in% c("PT_M", "PT_F"))),]

summary(parents$alt + parents$ref)
summary(progeny$alt + progeny$ref)


data1 <-cbind(dataset= "Rose", perfumaria(data1))
data2 <- cbind(dataset="Aspen", perfumaria(data2))

data_emp <- rbind(data1, data2)

data_emp$gt.vcf.alt.ref[which(is.na(data_emp$gt.vcf.alt.ref))] <- "missing"

data_emp$pop <- NA
data_emp$pop[which(data_emp$ind == "PH_s1+s2+s3" | data_emp$ind == "J14-3_s1+s2" | data_emp$ind == "PT_M" | data_emp$ind == "PT_F")] <- "parents"
data_emp$pop[-which(data_emp$ind == "PH_s1+s2+s3" | data_emp$ind == "J14-3_s1+s2" | data_emp$ind == "PT_M" | data_emp$ind == "PT_F")] <- "progeny"

labels_df <- data_emp %>% group_by(SNPCall, dataset, CountsFrom, GenoCall, gt.vcf.alt.ref, pop) %>% 
  summarise(cnt = n()) %>%  group_by(SNPCall, dataset, CountsFrom, GenoCall, pop) %>%
  mutate(freq = round((cnt / sum(cnt))*100, 2)) %>% pivot_wider(names_from = pop, values_from = freq) 

parents <- labels_df[,-c(6,8)]
progeny <- labels_df[,-c(6,7)]
parents <- parents[-which(is.na(parents$parents)),]
progeny <- progeny[-which(is.na(progeny$progeny)),]

labels_df <- full_join(parents, progeny)

labels_df$parents[which(is.na(labels_df$parents))] <- 0
labels_df$progeny[which(is.na(labels_df$progeny))] <- 0

labels_df$perc <- paste0(labels_df$parents, "% ",labels_df$progeny, "%")

labels_df <- labels_df[order(labels_df$dataset, labels_df$SNPCall, labels_df$CountsFrom,  labels_df$GenoCall,  labels_df$gt.vcf.alt.ref),]
pos <- labels_df %>% group_by(dataset, SNPCall,  CountsFrom, GenoCall) %>% summarise(cnt = n()) 
pos <- pos$cnt

max.y <- 600
max.x <- 600

pos.y <- list()
for(i in 1:length(pos))
  pos.y[[i]] <- seq(max.y-max.y/15, max.y-max.y/2, -max.y/14)[1:max(pos[i])]

labels_df <- cbind(x = max.x - max.x/3 , y = unlist(pos.y),labels_df)

df <- data.frame(
  x = c(max.x - max.x/3),
  y = c(max.y),
  text = c("parents  progeny")
)

colors_emp <- c("#FFC107", "#D81B60", "#1E88E5", "#004D40")
names(colors_emp) <-  c("missing", 
                        "heterozygous", "homozygous-ref", "homozygous-alt")

software <- c("polyRAD", "updog", "SuperMASSA", "freebayes/GATK")
alpha <- 0.1
p <- list()
for(i in 1:length(software)){
  labels_df_temp <- labels_df %>% filter(GenoCall == software[i])
  
  data_temp <- data_emp %>% filter(GenoCall == software[i])
  subset_data <- data_temp[sample(1:dim(data_temp)[1], dim(data_temp)[1]*0.05),]
  
  p[[i]] <- subset_data %>% ggplot(aes(x=ref, y=alt, color=gt.vcf.alt.ref)) + 
    geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
    scale_color_manual(values = colors_emp) +
    scale_shape_manual(values=c(3, 19)) +
    labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
    geom_text(data= labels_df_temp, aes(x,y, label = perc, color = gt.vcf.alt.ref), 
              inherit.aes = FALSE, show.legend = FALSE, size = 2.5, fontface = "bold") + 
    geom_text(data = df, aes(x,y, label=text), 
              inherit.aes = FALSE, show.legend = FALSE, size = 2.5, fontface = "bold") + 
    facet_grid(dataset~SNPCall + CountsFrom ) +
    xlim(c(0,max.x)) + ylim(c(0,max.y)) + 
    guides(colour = guide_legend(override.aes = list(alpha = 1), nrow=2), 
           shape = guide_legend(override.aes = list(alpha = 1)))  + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust=1),
          legend.position = "bottom", legend.title = element_blank(), 
          axis.text =  element_text(size=6),
          axis.title = element_text(size=8),
          legend.text = element_text(size=6),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 7))  +
    ggtitle(software[i])
}

emp_depth1 <- ggarrange(plotlist = p[1:2], ncol = 1, common.legend = T)
emp_depth2 <- ggarrange(plotlist = p[3:4], ncol = 1, common.legend = T)

ggsave(emp_depth1, filename = "depths_emp1.png", width = 170, height = 225, units = "mm")
ggsave(emp_depth2, filename = "depths_emp2.png", width = 170, height = 225, units = "mm")

