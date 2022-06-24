# Packages
library(vroom)
library(ggVennDiagram)
library(vcfR)

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

# Venn graphic

## Simulations and empirical
# The title needs to have the number of simulated markers
# SNP caller dataset #SNP #MNP time memory*

# This table will have 12*5 repetitions

data20.gz <- data.gz[grep("depth20", data.gz)]
data10.gz <- data.gz[grep("depth10", data.gz)] # I run the code bellow for this two datasets

list_files <- list_pos <- all_toveen <- list()
df_total <- data.frame()
for(i in 1:length(data20.gz)){
  path_dir <- file.path(tempdir(),paste0("/temp", i))
  dir.create(path_dir, showWarnings = FALSE)
  untar(data20.gz[i], exdir = path_dir)
  list_files[[i]] <- untar(data20.gz[i], list = T)
  list_files[[i]] <- file.path(path_dir, list_files[[i]])
  path.file <- list_files[[i]][grep("positions", list_files[[i]])]

  toveen <- list()
  for(j in 1:length(path.file)){
    untar(path.file[j], exdir = dirname(path.file[j]))
    five_files <- list.files(paste0(gsub(".tar.gz", "", path.file[j])))
    five_files <- five_files[-c(1,3)]
    for(z in 1:length(five_files))
      list_pos[[z]] <- vroom(paste0(gsub(".tar.gz", "", path.file[j]), "/", five_files[z]), delim = "\t")
    names(list_pos) <- c("free", "gatk", "true")
    toveen[[j]] <- lapply(list_pos, function(x) as.vector(t(x)))
    df <- data.frame(dataset = "depth10", uniq.freebayes = sum(!(toveen[[j]]$free %in% toveen[[j]]$gatk)  & !(toveen[[j]]$free %in% toveen[[j]]$true)),
                     uniq.gatk = sum(!(toveen[[j]]$gatk %in% toveen[[j]]$free)  & !(toveen[[j]]$gatk %in% toveen[[j]]$true)),
                     uniq.true = sum(!(toveen[[j]]$true %in% toveen[[j]]$gatk)  & !(toveen[[j]]$true %in% toveen[[j]]$free)),
                     free.gatk = sum((toveen[[j]]$free %in% toveen[[j]]$gatk)  & !(toveen[[j]]$free %in% toveen[[j]]$true)),
                     free.true = sum(!(toveen[[j]]$free %in% toveen[[j]]$gatk)  & (toveen[[j]]$free %in% toveen[[j]]$true)),
                     gatk.true = sum(!(toveen[[j]]$gatk %in% toveen[[j]]$free)  & (toveen[[j]]$gatk %in% toveen[[j]]$true)),
                     free.gatk.true = sum((toveen[[j]]$free %in% toveen[[j]]$gatk)  & (toveen[[j]]$free %in% toveen[[j]]$true)))
    df_total <- rbind(df_total, df)
  }
  all_toveen[[i]] <- toveen
}

all_toveen <- unlist(all_toveen, recursive = F)

free <- unlist(lapply(all_toveen, "[",1),recursive = F)
gatk <- unlist(lapply(all_toveen, "[",2),recursive = F)

depth10 <- data.frame(gatk.depth10.mean = mean(sapply(gatk, length)),
                      gatk.depth10.sd = sd(sapply(gatk, length)),
                      gatk.depth10.intersect = length(Reduce(intersect, gatk)),
                      free.depth10.mean = mean(sapply(free, length)),
                      free.depth10.sd = sd(sapply(free, length)),
                      free.depth10.intersect = length(Reduce(intersect, free)))

depth20 <- data.frame(gatk.depth20.mean = mean(sapply(gatk, length)),
                      gatk.depth20.sd = sd(sapply(gatk, length)),
                      gatk.depth20.intersect = length(Reduce(intersect, gatk)),
                      free.depth20.mean = mean(sapply(free, length)),
                      free.depth20.sd = sd(sapply(free, length)),
                      free.depth20.intersect = length(Reduce(intersect, free)))


df_total10 <- df_total
df_total20 <- df_total # second time

# Mean
apply(df_total10[,-1], 2, function(x) mean(x))
apply(df_total20[,-1], 2, function(x) mean(x))

# Standard error
apply(df_total10[,-1], 2, function(x) sd(x))
apply(df_total20[,-1], 2, function(x) sd(x))

# Same graphic for empiricals

## populus
vcf.pop <- read.vcfR("../filtered_populus/gatk_vcf.recode.vcf")
pos.pop.gatk <- paste0(vcf.pop@fix[,1],"_", vcf.pop@fix[,2])
vcf.pop.free <- read.vcfR("../filtered_populus/freebayes_vcf.recode.vcf")
pos.pop.free <- paste0(vcf.pop.free@fix[,1],"_", vcf.pop.free@fix[,2])

toveen <- list(free = pos.pop.free, gatk = pos.pop.gatk)
ggVennDiagram(toveen)

## roses
vcf.rose <- read.vcfR("../filtered_roses/gatk_vcf.recode.vcf")
pos.rose.gatk <- paste0(vcf.rose@fix[,1],"_", vcf.rose@fix[,2])
vcf.rose.free <- read.vcfR("../filtered_roses/freebayes_vcf.recode.vcf")
pos.rose.free <- paste0(vcf.rose.free@fix[,1],"_", vcf.rose.free@fix[,2])

toveen <- list(free = pos.rose.free, gatk = pos.rose.gatk)
ggVennDiagram(toveen)

# Graphic made in inkscape

# * supplementary material for details


