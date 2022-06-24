library(Reads2MapTools)
library(vcfR)

vcf <- read.vcfR("C:/Users/Rose_Lab/Documents/Cris_temp/Simulations_inputs/gatk_chr10_maxmissing0.75_bi_maf0.05.recode.vcf")
ref_map <- read.csv("C:/Users/Rose_Lab/Documents/Cris_temp/Simulations_inputs/ref.map.chr10.taniguti.csv")

ref_map <- remove_outlier(ref_map, thr=0) # Remove inverted markers

seed <- 92
popsize <- 200
mapsize <- 38
depth <- 10
# PedigreeSim inputs
founderfile <- create_haplo(vcfR.obj = vcf, ref.map = ref_map, seed = seed,
                            P1 = "PT_F", P2= "PT_M")

## This function generates the mapfile and the ref_alt_alleles file
mapfile <- create_mapfile(vcf, ref_map)
#create_parfile(~{seed}, 50*~{popsize})
create_parfile(seed, popsize, name.mapfile = "/opt/mapfile.txt", 
               name.founderfile = "/opt/founders.txt", 
               name.chromfile = "/opt/chromosome.txt", name.out = "/opt/sim")
create_chromfile(mapfile[[1]])

ref_alt_alleles <- mapfile[[2]]
write.table(ref_alt_alleles, file="ref_alt_alleles.txt")

# Codifying phases for comparision with gusmap
compare_phases(founderfile, ref_alt_alleles)

system("docker run -v C:/Users/Rose_Lab/Documents/Cris_temp/Simulations_inputs:/opt cristaniguti/java-in-the-cloud:0.0.1 java -jar /usr/jars/PedigreeSim.jar /opt/parameters.txt")

library(onemap)

mks <- read.table("ref_alt_alleles.txt", stringsAsFactors = FALSE)

set.seed(seed)
pedsim2vcf(inputfile = "sim_genotypes.dat",
           map.file = "mapfile.txt",
           chrom.file = "chromosome.txt",
           out.file = "temp.vcf",
           miss.perc = 0,
           counts = FALSE,
           pos = mks[,2],
           haplo.ref = "P1_1",
           chr = mks[,1],
           phase = TRUE,
           reference.alleles = mks[,3],
           use.as.alleles=TRUE,
           #  n_selected_loci = 1, 
           #  selection_str_mean = 0.5, 
           #  selection_str_var = 0.0001, 
           #  pop.size = ~{popsize}, 
           #  selected_mks = 30,
           map.size = mapsize)

vcfR.object <- read.vcfR("temp.vcf")

test.obj <- onemap_read_vcfR(vcfR.object = vcfR.object, cross = "outcross", parent1 = "P1", parent2 = "P2")
twopts <- rf_2pts(test.obj)
seq1 <- make_seq(twopts, "all")
batch_size <- pick_batch_sizes(input.seq = seq1, 
                               size = 80, 
                               overlap = 30, 
                               around = 10)

batch_map_par <- map_avoid_unlinked(input.seq = seq1,
                                    size = batch_size,
                                    phase_cores = 4,
                                    overlap = 30)

vcf.geno <- as.data.frame(vcfR.object@gt[,2:15])
write_xlsx(vcf.geno, path = "vcf_genotypes.xlsx")

vcf_simu <- data.frame(vcfR.object@fix, vcfR.object@gt, stringsAsFactors = FALSE)

vcf_simu[,6] <- "."
vcf_simu[,8] <- "."

add_head(vcf_simu, paste0(seed, "_", depth, "_simu.vcf"))

INDS_temp <- dimnames(vcfR.object@gt)[[2]][-1]
inds_sele <- INDS_temp[-c(which(INDS_temp=="P1"), which(INDS_temp=="P2"))]

time1 <- system.time(progeny_dat <- vcf2progeny_haplotypes(vcfR.object = vcfR.object, ind.id = inds_sele[16:20],
                                      parent1 = "P1", parent2 = "P2",
                                      crosstype = "outcross"))

onemap:::plot.onemap_progeny_haplotypes(progeny_dat)
counts <- progeny_haplotypes_counts2(x = progeny_dat)
counts

haplo_simu <- cbind(seed= seed, depth=depth,progeny_dat)
