# Global objects
#=======================================================================
## Datasets available
#=======================================================================
# Simulated
id <- c("P. tremula 20 cM of chromosome 10 - without multiallelics",
        "P. tremula 20 cM of chromosome 10 - with multiallelics",
        "P. tremula 37 cM of chromosome 10 - without multiallelics",
        "P. tremula 37 cM of chromosome 10 - with multiallelics",
        "Toy sample without multiallelics",
        "Toy sample with multiallelics")
depth_parents <- c(rep("80,160", 4), rep("20,30", 2))
depth_progeny <- c(rep("10,20",4), rep("10,20",2))
population_size <- c(rep(200,4), 20,20)
number_fam <- c(rep(5,4), 2,2)
ref_gen <- c(rep("Populus trichocarpa v4.0 chromosome 10:0-7000000",2),
             rep("Populus trichocarpa v4.0 chromosome 10:0-8500000",2),
             rep("Populus trichocarpa v4.0 chromosome 10:0-2000000",2))
ref_vcf <- rep("From EmpiricalReads2Map with Bioproject PRJNA395 raw data", 6)
ref_map <- rep("From thesis chapter 2",6)
library <- rep("RADinitio",6)

simulated_datasets <- data.frame(id, depth_parents, depth_progeny,
                                 population_size, number_fam,
                                 ref_gen, ref_vcf, ref_map,
                                 library)

colnames(simulated_datasets) <-
  c("ID", "Depth - Parents", "Depth - Progeny",
    "Population size", "Number of families", "Reference genome",
    "Reference VCF", "Reference genetic map", "Library type")
#=======================================================================
### Empirical
#=======================================================================
id <- c("P. tremula - with contaminants; without multiallelics",
        "P. tremula - without multiallelics	",
        "P. tremula - with multiallics",
        "Toy sample without multiallelics",
        "Toy sample with multiallelics")
fastq <- c(rep("Bioproject PRJNA395",3),
           rep("Bioproject PRJNA395",2))
pop_size <- c(122,116,116,5,5)
library_type <- rep("RADseq",5)
ref_gen <- c(rep("Populus trichocarpa v4.0", 3),
             rep("Populus trichocarpa v4.0",2))
chr_sele <- rep("10", 5)

empirical_datasets <- data.frame(id,
                                 fastq,
                                 pop_size,
                                 library_type,
                                 ref_gen,
                                 chr_sele)

colnames(empirical_datasets) <-
  c("ID", "FASTQ files",
    "Population size", "Library type",
    "Reference genome", "Selected genome")


#=======================================================================
# Choices
#=======================================================================
## Permanently choices - If add more softwares in workflow comparision
## this part requires update
overview_emp_choices <-
  list("n_markers", "redundants", "n_markers_map", "filt_mks",
       "map_size", "time", "breakpoints", "mean_break", "se_break")

names(overview_emp_choices) <-
  c("Informative markers in VCF", "Redundant markers",
    "Mapped markers", "Filtered markers", "Map size (cM)",
    "Time (s)","Total breakpoints", "Mean breakpoints",
    "Standard error breakpoints")

overview_choices <-
  list("geno", "phases", "marker", "noninfo", "mapsize",
       "nmarker", "conco_break", "corr_break", "time")
names(overview_choices) <-
  c("Kappa's coefficient for genotypes",
    "Kappa's coefficient for phases",
    "Kappa's coefficient for marker types",
    "Percentage of noninformative markers",
    "Map size (cM)",
    "Number markers in map",
    "Kendall's coefficient of concordance for breakpoints",
    "Kendall's coefficient of correlation for breakpoints",
    "Time spent (s)")

ErrorProb_choice <-
  list("OneMap_version2" = "SNPCallerdefault",
       "polyRAD"="polyrad",
       "freebayes/GATK"="SNPCaller",
       "SuperMASSA"="supermassa",
       "updog"="updog")

ErrorProb_choice_unique <-
  list("polyRAD"="polyrad",
       "freebayes/GATK"="SNPCaller",
       "SuperMASSA"="supermassa",
       "updog"="updog")

global0.05_choices <-
  list("global error of 0.05"=TRUE,
       "variable error" = FALSE)

maps_choice <- list("GUSMap" = "gusmap",
                    "OneMap_version2" = "SNPCallerdefault",
                    "polyRAD"="polyrad",
                    "freebayes/GATK"="SNPCaller",
                    "SuperMASSA"="supermassa",
                    "updog"="updog")

SNPCall_choice <- list("freebayes"="freebayes",
                       "GATK" = "gatk")
CountsFrom_choice <- list("BAM"="bam",
                          "VCF"="vcf")

stats_choice <-
  list("euclidean_dist", "mean", "median", "var",
       "total", "total_size")
names(stats_choice) <-
  c("Euclidean distance (D)", "mean", "median", "var",
    "total", "total size")

fake_choices <- list("with-false", "without-false")
names(fake_choices) <- c("yes", "no")
