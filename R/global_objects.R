# Global objects
##########################
## Datasets available
##########################
# Simulated

id <- c("Biallelics GQ",
        "Biallelics GQ with segregation distortion",
        "Biallelics filtered GQ",
        "Biallelics GQ filtered with segregation distortion",
        "Biallelics GQ and non-informative filtered",
        "Biallelics GQ and non-informative filtered with segregation distortion",
        "Biallelics PL filtered",
        "Biallelics + multiallelics GQ",
        "Biallelics + multiallelics GQ with segregation distortion",
        "Biallelics + multiallelics filtered GQ", 
        "Biallelics + multiallelics filtered GQ with segregation distortion", 
        "Biallelics + multiallelics filtered GQ and non-informative",
        "Biallelics + multiallelics filtered GQ and non-informative with segregation distortion",
        "Toy sample without multiallelics",
        "Toy sample with multiallelics")

depth_parents <- c(rep("80,160", 13), rep("20,30", 2))
depth_progeny <- c(rep("10,20",13), rep("10,20",2))
population_size <- c(rep(200,13), 20,20)
number_fam <- c(rep(5,13), 2,2)
ref_gen <- c(rep("Populus trichocarpa v4.0 chromosome 10:0-8424600",13),
             rep("Populus trichocarpa v4.0 chromosome 10:0-2000000",2))
ref_vcf <- c(rep("From EmpiricalReads2Map with Bioproject PRJNA395 raw data", 15))
ref_map <- (rep("From thesis chapter 2",15))
library <- c(rep("RADinitio",15))

simulated_datasets <- data.frame(id, depth_parents, depth_progeny,
                                 population_size, number_fam,
                                 ref_gen, ref_vcf, ref_map,
                                 library)

colnames(simulated_datasets) <- c("ID", "Depth - Parents", "Depth - Progeny",
                                  "Population size", "Number of families", "Reference genome",
                                  "Reference VCF", "Reference genetic map", "Library type")
### Empirical

id <- c("Roses 37% Chr01	- biallelics filt GQ and noninfo",
        "Roses 37% Chr01	- multiallelics filt GQ and noninfo",
        "Roses 37% Chr01	- biallelics  GQ",
        "Roses 37% Chr01	- multiallelics GQ",
        "Roses 37% Chr01	- biallelics filt GQ",
        "Roses 37% Chr01	- multiallelics filt GQ",
        "P. tremula 37% Chr10 - biallelics GQ",
        "P. tremula 37% Chr10 - biallelics filt GQ",
        "P. tremula 37% Chr10 - biallelics filt GQ and noninfo",
        "P. tremula 37% Chr10 - multiallelics GQ",
        "P. tremula 37% Chr10 - multiallelics filt GQ",
        "P. tremula 37% Chr10 - multiallelics filt GQ and noninfo",
        "P. tremula 37% Chr10 with 6 contaminants - biallelics GQ",
        "P. tremula 37% Chr10 with 6 contaminants - biallelics filt GQ",
        "P. tremula 37% Chr10 with 6 contaminants - biallelics filt GQ and noninfo",
        "P. tremula 37% Chr10 with 6 contaminants - multiallelics GQ",
        "P. tremula 37% Chr10 with 6 contaminants - multiallelics filt GQ",
        "P. tremula 37% Chr10 with 6 contaminants - multiallelics filt GQ and noninfo",
        "Toy sample without multiallelics")

fastq <- c(rep("to be released", 6),
           rep("Bioproject PRJNA395",13))

pop_size <- c(rep(138,6), rep(116, 6), rep(122, 6), c(5))
library_type <- rep("RADseq",19)
ref_gen <- c(rep("Rosa chinensis v1.0", 6),
             rep("Populus trichocarpa v4.0",13))
chr_sele <- c(rep("1", 6), rep("10", 13))

empirical_datasets <- data.frame(id,
                                 fastq,
                                 pop_size,
                                 library_type,
                                 ref_gen,
                                 chr_sele)

colnames(empirical_datasets) <- c("ID", "FASTQ files",
                                  "Population size", "Library type",
                                  "Reference genome", "Selected genome")


#####################
# Choices
#####################
## Permanently choices - If add more softwares in workflow comparision this part requires update
overview_emp_choices <- list("n_markers", "redundants", "n_markers_map", "filt_mks",
                             "map_size", "time", "breakpoints", "mean_break", "se_break")

names(overview_emp_choices) <- c("Informative markers in VCF", "Redundant markers",
                                 "Mapped markers", "Filtered markers", "Map size (cM)",
                                 "Time (s)","Total breakpoints", "Mean breakpoints",
                                 "Standard error breakpoints")

overview_choices <- list("geno", "phases", "marker", "noninfo", "mapsize",
                         "nmarker", "conco_break", "corr_break", "time")
names(overview_choices) <- c("Kappa's coefficient for genotypes",
                             "Kappa's coefficient for phases",
                             "Kappa's coefficient for marker types",
                             "Percentage of noninformative markers",
                             "Map size (cM)",
                             "Number markers in map",
                             "Kendall's coefficient of concordance for breakpoints",
                             "Kendall's coefficient of correlation for breakpoints",
                             "Time spent (s)")

ErrorProb_choice <- list("OneMap_version2" = "SNPCallerdefault",
                         "polyRAD"="polyrad",
                         "freebayes/GATK"="SNPCaller",
                         "SuperMASSA"="supermassa",
                         "updog"="updog")

ErrorProb_choice_unique <- list("polyRAD"="polyrad",
                                "freebayes/GATK"="SNPCaller",
                                "SuperMASSA"="supermassa",
                                "updog"="updog")

global0.05_choices <- list("global error of 0.05"=TRUE,
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

stats_choice <- list("euclidean_dist", "mean", "median", "var", "total", "total_size")
names(stats_choice) <- c("Euclidean distance (D)", "mean", "median", "var", "total", "total size")

fake_choices <- list("with-false", "without-false")
names(fake_choices) <- c("yes", "no")