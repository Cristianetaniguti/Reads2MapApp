#' Read input data
#' 
#' @param x inputted dataset
#' @param example_simu selected example id
#' 
#' @import vroom
#' @import largeList
#' 
prepare_datas_simu <- function(x, example_simu){
  withProgress(message = 'Working:', value = 0, {
    incProgress(0.2, detail = paste("Uploading simulation data..."))
    # This function makes adjustments in the input tar.gz file to be processed inside the app
    # It returns six data objects and the app options in a list format
    if(!is.null(x)){
      data.gz <- x[,4]
      path = "data/"
    } else if(example_simu=="none") {
      cat("Wait credentials\n")
      data.gz <- "Wait"
    } else { ######## Only the toy_sample in the package - the rest in server
      if(example_simu == "bi"){
        data.gz <- c(system.file("ext", "simu_results/depth10/biallelics/SimulatedReads_results_depth10_seed8085.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/biallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "bi_dev"){
        data.gz <- c(system.file("ext", "simu_results/depth20/biallelics_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/biallelics_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "bi_filt_gq"){ 
        data.gz <- c(system.file("ext", "simu_results/depth10/biallelics_filt_GQ/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/biallelics_filt_GQ/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "bi_filt_gq_dev"){
        data.gz <- c(system.file("ext", "simu_results/depth20/multiallelics_dev/SimulatedReads_results_depth20_seed8080.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/multiallelics_dev/SimulatedReads_results_depth10_seed8081.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "bi_filt_gq_noninfo"){ 
        data.gz <- c(system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8296.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8297.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "bi_filt_gq_noninfo_dev"){ 
        data.gz <- c(system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20_seed8295.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/biallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10_seed8294.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "bi_pl_filt"){
        data.gz <- c(system.file("ext", "simu_results/depth10/biallelics_pl_filt/SimulatedReads_results_depth10_seed8090.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/biallelics_pl_filt/SimulatedReads_results_depth20_seed8089.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "multi"){
        data.gz <- c(system.file("ext", "simu_results/depth20/multiallelics/SimulatedReads_results_depth20_seed8082.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/multiallelics/SimulatedReads_results_depth10_seed8084.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "multi_dev"){
        data.gz <- c(system.file("ext", "simu_results/depth20/multiallelics_dev/SimulatedReads_results_depth20_seed8080.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/multiallelics_dev/SimulatedReads_results_depth10_seed8081.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "multi_filt_gq"){ 
        data.gz <- c(system.file("ext", "simu_results/depth20/multiallelics_filt_GQ/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/multiallelics_filt_GQ/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "multi_filt_gq_dev"){
        data.gz <- c(system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "multi_filt_gq_noninfo"){ 
        data.gz <- c(system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8299.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo/SimulatedReads_results_depth20_seed8300.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "multi_filt_gq_noninfo_dev"){ 
        data.gz <- c(system.file("ext", "simu_results/depth10/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth10.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20_seed8303.tar.gz", package = "Reads2MapApp"),
                     system.file("ext", "simu_results/depth20/multiallelics_filt_GQ_noninfo_dev/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "toy_sample_bi"){
        data.gz <- c(system.file("ext", "toy_sample_simu/biallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp"))
      } else if(example_simu == "toy_sample_multi"){
        data.gz <- system.file("ext", "toy_sample_simu/multiallelics/SimulatedReads_results_depth20.tar.gz", package = "Reads2MapApp")
      }
    }
    incProgress(0.4, detail = paste("Uploading simulation data..."))
    
    if(data.gz == "Wait"){
      cat("Waiting...\n")
    } else {
      
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
      
      # Remove GATK Qual plots
      if(length(grep("QualPlots", list_files)) > 0)
        list_files <- lapply(list_files, function(x)  if(any(grepl("QualPlots", x))) x[-grep("QualPlots", x)] else x)
      
      # positions
      positions <- list()
      for(i in 1:length(list_files)){
        idx <- which(grepl("positions.tar.gz", list_files[[i]]))
        positions[[i]] <- list_files[[i]][idx]
        list_files[[i]] <- list_files[[i]][-idx]
      }
      
      # Data
      datas <- list()
      for(i in 1:length(list_files[[1]])){
        datas[[i]] <- sapply(list_files, "[", i)
      }
      
      datas[[length(datas)+1]] <- unlist(positions)
      
      ## Tables
      data1_depths_geno_prob <- data2_maps <- data3_filters <- data10_counts <- vector()
      data4_times <- data5_SNPCall_efficiency <- simu_haplo <- vector()
      data6 <- names_rdatas  <- vcf_pos <- vcf_pos_temp <- list()
      #seeds <- depths <- seeds_choices <- depths_choices <- vector()
      
      temp_name <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".llo")
      for(i in 1:length(datas)){
        for(j in 1:length(datas[[i]])){
          if(all(grepl("gusmap_RDatas.RData", datas[[i]]))){
            temp1 <- load(datas[[i]][[j]])
            temp1 <- base::get(temp1)
            data6 <- c(data6, temp1)
          } else if(all(grepl("positions.tar.gz", datas[[i]]))){
            depth <- sapply(strsplit(datas[[i]], "depth"), "[",2)
            depth <- sapply(strsplit(depth, "/"), "[",1)
            path_dir_pos <- tempdir()
            list_files_pos <- list()
            untar(datas[[i]][[j]], exdir = path_dir_pos)
            list_files_pos[[j]] <- untar(datas[[i]][[j]], list = T)
            vcf_pos_temp[[j]] <- sapply(file.path(path_dir_pos, list_files_pos[[j]][-1]), read.table)
            names(vcf_pos_temp[[j]]) <- paste0(gsub(".tsv","",sapply(strsplit(list_files_pos[[j]][-1], "/"), "[",2)))
            vcf_pos <- c(vcf_pos, vcf_pos_temp)
          } else if(all(grepl("sequences.llo", datas[[i]]))){
            temp1 <- readList(datas[[i]][[j]])
            if(j == 1){
              saveList(temp1, file = temp_name, append = F, compress = T)
              inds <- rownames(temp1[[1]]$data.name$geno)
            } else {
              saveList(temp1, file = temp_name, append = T, compress = T)
            }
          } else if(all(grepl("names.tsv.gz", datas[[i]]))){
            temp1 <-  vroom(datas[[i]][[j]], delim = "\t")
            names_rdatas <- c(names_rdatas, temp1)
          } else {
            temp1 <-  vroom(datas[[i]][[j]], delim = "\t")
            name_temp <- unlist(strsplit(datas[[i]][[j]], "/"))
            name_temp <- unlist(strsplit(name_temp[length(name_temp)], "[.]"))[1]
            assign(name_temp, rbind(base::get(name_temp), temp1))
          }
        }
      }
      incProgress(0.8, detail = paste("Uploading simulation data..."))
      
      temp <- unique(paste0(data2_maps$depth, "_", data2_maps$seed))
      seeds <- sapply(strsplit(temp, "_"), "[", 2)
      depths <- sapply(strsplit(temp, "_"), "[", 1)
      depths_choices <- as.list(depths)
      names(depths_choices) <- depths
      seed_depth <- unique(paste("Depth",data2_maps$depth, "seed", data2_maps$seed))
      temp_names <- seed_depth
      seeds_choices <- as.list(1:length(seed_depth))
      names(seeds_choices) <- temp_names
      inds_choices <- sort(inds)
      names(inds_choices) <- sort(inds)
      names_rdatas <- unlist(names_rdatas)
      names_rdatas <- names_rdatas[-grep("gusmap", names_rdatas)]
      
      result_list <- list("data1"=data1_depths_geno_prob, 
                          "data2"=data2_maps, 
                          "data3"=data3_filters, 
                          "data4"=data4_times, 
                          "data5"=data5_SNPCall_efficiency, 
                          "data6"=data6, 
                          "choices"=list(depths, seeds, seeds_choices, depths_choices, inds_choices),
                          "names"=names_rdatas, 
                          "haplo"=simu_haplo,
                          "sequence.llo"=temp_name,
                          "vcf_pos" = vcf_pos)
      
      system(paste("rm -r", paste(for_rm, collapse = " ")))
      
      result_list
    }
  })
}


#' Functions to build graphics for the data from simulations
#' 
#' @param data data.frame
#' @param genotypes to be documented
#' @param alpha transparency of dots
#' 
errorProb_graph <- function(data, genotypes, alpha){
  data <- data.frame(data)
  if(dim(data)[1] == 0) stop(safeError("This marker does not exists in this dataset."))
  
  #colors <- c("gray", "#5289C7", "#4EB265", "#882E72", "#F6C141")
  colors <- c("gray", "#5289C7", "#4EB265", "#882E72")
  #names(colors) <-  c("missing", "homozygous-alt","homozygous-ref", "heterozygous", "homozygous-alt == ref")
  names(colors) <-  c("missing", "homozygous-alt","homozygous-ref", "heterozygous")
  
  data$pop <- "progeny"
  data$pop[data$ind %in% c("P1", "P2")] <- "parents"
  
  data$alt <- as.numeric(data$alt)
  data$gt.onemap.alt.ref <- as.factor(data$gt.onemap.alt.ref)
  
  if(genotypes == "simulated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
      geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
      scale_shape_manual(values=c(3, 19))+
      labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
      scale_color_manual(values = colors) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)), 
             shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw()
  }else if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref)) + 
      geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
      scale_shape_manual(values=c(3, 19))+
      labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
      scale_color_manual(values = colors) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)), 
             shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw()
  } else if(genotypes == "estimated_errors"){
    data %>% ggplot(aes(x=ref, y=alt, color=errors)) + 
      geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
      scale_shape_manual(values=c(3, 19))+
      labs(x="reference allele counts", y = "alternative allele counts", color="Error rate", shape = "Individuals") +
      scale_colour_gradient(low = "#70ED57", high = "#F62A2C") +
      guides(colour = guide_legend(override.aes = list(alpha = 1)), 
             shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw()
  }
}

ind_size_graph <- function(data, data_n){
  colors_dots <- c("blue", "red", "green")
  names(colors_dots) <- c( "true marker", "false positive", "multiallelic")
  
  if(unique(data$fake) == "without-false"){
    legend.y <- "distance between real and estimated (cM)"
  } else {
    legend.y <- "distance between markers (cM)"
  }
  
  p1 <- data %>% ggplot() +
    geom_boxplot(aes(x=GenoCall, y = `diff (cM)`, fill=GenoCall), alpha = 0.6) +
    geom_point(aes(x=GenoCall, y = `diff (cM)`,color = factor(real.mks)), 
               position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.5)) +
    scale_fill_viridis_d() +
    facet_wrap(SNPCall + CountsFrom~., scales = "fixed", strip.position = "top", ncol = 1) +
    scale_color_manual("Markers", values = colors_dots) +    
    guides(fill=FALSE) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "top") + 
    labs(x = "", title = "Genetic distances") +
    ylab(legend.y) + theme_bw()
  
  
  p2 <- data_n %>% ggplot()+
    geom_bar(aes(x=GenoCall, y=`n markers`, fill=GenoCall), stat = "identity") + 
    scale_fill_viridis_d() +
    facet_wrap(SNPCall + CountsFrom~., scales = "fixed", strip.position = "top", ncol = 1) +
    labs(x = "", y= "number of markers", fill = "Genotyping", title = "Number of markers") +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "none",
    ) + theme_bw()
  
  ggarrange(p1, p2, legend = "top", ncol = 2, align = "hv")
}

all_size_graph <- function(data, data_n,stat, fake){
  
  if(stat == "euclidean_dist" & fake == "with-false")
    stop(safeError("In the presence of false positives, euclidean distance can not be calculated. 
                   Please, select other option."))
  
  p1 <- data %>% ggplot(aes(x=GenoCall, y=.[[dim(data)[2]]], color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5)  +
    labs(y = colnames(data)[dim(data)[2]], x = "Genotyping method", title = "Statistic") + 
    facet_wrap(depth~CountsFrom, ncol=1, scales = "fixed") + theme_bw()
  
  p2 <- data_n %>% ggplot(aes(x=GenoCall, y=`n markers`, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5)  +
    labs(x = "Genotyping method", y = "Number of markers", title= "Number of markers") + 
    facet_wrap(depth~CountsFrom, ncol=1, scales = "fixed") + theme_bw()
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  
  if(n_fam < 20) {
    p1 <- p1 + geom_point(position=position_dodge(width=0.5))  
    p2 <- p2 + geom_point(position=position_dodge(width=0.5))
  } else {
    p1 <- p1 + geom_boxplot()
    p2 <- p2 + geom_boxplot()
  }
  ggarrange(p1,p2, legend = "top", common.legend = T)
}

marker_type_graph <- function(data){
  data %>% ggplot(aes(x=real.mks, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_viridis_d(name="Marker type") + 
    labs(x = NULL, y = "Number of markers") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_grid(SNPCall+CountsFrom+key~GenoCall) + theme_bw()
}

phases_graph <- function(data){
  p1 <- data %>% filter(key == "% correct") %>%  
    ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "%", title="Percent of corrected phases") +
    facet_wrap(depth + CountsFrom~., ncol=1) + theme_bw() 
  
  p2 <- data %>% filter(key == "n markers") %>%  
    ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "Number of markers", title = "Number of markers") +
    facet_wrap(depth + CountsFrom~., ncol=1) + theme_bw()
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)  {
    p1 <- p1 + geom_point(position=position_dodge(width=0.5))
    p2 <- p2 + geom_point(position=position_dodge(width=0.5))
  } else p +  {
    p1 <- p1 + geom_boxplot()
    p2 <- p2 + geom_boxplot()
  }
  ggarrange(p1,p2, common.legend = T, legend = "top")
}

times_graph <- function(data){
  data$value <- as.numeric(data$value)
  p1 <- data %>% filter(key == "minutes") %>% ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "minutes", title = "Time spent") +
    facet_grid(depth + CountsFrom ~.) 
  
  p2 <- data %>% filter(key == "n markers") %>% ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "number of markers", title = "Number of markers") +
    facet_grid(depth + CountsFrom ~.) 
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)  {
    p1 <- p1 + geom_point(position=position_dodge(width=0.5)) + theme_bw()
    p2 <- p2 + geom_point(position=position_dodge(width=0.5)) + theme_bw()
  } else {
    p1 <- p1 + geom_boxplot(position=position_dodge(width=0.5)) + theme_bw()
    p2 <- p2 + geom_boxplot(position=position_dodge(width=0.5)) + theme_bw()
  }
  ggarrange(p1, p2, common.legend = T)
}

avalSNPs_graph <- function(data){
  p <- data %>% filter(counts == TRUE) %>% ggplot(aes(x=name, y=value, color= SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "", y = "Number of markers") + 
    facet_grid(depth ~ .) + theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  
  p2 <- data %>% filter(counts == FALSE) %>% ggplot(aes(x=name, y=value, color= SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "", y = "Percentage") + 
    facet_grid(depth ~ .) + theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)   {
    p <- p + geom_point(position=position_dodge(width=0.5))
    p2 <- p2 + geom_point(position=position_dodge(width=0.5))
  } else {
    p <- p + geom_boxplot(position=position_dodge(width=0.5))
    p2 <- p2 + geom_boxplot(position=position_dodge(width=0.5))
  }
  
  ggarrange(p, p2, common.legend = T)
}

avalSNPs_graph_filt <- function(seeds){
  p <- list()
  for(i in 1:length(seeds)){
    p[[i]] <- ggVennDiagram(seeds[[i]][1:3], label = "both", color = 1) + 
      scale_color_grey() + scale_fill_viridis_c() + theme_bw() + coord_sf(clip = "off")
  }
  ggarrange(plotlist = p)
}

filters_graph <- function(data){
  p <- data %>% ggplot(aes(x= name, y=value, color= GenoCall)) +
    scale_color_viridis_d(name="Genotype call") + 
    labs(x = "", y = "number of markers") +
    facet_grid(depth + CountsFrom~SNPCall, scales = "fixed")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme_bw()
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)   p + geom_point(position=position_dodge(width=0.5))  else p + geom_boxplot(position=position_dodge(width=0.5))
}

agree_coefs <- function(m, method= "all"){
  
  data1 <- data.frame(c("value", "p-value"))
  
  if(any(method %in% "kendall.concor") | all(method == "all")){
    kendall.concor <- kendall(m)
    data_temp <- data.frame(c(kendall.concor$value, kendall.concor$p.value))
    colnames(data_temp) <- "Kendall's coefficient of concordance"
    data1 <- cbind(data1, data_temp)
  }
  
  if(any(method %in% "kendall.corr") | all(method == "all")){
    kendall.corr <- cor.test(m[,1],m[,2], method="kendall", use="pairwise")
    data_temp <- data.frame(c(kendall.corr$estimate, kendall.corr$p.value))
    colnames(data_temp) <- "Kendall's correlation coefficient"
    data1 <- cbind(data1, data_temp)
  }
  
  if(any(method %in% "kappa") | all(method == "all")){
    kappa <- kappa2(m)
    data_temp <- data.frame(c(kappa$value, kappa$p.value))
    colnames(data_temp) <- "Kappa statistics"
    data1 <- cbind(data1, data_temp)
  }
  perc.agree <- agree(m)
  
  data_temp <- data.frame(c(round(perc.agree$value,2), "-"))
  colnames(data_temp) <- "Percentage of agreement"
  
  data1 <- cbind(data1, data_temp)
  colnames(data1)[1] <- ""
  
  return(data1)
}

overview_graph <- function(df_overview, depth_select, reescale = NULL){
  df_overview$`Number of non-informative markers in map`  <- df_overview$`Number markers in map`*((df_overview$`Percentage of non-informative markers`)/100)
  df_overview$`Number of informative markers in map` <-  df_overview$`Number markers in map` - df_overview$`Number of non-informative markers in map`
  
  y_lim_nmks <- max(df_overview$`Number markers in map`)
  mycolors <- brewer.pal(12, "Paired")
  ps <- df_overview %>% filter(depth == depth_select) %>% filter(Value == "value") %>%
    rename(., "Genotypes (kappa)" = "Kappa's coefficient for genotypes", 
           "Phases (kappa)" = "Kappa's coefficient for phases",
           "Marker types (kappa)" = "Kappa's coefficient for marker types", 
           "Breakpoints (Kendall)" = "Kendall's coefficient of concordance for breakpoints") %>%
    gather(key, value, - SNPCall, -GenoCall, -depth, -seed, -CountsFrom, -Value) %>%
    filter(key %in% c("Genotypes (kappa)", "Phases (kappa)",
                      "Marker types (kappa)", 
                      "Breakpoints (Kendall)")) %>%
    split(., list(.$CountsFrom,.$SNPCall)) %>% 
    lapply(., function(x) ggplot(x, aes(x=GenoCall, y=as.numeric(value), color = key)) + 
             geom_boxplot()+
             theme(axis.text.x = element_blank()) +
             scale_color_manual(values=mycolors[c(2,4,6,8)]) +
             labs(title= paste(x$SNPCall[1], "-", x$CountsFrom[1]),
                  x=element_blank(), y = "coef value", color= "Coeficients") + 
             # theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
             #       legend.key.size = unit(0.8, "cm"),
             #       legend.key.width = unit(1,"cm")) +
             scale_y_continuous(sec.axis = sec_axis(~., name = " ")) +
             ylim(0,1)
    )
  
  n_tot <-df_overview %>% filter(depth == depth_select) %>% filter(Value == "value") %>%
    select(SNPCall, GenoCall, seed, CountsFrom, "Number markers in map", "Number of non-informative markers in map") %>%
    rename(., "Non-informative" = "Number of non-informative markers in map") %>%
    gather(key, value, - SNPCall, -GenoCall, -seed, -CountsFrom)  %>%
    group_by(., SNPCall, GenoCall, CountsFrom, key) %>%
    summarise(mean2 = mean(value),
              se2 = sd(value)/sqrt(length(value))) %>% ungroup()
  
  n_tot$key[n_tot$key == "Number markers in map"] <- "Informative"
  
  map_size <- df_overview %>% filter(depth == depth_select) %>% filter(Value == "value") %>%
    select(SNPCall, GenoCall, seed, CountsFrom, "Map size (cM)") %>%
    gather(key3, value, - SNPCall, -GenoCall, -seed, -CountsFrom)  %>%
    group_by(., SNPCall, GenoCall, CountsFrom, key3) %>%
    summarise(mean3 = mean(value),
              se3 = sd(value)/sqrt(length(value))) %>% ungroup() 
  
  y_lim_cm <- max(map_size$mean3)
  
  if(is.null(reescale)) reescale = y_lim_cm/y_lim_nmks
  
  ps2 <- df_overview %>% filter(depth == depth_select) %>% filter(Value == "value") %>%
    select(SNPCall, GenoCall, seed, CountsFrom,
           "Number of non-informative markers in map", "Number of informative markers in map") %>%
    rename(., "Non-informative" = "Number of non-informative markers in map",
           "Informative"= "Number of informative markers in map") %>%
    gather(key, value, - SNPCall, -GenoCall, -seed,    -CountsFrom)  %>%
    group_by(., SNPCall, GenoCall, CountsFrom, key) %>%
    summarise(mean = mean(value),
              se = sd(value)/sqrt(length(value))) %>% ungroup() %>%
    merge(., n_tot) %>%
    merge(., map_size) %>%
    split(., list(.$CountsFrom,.$SNPCall)) %>%
    lapply(., function(z) ggplot(z, aes(x=GenoCall)) + 
             geom_bar(aes(y= mean*reescale,fill=key),stat="identity", width = 0.8) +
             ylim(0, y_lim_nmks*reescale) +
             theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) +
             labs(x="Genotype method", y = "Map size (cM)", fill= "Number of markers") +
             scale_fill_manual(values=mycolors[9:10]) +
             geom_errorbar(aes(ymin=mean2*reescale-se2*reescale, ymax=mean2*reescale+se2*reescale), width=0.3) + 
             geom_point(aes(y = mean3, shape= factor(key3), 
                            colour = factor(key3)), size = 3, colour = mycolors[11]) +
             labs(shape= "Mean map size") +
             scale_y_continuous(sec.axis = sec_axis(~./reescale, name = "N markers"), limits = c(0, y_lim_cm)) 
           # theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
           #       legend.key.size = unit(0.8, "cm"),
           #       legend.key.width = unit(0.5,"cm"))
    )
  
  ps[[1]] <- ps[[1]] + theme(legend.position = "none")
  ps[[2]] <- ps[[2]] + theme(legend.position = "none", axis.title.y = element_blank())
  
  ps2[[1]] <- ps2[[1]] + theme(legend.position = "none", axis.title.y.right = element_blank(), axis.title.x =  element_blank())
  ps2[[2]] <- ps2[[2]] + theme(legend.position = "none", axis.title.y.left =  element_blank(), axis.title.x =  element_blank())
  
  ps[[3]] <- ps[[3]] + theme(legend.position = "none")
  ps[[4]] <- ps[[4]] + theme(legend.position = "none", axis.title.y = element_blank())
  
  ps2[[3]] <- ps2[[3]] + theme(legend.position = "none", axis.title.y.right = element_blank())
  ps2[[4]] <- ps2[[4]] + theme(legend.position = "none", axis.title.y.left =  element_blank()) 
  
  p1 <- ggarrange(plotlist = list(ps[[1]], ps[[2]]), ncol = 2, common.legend = T, legend = "top")
  p2 <- ggarrange(plotlist = list(ps2[[1]], ps2[[2]]), ncol = 2)
  p3 <- ggarrange(plotlist = list(ps[[3]], ps[[4]]), ncol = 2)
  p4 <- ggarrange(plotlist = list(ps2[[3]], ps2[[4]]), ncol = 2, common.legend = T, legend = "bottom")
  
  p_joint <- ggarrange(plotlist = list(p1,p2,p3,p4), ncol = 1)
  return(p_joint)
}

cmbymb <- function(data){
  data$key <- gsub("poscM.norm", "real",data$key)
  data$key <- gsub("rf", "estimated",data$key)
  
  ggplot(data, aes(x=pos/1000000, y=value, color=real.mks)) +
    geom_point() + 
    xlab("position (MB)") +
    ylab("position (cM)") + 
    scale_color_viridis_d(begin = 0, end = 0.5) +
    guides(color=guide_legend(title="Markers")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    facet_grid(SNPCall+GenoCall~key, scales = "fixed") +
    theme_bw() 
}

#' @importFrom ggpubr ggarrange
#' @import ggplot2
#' @import dplyr
marker_type_probs <- function(data_plot_par){
  if(!is.data.frame(data_plot_par)) stop(safeError("The probabilities can not be calculated in the presence of false positives.\n
                              If you set option without-false, then there are no biallelic/multiallelic marker in this dataset. 
                              Please, select other option."))
  
  labels_real <- c("Real: B3.7", "Real: D1.10", "Real: D2.15")
  labels_est <-  c("Est: B3.7", "Est: D1.10", "Est: D2.15")
  labels_title1 <- c(expression("P(E=B3.7|M"*intersect("R=B3.7)")), 
                     expression("P(E=D1.10|M"*intersect("R=D1.10)")),
                     expression("P(E=D2.15|M"*intersect("R=D2.15)")))
  labels_title2 <- c(expression("P(E|M"*intersect("R=B3.7)")), 
                     expression("P(E|M"*intersect("R=D1.10)")),
                     expression("P(E|M"*intersect("R=D2.15)")))
  p_comb <- list()
  for(i in 1:3){
    data_temp <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == labels_real[i] & 
                                                                     est == labels_est[i] )
    if(dim(data_temp)[1] > 0){
      p1 <- data_temp %>%
        ggplot(aes(x=GenoCall, y = value, color = depth)) +
        geom_point(position = position_dodge(width=0.7)) +
        scale_color_viridis_d(begin = 0, end = 0.5)+
        theme_bw() + ylim(0,1) + 
        facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
        labs(title= labels_title1[i], x = "Genotype caller", color = "mean depth")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
              axis.title.y = element_blank(), legend.position = "top",
              legend.text=element_text(size=12))
    } else { p1 <- NULL }
    
    data_temp <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == labels_real[i] & 
                                                                     est != labels_est[i]) 
    if(dim(data_temp)[1] > 0){
      p2 <- data_temp %>% 
        ggplot(aes(x=GenoCall, y = value, color = depth)) +
        geom_point(position = position_dodge(width=0.7)) +
        scale_color_viridis_d(begin = 0, end = 0.5)+
        theme_bw() + ylim(0,1) + 
        facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
        labs(title= labels_title2[i], x = "Genotype caller", color = "mean depth")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
              axis.title.y = element_blank(), legend.position = "top",
              legend.text=element_text(size=12))
    } else {p2 <- NULL}
    p_comb[[i]] <- ggarrange(p1, p2, common.legend = T, widths = c(3,10), heights = c(16,16))
  }
  
  data_temp <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == "Real: non-informative") 
  
  if(dim(data_temp)[1] > 0){
    p3 <- data_temp %>%
      ggplot(aes(x=GenoCall, y = value, color = depth)) +
      geom_point(position = position_dodge(width=0.7)) +
      scale_color_viridis_d(begin = 0, end = 0.5)+
      theme_bw() + ylim(0,1) + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= expression("P(E|M"*intersect("R=non-informative)")), x = "Genotype caller", color = "mean depth")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=12))
    p3_comb <- ggarrange(p3, widths = 13, heights = 16)
  } else { p3_comb <- NULL}
  
  ggarrange(p_comb[[1]],p_comb[[2]],p_comb[[3]],p3_comb, ncol = 1)
}


#' @importFrom ggpubr ggarrange
#' @import ggplot2
#' @import dplyr
marker_type_probs_multi <- function(data_plot_par){
  if(!is.data.frame(data_plot_par)) stop(safeError("The probabilities can not be calculated in the presence of false positives.\n
                              If you set option without-false, then there are no biallelic/multiallelic marker in this dataset. 
                              Please, select other option."))
  
  labels_real <- c("Real: B3.7", "Real: D1.10", "Real: D2.15")
  labels_est <-  c("Est: A.1", "Est: A.2", "Est: D2.14", "Est: D1.09")
  labels_title1 <- c(expression("P(E= A.1|M"*intersect("R=B3.7)")), 
                     expression("P(E=A.2|M"*intersect("R=D1.10)")),
                     expression("P(E=D2.14|M"*intersect("R=D2.15)")),
                     expression("P(E=D1.09|M"*intersect("R=D2.15)")))
  labels_title2 <- c(expression("P(E|M"*intersect("R=B3.7)")), 
                     expression("P(E|M"*intersect("R=D1.10)")),
                     expression("P(E|M"*intersect("R=D2.15)")))
  p_comb <- list()
  for(i in 1:3){
    data_set <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == labels_real[i] & 
                                                                    est != labels_real[i])
    if(dim(data_set)[1] == 0) next
    
    p <- data_set %>%
      ggplot(aes(x=GenoCall, y = value, color = depth)) +
      geom_point(position = position_dodge(width=0.7)) +
      scale_color_viridis_d(begin = 0, end = 0.5)+
      theme_bw() + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= labels_title2[i], x = "Genotype caller", color = "mean depth")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=12))
    
    p_comb[[i]] <- p
  }
  
  p3 <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == "Real: non-informative") %>%
    ggplot(aes(x=GenoCall, y = value, color = depth)) +
    geom_point(position = position_dodge(width=0.7)) +
    scale_color_viridis_d(begin = 0, end = 0.5)+
    theme_bw() + 
    facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
    labs(title= expression("P(E|M"*intersect("R=non-informative)")), x = "Genotype caller", color = "mean depth")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          axis.title.y = element_blank(), legend.position = "top",
          legend.text=element_text(size=12))
  
  p_comb[[length(p_comb) + 1]] <- p3
  
  ggarrange(plotlist = p_comb, ncol = 1)
}

geno_probs <- function(data_plot_par){
  labels_real <- c("Real: heterozygous", "Real: homozygous-ref", "Real: homozygous-alt")
  labels_est <-  c("Est: heterozygous", "Est: homozygous-ref", "Est: homozygous-alt")
  labels_title1 <- c(expression("P(E=heterozygous|M"*intersect("R=heterozygous)")), 
                     expression("P(E=homozygous-ref|M"*intersect("R=homozygous-ref)")),
                     expression("P(E=homozygous-alt|M"*intersect("R=homozygous-alt)")))
  labels_title2 <- c(expression("P(E|M"*intersect("R=heterozygous)")), 
                     expression("P(E|M"*intersect("R=homozygous-ref)")),
                     expression("P(E|M"*intersect("R=homozygous-alt)")))
  p_comb <- list()
  for(i in 1:3){
    p1 <- data_plot_par %>% pivot_longer(cols=8:9) %>% filter(simu == labels_real[i] & 
                                                                est == labels_est[i] ) %>%
      ggplot(aes(x=GenoCall, y = value, color = name, shape = depth)) +
      geom_point(position = position_dodge(width=0.8)) +
      scale_shape_manual(values=c(1, 3))+
      theme_bw() + ylim(0,1) + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= labels_title1[i], x = "Genotype caller", shape = "mean depth", color="")+
      scale_color_manual(labels=c("mean error rate", 
                                  expression("P(E|M"*intersect("R)"))), values = c("red", "blue"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=11))
    
    p2 <- data_plot_par %>% pivot_longer(cols=8:9) %>% filter(simu == labels_real[i] & 
                                                                est != labels_est[i]) %>%
      ggplot(aes(x=GenoCall, y = value, color = name, shape = depth)) +
      geom_point(position = position_dodge(width=0.8)) +
      scale_shape_manual(values=c(1, 3)) +
      theme_bw() + ylim(0,1) + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= labels_title2[i], x = "Genotype caller", shape = "mean depth", color = "")+
      scale_color_manual(labels=c("mean error rate", 
                                  expression("P(E|M"*intersect("R)"))), values = c("red", "blue"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=11))
    
    p_comb[[i]] <- ggarrange(p1, p2, common.legend = T, widths = c(4,8), heights = c(16,16))
  }
  
  ggarrange(p_comb[[1]],p_comb[[2]],p_comb[[3]], ncol = 1)
}

roc_graph <- function(data){
  data %>% ggplot(aes(x=1-specificity, y=sensitivity, color = GenoCall)) +
    geom_point() + 
    geom_line()  +
    facet_grid(CountsFrom + depth ~ SNPCall) + 
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    labs(color = "Genotype caller") +
    geom_abline(intercept = 0, slope = 1)
}
