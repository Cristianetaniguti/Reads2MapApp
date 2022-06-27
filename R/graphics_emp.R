#' Read empirical input data
#' 
#' @param x inputted dataset
#' @param example_emp selected example id
#' 
#' @importFrom utils read.table untar
#' @import vroom
#' @import largeList
prepare_datas_emp <- function(x, example_emp){
  # This function makes adjustments in the input tar.gz file to be processed inside the app
  # It returns six data objects and the app options in a list format
  cat(example_emp)
  cat(x)
    if(!is.null(x)){
      data.gz <- x[,4]
      path = "data/"
    } else if(example_emp== "none"){
      cat("Wait credentials\n")
      data.gz <- "Wait"
    } else { ######## Available examples
      if(example_emp == "rose_biallelics_filt_GQ_noninfo"){
        data.gz <- system.file("ext", "rose/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "rose_multiallelics_filt_GQ_noninfo"){
        data.gz <- system.file("ext", "rose/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "rose_biallelics_GQ"){
        data.gz <- system.file("ext", "rose/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "rose_multiallelics_GQ"){
        data.gz <- system.file("ext", "rose/multiallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
       } else if(example_emp == "rose/biallelics_filt_GQ"){
        data.gz <- system.file("ext", "rose/biallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "rose_multiallelics_filt_GQ"){
        data.gz <-system.file("ext", "rose/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_biallelics_GQ"){
        data.gz <- system.file("ext", "populus/biallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_biallelics_filt_GQ"){
        data.gz <- system.file("ext", "populus/biallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_biallelics_filt_GQ_noninfo"){
        data.gz <- system.file("ext", "populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_multiallelics_GQ"){
        data.gz <-  system.file("ext", "populus/multiallelics_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_multiallelics_filt_GQ"){
        data.gz <- system.file("ext", "populus/multiallelics_filt_GQ/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_multiallelics_filt_GQ_noninfo"){
        data.gz <- system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_biallelics_GQ_cont"){
        data.gz <- system.file("ext", "populus/biallelics_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_biallelics_filt_GQ_cont"){
        data.gz <- system.file("ext", "populus/biallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_biallelics_filt_GQ_noninfo_cont"){
        data.gz <- system.file("ext", "populus/biallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_multiallelics_GQ_cont"){
        data.gz <- system.file("ext", "populus/multiallelics_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_multiallelics_filt_GQ_cont"){
        data.gz <- system.file("ext", "populus/multiallelics_filt_GQ/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "populus_multiallelics_filt_GQ_noninfo_cont"){
        data.gz <- system.file("ext", "populus/multiallelics_filt_GQ_noninfo/EmpiricalReads_results_cont.tar.gz", package = "Reads2MapApp")
      } else if(example_emp == "toy_sample_multi"){
         data.gz <- system.file("ext", "toy_sample_emp/multiallelics/EmpiricalReads_results.tar.gz", package = "Reads2MapApp")
      }
    }
    
    if(data.gz == "Wait"){
      cat("Waiting...\n")
    } else {
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
      
      for_rm <- sapply(list_files, "[", -grep("sequences",datas))
      
      temp_dat <- readList(datas[[grep("sequences",datas)]], index = 1)
      inds <- rownames(temp_dat[[1]]$data.name$geno)
      inds_list <- as.list(1:length(inds))
      names(inds_list) <- paste0(inds, " (", inds, ")")
      
      data5 <- load(datas[[grep("gusmap_RDatas.RData", datas)]])
      data5 <- base::get(data5)
      
      names_rdatas <- vroom(datas[[grep("names.tsv.gz", datas)]], delim = "\t")
      names_rdatas <- as.data.frame(names_rdatas)[,1]
      names_rdatas <- names_rdatas[-grep("gusmap", names_rdatas)]
      result_list <- list("data1" = vroom(datas[[grep("data1_depths_geno_prob.tsv.gz", datas)]]), 
                          "data2" = vroom(datas[[grep("data2_maps.tsv.gz", datas)]]), 
                          "data3" = vroom(datas[[grep("data3_filters.tsv.gz", datas)]]), 
                          "data4" = vroom(datas[[grep("data4_times.tsv.gz", datas)]]), 
                          "data5" = data5, 
                          "names" = names_rdatas, 
                          "ind_names" = inds_list,
                          "sequence.llo" = datas[[grep("sequences",datas)]])
      
      system(paste("rm -r", paste(for_rm, collapse = " ")))
      
      result_list
    }
}

#' Functions to build graphics for the empirical datas
#' @param data data.frame
#' @param genotypes to be documented
#' @param from to be documented
#' 
errorProb_graph_emp <- function(data, genotypes, from){
  
  # Not consider NA
  if(length(which(is.na(data$gt.vcf))) > 0)
    data <- data[-which(is.na(data$gt.vcf)),]
  
  if(from == "vcf"){
    geno <- data$gt.vcf
    colors <- rainbow(length(levels(as.factor(geno))))
    names(colors) <- levels(geno)
  } else {
    geno <- data$gt.onemap
    geno <- data$gt.vcf
    colors <- rainbow(length(levels(as.factor(geno))))
    names(colors) <- levels(geno)
  }
  
  # Parents
  parents <- apply(data[,10:13], 1, function(x) all(is.na(x)))
  
  if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=geno, shape = parents)) + 
      geom_point(alpha = 0.2) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors) + 
      scale_shape_manual(values=c(1, 3)) + 
      guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
      theme_bw()
  } else if(genotypes == "estimated_errors"){
    errors <- apply(data[,10:13], 1, function(x) {
      if(all(is.na(x))){
        return(NA)
      } else {
        y <- 1- x[which.max(x)]
        return(y)
      }
    }) 
    data %>% ggplot(aes(x=ref, y=alt, color=errors)) + 
      geom_point(alpha = 0.2) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_gradient(low = "#70ED57", high = "#F62A2C") + 
      guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
      theme_bw()
  }
}

ind_size_graph_emp <- function(data){
  
  # colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  # names(colors) <- levels(data$ErrorProb)
  
  colors <- rainbow(2)
  names(colors) <- levels(data$SNPCall)
  
  p1 <- data %>% filter(key == "n markers") %>% 
    ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= value), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    labs(x="Genotype call", y = "Number of markers", fill = "SNP call", title = "Number of markers") +
    scale_fill_viridis_d(begin=0, end = 0.6) + facet_grid(CountsFrom~.) + theme_bw()
  
  p2 <- data %>% filter(key == "Distance between markers (cM)") %>% 
    ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    geom_point(position=position_dodge(width = 0.5)) + 
    labs(x="Genotype call", y = "Distance between markers (cM)", fill = "SNP call", title = "Genetic distances") +
    scale_color_viridis_d(begin=0, end = 0.6) + facet_grid(CountsFrom~.) + theme_bw()
                          
  ggarrange(p1,p2, common.legend=T)  
}

all_size_graph_emp <- function(data, stat){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot() + geom_hline(yintercept=0, color="red") +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = paste(stat, "cM (haldane)")) 
}

marker_type_graph_emp <- function(data){
  
  data %>% ggplot(aes(x=GenoCall, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_viridis_d() + 
    labs(x = "Genotyping method", y = "Number of markers", fill="Marker type") +
    facet_grid(CountsFrom~SNPCall) + theme_bw()
}

times_graph_emp <- function(data){

  p1 <- data %>% filter(key == "number of markers") %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,3)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_viridis_d(begin=0, end = 0.6) + 
    labs(x = "Genotyping method", y = "", fill="SNP call", title = "Number of markers") +
    facet_grid(CountsFrom~., scales = "free") + theme_bw()
  
  p2 <- data %>% filter(key == "time (seconds)") %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,3)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_viridis_d(begin=0, end = 0.6) + 
    labs(x = "Genotyping method", y = "", fill="SNP call", title = "Time spent (seconds)") +
    facet_grid(CountsFrom~., scales = "free") + theme_bw()
  
  ggarrange(p2, p1, common.legend = T)
}

filters_graph_emp <- function(data){
  data %>% ggplot(aes(x= key, y=value, fill= GenoCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_viridis_d() + 
    labs(x = "", y = "Number of markers", fill = "Genotype method") +
    facet_grid(CountsFrom~SNPCall, scales = "free") +
    theme(axis.text.x = element_text(angle = 35, hjust=1)) + theme_bw()
}


overview_graph_emp <- function(df_overview, reescale = NULL){
  map_size <- df_overview %>% rename(., SNPCall = "SNP caller", GenoCall = "Genotype caller", 
                                     CountsFrom= "Read counts from", 
                                     map.size = "Map size (cM)") %>%
    select(., SNPCall, GenoCall, CountsFrom, map.size) %>%
  gather(key3, value3, - SNPCall, -GenoCall, -CountsFrom)  
  
  y_lim_nmks <- max(df_overview$`Mapped markers`)
  y_lim_cm <- max(df_overview$`Map size (cM)`)
  if(is.null(reescale)) reescale = y_lim_cm/y_lim_nmks
  mycolors <- brewer.pal(12, "Paired")
  
  ps2 <- df_overview %>% rename(., SNPCall = "SNP caller", GenoCall = "Genotype caller", 
                                CountsFrom= "Read counts from", 
                                n.mk = "Mapped markers") %>%
    select(., SNPCall, GenoCall, CountsFrom, n.mk) %>%
    gather(key, value, - SNPCall, -GenoCall, -CountsFrom)  %>%
    group_by(., SNPCall, GenoCall, CountsFrom, key) %>%
    merge(., map_size) %>%
    split(., list(.$CountsFrom,.$SNPCall)) %>%
    lapply(., function(z) ggplot(z, aes(x=GenoCall)) + 
             geom_bar(aes(y= value*reescale,fill=key),stat="identity", width = 0.8) +
             ylim(0, y_lim_nmks*reescale) +
             theme(axis.text.x = element_text(angle = 25, vjust = 1, hjust=1)) +
             labs(x="Genotype method", y = "Map size (cM)", fill= "Number of markers",
                  title= paste(z$SNPCall[1], "-", z$CountsFrom[1])) +
             scale_fill_manual(values=mycolors[9:10]) +
             geom_point(aes(y = value3, shape= factor(key3), 
                            colour = factor(key3)), size = 3, colour = mycolors[11]) +
             labs(shape= "Map size") +
             scale_y_continuous(sec.axis = sec_axis(~./reescale, name = "N markers"), limits = c(0, y_lim_cm)) 
           # theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
           #       legend.key.size = unit(0.8, "cm"),
           #       legend.key.width = unit(0.5,"cm"))
    )
  
  p1 <- ggarrange(plotlist = ps2, ncol = 2, nrow = 2, common.legend = T, legend = "right")

  return(p1)
}

#' Performs comparisons between maximum datasets:
#' 1) Venn diagram with exactly matching markers positions;
#' 
#' @param data list with marker names (chr_pos) of each dataset
#' @param data_names character defining the name of each vcf data
#' 
#' @import ggVennDiagram
#' @import ggplot2
#' 
SNPCalling_efficiency_graph_emp <- function(data, data_names){
  ggVennDiagram(data, label = "both", color = 1) + 
    scale_color_grey() + scale_fill_viridis_c() + theme_bw() + coord_sf(clip = "off") 
}


