# Functions to build graphics for the data from simulations

errorProb_graph <- function(data, genotypes){
  
  colors <- c("#58355e", "#4D9DE0", "#ADE25D")
  names(colors) <-  c("missing", "homozygous", "heterozygote")
  
  if(genotypes == "real_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
      geom_point(alpha = 0.2) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
  }else if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=methGT)) + 
      geom_point(alpha = 0.2) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors)+
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
  } else if(genotypes == "estimated_errors"){
    data %>% ggplot(aes(x=ref, y=alt, color=errors)) + 
      geom_point(alpha = 0.2) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_gradient(low = "#70ED57", high = "#F62A2C") +
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
  }
}

ind_size_graph <- function(data){
  
  # colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  # names(colors) <- levels(data$ErrorProb)
  
  colors_dots <- c("blue", "red")
  names(colors_dots) <- c( "true markers", "false positives")
  
  data %>% ggplot(aes(x=GenoCall, y=diff)) +
    geom_boxplot(alpha = 0.6) + 
    geom_point(position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.5),aes(color = factor(real.mks))) +
    facet_grid(SNPCall~.) +
    scale_color_manual("Markers", values = colors_dots)
}

all_size_graph <- function(data, stat){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot() + geom_hline(yintercept=0, color="red") +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = paste(stat, "cM (haldane)")) + 
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

marker_type_graph <- function(data){
  
  colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  names(colors) <- levels(data$type)
  
  data %>% ggplot(aes(x=GenoCall, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(name="Marker type", values = colors) + 
    labs(x = "Genotyping method", y = "Number of markers") +
    facet_grid(key~SNPCall) 
}


phases_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent of corrected phases") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

times_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot(position=position_dodge())  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "") +
    facet_wrap( depth~key, ncol=1, scales = "free", strip.position = "right")
}

coverage_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=coverage, fill=SNPCall)) +
    geom_boxplot(position=position_dodge())  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

avalSNPs_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=key, y=value, fill= SNPCall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") + 
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

filters_graph <- function(data){
  
  colors <- rainbow(length(levels(data$GenoCall)))
  names(colors) <- levels(data$GenoCall)
  
  levels(data$GenoCall) <- c(levels(data$GenoCall), "SNP caller genotype")
  data$GenoCall[data$GenoCall == 'df'] <- 'SNP caller genotype'
  
  data %>% ggplot(aes(x= key, y=value, fill= GenoCall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") +
    facet_wrap( ~SNPCall, ncol=1, scales = "free", strip.position = "right")
}
