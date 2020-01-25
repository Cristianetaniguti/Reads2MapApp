# Functions to build graphics for the data from simulations

errorProb_graph <- function(data, genotypes){
  
  colors <- c("#58355e", "#4D9DE0", "#ADE25D")
  names(colors) <-  c("missing", "homozygous", "heterozygote")
  
  if(genotypes == "real_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
      geom_point() +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors)
  }else if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=methGT)) + 
      geom_point() +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors)
  } else if(genotypes == "estimated_errors"){
    data %>% ggplot(aes(x=ref, y=alt, color=errors)) + 
      geom_point() +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_gradient(low = "#70ED57", high = "#F62A2C")
  }
}

ind_size_graph <- function(data){
  
  # colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  # names(colors) <- levels(data$ErrorProb)
  
  colors_dots <- c("blue", "red")
  names(colors_dots) <- c( "true markers", "false positives")
  
  data %>% ggplot(aes(x=Genocall, y=rf)) +
    geom_boxplot(alpha = 0.6) + 
    geom_point(position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.5),aes(color = factor(real.mks))) +
    facet_grid(SNPcall~.) +
    scale_color_manual("Markers", values = colors_dots)
}

all_size_graph <- function(data, stat){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=Genocall, y=value, fill=SNPcall)) +
    geom_boxplot() + geom_hline(yintercept=0, color="red") +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = paste(stat, "cM (haldane)")) + 
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

marker_type_graph <- function(data){
  
  colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  names(colors) <- levels(data$type)
  
  data %>% ggplot(aes(x=Genocall, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(name="Marker type", values = colors) + 
    labs(x = "Genotyping method", y = "Number of markers") +
    facet_grid(key~SNPcall) 
}


phases_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=Genocall, y=value, fill=SNPcall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent of corrected phases") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

times_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=Genocall, y=value, fill=SNPcall)) +
    geom_boxplot(position=position_dodge())  +
    geom_text(aes(label= round(value,1)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "") +
    facet_wrap( key~depth, ncol=1, scales = "free", strip.position = "right")
}

coverage_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=Genocall, y=coverage, fill=SNPcall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

avalSNPs_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=key, y=value, fill= SNPcall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") + 
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

filters_graph <- function(data){
  
  colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  names(colors) <- levels(data$Genocall)
  
  data %>% ggplot(aes(x= key, y=value, fill= Genocall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") +
    facet_wrap( ~SNPcall, ncol=1, scales = "free", strip.position = "right")
}
