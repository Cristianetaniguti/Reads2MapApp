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
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  colors_dots <- c("blue", "red")
  names(colors_dots) <- c( "true markers", "false positives")
  
  data %>% ggplot(aes(x=Genocall, y=rf)) +
    geom_boxplot(alpha = 0.6, aes(fill=SNPcall)) + 
    geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
               aes(color=factor(real.mks)), show.legend = T) +
    scale_fill_manual(name="SNP call", values = colors) +
    scale_color_manual(name=" ", values = colors_dots)
}

all_size_graph <- function(data, stat){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=ErrorProb, y=value, fill=SNPcall)) +
    geom_boxplot() + geom_hline(yintercept=0, color="red") +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = paste(stat, "cM (haldane)")) + 
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

marker_type_graph <- function(data){
  
  colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  names(colors) <- levels(data$type)
  
  data %>% ggplot(aes(x=ErrorProb, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(name="Marker type", values = colors) + 
    labs(x = "Genotyping method", y = "Number of markers") +
    facet_grid(SNPcall~key) 
}


phases_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=ErrorProb, y=value, fill=SNPcall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent of corrected phases") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

times_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=Genocall, y=times, fill=SNPcall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "Time (seconds)") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

coverage_graph <- function(data){
  
  colors <- c("#58355e", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=ErrorProb, y=coverage, fill=SNPcall)) +
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
  names(colors) <- levels(data$ErrorProb)
  
  data %>% ggplot(aes(x= key, y=value, fill= GenoCall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") +
    facet_wrap( ~SNPcall, ncol=1, scales = "free", strip.position = "right")
}
