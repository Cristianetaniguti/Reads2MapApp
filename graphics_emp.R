# Functions to build graphics for the empirical datas

errorProb_graph_emp <- function(data, genotypes, from){
  
  if(from == "vcf"){
    geno <- data$gt.vcf
    colors <- rainbow(length(levels(geno)))
    names(colors) <- levels(geno)
  } else {
    colors <- rainbow(3)
    names(colors) <-  c("missing", "homozygous", "heterozygote")
    geno <- data$gt.onemap
  }
  
  if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=geno)) + 
      geom_point(alpha = 0.2) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors)
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
      scale_colour_gradient(low = "#70ED57", high = "#F62A2C")
  }
}

ind_size_graph_emp <- function(data){
  
  # colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  # names(colors) <- levels(data$ErrorProb)
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot() + geom_hline(yintercept=0, color="red") +
    labs(x="Genotype call", y = "centimorgan") +
    facet_wrap(key~., ncol=1, scales = "free", strip.position = "right") +
    scale_fill_manual(name="SNP call", values = colors) 
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
  
  colors <- rainbow(length(levels(as.factor(data$value))))
  names(colors) <- levels(as.factor(data$value))
  
  data %>% ggplot(aes(x=GenoCall, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(name="Marker type", values = colors) + 
    labs(x = "Genotyping method", y = "Number of markers") +
    facet_grid(SNPCall~.) 
}

times_graph_emp <- function(data){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,3)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "") +
    facet_grid(key~.)
}

coverage_graph_emp <- function(data){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=coverage, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(coverage,2)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") 
}

filters_graph_emp <- function(data){
  
  colors <- rainbow(length(levels(data$GenoCall)))
  names(colors) <- levels(data$GenoCall)
  
  data %>% ggplot(aes(x= key, y=value, fill= GenoCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_manual(name="Genotype method", values = colors) + 
    labs(x = "", y = "Number of markers") +
    facet_grid(SNPCall~., scales = "free") +
    theme(axis.text.x = element_text(angle = 35, hjust=1))
}
