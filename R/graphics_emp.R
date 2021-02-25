#' Functions to build graphics for the empirical datas
#' 
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
  
  colors <- rainbow(2)
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
  
  data %>% ggplot(aes(x=GenoCall, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_viridis_d() + 
    labs(x = "Genotyping method", y = "Number of markers", fill="Marker type") +
    facet_grid(SNPCall~.) 
}

times_graph_emp <- function(data){

  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,3)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_viridis_d(begin=0, end = 0.6) + 
    labs(x = "Genotyping method", y = "", fill="SNP call") +
    facet_grid(key~., scales = "free")
}

coverage_graph_emp <- function(data){
  
  colors <- rainbow(2)
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=coverage, fill=SNPCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(coverage,2)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") 
}

filters_graph_emp <- function(data){
  
  data %>% ggplot(aes(x= key, y=value, fill= GenoCall)) +
    geom_bar(stat="identity", position=position_dodge())  +
    geom_text(aes(label= round(value,2)), position=position_dodge(width=0.9), vjust=-0.25) +
    scale_y_continuous(expand = c(.1,.1)) +
    scale_fill_viridis_d() + 
    labs(x = "", y = "Number of markers", fill = "Genotype method") +
    facet_grid(SNPCall~., scales = "free") +
    theme(axis.text.x = element_text(angle = 35, hjust=1))
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
