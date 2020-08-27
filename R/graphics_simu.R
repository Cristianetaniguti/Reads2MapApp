#' Functions to build graphics for the data from simulations
#' 
errorProb_graph <- function(data, genotypes){
  
  colors <- c("black", "blue", "red", "green")
  names(colors) <-  c("missing", "homozygote-alt","homozygote-ref", "heterozygote")
  
  if(genotypes == "simulated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
      geom_point(alpha = 0.3) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
  }else if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=methGT)) + 
      geom_point(alpha = 0.3) +
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
      scale_colour_manual(name="Genotypes", values = colors)+
      guides(colour = guide_legend(override.aes = list(alpha = 1)))
  } else if(genotypes == "estimated_errors"){
    data %>% ggplot(aes(x=ref, y=alt, color=errors)) + 
      geom_point(alpha = 0.3) +
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
  
  data %>% ggplot(aes(x=GenoCall, y=value)) +
    geom_boxplot(alpha = 0.6) + 
    geom_point(position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.5),aes(color = factor(real.mks))) +
    facet_wrap( SNPCall~key, ncol=1, scales = "free", strip.position = "right") +
    scale_color_manual("Markers", values = colors_dots) +    
    labs(x = "Genotyping method", y = "") 
}

all_size_graph <- function(data, stat){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot() + 
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = " ") + 
    facet_wrap(depth~key, ncol=1, scales = "free", strip.position = "right")
}

marker_type_graph <- function(data){
  
  colors <- rainbow(4)
  names(colors) <- levels(data$type)
  
  data %>% ggplot(aes(x=GenoCall, y = n, fill=value)) +
    geom_bar(stat="identity", position=position_dodge())  +
    scale_fill_manual(name="Marker type", values = colors) + 
    labs(x = "Genotyping method", y = "Number of markers") +
    facet_grid(key~SNPCall) 
}


phases_graph <- function(data){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot()  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent of corrected phases") +
    facet_wrap(depth~key, ncol=1, scales = "free", strip.position = "right")
}

times_graph <- function(data){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  data$value <- as.numeric(data$value)
  data %>% ggplot(aes(x=GenoCall, y=value, fill=SNPCall)) +
    geom_boxplot(position=position_dodge())  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "") +
    facet_wrap(depth~key, ncol=1, scales = "free", strip.position = "right")
}

coverage_graph <- function(data){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=GenoCall, y=coverage, fill=SNPCall)) +
    geom_boxplot(position=position_dodge())  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "percent covered") +
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right")
}

avalSNPs_graph <- function(data){
  
  colors <- rainbow(length(levels(data$SNPCall)))
  names(colors) <- levels(data$SNPCall)
  
  data %>% ggplot(aes(x=key, y=value, fill= SNPCall)) +
    geom_boxplot(position=position_dodge())  +
    scale_fill_manual(name="SNP call", values = colors) + 
    labs(x = "Genotyping method", y = "number of markers") + 
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

overview_graph <- function(df_overview, depth, reescale = 30){
  
  df_overview$`Number of non-informative markers in map`  <- df_overview$`Number markers in map`*((df_overview$`Percentage of non-informative markers`)/100)
  df_overview$`Number of informative markers in map` <-  df_overview$`Number markers in map` - df_overview$`Number of non-informative markers in map`
  
  y_lim_nmks <- max(df_overview$`Number markers in map`)
  mycolors <- brewer.pal(12, "Paired")
  ps <- df_overview %>% filter(depth == depth) %>% filter(Value == "value") %>%
    rename(., "Genotypes (kappa)" = "Kappa's coefficient for genotypes", 
           "Phases (kappa)" = "Kappa's coefficient for phases",
           "Marker types (kappa)" = "Kappa's coefficient for marker types", 
           "Breakpoints (Kendall)" = "Kendall's coefficient of concordance for breakpoints") %>%
    gather(key, value, - SNPCall, -GenoCall, -depth, -seed, -CountsFrom, -Value) %>%
    filter(key %in% c("Genotypes (kappa)", "Phases (kappa)",
                      "Marker types (kappa)", 
                      "Breakpoints (Kendall)")) %>%
    split(., list(.$CountsFrom,.$SNPCall)) %>% 
    lapply(., function(x) ggplot(x, aes(x=GenoCall, y=as.numeric(value), fill=key)) + 
             geom_boxplot()+
             theme(axis.text.x = element_blank()) +
             scale_fill_manual(values=mycolors[c(2,4,6,8)]) +
             labs(title= paste(x$SNPCall[1], "-", x$CountsFrom[1]),
                  x=element_blank(), y = "coef value", fill= "Coeficients") + 
             # theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm"),
             #       legend.key.size = unit(0.8, "cm"),
             #       legend.key.width = unit(1,"cm")) +
             scale_y_continuous(sec.axis = sec_axis(~., name = " ")) +
             ylim(0,1)
    )
  
  n_tot <-df_overview %>% filter(depth == depth) %>% filter(Value == "value") %>%
    select(SNPCall, GenoCall, seed, CountsFrom, "Number markers in map", "Number of non-informative markers in map") %>%
    rename(., "Non-informative" = "Number of non-informative markers in map") %>%
    gather(key, value, - SNPCall, -GenoCall, -seed, -CountsFrom)  %>%
    group_by(., SNPCall, GenoCall, CountsFrom, key) %>%
    summarise(mean2 = mean(value),
              se2 = sd(value)/sqrt(length(value))) %>% ungroup()
  
  n_tot$key[n_tot$key == "Number markers in map"] <- "Informative"
  
  map_size <- df_overview %>% filter(depth == depth) %>% filter(Value == "value") %>%
    select(SNPCall, GenoCall, seed, CountsFrom, "Map size (cM)") %>%
    gather(key3, value, - SNPCall, -GenoCall, -seed, -CountsFrom)  %>%
    group_by(., SNPCall, GenoCall, CountsFrom, key3) %>%
    summarise(mean3 = mean(value),
              se3 = sd(value)/sqrt(length(value))) %>% ungroup() 
  
  y_lim_cm <- max(map_size$mean3)
  
  ps2 <- df_overview %>% filter(depth == depth) %>% filter(Value == "value") %>%
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
  
  p1 <- ggarrange(plotlist = list(ps[[1]], ps[[2]]), ncol = 2, common.legend = T, legend = "right")
  p2 <- ggarrange(plotlist = list(ps2[[1]], ps2[[2]]), ncol = 2, common.legend = T, legend = "right")
  p3 <- ggarrange(plotlist = list(ps[[3]], ps[[4]]), ncol = 2, common.legend = T, legend = "right")
  p4 <- ggarrange(plotlist = list(ps2[[3]], ps2[[4]]), ncol = 2, common.legend = T, legend = "right")
  
  p_joint <- ggarrange(plotlist = list(p1,p2,p3,p4), ncol = 1)
  return(p_joint)
}


