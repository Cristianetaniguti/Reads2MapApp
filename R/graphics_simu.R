#' Functions to build graphics for the data from simulations
#' 
errorProb_graph <- function(data, genotypes, alpha){
  data <- data.frame(data)
  if(dim(data)[1] == 0) stop("This marker does not exists in this dataset.")
  
  colors <- c("gray", "#FDE725FF", "#440154", "#55C667FF", "#2D708EFF")
  names(colors) <-  c("missing", "homozygous-alt","homozygous-ref", "heterozygous", "homozygous-alt == ref")
  data$pop <- "progeny"
  data$pop[data$ind %in% c("P1", "P2")] <- "parents"
  
  if(genotypes == "simulated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
      geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
      scale_shape_manual(values=c(3, 19))+
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes", shape = "Individuals") +
      scale_color_manual(values = colors) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)), 
             shape = guide_legend(override.aes = list(alpha = 1)))
  }else if(genotypes == "estimated_genotypes"){
    data %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref)) + 
      geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
      scale_shape_manual(values=c(3, 19))+
      labs(title= "Depths",x="ref", y = "alt", color="Genotypes", shape = "Individuals") +
      scale_color_manual(values = colors) +
      guides(colour = guide_legend(override.aes = list(alpha = 1)), 
             shape = guide_legend(override.aes = list(alpha = 1)))
  } else if(genotypes == "estimated_errors"){
    data %>% ggplot(aes(x=ref, y=alt, color=errors)) + 
      geom_point(alpha = alpha, aes(shape=pop), size = 1.5) +
      scale_shape_manual(values=c(3, 19))+
      labs(title= "Depths",x="ref", y = "alt", color="Error rate", shape = "Individuals") +
      scale_colour_gradient(low = "#70ED57", high = "#F62A2C") +
      guides(colour = guide_legend(override.aes = list(alpha = 1)), 
             shape = guide_legend(override.aes = list(alpha = 1)))
  }
}

ind_size_graph <- function(data, data_n){
  colors_dots <- c("blue", "red", "green")
  names(colors_dots) <- c( "true marker", "false positive", "multiallelic")
  
  if(unique(data$fake) == "without-false"){
    legend.y <- "Distance between real and estimated (cM)"
  } else {
    legend.y <- "Distance between markers (cM)"
  }
  
  p1 <- data %>% ggplot() +
    geom_boxplot(aes(x=GenoCall, y = diff, fill=GenoCall), alpha = 0.6) +
    geom_point(aes(x=GenoCall, y = diff,color = factor(real.mks)), 
               position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.5)) +
    scale_fill_viridis_d() +
    facet_wrap(SNPCall~., ncol=2, scales = "fixed", strip.position = "top") +
    scale_color_manual("Markers", values = colors_dots) +    
    guides(fill=FALSE) +
    theme(
      axis.title.x = element_blank(),
      legend.position = "top") + 
    ylab(legend.y)
  
  
  p2 <- data_n %>% ggplot()+
    geom_bar(aes(x=GenoCall, y=`n markers`, fill=GenoCall), stat = "identity") + 
    scale_fill_viridis_d() +
    facet_wrap(.~SNPCall, ncol=2, scales = "fixed", strip.position = "bottom") +
    labs(x = "Genotyping method") +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "none",
    )
  
  p1 / p2
}

all_size_graph <- function(data, data_n,stat, fake){
  
  if(stat == "euclidean_dist" & fake == "with-false")
    stop("In the presence of false positives, euclidean distance can not be calculated. Please, select other option.")
  
  p1 <- data %>% ggplot(aes(x=GenoCall, y=.[[dim(data)[2]]], color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5)  +
    labs(y = colnames(data)[dim(data)[2]]) + 
    facet_wrap(depth~CountsFrom, ncol=2, scales = "fixed", strip.position = "top") +
    theme(
      axis.title.x = element_blank(),
      legend.position = "top"
    )
  
  p2 <- data_n %>% ggplot(aes(x=GenoCall, y=`n markers`, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5)  +
    labs(x = "Genotyping method") + 
    facet_wrap(depth~., ncol=2, scales = "fixed", strip.position = "bottom") +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "none")
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  
  if(n_fam < 20) {
    p1 <- p1 + geom_point(position=position_dodge(width=0.5))  
    p2 <- p2 + geom_point(position=position_dodge(width=0.5))
  } else {
    p1 <- p1 + geom_boxplot()
    p2 <- p2 + geom_boxplot()
  }
  
  p1 / p2  
}

marker_type_graph <- function(data){
  data %>% ggplot(aes(x=real.mks, y = n, fill=value)) +
    geom_bar(stat="identity")  +
    scale_fill_viridis_d(name="Marker type") + 
    labs(x = NULL, y = "Number of markers") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    facet_grid(SNPCall+key~GenoCall) 
}

phases_graph <- function(data){
  p <- data %>% ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "percent of corrected phases") +
    facet_wrap(depth~key, ncol=1, scales = "free", strip.position = "right") 
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)   p + geom_point(position=position_dodge(width=0.5))  
  else p + geom_boxplot()
}

times_graph <- function(data){
  data$value <- as.numeric(data$value)
  p <- data %>% ggplot(aes(x=GenoCall, y=value, color=SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "") +
    facet_wrap(depth~key, ncol=1, scales = "free", strip.position = "right")
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)   p + geom_point(position=position_dodge(width=0.5))  
  else p + geom_boxplot(position=position_dodge(width=0.5))
}

avalSNPs_graph <- function(data){
  p <- data %>% ggplot(aes(x=name, y=value, color= SNPCall)) +
    scale_color_viridis_d(name="SNP call", begin = 0, end = 0.5) + 
    labs(x = "Genotyping method", y = "number of markers") + 
    facet_wrap( ~depth, ncol=1, scales = "free", strip.position = "right") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  
  n_fam <- length(unique(paste0(data$seed,data$depth)))
  if(n_fam < 20)   p + geom_point(position=position_dodge(width=0.5))  
  else p + geom_boxplot(position=position_dodge(width=0.5))
}

filters_graph <- function(data){
  p <- data %>% ggplot(aes(x= name, y=value, color= GenoCall)) +
    scale_color_viridis_d(name="Genotype call") + 
    labs(x = "", y = "number of markers") +
    facet_wrap(SNPCall~CountsFrom, ncol=2, scales = "fixed", strip.position = "top")+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
  
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
    facet_grid(key~SNPCall+GenoCall, scales = "fixed") +
    theme_bw() 
}

marker_type_probs <- function(data_plot_par){
  if(!is.data.frame(data_plot_par)) stop("The probabilities can not be calculated in the presence of false positives.\n
                              If you set option without-false, then there are no biallelic/multiallelic marker in this dataset. 
                              Please, select other option.")
  
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
    p1 <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == labels_real[i] & 
                                                            est == labels_est[i] ) %>%
      ggplot(aes(x=GenoCall, y = value, color = depth)) +
      geom_point(position = position_dodge(width=0.7)) +
      scale_color_viridis_d(begin = 0, end = 0.5)+
      theme_bw() + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= labels_title1[i], x = "Genotype caller", color = "mean depth")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=12))
    
    p2 <- data_plot_par %>% pivot_longer(cols=8) %>% filter(simu == labels_real[i] & 
                                                            est != labels_est[i]) %>%
      ggplot(aes(x=GenoCall, y = value, color = depth)) +
      geom_point(position = position_dodge(width=0.7)) +
      scale_color_viridis_d(begin = 0, end = 0.5)+
      theme_bw() + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= labels_title2[i], x = "Genotype caller", color = "mean depth")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=12))
    
    p_comb[[i]] <- ggarrange(p1, p2, common.legend = T, widths = c(3,10), heights = c(16,16))
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
  p3_comb <- ggarrange(p3, widths = 13, heights = 16)
  
  
  p_comb[[1]]/p_comb[[2]]/p_comb[[3]]/p3_comb
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
      theme_bw() + 
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
      theme_bw() + 
      facet_grid(SNPCall + CountsFrom ~simu + est, scales = "fixed") +
      labs(title= labels_title2[i], x = "Genotype caller", shape = "mean depth", color = "")+
      scale_color_manual(labels=c("mean error rate", 
                                  expression("P(E|M"*intersect("R)"))), values = c("red", "blue"))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
            axis.title.y = element_blank(), legend.position = "top",
            legend.text=element_text(size=11))
    
    p_comb[[i]] <- ggarrange(p1, p2, common.legend = T, widths = c(4,8), heights = c(16,16))
  }

  p_comb[[1]]/p_comb[[2]]/p_comb[[3]]
}
