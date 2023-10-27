library(ggpubr)
library(ggplot2)
library(ggrepel)
library(tidyr)

colors <- c("gray", "#5289C7", "#4EB265", "#882E72")
names(colors) <-  c("missing", "homozygous-alt","homozygous-ref", "heterozygous")

# 
# temp <- load("gatk_BAM_polyrad_Chr10_3940585_estimated_seed90_depth20_bi_GQ_dev_case1.RData")
# case1.3.estimated <- get(temp)
# temp <- load("gatk_BAM_polyrad_Chr10_3940585_real_seed90_depth20_bi_GQ_dev_case1.RData")
# case1.3.real <- get(temp)
# temp <- load("gatk_VCF_polyrad_Chr10_3940585_estimated_seed90_depth20_bi_GQ_dev_case1.RData")
# case1.4.estimated <- get(temp)
# temp <- load("gatk_VCF_polyrad_Chr10_3940585_real_seed90_depth20_bi_GQ_dev_case1.RData")
# case1.4.real <- get(temp)

# # Case 1.3
# data <- case1.3.real$data
# p_temp1 <- data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
#   geom_point(aes(shape=pop), size = 1.5) +
#   scale_shape_manual(values=c(3, 1))+
#   labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
#   scale_color_manual(values = colors) +
#   guides(colour = guide_legend(override.aes = list(alpha = 1)), 
#          shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("True") + theme(legend.key.size = unit(1, 'cm'), #change legend key size
#                                                                                                       legend.key.height = unit(1, 'cm'), #change legend key height
#                                                                                                       legend.key.width = unit(1, 'cm'), #change legend key width
#                                                                                                       legend.title = element_text(size=12), #change legend title font size
#                                                                                                       legend.text = element_text(size=10)) +
#   guides(shape = guide_legend(override.aes = list(size = 5)), 
#          color = guide_legend(override.aes = list(size = 5)))
# 
# 
# data <- case1.3.estimated$data
# p_temp2 <- data %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref)) + 
#   geom_point(aes(shape=pop), size = 1.5) +
#   scale_shape_manual(values=c(3, 1))+
#   labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
#   scale_color_manual(values = colors) +
#   guides(colour = guide_legend(override.aes = list(alpha = 1)), 
#          shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("Estimated") + theme(legend.key.size = unit(1, 'cm'), #change legend key size
#                                                                                                            legend.key.height = unit(1, 'cm'), #change legend key height
#                                                                                                            legend.key.width = unit(1, 'cm'), #change legend key width
#                                                                                                            legend.title = element_text(size=12), #change legend title font size
#                                                                                                            legend.text = element_text(size=10)) +
#   guides(shape = guide_legend(override.aes = list(size = 5)), 
#          color = guide_legend(override.aes = list(size = 5)))
# 
# 
# p <- list(p_temp2, p_temp1) 
# plot <- ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
# plot1 <- annotate_figure(plot, top = text_grob("B) Depth 10 - GATK - polyrad - Counts from BAM", 
#                                                color = "black", face = "bold", size = 14, hjust = 0.7))
# 
# 
# # Case 1.4
# data <- case1.4.real$data
# p_temp1 <- data %>% ggplot(aes(x=ref, y=alt, color=gabGT)) + 
#   geom_point(aes(shape=pop), size = 1.5) +
#   scale_shape_manual(values=c(3, 1))+
#   labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
#   scale_color_manual(values = colors) +
#   guides(colour = guide_legend(override.aes = list(alpha = 1)), 
#          shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("True") + theme(legend.key.size = unit(1, 'cm'), #change legend key size
#                                                                                                       legend.key.height = unit(1, 'cm'), #change legend key height
#                                                                                                       legend.key.width = unit(1, 'cm'), #change legend key width
#                                                                                                       legend.title = element_text(size=12), #change legend title font size
#                                                                                                       legend.text = element_text(size=10)) +
#   guides(shape = guide_legend(override.aes = list(size = 5)), 
#          color = guide_legend(override.aes = list(size = 5)))
# 
# p_temp2 <- data %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref)) + 
#   geom_point(aes(shape=pop), size = 1.5) +
#   scale_shape_manual(values=c(3, 1))+
#   labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
#   scale_color_manual(values = colors) +
#   guides(colour = guide_legend(override.aes = list(alpha = 1)), 
#          shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("Estimated") + theme(legend.key.size = unit(1, 'cm'), #change legend key size
#                                                                                                            legend.key.height = unit(1, 'cm'), #change legend key height
#                                                                                                            legend.key.width = unit(1, 'cm'), #change legend key width
#                                                                                                            legend.title = element_text(size=12), #change legend title font size
#                                                                                                            legend.text = element_text(size=10)) +
#   guides(shape = guide_legend(override.aes = list(size = 5)), 
#          color = guide_legend(override.aes = list(size = 5)))
# 
# p <- list(p_temp2, p_temp1) 
# plot <- ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
# plot2 <- annotate_figure(plot, top = text_grob("A) Depth 10 - GATK - polyrad - Counts from VCF", 
#                                                color = "black", face = "bold", size = 14, hjust = 0.7))
# 
# case1 <- ggarrange(plot2,plot1, ncol = 1)


temp <- load("multi_GQ_updog_BAM_depth20_estimated_Chr10_3399436_seed31_case2.RData")
case2.3.estimated <- get(temp)
temp <- load("multi_GQ_updog_BAM_depth20_real_Chr10_3399436_seed31_case2.RData")
case2.3.real <- get(temp)
temp <- load("multi_GQ_updog_VCF_depth20_estimated_Chr10_3399436_seed31_case2.RData")
case2.4.estimated <- get(temp)
temp <- load("multi_GQ_updog_VCF_depth20_real_Chr10_3399436_seed31_case2.RData")
case2.4.real <- get(temp)

# Case 2.3
data <- case2.3.real$data
colors_temp <- colors[match(names(table(data$gabGT)), names(colors))]
p_temp1 <- data %>% droplevels() %>% ggplot(aes(x=ref, y=alt, color=gabGT, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1), drop=TRUE)+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp, drop=TRUE) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("True") + 
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2)) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))
  
colors_temp <- colors[match(names(table(data$gt.onemap.alt.ref)), names(colors))]
p_temp2 <- data %>% droplevels() %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("Estimated") + 
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2)) +
  geom_label_repel (aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

p <- list(p_temp2, p_temp1) 
plot <- ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
plot1 <- annotate_figure(plot, top = text_grob("B) Depth 20 - GATK - updog - Counts from BAM", 
                                               color = "black", face = "bold", size = 14, hjust = 0.6))

ggsave(plot1, filename = "geno_error1_B.png", width = 7.5, height = 5)

# Case 2.4
data <- case2.4.real$data
colors_temp <- colors[match(names(table(data$gabGT)), names(colors))]
p_temp1 <- data %>% droplevels() %>% ggplot(aes(x=ref, y=alt, color=gabGT, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("True") + 
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2)) +
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

colors_temp <- colors[match(names(table(data$gt.onemap.alt.ref)), names(colors))]
p_temp2 <- data %>% droplevels() %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp, drop=FALSE) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("Estimated") + 
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2)) +
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE , box.padding = 0.3) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

p <- list(p_temp2, p_temp1) 
plot <- ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
plot2 <- annotate_figure(plot, top = text_grob("A) Depth 20 - GATK - updog - Counts from VCF", 
                                               color = "black", face = "bold", size = 14, hjust = 0.6)) 

ggsave(plot2, filename = "geno_error1_A.png", width = 7.5, height = 5)

#case2 <- ggarrange(plot2,plot1, ncol = 1)

#ggsave(case2, filename = "case2.png", width = 170, height = 225, units = "mm")


temp <- load("bi_GQ_polyrad_VCF_depth20_estimated_seed90_case3.RData")
case3.1.estimated <- get(temp)
temp <- load("bi_GQ_polyrad_VCF_depth20_real_seed90_case3.RData")
case3.1.real <- get(temp)
temp <- load("freebayes_depth10_multi_supermassa_VCF_estimated.RData")
case2.3.estimated <- get(temp)
temp <- load("freebayes_depth10_multi_supermassa_VCF_real.RData")
case2.3.real <- get(temp)

# Case 3.1
data <- case3.1.real$data
colors_temp <- colors[match(names(table(data$gabGT)), names(colors))]
p_temp1 <- data %>% ggplot(aes(x=ref, y=alt, color=gabGT, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("True") +
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2))+
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

p_temp2 <- data %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + 
  ggtitle("Estimated") + 
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2))+
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

p <- list(p_temp2, p_temp1) 
plot <- ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
plot1 <- annotate_figure(plot, top = text_grob("B) Depth 20 - GATK - polyRAD - Counts from VCF", 
                                               color = "black", face = "bold", size = 14, hjust = 0.57))

ggsave(plot1, filename = "geno_error2_B.png", width = 7.5, height = 5)

# Case 3.2
data <- case2.3.real$data
colors_temp <- colors[match(names(table(data$gabGT)), names(colors))]
p_temp1 <- data %>% ggplot(aes(x=ref, y=alt, color=gabGT, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("True") +
  theme(legend.position="none") +
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2))+
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

p_temp2 <- data %>% ggplot(aes(x=ref, y=alt, color=gt.onemap.alt.ref, label = ind)) + 
  geom_point(aes(shape=pop), size = 1.5) +
  scale_shape_manual(values=c(3, 1))+
  labs(x="reference allele counts", y = "alternative allele counts", color="Genotypes", shape = "Individuals") +
  scale_color_manual(values = colors_temp) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)), 
         shape = guide_legend(override.aes = list(alpha = 1))) + theme_bw() + ggtitle("Estimated") + 
  theme(legend.position="none") +
  guides(shape = guide_legend(override.aes = list(size = 2.5), nrow=2), 
         color = guide_legend(override.aes = list(size = 2.5), nrow=2))+
  geom_label_repel(aes(label=ifelse(pop == "parents",as.character(ind),'')), show.legend = FALSE) +
  xlim(c(0,150)) + ylim(c(0, 150)) + theme(legend.spacing.x = unit(10, "pt"))

p <- list(p_temp2, p_temp1) 
plot <- ggarrange(plotlist = p, common.legend = T, legend = "bottom") 
plot2 <- annotate_figure(plot, top = text_grob("A) Depth 10 - freebayes - SuperMASSA - Counts from VCF", 
                                               color = "black", face = "bold", size = 14, hjust = 0.48))

ggsave(plot2, filename = "geno_error2_A.png", width = 7.5, height = 5)

# case3 <- ggarrange(plot2, plot1, ncol = 1)
# 
# ggsave(case3, filename = "case3.png", width = 170, height = 225, units = "mm")
