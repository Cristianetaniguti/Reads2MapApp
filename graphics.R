# Write the functions here

errorProb_graph <- function(data){
  data$methGT <- factor(data$methGT, labels = c("missing", "AA", "AB", "BB"), levels = c(0,1,2,3))
  
  colors <- c("#58355e", "#4D9DE0", "#ADE25D", "#FE5D26")
  names(colors) <- levels(data$methGT)
  
  data %>% ggplot(aes(x=ref, y=alt, color=methGT)) + 
    geom_point() +
    labs(title= "Depths",x="ref", y = "alt", color="Genotypes") +
    scale_colour_manual(name="Genotypes", values = colors)

}

maps_graph <- function(data){

  data$ErrorProb <- as.factor(data$ErrorProb)
  
  # colors <- c("#55DDE0", "#33658A", "#006D68", "#F6AE2D", "#F26419")
  # names(colors) <- levels(data$ErrorProb)
  
  colors <- c("#FE5D26", "#4D9DE0")
  names(colors) <- levels(data$SNPcall)
  
  data %>% ggplot(aes(x=ErrorProb, y=rf, fill=SNPcall)) +
    geom_boxplot() + geom_hline(yintercept=0, color="red")
}
