# Write the functions here

errorProb_graph <- function(data){
  data %>% ggplot(aes(x=ref, y=alt, color=as.factor(methGT))) + 
    geom_point(size=0.5) +
    labs(title= "Depths",x="ref", y = "alt", color="Genotypes") 
    #xlim(0,~{depth}*2) + ylim(0,~{depth}*2)
}
