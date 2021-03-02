#' Measuring cromwell tasks time
#' 
#' @param log.file character with log file name in cromwell-workflow-logs directory after 
#' running WDL workflow
#' @param interactive plots an plotly-type graphic with possibility to see other values in an interactive way
#' 
#' @return ggplot boxplot with time spent by each task
#' 
workflow_times <- function(log.file, interactive=FALSE){
  system(paste("grep 'job id'",log.file, "> temp.starting"))
  system(paste("grep 'to Done'",log.file, "> temp.done"))
  
  start <- read.table("temp.starting")
  gsub(pattern = "[[]", replacement = "", x = start$V2)
  dates.start <- as.POSIXct(paste(start$V2, start$V3), format="[%m/%d/%Y %H:%M:%OS]")
  start.df <- data.frame(dates.start, id=factor(start$V7, levels = unique(as.character(start$V7))))
  
  done <- read.table("temp.done")
  dates.done <- as.POSIXct(paste(done$V2, done$V3), format="[%m/%d/%Y %H:%M:%OS]")
  done.df <- data.frame(dates.done, id= factor(done$V7, levels = unique(as.character(done$V7))))
  
  file.remove(c("temp.starting", "temp.done"))
  
  tot.df <- merge(done.df, start.df)

  tot.df <- cbind(tot.df, diff=difftime(tot.df[,2], tot.df[,3], units='mins'))
  
  tot.df <- cbind(task=sapply(strsplit(as.character(tot.df$id), ":"), "[", 1), tot.df)
  means <- aggregate(diff ~  task, tot.df, mean)
  
  means$diff <- round(means$diff,2)
  max.value <- max(as.numeric(tot.df$diff))
  
  tot.df$task <- sapply(strsplit(tot.df$task, "[.]"), "[[",2)
  
  p <- ggplot(tot.df, aes(x=task, y=diff)) + geom_boxplot() +
    guides(fill=FALSE) + coord_flip() + ylab("Time in minutes") + scale_y_continuous() 
  #  geom_text(data = means, aes(label =  paste0("mean: ",diff), y = diff + max.value/2))
  
  if(interactive){
    p <- ggplotly(p, height = 1000)
  }
  
  return(p)
}

