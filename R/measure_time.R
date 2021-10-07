#' Measuring cromwell tasks time
#' 
#' @param log.file character with log file name in cromwell-workflow-logs directory after 
#' running WDL workflow
#' @param interactive plots an plotly-type graphic with possibility to see other values in an interactive way
#' 
#' @return ggplot boxplot with time spent by each task
#' 
workflow_times <- function(log.file, interactive=FALSE){

  input.file <- read.table(log.file, header = F, sep = "~")
  input.file <- t(input.file)
  
  start <- input.file[grep("job id", input.file)]
  start <- sapply(start, function(x) strsplit(x, " "))
  names(start) <- NULL
  start <- do.call(rbind, start)
  dates.start <- as.POSIXct(paste(start[,2], start[,3]), format="[%m/%d/%Y %H:%M:%OS]")
  start.df <- data.frame(dates.start, id=factor(start[,7], levels = unique(as.character(start[,7]))))
  
  done <- input.file[grep("to Done", input.file)]
  done <- sapply(done, function(x) strsplit(x, " "))
  names(done) <- NULL
  done <- do.call(rbind, done)
  dates.done <- as.POSIXct(paste(done[,2], done[,3]), format="[%m/%d/%Y %H:%M:%OS]")
  done.df <- data.frame(dates.done, id= factor(done[,7], levels = unique(as.character(done[,7]))))

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

