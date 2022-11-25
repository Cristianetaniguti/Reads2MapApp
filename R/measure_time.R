#' Measuring cromwell tasks time
#' 
#' @param log.file character with log file name in cromwell-workflow-logs directory after 
#' running WDL workflow
#' @param interactive plots an plotly-type graphic with possibility to see other values in an interactive way
#' 
#' @import ggplot2
#' @import plotly
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
  
  tot.df <- merge(done.df, start.df, by = "id")
  
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


transform_day <- function(x){
  day <- as.numeric(x[1])*24
  split.t <- strsplit(x[2], ":")
  new.h <- as.numeric(sapply(split.t, "[[", 1)) + day
  paste0(new.h, ":",sapply(split.t, "[",2),":",sapply(split.t, "[",3))
}


#' Convert data obtained by efficiency script to 
#' tables
#' 
#' @param infos_seff output of seff bash command in HPRC
#' @param jobs data.frame with first column being the task name and the second the job id
#' 
#' @import lubridate
#'
#' @export
efficiency_table <- function(infos_seff, jobs){
  
  df <- read.table(infos_seff, sep = "\n")
  jobs_df <- read.table(jobs, sep = " ")
  
  cpu_used <- sapply(strsplit(df$V1[grep("CPU Utilized", df$V1)],": "), "[[",2)
  time <- sapply(strsplit(df$V1[grep("Job Wall-clock time", df$V1)],": "), "[[",2)
  
  cpu_used[grep(cpu_used, pattern = "-")] <- sapply(strsplit(cpu_used[grep(cpu_used, pattern = "-")], "-"), transform_day)
  time[grep(cpu_used, pattern = "-")] <- sapply(strsplit(time[grep(time, pattern = "-")], "-"), transform_day)
  
  df.infos <- data.frame(
    cores = sapply(strsplit(df$V1[grep("Cores", df$V1)], ":"), "[[",2),
    cpu_used = period_to_seconds(hms(cpu_used)),
    cpu_eff = sapply(strsplit(df$V1[grep("CPU Efficiency", df$V1)],": "), "[[",2),
    time = period_to_seconds(hms(time)),
    mem_used_GB = sapply(strsplit(df$V1[grep("Memory Utilized", df$V1)],": "), "[[",2),
    mem_eff = sapply(strsplit(df$V1[grep("Memory Efficiency", df$V1)],": "), "[[",2)
  )
  
  rm.lines <- which(is.na(as.numeric(jobs_df$V2)))
  if(length(rm.lines) >0)
    jobs_df <- jobs_df[-rm.lines,]
  
  df.infos <- cbind(jobs_df, df.infos)
  
  df.infos$mem_GB <- sapply(strsplit(df.infos$mem_eff, "of"),"[[",2)
  
  idx.mb <- grep(df.infos$mem_GB, pattern = "MB")
  idx.kb <- grep(df.infos$mem_GB, pattern = "KB")
  
  df.infos$mem_GB <- as.numeric(sapply(strsplit(df.infos$mem_GB, " "), "[[",2))
  df.infos$mem_GB[idx.mb] <- df.infos$mem_GB[idx.mb]/1000
  df.infos$mem_GB[idx.kb] <- df.infos$mem_GB[idx.kb]/1000000
  
  idx.mb <- grep(df.infos$mem_used, pattern = "MB")
  idx.kb <- grep(df.infos$mem_used, pattern = "KB")
  
  df.infos$mem_used_GB <- as.numeric(sapply(strsplit(df.infos$mem_used_GB, " "), "[[",1))
  df.infos$mem_used_GB[idx.mb] <- df.infos$mem_used_GB[idx.mb]/1000
  df.infos$mem_used_GB[idx.kb] <- df.infos$mem_used[idx.kb]/1000000
  
  df.infos$cpu <- sapply(strsplit(df.infos$cpu_eff, "of"),"[[",2)
  df.infos$cpu <- gsub(df.infos$cpu, pattern = " core-walltime", replacement = "")
  
  df.infos$cpu <- sapply(strsplit(df.infos$cpu, " "), "[[",2)
  df.infos$cpu[grep(df.infos$cpu, pattern = "-")] <- sapply(strsplit(df.infos$cpu[grep(df.infos$cpu, pattern = "-")], "-"), transform_day)
  
  df.infos$cpu <- period_to_seconds(hms(df.infos$cpu))
  
  df.infos <- df.infos[,-c(5,8)]
  colnames(df.infos)[1:2] <- c("tasks","job_id")
  
  
  return(df.infos)
}

#' HPC efficiency graphics
#' 
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @import plotly
#' 
#' @export
efficiency_graphics_perc <- function(eff_table, interactive=TRUE){
  p <- eff_table %>%
    mutate(mem = (mem_used_GB/mem_GB)*100 , cpu = (cpu_used/cpu)*100) %>%
    select(tasks, mem, cpu) %>% pivot_longer(cols = c(2:3), names_to="Usage (%)", values_to = "percentage") %>%
    ggplot(aes(x=tasks, y=percentage, fill=`Usage (%)`, color = `Usage (%)`)) +   coord_flip() +
    geom_boxplot()  + theme_bw()
  
  if(interactive){
    p <- ggplotly(p)
  }
  
  return(p)
}

#' HPC efficiency graphics
#' 
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @import plotly
#' 
#' @export
efficiency_graphics_cpu <- function(eff_table, interactive=TRUE){
  p <- eff_table %>%
    select(tasks, cpu_used) %>% pivot_longer(cols = c(2), values_to = "usage (min)") %>%
    ggplot(aes(x=tasks, y = `usage (min)`/60)) +   coord_flip() +
    geom_boxplot() + ylab("usage cpus (min)") + theme_bw()
  
  if(interactive){
    p <- ggplotly(p)
  }
  
  return(p)
  
}

#' HPC efficiency graphics
#' 
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' @import plotly
#' 
#' @export
efficiency_graphics_mem <- function(eff_table, interactive=TRUE){
  p <- eff_table %>%
    select(tasks, mem_used_GB) %>% pivot_longer(cols = c(2), values_to="usage (GB)") %>%
    ggplot(aes(x=tasks, y=`usage (GB)`)) +   coord_flip() +
    geom_boxplot()  + ylab("usage mem (GB)")  + theme_bw()
  
  if(interactive){
    p <- ggplotly(p)
  }
  
  return(p)
}

#' HPC efficiency graphics
#' 
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#' 
#' @export
efficiency_graphics_mem_cpu <- function(eff_table, reescale = 15){
  p <- eff_table %>%
    select(tasks, cpu_used, mem_used_GB) %>%
    ggplot(aes(x=tasks)) +   coord_flip() +
    geom_boxplot(aes(y = (cpu_used/60)/reescale), color = "blue", position = position_jitter(w = 0.05, h = 0)) +  
    geom_boxplot(aes(y=mem_used_GB), color = "red") +
    scale_y_continuous(
      
      # Features of the first axis
      name = "Memory Utilized (GB)",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*reescale, name="CPU Utilized (min)")
    ) +
    theme_bw() +
    theme(axis.text.x.top = element_text(colour="#005AB5"),
          axis.text.x.bottom = element_text(colour="#DC3220"),
          axis.title.x.top = element_text(colour="#005AB5"),
          axis.title.x.bottom = element_text(colour="#DC3220"))
}

