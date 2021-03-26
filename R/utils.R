test_geno <- function(global, error){
  if(global){
    geno <- paste0(error, 0.05)
    if(any(error %in% "OneMap_version2"))
      geno[which(error == "OneMap_version2")] <- "SNPCaller0.05"
    if(any(error %in% "gusmap"))
      stop(safeError("Gusmap do not allow to change the error rate. Please, select other option."))
  } else {
    geno <- error
  }
  return(geno)
}

test_geno_with_gus <- function(global, error){
  if(global){
    if(error == "OneMap_version2"){
      geno <- paste0("SNPCaller", 0.05)
    } else {
      geno <- paste0(error, 0.05)
    }
  } else {
    geno <- error
  }
  return(geno)
}

stop_bam <- function(countsfrom, error){
  if(countsfrom == "bam" & (error == "OneMap_version2" | error == "SNPCaller")){
    stop(safeError("This option is not available. The SNP callers performs together the SNP 
                   and genotype calling using the same read counts, we did not find a way to 
                   substitute the depths already used. Please select other option."))
  }
}

perfumaria <- function(data){
  
  if(any(colnames(data) %in% "SNPCall")){
    snpcall <- c(GATK = "gatk", freebayes = "freebayes")
    data$SNPCall <- names(snpcall)[match(data$SNPCall, snpcall)]
  }
  
  if(any(colnames(data) %in% "GenoCall")){
    genocall <- c(polyRAD = "polyrad", 
                  SuperMASSA = "supermassa", 
                  GUSMap = "gusmap", 
                  updog = "updog", 
                  OneMap_version2 = "OneMap_version2", 
                  `freebayes/GATK` = "SNPCaller",
                  `polyRAD (5%)` = "polyrad0.05", 
                  `updog (5%)` = "updog0.05", 
                  `SuperMASSA (5%)` = "supermassa0.05", 
                  `freebayes/GATK (5%)` = "SNPCaller0.05")
    
    data$GenoCall <- names(genocall)[match(data$GenoCall, genocall)]
  }
  
  if(any(colnames(data) %in% "CountsFrom")){
    countsfrom <- c(BAM = "bam", VCF = "vcf")
    data$CountsFrom <- names(countsfrom)[match(data$CountsFrom, countsfrom)]
  }
  
  if(any(colnames(data) %in% "depth")){
    depth.n <- paste("Depth", unique(data$depth))
    depth <- unique(data$depth)
    names(depth) <- depth.n
    data$depth <- names(depth)[match(data$depth, depth)]
  }
  
  return(data)
}


# Function by Thiago Oliveira
prob_f <- function(data, method, estimate, gabarito){
  data <-data.frame(data)
  n_method <- length(levels(data[,method]))
  n_gab <- length(levels(data[,gabarito]))
  n_estimate <- length(levels(data[,estimate]))
  p <- array(data=NA, dim=c(n_estimate, n_gab, n_method), 
             dimnames = list(paste("Est:",levels(data[,estimate])),
                             paste("Real:",levels(data[,gabarito])),
                             levels(data[,method])))
  for(i in 1:n_estimate){
    for(j in 1:n_gab){
      for(k in 1:n_method){
        num <- nrow(data[data[,method]==levels(data[,method])[k] &
                           data[,estimate]==levels(data[,estimate])[i] & 
                           data[,gabarito]==levels(data[,gabarito])[j],])
        den <- nrow(data[data[,method]==levels(data[,method])[k] & 
                           data[ , gabarito] == levels(data[, gabarito])[j],]) 
        if(num == 0){
          p[i,j,k] <- 0
        }else{
          p[i,j,k] <- num/den
        }
      }
    }
  }
  p
  return(p)
}


prob_error <- function(data, method, estimate, gabarito, error){
  data <-data.frame(data)
  n_method <- length(levels(data[,method]))
  n_gab <- length(levels(data[,gabarito]))
  n_estimate <- length(levels(data[,estimate]))
  
  p <- array(data=NA, dim=c(n_estimate, n_gab, n_method), 
             dimnames = list(paste("Est:",levels(data[,estimate])),
                             paste("Real:",levels(data[,gabarito])),
                             levels(data[,method])))
  for(i in 1:n_estimate){
    for(j in 1:n_gab){
      for(k in 1:n_method){
        num <- mean(data[,error][data[,method]==levels(data[,method])[k] &
                                   data[,estimate]==levels(data[,estimate])[i] & 
                                   data[,gabarito]==levels(data[,gabarito])[j]], na.rm = T)
        
        if(num == 0 | is.nan(num)){
          p[i,j,k] <- NA
        }else{
          p[i,j,k] <- num
        }
      }
    }
  }
  p
  return(p)
}

map_name <- function(depth, seed, geno, fake, snpcall, countsfrom, data_names){
  
  bugfix <- lapply(strsplit(data_names[1], "_"), "[[", 1) # exclusive for 20cM dataset
  if(bugfix == "map"){
    temp_n <- paste0("map_",seed, "_", depth, "_", snpcall, "_", 
                     countsfrom, "_", geno, "_", fake)
  } else {
    if(geno == "OneMap_version2") geno <- "default"
    temp_n <- paste0(seed, "_", depth, "_map_", snpcall, "_", 
                     countsfrom, "_", geno, "_", fake)
  }
}

map_name_gus <- function(geno, seed, depth, fake, snpcall, countsfrom, data_names){
  
  bugfix <- lapply(strsplit(data_names[1], "_"), "[[", 1) # exclusive for 20cM dataset
  if(bugfix == "map"){
    temp_n <- paste0("map_", snpcall, "_", 
                     countsfrom, "_", geno, "_", fake)
  } else {
    temp_n <- paste0(seed, "_", depth, "_map_", snpcall, "_", 
                     countsfrom, "_", geno, "_", fake)
  }
}
