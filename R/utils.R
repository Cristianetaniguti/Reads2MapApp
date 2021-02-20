test_geno <- function(global, error){
  if(global){
    geno <- paste0(error, 0.05)
    if(any(error %in% "OneMap_version2"))
      geno[which(error == "OneMap_version2")] <- "SNPCaller0.05"
    if(any(error %in% "gusmap"))
      stop("Gusmap do not allow to change the error rate. Please, select other option.")
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
    stop("This option is not available. The SNP callers performs together the SNP and genotype calling using the same read counts, we did not find a way to substitute the depths already used. Please select other option.")
  }
}

perfumaria <- function(data){
  snpcall <- c(GATK = "gatk", freebayes = "freebayes")
  data$SNPCall <- names(snpcall)[match(data$SNPCall, snpcall)]
  
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
  
  countsfrom <- c(BAM = "bam", VCF = "vcf")
  data$CountsFrom <- names(countsfrom)[match(data$CountsFrom, countsfrom)]

  depth.n <- paste("Depth", unique(data$depth))
  depth <- unique(data$depth)
  names(depth) <- depth.n
  data$depth <- names(depth)[match(data$depth, depth)]
  
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
