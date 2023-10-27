# Debugging OneMap
#Check breakpoint_count.R empirical data
#i <- 22
#j <- 7

# Onemap sequence
onemap.obj <- data$data.name
twopts <- data$twopt
input.seq <- data

pos <- as.numeric(input.seq$data.name$POS[input.seq$seq.num])
sort.pos <- sort(pos)
if(any(pos != sort.pos)){
  cat("The markers are not ordered by genome position")
  input.seq <- make_seq(input.seq$twopt, input.seq$seq.num[order(as.numeric(input.seq$data.name$POS[input.seq$seq.num]))])
} 

max_cores <- 4

if(length(input.seq$seq.num) > 100){
  # batch size close to 60 and the overlap is 3/5 of the size (according with paper)
  #div <- round((length(input.seq$seq.num)/60),0)
  #size = round(length(input.seq$seq.num)/div,0)
  #overlap = round(size*(3/5),0)
  #around = 10
  
  batch_size <- pick_batch_sizes(input.seq,
                                 size = 50,
                                 overlap = 30,
                                 around = 10)
  
  map_out <- map_avoid_unlinked(input.seq, 
                                size = batch_size, 
                                phase_cores = max_cores, 
                                overlap = 30,
                                parallelization.type = "FORK")
} else {
  map_out <- map_avoid_unlinked(input.seq)
}