n = 10000
directory <- "C:/Flatfish_MSC/code/"

# Seed file for output
seed.file <- paste(directory,"seed_list",sep="")

Get_Seed <- function(n){ 
  seed <- round(runif(n, 1, 10000000),0)
  #Make sure they are all unique
  unq.seed <- unique(seed)
  while (length(unq.seed) < n){
  	seed2    <- round(runif(n - length(unq.seed), 1, 10000000),0)
  	new.seed <- c(unq.seed, seed2)
  	unq.seed <- unique(new.seed)
  }
  return(unq.seed)
}

seed.list     <- list()
recruit.seed  <- Get_Seed(n)
catch.seed    <- Get_Seed(n) 
survey.seed   <- Get_Seed(n) 
comp.seed     <- Get_Seed(n)
m.seed        <- Get_Seed(n)
Lmin.seed     <- Get_Seed(n)
Lmax.seed     <- Get_Seed(n)
spare1        <- Get_Seed(n)
spare2        <- Get_Seed(n)
spare3        <- Get_Seed(n)
spare4        <- Get_Seed(n)

#Save the seeds and write them out
seed.list[[1]] <- cbind(recruit.seed, catch.seed, survey.seed, comp.seed, m.seed, Lmin.seed, Lmax.seed, spare1, spare2, spare3, spare4)
save(seed.list, file = seed.file)