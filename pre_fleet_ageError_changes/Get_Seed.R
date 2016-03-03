#########################################################
##            Random Seed Function                     ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################


Get_Seed <- function(n){ 
  seed <- round(runif(n, 1, 10000000),0)
  #Make sure they are all unique
  unq.seed <- unique(seed)
  while (unq.seed < n){
  	for (i in length(unq.seed):n){
  		seed.2 <- round(runif(n, 1, 10000000),0)
  	}
  	new.seed <- c(seed, seed2)
  	unq.seed <- unique(new.seed)
  }

  return(unq.seed)
}