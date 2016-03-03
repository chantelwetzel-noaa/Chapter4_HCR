#########################################################
##       Generate Multinomial Length Samples           ##
##        for the Fishery or the Survey                ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-22-2014                        ##
#########################################################


Multinom_Lengths <- function(catch.type, len.samp){
     
  len.expect <-  catch.type/(sum(catch.type)) 
  small <- 0.0 #Set to the value in the data file
  new.prob.len.m <- len.expect[,2] + small
  new.prob.len.f <- len.expect[,1] + small  
  
  prob.len.m <- new.prob.len.m/sum(new.prob.len.f + new.prob.len.m)
  prob.len.f <- new.prob.len.f/sum(new.prob.len.f + new.prob.len.m)
  prob       <- c(prob.len.f, prob.len.m)

  len.samp   <- rmultinom(n = 1, size = len.samp, prob = prob)

  #Add together the small sizes into the minimum bin and the plus group as well
  low.bin = data.len.step[1] ; high.bin = data.len.step[length(data.len.step)]
  low.combine = sum(len.step <=low.bin) ; high.combine = sum(len.step >= high.bin)
  #females
  sum.low.f = sum(len.samp[1:low.combine]) ; sum.high.f = sum(len.samp[(length(len.step)-high.combine+1): length(len.step)])
  #males  
  sum.low.m = sum(len.samp[(length(len.step)+1):(length(len.step)+low.combine)])
  sum.high.m = sum(len.samp[(length(len.step)*2-high.combine+1): (length(len.step)*2)])

  #Compress down
  new.len.samp = c(sum.low.f, len.samp[(low.combine+1):(length(len.step)-high.combine)], sum.high.f,
                   sum.low.m, len.samp[(length(len.step)+low.combine+1):(length(len.step)*2-high.combine)], sum.high.m)

  lengths  <- as.vector(new.len.samp)
  return(lengths)  
}
