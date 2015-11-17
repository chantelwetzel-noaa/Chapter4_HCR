#########################################################
##      Generate Multinomial Age Samples               ##
##        for the Fishery or the Survey                ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################

Multinom_Ages <- function(catch.type , age.samp , AgeError) {
  
  age.expect <-  catch.type/(sum(catch.type))       
  small <- 0 #Set to match the value in the dat files
  #small <- 0.0000001
  new.prob.m <- age.expect[2:ages,2] + small
  new.prob.f <- age.expect[2:ages,1] + small  
  
  prob.m <- new.prob.m/sum(new.prob.f + new.prob.m)
  prob.f <- new.prob.f/sum(new.prob.f + new.prob.m)

  prob   <- c(prob.f, prob.m)
  
  if (AgeError == FALSE){
    samp    <- rmultinom(n = 1, size = age.samp, prob = prob)
    ages.out <- as.vector(samp)
  }  
 
  #AGE ERROR   
  if (AgeError == TRUE) {
    set.seed(age.err.seed[nsim] + y)
    ages.out <- numeric(max.age * 2)
    #ages.out <- numeric(2*ages -2)
    age.sd   <- 1/(20 + exp(-0.10*(1:(ages-1))))
    age.sd   <- c(age.sd, age.sd)

    for(a in 1:age.samp){
      samp    <- rmultinom(n = 1, size = 1, prob = prob)
      find    <- sort(samp == 1, index.return =T)$ix[ages*2 -2]
      age.err <- rnorm(1, 0, age.sd[find])
      temp1   <- ifelse(find > (ages-1), find - (ages-1), find)
      temp2   <- temp1*exp(age.err - 0.5 * age.err^2) 
      temp3   <- ifelse(temp2 > (floor(temp2)+ 0.50), ceiling(temp2), floor(temp2)) 
      temp3   <- min(temp3, max.age)
      ages.out[find + temp3 - temp1]<- ages.out[find + temp3 - temp1] + 1
      #print(cbind(temp1,temp2,temp3, deparse.level = 0))
    }
  }
  
  return(ages.out)
}
