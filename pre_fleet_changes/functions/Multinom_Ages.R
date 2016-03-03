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
    if ((max.age + 1) < ages){
      plus.f = sum(ages.out[max.age:(ages-1)])
      plus.m = sum(ages.out[(2*max.age + ages - max.age - 1):(2*ages-1-1)])
      temp = c(ages.out[1:(max.age - 1)], plus.f, ages.out[ages:(2*max.age + ages - max.age - 2)], plus.m)
    }
  }  
 
  #AGE ERROR   
  if (AgeError == TRUE) {
    #set.seed(age.err.seed[nsim] + y)
    ages.out <- numeric(max.age * 2)
    #ages.out <- numeric(2*ages -2)
    age.sd   <- rep(0.05, ages - 1) #1/(20 + exp(-0.05*(1:(ages-1))))
    age.sd   <- c(age.sd, age.sd)

    for(z in 1:age.samp){
      samp    <- rmultinom(n = 1, size = 1, prob = prob)
      find    <- sort(samp == 1, index.return =T)$ix[ages*2 -2]
      age.err <- rnorm(1, 0, age.sd[find])
      temp1   <- ifelse(find > (ages-1), find - (ages-1), find)
      temp2   <- temp1*exp(age.err - 0.5 * age.err^2) 
      temp3   <- ifelse(temp2 > (floor(temp2)+ 0.50), ceiling(temp2), floor(temp2)) 
      temp3   <- min(temp3, ages-1)
      # Females go into 1:max.age
      if ((find + temp3 - temp1) <= ages) { 
          index = ifelse( (find + temp3 - temp1) < max.age, 
                           find + temp3 - temp1, max.age) }
      # Males go into max.age + 1 : max.age * 2
      if ((find + temp3 - temp1) > ages) { 
          index = ifelse( (find + temp3 - temp1 ) < max.age * 2, 
                            temp3 + max.age, 
                            max.age * 2) }
      #ages.out[find + temp3 - temp1]<- ages.out[find + temp3 - temp1] + 1
      ages.out[index]<- ages.out[index] + 1   
      #print(cbind(temp1,temp2,temp3, deparse.level = 0))
    }
  }
  
  return(ages.out)
}
