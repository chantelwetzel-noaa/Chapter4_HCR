#########################################################
##         Survey for Index of Abundance               ##
##                                                     ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################


Do_Survey <- function(biology, f.values, numbers, index) 
{
  Survey         <- list()
  temp.cal       <- array(NA, dim = c(length(index), ages, length(len.step), sexes))
  temp.index     <- numeric(length(index))
  temp.vul.total <- numeric(length(index))
  
  #Used in Catch @ Age and Length Calculation
  calc1 <- biology$obs.selec[,1] * biology$mid.phi.f
  calc2 <- biology$obs.selec[,2] * biology$mid.phi.m

  #Used in  Vulnerable Biomass Calculation
  temp2 <- (t(biology$mid.phi.m)) %*% (biology$mid.wght.at.len[,2] * biology$obs.selec[,2])
  temp1 <- (t(biology$mid.phi.f)) %*% (biology$mid.wght.at.len[,1] * biology$obs.selec[,1])

  for (a in 1:length(index))
  {
    #ind is used to access biological values that are track prior to the survey start (numbers and f)
    #ind <- pre.fishery.yrs + index[a]
    ind  <- index[a]
    vul.bio.obs        <- (temp1) * numbers[ind,,1] * exp(-survey.time * (m.f + biology$selec.age.f * f.values[ind])) +
                          (temp2) * numbers[ind,,2] * exp(-survey.time * (m.m + biology$selec.age.m * f.values[ind]))   
    temp.vul.total[a]  <- sum(vul.bio.obs)     
    temp.index    [a]  <- Q * temp.vul.total[a] * exp(survey.err[ind] - 0.5 * (survey.err[ind])^2 )

    
    #Catch @ age and length
    for (b in 1:ages)
    {   
      temp.cal[a,b,,1] <- calc1[,b] * (numbers[ind,b,1] * exp(-survey.time * (m.f + biology$selec.age.f[b] * f.values[ind])))      
      temp.cal[a,b,,2] <- calc2[,b] * (numbers[ind,b,2] * exp(-survey.time * (m.m + biology$selec.age.m[b] * f.values[ind])))
    } 
          
  }
  
  Survey[[1]] <- temp.cal
  Survey[[2]] <- temp.index
  Survey[[3]] <- temp.vul.total
  
  names(Survey) <- c("temp.cal", "temp.index", "temp.vul.total")
  return(Survey)
}
