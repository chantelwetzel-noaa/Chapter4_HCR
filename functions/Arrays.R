#Arrays----------------------------------------------------------------------------------------------------
  age.bins <- seq(1,num.ages,1)
  numbers  <- array(0, dim = c((total.yrs + 1), ages, sexes)) 
  biomass  <- array(0, dim = c(total.yrs, ages, sexes)) 
  
  #Matrix Storage for output to R list
  index.expect         <- matrix(0, total.yrs, 1) 
  survey.catch.age.len <- array(NA,c(total.yrs, ages, length(len.step), sexes)) 
  hist.catch <- catch.wght.values <- matrix(0, total.yrs, f.fleets)
  fore.catch           <- rep(0, total.yrs)

  #Storage matrices for data samples
  # f.lengths            <- matrix(0, total.yrs, (2*length(data.len.step))) 
  # s.lengths            <- matrix(0, total.yrs, (2*length(data.len.step)))
  # f.a.ca               <- matrix(0, total.yrs, (max.age*2)) 
  # s.a.ca               <- matrix(0, total.yrs, (max.age*2)) 
  # f.sample.size        <- rep(0, total.yrs) 
  # s.sample.size        <- rep(0, total.yrs)

  f.lengths            <- array(0, dim = c(f.fleets, total.yrs, (2*length(data.len.step))) ) 
  s.lengths            <- matrix(0, total.yrs, (2*length(data.len.step)))
  f.a.ca               <- array(0, dim = c(f.fleets, total.yrs, (max.age*2)) ) 
  s.a.ca               <- matrix(0, total.yrs, (max.age*2)) 
  f.sample.size        <- matrix(0, f.fleets, total.yrs) 
  s.sample.size        <- rep(0, total.yrs)  

  #Arrays to Store the Estimation Results from SS
  SB          <- array(NA, dim=c(total.yrs, ass.num)) 
  Bratio      <- array(NA, dim=c(total.yrs, ass.num)) 
  Recruits    <- array(NA, dim=c(total.yrs, ass.num)) 
  OFL         <- array(NA, dim=c(total.yrs + 4))
  ForeCat     <- array(NA, dim=c(total.yrs + 4))
  FSPR        <- array(NA, dim=c(1, ass.num))
  Fmult       <- array(NA, dim=c(1, ass.num))
  CrashPen    <- array(NA, dim=c(1, ass.num))
  R0.out      <- array(NA, dim=c(1, ass.num))
  Gradiant.out<- array(NA, dim=c(1, ass.num))
  m.store     <- array(NA, dim=c(2, ass.num))
  lmin.store  <- array(NA, dim=c(2, ass.num))
  lmax.store  <- array(NA, dim=c(2, ass.num))
  k.store     <- array(NA, dim=c(2, ass.num))
  cv.y.store  <- array(NA, dim=c(2, ass.num))
  cv.old.store<- array(NA, dim=c(2, ass.num))
  f.selex     <- array(NA, dim=c(f.fleets, sexes, 6, ass.num))
  s.selex     <- array(NA, dim=c(sexes, 6, ass.num))
  
  #Dynamics
  Ry           <- matrix(0, total.yrs+1, 1)
  SSB          <- matrix(0, total.yrs+1, 1)
  depl         <- matrix(0, total.yrs+1, 1)
  ofl.true     <- matrix(0, total.yrs+1, 1)
  acl.true     <- matrix(0, total.yrs+1, 1)
  catch.at.age <<- array(NA, dim=c(total.yrs, f.fleets, ages, sexes))
  catch.at.len <<- array(NA, dim=c(total.yrs, f.fleets, length(len.step), sexes))
  z.rate       <- array(NA, dim = c(total.yrs, f.fleets, ages, sexes)) ; rownames(z.rate) <- years
  f.values     <- matrix(0, f.fleets, total.yrs)
  catch.wght.values <- rep(0, total.yrs)
