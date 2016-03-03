depl.90 = matrix(NA, 90, 1000)
ssb.90  = matrix(NA, 90, 1000)
recr.90 = matrix(NA, 90, 1000)

catchEQ = "ramp_constant"
catchEQ = "ramp_decline"
catchEQ = "ramp"

for (nsim in 1:1000)
 {
  
  if (github) { 
    git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Flatfish_MSC/"
    source(paste(drive, git.wd, "functions/Functions.R", sep = ""))  }
  if (!github){ 
    source(paste(drive,"/Flatfish_MSC/code/functions/Functions.R",sep="")) }

  #Save Output
  projections <- paste(directory,"save/om_proj_",nsim,sep="")
  estimates   <- paste(directory,"save/ss_ests_",nsim,sep="")
  dat    	    <- paste0("sim",".dat")
  ctl   	    <- paste0("sim",".ctl")
  
  #Read in the seeds 
  load(paste(drive,"/Flatfish_MSC/seed_list",sep=""))
  recruit.seed  <- as.numeric(seed.list[[1]][,"recruit.seed"])
  catch.seed    <- as.numeric(seed.list[[1]][,"catch.seed"])
  survey.seed   <- as.numeric(seed.list[[1]][,"survey.seed"])
  comp.seed     <- as.numeric(seed.list[[1]][,"comp.seed"])  
  age.err.seed  <- as.numeric(seed.list[[1]][,"spare1"]) 
  select.seed   <- as.numeric(seed.list[[1]][,"spare2"])

  # Read in the parameter lsit and set according to simulation
  load(paste(drive,"/Flatfish_MSC/parm_dist_list",sep=""))
  m.f <- parm.list$m.f[[nsim]]
  m.m <- parm.list$m.m[[nsim]]
  kf  <- parm.list$kf[[nsim]]
  km  <- parm.list$km[[nsim]]
  L1f <- parm.list$L1f[[nsim]]
  L1m <- parm.list$L1m[[nsim]]
  L2f <- parm.list$L2f[[nsim]]
  L2m <- parm.list$L2m[[nsim]]
 
  #Set up the bias adjustment parameters -----------------------------------------------------------------------------------
  #Bias adjustment parameters
  main.rec.start <- 1 #ages
  main.rec.end   <- setup.yrs + pre.fishery.yrs # - 6   
  start.devs     <- 0 #ages - 10
  pre.dev.phase  <- 4        
  start.bias     <- min(start.s.data, start.f.data) - (ages - 1)
  full.bias      <- min(start.s.data, start.f.data) - (ages - 1)/2
  last.bias      <- setup.yrs + pre.fishery.yrs - 5        
  last.no.bias   <- setup.yrs + pre.fishery.yrs - 4
  max.bias.adj   <- 0.80
  #pre.model.devs <- 1
   
 # Catch History -----------------------------------------------------------------------------------------------------------
  if ( use_catch ){
 	catch.dev <- c(rnorm(10, 0, 0.50),
 				rnorm(20, 0, 0.07),
 				rnorm(20, 0, 0.15))
 	CatchTot <- rep(0,setup.yrs) ;  CatchTot[1] <- 20
 	  
 	for (y in 2:(setup.yrs-11)) { 
 	   CatchTot[y]<- CatchTot[y-1]*1.5 
 	   if (CatchTot[y-1]*1.4 > 1000 ) { 
 	       CatchTot[y] <- 1000 }  
 	}
 	for (y in (setup.yrs-20):setup.yrs) { 
 	   CatchTot[y]<- CatchTot[y-1]*0.97
 	}
 	  
 	CatchTot     <- round(CatchTot,0)    
 	CatchTot.err <- round(CatchTot + CatchTot * catch.dev,0)
 	hist.catch   <- c(rep(0,pre.fishery.yrs),CatchTot.err)
 	
 	# Set temporary constant catches
 	hist.catch   <- c(rep(0,pre.fishery.yrs), rep(1000, setup.yrs))
  }


# Effort based catch alternative ------------------------------------------------------------------------------------------------
 if ( !use_catch ){
 	effort = hist.catch = numeric(0)
 	#Constant Effort
  if (catchEQ == "constant") { 
    effort[1:(ages - 1)] <- 0
    effort[ages:(pre.fishery.yrs + setup.yrs)] <- 0.15
  }

  #Ramp up
  if (catchEQ == "ramp"){
    effort[1:(ages - 1)] <- 0
    ind <- ages:(pre.fishery.yrs + setup.yrs)
    effort[ind] <- 0.50 * ((ind + 1 - (ages)) / length(ind) )
  }
 	
 	#Ramp up and then constant effort
  if (catchEQ == "ramp_constant"){
    effort[1:(ages - 1)] <- 0
    ind <- ages:(pre.fishery.yrs + setup.yrs - 24)
    effort[ind] <- 0.40 * ((ind + 1 - (ages)) / length(ind) )
    ind2 <- (max(ind)+1) : (pre.fishery.yrs + setup.yrs)
    effort[ind2] <- 0.40
  }

 	#Ramp and Decline Effort
 	if (catchEQ == "ramp_decline"){
 		yr1 <- ages
 		yr2 <- ages + 15
	  effort[1:(ages - 1)] <- 0
 		effort[yr1: yr2] <- 0.50 * (yr1:yr2 - yr1 + 1) / (yr2 - yr1 + 1)
 		effort[(yr2 + 1):(yr2 + 20)] <- 0.50
 		yr1 <- yr2 + 21
 		yr2 <- (pre.fishery.yrs + setup.yrs)
 		effort[yr1:yr2] <- 0.50 * (0.80 * yr1 - yr2 + 0.20*(yr1:yr2)) / (yr1 - yr2)
 	}
 }
 

# Starting population values -----------------------------------------------------------------------------------------------------
 R0 <- 1000
 if ( determ ) { R0 <- 10000 }

 #Draw recruitment deviations----------------------------------------------------------------------------------------------------- 
 if ( determ ) { sigmaR = survey.cv = 0 ; sigmaR.set = 0.01 ; ss.survey.cv = 0.05 }
 #sigmaR = sigmaR.set = 0.20 ; survey.cv = ss.survey.cv = 0.05
 set.seed(recruit.seed[nsim])
 rho      <- 0
 if ( auto ) { rho <- 1 / sqrt(2) }
 recdevs  <- rnorm(total.yrs, 0, sigmaR)
 autocorr <- rep(0, total.yrs)
 autocorr[1] <- recdevs[1]  
 for (e in 2:total.yrs) { 
    autocorr[e] <- rho*autocorr[e-1]+sqrt(1-rho*rho)*recdevs[e] 
 }
 
 #Draw Survey Error---------------------------------------------------------------------------------------------------------------  
 set.seed(survey.seed[nsim])
 survey.err <<- rnorm(total.yrs, 0, survey.cv)
 
 #Recruits Spawning biomass  Vulnerable biomas------------------------------------------------------------------------------------
 Update_Dynamics <- function(R0, hist.catch, biology)
 {
    UpdateDyn <- list() 
    
    #Virgin Population Structure ----------------------------------------------------------------------------------------------------
    Ry[1]<- R0 / 2
    # Initial female numbers-at-age
    numbers[1,1:(ages-1),1] <- (R0 / 2) * exp(-m.f * (0:(ages-2)))
    numbers[1,ages,1]       <- numbers[1,ages-1,1] * exp( -m.f ) / (1 - exp(-m.f)) 
    # Initial male numbers-at-age
	  numbers[1,1:(ages-1),2] <- (R0 / 2) * exp(-m.m * (0:(ages-2)))
    numbers[1,ages,2]       <- numbers[1,ages-1,2] * exp( -m.m ) / (1 - exp(-m.m)) 
       
    #Virgin Biomass By Age  
    SSB0 <- SSB[1] <- sum(numbers[1,,1] * biology$fecund)

    #Find F values based on catch-----------------------------------------------------------------------------------------------------
    Findf <- function(f){
       z.f <- (1 - exp(-(m.f + selec.age.f * f))) / (m.f + selec.age.f * f)
       z.m <- (1 - exp(-(m.m + selec.age.m * f))) / (m.m + selec.age.m * f)
       
       #Catch at Age
       catch.at.age.f <- f * (numbers[y,,1] * selec.age.f) * z.f
       catch.at.age.m <- f * (numbers[y,,2] * selec.age.m) * z.m
       
       #Catch At Length
       mid.temp.f <- numbers[y,,1] * z.f 
       mid.temp.m <- numbers[y,,2] * z.m 

       catch.at.len.f <- ((biology$mid.phi.f * selec[,1]) %*% (mid.temp.f))
       catch.at.len.m <- ((biology$mid.phi.m * selec[,2]) %*% (mid.temp.m))
           
       #Catch in Weight by Sex, mid.wght (41X2) calculated in the GetWght() function  
       catch.wght <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f) +
                          sum(biology$mid.wght.at.len[,2] * catch.at.len.m) )    
    
       output <- NULL
       output$catch.at.len.f <- catch.at.len.f
       output$catch.at.len.m <- catch.at.len.m
       output$catch.wght     <- catch.wght
       output$catch.at.age.f <- catch.at.age.f
       output$catch.at.age.m <- catch.at.age.m
       return(output)
    } #End FindF function
    
    #Objective Function----------------------------------------------------------------------------------------------------------
    Obj.Fun.F <- function(f) {
       obj.fun.f <- (Findf(f)$catch.wght - hist.catch[y])^2
       return(obj.fun.f) 
    }
         
    for(y in 1:(pre.fishery.yrs+setup.yrs - 1)) {  

      selec.age.m     <- biology$selec.age.m
      selec.age.f     <- biology$selec.age.f
      selec           <- biology$selec

      #f <- ifelse(y > pre.fishery.yrs, optimize(Obj.Fun.F, lower=0, upper=2, tol = 1e-6)$minimum, 0)
      #if(f > 2) print(paste("F > 2 in year", y))
      #  f.values[y] <- f
        
      #find.f <- Findf(f)
      #catch.at.age[y,,1]   <- find.f$catch.at.age.f
      #catch.at.age[y,,2]   <- find.f$catch.at.age.m
      #catch.at.len[y,,1]   <- find.f$catch.at.len.f
      #catch.at.len[y,,2]   <- find.f$catch.at.len.m
      #catch.wght.values[y] <- find.f$catch.wght

      f             <- effort[y]
	    f.values[y]   <- f
      z.m           <- (1 - exp(-(m.m + selec.age.m * f))) / (m.m + selec.age.m * f)
      z.f           <- (1 - exp(-(m.f + selec.age.f * f))) / (m.f + selec.age.f * f)
      catch.at.age[y,,1] <- f * (numbers[y,,1] * selec.age.f) * z.f
      catch.at.age[y,,2] <- f * (numbers[y,,2] * selec.age.m) * z.m

      #Catch At Length
      mid.temp.f <- numbers[y,,1] * z.f 
      mid.temp.m <- numbers[y,,2] * z.m 
      catch.at.len[y,,1] <- ((biology$mid.phi.f * selec[,1]) %*% (mid.temp.f))
      catch.at.len[y,,2] <- ((biology$mid.phi.m * selec[,2]) %*% (mid.temp.m))
	    hist.catch[y] <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len[y,,1]) +
                            sum(biology$mid.wght.at.len[,2] * catch.at.len[y,,2]) )  
        
      # survival at age by gender
      S.f <- exp(-(m.f + selec.age.f * f))
      S.m <- exp(-(m.m + selec.age.m * f))
        
      #Update the numbers and remove the catch by applying the solved for f value
      numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
      numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
      numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * exp(-m.f - selec.age.f[ages] * f)
      numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * exp(-m.m - selec.age.m[ages] * f)
        
      SSB[y+1] <- sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages])
  
      #Expected (and then realized) recruitment
      Ry[y+1] <- (4 * steep * ( R0 / 2 ) * SSB[y+1]) / (SSB0 * (1 - steep) + SSB[y+1] * (5 * steep - 1))
      if (autocorr[y+1] != 0 ) { Ry[y+1] <- Ry[y+1] * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1]) }
      numbers[y+1,1,] <- Ry[y+1]
           
    } #closes yearly loop
    
    UpdateDyn[[1]] <- f.values
    UpdateDyn[[2]] <- catch.wght.values
    UpdateDyn[[3]] <- catch.at.age
    UpdateDyn[[4]] <- catch.at.len
    UpdateDyn[[5]] <- numbers
    UpdateDyn[[6]] <- SSB
    UpdateDyn[[7]] <- Ry
    UpdateDyn[[8]] <- selec
    UpdateDyn[[9]] <- selec.age.f
    UpdateDyn[[10]]<- selec.age.m
    UpdateDyn[[11]]<- NULL#caa
    UpdateDyn[[12]]<- hist.catch
    names(UpdateDyn) <- c("f.values","catch.wght.values","catch.at.age","catch.at.len","numbers","SSB","Ry", "selec", 
                    "selec.age.f", "selec.age.m", "caa", "hist.catch")
    return(UpdateDyn)
 }


 # Create Virgin population
 dyn      <- Update_Dynamics(R0, hist.catch, biology = Get_Biology())
 Dyn      <- list() 

 ssb.90[,nsim] <- dyn$SSB[1:90]
 recr.90[,nsim]<- dyn$Ry[1:90]
 depl.90[,nsim]<- dyn$SSB[1:90] / dyn$SSB[1]
}

hist(depl.90[90,], xlim = c(0,1))
hist(ssb.90[1,], xlim = c(0, 10000))

plot(1:90, depl.90[,1], ylim = c(0,1.4), type = 'l')
for (a in 2:1000){ lines(1:90, depl.90[,a] )}

plot(1:90, recr.90[,1], ylim = c(0,2500), type = 'l')
for (a in 2:1000){ lines(1:90, recr.90[,a] )}

plot(1:90, ssb.90[,1], ylim = c(0,5000), type = 'l')
for (a in 2:1000){ lines(1:90, ssb.90[,a] )}


Length <- function(){
 #Calculate Lengths
 Len     <- list()
 len     <- matrix(NA, ages, sexes)
 mid.len <- matrix(0,  ages, sexes)
 
 #L infinity (cm)
  Linf_f <- L1f + ((L2f - L1f) / (1 - exp( -kf * (a4 - a3))))
  Linf_m <- L1m + ((L2m - L1m) / (1 - exp( -km * (a4 - a3))))
  len.slope.f <- (L1f-len.step[1])/a3
  len.slope.m <- (L1m-len.step[1])/a3
 
  # Length at the start of the year (cm)
  len[1:(a.linear+1),1]<-len.step[1]+len.slope.f*(seq(1,(a.linear+1),1)-1)  #For smallest fish length is a linear function  
  len[1:(a.linear+1),2]<-len.step[1]+len.slope.m*(seq(1,(a.linear+1),1)-1)
  # Growth based on the VB
  len[(a.linear+2):ages,2]<-Linf_m+(L1m-Linf_m)*exp(-km*((seq(a.linear+2,ages,1)-1)-a3))
  len[(a.linear+2):ages,1]<-Linf_f+(L1f-Linf_f)*exp(-kf*((seq(a.linear+2,ages,1)-1)-a3))

  #Plus Group Growth
  temp1 <- sum(exp(-0.20*(ages:(ages*2)-ages))*(len[ages,2]+((ages:(ages*2)-ages)/ages)*(Linf_m-len[ages,2])))
  temp2 <- sum(exp(-0.20*(ages:(ages*2)-ages)))
  len[ages,2] <- temp1/temp2
  
  temp1 <- sum(exp(-0.20*(ages:(ages*2)-ages))*(len[ages,1]+((ages:(ages*2)-ages)/ages)*(Linf_f-len[ages,1])))
  temp2 <- sum(exp(-0.20*(ages:(ages*2)-ages)))
  len[ages,1] <- temp1/temp2
  return(len)
}

len = len.alt = len.org = array(NA, dim = c(2, ages, 1000))

for (nsim in 1:1000){

  load(paste(drive,"/Flatfish_MSC/parm_dist_list",sep=""))
  len.step = seq(4, 78,1)
  #source(paste0(drive, "/Flatfish_MSC/code/functions/Arrays.R"))
  
  a3 = 2; a4 = 40 ; a.linear = 2
  #a3 = a.linear = 1
  m.f <- parm.list$m.f[[nsim]]
  m.m <- parm.list$m.m[[nsim]]
  kf  <- parm.list$kf[[nsim]]
  #km  <- (0.202  - 0.134) + kf
  km  <- parm.list$km[[nsim]]
  L1f <- parm.list$L1f[[nsim]]
  L1m <- L1f #parm.list$L1m[[nsim]]
  L2f <- parm.list$L2f[[nsim]]
  L2m <- parm.list$L2m[[nsim]]
  if (L1f < 16) {a3 = a.linear = 1}
  if (L1m < 16) {a3 = a.linear = 1}

  biology = Length()
  len[1,,nsim] <- biology[,1]
  len[2,,nsim] <- biology[,2]

  L1m <- parm.list$L1m[[nsim]]
  biology = Length()
  len.org[1,,nsim] <- biology[,1]
  len.org[2,,nsim] <- biology[,2]

  L2m <- L2f - 9 #
  L1m <- L1f
  #km  <- (0.202  - 0.134) + kf
  biology = Length()
  len.alt[1,,nsim] <- biology[,1]
  len.alt[2,,nsim] <- biology[,2]
}

par(mfrow= c(3,2))
plot(1:ages, len.org[1,,1], type ='l', ylim = c(0, 80), col =2)
for (a in 2:1000){ lines(1:ages, len.org[1,,a],col =2)}

plot(1:ages, len.org[2,,1], type ='l', ylim = c(0, 80),col =4)
for (a in 2:1000){ lines(1:ages, len.org[2,,a], col =4)}

plot(1:ages, len[1,,1], type ='l', ylim = c(0, 80),col = 2)
for (a in 2:1000){ lines(1:ages, len[1,,a], col =2 )}

plot(1:ages, len[2,,1], type ='l', ylim = c(0, 80),col =4)
for (a in 2:1000){ lines(1:ages, len[2,,a], xlim = c(0, 10),col =4)}

plot(1:ages, len.alt[1,,1], type ='l', ylim = c(0, 80),col =2)
for (a in 2:1000){ lines(1:ages, len.alt[1,,a], col =2)}

plot(1:ages, len.alt[2,,1], type ='l', ylim = c(0, 80),col =4)
for (a in 2:1000){ lines(1:ages, len.alt[2,,a], col =4)}