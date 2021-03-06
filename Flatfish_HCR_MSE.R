##############################################
##     Management Strategy Evaluation 		  ##
##                  for                     ##
##        U.S. West Coast Flatfish          ##
##          Harvest Control Rule            ##
##                                          ##
##      Written by: Chantel Wetzel          ##
##       Started: September 2015            ##
##############################################

#source("//even_more_home//h_cwetzel//PhD//Flatfish_MSC//code//Flatfish_HCR_MSE.R") 
#drive   <-"//home//cwetzel//h_cwetzel//PhD"
drive   <-"C:" #"//home//cwetzel//h_cwetzel"
start.n <- 1
end.n   <- 200
hcr.opt <- "hcr_20_5"
#HCR Options: 25_5; 40_10; 30_10; 20_5
steep     <- 0.85
steep.set <- 0.85
sigmaR    <- 0.40 #0.60
auto      <- FALSE
reduce.data <- FALSE
fish.sel.mat  <- TRUE # sensitivity to shift fishery selectivity
fish.sel.pre  <- FALSE # sensitivity to shift fishery selectivity

tantalus <- FALSE
github   <- TRUE

NSIM      <- end.n
seed.list <- list()
determ    <- TRUE
use_catch <- FALSE
catchEQ   <- "ramp_constant"

use.len.data <- TRUE
n.lambda     <- ifelse(use.len.data, 0, 3)
data.type    <- 4 #4=lengths 5 = ages
skip.len     <- ifelse(use.len.data, FALSE, TRUE)
 
require( compiler )
require( r4ss )

#Set the directory 
if (steep == steep.set){ directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100, "/") }
if (steep == steep.set  & sigmaR == 0.60){ directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100, "_sigmaR_", sigmaR*100,"/") }
if (steep != steep.set){ directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100, "_", steep.set*100,"/")}
if (steep == steep.set & auto == TRUE ) { directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100,"_auto", "/") }
if (steep == steep.set & auto == TRUE & sigmaR == 0.60) { directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100,"_sigmaR_", sigmaR*100,"_auto", "/") }
if (steep == steep.set & fish.sel.mat == TRUE ){ directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100, "_selec_at_mat/") }
if (steep == steep.set & fish.sel.pre == TRUE ){ directory <-  paste0(drive, "/PhD/Chapter4/Steep_", steep*100, "_selec_pre_mat/") }
dir.create(directory)
directory <<- paste0(directory, "hcr_option_", hcr.opt,"_", catchEQ, "_sims_",start.n,"_",end.n,"/")
dir.create(directory)
dir.create(paste(directory,"/save",sep=""))
dir.create(paste(directory,"/ctl", sep=""))
dir.create(paste(directory,"/report", sep=""))

 #Move the executable to the correct folder to run simulations
 if (!tantalus) {
  	file.copy(paste(drive,"/PhD/Chapter4/ss3.exe",sep=""),paste(directory,"/ss3.exe",sep="")) }
 if (tantalus)  {
    file.copy(paste(drive,"/PhD/Chapter4/SS3",sep=""),paste(directory,"/SS3",sep="")) }
 
 #Source in external functions
 if (github) { 
  	git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/"
  	source(paste(drive, git.wd, "functions/Functions.R", sep = "")) }
 if (!github){ 
 	source(paste(drive,"/PhD/Chapter4/code/functions/Functions.R",sep="")) }

#---------------------------------------------------------------------------------------------------
for (nsim in start.n:end.n)
 {
  
  if (github) { 
    git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/"
    source(paste(drive, git.wd, "functions/Functions.R", sep = ""))  }
  if (!github){ 
    source(paste(drive,"/PhD/Chapter4/code/functions/Functions.R",sep="")) }

  #Save Output
  projections <- paste(directory,"save/om_proj_",nsim,sep="")
  estimates   <- paste(directory,"save/ss_ests_",nsim,sep="")
  dat    	    <- paste0("sim",".dat")
  ctl   	    <- paste0("sim",".ctl")
  
  #Read in the seeds 
  load(paste(drive,"/PhD/Chapter4/seed_list",sep=""))
  recruit.seed  <- as.numeric(seed.list[[1]][,"recruit.seed"])
  catch.seed    <- as.numeric(seed.list[[1]][,"catch.seed"])
  survey.seed   <- as.numeric(seed.list[[1]][,"survey.seed"])
  comp.seed     <- as.numeric(seed.list[[1]][,"comp.seed"])  
  age.err.seed  <- as.numeric(seed.list[[1]][,"spare1"]) 
  select.seed   <- as.numeric(seed.list[[1]][,"spare2"])

  # Read in the parameter lsit and set according to simulation
  load(paste(drive,"/PhD/Chapter4/parm_dist_list",sep=""))
  m.f <- parm.list$m.f[[nsim]]
  m.m <- parm.list$m.m[[nsim]]
  kf  <- parm.list$kf[[nsim]]
  km  <- parm.list$km[[nsim]]
  L1f <- parm.list$L1f[[nsim]]
  L1m <- L1f # parm.list$L1m[[nsim]] 
  # since males and females have the same Lmin for petrale I am using the same distribution
  # odd growth curves were observed when this was not done.  Not exactly sure why. 
  L2f <- parm.list$L2f[[nsim]]
  L2m <- parm.list$L2m[[nsim]]

  if (L1f < 16) {a3 = a.linear = 1}
  if (L1m < 16) {a3 = a.linear = 1}
 
  # Set up the bias adjustment parameters -----------------------------------------------------------------------------------
  # Bias adjustment parameters
  main.rec.start <- 1 #ages
  main.rec.end   <- setup.yrs + pre.fishery.yrs # - 6   
  start.devs     <- 0 #ages - 10
  pre.dev.phase  <- 4        
  start.bias     <- min(start.s.data, start.f.data) - (ages - 1)
  full.bias      <- min(start.s.data, start.f.data) - (ages - 1)/2
  last.bias      <- setup.yrs + pre.fishery.yrs - 5        
  last.no.bias   <- setup.yrs + pre.fishery.yrs - 4
  max.bias.adj   <- 0.80
   
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
 	  effort  = numeric(0)
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

# Draw recruitment deviations----------------------------------------------------------------------------------------------------- 
 if ( determ ) { sigmaR = survey.cv = 0 ; sigmaR.set = 0.01 ; ss.survey.cv = 0.05 }

 set.seed(recruit.seed[nsim])
 rho      <- 0
 if ( auto ) { rho <- 1 / sqrt(2) }
 recdevs  <- rnorm(total.yrs, 0, sigmaR)
 autocorr <- rep(0, total.yrs)
 autocorr[1] <- recdevs[1]  
 for (e in 2:total.yrs) { 
    autocorr[e] <- rho*autocorr[e-1]+sqrt(1-rho*rho)*recdevs[e] 
 }
 
# Draw Survey Error---------------------------------------------------------------------------------------------------------------  
 set.seed(survey.seed[nsim])
 survey.err <<- rnorm(total.yrs, 0, survey.cv)
 
# Recruits Spawning biomass  Vulnerable biomas------------------------------------------------------------------------------------
 Update_Dynamics <- function(R0, biology)
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
         
    for(y in 1:(pre.fishery.yrs+setup.yrs - 1)) {  

      selec.age          <- biology$selec.age
      #selec.age.f       <- biology$selec.age.f
      selec              <- biology$selec
   
      f                  <- c(f.split[1] * effort[y], f.split[2] * effort[y])
	    f.values[,y]       <- f
      z.m                <- (1 - exp(-(m.m + selec.age[1,2,] * f[1] + selec.age[2,2,]*f[2]))) / 
                              (m.m + selec.age[1,2,] * f[1] + selec.age[2,2,] * f[2])
      z.f                <- (1 - exp(-(m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2]))) / 
                              (m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2])
      for (fleet in 1:f.fleets){
        catch.at.age[y,fleet,,1] <- f[fleet] * (numbers[y,,1] * selec.age[fleet,1,]) * z.f
        catch.at.age[y,fleet,,2] <- f[fleet] * (numbers[y,,2] * selec.age[fleet,2,]) * z.m
      }

      #Catch At Length
      mid.temp.f <- numbers[y,,1] * z.f 
      mid.temp.m <- numbers[y,,2] * z.m

      for (fleet in 1:f.fleets){
        catch.at.len[y,fleet,,1] <- ((biology$mid.phi.f * selec[fleet,,1]) %*% (mid.temp.f))
        catch.at.len[y,fleet,,2] <- ((biology$mid.phi.m * selec[fleet,,2]) %*% (mid.temp.m))
      } 

	    hist.catch[y,1] <- f[1] * (sum(biology$mid.wght.at.len[,1] * catch.at.len[y,1,,1]) +
                                 sum(biology$mid.wght.at.len[,2] * catch.at.len[y,1,,2]) ) 

      hist.catch[y,2] <- f[2] * (sum(biology$mid.wght.at.len[,1] * catch.at.len[y,2,,1]) +
                                 sum(biology$mid.wght.at.len[,2] * catch.at.len[y,2,,2]) )   
        
      # survival at age by gender
      S.f <- exp(-(m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2]))
      S.m <- exp(-(m.m + selec.age[1,2,] * f[1] + selec.age[2,2,] * f[2]))
        
      #Update the numbers and remove the catch by applying the solved for f value
      numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
      numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
      #numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * exp(-m.f - selec.age.f[ages] * f)
      #numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * exp(-m.m - selec.age.m[ages] * f)
      numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * S.f[ages]
      numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * S.m[ages]

      SSB[y+1] <- sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages])
  
      #Expected (and then realized) recruitment
      Ry[y+1] <- (4 * steep * ( R0 / 2 ) * SSB[y+1]) / (SSB0 * (1 - steep) + SSB[y+1] * (5 * steep - 1))
      if (autocorr[y+1] != 0 ) { Ry[y+1] <- Ry[y+1] * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1]) }
      numbers[y+1,1,] <- Ry[y+1]
           
    } #closes yearly loop
    
    UpdateDyn[[1]] <- f.values
    UpdateDyn[[2]] <- catch.wght.values <- hist.catch
    UpdateDyn[[3]] <- catch.at.age
    UpdateDyn[[4]] <- catch.at.len
    UpdateDyn[[5]] <- numbers
    UpdateDyn[[6]] <- SSB
    UpdateDyn[[7]] <- Ry
    UpdateDyn[[8]] <- selec
    UpdateDyn[[9]] <- selec.age
    UpdateDyn[[10]]<- hist.catch
    names(UpdateDyn) <- c("f.values","catch.wght.values","catch.at.age","catch.at.len","numbers","SSB","Ry", "selec", 
                    "selec.age", "hist.catch")
    return(UpdateDyn)
 }

#=================================================================================================================================

 # Create Virgin population
 dyn      <- Update_Dynamics(R0, biology = Get_Biology())
 Dyn      <- list() 
 Dyn[[1]] <- dyn$numbers
 Dyn[[2]] <- dyn$SSB
 Dyn[[3]] <- dyn$Ry
 Dyn[[4]] <- dyn$SSB / dyn$SSB[1]
 Dyn[[5]] <- dyn$f.values
 Dyn[[6]] <- dyn$catch.at.age
 Dyn[[7]] <- dyn$catch.at.len
 Dyn[[8]] <- dyn$selec
 Dyn[[9]] <- dyn$selec.age
 Dyn[[10]]<- dyn$catch.wght.values
 names(Dyn) <- c("numbers", "SSB", "Ry", "Depl", "f.values", "catch.at.age", "catch.at.len",
                  "selec", "selec.age", "catch.wght.values") 

#Format Data ----------------------------------------------------------------------------------------------------------
 #Get the survey data from the start of the survey until year 50
 survey.dur     <- start.s.data:(pre.fishery.yrs + setup.yrs - 1)
 survey.out     <- Do_Survey(biology = Get_Biology(), f.values = Dyn$f.values , numbers = Dyn$numbers, index = survey.dur)
 survey.proj    <- numeric(total.yrs)
 survey.proj[survey.dur]    <- survey.out$temp.index
 survey.catch.age.len[survey.dur,,,] <- survey.out$temp.cal
 
#Generate Data for the base fishery years -----------------------------------------------------------------------------
 #Bring in the needed population values from the fishery
 catch.at.age         <- Dyn$catch.at.age#[(pre.fishery.yrs+1):(pre.fishery.yrs+fishery.yrs), , ]
 catch.at.len         <- Dyn$catch.at.len#[(pre.fishery.yrs+1):(pre.fishery.yrs+fishery.yrs), , ]
 
 #Set the seed for the composition sampling
 set.seed(comp.seed[nsim])

 #Survey Data
 for (a in start.s.data:(pre.fishery.yrs + setup.yrs - 1)) {
     #Format the survey data for sampling
     ind <- y           <- a
     survey.catch.len   <- cbind(apply(survey.catch.age.len[ind,,,1],2,sum), apply(survey.catch.age.len[ind,,,2], 2, sum))
     survey.catch.age   <- cbind(apply(survey.catch.age.len[ind,,,1],1,sum), apply(survey.catch.age.len[ind,,,2], 1, sum))
   
     s.lengths[ind,]    <- Multinom_Lengths(catch.type = survey.catch.len,    len.samp = s.len.samp[ind])

     age.out            <- Multinom_Ages(catch.type = survey.catch.age , age.samp=s.age.samp[ind] , AgeError) 
     s.a.ca[ind,]       <- age.out
     s.sample.size[ind] <- sum(age.out)
 }
 
 # Fishery Data
  for (fleet in 1:f.fleets){
    for (a in start.f.data[fleet]:(pre.fishery.yrs + setup.yrs - 1)) {
        ind                     <- a
        f.lengths[fleet,ind,]   <- Multinom_Lengths(catch.type = catch.at.len[ind,fleet,,], len.samp = f.len.samp[ind,fleet]) 
        age.out                 <- Multinom_Ages(catch.type = catch.at.age[ind,fleet,,], age.samp = f.age.samp[ind,fleet] , AgeError)
        f.a.ca[fleet,ind,]      <- age.out
        f.sample.size[fleet,ind]<- sum(age.out)
     }
 }

 #Format the data for SS
 SS.survey.data      <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)),
                               rep(f.fleets + 1, length(survey.dur)), 
                               survey.proj[survey.dur], 
                               rep(ss.survey.cv, 
                               length(survey.dur))) 

 fishery.length.data <- fishery.age.data <- NULL
 for (fleet in 1:f.fleets){
    f.data.dur = start.f.data[fleet]:(pre.fishery.yrs + setup.yrs - 1)
    temp                <- cbind(f.data.dur, 
                               rep(1, length(f.data.dur)), 
                               rep(fleet, length(f.data.dur)), 
                               rep(3, length(f.data.dur)),
                               rep(2, length(f.data.dur)), 
                               f.len.samp[f.data.dur, fleet], 
                               f.lengths[fleet, f.data.dur, ])
    fishery.length.data <<- rbind(fishery.length.data, temp)

    temp                <- cbind(f.data.dur, 
                               rep(1, length(f.data.dur)), 
                               rep(fleet, length(f.data.dur)), 
                               rep(3, length(f.data.dur)), 
                               rep(0, length(f.data.dur)), 
                               rep(1, length(f.data.dur)), 
                               rep(-1, length(f.data.dur)), 
                               rep(-1, length(f.data.dur)),
                               f.sample.size[fleet, f.data.dur], 
                               f.a.ca[fleet,f.data.dur, ])
    fishery.age.data   <<- rbind(fishery.age.data, temp)
 }
 
 survey.length.data  <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)), 
                               rep(f.fleets + 1, length(survey.dur)), 
                               rep(3, length(survey.dur)),
                               rep(2, length(survey.dur)), 
                               s.len.samp[survey.dur], 
                               s.lengths[survey.dur, ])

 survey.age.data     <<- cbind(survey.dur, 
                               rep(1, length(survey.dur)), 
                               rep(f.fleets + 1, length(survey.dur)), 
                               rep(3, length(survey.dur)), 
                               rep(0, length(survey.dur)), 
                               rep(1, length(survey.dur)), 
                               rep(-1, length(survey.dur)), 
                               rep(-1, length(survey.dur)),
                               s.sample.size[survey.dur], 
                               s.a.ca[survey.dur, ])
                            
 n.length.obs        <<- length(fishery.length.data[,6]) + length(survey.length.data[,6])
 n.age.obs           <<- length(fishery.age.data[,9])    + length(survey.age.data[,9])   
 
#Project with the estimated harvest----------------------------------------------------------------------------------------
 setwd(directory)
 Proj <- Est       <- list()
 biology           <- Get_Biology()

 #Pull in values from the Dynamic Setup Years Section
 f.values          <- Dyn$f.values 
 numbers           <- Dyn$numbers
 SSB               <- Dyn$SSB
 SSB0              <- Dyn$SSB[1]
 Ry                <- Dyn$Ry
 catch.at.age      <- Dyn$catch.at.age
 catch.at.len      <- Dyn$catch.at.len
 #catch.wght.values <- Dyn$catch.wght.values
 selec.age         <- Dyn$selec.age
 selec             <- biology$selec
 catch.wght.values <- Dyn$catch.wght.values
 
 #Pull in values from the original survey year
 index.expect[survey.dur]      <- survey.out$temp.index
 
 true.ofl          <- numeric(fishery.yrs)
 true.f            <- numeric(fishery.yrs)
 #fore.catch        <- numeric(total.yrs)
 fore.catch[1:(pre.fishery.yrs + setup.yrs)] <- 0 #hist.catch[1:(pre.fishery.yrs + setup.yrs),]
 
 counter = 0
 decl.overfished = FALSE
 rerun = 0
 overfished.counter = 0

 #Find F values based on  catch-----------------------------------------------------------------------------------------------------
 Findf <- function(f){

    z.m                <- (1 - exp(-(m.m + selec.age[1,2,] * f.split[1] * f + selec.age[2,2,] * f.split[2] * f))) / 
                                    (m.m + selec.age[1,2,] * f.split[1] * f + selec.age[2,2,] * f.split[2] * f)
    z.f                <- (1 - exp(-(m.f + selec.age[1,1,] * f.split[1] * f + selec.age[2,1,] * f.split[2] * f))) / 
                                    (m.f + selec.age[1,1,] * f.split[1] * f + selec.age[2,1,] * f.split[2] * f)

    catch.at.age.f <- cbind( f.split[1] * f * (numbers[y,,1] * selec.age[1,1,]) * z.f, 
                             f.split[2] * f * (numbers[y,,1] * selec.age[2,1,]) * z.f )

    catch.at.age.m <- cbind( f.split[1] * f * (numbers[y,,2] * selec.age[1,2,]) * z.m, 
                             f.split[2] * f * (numbers[y,,2] * selec.age[2,2,]) * z.m)

    #Catch At Length
    mid.temp.f <- numbers[y,,1] * z.f 
    mid.temp.m <- numbers[y,,2] * z.m

    catch.at.len.f <- cbind( ((biology$mid.phi.f * selec[1,,1]) %*% (mid.temp.f)), 
                             ((biology$mid.phi.f * selec[2,,1]) %*% (mid.temp.f)) ) 

    catch.at.len.m <- cbind( ((biology$mid.phi.m * selec[1,,2]) %*% (mid.temp.m)), 
                             ((biology$mid.phi.m * selec[2,,2]) %*% (mid.temp.m)) ) 

    catch.wght     <- c( f.split[1] * f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f[,1]) +
                                           sum(biology$mid.wght.at.len[,2] * catch.at.len.m[,1]) ), 
                         f.split[2] * f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f[,2]) +
                                           sum(biology$mid.wght.at.len[,2] * catch.at.len.m[,2]) ) )   
 
    output <- NULL
    output$catch.wght     <- catch.wght
    output$catch.at.len.f <- catch.at.len.f
    output$catch.at.len.m <- catch.at.len.m
    output$catch.at.age.f <- catch.at.age.f
    output$catch.at.age.m <- catch.at.age.m
    return(output)
 } #End FindF function
 
 #Objective Function----------------------------------------------------------------------------------------------------------
 Obj.Fun.F <- function(f) {
    obj.fun.f <- (sum(Findf(f)$catch.wght) - fore.catch[y])^2
    return(obj.fun.f) 
 }
  
 for (y in (pre.fishery.yrs + setup.yrs):total.yrs) {


    if ( y %% ass.freq == 0 ){
        counter = counter + 1

        writeStarter(starter = "starter.ss")
        writeForecast(forecast = "forecast.ss", y = y)
        writeCtl(ctl, y = y)
        writeDat(dat, y = y, fore.catch = catch.wght.values)
        
        #Read in the Report File to run the bias ramp code 
        if (y <= (pre.fishery.yrs + setup.yrs + 9)) {
            if ( tantalus )  { system("./SS3  > test.txt 2>&1")  }
            if ( !tantalus ) { shell("ss3.exe > test.txt 2>&1")  }
            #Check for convergence
            rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
            gradiant  <- as.numeric(strsplit(rep.new[grep(paste("Convergence_Level",sep=""),rep.new)]," ")[[1]][2])
            virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])

            while(virgin.SB < (SSB0/4) || virgin.SB > (SSB0*4)) {
              rerun = rerun + 1  
              starter.file = SS_readstarter(paste(directory, "starter.ss", sep = ""))
              starter.file$jitter_fraction = 0.10
              SS_writestarter(starter.file, paste(directory, sep = ""), overwrite = T )
              if (tantalus == T) { system("./SS3  > test.txt 2>&1")  }
              if (tantalus == F) { shell("ss3.exe > test.txt 2>&1")  }
              rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
              virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
              if (virgin.SB > (SSB0/4) && virgin.SB < (SSB0*4)) {
                break()
              }
              if(rerun > 2) { break () }
            }

            if (rerun > 2){
              if (tantalus == T) { system("./SS3 -nohess > test.txt 2>&1")  }
              if (tantalus == F) { shell("ss3.exe -nohess > test.txt 2>&1")  }
              last.bias    <- y - 3 
              last.no.bias <- y - 2 
              main.rec.end <- y 
            }
            
            if ( !determ & rerun <= 2){
            	#Read model and rump bias ramp adjustment function
            	rep.bias     <- SS_output(directory, covar = TRUE, printstats = FALSE, forecast = FALSE)
            	new.bias     <- SS_fitbiasramp(rep.bias, 
            	                startvalues = c(start.bias, full.bias , last.bias, last.no.bias ,max.bias.adj))
            	start.bias   <- new.bias$df[1,1]
            	full.bias    <- new.bias$df[2,1]
            	last.bias    <- y - 3 #new.bias$df[3,1]
            	last.no.bias <- y - 2 #new.bias$df[4,1]
            	max.bias.adj <- new.bias$df[5,1]
            	main.rec.end <- y #last.bias - 1
            	converge     <- new.bias$newbias$message
            }

            # Data Weighting
            # f.age.wght = as.numeric(SSMethod.TA1.8(rep.bias, type ='age', fleet = 1, part = 0, pick.gender = 3)[1])
            # f.age.wght = min(round(f.age.wght,2), 1)
            # s.age.wght = as.numeric(SSMethod.TA1.8(rep.bias, type ='age', fleet = 2, part = 0, pick.gender = 3)[1])
            # s.age.wght = min(round(s.age.wght,2), 1)     
        }      
        
        if (y > (pre.fishery.yrs + setup.yrs + 9)){
        	main.rec.end <- y
        	last.bias    <- last.bias + ass.freq
        	last.no.bias <- last.no.bias + ass.freq
        }

        writeCtl(ctl = "sim.ctl", y = y) 
        if ( tantalus ) { system("./SS3 -nohess > test.txt 2>&1")  }
        if (!tantalus ) { shell("ss3.exe -nohess > test.txt 2>&1") }

        file.copy("sim.ctl", paste(directory,"/ctl/sim.ctl",sep =""))
        file.rename(paste(directory,"/ctl/sim.ctl",sep =""), 
                    paste(directory,"/ctl/sim",nsim,"_",y,".ctl",sep =""))    
                        
        file.copy("sim.dat", paste(directory,"/ctl/sim.dat",sep =""))
        file.rename(paste(directory,"/ctl/sim.dat",sep =""), 
                    paste(directory,"/ctl/sim",nsim,"_",y,".dat",sep =""))   

        file.copy("Report.sso", paste(directory,"/report/Report.sso",sep =""))
        file.rename(paste(directory,"/report/Report.sso",sep =""), 
                    paste(directory,"/report/Report",nsim,"_",y,".sso",sep ="")) 
        
        rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
        gradiant  <- as.numeric(strsplit(rep.new[grep(paste("Convergence_Level",sep=""),rep.new)]," ")[[1]][2])
        virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
        while(virgin.SB < (SSB0/4) || virgin.SB > (SSB0*4)) {
          rerun = rerun + 1  
          starter.file = SS_readstarter(paste(directory, "starter.ss", sep = ""))
          starter.file$jitter_fraction = 0.05
          SS_writestarter(starter.file, paste(directory, sep = ""), overwrite = T )
          if (tantalus == T) { system("./SS3  > test.txt 2>&1")  }
          if (tantalus == F) { shell("ss3.exe > test.txt 2>&1")  }
          rep.new   <- readLines(paste(directory, "Report.sso", sep=""))
          virgin.SB <- as.numeric(strsplit(rep.new[grep(paste("SPB_Virgin",sep=""),rep.new)]," ")[[1]][3])
          if (virgin.SB > (SSB0/4) && virgin.SB < (SSB0*4)) {
            break()
          }
          if(rerun > 2) { break () }
        }

        #Read in the Report File and Save Quantities 
        rep.out   <- Rep_Summary(rep.new, y)
        ind       <- y - 1
        Recruits[1:ind,counter]  <- rep.out$Recruits
        
        OFL[y:(y+ass.freq - 1)]     <- rep.out$OFL
        ForeCat[y:(y+ass.freq - 1)] <- rep.out$ForeCatch
        FSPR[,counter]              <- rep.out$FSPR
        m.store[,counter]           <- rep.out$M
        lmin.store[,counter]        <- rep.out$Lmin
        lmax.store[,counter]        <- rep.out$Lmax
        k.store[,counter]           <- rep.out$k
        R0.out[,counter]            <- rep.out$R0
        f.selex[1,1,,counter]       <- rep.out$f.selex[,1]
        f.selex[2,1,,counter]       <- rep.out$f.selex[,2]
        f.selex[1,2,,counter]       <- rep.out$f.selex.m[,1]
        f.selex[2,2,,counter]       <- rep.out$f.selex.m[,2]
        s.selex[1,,counter]         <- rep.out$s.selex
        s.selex[2,,counter]         <- rep.out$s.selex.m
        ind                         <- y #- pre.fishery.yrs
        SB[1:ind,counter]           <- rep.out$SB
        Bratio[1:ind,counter]       <- rep.out$Depl
        Gradiant.out[,counter]      <- gradiant
               
        Est[[1]] <- OFL
        Est[[2]] <- ForeCat
        Est[[3]] <- FSPR
        Est[[4]] <- m.store
        Est[[5]] <- R0.out
        Est[[6]] <- SB
        Est[[7]] <- Bratio
        Est[[8]] <- f.selex
        Est[[9]] <- s.selex
        Est[[10]]<- Recruits
        Est[[11]]<- Gradiant.out
        Est[[12]]<- rerun
        Est[[13]]<- lmin.store
        Est[[14]]<- lmax.store
        Est[[15]]<- k.store
        Est[[16]]<- m.store

        names(Est) <- c("OFL","ForeCat","FSPR","m.store","R0.out","SB","Bratio","f.selex","s.selex","Recruits",
                          "Gradiant.out", "rerun", "lmin.store", "lmax.store", "k.store", "m.store")
        save(Est, file=estimates)  
        
        #Pull the ACL from the Report File and use that for the next two years 
        fore.catch[y:(y + ass.freq - 1)]         <- rep.out$ForeCatch 
        #temp.catch         <- rep.out$ForeCatch
        
      } #Close the assessment loop  
   
                                    
    # Calculate what the true F should be based on the HCR
    #if (SSB[y-1] / SSB0 >= ctl.rule.tgt) {
    #  Fadj      <- spr.target 
    #}
    #if (SSB[y-1] / SSB0 < ctl.rule.tgt) {
    #  Fadj      <- (spr.target  * ((SSB[y-1] / SSB[1]) - ctl.rule.thres) * ctl.rule.tgt) / 
    #              ((ctl.rule.tgt - ctl.rule.thres) * (SSB[y-1] / SSB[1]))
    #}
    #if (SSB[y-1] / SSB0 < ctl.rule.thres) {
    #  Fadj      <- 0 
    #}  
    #true.fspr[y]     <- Fadj
    #true.fspr.ofl[y] <- Findf(Fadj)$catch.wght  
    
    # Calculate what the true F should be based on the HCR
    #if (SSB[y-1] / SSB0 >= ctl.rule.tgt) {
    #  Fadj      <- Fmsy 
    #}
    #if (SSB[y-1] / SSB0 < ctl.rule.tgt) {
    #  Fadj      <- (Fmsy * ((SSB[y-1] / SSB[1]) - ctl.rule.thres) * ctl.rule.tgt) / 
    #              ((ctl.rule.tgt - ctl.rule.thres) * (SSB[y-1] / SSB[1]))
    #}
    #if (SSB[y-1] / SSB0 < ctl.rule.thres) {
    #  Fadj      <- 0 
    #}
    #  
    #true.f[y]   <- Fadj
    #true.ofl[y] <- Findf(Fadj)$catch.wght  


    #Determine the F value from the estimated ACL      
    f.total               <- optimize(Obj.Fun.F, lower = 0, upper = 2)$minimum  
    find.f                <- Findf(f.total)
    f.values[,y]          <- f <- f.total * f.split
    for (fleet in 1:f.fleets){
      catch.at.age[y,fleet,,1]   <- find.f$catch.at.age.f[,fleet]
      catch.at.age[y,fleet,,2]   <- find.f$catch.at.age.m[,fleet]
      catch.at.len[y,fleet,,1]   <- find.f$catch.at.len.f[,fleet]
      catch.at.len[y,fleet,,2]   <- find.f$catch.at.len.m[,fleet]
    }
    catch.wght.values[y,] <- find.f$catch.wght 
         
    # survival at age by gender
    S.f                  <- exp(-(m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2]))
    S.m                  <- exp(-(m.m + selec.age[1,2,] * f[1] + selec.age[2,2,] * f[2]))
      
    #Update the numbers and remove the catch by applying the solved for f value
    numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
    numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
    numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * S.f[ages]
    numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * S.m[ages]
      
    SSB[y+1] <- max(0, sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages]))

    #Expected (and then realized) recruitment
    Ry[y+1] <- (4 * steep * (R0 / 2) * SSB[y+1]) / (SSB0 * (1-steep) + SSB[y+1] * (5 * steep - 1))
    Ry[y+1] <- Ry[y+1] * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1])
         
    numbers[y+1,1,] <- Ry[y+1]   
    
    #Observation Index------------------------------------------------------------------------------------------------------------
    survey.out                 <- Do_Survey(biology = Get_Biology(), f.values = f.values , numbers = numbers, index = y)
    survey.proj[y]             <- survey.out$temp.index
    index.expect[y]            <- survey.out$temp.index
    survey.catch.age.len[y,,,] <- survey.out$temp.cal
    survey.dur                 <- start.s.data:y
    
    #Format data for SS 
    SS.survey.data <- cbind(survey.dur, 
                            rep(1, length(survey.dur)),
                            rep(f.fleets + 1, length(survey.dur)), 
                            survey.proj[survey.dur], 
                            rep(ss.survey.cv, 
                            length(survey.dur))) 
                    
    #Get Composition Data    
    survey.catch.len  <- cbind(apply(survey.catch.age.len[y,,,1],2,sum), apply(survey.catch.age.len[y,,,2], 2, sum))
    survey.catch.age  <- cbind(apply(survey.catch.age.len[y,,,1],1,sum), apply(survey.catch.age.len[y,,,2], 1, sum))
    # Survey Comp Data
    s.lengths[y,]     <- Multinom_Lengths(catch.type = survey.catch.len,    len.samp = s.len.samp[y])
    age.out           <- Multinom_Ages(catch.type = survey.catch.age, age.samp = s.age.samp[y], AgeError) 
    s.a.ca[y,]        <- age.out
    s.sample.size[y]  <- sum(age.out)
    # Fishery Comp Data
    for(fleet in 1:f.fleets){
      f.lengths[fleet,y,]     <- Multinom_Lengths(catch.type = catch.at.len[y,fleet,,], len.samp = f.len.samp[y,fleet]) 
      age.out                 <- Multinom_Ages(catch.type = catch.at.age[y,fleet,,],    age.samp = f.age.samp[y,fleet], AgeError)
      f.a.ca[fleet,y,]        <- age.out
      f.sample.size[fleet, y] <- sum(age.out)
    }
     
    #Format the data for SS
    # Fishery Data
    fishery.length.data <- fishery.age.data <- NULL
    for (fleet in 1:f.fleets){
      f.data.dur <- start.f.data[fleet]:y
      temp              <- cbind(f.data.dur, 
                                 rep(1, length(f.data.dur)), 
                                 rep(fleet, length(f.data.dur)), 
                                 rep(3, length(f.data.dur)),
                                 rep(2, length(f.data.dur)), 
                                 f.len.samp[f.data.dur,fleet], 
                                 f.lengths[fleet, f.data.dur, ])
      fishery.length.data <<- rbind(fishery.length.data, temp)

      temp              <- cbind(f.data.dur, 
                                 rep(1, length(f.data.dur)), 
                                 rep(fleet, length(f.data.dur)), 
                                 rep(3, length(f.data.dur)), 
                                 rep(0, length(f.data.dur)), 
                                 rep(1, length(f.data.dur)), 
                                 rep(-1,length(f.data.dur)), 
                                 rep(-1, length(f.data.dur)),
                                 f.age.samp[f.data.dur,fleet], 
                                 f.a.ca[fleet,f.data.dur, ])
      fishery.age.data <<- rbind(fishery.age.data, temp)
    }

    survey.length.data  <<- cbind(survey.dur, 
                                  rep(1, length(survey.dur)), 
                                  rep(f.fleets + 1, length(survey.dur)), 
                                  rep(3, length(survey.dur)),
                                  rep(2, length(survey.dur)), 
                                  s.len.samp[survey.dur], 
                                  s.lengths[survey.dur, ]) 

    survey.age.data     <<- cbind(survey.dur, 
                                  rep(1, length(survey.dur)), 
                                  rep(f.fleets + 1, length(survey.dur)), 
                                  rep(3, length(survey.dur)), 
                                  rep(0, length(survey.dur)), 
                                  rep(1, length(survey.dur)), 
                                  rep(-1, length(survey.dur)), 
                                  rep(-1, length(survey.dur)),
                                  s.age.samp[survey.dur], 
                                  s.a.ca[survey.dur, ]) 
    
    n.length.obs        <<- length(fishery.length.data[,6]) + length(survey.length.data[,6])
    n.age.obs           <<- length(fishery.age.data[,9])    + length(survey.age.data[,9])
     
    Proj[[1]] <- f.values 
    Proj[[2]] <- catch.wght.values 
    Proj[[3]] <- catch.at.age
    Proj[[4]] <- catch.at.len
    Proj[[5]] <- numbers 
    Proj[[6]] <- SSB
    Proj[[7]] <- SSB / SSB[1]
    Proj[[8]] <- Ry
    Proj[[9]] <- index.expect
    Proj[[10]]<- fore.catch
    Proj[[11]]<- true.ofl
    Proj[[12]]<- true.f
    Proj[[13]]<- f.len.samp
    Proj[[14]]<- s.len.samp
    Proj[[15]]<- f.age.samp
    Proj[[16]]<- s.age.samp
    Proj[[17]]<- selec
    Proj[[18]]<- selec.age

    names(Proj) <- c("f.values", "catch.wght.values", "catch.at.age", "catch.at.len", "numbers", "SSB", "depl","Ry", "index.expect",
                "fore.catch","true.ofl", "true.f","f.len.samp","s.len.sam","f.age.samp","s.age.samp", "selec", "selec.age")
    save(Proj, file=projections)  
        
   } #closes yearly projection loop

  print(paste("Simulation",nsim, "Done", sep =" "))
 } #end simulation loop
 
