N = 200

f.val <- seq(0, 4, 0.01)
bmsy = msy = max.spr = numeric(0)
depl = matrix(NA, length(f.val), N)
catch = matrix(NA, length(f.val), N)

steep.vec = c(0.85, 0.75, 0.95)
for (s in 1:length(steep.vec))
{
	steep = steep.vec[s]

	for (nsim in 1:N) {
    drive = "C:"
    github = TRUE
    git.wd = "/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR"
    #source("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/functions/Functions.R")
    source(paste0(drive, git.wd, "/functions/LH_parameters.R"))
    source(paste0(drive, git.wd, "/functions/Get_Biology.R"))

    R0 = 1000
    project <- 50
    
    f.fleets = 2
    f.split = c(0.75, 0.25)

 	numbers      <- array(0,dim=c(project,ages,2)) 
 	SSB <- Ry    <- matrix(0, project, 1)  
 	f.values     <- matrix(0, 2, project)  
 	hist.catch   <- matrix(0, project, 1)  
 	#steep.orig = steep
 	#steep = 1

  	#Read in the seeds 
  	load(paste0(drive, git.wd, "/seed_list"))
  	recruit.seed  <- as.numeric(seed.list[[1]][,"recruit.seed"])
  	catch.seed    <- as.numeric(seed.list[[1]][,"catch.seed"])
  	survey.seed   <- as.numeric(seed.list[[1]][,"survey.seed"])
  	comp.seed     <- as.numeric(seed.list[[1]][,"comp.seed"])  
  	age.err.seed  <- as.numeric(seed.list[[1]][,"spare1"]) 
  	select.seed   <- as.numeric(seed.list[[1]][,"spare2"])
	
  	# Read in the parameter lsit and set according to simulation
  	load(paste0(drive, git.wd, "/parm_dist_list"))
  	m.f <- parm.list$m.f[[nsim]]
  	m.m <- parm.list$m.m[[nsim]]
  	kf  <- parm.list$kf[[nsim]]
  	km  <- parm.list$km[[nsim]]
  	L1f <- parm.list$L1f[[nsim]]
  	L1m <- L1f 
  	L2f <- parm.list$L2f[[nsim]]
  	L2m <- parm.list$L2m[[nsim]]

  	#biology <- Get_Biology()
	
  	if (L1f < 16) {a3 = a.linear = 1}
  	if (L1m < 16) {a3 = a.linear = 1}


 	Solve_Dynamics <- function(R0, effort, biology)
 	{	   
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
 	        
 	   for(y in 1:(project-1)) 
 	   { 
	
 	    	selec.age          <- biology$selec.age
 	    	selec              <- biology$selec
 	  
 	    	f                  <- c(f.split[1] * effort, f.split[2] * effort)
 	    	z.m                <- (1 - exp(-(m.m + selec.age[1,2,] * f[1] + selec.age[2,2,] * f[2]))) / 
 	    	                        (m.m + selec.age[1,2,] * f[1] + selec.age[2,2,] * f[2])
 	    	z.f                <- (1 - exp(-(m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2]))) / 
 	    	                        (m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2])
 	    	#for (fleet in 1:f.fleets){
 	    	#  catch.at.age[y,fleet,,1] <- f[fleet] * (numbers[y,,1] * selec.age[fleet,1,]) * z.f
 	    	#  catch.at.age[y,fleet,,2] <- f[fleet] * (numbers[y,,2] * selec.age[fleet,2,]) * z.m
 	    	#}
	
 	     	#Catch At Length
 	     	mid.temp.f <- numbers[y,,1] * z.f 
 	     	mid.temp.m <- numbers[y,,2] * z.m
		
			catch.at.len.fem <- catch.at.len.mal <- NULL
 	     	for (fleet in 1:f.fleets){
 	     	  catch.at.len.fem <- cbind(catch.at.len.fem, ((biology$mid.phi.f * selec[fleet,,1]) %*% (mid.temp.f)))
 	     	  catch.at.len.mal <- cbind(catch.at.len.mal, ((biology$mid.phi.m * selec[fleet,,2]) %*% (mid.temp.m)))
 	     	} 
	
		    hist.catch[y,1] <- f[1] * (sum(biology$mid.wght.at.len[,1] * catch.at.len.fem[,1]) +
 	                                   sum(biology$mid.wght.at.len[,2] * catch.at.len.mal[,1]) ) + 
		    				   f[2] * (sum(biology$mid.wght.at.len[,1] * catch.at.len.fem[,2]) +
 	                                   sum(biology$mid.wght.at.len[,2] * catch.at.len.mal[,2]) )   
 	       
 	     	# survival at age by gender
 	     	S.f <- exp(-(m.f + selec.age[1,1,] * f[1] + selec.age[2,1,] * f[2]))
 	     	S.m <- exp(-(m.m + selec.age[1,2,] * f[1] + selec.age[2,2,] * f[2]))
 	     	  
 	     	#Update the numbers and remove the catch by applying the solved for f value
 	     	numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
 	     	numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
 	     	numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * S.f[ages]
 	     	numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * S.m[ages]
		
 	     	SSB[y+1] <- sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages])
 	 	
 	     	#Expected (and then realized) recruitment
 	     	Ry[y+1] <- (4 * steep * ( R0 / 2 ) * SSB[y+1]) / (SSB0 * (1 - steep) + SSB[y+1] * (5 * steep - 1))
 	     	numbers[y+1,1,] <- Ry[y+1]
 	          
 	   } #closes yearly loop
 	   
 	   out <- NULL
 	   #out$f.values <- f.values
 	   out$hist.catch <- hist.catch
 	   out$ssb <- SSB
 	   out$ry <- Ry
 	   return(out)
 	}


 	#steep = 0.85
 	ssb.fraction <- matrix(NA, 1, length(f.val))
 	ssb.store    <- matrix(NA, project, length(f.val))
 	ry.store     <- matrix(NA, project, length(f.val))
 	catch.store  <- matrix(NA, project-1,length(f.val))
 	#ssb.fraction <- ssb.store <- ry.store <- catch.store <- numeric(0)
	#Loop over F values
	start <- Sys.time()
	for (l in 1:length(f.val))
	{
	    #CatchTot<-Catch.Values[l]
	    effort <- f.val[l]
	    # Create Virgin population
	    dyn  <- Solve_Dynamics(R0, effort, biology = Get_Biology())
	    ssb  <- dyn$ssb
	    ry   <- dyn$ry
	    catch.wght <- dyn$hist.catch
	    
	    ssb.fraction[,l] <- ssb[project,]/ssb[1]
	    ssb.store[,l]    <- ssb[1:project]
	    ry.store[,l]     <- ry [1:project]
	    catch.store[,l]  <- catch.wght[1:(project-1)]
	}
	end <- Sys.time()
	#print(end - start)

	ind  <- sort(catch.store[project - 1,], index.return = TRUE ) 
	bmsy[nsim] <- ssb.fraction[ind$ix[length(f.val)]]
	msy[nsim] <- catch.store[(project-1),ind$ix[length(f.val)]]
	ypr  <- catch.store[project -1,] / (R0/2) 
	bot = ssb.store[1,] / ry.store[1,]
	top = ssb.store[project,] / ry.store[project,]
	spr = top/bot
	max.spr[nsim] = spr[ind$ix[length(f.val)]]

	depl[,nsim] = ssb.fraction
	catch[,nsim] = catch.store[project -1, ]
	
}

quants <- paste0(drive,"/PhD/Chapter4/Steep_", 100*steep,"/quants_", 100*steep)
Out <- list()
Out$bmsy <- bmsy
Out$msy  <- msy
Out$max.spr <- max.spr
Out$depl <- depl
Out$catch <- catch
save(Out, file = quants)
}


	#par(mfrow = c(2,2))
	#plot(ssb.fraction, catch.store[project - 1,], xlim = c(0,1))
	#plot(f.val, catch.store[project - 1,], xlim = c(0,4))
	#plot(f.val, ypr, xlim = c(0, 4))
	#plot(f.val, spr, xlim = c(0, 4))