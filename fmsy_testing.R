

Get_Fmsy <- funtion(){
	R0 = 10000
	spr.target = 0.30
	total.yrs <- 100
	f.val <- seq(0, 1,0.01)
	source("C:\\Flatfish_MSC\\code\\functions\\LH_parameters.R")
	source("C:\\Flatfish_MSC\\code\\functions\\Get_Biology.R")
	L1f = L1f.mean ; L1m = L1m.mean
	L2f = L2f.mean ; L2m = L2m.mean
	m.m = m.m.mean ; m.f = m.f.mean
	kf  = kf.mean  ; km  = km.mean
	biology <- Get_Biology()

 	numbers      <- array(0,dim=c(total.yrs,ages,2)) 
 	SSB <- Ry <- hist.catch   <- matrix(0, total.yrs,1)  
 	SSB.fraction <- matrix(NA,1,length(f.val))
 	SSB.store    <- matrix(NA,total.yrs,length(f.val))
 	Ry.store     <- matrix(NA,total.yrs,length(f.val))
 	catch.store  <- matrix(NA,total.yrs-1,length(f.val))

 	steep.orig = steep
 	steep = 1

 	Solve_Dynamics <- function(R0, f, biology){
   		SolveDyn <- list() 
   		
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
   		     
   		for(y in 1:(total.yrs-1)) {  

   		  selec.age.m     <- biology$selec.age.m
   		  selec.age.f     <- biology$selec.age.f
   		  selec           <- biology$selec

   		  z.m           <- (1 - exp(-(m.m + selec.age.m * f))) / (m.m + selec.age.m * f)
   		  z.f           <- (1 - exp(-(m.f + selec.age.f * f))) / (m.f + selec.age.f * f)

   		  # Catch At Length
   		  mid.temp.f <- numbers[y,,1] * exp(-(m.f + selec.age.f * f)*0.50) #z.f 
   		  mid.temp.m <- numbers[y,,2] * exp(-(m.m + selec.age.m * f)*0.50) #z.m 
   		  catch.at.len.f <- ((biology$mid.phi.f * selec[,1]) %*% (mid.temp.f))
   		  catch.at.len.m <- ((biology$mid.phi.m * selec[,2]) %*% (mid.temp.m))
	      hist.catch[y] <- f * (sum(biology$mid.wght.at.len[,1] * catch.at.len.f) +
   		                        sum(biology$mid.wght.at.len[,2] * catch.at.len.m) )  
   		    
   		  # survival at age by gender
   		  S.f <- exp(-(m.f + selec.age.f * f))
   		  S.m <- exp(-(m.m + selec.age.m * f))
   		    
   		  # Update the numbers and remove the catch by applying the solved for f value
   		  numbers[y+1, 2:ages, 1] <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
   		  numbers[y+1, 2:ages, 2] <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
   		  numbers[y+1, ages, 1]   <- numbers[y+1, ages, 1] + numbers[y, ages, 1] * exp(-m.f - selec.age.f[ages] * f)
   		  numbers[y+1, ages, 2]   <- numbers[y+1, ages, 2] + numbers[y, ages, 2] * exp(-m.m - selec.age.m[ages] * f)
   		    
   		  SSB[y+1] <- sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages])
  
   		  #Expected (and then realized) recruitment
   		  Ry[y+1] <- (4 * steep * ( R0 / 2 ) * SSB[y+1]) / (SSB0 * (1 - steep) + SSB[y+1] * (5 * steep - 1))
   		  #if (autocorr[y+1] != 0 ) { Ry[y+1] <- Ry[y+1] * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1]) }
   		  numbers[y+1,1,] <- Ry[y+1]
   		       
   		} #closes yearly loop
   		
   		#SolveDyn[[1]] <- f.values
   		#SolveDyn[[1]] <- hist.catch
   		SolveDyn[[1]] <- SSB
   		SolveDyn[[2]] <- Ry
   		SolveDyn[[3]] <- hist.catch
   		names(SolveDyn) <- c("SSB","Ry", "hist.catch")
   		return(SolveDyn)
    }

 	f.out <-uniroot(function(f)(Solve_Dynamics(R0, f, biology)$SSB[total.yrs-1]/
                    Solve_Dynamics(R0, f, biology)$SSB[1])-spr.target,lower=0.01,upper=0.70,tol=0.0001) 

 	steep = 0.85
 	f.out <-uniroot(function(f)(Solve_Dynamics(R0, f, biology)$SSB[total.yrs-1]/
                    Solve_Dynamics(R0, f, biology)$SSB[1])-0.25,lower=0.01,upper=0.70,tol=0.0001) 

 	steep = 0.85
	#Loop over F values
	for (l in 1:length(f.val))
	{
	    #CatchTot<-Catch.Values[l]
	    f <- f.val[l]
	    # Create Virgin population
	    dyn  <- Solve_Dynamics(R0, f, biology = Get_Biology())
	    SSB  <- dyn$SSB
	    Ry   <- dyn$Ry
	    catch.wght <- dyn$hist.catch
	    
	    SSB.fraction[,l] <-SSB[total.yrs,]/SSB[1]
	    SSB.store[,l]    <- SSB[1:total.yrs]
	    Ry.store[,l]     <- Ry[1:total.yrs]
	    catch.store[,l]  <- catch.wght[1:(total.yrs-1)]
	}

	plot(SSB.store[(total.yrs-1),], catch.store[(total.yrs-1),], ylab="Yield", xlab="SSB",type="b")
	plot(SSB.fraction, catch.store[(total.yrs-1),], ylab = "Yield", xlab = "depl", type = "b")

	plot(f.val, catch.store[(total.yrs-1),],ylab="Yield",xlab="f",type="b")

	max.f <- which.max(catch.store[total.yrs-1,])
	max.yield <- catch.store[total.yrs-1,max.f]
	SSB.msy <- SSB.store[total.yrs,max.f]
	F.msy <- f.val[max.f]


}