############################################
#   Flatfish Harvest Control Rule MSE      #
#Load in the files from the Operating Model#
#        and the estimation model.         #
#    									   #
#		Data Processing for all runs       #
#      Created January 13, 2016 by         #
#           Chantel Wetzel                 #
############################################


drive = "C:"
sim.range = c(1,200)

steep.vec <- c("Steep_85", "Steep_75", "Steep_95", "Steep_85_75", "Steep_85_95")#, "Steep_85_auto")
hcr.vec   <- c( "hcr_25_5_ramp_constant", 
			    "hcr_40_10_ramp_constant", 
				"hcr_30_10_ramp_constant", 
				"hcr_20_05_ramp_constant")

for (a in 1:length(steep.vec)){

	# Dimensions by Steepness  ===============================================================
	code.dir = paste("C:/Flatfish_MSC/", sep = "")
	# code.dir = paste("C:/Users/Chantell.Wetzel/Documents/GitHub/Ch4_Flatfish_HCR/", sep = "")
	source(paste(code.dir,"code/functions/LH_parameters.R",sep = ""))
	source(paste(code.dir,"code/functions/Data_Scenarios.R",sep = ""))
	# The specific years an assessment was performed
	ass.yrs      = seq(ages + setup.yrs - 1, total.yrs, ass.freq) 
	# Years to summarize the performance over
	sum.yrs      = (total.yrs - 24): total.yrs
	# Target stock sizes by hcr
	target = c(0.25, 0.40, 0.30, 0.20)
	msst.vec = 0.50*target

	# Create the Storage Matrices and Arrays ================================================================================
	ssb   <- array(0, dim = c(length(hcr.vec), total.yrs, sim.range[2] - sim.range[1] + 1))
	depl  <- array(0, dim = c(length(hcr.vec), total.yrs, sim.range[2] - sim.range[1] + 1))
	rec   <- array(0, dim = c(length(hcr.vec), total.yrs, sim.range[2] - sim.range[1] + 1))
	catch <- array(0, dim = c(length(hcr.vec), total.yrs, sim.range[2] - sim.range[1] + 1))
	
	ssb.est    <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, sim.range[2] - sim.range[1] + 1))
	depl.est   <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, sim.range[2] - sim.range[1] + 1))
	rec.est    <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, sim.range[2] - sim.range[1] + 1))
	
	acl.est    <- array(0, dim = c(length(hcr.vec), total.yrs, sim.range[2] - sim.range[1] + 1))
	ofl.est    <- array(0, dim = c(length(hcr.vec), total.yrs, sim.range[2] - sim.range[1] + 1))
	
	cum.catch  <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	mean.catch <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	catch.sum  <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	aav        <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	below.msst.est  <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	below.msst.true <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	target.est  <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	target.true <- array(0, dim = c(length(hcr.vec), project.yrs + 1))

	med.ssb.est  <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, 3))
	med.depl.est <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, 3))
	med.rec.est  <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, 3))
    med.ssb      <- array(0, dim = c(length(hcr.vec), total.yrs, 3))
	med.depl     <- array(0, dim = c(length(hcr.vec), total.yrs, 3))
	med.rec      <- array(0, dim = c(length(hcr.vec), total.yrs, 3))
	
	for(b in 1:length(hcr.vec)){
		dir = paste(drive,"/Flatfish_MSC/",steep.vec[a], "/hcr_option_", hcr.vec[b], "_sims_", sim.range[1], "_", sim.range[2], sep = "")

		for(c in sim.range[1]:sim.range[2]){
			# Operating Model
			load(paste(dir, "/save/om_proj_", c, sep = ""))
			ssb[b,,c]  <- Proj$SSB[1:total.yrs]
			depl[b,,c] <- Proj$depl[1:total.yrs]
			catch[b,,c]<- Proj$fore.catch[1:total.yrs]
			rec[b,,c]  <- Proj$Ry[1:total.yrs] * 2

			# Estimation Model
			load(paste(dir, "/save/ss_ests_", c, sep = ""))
			ssb.est[b, ,1:ass.num, c] <- Est$SB [1:total.yrs, 1:ass.num]
			depl.est[b,,1:ass.num, c] <- Est$Bratio[1:total.yrs, 1:ass.num]
			rec.est[b, ,1:ass.num, c] <- Est$Recruits[1:total.yrs, 1:ass.num]
			acl.est[b,,c]      <- Est$ForeCat[1:total.yrs]
			ofl.est[b,,c]      <- Est$OFL[1:total.yrs]
		} # simulation loop closure

		# Calculate medians and relative errors====================================================
		med.ssb[b,,]   = t(apply( ssb[b,,],  1, quantile, c(0.025, 0.50, 0.975), na.rm = T))
		med.depl[b,,]  = t(apply(depl[b,,],  1, quantile, c(0.025, 0.50, 0.975), na.rm = T))
		med.rec[b,,]   = t(apply( rec[b,,],  1, quantile, c(0.025, 0.50, 0.975), na.rm = T))

		for (d in 1:ass.num){
			ind = ages:ass.yrs[d]
			med.ssb.est [b,ind,d,] <- t(apply(ssb.est [b,ind,d,], 1, quantile, c(0.025,0.50,0.975)))
			med.depl.est[b,ind,d,] <- t(apply(depl.est[b,ind,d,], 1, quantile, c(0.025,0.50,0.975)))
			ind = ages:(ass.yrs[d]- 1)
			med.rec.est [b,ind,d,] <- t(apply(rec.est [b,ind,d,], 1, quantile, c(0.025,0.50,0.975)))
		}

		# Calculate performance metrics ==========================================================

		for(c in sim.range[1]:sim.range[2]){
			# AAV over the summary period
			abs.catch = mapply(function(x) abs(acl.est[b,x-1,c] - acl.est[b,x,c]), x = sum.yrs)
			sum.catch.by.sim = sum(acl.est[b,sum.yrs,c])
			aav[b,c] = 100*(sum(abs.catch))/sum.catch.by.sim

			# Total catch over the summary period
		    catch.sum[b,c] = sum.catch.by.sim
		}	

		# Calculate the percentage the P < MSST
		for (d in 1:ass.num){
			if (d == 1) { 
				ind = ass.yrs[1] ; temp = 1 
				below.msst.est[b,temp]  = sum(depl.est[b,ind,d,] < msst.vec[b])
				below.msst.true[b,temp] = sum(depl[b,ind,] < msst.vec[b])
			}

			if (d != 1) { 
				ind = (ass.yrs[d-1]+1):ass.yrs[d]
				temp = (ass.yrs[d] - ass.yrs[1] - 3):(ass.yrs[d] - ass.yrs[1] + 1) 
				below.msst.est[b,temp]  = apply(depl.est[b,ind,d,] < msst.vec[b], 1, sum)
				below.msst.true[b,temp] = apply(depl[b,ind,] < msst.vec[b], 1, sum)
			}
		}
		
		# Calculate the percentage the P < 0.10 +- Target Stock Size
		for (d in 1:ass.num){
			if (d == 1) { 
				ind = ass.yrs[1] ; temp = 1 

				target.est[b,temp]  = sum(ifelse(depl.est[b,ind,d,] < target[b] - 0.10*target[b], 0, 
									      ifelse(depl.est[b,ind,d,] < target[b] + 0.10*target[b], 1, 0)))
				target.true[b,temp] = sum(ifelse(depl[b,ind,] < target[b] - 0.10*target[b], 0, 
									      ifelse(depl[b,ind,] < target[b] + 0.10*target[b], 1, 0)))
			}

			if (d != 1) { 
				ind = (ass.yrs[d-1]+1):ass.yrs[d]
				temp = (ass.yrs[d] - ass.yrs[1] - 3):(ass.yrs[d] - ass.yrs[1] + 1) 
				target.est[b,temp]  = apply(ifelse(depl.est[b,ind,d,] < target[b] - 0.10*target[b], 0, 
									        ifelse(depl.est[b,ind,d,] < target[b] + 0.10*target[b], 1, 0)), 1, sum)
				target.true[b,temp] = apply(ifelse(depl[b,ind,] < target[b] - 0.10*target[b], 0, 
									        ifelse(depl[b,ind,] < target[b] + 0.10*target[b], 1, 0)), 1, sum)
			}
		}	

	} # HCR loop closure

	om.all <- ss.all <- hcr.all <- med.all <-  list()

	om.out 	<- paste0(drive,"/Flatfish_MSC/output/", steep.vec[a], "_om_all")
	ss.out 	<- paste0(drive,"/Flatfish_MSC/output/", steep.vec[a], "_ss_all")
	hcr.out <- paste0(drive,"/Flatfish_MSC/output/", steep.vec[a], "_hcr_all")
	med.out <- paste0(drive,"/Flatfish_MSC/output/", steep.vec[a], "_medians_all")

	om.all$ssb  <- ssb
	om.all$depl <- depl
	om.all$rec  <- rec
	om.all$catch<- catch
	save(om.all, file = om.out)

	ss.all$ssb.est 	<- ssb.est
	ss.all$depl.est	<- depl.est
	ss.all$rec.est  <- rec.est
	ss.all$acl.est  <- acl.est
	ss.all$ofl.est  <- ofl.est
	save(ss.all, file = ss.out)

	hcr.all$aav            <- aav
	hcr.all$catch.sum      <- catch.sum
	hcr.all$below.msst.est <- below.msst.est
	hcr.all$below.msst.true<- below.msst.true
	hcr.all$target.est     <- target.est
	hcr.all$target.true    <- target.true
	save(hcr.all, file = hcr.out)

	med.all$med.ssb	    <- med.ssb
	med.all$med.depl    <- med.depl
	med.all$med.rec     <- med.rec
	med.all$med.ssb.est <- med.ssb.est
	med.all$med.depl.est<- med.depl.est
	med.all$med.rec.est <- med.rec.est
	save(med.all, file = med.out)
}