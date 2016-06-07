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
sim.range = c(1, 200)

steep.vec <- c("Steep_85", "Steep_75", "Steep_95", 
				"Steep_85_75", "Steep_85_95", "Steep_85_data_30",
				"Steep_85_sigmaR_60","Steep_85_auto","Steep_85_auto_sigmaR_60")
hcr.vec   <- c( "hcr_20_5_ramp_constant",
				"hcr_25_5_ramp_constant", 
				"hcr_30_10_ramp_constant", 
			    "hcr_40_10_ramp_constant")

for (a in 1:length(steep.vec)){

	# Dimensions by Steepness  ===============================================================
	#code.dir = paste("C:/Flatfish_MSC/code", sep = "")
	reduce.data = FALSE
	code.dir = paste("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/", sep = "")
	source(paste(code.dir,"/functions/LH_parameters.R",sep = ""))
	source(paste(code.dir,"/functions/Data_Scenarios.R",sep = ""))
	load("C:/PhD/Chapter4/parm_dist_list")
	# The specific years an assessment was performed
	ass.yrs      = seq(ages + setup.yrs - 1, total.yrs, ass.freq) 
	# Years to summarize the performance over
	sum.yrs      = (total.yrs - 25): (total.yrs - 1)
	# Target stock sizes by hcr
	target = c(0.20, 0.25, 0.30, 0.40)
	msst.vec = 0.50*target
	# Quantiles for summary statistics
	quant.vec = c(0.10, 0.50, 0.90)

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
	m.est      <- array(0, dim = c(length(hcr.vec), 2, ass.num, sim.range[2] - sim.range[1] + 1))
	k.est      <- array(0, dim = c(length(hcr.vec), 2, ass.num, sim.range[2] - sim.range[1] + 1))
	lmax.est   <- array(0, dim = c(length(hcr.vec), 2, ass.num, sim.range[2] - sim.range[1] + 1))

	catch.var  <- array(0, dim = c(length(hcr.vec), length(sum.yrs)))
	cum.catch  <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	mean.catch <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	catch.sum  <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	catch.ave  <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	aav        <- array(0, dim = c(length(hcr.vec), sim.range[2] - sim.range[1] + 1))
	below.msst.est  <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	below.msst.true <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	target.est      <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	target.true     <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	target.20.est   <- array(0, dim = c(length(hcr.vec), project.yrs + 1))
	target.20.true  <- array(0, dim = c(length(hcr.vec), project.yrs + 1))

	med.ssb.est  <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, 3))
	med.depl.est <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, 3))
	med.rec.est  <- array(0, dim = c(length(hcr.vec), total.yrs, ass.num, 3))
    med.ssb      <- array(0, dim = c(length(hcr.vec), total.yrs, 3))
	med.depl     <- array(0, dim = c(length(hcr.vec), total.yrs, 3))
	med.rec      <- array(0, dim = c(length(hcr.vec), total.yrs, 3))
	re.ssb       <- array(0, dim = c(length(hcr.vec), length(sum.yrs), sim.range[2] - sim.range[1] + 1))
	re.depl      <- array(0, dim = c(length(hcr.vec), length(sum.yrs), sim.range[2] - sim.range[1] + 1))
	re.m 		 <- array(0, dim = c(length(hcr.vec), 2, ass.num, sim.range[2] - sim.range[1] + 1))
	re.k		 <- array(0, dim = c(length(hcr.vec), 2, ass.num, sim.range[2] - sim.range[1] + 1))
	re.lmax		 <- array(0, dim = c(length(hcr.vec), 2, ass.num, sim.range[2] - sim.range[1] + 1))
	prop.catch   <- array(0, dim = c(length(hcr.vec), length(sum.yrs), sim.range[2] - sim.range[1] + 1))
	prop.msy     <- array(0, dim = c(length(hcr.vec), length(sum.yrs), sim.range[2] - sim.range[1] + 1))

	if (steep.vec[a] == "Steep_85") { load("C:/PhD/Chapter4/Steep_85/quants_85") ; msy.85 <- Out$msy }
	if (steep.vec[a] == "Steep_75") { load("C:/PhD/Chapter4/Steep_75/quants_75") ; msy.75 <- Out$msy }
	if (steep.vec[a] == "Steep_95") { load("C:/PhD/Chapter4/Steep_95/quants_95") ; msy.95 <- Out$msy }

	msy  <- Out$msy 
	bmsy <- Out$bmsy
	spr  <- Out$max.spr
	
	for(b in 1:length(hcr.vec)){
		dir = paste(drive,"/PhD/Chapter4/",steep.vec[a], "/hcr_option_", hcr.vec[b], "_sims_", sim.range[1], "_", sim.range[2], sep = "")

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
			acl.est[b,,c]             <- Est$ForeCat[1:total.yrs]
			ofl.est[b,,c]             <- Est$OFL[1:total.yrs]
			m.est[b,,,c]			  <- Est$m.store
			k.est[b,,,c]			  <- Est$k.store
			lmax.est[b,,,c]			  <- Est$lmax.store
		} # simulation loop closure

		# Calculate medians =========================================================================
		med.ssb[b,,]   = t(apply( ssb[b,,],  1, quantile, quant.vec, na.rm = T))
		med.depl[b,,]  = t(apply(depl[b,,],  1, quantile, quant.vec, na.rm = T))
		med.rec[b,,]   = t(apply( rec[b,,],  1, quantile, quant.vec, na.rm = T))

		for (d in 1:ass.num){
			ind = ages:ass.yrs[d]
			med.ssb.est [b,ind,d,] <- t(apply(ssb.est [b,ind,d,], 1, quantile, quant.vec))
			med.depl.est[b,ind,d,] <- t(apply(depl.est[b,ind,d,], 1, quantile, quant.vec))
			ind = ages:(ass.yrs[d]- 1)
			med.rec.est [b,ind,d,] <- t(apply(rec.est [b,ind,d,], 1, quantile, quant.vec))
		}

		# Calculate relative errors==================================================================
		for(c in sim.range[1]:sim.range[2]){
			re.m[b,1,,c] = (m.est[b,1,,c] - parm.list$m.f[[c]]) / parm.list$m.f[[c]]
			re.m[b,2,,c] = (m.est[b,2,,c] - parm.list$m.m[[c]]) / parm.list$m.m[[c]]
			re.k[b,1,,c] = (k.est[b,1,,c] - parm.list$kf[[c]] ) / parm.list$kf[[c]] 
			re.k[b,2,,c] = (k.est[b,2,,c] - parm.list$km[[c]] ) / parm.list$km[[c]] 
			re.lmax[b,1,,c] = (lmax.est[b,1,,c] - parm.list$L2f[[c]]) / parm.list$L2f[[c]]
			re.lmax[b,2,,c] = (lmax.est[b,2,,c] - parm.list$L2m[[c]]) / parm.list$L2m[[c]]
		}

		for(c in sim.range[1]:sim.range[2]){
			re.ssb[b,,c]  = (ssb.est[b,sum.yrs,ass.num,c]  - ssb[b,sum.yrs,c])  / ssb[b,sum.yrs,c]
			re.depl[b,,c] = (depl.est[b,sum.yrs,ass.num,c] - depl[b,sum.yrs,c]) / depl[b,sum.yrs,c]
		}

		# Calculate performance metrics ===========================================================
		for(c in sim.range[1]:sim.range[2]){
			# AAV over the summary period
			abs.catch = mapply(function(x) abs(acl.est[b,x-1,c] - acl.est[b,x,c]), x = sum.yrs)
			sum.catch.by.sim = sum(acl.est[b,sum.yrs,c])
			aav[b,c] = 100*(sum(abs.catch))/sum.catch.by.sim

			# Total catch over the summary period
		    catch.sum[b,c] = sum.catch.by.sim		    
		}	

		# Calculate the proportion of the MSY attained by each hcr ==================================
		for (c in sim.range[1]:sim.range[2]){
			prop.msy[b,,c] = ofl.est[b,sum.yrs,c] / msy.85[c]
			if (steep.vec[a] == "Steep_75" ) { prop.msy[b,,c] = ofl.est[b,sum.yrs,c] / msy.75[c] }
			if (steep.vec[a] == "Steep_95" ) { prop.msy[b,,c] = ofl.est[b,sum.yrs,c] / msy.95[c] }			
		}

		for (c in sim.range[1]:sim.range[2]){
			prop.catch[b,,c] = acl.est[b,sum.yrs,c] / (msy.85[c] * buffer)
			if (steep.vec[a] == "Steep_75" ) { prop.catch[b,,c] = acl.est[b,sum.yrs,c] / (msy.75[c] * buffer)}
			if (steep.vec[a] == "Steep_95" ) { prop.catch[b,,c] = acl.est[b,sum.yrs,c] / (msy.95[c] * buffer)}			
		}


		# Average catch over simulations
		catch.ave[b,] = catch.sum[b,]/length(sum.yrs)
		# Variance of catch across simulations
		catch.var[b,] = apply(acl.est[b,sum.yrs,], 1, sd)

		# Calculate the percentage the P < MSST
		for (d in 1:ass.num){
			if (d == 1) { 
				ind = ass.yrs[1] ; temp = 1 
				below.msst.est[b,temp]  = sum(depl.est[b,ind,d,] < msst.vec[b])				
			}

			if (d != 1) { 
				ind  = (ass.yrs[d-1]+1):ass.yrs[d]
				temp = (ass.yrs[d] - ass.yrs[1] - 3):(ass.yrs[d] - ass.yrs[1] + 1) 
				below.msst.est[b,temp]  = apply(depl.est[b,ind,d,] < msst.vec[b], 1, sum)				
			}
		}

		below.msst.true[b,] = apply(depl[b,ass.yrs[1]:ass.yrs[length(ass.yrs)],] < msst.vec[b], 1, sum)
		
		# Calculate the percentage the P < 0.10 +- Target Stock Size
		N = sim.range[2] - sim.range[1] + 1
		for (d in 1:ass.num){
			if (d == 1) { 
				ind = ass.yrs[1] ; temp = 1 

				target.est[b,temp]  = sum(ifelse(depl.est[b,ind,d,] < (target[b] - 0.10*target[b]), 0, 
									      ifelse(depl.est[b,ind,d,] < (target[b] + 0.10*target[b]), 1, 0)))

				#target.true[b,temp] = sum(ifelse(depl[b,ind,] < (target[b] - 0.10*target[b]), 0, 
				#					      ifelse(depl[b,ind,] < (target[b] + 0.10*target[b]), 1, 0)))

				target.20.est[b,temp]  = sum(ifelse(depl.est[b,ind,d,] < (target[b] - 0.20*target[b]), 0, 
									         ifelse(depl.est[b,ind,d,] < (target[b] + 0.20*target[b]), 1, 0)))

				#target.20.true[b,temp] = sum(ifelse(depl[b,ind,] < (target[b] - 0.20*target[b]), 0, 
				#					         ifelse(depl[b,ind,] < (target[b] + 0.20*target[b]), 1, 0)))
			}

			if (d != 1) { 
				ind  = (ass.yrs[d-1]+1):ass.yrs[d]
				temp = (ass.yrs[d] - ass.yrs[1] - 3):(ass.yrs[d] - ass.yrs[1] + 1) 
				target.est[b,temp]  = apply(ifelse(depl.est[b,ind,d,] < (target[b] - 0.10*target[b]), 0, 
									        ifelse(depl.est[b,ind,d,] < (target[b] + 0.10*target[b]), 1, 0)), 1, sum)
				#target.true[b,temp] = apply(ifelse(depl[b,ind,] < (target[b] - 0.10*target[b]), 0, 
				#					        ifelse(depl[b,ind,] < (target[b] + 0.10*target[b]), 1, 0)), 1, sum)

				target.20.est[b,temp]  = apply(ifelse(depl.est[b,ind,d,] < (target[b] - 0.20*target[b]), 0, 
									           ifelse(depl.est[b,ind,d,] < (target[b] + 0.20*target[b]), 1, 0)), 1, sum)
				#target.20.true[b,temp] = apply(ifelse(depl[b,ind,] < (target[b] - 0.20*target[b]), 0, 
				#					           ifelse(depl[b,ind,] < (target[b] + 0.20*target[b]), 1, 0)), 1, sum)
			}
		}
		target.est[b,]  = target.est[b,] / N
		target.20.est[b,]  = target.20.est[b,] / N
		
		target.true[b,] = apply(ifelse(depl[b,ass.yrs[1]:ass.yrs[length(ass.yrs)],] < (target[b] - 0.10*target[b]), 0, 
									      ifelse(depl[b,ass.yrs[1]:ass.yrs[length(ass.yrs)],] < (target[b] + 0.10*target[b]), 1, 0)), 1, sum)

		target.20.true[b,] = apply(ifelse(depl[b,ass.yrs[1]:ass.yrs[length(ass.yrs)],] < (target[b] - 0.20*target[b]), 0, 
									         ifelse(depl[b,ass.yrs[1]:ass.yrs[length(ass.yrs)],] < (target[b] + 0.20*target[b]), 1, 0)), 1, sum)

		target.true[b,] = target.true[b,] / N	
		target.20.true[b,] = target.20.true[b,] / N	

	} # HCR loop closure

	om.all <- ss.all <- hcr.all <- med.all <-  list()

	om.out 	<- paste0(drive,"/PhD/Chapter4/output/", steep.vec[a], "_om_all")
	ss.out 	<- paste0(drive,"/PhD/Chapter4/output/", steep.vec[a], "_ss_all")
	hcr.out <- paste0(drive,"/PhD/Chapter4/output/", steep.vec[a], "_hcr_all")
	med.out <- paste0(drive,"/PhD/Chapter4/output/", steep.vec[a], "_medians_all")

	om.all$ssb  <- ssb
	om.all$depl <- depl
	om.all$rec  <- rec
	om.all$catch<- catch
	om.all$msy  <- msy
	om.all$bmsy <- bmsy
	om.all$spr  <- spr
	save(om.all, file = om.out)

	ss.all$ssb.est 	<- ssb.est
	ss.all$depl.est	<- depl.est
	ss.all$rec.est  <- rec.est
	ss.all$acl.est  <- acl.est
	ss.all$ofl.est  <- ofl.est
	ss.all$m.est    <- m.est
	ss.all$k.est    <- k.est
	ss.all$lmax.est <- lmax.est
	save(ss.all, file = ss.out)

	hcr.all$aav            <- aav
	hcr.all$catch.sum      <- catch.sum
	hcr.all$catch.ave      <- catch.ave
	hcr.all$catch.var      <- catch.var
	hcr.all$below.msst.est <- below.msst.est
	hcr.all$below.msst.true<- below.msst.true
	hcr.all$target.est     <- target.est
	hcr.all$target.true    <- target.true
	hcr.all$target.20.est  <- target.20.est
	hcr.all$target.20.true <- target.20.true
	hcr.all$quant.vec      <- quant.vec
	hcr.all$prop.msy       <- prop.msy
	hcr.all$prop.catch     <- prop.catch
	save(hcr.all, file = hcr.out)

	med.all$med.ssb	    <- med.ssb
	med.all$med.depl    <- med.depl
	med.all$med.rec     <- med.rec
	med.all$med.ssb.est <- med.ssb.est
	med.all$med.depl.est<- med.depl.est
	med.all$med.rec.est <- med.rec.est
	med.all$re.m        <- re.m
	med.all$re.k        <- re.k
	med.all$re.lmax     <- re.lmax
	med.all$re.ssb      <- re.ssb
	med.all$re.depl     <- re.depl
	save(med.all, file = med.out)
}