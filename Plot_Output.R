############################################
#   Flatfish Harvest Control Rule MSE      #
#Load in the files from the Operating Model#
#        and the estimation model.         #
#    									   #
#		Plotting and Reporting             #
#      Created January 13, 2016 by         #
#           Chantel Wetzel                 #
############################################


#Load in the R objects from the Simulation Eval Code ========================================
drive = "C:"

steep.vec <- c("Steep_85", "Steep_75", "Steep_95", "Steep_85_75", "Steep_85_95")#, "Steep_85_auto")
hcr.vec   <- c( "hcr_25_5_ramp_constant", 
			    "hcr_40_10_ramp_constant", 
				"hcr_30_10_ramp_constant", 
				"hcr_20_05_ramp_constant")

dir = paste0(drive, "/Flatfish_MSC/output")

om.out <- ss.out <- hcr.out <- medians.out <- list()
for (a in 1:length(steep.vec)){
	load(paste0(dir,"/", steep.vec[a], "_om_all"))
	om.out[[a]] <- om.all

	load(paste0(dir,"/", steep.vec[a], "_ss_all"))
	ss.out[[a]] <- ss.all

	load(paste0(dir,"/", steep.vec[a], "_hcr_all"))
	hcr.out[[a]] <- hcr.all

	load(paste0(dir,"/", steep.vec[a], "_medians_all"))
	medians.out[[a]] <- med.all
}

par(mfrow = c(3, 2))
x = 150:175; sim = 17; ymax = 10
# Create vectors of the depletion for the last 25 years across assessments
sim.vec = seq(150, 175, 5)
depl <- list()
for(b in 1:length(hcr.vec)){
	temp1 <- NULL
	for(s in 1:length(sim.vec)){
		ind = sim.vec[1]:sim.vec[s]
		ass = 17 - length(sim.vec) + s 
		if (s == 1) { temp1 = rbind(temp1, ss.out[[1]]$depl.est[b, ind, ass, ])}
		if (s != 1) { temp1 = rbind(temp1, ss.out[[1]]$depl.est[b, ind, ass, ])}
	}
	temp.vec = as.vector(temp1)
	depl[[b]] = temp.vec
}

plot(density(depl[[1]]), xlim = c(0, 0.70), ylim = c(0,ymax), main = '', xlab = "B/B0", ylab = '')
	for (b in 2:length(hcr.vec)){
		lines(density(depl[[b]]), col = b)
	}
	abline(v = 0.125, lty = 2, col = 1)
	abline(v = 0.20,  lty = 2, col = 2)
	abline(v = 0.15,  lty = 2, col = 3)
	abline(v = 0.10,  lty = 2, col = 4)


for (a in 1:length(steep.vec)) {
	plot(density(ss.out[[a]]$depl.est[1,x,sim,]), xlim = c(0, 0.70), ylim = c(0,ymax), main = '', xlab = "B/B0", ylab = '')
	for (b in 2:length(hcr.vec)){
		lines(density(ss.out[[a]]$depl.est[b,x,sim,]), col = b)
	}
	abline(v = 0.125, lty = 2, col = 1)
	abline(v = 0.20,  lty = 2, col = 2)
	abline(v = 0.15,  lty = 2, col = 3)
	abline(v = 0.10,  lty = 2, col = 4)
}


par(mfrow = c(3, 2))

x = 46:175
for(a in 1:length(steep.vec)){
	plot(x, medians.out[[a]]$med.depl[1,x,2], type = 'l', ylim = c(0,1), col = 1, lwd = 2)
	lines(x, medians.out[[a]]$med.depl.est[1,x, 17, 2], lty = 2, col = 1)
	for (b in 2:length(hcr.vec)){
		lines(x, medians.out[[a]]$med.depl[b,x,2], lty = 1, lwd = 2, col = b)
		lines(x, medians.out[[a]]$med.depl.est[b,x, 17, 2], lty = 2, col = b)
	}
	abline(h = 0.20, lty = 2, col = 'grey')
	abline(h = 0.40, lty = 2, col = 'grey')
	abline(h = 0.25, lty = 2, col = 'grey')
	abline(h = 0.30, lty = 2, col = 'grey')
}

par(mfrow = c(3, 2))
for (a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$aav))
}

par(mfrow = c(3, 2))
for (a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$catch.sum), ylim = c(0, 7000))
	abline(h = median(hcr.out[[a]]$catch.sum[1,]), lty = 2)
}

par(mfrow = c(5, 4))
for(a in 1:length(steep.vec)){
	for (b in 1:length(hcr.vec)){
		hist(ss.out[[a]]$depl.est[b,175,17,], xlim = c(0,1), ylim=c(0,80))
	}
}
