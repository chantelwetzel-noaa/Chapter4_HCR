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
drive = "E:/SyncBack"

steep.vec <- c("Steep_75","Steep_85", "Steep_95") #"Steep_85_75", "Steep_85_95",  "Steep_85_data_30",
				#"Steep_85_sigmaR_60", "Steep_85_auto", "Steep_85_auto_sigmaR_60")

hcr.vec   <- c( "hcr_20_5_ramp_constant",
				"hcr_25_5_ramp_constant", 
				"hcr_30_10_ramp_constant", 
			    "hcr_40_10_ramp_constant")

dir = paste0(drive, "/PhD/Chapter4/output")

om.out <- ss.out <- hcr.out <- med.out <- list()
for (a in 1:length(steep.vec)){
	load(paste0(dir,"/", steep.vec[a], "_om_all"))
	om.out[[a]] <- om.all

	load(paste0(dir,"/", steep.vec[a], "_ss_all"))
	ss.out[[a]] <- ss.all

	load(paste0(dir,"/", steep.vec[a], "_hcr_all"))
	hcr.out[[a]] <- hcr.all

	load(paste0(dir,"/", steep.vec[a], "_medians_all"))
	med.out[[a]] <- med.all
}


print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

print.numeric<-function(x, digits) { formatC(x, digits = digits, format = "f") }

target = c(0.20, 0.25, 0.30, 0.40)
#alpha.label = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)','(g)', '(h)', '(i)', '(j)')

labels2 =c(expression("steepness"["75"]), #expression(italic("h")['75']),
		   expression("steepness"["85"]),#expression(italic("h")['85']), 
		   expression("steepness"["95"]))#expression(italic("h")['95']),
		   #"recr. var.",
		   #"recr. auto.",
		   #"var. & auto")
		   #expression(italic("h")[LO]),
		   #expression(italic("h")[HI]),
		   #'RD',
		   #expression(sigma[R]),
		   #expression(rho[R]),
		   #expression(sigma[R]~' & '~rho[R]))

hcr.lab = c("20-5", "25-5", "30-10", "40-10")
x = 151:175 # Summary years for the operating model 
quant.vec = c(0.05, 0.50, 0.95)

green = rgb(93/255,151/255,50/255)
blue  = rgb(0/255,86/255,149/255)
shale = rgb(29/255,37/255,45/255)

#=====================================================================================================================
# Calculate the percentage below the target stock size
#=====================================================================================================================
above.target = matrix(NA, length(steep.vec), length(hcr.vec))
med.dist     = matrix(NA, length(steep.vec), length(hcr.vec))
below.msst.all = matrix(NA, length(steep.vec), length(hcr.vec))
si = matrix(NA, length(steep.vec), length(hcr.vec))
x = 151:175 # Summary years for the operating model 
for (a in 1:length(steep.vec)){
	for(b in 1:length(hcr.vec)){
		above.target[a,b] = sum(om.out[[a]]$depl[b,x,] > target[b]) / length(om.out[[a]]$depl[b,x,])
		med.dist[a,b]     = median(om.out[[a]]$depl[b,x,])
		below.msst.all[a,b]   = sum(om.out[[a]]$depl[b,x,] < target[b] * 0.50) / length(om.out[[a]]$depl[b,x,])
		temp = quantile(om.out[[a]]$depl[b,x,], quant.vec[c(1,3)])
		si[a,b] = paste(print(temp[1],2), "-", print(temp[2],2))
	}
}


#=====================================================================================================================
# Relative Stock Size Distributions
#=====================================================================================================================
setwd("C:/Users/chantell.wetzel/Documents/GitHub/Dissertation/Presentation")
#setwd(paste0(drive,"/PhD/Chapter4/Presentation/Plots/"))
png(filename = "Depletion_Dist_95SI_alt_3.png", width = 12, height = 8.5, units = 'in', res = 256)

par(mfrow = c(1, 3), mar = c(0.7,0.5,1.2,1), oma = c(4,4,4,5))
letter.cex = 1.8; axis.cex = 1.7; label.cex = 1.5 #0.8
dep.xvec = c(0.2, 0.35, 0.40, 0.50); dep.yvec = c(0.22, 0.47, 0.72, 0.97)
sim = 17; ymax = 10 * length(hcr.vec) 
lab.pos = c(rep(0.23,3), 0.17, 0.18, 0.23)
set.bw = 0.021 
main.lab = c("Steepness = 0.75", "Steepness = 0.85", "Steepness = 0.95")
adj.vec  = c(-3, 0, 3)
col.vec = c("deepskyblue", "dodgerblue3")
col.vec = c(shale,blue)

for (a in 1:length(steep.vec)) {
	offset = 0
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = 0.01, to = 1)
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	plot("","",xlim = c(0, 0.70), ylim = c(0, ymax), xlab = "", ylab = '', axes = F, yaxs="i", 
		xaxs = 'i', main = "")
	mtext(main.lab[a], side = 3, outer = F, line = 1, cex = label.cex)
	polygon(xx, yy, col = col.vec[1], lty = 0)		
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[1,x,], quant.vec[1]), 
					to = quantile(om.out[[a]]$depl[1,x,], quant.vec[3]))
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	polygon(xx, yy, col = col.vec[2], lty = 0)
	lines(c(target[1], target[1]), c(0, max(yy)+ 2), lty = 2, lwd = 2) # Target line
	print.letter(xy = c(0.80, dep.yvec[1] - 0.05), label = paste("med =", print(med.dist[a,1],2)), cex = letter.cex)
	print.letter(xy = c(0.77, dep.yvec[1]), label = paste0("> target = ", print(100*above.target[a,1],0), "%"), cex = letter.cex)
	box()
	#print.letter(xy = c(lab.pos[a], 0.95), label = labels2[a], cex = (letter.cex+0.1))
	#print.letter(xy = c(0.06, 0.95), label = alpha.label[a], cex = letter.cex)
	for (b in 2:length(hcr.vec)){
		offset = offset + 10 
		out = density(om.out[[a]]$depl[b,x,], bw = set.bw,from = 0.01, to = 1)
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx,yy, col = col.vec[1], lty = 0)
		out = density(om.out[[a]]$depl[b,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[b,x,], quant.vec[1]), 
				to = quantile(om.out[[a]]$depl[b,x,], quant.vec[3]))
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx, yy, col = col.vec[2], lty = 0)
		abline(h = offset, lty = 1)		
		lines(c(target[b], target[b]), c(offset , max(yy) + 2), lty = 2, lwd = 2)
		print.letter(xy = c(0.80, dep.yvec[b] - 0.05), label = paste("med =",print(med.dist[a,b],2)), cex = letter.cex)
		print.letter(xy = c(0.75, dep.yvec[b]), label = paste0("> target = ", print(100*above.target[a,b],0), "%"), cex = letter.cex)
	}
	axis(side = 1, at = seq(0, 0.60, 0.10), cex.axis = axis.cex)
	if (a == 3 || a == 6 ){
		axis(side = 4, at = c(0.15*ymax, 0.38*ymax, 0.62*ymax, 0.85*ymax), label = c("20-5", "25-5", "30-10", "40-10"), las = 1, 
			cex.axis = axis.cex, tick = FALSE, padj = 0, hadj = 0.25 )
	}
}
mtext("Frequency", side = 2, outer = T, line = 1, cex = label.cex)
mtext("Relative biomass", side = 1, outer = T, line = 2, cex = label.cex)
p = par('usr')
mtext("Harvest control rule", side = 4, outer = T, line = 4, cex = label.cex, las = 3)

dev.off()


# Single panel
png(filename = "Depletion_Dist_95SI_alt_1.png", width = 7, height = 8.5, units = 'in', res = 256)

par(mfrow = c(1, 1), mar = c(0.7,0.5,1.2,1), oma = c(4,4,4,5))
letter.cex = 1.8; axis.cex = 1.7; label.cex = 1.5 #0.8
dep.xvec = c(0.2, 0.35, 0.40, 0.50); dep.yvec = c(0.22, 0.47, 0.72, 0.97)
sim = 17; ymax = 10 * length(hcr.vec) 
lab.pos = c(rep(0.23,3), 0.17, 0.18, 0.23)
set.bw = 0.021 
main.lab = c("Steepness = 0.75", "Steepness = 0.85", "Steepness = 0.95")
adj.vec  = c(-3, 0, 3)
col.vec = c("deepskyblue", "dodgerblue3")
col.vec = c(shale,blue)

for (a in 1:1) {
	offset = 0
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = 0.01, to = 1)
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	plot("","",xlim = c(0, 0.70), ylim = c(0, ymax), xlab = "", ylab = '', axes = F, yaxs="i", 
		xaxs = 'i', main = "")
	mtext(main.lab[a], side = 3, outer = F, line = 1, cex = label.cex)
	polygon(xx, yy, col = col.vec[1], lty = 0)		
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[1,x,], quant.vec[1]), 
					to = quantile(om.out[[a]]$depl[1,x,], quant.vec[3]))
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	polygon(xx, yy, col = col.vec[2], lty = 0)
	lines(c(target[1], target[1]), c(0, max(yy)+ 2), lty = 2, lwd = 2) # Target line
	print.letter(xy = c(0.80, dep.yvec[1] - 0.05), label = paste("med =", print(med.dist[a,1],2)), cex = letter.cex)
	print.letter(xy = c(0.77, dep.yvec[1]), label = paste0("> target = ", print(100*above.target[a,1],0), "%"), cex = letter.cex)
	box()
	#print.letter(xy = c(lab.pos[a], 0.95), label = labels2[a], cex = (letter.cex+0.1))
	#print.letter(xy = c(0.06, 0.95), label = alpha.label[a], cex = letter.cex)
	for (b in 2:length(hcr.vec)){
		offset = offset + 10 
		out = density(om.out[[a]]$depl[b,x,], bw = set.bw,from = 0.01, to = 1)
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx,yy, col = col.vec[1], lty = 0)
		out = density(om.out[[a]]$depl[b,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[b,x,], quant.vec[1]), 
				to = quantile(om.out[[a]]$depl[b,x,], quant.vec[3]))
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx, yy, col = col.vec[2], lty = 0)
		abline(h = offset, lty = 1)		
		lines(c(target[b], target[b]), c(offset , max(yy) + 2), lty = 2, lwd = 2)
		print.letter(xy = c(0.80, dep.yvec[b] - 0.05), label = paste("med =",print(med.dist[a,b],2)), cex = letter.cex)
		print.letter(xy = c(0.75, dep.yvec[b]), label = paste0("> target = ", print(100*above.target[a,b],0), "%"), cex = letter.cex)
	}
	axis(side = 1, at = seq(0, 0.60, 0.10), cex.axis = axis.cex)
	axis(side = 4, at = c(0.15*ymax, 0.38*ymax, 0.62*ymax, 0.85*ymax), label = c("20-5", "25-5", "30-10", "40-10"), las = 1, 
			cex.axis = axis.cex, tick = FALSE, padj = 0, hadj = 0.25 )
}
mtext("Frequency", side = 2, outer = T, line = 1, cex = label.cex)
mtext("Relative biomass", side = 1, outer = T, line = 2, cex = label.cex)
p = par('usr')
mtext("Harvest control rule", side = 4, outer = T, line = 4, cex = label.cex, las = 3)

dev.off()

#=====================================================================================================================
# Trade-off plot
#=====================================================================================================================
msy.vec = c(160, 142, 182)
# Calculate the median probability over the last 25 years
target10 = ave.catch = aav = matrix(NA, length(steep.vec), length(hcr.vec))
sum.yrs = 56:80
for (a in 1:length(steep.vec)){ 
	target10[a,]  = apply(hcr.out[[a]]$target.true[,sum.yrs], 1, median) 
	ave.catch[a,] = apply(hcr.out[[a]]$catch.ave, 1, median)
	aav[a,]       = apply(hcr.out[[a]]$aav, 1, median) }

png(filename = "Tradeoffs_msy_3.png", width = 12, height = 8.5, units = 'in', res = 256)

pch.vec = 21:24
pch.col = c('red', 'green', 'blue') #pch.col = c(1,  "grey50", "white")
#pch.col = c(green, blue, shale)
par(mfrow = c(1, 3), mar = c(3,4,3,3), oma = c(2,2,1,0))
letter.cex = 1.6; axis.cex = 1.8; label.cex = 1.5; pch.cex = 3; main.cex = 1
part2 = expression("Biomass"[target] %+-% "10%")
max.prob = 0.50 ; max.catch = 190; min.catch = 120

#1 ave catch vs. 10% for correct steep range
plot(target10[1,], ave.catch[1,], axes = F, xlim = c(0,max.prob), ylim = c(min.catch,max.catch), yaxs = 'i', 
	xaxs = 'i', xlab = "", ylab = "")
abline(h = msy.vec[1], lty = 2, col = 'grey70')
points(0.03, msy.vec[1], pch = pch.vec[1], bg = pch.col[2], cex = pch.cex)
abline(h = msy.vec[2], lty = 2, col = 'grey70')
points(0.03, msy.vec[2], pch = pch.vec[1], bg = pch.col[1], cex = pch.cex)
abline(h = msy.vec[3], lty = 2, col = 'grey70')
points(0.03, msy.vec[3], pch = pch.vec[1], bg = pch.col[3], cex = pch.cex)
box()
for(a in 1:3){
	lines(target10[a,], ave.catch[a,], lty = 2)
	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(120, 180, 20), cex.axis = axis.cex)
	mtext(side = 2, "Average catch", line = 3.5, cex = label.cex)
	mtext(side = 1, part2, line = 3.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b],cex = pch.cex,  bg = pch.col[a])
	}
}

#2 aav vs 10%  probability
plot(target10[1,], aav[1,], axes = F, xlim = c(0,max.prob), ylim = c(0, 12), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(0, 15, 5), cex.axis = axis.cex)
	mtext(side = 2, "Average annual variation - catch", line = 3, cex = label.cex)
	mtext(side = 1, part2, line = 3.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex, bg = pch.col[a])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[2], cex = letter.cex)
#mtext(side = 3, comb.lab[1], line = 0, cex = main.cex)
legend('topright', legend = main.lab, pch = rep(16,3), col = pch.col, bty = 'n', cex = letter.cex + 0.5)
legend('topright', legend = main.lab, pch = rep(21,3), col = rep(1,3), bty = 'n', cex = letter.cex + 0.5)

#3 aav vs 10%  probability
plot(ave.catch[1,], aav[1,], axes = F, xlim = c(min.catch,max.catch), ylim = c(0, 12), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(ave.catch[a,], aav[a,], lty = 2)
	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, at = seq(120, 180, 20),cex.axis = axis.cex) 
	axis(side = 2, las = 1, at = c(0,5,10,15), cex.axis = axis.cex)
	mtext(side = 2, "Average annual variation - catch", line = 3, cex = label.cex)
	mtext(side = 1, "Average catch", line = 3, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex, bg = pch.col[a])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[3], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex + 0.5)


dev.off()

# =========================================================================================================
# Stock Recruitment
# =========================================================================================================

 set.seed(2)
 steep = 0.8
 SSB0 = 1000
 R0 = 100
 SSB = seq(0,SSB0,10)

 sigmaR = 0.10
 rho <- 0.99 #1 / sqrt(2) 
 recdevs  <- rnorm(100, 0, sigmaR)
 autocorr <- rep(0, 100)
 autocorr[1] <- recdevs[1]  
 for (e in 2:100) { 
    autocorr[e] <- rho*autocorr[e-1]+sqrt(1-rho*rho)*recdevs[e] 
 }
 Rauto = Rdev = numeric(0)
 
 Rdeterm <- (4 * steep * ( R0 ) * SSB) / (SSB0 * (1 - steep) + SSB * (5 * steep - 1))
 steep = 0.65
 Rmid    <- (4 * steep * ( R0 ) * SSB) / (SSB0 * (1 - steep) + SSB * (5 * steep - 1))
 steep = 0.50
 Rlow    <- (4 * steep * ( R0 ) * SSB) / (SSB0 * (1 - steep) + SSB * (5 * steep - 1))

 for (y in 1:99){
 	change = 100 - y
 	Rdev[change+1]  <- Rdeterm[change+1] * exp(-0.5 * (sigmaR^2)) * exp(recdevs[y+1]) 
 	#Rauto[change+1] <- R0 * exp(-0.5 * (sigmaR^2)) * exp(autocorr[y+1]) 
 }


 par(mfrow = c(1,1))
 plot(Rdeterm, type = 'l', col = 'blue', lwd = 3, ylim = c(0,120), axes = F)
 lines(Rmid, lty = 1, col = 2, lwd = 3)
 lines(Rlow, lty = 1, col = 3, lwd = 3)

 par(mfrow = c(1,1))
 plot(Rdeterm, type = 'l', col = 'blue', lwd = 3, ylim = c(0,120), axes = F)
 points(20:100,Rdev[20:100], pch = 16, col = 2)


 