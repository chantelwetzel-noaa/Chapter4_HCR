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

steep.vec <- c("Steep_85", "Steep_75", "Steep_95", "Steep_85_75", "Steep_85_95", "Steep_85_auto")
hcr.vec   <- c( "hcr_20_5_ramp_constant",
				"hcr_25_5_ramp_constant", 
				"hcr_30_10_ramp_constant", 
			    "hcr_40_10_ramp_constant")

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



print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

print.numeric<-function(x, digits) { formatC(x, digits = digits, format = "f") }

target = c(0.20, 0.25, 0.30, 0.40)
alpha.label = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)','(g)')
labels = c(expression(italic("h")[om]~" = 0.85"), 
		   expression(italic("h")[om]~" = 0.75"),
		   expression(italic("h")[om]~" = 0.95"),
		   expression(italic("h")[om]~" = 0.85;"~italic("h"[est])~" = 0.75"),
		   expression(italic("h")[om]~" = 0.85;"~italic("h"[est])~" = 0.95"),
		   expression(italic("h")[om]~" = 0.85 (auto-correltation in recruits)"))

hcr.lab = c("20-5", "25-5", "30-10", "40-10")
x = 151:175 # Summary years for the operating model 

#=====================================================================================================================
# Calculate the percentage below the target stock size
#=====================================================================================================================
above.target = matrix(NA, length(steep.vec), length(hcr.vec))
med.dist     = matrix(NA, length(steep.vec), length(hcr.vec))
below.msst.all = matrix(NA, length(steep.vec), length(hcr.vec))
for (a in 1:length(steep.vec)){
	for(b in 1:length(hcr.vec)){
		above.target[a,b] = sum(om.out[[a]]$depl[b,x,] > target[b]) / length(om.out[[a]]$depl[b,x,])
		med.dist[a,b]     = median(om.out[[a]]$depl[b,x,])
		below.msst.all[a,b]   = sum(om.out[[a]]$depl[b,x,] < target[b] * 0.50) / length(om.out[[a]]$depl[b,x,])
	}
}

#=====================================================================================================================
# Relative Stock Size Distributions
#=====================================================================================================================
setwd("C:/Flatfish_MSC/WriteUp/Plots")
png(filename = "Depeltion_Dist.png", width = 6.7, height = 7.5, units = 'in', res = 256)
par(mfrow = c(2, 3), mar = c(0.5,0.5,0.5,0.5), oma = c(4,4,4,5))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 0.9
dep.xvec = c(0.2, 0.35, 0.40, 0.50); dep.yvec = c(0.22, 0.47, 0.72, 0.97)
sim = 17; ymax = 10 * length(hcr.vec) 
for (a in 1:length(steep.vec)) {
	offset = 0
	out = density(om.out[[a]]$depl[1,x,], bw = 0.05)
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	plot("","",xlim = c(0, 0.70), ylim = c(0, ymax), main = '', xlab = "", ylab = '', axes = F, yaxs="i", xaxs = 'i')
	#axis(side = 1) #; axis(side =2, at = seq(0,40, 20), label = c("", "", ""))
	polygon(xx, yy, col = "grey", lty = 0)
	#lines(c(med.dist[a,1], med.dist[a,1]), c(0, max(yy)+ 2), lty = 1, col = 1) # Median Depletion 
	lines(c(target[1], target[1]), c(0, max(yy)+ 2), lty = 2) # Target line
	#print.letter(xy = c(dep.xvec[1], dep.yvec[1]), label = round(med.dist[a,1],2), cex = letter.cex)
	print.letter(xy = c(0.76, dep.yvec[1] - 0.05), label = paste("median =", print(med.dist[a,1],2)), cex = letter.cex)
	print.letter(xy = c(0.69, dep.yvec[1]), label = paste0("above target = ", print(100*above.target[a,1],0), "%"), cex = letter.cex)
	box()
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
	for (b in 2:length(hcr.vec)){
		offset = offset + 10 
		out = density(om.out[[a]]$depl[b,x,], bw = 0.05)
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx,yy, col = 'grey', lty = 0)
		abline(h = offset, lty = 1)		
		#lines(c(med.dist[a,b], med.dist[a,b]), c(offset, max(yy)+ 2), lty = 1, col = 1) # Median Depletion 
		lines(c(target[b], target[b]), c(offset , max(yy) + 2), lty = 2)
		print.letter(xy = c(0.76, dep.yvec[b] - 0.05), label = paste("median =",print(med.dist[a,b],2)), cex = letter.cex)
		print.letter(xy = c(0.69, dep.yvec[b]), label = paste0("above target = ", print(100*above.target[a,b],0), "%"), cex = letter.cex)
	}
	if (a > 3) { axis(side = 1, at = seq(0, 0.60, 0.10), cex.axis = axis.cex)}
	if (a == 3 || a == 6){
		axis(side = 4, at = c(0.15*ymax, 0.38*ymax, 0.62*ymax, 0.85*ymax), label = c("20-5", "25-5", "30-10", "40-10"), las = 1, 
			cex.axis = axis.cex, tick = FALSE, padj = 0, hadj = 0.25 )
		#print.letter(xy = c(0.90, 0.20), label = "20-5", cex = letter.cex)
		#print.letter(xy = c(0.90, 0.45), label = "25-5", cex = letter.cex)
		#print.letter(xy = c(0.90, 0.70), label = "30-10", cex = letter.cex)
		#print.letter(xy = c(0.90, 0.95), label = "40-10", cex = letter.cex) 
	}
}
mtext("Frequency", side = 2, outer = T, line = 1, cex = label.cex)
mtext("Relative biomass", side = 1, outer = T, line = 2, cex = label.cex)
p = par('usr')
#mtext("Harvest control rule", side = 4, outer = T, xpd = NA, srt = 90, cex = label.cex)
#text(p[2], mean(p[3:4]), labels = "Harvest control rule", outer = T, xpd = NA, srt = -90, cex = label.cex)
mtext("Harvest control rule", side = 4, outer = T, line = 3.5, cex = label.cex, las = 3)
dev.off()

#=====================================================================================================================
# Trade-off plot
#=====================================================================================================================

# Calculate the median probability over the last 25 years
target10 = ave.catch = aav = matrix(NA, length(steep.vec), length(hcr.vec))
sum.yrs = 56:80
for (a in 1:length(steep.vec)){ 
	target10[a,]  = apply(hcr.out[[a]]$target.true[,sum.yrs], 1, median) 
	ave.catch[a,] = apply(hcr.out[[a]]$catch.ave, 1, median)
	aav[a,]       = apply(hcr.out[[a]]$aav, 1, median) }

png(filename = "Tradeoffs2.png", width = 6.7, height = 6.7/2, units = 'in', res = 256)
pch.vec = 21:24; pch.col = c(1, "grey", "white")
par(mfrow = c(1, 3), mar = c(3,3.5,3,2.75), oma = c(1,1,1,0))
letter.cex = 1.1; axis.cex = 1.1; label.cex = 0.9; pch.cex = 1.4
part2 = expression("SB"[PROXY] %+-% "10%")

# ave catch vs. 10% for correct steep range
plot(target10[1,], ave.catch[1,], axes = F, xlim = c(0,1), ylim = c(100, 200), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(target10[a,], ave.catch[a,], lty = 2)
	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, cex.axis = axis.cex)
	mtext(side = 2, "Average catch (median)", line = 3, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[1], cex = letter.cex)
# aav vs 10%  probability
plot(target10[1,], aav[1,], axes = F, xlim = c(0,1), ylim = c(0, 12), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[2], cex = letter.cex)

# aav vs 10%  probability
plot(ave.catch[1,], aav[1,], axes = F, xlim = c(100,200), ylim = c(0, 12), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(ave.catch[a,], aav[a,], lty = 2)
	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, "Average catch (median)", line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[3], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)

# ave catch vs. 10% for correct steep range
#plot(target10[1,], ave.catch[1,], axes = F, xlim = c(0,1), ylim = c(100, 200), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
#box()
#for(a in 4:6){
#	lines(target10[a,], ave.catch[a,], lty = 2)
#	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
#	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, cex.axis = axis.cex)
#	mtext(side = 2, "Average catch (median)", line = 3, cex = label.cex)
#	mtext(side = 1, part2, line = 2.5, cex = label.cex)
#	for(b in 2:length(hcr.vec)){
#		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
#	}
#}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[4], cex = letter.cex)
## aav vs 10%  probability
#plot(target10[1,], aav[1,], axes = F, xlim = c(0,1), ylim = c(0, 12), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
#box()
#for(a in 4:6){
#	lines(target10[a,], aav[a,], lty = 2)
#	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
#	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, cex.axis = axis.cex)
#	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
#	mtext(side = 1, part2, line = 2.5, cex = label.cex)
#	for(b in 2:length(hcr.vec)){
#		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
#	}
#}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[5], cex = letter.cex)
#
## aav vs 10%  probability
#plot(ave.catch[1,], aav[1,], axes = F, xlim = c(100,200), ylim = c(0, 12), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
#box()
#for(a in 4:6){
#	lines(ave.catch[a,], aav[a,], lty = 2)
#	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
#	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, cex.axis = axis.cex)
#	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
#	mtext(side = 1, "Average catch (median)", line = 2.5, cex = label.cex)
#	for(b in 2:length(hcr.vec)){
#		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
#	}
#}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[6], cex = letter.cex)
#legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)

dev.off()



#=====================================================================================================================
# Average Catch by HCR 
#=====================================================================================================================
png(filename = "AverageCatch.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow = c(2, 3), mar = c(0.5,0.5,0.5,0.5), oma = c(4,4,4,4))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$catch.ave), ylab = "", xlab = "", axes = F, ylim = c(0, 290), yaxs = 'i')
	box()
	if (a > 3) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = label.cex)}
	if (a == 1) { axis(side = 2, at = seq(0, 250, 50), cex.axis = label.cex, las = 1) }
	if (a == 4) { axis(side = 2, at = seq(0, 250, 50), cex.axis = label.cex, las = 1) }
	mtext(side = 2, "Average Catch", line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Harvest Control Rule", line = 2.5, outer = T, cex = label.cex)
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
dev.off()


#=====================================================================================================================
# Varinance in catch by HCR 
#=====================================================================================================================
png(filename = "CatchVariance.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow = c(2, 3), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$catch.var), ylab = "", xlab = "", axes = F, ylim = c(0, 90), yaxs = 'i')
	box()
	if ( a > 3 ) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = axis.cex)}
	if ( a == 1) { axis(side = 2, at = seq(0, 80, 20), cex.axis = axis.cex, las = 1) }
	if ( a == 4) { axis(side = 2, at = seq(0, 80, 20), cex.axis = axis.cex, las = 1) }
	mtext(side = 2, "Variance in catch (across simulations)", line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Harvest Control Rule", line = 2.5, outer = T, cex = label.cex)
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
dev.off()


#=====================================================================================================================
# AAV in catch by HCR 
#=====================================================================================================================
png(filename = "AAV.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow = c(2, 3), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$aav), ylab = "", xlab = "", axes = F, ylim = c(0, 25), yaxs = 'i')
	box()
	if ( a > 3 ) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = axis.cex)}
	if ( a == 1) { axis(side = 2, at = seq(0, 20, 5), cex.axis = axis.cex, las = 1) }
	if ( a == 4) { axis(side = 2, at = seq(0, 20, 5), cex.axis = axis.cex, las = 1) }
	mtext(side = 2, "AAV", line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Harvest Control Rule", line = 2.5, outer = T, cex = label.cex)
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
dev.off()


#=====================================================================================================================
# Trajectories over the management period
#=====================================================================================================================
png(filename = "DeplTrajectories.png", width = 6.7, height = 5, units = 'in', res = 256)
#library(RColorBrewer)
#a = 256; trans = 0.20
#trans.color = c(rgb(0, 153/a, 0, trans), rgb(255/a, 128/a, 0, trans+ 0.05), rgb(0,0,1, trans), rgb(0,0.5, 0.5, trans))
par(mfrow = c(3, 2), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
x = 80:175 ; ymax = 0.60 
letter.cex = 1.2; axis.cex = 1.1; label.cex = 1.1
for(a in 1:length(steep.vec)){
	plot(x, medians.out[[a]]$med.depl[1,x,2], type = 'l', ylim = c(0,ymax), col = 1, lwd = 2, yaxs = 'i', xaxs = 'i', axes = F)
	xx = c(x, rev(x)); yy = c(medians.out[[a]]$med.depl[1,x,3], rev(medians.out[[a]]$med.depl[1,x,1]))
	#polygon(xx, yy, lty = 0, col = trans.color[1])
	#lines(x, medians.out[[a]]$med.depl.est[1,x, 17, 2], lty = 2, col = 1)
	#lines(x, medians.out[[a]]$med.depl[1,x, 1], lty = 2, col = 1)
	#lines(x, medians.out[[a]]$med.depl[1,x, 3], lty = 2, col = 1)
	box()
	for (b in 2:length(hcr.vec)){
		lines(x, medians.out[[a]]$med.depl[b,x,2], lty = 1, lwd = 2, col = b)
		xx = c(x, rev(x)); yy = c(medians.out[[a]]$med.depl[b,x,3], rev(medians.out[[a]]$med.depl[b,x,1]))
		#polygon(xx, yy, lty = 0, col = trans.color[b])
		#lines(x, medians.out[[a]]$med.depl.est[b,x, 17, 2], lty = 2, col = b)
	}
	if (a == 1 || a == 3 || a == 5) { axis(side = 2, at = seq(0, 0.50, 0.10), cex.axis = axis.cex, las = 1)}
	if (a == 5 || a == 6) { axis(side = 1, cex.axis = axis.cex, las = 1)}
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
	abline(h = 0.20, lty = 2, col = 1)
	abline(h = 0.25, lty = 2, col = 2)
	abline(h = 0.30, lty = 2, col = 3)
	abline(h = 0.40, lty = 2, col = 4)
	mtext(side = 2, "Relative Biomass", line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Year", line = 2.5, outer = T, cex = label.cex)
}
dev.off()


#====================================================================================================================
# Table Values
#====================================================================================================================
output = paste(getwd(), "/Summary_Table", sep = "")
x = (80-25+1):80; N = dim(om.out[[1]]$ssb)[3]
quant.vec = hcr.out[[1]]$quant.vec

# Create tables
below.msst = matrix(NA, length(steep.vec), length(hcr.vec)*2)
target10   = matrix(NA, length(steep.vec), length(hcr.vec)*2)
target20   = matrix(NA, length(steep.vec), length(hcr.vec)*2)
catch.var  = matrix(NA, length(steep.vec), length(hcr.vec)*2)
colnames(below.msst) = colnames(target10) = colnames(target20) = colnames(catch.var) = 
						c("20_5","80qi", "25_5", "80qi", "30_10", "80qi", "40_10", "80qi")
rownames(below.msst) = rownames(target10) = rownames(target20) = rownames(catch.var) = steep.vec
med = c(1, 3, 5, 7)
qi  = c(2, 4, 6, 8)

for (a in 1:length(steep.vec)){
	temp = apply(hcr.out[[a]]$below.msst.true[,x] / N, 1, quantile, quant.vec)
	below.msst[a,med] =  temp[2,]
	below.msst[a,qi]  =  paste0( "(", round(temp[1,],2), " - ", round(temp[3,],2), ")")

	temp = apply(hcr.out[[a]]$target.true[,x],  1, quantile, quant.vec)
	target10[a,med] =  temp[2,]
	target10[a,qi]  =  paste0( "(", round(temp[1,],2), " - ", round(temp[3,],2), ")")

	temp = apply(hcr.out[[a]]$target.20.true[,x] , 1, quantile, quant.vec)
	target20[a,med] =  temp[2,]
	target20[a,qi]  =  paste0( "(", round(temp[1,],2), " - ", round(temp[3,],2), ")")

	#temp = apply(hcr.out[[a]]$catch.var[,x] , 1, quantile, quant.vec)
	#catch.var[a,med] =  temp[2,]
	#catch.var[a,qi]  =  paste0( "(", round(temp[1,],2), " - ", round(temp[3,],2), ")")
}	

out <- list()
out[[1]] = below.msst
out[[2]] = target10
out[[3]] = target20
names(out) = c("Below_MSST", "Target10", "Target20")
capture.output(out, file = output, append = F)