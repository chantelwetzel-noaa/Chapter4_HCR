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

steep.vec <- c("Steep_85", "Steep_75", "Steep_95", "Steep_85_75", "Steep_85_95",  "Steep_85_data_30",
				"Steep_85_sigmaR_60", "Steep_85_auto", "Steep_85_selec_at_mat",
				"Steep_85_auto_sigmaR_60")

hcr.vec   <- c( "hcr_20_5_ramp_constant",
				"hcr_25_5_ramp_constant", 
				"hcr_30_10_ramp_constant") 
			    #"hcr_40_10_ramp_constant")

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
alpha.label = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)','(g)', '(h)', '(i)', '(j)')

labels2 =c(expression(italic("h")['COR 85']), 
		   expression(italic("h")['COR 75']),
		   expression(italic("h")['COR 95']),
		   expression(italic("h")[LO]),
		   expression(italic("h")[HI]),
		   'RD',
		   expression(sigma[R]),
		   expression(rho[R]),
		   expression(sigma[R]~' & '~rho[R]))

hcr.lab = c("20-5", "25-5", "30-10", "40-10")
x = 151:175 # Summary years for the operating model 
quant.vec = c(0.05, 0.50, 0.95)
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
# Distribution of management quantities for each steepness
#=====================================================================================================================
#setwd("C:/PhD/Chapter4/WriteUp/Plots")
png(filename = "Management_Dist.png", width = 6.7, height = 9, units = 'in', res = 256)

par(mfrow = c(3, 3), mar = c(0.7,0.5,1.2,0.5), oma = c(4,4,4,5))
letter.cex = 1; axis.cex = 1.2; label.cex = 0.8
ymax = 10
for (a in 1:3){

	out = density(om.out[[a]]$bmsy, bw = 0.05)#, bw = 0.05)
	xx = c(out$x, rev(out$x)); yy = c(out$y , rev(rep(0, length(out$y))))
	plot("","",xlim = c(0, 0.50), ylim = c(0, ymax), main = '', xlab = "", ylab = '', axes = F, yaxs="i", xaxs = 'i')
	polygon(xx, yy, col = "grey", lty = 0)
	box()
	if (a == 3) { axis(side = 1, at = seq(0, 0.50, 0.10)); mtext(side = 1, line = 3, expression(B[MSY]))}
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)


	out = density(om.out[[a]]$spr, bw = 0.05)
	xx = c(out$x, rev(out$x)); yy = c(out$y , rev(rep(0, length(out$y))))
	plot("","",xlim = c(0, 0.50), ylim = c(0, ymax), main = '', xlab = "", ylab = '', axes = F, yaxs="i", xaxs = 'i')
	polygon(xx, yy, col = "grey", lty = 0)
	box()
	if (a == 3) { axis(side = 1,  at = seq(0, 0.50, 0.10) ); mtext(side = 1, line = 3, expression(SPR["%"])) }
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)

	out = density(om.out[[a]]$msy, bw = 11)
	xx = c(out$x, rev(out$x)); yy = c(out$y , rev(rep(0, length(out$y))))
	plot("","",xlim = c(0, 300), ylim = c(0,0.015), main = '', xlab = "", ylab = '', axes = F, yaxs="i", xaxs = 'i')
	polygon(xx, yy, col = "grey", lty = 0)
	box()
	if (a == 3) { axis(side = 1, at = seq(50, 300, 50)); mtext(side = 1, line = 3, "MSY")}
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
#=====================================================================================================================
# Relative Stock Size Distributions
#=====================================================================================================================
setwd("C:/PhD/Chapter4/WriteUp/Plots")
png(filename = "Depletion_Dist_95SI.png", width = 6.7, height = 8.5, units = 'in', res = 256)
par(mfrow = c(3, 3), mar = c(0.7,0.5,1.2,0.5), oma = c(4,4,4,5))
letter.cex = 1; axis.cex = 1.1; label.cex = 0.8
dep.xvec = c(0.2, 0.35, 0.40, 0.50); dep.yvec = c(0.22, 0.47, 0.72, 0.97)
sim = 17; ymax = 10 * length(hcr.vec) 
lab.pos = c(rep(0.21,3), 0.16, 0.15, 0.14, 0.14, 0.14, 0.23)
set.bw = 0.021 

for (a in 1:length(steep.vec)) {
	offset = 0
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = 0.01, to = 1)
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	plot("","",xlim = c(0, 0.70), ylim = c(0, ymax), main = '', xlab = "", ylab = '', axes = F, yaxs="i", xaxs = 'i')
	polygon(xx, yy, col = "grey", lty = 0)		
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[1,x,], quant.vec[1]), 
					to = quantile(om.out[[a]]$depl[1,x,], quant.vec[3]))
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	polygon(xx, yy, col = "grey50", lty = 0)
	lines(c(target[1], target[1]), c(0, max(yy)+ 2), lty = 2) # Target line
	print.letter(xy = c(0.80, dep.yvec[1] - 0.05), label = paste("med =", print(med.dist[a,1],2)), cex = letter.cex)
	print.letter(xy = c(0.77, dep.yvec[1]), label = paste0("> target = ", print(100*above.target[a,1],0), "%"), cex = letter.cex)
	#print.letter(xy = c(0.77, dep.yvec[1] - 0.10), label = paste("si =", si[a,1]), cex = letter.cex)
	#print.letter(xy = c(0.80, dep.yvec[1] - 0.10), label = paste("med =", print(med.dist[a,1],2)), cex = letter.cex)
	#print.letter(xy = c(0.77, dep.yvec[1] ), label = paste0("> target = ", print(100*above.target[a,1],0), "%"), cex = letter.cex)
	#print.letter(xy = c(0.77, dep.yvec[1] - 0.05), label = paste("si =", si[a,1]), cex = letter.cex)
	box()
	print.letter(xy = c(lab.pos[a], 0.95), label = labels2[a], cex = letter.cex)
	print.letter(xy = c(0.06, 0.95), label = alpha.label[a], cex = letter.cex)
	for (b in 2:length(hcr.vec)){
		offset = offset + 10 
		out = density(om.out[[a]]$depl[b,x,], bw = set.bw,from = 0.01, to = 1)
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx,yy, col = 'grey', lty = 0)
		out = density(om.out[[a]]$depl[b,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[b,x,], quant.vec[1]), 
				to = quantile(om.out[[a]]$depl[b,x,], quant.vec[3]))
		xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
		polygon(xx, yy, col = "grey50", lty = 0)
		abline(h = offset, lty = 1)		
		lines(c(target[b], target[b]), c(offset , max(yy) + 2), lty = 2)
		#print.letter(xy = c(0.80, dep.yvec[b] - 0.10), label = paste("med =",print(med.dist[a,b],2)), cex = letter.cex)
		#print.letter(xy = c(0.77, dep.yvec[b] ), label = paste0("> target = ", print(100*above.target[a,b],0), "%"), cex = letter.cex)
		#print.letter(xy = c(0.77, dep.yvec[b] - 0.05), label = paste("si =", si[a,b]), cex = letter.cex)
		print.letter(xy = c(0.80, dep.yvec[b] - 0.05), label = paste("med =",print(med.dist[a,b],2)), cex = letter.cex)
		print.letter(xy = c(0.77, dep.yvec[b]), label = paste0("> target = ", print(100*above.target[a,b],0), "%"), cex = letter.cex)
		#print.letter(xy = c(0.77, dep.yvec[b] - 0.10), label = paste("si =", si[a,b]), cex = letter.cex)
	}
	if (a > 6) { axis(side = 1, at = seq(0, 0.60, 0.10), cex.axis = axis.cex)}
	if (a == 3 || a == 6 || a == 9){
		axis(side = 4, at = c(0.15*ymax, 0.38*ymax, 0.62*ymax, 0.85*ymax), label = c("20-5", "25-5", "30-10", "40-10"), las = 1, 
			cex.axis = axis.cex, tick = FALSE, padj = 0, hadj = 0.25 )
	}
}
mtext("Frequency", side = 2, outer = T, line = 1, cex = label.cex)
mtext("Relative biomass", side = 1, outer = T, line = 2, cex = label.cex)
p = par('usr')
mtext("Harvest control rule", side = 4, outer = T, line = 3.5, cex = label.cex, las = 3)

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

png(filename = "Tradeoffs_msy.png", width = 6.7, height = 7, units = 'in', res = 256)

pch.vec = 21:24; pch.col = c(1,  "grey50", "white")
par(mfrow = c(3, 3), mar = c(3,2.85,2.75,2.75), oma = c(1,1,1,0))
letter.cex = 1.1; axis.cex = 1; label.cex = 0.75; pch.cex = 1.4; main.cex = 0.8
part2 = expression(italic("SB")[PROXY] %+-% "10%")
max.prob = 0.50 ; max.catch = 190; min.catch = 120

#1 ave catch vs. 10% for correct steep range
plot(target10[1,], ave.catch[1,], axes = F, xlim = c(0,max.prob), ylim = c(min.catch,max.catch), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
abline(h = msy.vec[1], lty = 2, col = 'grey70')
points(0.03, msy.vec[1], pch = pch.vec[1], bg = pch.col[1])
abline(h = msy.vec[2], lty = 2, col = 'grey70')
points(0.03, msy.vec[2], pch = pch.vec[1], bg = pch.col[2])
abline(h = msy.vec[3], lty = 2, col = 'grey70')
points(0.03, msy.vec[3], pch = pch.vec[1], bg = pch.col[3])
box()
for(a in 1:3){
	lines(target10[a,], ave.catch[a,], lty = 2)
	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(120, 180, 20), cex.axis = axis.cex)
	mtext(side = 2, "Average catch (median)", line = 3, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[1], cex = letter.cex)


#2 aav vs 10%  probability
plot(target10[1,], aav[1,], axes = F, xlim = c(0,max.prob), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(0, 15, 5), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[2], cex = letter.cex)
#mtext(side = 3, comb.lab[1], line = 0, cex = main.cex)
legend('topright', legend = labels2[1:3], pch = rep(16,3), col = pch.col, bty = 'n', cex = letter.cex + 0.10)
legend('topright', legend = labels2[1:3], pch = rep(21,3), col = rep(1,3), bty = 'n', cex = letter.cex + 0.10)

#3 aav vs 10%  probability
plot(ave.catch[1,], aav[1,], axes = F, xlim = c(min.catch,max.catch), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(ave.catch[a,], aav[a,], lty = 2)
	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, at = seq(120, 180, 20),cex.axis = axis.cex) 
	axis(side = 2, las = 1, at = c(0,5,10,15), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, "Average catch (median)", line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[3], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)

#4 ave catch vs. 10% for correct steep range
plot(target10[4,], ave.catch[4,], axes = F, xlim = c(0,max.prob), ylim = c(min.catch,max.catch), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
abline(h = msy.vec[1], lty = 2, col = 'grey70')
points(0.03, msy.vec[1], pch = pch.vec[1], bg = pch.col[1])
points(0.05, msy.vec[1], pch = pch.vec[1], bg = pch.col[2])
points(0.07, msy.vec[1], pch = pch.vec[1], bg = pch.col[3])
box()
for(a in 4:6){
	lines(target10[a,], ave.catch[a,], lty = 2)
	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(120, 180, 20), cex.axis = axis.cex)
	mtext(side = 2, "Average catch (median)", line = 3, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[4], cex = letter.cex)


#5 aav vs 10%  probability
plot(target10[4,], aav[4,], axes = F, xlim = c(0,max.prob), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 4:6){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, at = c(0,5,10,15), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[5], cex = letter.cex)
#mtext(side = 3, comb.lab[2], line = 0, cex = main.cex)
legend('topright', legend = labels2[4:6], pch = rep(16,3), col = pch.col, bty = 'n', cex = letter.cex + 0.10)
legend('topright', legend = labels2[4:6], pch = rep(21,3), col = rep(1,3), bty = 'n', cex = letter.cex + 0.10)

#6 aav vs 10%  probability
plot(ave.catch[4,], aav[4,], axes = F, xlim = c(min.catch,max.catch), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 4:6){
	lines(ave.catch[a,], aav[a,], lty = 2)
	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
	axis(side = 1, at=seq(120, 180, 20), cex.axis = axis.cex)
	axis(side = 2, las = 1,  at = c(0,5,10,15), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, "Average catch (median)", line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[6], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)


#7 ave catch vs. 10% for correct steep range
plot(target10[7,], ave.catch[7,], axes = F, xlim = c(0,max.prob), ylim = c(min.catch,max.catch), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
abline(h = msy.vec[1], lty = 2, col = 'grey70')
points(0.03, msy.vec[1], pch = pch.vec[1], bg = pch.col[1])
points(0.05, msy.vec[1], pch = pch.vec[1], bg = pch.col[2])
points(0.07, msy.vec[1], pch = pch.vec[1], bg = pch.col[3])
box()
for(a in 7:9){
	lines(target10[a,], ave.catch[a,], lty = 2)
	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-6])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, at=seq(120, 180, 20), las = 1, cex.axis = axis.cex)
	mtext(side = 2, "Average catch (median)", line = 3, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-6])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[7], cex = letter.cex)


#8 aav vs 10%  probability
plot(target10[7,], aav[7,], axes = F, xlim = c(0,max.prob), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 7:9){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-6])
	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, at = c(0,5,10,15), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, part2, line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-6])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[8], cex = letter.cex)
legend('topright', legend = labels2[7:9], pch = rep(16,3), col = pch.col, bty = 'n', cex = letter.cex + 0.10)
legend('topright', legend = labels2[7:9], pch = rep(21,3), col = rep(1,3), bty = 'n', cex = letter.cex + 0.10)
#mtext(side = 3, comb.lab[3], line = 0, cex = main.cex)

#9 aav vs 10%  probability
plot(ave.catch[7,], aav[7,], axes = F, xlim = c(min.catch,max.catch), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 7:9){
	lines(ave.catch[a,], aav[a,], lty = 2)
	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-6])
	axis(side = 1, at = seq(120, 180, 20), cex.axis = axis.cex) 
	axis(side = 2, las = 1, at = c(0,5,10,15),cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = 2.5, cex = label.cex)
	mtext(side = 1, "Average catch (median)", line = 2.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-6])
	}
}
print.letter(xy = c(0.08, 0.95), label = alpha.label[9], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)

dev.off()

#=====================================================================================================================
# RE for SB0
#=====================================================================================================================
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5), oma = c(4,4,4,4))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1


for(a in 1:length(steep.vec)){
	re.sb0 = matrix(0, length(hcr.vec), 200)
	for(b in 1:length(hcr.vec)){
		re.sb0[b,] = (ss.out[[a]]$ssb.est[b,1,15,] - om.out[[a]]$ssb[b,1,])/om.out[[a]]$ssb[b,1,]
	}

	boxplot(t(re.sb0), ylim = c(-0.5,0.5))
	abline (h = 0)
	mtext(side = 2, outer =T, "RE SB0", line = 2)
}

ass = 17; final = 175
for(a in 1:length(steep.vec)){
	re.sb0 = matrix(0, length(hcr.vec), 200)
	for(b in 1:length(hcr.vec)){
		re.sb0[b,] = (ss.out[[a]]$ssb.est[b,final,ass,]/ss.out[[a]]$ssb.est[b,1,ass,] - 
					      om.out[[a]]$ssb[b,final,]/ om.out[[a]]$ssb[b,1,]) / 
				         (om.out[[a]]$ssb[b,final,] / om.out[[a]]$ssb[b,1,])
	}

	boxplot(t(re.sb0), ylim = c(-0.5,0.5))
	abline (h = 0)
	mtext(side = 2, outer =T, "RE depl", line = 2)
}

for(a in 1:length(steep.vec)){
	boxplot(t(med.out[[a]]$re.ssb[,25,]), ylab = "", xlab = "", axes = F, ylim = c(-0.25, 0.25), yaxs = 'i')
	box(); axis(side = 2); abline (h = 0)
	mtext( side = 2, "RE SSB final", line = 2.5, outer = T, cex = label.cex)
	if (a > 6){axis(side = 1, at = 1:4, label = c('20-5', '25-5', '30-10', '40-10'))}
}

for(a in 1:length(steep.vec)){
	boxplot(t(med.out[[a]]$re.depl[,25,]), ylab = "", xlab = "", axes = F, ylim = c(-0.25, 0.2), yaxs = 'i')
	box(); axis(side = 2);abline (h = 0)
	mtext( side = 2, expression("RE "~italic(SB[y])~"/"~italic(SB[0])), line = 2.5, outer = T, cex = label.cex)
	if (a > 6){axis(side = 1, at = 1:4, label = c('20-5', '25-5', '30-10', '40-10'))}
}

for(a in 1:length(steep.vec)){
	boxplot(t(med.out[[a]]$re.depl[,1,]), ylab = "", xlab = "", axes = F, ylim = c(-0.25, 0.2), yaxs = 'i')
	box(); axis(side = 2);abline (h = 0)
	mtext( side = 2, expression("RE "~italic(SB[y-25])~"/"~italic(SB[0])), line = 2.5, outer = T, cex = label.cex)
	if (a > 6){axis(side = 1, at = 1:4, label = c('20-5', '25-5', '30-10', '40-10'))}
}


for(a in 1:length(steep.vec)){
	
	boxplot(t(med.out[[a]]$re.m[,1,17,]), ylab = "", xlab = "", axes = F, ylim = c(-0.25, 0.25), yaxs = 'i')
	box(); axis(side = 2); abline (h = 0)
	mtext( side = 2, "RE M females" , line = 2.5, outer = T, cex = label.cex)
	if (a > 6){axis(side = 1, at = 1:4, label = c('20-5', '25-5', '30-10', '40-10'))}		
}

for(a in 1:length(steep.vec)){
	
	boxplot(t(med.out[[a]]$re.m[,2,17,]), ylab = "", xlab = "", axes = F, ylim = c(-0.25, 0.25), yaxs = 'i')
	box(); axis(side = 2); abline (h = 0)
	mtext( side = 2, "RE M males" , line = 2.5, outer = T, cex = label.cex)
	if (a > 6){axis(side = 1, at = 1:4, label = c('20-5', '25-5', '30-10', '40-10'))}	
}




#=====================================================================================================================
# Average Catch by HCR 
#=====================================================================================================================
png(filename = "AverageCatch.png", width = 6.7, height = 6.7, units = 'in', res = 256)
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5), oma = c(4,4,4,4))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$catch.ave), ylab = "", xlab = "", axes = F, ylim = c(0, 290), yaxs = 'i')
	box()
	if (a > 6 ) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = label.cex)}
	if (a == 1) { axis(side = 2, at = seq(0, 250, 50), cex.axis = label.cex, las = 1) }
	if (a == 4) { axis(side = 2, at = seq(0, 250, 50), cex.axis = label.cex, las = 1) }
	if (a == 7) { axis(side = 2, at = seq(0, 250, 50), cex.axis = label.cex, las = 1) }
	mtext(side = 2, "Average Catch", line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Harvest Control Rule", line = 2.5, outer = T, cex = label.cex)
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
dev.off()

#=====================================================================================================================
# Parameter estimates
#=====================================================================================================================
png(filename = "RE_Depl.png", width = 6.7, height = 6.7, units = 'in', res = 256)
par(mfrow = c(3, 3), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	#med = NULL; med.list <- list()
	#for (b in 1:length(hcr.vec)){
	#	for (y in 1:25){
	#		get.vals = med.out[[a]]$re.depl[b,y,]
	#		med = c(med, get.vals)
	#	}
	#	med.list[[b]] = med
	#}
	#boxplot(med.list, ylab = "", xlab = "", axes = F, ylim = c(-0.6, 0.6), yaxs = 'i')
	boxplot(t(med.out[[a]]$re.depl[,25,]), ylab = "", xlab = "", axes = F, ylim = c(-0.6, 0.6), yaxs = 'i')
	abline (h = 0)
	box()
	if ( a > 6 ) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = axis.cex)}
	if ( a == 1) { axis(side = 2, at = seq(-0.4, 0.4, 0.20), cex.axis = axis.cex, las = 1) }
	if ( a == 4) { axis(side = 2, at = seq(-0.4, 0.4, 0.20), cex.axis = axis.cex, las = 1) }
	if ( a == 7) { axis(side = 2, at = seq(-0.4, 0.4, 0.20), cex.axis = axis.cex, las = 1) }
	mtext(side = 2, expression("Relative error "~ italic(B)[final]~"/"~italic(B)[0]), line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Harvest Control Rule", line = 2.5, outer = T, cex = label.cex)
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
dev.off()




#=====================================================================================================================
# Varinance in catch by HCR 
#=====================================================================================================================
png(filename = "CatchVariance.png", width = 6.7, height = 6.7, units = 'in', res = 256)
par(mfrow = c(3, 3), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$catch.var), ylab = "", xlab = "", axes = F, ylim = c(0, 120), yaxs = 'i')
	box()
	if ( a > 6 ) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = axis.cex)}
	if ( a == 1) { axis(side = 2, at = seq(0, 120, 20), cex.axis = axis.cex, las = 1) }
	if ( a == 4) { axis(side = 2, at = seq(0, 120, 20), cex.axis = axis.cex, las = 1) }
	if ( a == 7) { axis(side = 2, at = seq(0, 120, 20), cex.axis = axis.cex, las = 1) }
	mtext(side = 2, "Variance in catch (across simulations)", line = 2.5, outer = T, cex = label.cex)
	mtext(side = 1, "Harvest Control Rule", line = 2.5, outer = T, cex = label.cex)
	print.letter(xy = c(0.05, 0.95), label = alpha.label[a], cex = letter.cex)
}
dev.off()


#=====================================================================================================================
# AAV in catch by HCR 
#=====================================================================================================================
png(filename = "AAV.png", width = 6.7, height = 5, units = 'in', res = 256)
par(mfrow = c(3, 3), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
letter.cex = 1.1; axis.cex = 1.2; label.cex = 1
for(a in 1:length(steep.vec)){
	boxplot(t(hcr.out[[a]]$aav), ylab = "", xlab = "", axes = F, ylim = c(0, 30), yaxs = 'i')
	box()
	if ( a > 6 ) { axis(side = 1, at = 1:4, label = hcr.lab, cex.axis = axis.cex)}
	if ( a == 1) { axis(side = 2, at = seq(0, 30, 5), cex.axis = axis.cex, las = 1) }
	if ( a == 4) { axis(side = 2, at = seq(0, 30, 5), cex.axis = axis.cex, las = 1) }
	if ( a == 7) { axis(side = 2, at = seq(0, 30, 5), cex.axis = axis.cex, las = 1) }
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
par(mfrow = c(3, 3), mar = c(0.25,0.25,0.25,0.25), oma = c(4,4,1,1))
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
	if (a == 1 || a == 3 || a == 5 || a == 7) { axis(side = 2, at = seq(0, 0.50, 0.10), cex.axis = axis.cex, las = 1)}
	if (a == 7 || a == 8 || a == 9) { axis(side = 1, cex.axis = axis.cex, las = 1)}
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
output = paste(getwd(), "/Summary_Table_95SI", sep = "")
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

#=========================================================================================================
output = paste(getwd(), "/OM_ManagQuants", sep = "")
bmsy = msy = spr = matrix(NA, 3, 2)
rownames(bmsy) = rownames(msy) = rownames(spr) = c("steep_85", "steep_75", "steep_95")
colnames(bmsy) = colnames(msy) = colnames(spr) = c("median", "90si")
alt.quants = c(0.05, 0.50, 0.95)

for (a in 1: 3){
	temp = quantile(om.out[[a]]$bmsy, alt.quants)
	bmsy[a, 1] = temp[2]
	bmsy[a, 2] = paste0( "(", round(temp[1],2), " - ", round(temp[3],2), ")")

	temp = quantile(om.out[[a]]$spr, alt.quants)
	spr[a, 1] = temp[2]
	spr[a, 2] = paste0( "(", round(temp[1],2), " - ", round(temp[3],2), ")")

	temp = quantile(om.out[[a]]$msy, alt.quants)
	msy[a, 1] = temp[2]
	msy[a, 2] = paste0( "(", round(temp[1],0), " - ", round(temp[3],0), ")")
}

out <- list()
out$bmsy = bmsy
out$spr = spr
out$msy = msy
capture.output(out, file = output, append = F)