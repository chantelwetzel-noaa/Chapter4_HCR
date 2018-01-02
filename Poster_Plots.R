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

steep.vec <- c("Steep_75","Steep_85", "Steep_95", #) #"Steep_85_75", "Steep_85_95",  "Steep_85_data_30",
				"Steep_85_sigmaR_60", "Steep_85_auto", "Steep_85_auto_sigmaR_60")

hcr.vec   <- c( "hcr_20_5_ramp_constant",
				"hcr_25_5_ramp_constant", 
				"hcr_30_10_ramp_constant", 
			    "hcr_40_10_ramp_constant")

dir = paste0(drive, "/PhD/Chapter4/output_Jrn_Sub")

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


labels2 =c(expression(paste(italic("h"),'=0.75')), 
		   expression(paste(italic("h"),'=0.85')),
		   expression(paste(italic("h"),'=0.95')),
		   expression(sigma[R]),
		   expression(rho[R]),
		   expression(sigma[R]~' & '~rho[R]))

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
setwd("C:/PhD/Chapter4/Poster")

png(filename = "Depletion_Dist_95SI_alt_6.png", width = 12, height = 8.5, units = 'in', res = 256)

par(mfrow = c(2, 3), mar = c(1.5,1.5,1.5,1.5), oma = c(4,5,4,6))
letter.cex = 1.8; axis.cex = 1.7; label.cex = 1.5 #0.8
dep.xvec = c(0.2, 0.35, 0.40, 0.50); dep.yvec = c(0.22, 0.47, 0.72, 0.97)
sim = 17; ymax = 10 * length(hcr.vec) 
lab.pos = c(rep(0.23,3), 0.17, 0.18, 0.23)
set.bw = 0.021 
main.lab = labels2
adj.vec  = rep(c(-3, 0, 3),2)
col.vec = c("deepskyblue", "dodgerblue3")
col.vec = rep(c(shale,blue, green),2)
x.lab.vec = c(0.12, 0.12, 0.12, 0.09, 0.09, 0.14)

for (a in 1:length(steep.vec)) {
	offset = 0
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = 0.01, to = 1)
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	plot("","",xlim = c(0, 0.70), ylim = c(0, ymax), xlab = "", ylab = '', axes = F, yaxs="i", 
		xaxs = 'i', main = "")
	#mtext(main.lab[a], side = 3, outer = F, line = 1, cex = label.cex)
	polygon(xx, yy, col = col.vec[1], lty = 0)		
	out = density(om.out[[a]]$depl[1,x,], bw = set.bw, from = quantile(om.out[[a]]$depl[1,x,], quant.vec[1]), 
					to = quantile(om.out[[a]]$depl[1,x,], quant.vec[3]))
	xx = c(out$x, rev(out$x)); yy = c(out$y + offset, rev(rep(offset, length(out$y))))
	polygon(xx, yy, col = col.vec[2], lty = 0)
	lines(c(target[1], target[1]), c(0, max(yy)+ 2), col = col.vec[3], lty = 1, lwd = 3) # Target line
	print.letter(xy = c(0.80, dep.yvec[1] - 0.05), label = paste("med =", print(med.dist[a,1],2)), cex = letter.cex)
	print.letter(xy = c(0.77, dep.yvec[1]), label = paste0("> target = ", print(100*above.target[a,1],0), "%"), cex = letter.cex)
	box()
	#print.letter(xy = c(lab.pos[a], 0.95), label = labels2[a], cex = (letter.cex+0.1))
	print.letter(xy = c(x.lab.vec[a], 0.95), label = main.lab[a], cex = letter.cex)
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
		lines(c(target[b], target[b]), c(offset , max(yy) + 2), col = col.vec[3], lty = 1, lwd = 3)
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



#=====================================================================================================================
# Trade-off plot
#=====================================================================================================================
msy.vec = c(142, 160, 182)
# Calculate the median probability over the last 25 years
target10 = ave.catch = aav = matrix(NA, length(steep.vec), length(hcr.vec))
sum.yrs = 56:80
for (a in 1:length(steep.vec)){ 
	target10[a,]  = apply(hcr.out[[a]]$target.true[,sum.yrs], 1, median) 
	ave.catch[a,] = apply(hcr.out[[a]]$catch.ave, 1, median)
	aav[a,]       = apply(hcr.out[[a]]$aav, 1, median) }

png(filename = "Tradeoffs_msy_1.png", width = 12, height = 8.5, units = 'in', res = 256)
main.lab = labels2

pch.vec = 21:24
pch.col = c('green', 'red', 'blue') #pch.col = c(1,  "grey50", "white")
#pch.col = c(green, blue, shale)
par(mfrow = c(2, 3), mar = c(3,3,2,2), oma = c(2,3,2,2))
letter.cex = 1.6; axis.cex = 1.4; label.cex = 1.2; pch.cex = 2; main.cex = 1
part2 = expression(paste("P" %+-% "0.10 ", italic(B)[PROXY]))
max.prob = 0.50 ; max.catch = 190; min.catch = 120
x.lab.pos = 3.5; y.lab.pos = 3


#1 ave catch vs. 10% for correct steep range
plot(target10[1,], ave.catch[1,], axes = F, xlim = c(0,max.prob), ylim = c(min.catch,max.catch), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
abline(h = msy.vec[1], lty = 2, col = 'grey70')
points(0.03, msy.vec[1], pch = pch.vec[1], bg = pch.col[1], cex = pch.cex)
abline(h = msy.vec[2], lty = 2, col = 'grey70')
points(0.03, msy.vec[2], pch = pch.vec[1], bg = pch.col[2], cex = pch.cex)
abline(h = msy.vec[3], lty = 2, col = 'grey70')
points(0.03, msy.vec[3], pch = pch.vec[1], bg = pch.col[3], cex = pch.cex)
box()
for(a in 1:3){
	lines(target10[a,], ave.catch[a,], lty = 2)
	points(target10[a,1], ave.catch[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(120, 180, 20), cex.axis = axis.cex)
	mtext(side = 2, "Average catch (median)", line = y.lab.pos + 0.5, cex = label.cex)
	mtext(side = 1, part2, line = x.lab.pos , cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[1], cex = letter.cex)


#2 aav vs 10%  probability
plot(target10[1,], aav[1,], axes = F, xlim = c(0,max.prob), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 1:3){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a])
	axis(side = 1, cex.axis = axis.cex)
	axis(side = 2, las = 1, at = seq(0, 15, 5), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = y.lab.pos , cex = label.cex)
	mtext(side = 1, part2, line = x.lab.pos , cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[2], cex = letter.cex)
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
	mtext(side = 2, "AAV (median)", line = y.lab.pos , cex = label.cex)
	mtext(side = 1, "Average catch (median)", line = x.lab.pos-0.5, cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[3], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)

#7 ave catch vs. 10% for correct steep range
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
	axis(side = 2, at=seq(120, 180, 20), las = 1, cex.axis = axis.cex)
	mtext(side = 2, "Average catch (median)", line = y.lab.pos + 0.5, cex = label.cex)
	mtext(side = 1, part2, line = x.lab.pos , cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], ave.catch[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[4], cex = letter.cex)


#8 aav vs 10%  probability
plot(target10[4,], aav[4,], axes = F, xlim = c(0,max.prob), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 4:6){
	lines(target10[a,], aav[a,], lty = 2)
	points(target10[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
	axis(side = 1, cex.axis = axis.cex); axis(side = 2, las = 1, at = c(0,5,10,15), cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = y.lab.pos , cex = label.cex)
	mtext(side = 1, part2, line = x.lab.pos , cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(target10[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[8], cex = letter.cex)
legend('topright', legend = labels2[4:6], pch = rep(16,3), col = pch.col, bty = 'n', cex = letter.cex + 0.10)
legend('topright', legend = labels2[4:6], pch = rep(21,3), col = rep(1,3), bty = 'n', cex = letter.cex + 0.10)
#mtext(side = 3, comb.lab[3], line = 0, cex = main.cex)

#9 aav vs 10%  probability
plot(ave.catch[4,], aav[4,], axes = F, xlim = c(min.catch,max.catch), ylim = c(0, 15), yaxs = 'i', xaxs = 'i', xlab = "", ylab = "")
box()
for(a in 4:6){
	lines(ave.catch[a,], aav[a,], lty = 2)
	points(ave.catch[a,1], aav[a,1], pch = pch.vec[1], cex = pch.cex, bg = pch.col[a-3])
	axis(side = 1, at = seq(120, 180, 20), cex.axis = axis.cex) 
	axis(side = 2, las = 1, at = c(0,5,10,15),cex.axis = axis.cex)
	mtext(side = 2, "AAV (median)", line = y.lab.pos, cex = label.cex)
	mtext(side = 1, "Average catch (median)", line = x.lab.pos -0.5 , cex = label.cex)
	for(b in 2:length(hcr.vec)){
		points(ave.catch[a,b], aav[a,b], pch = pch.vec[b], cex = pch.cex,  bg = pch.col[a-3])
	}
}
#print.letter(xy = c(0.08, 0.95), label = alpha.label[9], cex = letter.cex)
legend('topright', pch = pch.vec, legend = c("20-5", "25-5", "30-10", "40-10"), bty = 'n', cex = letter.cex)

dev.off()


#=====================================================================================================================
# Alternative harvest control rules plot
#=====================================================================================================================

low = 0.05; high = 0.25
step = 0.05
bio<- seq(0,100,5)
depl<-bio/bio[length(bio)]	


yield <- function(low, high, f){
	
	z1 = length(seq(0.05, low, step))+1 ; z2 = length(seq(0.05, high, step))+1
	f.vec  <- rep(f,12)
	ofl    <- f*bio
	buffer <- 0.85 #Value used to make plotting better
	abc    <- ofl*buffer
	
	f.adj<-(f*(seq(low, high, step)-low)*high)/((high - low)*seq(low, high, step))
	#ofl[z1:z2] <- f.adj*bio[z1:z2]
	abc.hcr <- f.adj*bio[z1:z2]*buffer
	ofl.hcr <- f.adj*bio[z1:z2]

	out <-NULL
	out$ofl <- ofl
	out$abc <- abc
	out$abc.hcr <- abc.hcr
	out$ofl.hcr <- ofl.hcr
	out$z1  <- z1
	out$z2  <- z2
	return(out)
}


fspr<-expression(italic("F")[SPR])
bproxy <- expression(italic("B")[PROXY])
blimit <- expression(italic("B")[LIMIT]) 

bproxy <- c(expression(paste(italic("B")[PROXY], "  = 0.20")),
		   expression(paste(italic("B")[PROXY], "  = 0.25")),
		   expression(paste(italic("B")[PROXY], "  = 0.30")),
		   expression(paste(italic("B")[PROXY], "  = 0.40"))) 

blimit <- c(expression(paste(italic("B")[LIMIT], "  = 0.05")),
			expression(paste(italic("B")[LIMIT], "  = 0.05")),
			expression(paste(italic("B")[LIMIT], "  = 0.10")),
			expression(paste(italic("B")[LIMIT], "  = 0.10")))

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}


setwd("C:/PhD/Chapter4/Poster")
png(filename = "HCR.png", width = 6.7, height = 6, units = 'in', res = 256)
par(mfrow = c(2,2), mar = rep(0.50, 4), oma = c(3,3,3,3))
ofl.col = 1; abc.col = 'grey50' ; t.col = "grey63"; acl.col = 1

# 20-05 ==========================================================
low = 0.05; high = 0.20; f = 0.25
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i", lwd=2, cex.axis=1.2, col = ofl.col)
box()

lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)
# abc
lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)

# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)
#lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)
print.letter(xy = c(0.06, 0.23), blimit[1], srt = 90)
print.letter(xy = c(0.37, 0.45), bproxy[1], srt = 90)
print.letter(xy = c(0.90, 0.10), hcr.lab[1])
print.letter(xy = c(0.25, 0.80), expression(italic("F")["0.25"]))

mtext(side=1,"Relative biomass",line=2, cex=1, outer = T)
legend("topright", bty ='n', lty = c(1,2), col = c(ofl.col, acl.col), lwd = c(2,2), cex = 1,
	legend = c("Overfishing limit","Anual catch limit"))

# 25-5 ==========================================================
low = 0.05; high = 0.25; f=0.30
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col=ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)

# abc
lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)
#lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)

# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)

print.letter(xy = c(0.06, 0.24), blimit[2], srt = 90)
print.letter(xy = c(0.47, 0.57), bproxy[2], srt = 90)
print.letter(xy = c(0.90, 0.10), hcr.lab[2])
print.letter(xy = c(0.30, 0.80), expression(italic("F")["0.30"]))


# 30-10 ==========================================================
low = 0.10; high = 0.30; f = 0.35
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col = ofl.col)
box()

lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)

# abc
lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)
#lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)

# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)
print.letter(xy = c(0.17, 0.34), blimit[3], srt = 90)
print.letter(xy = c(0.57, 0.73), bproxy[3], srt = 90)
print.letter(xy = c(0.90, 0.10), hcr.lab[3])
print.letter(xy = c(0.39, 0.80), expression(italic("F")["0.35"]))


axis(side=1,at=seq(0,0.4,.1), labels=c("0.00", "0.10", "0.20", "0.30", "0.40"), cex.axis = 1)
mtext(side=2,"Catch",line=1, cex=1, outer = T)


# 40-10 ==========================================================
low = 0.10; high = 0.40; f = 0.40
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 21.5),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col = ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)

# abc
lines(rep(low, 2),    c(0, 21.5), lty = 3, col = t.col)
lines(rep(high, 2),   c(0, 21.5), lty = 3, col = t.col)
#lines(rep(high/2, 2), c(0, 21.5), lty = 3, col = t.col)

# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)
print.letter(xy = c(0.17, 0.37), blimit[4], srt = 90)
print.letter(xy = c(0.77, 0.35), bproxy[4], srt = 90)
print.letter(xy = c(0.90, 0.10), hcr.lab[4])
print.letter(xy = c(0.50, 0.80), expression(italic("F")["0.40"]))

axis(side=1,at=seq(0,0.40,.1), labels=c("0.00","0.10", "0.20", "0.30", "0.40"), cex.axis = 1)


dev.off()
