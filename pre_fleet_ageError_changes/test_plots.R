wd = paste("C:/Flatfish_MSC/Steep_85/hcr_option_hcr_40_10_ramp_constant_sims_1_500/save/",sep="") 
par(mfrow =c (4,3), oma =c(1,1,1,1), mar = c(2,4,2,3))

setwd(wd)
sims = 200
start.year = 1
ss.years = 170
first.ass = 90

ass.length = 17
target = 0.25 
thres  = 0.125
ass.index = c(1,3,5,7)
ages = 41
end.year = ss.years + 1

ssb = matrix(0, end.year, sims)
ssb.est = array (0, dim = c(ss.years, ass.length, sims))
depl = matrix(0, end.year, sims)
depl.est = array (0, dim = c(ss.years, ass.length, sims))
for (i in 1:sims){
 	est = paste("ss_ests_",i,sep="")
	dat = paste("om_proj_",i,sep ="")
	load(est); load (dat)
	ssb[,i] = Proj$SSB[start.year:end.year]
	ssb.est[,,i] = Est$SB[1:ss.years ,]
	depl[,i] = Proj$depl[start.year:end.year]
	depl.est[,,i] = Est$Bratio[1:ss.years,] #Est$Bratio[1:ss.years,]
}


depl.est.quant = apply(depl.est[1:ss.years,17,], 1, quantile, c(0.10, 0.50, 0.90))
ssb.est.quant = apply(ssb.est[1:ss.years,17,], 1, quantile, c(0.10, 0.50, 0.90))
depl.quant = apply(depl[1:ss.years,], 1, quantile, c(0.10, 0.50, 0.90))
ssb.quant = apply(ssb[1:ss.years,], 1, quantile, c(0.10, 0.50, 0.90))

med.depl = apply(depl, 1, median)
med.ssb = apply(ssb, 1, median)
med.depl.est = matrix(0, ss.years, ass.length)
med.ssb.est = matrix(0, ss.years, ass.length)

for (a in 1:ass.length){
	med.depl.est[,a] = apply(depl.est[,a,], 1, median)
	med.ssb.est[,a] = apply(ssb.est[,a,], 1, median)
}

re.depl = matrix(0, sims, ass.length)
re.ssb  = matrix(0, sims, ass.length)
re.sb0  = matrix(0, sims, ass.length)
re.sb1  = matrix(0, sims, ass.length)
for (a in 1:ass.length){
	ind = first.ass + 4*a - 4
	re.depl[,a] = (depl.est[ind,a,] - depl[ind,])/depl[ind,]
	re.ssb [,a] = (ssb.est[ind,a,] - ssb[ind,]) / ssb[ind,]
	re.sb0[,a]  = (ssb.est[1,a,] - ssb[1,]) / ssb[1,]
	re.sb1[,a]  = (ssb.est[ages,a,] - ssb[ages,])/ ssb[ages,]
}

par(mfrow=c(2,2))
boxplot(re.depl, ylim = c(-0.5, 0.5), ylab = "re depl");abline(h = 0)
boxplot(re.ssb,  ylim = c(-0.5, 0.5), ylab = "re ssb"); abline(h = 0)
boxplot(re.sb0,  ylim = c(-0.5, 0.5), ylab = "re sb0"); abline(h = 0)
boxplot(re.sb1,  ylim = c(-0.5, 0.5), ylab = "re sb1"); abline(h = 0)

par(mfrow =c (2,1))
plot(1:ss.years, med.depl[1:ss.years], ylim =c(0,1.1), type ='l', lwd =2, ylab = "Depletion")
abline (h =target) ; abline ( h =thres)
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.depl.est[ind,a], lty = 2, col =2)
}
y.max = 4000
plot(1:ss.years, med.ssb[1:ss.years], type ='l', lwd =2, ylim = c(0, y.max), ylab = "SSB")
for (a in 1:ass.length){
	ind = 1:(first.ass + a*4 -4)
	if (a == ass.length) { ind = 1:(first.ass+a*4-5) }
	lines(ind, med.ssb.est[ind,a], lty = 2, col = a)
}

plot(1:ss.years,  depl.quant[2,], type = 'l', ylim = c(0, 1.2), col = 2, lwd = 2)
lines(1:ss.years, depl.quant[1,], lty = 1, col = 2)
lines(1:ss.years, depl.quant[3,], lty = 1, col = 2)
lines(1:ss.years, depl.est.quant[2,], lty = 2, lwd = 2, col = 1)
lines(1:ss.years, depl.est.quant[1,], lty = 2, col = 1)
lines(1:ss.years, depl.est.quant[3,], lty = 2, col = 1)
abline(h = 0.25); abline(h = 0.125); abline(v = 90)
