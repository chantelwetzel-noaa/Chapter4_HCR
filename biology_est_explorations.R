




sd.out = array(NA, dim = c(6, 17, 200)); sd.om = array(NA, dim = c(6, 200))
median.out = array(NA, dim = c(6, 17, 200)); median.om = array(NA, dim = c(6, 200))
mean.out = array(NA, dim = c(6, 17, 200))
par(mfrow = c(1,1))
plot("", "", xlim = c(0, 175), ylim = c(0, 4000))
for (c in 1:200){	
	for (a in 1: 6){
		for (b in 1:17){
			ind = 1:(95 + b*5 - 5 - 1)
			#lines(ss.out[[4]]$rec.est[2,ind,a,1], col = 3)
			#lines(ss.out[[5]]$rec.est[2,ind,a,1], col = 2)
			median.out[a, b, c] = median(ss.out[[a]]$rec.est[2,ind,b,c]) 
			median.om [a, c]    = median(om.out[[a]]$rec[2,ind,c]) 
			mean.out[a, b,c]    = mean(ss.out[[a]]$rec.est[2,ind,b,c]) 
			sd.out[a, b,c]      = sd(ss.out[[a]]$rec.est[2,ind,b,c]) 
			sd.om [a,c]         = sd(om.out[[a]]$rec[2,ind,c]) 
		}
	}	
}

par(mfrow = c(4, 4))
for (a in 1:16) { boxplot(t(median.out[,a,]), ylim = c(500, 2000)) }
mtext(side = 2, "Median Recruitment",outer = T, line =2)

par(mfrow = c(4, 4))
for (a in 1:16) { boxplot(t(sd.out[,a,]), ylim = c(500, 2000)) }
mtext(side = 2, "Median Recruitment",outer = T, line =2)

par(mfrow = c(4, 4))
for (a in 1:16) { boxplot(t(sd.out[,a,]), ylim = c(0, 500)) }
mtext(side = 2, "sd(recruitment)",outer = T, line =2)

par(mfrow = c(2, 3))
boxplot(t(sd.om[,]), ylim = c(0, 500)) 
mtext(side = 2, "sd(recruitment)",outer = T, line =2)


sd(ss.out[[5]]$rec.est[2,ind,a,1])
sd(ss.out[[4]]$rec.est[2,ind,a,1])

x = 151:175
above.target.ss = matrix(NA, length(steep.vec), length(hcr.vec))
med.ss.dist     = matrix(NA, length(steep.vec), length(hcr.vec))
below.msst.ss.all = matrix(NA, length(steep.vec), length(hcr.vec))
for (a in 1:length(steep.vec)){
	for(b in 1:length(hcr.vec)){
		above.target.ss[a,b] = sum(ss.out[[a]]$depl.est[b,x,17,] > target[b]) / length(ss.out[[a]]$depl.est[b,x,17,])
		med.ss.dist[a,b]     = median(ss.out[[a]]$depl.est[b,x,17,])
		below.msst.ss.all[a,b]   = sum(ss.out[[a]]$depl.est[b,x,17,] < target[b] * 0.50) / length(ss.out[[a]]$depl.est[b,x,17,])
	}
}



plot("", "", ylab = "", xlab = "",xlim = c(1, 17), ylim = c(0, 1400), axes = F); box()
points(1:17,median.out[1,], pch = 16, col = 1)
points(1:17,median.out[2,], pch = 16, col = 2)
points(1:17,median.out[3,], pch = 16, col = 3)
points(1:17,median.out[4,], pch = 16, col = 4)
points(1:17,median.out[5,], pch = 16, col = 5)
points(1:17,median.out[6,], pch = 16, col = 6)
axis(side = 1, at = 1:17); axis(side = 2)
mtext(side = 2, "median(recruit est)", line = 2)
mtext(side = 1, 'assessment', line = 2)
legend("bottomright", col = 1:6, pch = rep(16, 6), legend=c("h = 0.85", "h = 0.75", "h=0.95", "h=0.85/0.75", "h=0.85/0.95", "auto"))


#points(1:17,mean.out[1,], pch = 17, col = 3)
#points(1:17,mean.out[2,], pch = 17, col = 2)

points(1:17,sd.out[1,], pch = 18, col = 3)
points(1:17,sd.out[2,], pch = 18, col = 2)

plot("", "", xlim = c(1, 17), ylim = c(0, 1))
points(1:17,sd.out[1,]/mean.out[1,], pch = 19, col = 3)
points(1:17,sd.out[2,]/mean.out[2,], pch = 19, col = 2)


re.r0 = array(NA, dim = c(6, 17, 200))
for (a in 1:6)
for(b in 1:17){
	re.r0[a,b,] = (om.out[[a]]$rec[2,1,] - ss.out[[a]]$rec.est[2,1,b,])/om.out[[a]]$rec[2,1,]
}

re.sb0 = array(NA, dim = c(6, 17, 200))
for (a in 1:6)
for(b in 1:17){
	re.sb0[a,b,] = (om.out[[a]]$ssb[2,1,] - ss.out[[a]]$ssb.est[2,1,b,])/om.out[[a]]$ssb[2,1,]
}

re.rec = array(NA, dim = c(6, 175, 200))
for (a in 1:6){
	re.rec[a,,] = (om.out[[a]]$rec[2,1:175,] - ss.out[[a]]$rec.est[2,1:175,1,])/om.out[[a]]$rec[2,1:175,]
}


par(mfrow = c(4,4))
for(a in 1:16) { boxplot(t(re.r0[,a,]), ylim = c(-1,1)); abline(h = 0) }

par(mfrow = c(2,3), mar = c(2,2,2,2), oma = c(4,4,1,1))
for(a in 1:6) { boxplot(t(re.r0[a,,]), ylim = c(-1,1)); abline(h = 0) }
mtext(side = 2, "re R0", outer = T, line = 1)

par(mfrow = c(2,3), mar = c(2,2,2,2), oma = c(4,4,4,1))
for(a in 1:6) { boxplot(t(re.sb0[a,,]), ylim = c(-0.5,0.5)); abline(h = 0) }
mtext(side = 2, "re SB0", outer = T, line = 1)
mtext(side = 3, "hcr 25-5: All Scenarios Shown", outer = T, line = 1)


par(mfrow=c(6,2), mar = c(1,1,1,1), oma = c(1,1,1,1))
for(a in 1:6){
	boxplot(t(medians.out[[a]]$re.m[2,1,,]), ylim = c(-0.5, 0.50))
	abline(h = 0)
	boxplot(t(medians.out[[a]]$re.m[2,2,,]), ylim = c(-0.5, 0.50))
	abline(h = 0)
}

par(mfrow=c(6,2), mar = c(1,1,1,1), oma = c(1,1,1,1))
for(a in 1:6){
	boxplot(t(medians.out[[a]]$re.k[2,1,,]), ylim = c(-0.5, 0.50)); abline(h = 0)
	boxplot(t(medians.out[[a]]$re.k[2,2,,]), ylim = c(-0.5, 0.50)); abline(h = 0)
}

par(mfrow=c(3,2), mar = c(2,2,2,2), oma = c(4,4,4,4))
boxplot(t(medians.out[[1]]$re.m[2,1,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
mtext(side = 3, 'female')
boxplot(t(medians.out[[1]]$re.m[2,2,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
mtext(side = 3, 'male')
boxplot(t(medians.out[[4]]$re.m[2,1,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
boxplot(t(medians.out[[4]]$re.m[2,2,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
boxplot(t(medians.out[[5]]$re.m[2,1,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
boxplot(t(medians.out[[5]]$re.m[2,2,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
mtext(side = 2, "RE M", outer = T, line = 2.5)
mtext(side = 1, "assessment", outer = T, line = 2.5)
mtext(side = 3, "Scenario h =0.85 vs. h = 0.85/0.75 vs. h = 0.85/0.95", outer = T)

par(mfrow=c(3,2), mar = c(2,2,2,2), oma = c(4,4,4,4))
boxplot(t(medians.out[[1]]$re.k[2,1,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
mtext(side = 3, 'female')
boxplot(t(medians.out[[1]]$re.k[2,2,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
mtext(side = 3, 'male')
boxplot(t(medians.out[[4]]$re.k[2,1,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
boxplot(t(medians.out[[4]]$re.k[2,2,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
boxplot(t(medians.out[[5]]$re.k[2,1,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
boxplot(t(medians.out[[5]]$re.k[2,2,,]), ylim = c(-0.3, 0.30)); abline(h = 0)
mtext(side = 2, "RE k", outer = T, line = 2.5)
mtext(side = 1, "assessment", outer = T, line = 2.5)
mtext(side = 3, "Scenario h =0.85 vs. h = 0.85/0.75 vs. h = 0.85/0.95", outer = T)


par(mfrow=c(3,2), mar = c(2,2,2,2), oma = c(4,4,4,4))
boxplot(t(medians.out[[1]]$re.lmax[2,1,,]), ylim = c(-0.1, 0.10)); abline(h = 0)
mtext(side = 3, 'female')
boxplot(t(medians.out[[1]]$re.lmax[2,2,,]), ylim = c(-0.1, 0.10)); abline(h = 0)
mtext(side = 3, 'male')
boxplot(t(medians.out[[4]]$re.lmax[2,1,,]), ylim = c(-0.1, 0.1)); abline(h = 0)
boxplot(t(medians.out[[4]]$re.lmax[2,2,,]), ylim = c(-0.1, 0.1)); abline(h = 0)
boxplot(t(medians.out[[5]]$re.lmax[2,1,,]), ylim = c(-0.1, 0.1)); abline(h = 0)
boxplot(t(medians.out[[5]]$re.lmax[2,2,,]), ylim = c(-0.1, 0.1)); abline(h = 0)
mtext(side = 2, "RE Lmax", outer = T, line = 2.5)
mtext(side = 1, "assessment", outer = T, line = 2.5)
mtext(side = 3, "Scenario h =0.85 vs. h = 0.85/0.75 vs. h = 0.85/0.95", outer = T)