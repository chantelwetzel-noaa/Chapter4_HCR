source("C:/Flatfish_MSC/code/functions/LH_parameters.R")
source("C:/Flatfish_MSC/code/functions/Get_Biology.R")

# Read in the parameter lsit and set according to simulation
load("C:/Flatfish_MSC/parm_dist_list")

nsim = 1000
len <- array(NA, dim = c(ages, 2, nsim))
wght <- array(NA, dim = c(ages, 2, nsim))
wght.at.len <- array(NA, dim = c(length(len.step), 2, nsim))

for(i in 1:nsim){
  kf  <- parm.list$kf[[i]]
  km  <- parm.list$km[[i]]
  L1f <- parm.list$L1f[[i]]
  L1m <- L1f # parm.list$L1m[[nsim]] 
  L2f <- parm.list$L2f[[i]]
  L2m <- parm.list$L2m[[i]]

  if (L1f < 16) {a3 = a.linear = 1}
  if (L1m < 16) {a3 = a.linear = 1}

  out <- Get_Biology()
  len[,,i] <- out$len
  wght.at.len[,,i] <- out$wght.at.len
  wght[,,i] <- out$wght
}
 
m.f <- parm.list$m.f
m.m <- parm.list$m.m 
selec.f <- out$selec.age.f
selec.m <- out$selec.age.m
obs.selec.f <- out$obs.selec.age.f
obs.selec.m <- out$obs.selec.age.m

len.f.ci  <- apply(len[,1,], 1, quantile,  c(0.05, 0.50, 0.95))
len.m.ci  <- apply(len[,2,], 1, quantile,  c(0.05, 0.50, 0.95))
wght.f.ci <- apply(wght[,1,], 1, quantile, c(0.05, 0.50, 0.95))
wght.m.ci <- apply(wght[,2,], 1, quantile, c(0.05, 0.50, 0.95))

# change low values for smoother plot
len.f.ci[3,2] = 18
len.m.ci[3,2] = 18

lab1 = lab= 1.15
par(mfrow=c(4,2), mar = c(4,4,4,4), cex.axis = 1.2, cex.lab = 1.2)
# Natural mortality
hist(m.f, xlim = c(0, 0.30), ylim = c(0, 350), xlab = "Natural Mortality - Female", ylab = "Frequency", main = "")
box()
hist(m.m, xlim = c(0, 0.30), ylim = c(0, 350), xlab = "Natural Mortality - Male", ylab = "Frequency", main = "")
box()

#Plot length at age
plot(1:ages,  len.f.ci[2,1:ages], ylim = c(0,60), xlim = c(1, 42), ylab = "Length (cm) - Female", xlab = "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1)
xx <- c(1:ages, rev(1:ages)); yy <- c(len.f.ci[1,], rev(len.f.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, len.f.ci[2,], col = 'white', lty = 1, lwd = 2)
box()

plot(1:ages,  len.m.ci[2,1:ages], ylim = c(0,60), xlim = c(1, 42), ylab = "Length (cm) - Male", xlab = "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1)
xx <- c(1:ages, rev(1:ages)); yy <- c(len.m.ci[1,], rev(len.m.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, len.m.ci[2,], col = 'white', lty = 1, lwd = 2)

#Weight at age
plot(1:ages, wght.f.ci[1,], ylim = c(0, 3), xlim = c(1, 42), ylab = "Weight (kg) - Female", xlab= "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1)
xx <- c(1:ages, rev(1:ages)); yy <- c(wght.f.ci[1,], rev(wght.f.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, wght.f.ci[2,], col = 'white', lty = 1, lwd = 2)
box()

plot(1:ages, wght.m.ci[1,], ylim = c(0, 3), xlim = c(1, 42), ylab = "Weight (kg) - Male", xlab= "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1)
xx <- c(1:ages, rev(1:ages)); yy <- c(wght.m.ci[1,], rev(wght.m.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, wght.m.ci[2,], col = 'white', lty = 1, lwd = 2)
box()

# Fishery Selectivity
plot(1:ages, selec.f, ylab = "Fishery Selectivity", xlab = "Age", xlim = c(1, 42), ylim = c(0, 1.05), col = 1, type = 'l', xaxs="i", yaxs = 'i', lwd = 1)
lines(1:ages, selec.m, lty = 2, col = 1, lwd = 1)
legend("bottomright", legend = c("Female", "Male"), lty = c(1,2), bty = 'n', cex = 1.1)
#lines(1:ages, out$mature.age, col = 2, lty = 1, lwd =1)

# Survey Selectivity
plot(1:ages, obs.selec.f, ylab = "Survey Selectivity", xlab = "Age", xlim = c(1, 42), ylim = c(0, 1.05), col = 1, type = 'l', xaxs="i", yaxs = 'i', lwd = 1)
lines(1:ages, obs.selec.m, lty = 2, col = 1, lwd = 1)
legend("bottomright", legend = c("Female", "Male"), lty = c(1,2), bty = 'n',cex = 1.1)
#lines(1:ages, out$mature.age, col = 2, lty = 1, lwd =1)

#Maturity
plot(1:ages, out$mature.age, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)

plot(len.step, mature.len, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)

