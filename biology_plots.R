source("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/functions/LH_parameters.R")
source("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/functions/Get_Biology.R")

# Read in the parameter lsit and set according to simulation
load("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/parm_dist_list")

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

alpha.label = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)','(g)', '(h)', '(i)', '(j)')

nsim = 200 #1000
f.fleets = 2
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
selec.age <- out$selec.age
selec.age <- out$selec.age
obs.selec.age <- out$obs.selec.age
obs.selec.age <- out$obs.selec.age
selec <- out$selec
obs.selec <- out$obs.selec

len.f.ci  <- apply(len[,1,], 1, quantile,  c(0.05, 0.50, 0.95))
len.m.ci  <- apply(len[,2,], 1, quantile,  c(0.05, 0.50, 0.95))
wght.f.ci <- apply(wght[,1,], 1, quantile, c(0.05, 0.50, 0.95))
wght.m.ci <- apply(wght[,2,], 1, quantile, c(0.05, 0.50, 0.95))

# change low values for smoother plot
len.f.ci[3,2] = 18
len.m.ci[3,2] = 18

lab1 = lab= 1.15

setwd("C:/PhD/Chapter4/WriteUp/Plots")
png(filename = "biology_lenselex.png", width = 6.7, height = 9, units = 'in', res = 256)

par(mfrow=c(4,2), mar = c(5,4,3.5,2), oma = c(0,0,0,0), cex.axis = 1.1, cex.lab = 1.1)
# Natural mortality
out = density(m.f, bw = 0.01)
xx = c(out$x, rev(out$x)); yy = c(out$y, rev(rep(0,length(out$y))))
plot("", "", xlim = c(0, 0.30), ylim = c(0, 20), xlab = "Natural mortality - female", ylab = "Frequency", main = "", las = 1, axes = F)
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
print.letter(xy = c(0.05, 0.90), alpha.label[1])
#hist(m.f, xlim = c(0, 0.30), ylim = c(0, 50), xlab = "Natural mortality - female", ylab = "Frequency", main = "", las = 1, axes = F)
axis(side = 1) #; axis(side = 2, at = seq(0,100,25), las =1)
box()
#hist(m.m, xlim = c(0, 0.30), ylim = c(0, 75), xlab = "Natural mortality - male", ylab = "Frequency", main = "", las = 1, axes = F)
out = density(m.m, bw = 0.01)
xx = c(out$x, rev(out$x));  yy = c(out$y, rev(rep(0,length(out$y))))
plot("", "", xlim = c(0, 0.30), ylim = c(0, 20), xlab = "Natural mortality - male", ylab = "Frequency", main = "", las = 1, axes = F)
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
box(); axis(side =1) #; axis(side = 2, at = seq(0,100,25), las =1)
print.letter(xy = c(0.05, 0.90), alpha.label[2])

#Plot length at age
plot(1:ages,  len.f.ci[2,1:ages], ylim = c(0,60), xlim = c(1, 42), ylab = "Length (cm) - female", xlab = "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
xx <- c(1:ages, rev(1:ages)); yy <- c(len.f.ci[1,], rev(len.f.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, len.f.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side = 1); axis(side = 2, at = seq(0,60,20), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[3])

plot(1:ages,  len.m.ci[2,1:ages], ylim = c(0,60), xlim = c(1, 42), ylab = "Length (cm) - male", xlab = "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1, axes = F, las = 1)
xx <- c(1:ages, rev(1:ages)); yy <- c(len.m.ci[1,], rev(len.m.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, len.m.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side = 2, at = seq(0,60,20), las = 1); axis(side = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[4])

#Weight at age
plot(1:ages, wght.f.ci[1,], ylim = c(0, 3), xlim = c(1, 42), ylab = "Weight (kg) - female", xlab= "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
xx <- c(1:ages, rev(1:ages)); yy <- c(wght.f.ci[1,], rev(wght.f.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, wght.f.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side =1); axis(side =2, at = seq(0, 3, 1), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[5])

plot(1:ages, wght.m.ci[1,], ylim = c(0, 3), xlim = c(1, 42), ylab = "Weight (kg) - male", xlab= "Age", col = 'white', type = 'l', 
	xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
xx <- c(1:ages, rev(1:ages)); yy <- c(wght.m.ci[1,], rev(wght.m.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 0)
lines(1:ages, wght.m.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side =1); axis(side =2, at = seq(0, 3, 1), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[6])

# Fishery Selectivity

plot(len.step, selec[1,,1], ylab = "Fishery selectivity", xlab = "Length (cm)", xlim = c(0, max(len.step)),
   ylim = c(0, 1.05), col = 1, type = 'l', xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
lines(len.step, selec[1,,2], lty = 2, col = 1, lwd = 1)
lines(len.step, selec[2,,1], lty = 3, col = 'grey50', lwd = 2)
lines(len.step, selec[2,,2], lty = 4, col = 'grey50', lwd = 2)
legend("bottomright", legend = c("Fishery 1 (f)", "Fishery 1 (m)", "Fishery 2 (f)", "Fishery 2 (m)"), 
  lty = c(1,2,3,4), col = c(1,1,'grey50','grey50'), lwd = c(1,1,2,2), bty = 'n', cex = 1.1)
box(); axis(side =1); axis(side =2, at = seq(0, 1, 0.25), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[7])

# Survey Selectivity
plot(len.step, obs.selec[,1], ylab = "Survey selectivity", xlab = "Length (cm)", xlim = c(0, max(len.step)), 
  ylim = c(0, 1.05), col = 1, type = 'l', xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
lines(len.step, obs.selec[,2], lty = 2, col = 1, lwd = 1)
legend("bottomright", legend = c("female", "male"), lty = c(1,2), bty = 'n',cex = 1.1)
box(); axis(side =1); axis(side =2, at = seq(0, 1, 0.25), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[8])

dev.off()

# Fishery 1
#plot(1:ages, selec.age[1,1,], ylab = "Fishery selectivity", xlab = "Age", xlim = c(1, 42), ylim = c(0, 1.05), col = 1, type = 'l', 
#  xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
#lines(1:ages, selec.age[1,2,], lty = 2, col = 1, lwd = 1)
#lines(1:ages, selec.age[2,1,], lty = 3, col = 'grey50', lwd = 2)
#lines(1:ages, selec.age[2,2,], lty = 4, col = 'grey50', lwd = 2)
#legend("bottomright", legend = c("Fishery 1 (f)", "Fishery 1 (m)", "Fishery 2 (f)", "Fishery 2 (m)"), 
#  lty = c(1,2,3,4), col = c(1,1,'grey50','grey50'), lwd = c(1,1,2,2), bty = 'n', cex = 1.1)
#box(); axis(side =1); axis(side =2, at = seq(0, 1, 0.25), las = 1)
#print.letter(xy = c(0.05, 0.90), alpha.label[7])
#
#
## Survey Selectivity
#plot( 1:ages, obs.selec.age[,1], ylab = "Survey selectivity", xlab = "Age", xlim = c(1, 42), ylim = c(0, 1.05), col = 1, type = 'l', 
#    xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
#lines(1:ages, obs.selec.age[,2], lty = 2, col = 1, lwd = 1)
#legend("bottomright", legend = c("female", "male"), lty = c(1,2), bty = 'n',cex = 1.1)
#box(); axis(side =1); axis(side =2, at = seq(0, 1, 0.25), las = 1)
#print.letter(xy = c(0.05, 0.90), alpha.label[8])
#lines(1:ages, out$mature.age, col = 2, lty = 1, lwd =1)

#Maturity
#plot(1:ages, out$mature.age, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)
#plot(len.step, mature.len, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)



setwd("C:/PhD/Chapter4/WriteUp/Plots")
png(filename = "biology_4panel.png", width = 6.7, height = 6, units = 'in', res = 256)

par(mfrow=c(2,2), mar = c(4,4,3,2), oma = c(0,0,0,0), cex.axis = 1.1, cex.lab = 1.1)
# Natural mortality
# females
out = density(m.f, bw = 0.01)
xx = c(out$x, rev(out$x)); yy = c(out$y, rev(rep(0,length(out$y))))
plot("", "", xlim = c(0.07, 0.26), ylim = c(0, 20), xlab = "Natural mortality", ylab = "Frequency", main = "", las = 1, axes = F)
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[1])
print.letter(xy = c(0.28, 0.79), 'females')
axis(side = 1)
box()
# males
out = density(m.m, bw = 0.01)
xx = c(out$x, rev(out$x));  yy = c(out$y, rev(rep(0,length(out$y))))
polygon(xx, yy, col = rgb(0,0,0,0.25), lty = 3)
box(); axis(side =1) 
print.letter(xy = c(0.62, 0.74), 'males')

#Plot length at age
# females
plot(1:ages,  len.f.ci[2,1:ages], ylim = c(0,67), xlim = c(1, 42), ylab = "Length (cm)", xlab = "Age", col = 'white', type = 'l', 
  xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
xx <- c(1:ages, rev(1:ages)); yy <- c(len.f.ci[1,], rev(len.f.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.1), lty = 1)
lines(1:ages, len.f.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side = 1); axis(side = 2, at = seq(0,60,20), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[2])
print.letter(xy = c(0.80, 0.92), 'females')

# males
xx <- c(1:ages, rev(1:ages)); yy <- c(len.m.ci[1,], rev(len.m.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.25), lty = 3)
lines(1:ages, len.m.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side = 2, at = seq(0,60,20), las = 1); axis(side = 1)
print.letter(xy = c(0.80, 0.54), 'males')


#Weight at age
# females
plot(1:ages, wght.f.ci[1,], ylim = c(0, 3.5), xlim = c(1, 42), ylab = "Weight (kg)", xlab= "Age", col = 'white', type = 'l', 
  xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
xx <- c(1:ages, rev(1:ages)); yy <- c(wght.f.ci[1,], rev(wght.f.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 1)
lines(1:ages, wght.f.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side =1); axis(side =2, at = seq(0, 3, 1), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[3])
print.letter(xy = c(0.80, 0.90), 'females')


xx <- c(1:ages, rev(1:ages)); yy <- c(wght.m.ci[1,], rev(wght.m.ci[3,]))
polygon(xx, yy, col = rgb(0,0,0,0.2), lty = 3)
lines(1:ages, wght.m.ci[2,], col = 'white', lty = 1, lwd = 2)
box(); axis(side =1); axis(side =2, at = seq(0, 3, 1), las = 1)
print.letter(xy = c(0.80, 0.10), 'males')

# Fishery Selectivity
# Fishery 1
plot(len.step, selec[1,,1], ylab = "Selectivity", xlab = "Length (cm)", xlim = c(0,max(len.step)), 
  ylim = c(0, 1.05), col = 1, type = 'l', xaxs="i", yaxs = 'i', lwd = 1, las = 1, axes = F)
lines(len.step, selec[1,,2], lty = 1, col = 1, lwd = 1)
lines(len.step, selec[2,,1], lty = 2, col = 1, lwd = 1)
lines(len.step, selec[2,,2], lty = 2, col = 1, lwd = 1)
box(); axis(side =1); axis(side =2, at = seq(0, 1, 0.25), las = 1)
print.letter(xy = c(0.05, 0.90), alpha.label[4])
print.letter(xy = c(0.65, 0.54), 'females')
print.letter(xy = c(0.35, 0.65), 'males')

# Survey Selectivity
#lines(len.step, obs.selec[,1], lty = 3, col = 'grey50', lwd = 1)
#lines(len.step, obs.selec[,2], lty = 3, col = 'grey50', lwd = 1)
#legend("bottomright", legend = c("female", "male"), lty = c(1,2), bty = 'n',cex = 1.1)
#box(); axis(side =1); axis(side =2, at = seq(0, 1, 0.25), las = 1)
#lines(1:ages, out$mature.age, col = 2, lty = 1, lwd =1)
legend("bottomright", legend = c("Fishery 1", "Fishery 2"), 
  lty = c(1,2,3), col = c(1,1), lwd = c(1,1), bty = 'n', cex = 1)

#Maturity
#plot(1:ages, out$mature.age, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)
#plot(len.step, mature.len, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)
dev.off()

