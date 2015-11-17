
a3 = 1; a.linear = 0

len.step = seq(4, 78, 2)
a3 = 2 ; a.linear = 2

#Plot length at age
plot(1:ages,  len[,1], ylab = "Length (cm)", xlab = "Age", col = 2, type = 'l', lwd = 2)
lines(1:ages, len[,2], col = 4, lty = 1, lwd = 2)
lines(1:ages, mid.len[,1], col = 2, lty = 2, lwd = 2)
lines(1:ages, mid.len[,2], col = 4, lty = 2, lwd = 2)

#Weight at length
plot(len.step, wght.at.len[,1], ylab = "Weight", xlab= "Length", col = 2, type='l', lwd =2)
lines(len.step, wght.at.len[,2], col = 4, lty =2, lwd = 2 )

#Weight at age
plot(1:ages, wght[,1], ylab = "Weight", xlab= "Age", col = 2, type='l', lwd =2)
lines(1:ages, wght[,2], col = 4, lty =2, lwd = 2 )

#Maturity
plot(1:ages, mature.age, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)

plot(len.step, mature.len, ylab = "Mature", xlab= "Age", col = 2, type='l', lwd =2)

#Selectivity
plot(len.step, selec[,1], ylab = "Selectivity", xlab = "Length", col = 2, lwd = 2, type = 'l')
lines(len.step, selec[,2], lty = 1, col = 4, lwd = 2)
lines(len.step, obs.selec[,1], lty = 2, col = 2, lwd = 2)
lines(len.step, obs.selec[,2], lty = 2, col = 4, lwd = 2)