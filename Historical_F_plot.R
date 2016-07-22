#Historical F pattern plot

source("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/functions/LH_parameters.R")
source("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/functions/Get_Biology.R")
source("C:/Users/Chantell.Wetzel/Documents/GitHub/Chapter4_HCR/functions/Data_Scenarios.R")


effort  = numeric(0)
effort[1:(ages - 1)] <- 0
ind <- ages:(pre.fishery.yrs + setup.yrs - 24)
effort[ind] <- 0.40 * ((ind + 1 - (ages)) / length(ind) )
ind2 <- (max(ind)+1) : (pre.fishery.yrs + setup.yrs)
effort[ind2] <- 0.40

fishery1 = 0.75 * effort
fishery2 = 0.25 * effort

ylabel = expression("Historical "~italic(F))
setwd("C:/PhD/Chapter4/WriteUp/Plots")
png(filename = "HistF.png", width = 3.5, height = 4.5, units = 'in', res = 256)

par(mfrow = c(1,1))
plot(1:50, fishery1[ages:(ages + 49)], type = 'l', lwd = 1, ylim = c(0,0.50), axes = F, ylab = "", 
	xlab="", yaxs = 'i', xaxs = "i")
lines(1:50, fishery2[ages:(ages + 49)], lty = 2, lwd = 1)
box()
axis(side = 1, , cex.axis = 0.8); axis(side = 2, las = 1, cex.axis = 0.8)
mtext(side=1,"Year",line=2, cex=0.8)
mtext(side=2,ylabel,line=2.5, cex=0.8)
legend("topright", legend = c("Fishery 1", "Fishery 2"), lty = c(1,2), lwd = c(1,1), bty = 'n', 
	cex = 0.8)

dev.off()