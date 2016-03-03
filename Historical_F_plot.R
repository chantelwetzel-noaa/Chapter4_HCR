#Historical F pattern plot

source("C:/Flatfish_MSC/code/functions/LH_parameters.R")
source("C:/Flatfish_MSC/code/functions/Get_Biology.R")
source("C:/Flatfish_MSC/code/functions/Data_Scenarios.R")


effort  = numeric(0)
effort[1:(ages - 1)] <- 0
ind <- ages:(pre.fishery.yrs + setup.yrs - 24)
effort[ind] <- 0.40 * ((ind + 1 - (ages)) / length(ind) )
ind2 <- (max(ind)+1) : (pre.fishery.yrs + setup.yrs)
effort[ind2] <- 0.40

fishery1 = 0.75 * effort
fishery2 = 0.25 * effort

par(mfrow = c(1,1))
plot(1:50, fishery1[ages:(ages + 49)], type = 'l', lwd = 2, ylim = c(0,0.50), axes = F, ylab = "", xlab="", yaxs = 'i', xaxs = "i")
lines(1:50, fishery2[ages:(ages + 49)], lty = 2, lwd = 2)
box()
axis(side = 1); axis(side = 2)
mtext(side=1,"Year",line=3, cex=1.1)
mtext(side=2,"Historical F Pattern",line=3, cex=1.1)
legend("topright", legend = c("Fishery 1", "Fishery 2"), lty = c(1,2), lwd = c(2,2), bty = 'n', cex = 1.1)