########################################################
##			Create Parameter Distributions	          ##
##													  ##
##		Based on Petrale 2015 Estimated Values        ##
##		  used for the HCR MSE for Flatfish_MSC		  ##
##													  ##
##			  Writen by: Chantel Wetzel				  ##
##				  November 17, 2015					  ##
########################################################


directory <- "C:/Flatfish_MSC/"

# Source in the the estimated parameter values
source("C:/Flatfish_MSC/code/functions/LH_parameters.R")

# Number of samples
N = 1000
set.seed(123456789)

# Define standard deviations
m.sd = 0.15
k.sd = 0.15
lmin.sd = 2.5
lmax.sd = 2.5

# Define distributions===========================================
# Natural mortality - lognormal
m.f.vec = round(rlnorm(N, meanlog=( log(m.f.mean) - 0.5*m.sd^2 ), sdlog=m.sd), 3)
m.m.vec = round(rlnorm(N, meanlog=( log(m.m.mean) - 0.5*m.sd^2 ), sdlog=m.sd), 3)

# Brody's growth coefficient k - lognormal
kf.vec = round(rlnorm(N, meanlog = ( log(kf.mean) - 0.50*k.sd^2 ), sdlog = k.sd), 3)
km.vec = round(rlnorm(N, meanlog = ( log(km.mean) - 0.50*k.sd^2 ), sdlog = k.sd), 3)

# Minimum length - normal
lmin.f.vec = round(rnorm(N, L1f.mean, lmin.sd), 0)
lmin.m.vec = round(rnorm(N, L1m.mean, lmin.sd), 0)  

# Maximum length - normal
lmax.f.vec = round(rnorm(N, L2f.mean, lmax.sd), 0)
lmax.m.vec = round(rnorm(N, L2m.mean, lmax.sd), 0)  

par(mfrow = c(4, 2))
hist(m.f.vec)
hist(m.m.vec)
hist(kf.vec)
hist(km.vec)
hist(lmin.f.vec)
hist(lmin.m.vec)
hist(lmax.f.vec)
hist(lmax.m.vec)

out.file <- paste(directory,"parm_dist_list",sep="")
parm.list <- list()
parm.list[[1]] <- m.f.vec
parm.list[[2]] <- m.m.vec
parm.list[[3]] <- kf.vec
parm.list[[4]] <- km.vec
parm.list[[5]] <- lmin.f.vec
parm.list[[6]] <- lmin.m.vec
parm.list[[7]] <- lmax.f.vec
parm.list[[8]] <- lmax.m.vec
names(parm.list) <- c("m.f", "m.m", 'kf', "km", "L1f", "L1m", "L2f", "L2m")
save(parm.list, file = out.file)