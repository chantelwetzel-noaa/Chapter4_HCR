
wd = "C:/Flatfish_MSC/Petrale2015_REALbase/"
source("C:/Flatfish_MSC/code/getADMBHessian.R")
source("C:/Flatfish_MSC/code/functions/LH_parameters.R")

# Get hessian
#install.packages( "corpcor" )
require(corpcor)
require(mvtnorm)
require(Matrix)

HESS <- getADMBHessian(File=wd, FileName="admodel.hes")
cov <- corpcor::pseudoinverse(HESS$hes)
scale <- HESS$scale
cov.bounded <- cov*(scale %o% scale)
# Only define the parameters that I want
cov.bounded <- cov.bounded[1:12, 1:12]
H <-  corpcor::pseudoinverse(cov.bounded)
cov = solve(H)
roundCov <- round(cov, 1)*1.5



set.seed (123456)
# Define the mean values
parhat = c(m.f, L1f, L2f, kf, CV1, CV2, m.m, L1m, L2m, km, CV1, CV2)
rand = rmvt(1000, sigma=roundCov, df=10, method="svd")
Rand = rmvnorm(n=1000, mean=parhat, sigma=roundCov)
colnames(Rand) = c("M_f", "Lmin_f", "Lmax_f", "k_f", "cv_young_f", "cv_old_f",
				   "M_m", "Lmin_m", "Lmax_m", "k_m", "cv_young_m", "cv_old_m")
par(mfrow = c(3,4))
for (i in 1:12){hist(Rand[,i])}

par(mfrow = c(3,4))
for (i in 1:12){to.plot = parhat[i] + rand[,i]; hist(to.plot)}
#Petral covar.bounded diagonal quantities
#1 : M females
#2 : Lmin females
#3 : Lmax females
#4 : k females
#5 : cv young females
#6 : cv old females
#7 : M males
#8 : Lmin males
#9 : Lmax males
#10: k males
#11: cv young males
#12: cv old males
#13: R0
#14: h
