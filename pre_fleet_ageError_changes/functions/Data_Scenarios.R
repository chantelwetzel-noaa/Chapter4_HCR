###############################################################################################
#                                      Model Paramters                                        #
#                                                                                             #
#		           Set up basic model parameters for the OM and EM side                       #
#                                                                                             #
###############################################################################################

#Buffer Values for Management
sigma   <- 0.36
p.value <- 0.45
buffer  <- exp(qnorm(p.value, 0, sigma))

# SS Estimation Parameters
survey.time  <- 0.5
fishing.time <- -1        
max.F 		 <- 4 

# Auto-Correlated Recruitment Switch
auto    <- FALSE

# Swith to apply the ageing error matrix
AgeError <- FALSE 
#Adjust the max age value that would be used if ageing error is included
max.age = ages - 1
if(AgeError == TRUE) { max.age  <- ages + 4  }  

# Survey and Sigma R parameters
Q            <- 3 #1
sigmaR 	     <- sigmaR.set <- 0.40  
ss.survey.cv <- survey.cv  <- 0.20

# Model dimensions
pre.fishery.yrs <- ages - 1 
setup.yrs       <- 50 
project.yrs     <- 80
fishery.yrs     <- setup.yrs + project.yrs + 1
total.yrs       <- pre.fishery.yrs + fishery.yrs
start.s.data    <- ages + setup.yrs - setup.yrs  #20
start.f.data    <- ages + setup.yrs - setup.yrs  #30
ass.freq        <- 5
years           <- 1:total.yrs 
ass.num         <- (project.yrs / ass.freq ) + 1

# Determine when the data begins
start.fishery.len.samp <- start.f.data
start.fishery.age.samp <- start.f.data
start.survey.len.samp  <- start.s.data
start.survey.age.samp  <- start.s.data 

# Data Available Based on Scenario
N.f.len = 100 ; N.s.len = 100 ; N.f.age = 100 ; N.s.age = 100 
# Create the sample number vectors
data.yrs   <- start.fishery.len.samp : total.yrs
f.len.samp <- c(rep(0, start.f.data - 1), rep(N.f.len, length(data.yrs)))
data.yrs   <- start.survey.len.samp : total.yrs
s.len.samp <- c(rep(0,start.s.data - 1), rep(N.s.len,length(data.yrs)))
data.yrs   <- start.fishery.age.samp : total.yrs
f.age.samp <- c(rep(0, start.f.data - 1), rep(N.f.age, length(data.yrs)))
data.yrs   <- start.survey.age.samp : total.yrs
s.age.samp <- c(rep(0,start.s.data - 1), rep(N.s.age, length(data.yrs)))