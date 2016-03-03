
LH <-"flatfish"
SR <- "BH"


 R0.vals <- R0 <- 10000
 f.val <- seq(0,0.6,0.001)
 
 source("C:\\Flatfish_MSC\\code\\functions\\LH_parameters.R")

  L1f = L1f.mean ; L1m = L1m.mean
  L2f = L2f.mean ; L2m = L2m.mean
  m.m = m.m.mean ; m.f = m.f.mean
  kf  = kf.mean  ; km  = km.mean
 
 if (LH == "flatfish") 
 {
    #f.val <-  0.2363076#This is the SPR 30 value #0.1624733# This is the SPR 40 value Beverton Holt
    #f.val <-  0.2716982#Shepherds Bmsy/B0 =  0.2610069   
    #if (case.num == 4) { f.val = (f.val*(0.15-0.05)*(0.25))/(0.20*0.15)  }
    total.yrs <- 300
    steep=1
    survey.time = 0.50

  }

 if (LH == "rockfish") 
 {
    #f.val <- 0.03185843# This is the SPR 50 value
    #f.val <- 0.04467823# This is for the Shepherds curve with c= 1.75 Bmsy/B0 = 0.2654
    #if (case.num == 4) { f.val = (f.val*(0.20-0.10)*(0.40))/(0.30*0.20)  }
    total.yrs <- 500
    steep=1
    survey.time = 0.50


  }


 bin<-length(len.step)
 numbers <- array(0,dim=c(total.yrs,ages,2)) 
 biomass <- array(0,dim=c(total.yrs,ages,2))   
 SSB.fraction <-array(NA,dim=c(length(R0.vals),2,length(f.val)))
 SSB.store <- array(NA,dim=c(length(R0.vals),total.yrs,length(f.val))) 
 Ry.store <- array(NA,dim=c(length(R0.vals),total.yrs,length(f.val)))
 catch.store <- array(NA,dim=c(length(R0.vals),total.yrs-1,length(f.val))) 

#------------------------------------------------------------------------------------------------------
Get.Biology<- function()
{

#Calculate Lengths
 Len <- list()
 len<-matrix(NA,ages,sexes)
 mid.len<-matrix(0,ages,sexes)
 
 #L infinity (cm)
  Linf_f<-L1f+((L2f-L1f)/(1-exp(-kf*(a4-a3))))
  Linf_m<-L1f+((L2m-L1f)/(1-exp(-km*(a4-a3))))
  len.slope.f <- (L1f-len.step[1])/a3
  len.slope.m <- (L1m-len.step[1])/a3
 
 #Length at the start of the year (cm)
  len[1:a.linear,1]<-len.step[1]+len.slope.f*(seq(1,a.linear,1)-1)  #For smallest fish length is a linear function  
  len[1:a.linear,2]<-len.step[1]+len.slope.m*(seq(1,a.linear,1)-1) 
  #Growth based on the VB
  len[(a.linear+1):ages,2]<-Linf_m+(L1m-Linf_m)*exp(-km*((seq(a.linear+1,ages,1)-1)-a3))
  len[(a.linear+1):ages,1]<-Linf_f+(L1f-Linf_f)*exp(-kf*((seq(a.linear+1,ages,1)-1)-a3))
   
 #Mid-year lengths   (cm)
  mid.len[1:a.linear,1]<-mean(len[1:2,1])+len.slope.f*(seq(1,a.linear,1)-1)   #For smallest fish length is a linear function 
  mid.len[1:a.linear,2]<-mean(len[1:2,1])+len.slope.m*(seq(1,a.linear,1)-1)
  #Growth bases on VB
  mid.len[(a.linear+1):ages,2]<-len[(a.linear+1):ages,2]+(len[(a.linear+1):ages,2]-Linf_m)*(exp(-0.5*km)-1) 
  mid.len[(a.linear+1):ages,1]<-len[(a.linear+1):ages,1]+(len[(a.linear+1):ages,1]-Linf_f)*(exp(-0.5*kf)-1)
 
 Len[[1]] <- len
 Len[[2]] <- mid.len
 names(Len) <- c("len","mid.len")
 #rm(len, mid.len, Linf_f, Linf_m, len.slope)

#=================================================================================================================
#Calculate the Transisition Matrix
 Phi <- list()
 #St Dev of Mid-year lengths  (cm)
 mid.sigma<-matrix(0,ages,sexes) 
 sigma.phi<-matrix(0,ages,sexes)
 phi.m<-matrix(0,length(len.step),ages) 
 phi.f<-matrix(0,length(len.step),ages) 
 mid.phi.m<-matrix(0,length(len.step),ages) 
 mid.phi.f<-matrix(0,length(len.step),ages)

 #CV about Length at the start of year
  sigma.phi[1:a.linear,]<-Len$len[1:a.linear,]*CV1
  sigma.phi[(a.linear+1):floor(a4),] <-Len$len[(a.linear+1):floor(a4),]*(CV1+(Len$len[(a.linear+1):floor(a4),]
                                    -Len$len[a.linear,])/(Len$len[ceiling(a4),]-Len$len[a.linear,])*(CV2-CV1))
  sigma.phi[ceiling(a4):ages,]<-Len$len[ceiling(a4):ages,]*CV2

 #CV about Length at the mid-year
  mid.sigma[1:a.linear,]<- Len$mid.len[1:a.linear,]*CV1
  mid.sigma[(a.linear+1):floor(a4),] <-Len$mid.len[(a.linear+1):floor(a4),]*(CV1+(Len$mid.len[(a.linear+1):floor(a4),]
                                        -Len$mid.len[a.linear,])/(Len$mid.len[ceiling(a4),]-Len$mid.len[a.linear,])*(CV2-CV1))
  mid.sigma[ceiling(a4):ages,]<-Len$mid.len[ceiling(a4):ages,]*CV2

 #Start of year phi matrix
  for (b in 1:ages) 
   {   
    phi.m[1,b]<-pnorm(len.step[1+1],Len$len[b,2],sigma.phi[b,2])
    phi.f[1,b]<-pnorm(len.step[1+1],Len$len[b,1],sigma.phi[b,1])
    total.m <- phi.m[1,b]
    total.f<-phi.f[1,b] 
      for (a in 2:(length(len.step)-1)) 
      {
       p1 <- pnorm(len.step[a+1],Len$len[b,2],sigma.phi[b,2])
       p2 <- pnorm(len.step[a+1],Len$len[b,1],sigma.phi[b,1])
       phi.m[a,b] <- p1-total.m
       phi.f[a,b] <- p2-total.f
       total.m <- p1
       total.f <- p2
      } 
    phi.m[bin,b] <- 1-total.m
    phi.f[bin,b] <- 1-total.f
   }
        
  
 #Mid year phi matrix
  for (b in 1:ages) 
   {   
    mid.phi.m[1,b]<-pnorm(len.step[1+1],Len$mid.len[b,2],mid.sigma[b,2])
    mid.phi.f[1,b]<-pnorm(len.step[1+1],Len$mid.len[b,1],mid.sigma[b,1])
    total.m <- mid.phi.m[1,b]
    total.f<-mid.phi.f[1,b] 
     for (a in 2:(length(len.step)-1)) 
      {
       p1 <- pnorm(len.step[a+1],Len$mid.len[b,2],mid.sigma[b,2])
       p2 <- pnorm(len.step[a+1],Len$mid.len[b,1],mid.sigma[b,1])
       mid.phi.m[a,b] <- p1-total.m
       mid.phi.f[a,b] <- p2-total.f
       total.m <- p1
       total.f <- p2
      } 
    mid.phi.m[bin,b] <- 1-total.m
    mid.phi.f[bin,b] <- 1-total.f
   }
   
 Phi[[1]] <- mid.sigma
 Phi[[2]] <- sigma.phi
 Phi[[3]] <- phi.m
 Phi[[4]] <- phi.f
 Phi[[5]] <- mid.phi.m
 Phi[[6]] <- mid.phi.f
 names(Phi) <- c("mid.sigma","sigma.phi","phi.m","phi.f","mid.phi.m","mid.phi.f")
 rm(sigma.phi, mid.sigma, phi.m, phi.f, mid.phi.m, mid.phi.f, total.m, total.f, a, b, p1, p2)
 
#==========================================================================================
#GetWght

 Wght <- list()
 
 mid.wght<-matrix(0,ages,sexes)
 sample.mid.wght<-matrix(0,ages,sexes)
 wght<-matrix(NA,ages,sexes)
 wght.at.len<-matrix(0,length(len.step),sexes)
 mid.wght.at.len<-matrix(0,length(mid.len.step),sexes)

 #Virgin Weight @ Length (kg)
  wght.at.len[,2]<-(wght.coef.m*(len.step)^wght.exp.m)
  wght.at.len[,1]<-(wght.coef.f*(len.step)^wght.exp.f)
  mid.wght.at.len[,2]<-(wght.coef.m*(mid.len.step)^wght.exp.m)  #this is the value being outputted in ss3
  mid.wght.at.len[,1]<-(wght.coef.f*(mid.len.step)^wght.exp.f)

 #Virgin Weight @ Age  (kg)
  wght[,2]<-(t(Phi$phi.m))%*%mid.wght.at.len[,2]   
  wght[,1]<-(t(Phi$phi.f))%*%mid.wght.at.len[,1]
  mid.wght[,2]<-(t(Phi$mid.phi.m))%*%mid.wght.at.len[,2]
  mid.wght[,1]<-(t(Phi$mid.phi.f))%*%mid.wght.at.len[,1]    

 Wght[[1]] <- wght.at.len
 Wght[[2]] <- mid.wght.at.len 
 Wght[[3]] <- wght
 Wght[[4]] <- mid.wght
 names(Wght) <- c("wght.at.len","mid.wght.at.len","wght","mid.wght")
 rm(wght, mid.wght, wght.at.len, mid.wght.at.len)

#================================================================================================
#Fecundity
 Fecund <- list()
 
 mature.len<-1/(1+exp((ohm3)*(mid.len.step-ohm4)))#SS calcs using mid lengths
 mature.age<-(t(Phi$mid.phi.f))%*%mature.len  #SS uses mid phi values
 eggs<-ohm5+ohm6*Wght$wght.at.len[,1]  # eggs per kg

 fecund<-rep(0,ages)
 fecund<- (t(Phi$phi.f))%*%(mature.len*eggs*Wght$mid.wght.at.len[,1])

 Fecund[[1]] <- mature.age
 Fecund[[2]] <- mature.len
 Fecund[[3]] <- fecund
 names(Fecund) <- c("mature.age","mature.len","fecund")
 rm(mature.len, mature.age, fecund)

#Selectivity Function=============================================================
  Selex <- list()
  selec <- matrix(NA,length(len.step),sexes)
  selec.age.m<-matrix(0,ages,1)
  selec.age.f<-matrix(0,ages,1) 
  
  #Double Normal Selectivity
  startbin <- 1
  peak <- fsp1
  upselex <- exp(fsp3)
  downselex <- exp(fsp4)
  final <- fsp6

  point1 <- 1/(1+exp(-fsp5)) 
  t1min <- exp(-((len.step[startbin]+1)-peak)^2/upselex)
  peak2 <- peak + 2 + (0.99*(len.step[length(len.step)]+1)-peak-2)/(1+exp(-fsp2))
  point2 <- 1/(1+exp(-final))
  t2min <- exp(-((len.step[length(len.step)]+1)-peak2)^2/downselex)
  t1 <- len.step+1-peak
  t2 <- len.step+1-peak2
  join1 <- 1/(1+exp(-(20/(1+abs(t1)))*t1))
  join2 <- 1/(1+exp(-(20/(1+abs(t2)))*t2))
  asc <- point1 +(1-point1)*(exp(-t1^2/upselex)-t1min)/(1-t1min)
  #if (fsp5 <= -999) {asc =  exp(-(t1^2)/upselex)}
  dsc <- 1 +(point2-1)*(exp(-t2^2/downselex)-1)/(t2min-1)
  #if (fsp6 <- -999) {dsc = exp(-(t2^2)/downselex)}

  selec[,1] <- asc*(1-join1)+join1*(1-join2+dsc*join2) 
  selec[,2]<-selec[,1]


 #Mid-year Selectivity by Age
  selec.age.m <-(t(Phi$mid.phi.m))%*%selec[,2]
  selec.age.f <-(t(Phi$mid.phi.f))%*%selec[,1]
  
 Selex[[1]] <- selec
 Selex[[2]] <- selec.age.f
 Selex[[3]] <- selec.age.m
 names(Selex) <- c("selec","selec.age.f","selec.age.m")


#=========================================================================================================================================
#Obs_Selectivity 
 Obs.Selex <- list()
 obs.selec<-matrix(0,length(mid.len.step),sexes) 
 obs.selec.age.m<-matrix(0,ages,1)
 obs.selec.age.f<-matrix(0,ages,1)

 #Survey Selectivity pattern Double Normal 
  startbin <- 1
  peak <- ssp1
  upselex <- exp(ssp3)
  downselex <- exp(ssp4)
  final <- ssp6

  point1 <- 1/(1+exp(-ssp5))
  t1min <- exp(-((len.step[startbin]+1)-peak)^2/upselex)
  peak2 <- peak + 2 + (0.99*(len.step[1]+1)-peak-2)^2/(1+exp(-ssp2))
  point2 <- 1/(1+exp(-final))
  t2min <- exp(-((len.step[length(len.step)]+1)-peak2)^2/downselex)
  t1 <- len.step+1-peak
  t2 <- len.step+1-peak2
  join1 <- 1/(1+exp(-(20/(1+abs(t1)))*t1))
  join2 <- 1/(1+exp(-(20/(1+abs(t2)))*t2))
  asc <- point1 +(1-point1)*(exp(-t1^2/upselex)-t1min)/(1-t1min)
  dsc <- 1 +(point2-1)*exp(-t2^2/downselex-1)/(t2min-1)

  obs.selec[,1] <- asc*(1-join1)+join1*(1-join2+dsc*join2) 
  obs.selec[,2]<-obs.selec[,1]
 
 #Mid-year Selectivity by Age
   obs.selec.age.m <-(t(Phi$mid.phi.m))%*%obs.selec[,2]
   obs.selec.age.f <-(t(Phi$mid.phi.f))%*%obs.selec[,1]

   Obs.Selex[[1]] <- obs.selec
   Obs.Selex[[2]] <- obs.selec.age.m
   Obs.Selex[[3]] <- obs.selec.age.f
   names(Obs.Selex) <- c("obs.selec","obs.selec.age.m","obs.selec.f")

#=======================================================================================================

 Bio <- list()
 Bio[[1]] <- Phi$phi.f
 Bio[[2]] <- Phi$phi.m
 Bio[[3]] <- Phi$mid.phi.m
 Bio[[4]] <- Phi$mid.phi.f
 Bio[[5]] <- Wght$mid.wght
 Bio[[6]] <- Wght$wght
 Bio[[7]] <- Wght$mid.wght.at.len
 Bio[[8]] <- Wght$wght.at.len
 Bio[[9]] <- Len$mid.len
 Bio[[10]]<- Wght$wght
 Bio[[11]]<- Len$len
 Bio[[12]]<- Fecund$fecund
 Bio[[13]]<- Phi$sigma.len
 Bio[[14]]<- Selex$selec
 Bio[[15]]<- Selex$selec.age.f
 Bio[[16]]<- Selex$selec.age.m
 Bio[[17]]<- Obs.Selex$obs.selec
 Bio[[18]]<- Obs.Selex$obs.selec.age.f
 Bio[[19]]<- Obs.Selex$obs.selec.age.m
 names(Bio) <- c("phi.f","phi.m","mid.phi.m","mid.phi.f","mid.wght","wght","mid.wght.at.len","wght.at.len",
                "mid.len","wght","len","fecund","sigma.len","selec","selec.age.f","selec.age.m","obs.selec",
                "obs.selec.age.f","obs.selec.age.m")
 return(Bio)
}

#Virgin Population Structure========================================================================
Virgin <- function(R0,fecundity)#,fecundity) 
{ 
 nnew<-matrix(NA,ages,sexes)
 nnew[1:(ages-1),1]<-(R0/2)*exp(-m.f*(0:(ages-2)))
 nnew[1:(ages-1),2]<-(R0/2)*exp(-m.m*(0:(ages-2)))
 nnew[ages,1] <- nnew[ages-1]*exp(-m.f)/(1-exp(-m.f)) 
 nnew[ages,2] <- nnew[ages-1]*exp(-m.m)/(1-exp(-m.m)) 
    
 #Virgin Biomass By Age  
 SSB0<-sum(nnew[,1]*fecundity$fecund)
 
 output <- NULL
 output$SSB0<-SSB0
 output$nnew<-nnew
 return(output) 
}

#Recruits Spawning biomass  Vulnerable biomas  ===========================================================
Update_Dynamics <- function(R0, f, catch=hist.catch, biology)
 {
 UpdateDyn <- list()
 #Calculate Virgin SSB based on R0
 Virgin.list <- Virgin(R0,fecundity=biology)#,fecundity = biology$fecund)
 SSB0<-Virgin.list$SSB0
 numbers[1,,] <- Virgin.list$nnew
 
 #Create storage matrices and vectors
 Ry<-matrix(0,total.yrs,1)
 Ry[1]<-R0/2   
 SSB<-matrix(0,total.yrs,1)
 SSB[1]<-SSB0 
 catch.at.age <- array(NA,dim=c(total.yrs,ages,sexes))  
 catch.at.len <- array(NA,dim=c(total.yrs,length(len.step),sexes))
 f.values <- rep(0,(total.yrs-1))
 catch.wght <- rep(0,(total.yrs-1)) 
 z.rate <- array(NA,dim=c(total.yrs,ages,sexes)) 

      
 for(y in 1:(total.yrs-1))
 {  

     z.rate[y,,1] <- m.f+biology$selec.age.f*f
     z.rate[y,,2] <- m.m+biology$selec.age.m*f
     z.m <- (1-exp(-(z.rate[y,,2])))/(z.rate[y,,2])
     z.f <- (1-exp(-(z.rate[y,,1])))/(z.rate[y,,1])
     #Catch at Age
     catch.at.age[y,,1] <- max(0,f*(numbers[y,,1]*biology$selec.age.f)*z.f)
     catch.at.age[y,,2] <- max(0,f*(numbers[y,,2]*biology$selec.age.m)*z.m)
     #Catch At Length
     mid.temp.f <- numbers[y,,1]*z.f
     mid.temp.m <- numbers[y,,2]*z.m
     catch.at.len[y,,1] <- ((biology$mid.phi.f*biology$selec[,1])%*%(mid.temp.f))
     catch.at.len[y,,2] <- ((biology$mid.phi.m*biology$selec[,2])%*%(mid.temp.m))
        
     #Catch in Weight by Sex, mid.wght (41X2) calculated in the GetWght() function  
     catch.wght <- max(0,f*(sum(biology$mid.wght.at.len[,1]*catch.at.len[y,,1])
                                +sum(biology$mid.wght.at.len[,2]*catch.at.len[y,,2])))    
      
      # survival at age by gender
      S.f <- exp(-(m.f + biology$selec.age.f * f))
      S.m <- exp(-(m.m + biology$selec.age.m * f))
      
      #Update the numbers and remove the catch by applying the solved for f value
      tmp.f <- numbers[y, 1:(ages-1), 1] * S.f[1:(ages-1)]
      tmp.f[tmp.f < 0] <- 0 
      tmp.m <- numbers[y, 1:(ages-1), 2] * S.m[1:(ages-1)]
      tmp.m[tmp.m < 0] <- 0
      numbers[y+1, 2:ages, 1] <- tmp.f
      numbers[y+1, 2:ages, 2] <- tmp.m
      numbers[y+1, ages, 1] <- max(0, numbers[y+1, ages, 1] + numbers[y, ages, 1] * exp(-m.f - biology$selec.age.f[ages] * f))
      numbers[y+1, ages, 2] <- max(0, numbers[y+1, ages, 2] + numbers[y, ages, 2] * exp(-m.m - biology$selec.age.m[ages] * f))
      
      SSB[y+1] <- max(0, sum(numbers[y+1, 2:ages, 1] * biology$fecund[2:ages]))

      #Expected (and then realized) recruitment
      if (SR == "BH") {
        Ry[y+1]<-(4*steep*(R0/2)*SSB[y+1])/(SSB0*(1-steep)+SSB[y+1]*(5*steep-1)) }
      if (SR == "Shep") {
        alpha   <- (5*steep*(0.2^c-1))/((SSB0/(R0/2))*(5*steep*0.2^c-1))
        beta    <- (1-5*steep)/((SSB0^c)*(5*steep*0.2^c-1))
        Ry[y+1] <- (alpha*SSB[y+1])/(1+beta*(SSB[y+1])^c)   }
      
      #Ry[y+1]<-Ry[y+1]*exp(-0.5*(sigmaR^2))*exp(autocorr[y+1])#recdevs[y+1])
      
      numbers[y+1,1,]<-Ry[y+1]
         
 } #closes yearly loop
 
 UpdateDyn[[1]] <- "NA"#f.values
 UpdateDyn[[2]] <- z.rate
 UpdateDyn[[3]] <- catch.wght
 UpdateDyn[[4]] <- catch.at.age
 UpdateDyn[[5]] <- catch.at.len
 UpdateDyn[[6]] <- numbers
 UpdateDyn[[7]] <- SSB
 UpdateDyn[[8]] <- Ry
 names(UpdateDyn) <- c("f.values","z.rate","catch.wght","catch.at.age","catch.at.len","numbers","SSB","Ry")
 return(UpdateDyn)
}

 
f<-uniroot(function(f)(Update_Dynamics(R0,f,catch=hist.catch, biology=Get.Biology())$SSB[total.yrs]/
                    Update_Dynamics(R0,f,catch=hist.catch, biology=Get.Biology())$SSB[1])-0.30,lower=0,upper=4,tol=0.0001) 

f<-uniroot(function(f)(Update_Dynamics(R0,f,catch=hist.catch, biology=Get.Biology())$SSB[total.yrs]/
                    Update_Dynamics(R0,f,catch=hist.catch, biology=Get.Biology())$SSB[1])-0.40,lower=0,upper=4,tol=0.0001) 



R0.vals <- 10000
steep = 0.85

#Loop over R0 Values
for (r in 1:length(R0.vals))
{
 R0 <- R0.vals[r] 
 
    #Loop over Catch Values
    for (l in 1:length(f.val))#Catch.Values))
    {
        #CatchTot<-Catch.Values[l]
        f <- f.val[l]

        # Create Virgin population
        dyn <- Update_Dynamics(R0,f,catch=hist.catch, biology=Get.Biology())
        virgin <- Virgin(R0,fecundity=Get.Biology())
        numbers[1,,] <- virgin$nnew
        #biomass[1,,] <- virgin$biomass
        SSB0 <- virgin$SSB0 
        SSB<-dyn$SSB
        Ry <- dyn$Ry
        catch.wght <- dyn$catch.wght
        SSB[1] <- SSB0
        
        SSB.fraction[r,,l] <-SSB[total.yrs,]/SSB[1,]
        SSB.store[r,,l] <- SSB
        Ry.store[r,,l] <- Ry#[save.period]
        catch.store[r,,l] <- catch.wght#[save.period[1]:save.period[length(save.period)-1]]
    }
    print(r)
}

plot(SSB.store[1,total.yrs,],catch.store[1,(total.yrs-1),],ylab="Yield",xlab="SSB",type="b")
plot(SSB.fraction[1,1,],catch.store[1,(total.yrs-1),], ylab = "Yiedl", xlab = "depl", type = "b")
#Calculate SSBmsy and Fmsy===============================================================================================
max.yield <- matrix(NA,length(R0.vals),1)
SSB.msy <- matrix(NA,length(R0.vals),1)
F.msy <- matrix(NA,length(R0.vals),1)

for (r in 1:length(R0.vals))
{
max.f <- which.max(catch.store[r,total.yrs-1,])
max.yield[r,] <- catch.store[r,total.yrs-1,max.f]
SSB.msy[r,] <- SSB.store[r,total.yrs,max.f]
F.msy[r,] <- f.val[max.f]
}
