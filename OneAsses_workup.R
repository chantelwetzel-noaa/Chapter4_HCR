
drive = "C:"
run.name = "OneAss_noAE_fixedM"#smallN_noAE/"#"April16_PreCPUE/"#"CPUE_smallN_AE/"#"Ass_Freq/"#"April16_PreCPUE/"
#run.name = "OneAss_AE_hiageN"
#run.name = "OneAss_AE_sage50"
AE = FALSE
LH = "rockfish"
ds.list = c("ds1","ds2","ds4", "ds3", "ds6", "ds5") 
ds.list = c("ds1","ds4","ds6", "ds2", "ds3", "ds5")
ds.list = c("ds1", "ds2") 
#ds.list = c("ds1", "ds4", "ds6", "ds8")
#ds.list = c("full", "reduced", "eliminated", "tv_full", "tv_reduced", "tv_eliminated")

sim.range = c(1, 100) #c(1, 100)
max.sim = 100
if (max.sim != sim.range[2]){print ("Only working up a subset")}
order = c(1,2,3,4,5,6) 
data.scenario = ""
setup.yrs = 50

set.quant = c(0.10, 0.50, 0.90)


#Dimensions by life-history
git.wd = "C:/Users/Chantell.Wetzel/Documents/GitHub/Ch3_DataLoss/"
source(paste(git.wd, "/functions/LH_parameter_values.R", sep=""))
source(paste(git.wd, "/functions/Data_Scenarios.R", sep=""))

if (file.exists( file = paste(drive,"/PhD/Chapter3/", run.name, "/output", sep = "") ) == FALSE ){
  dir.create(paste0(drive,"/PhD/Chapter3/", run.name, "/output"))
}

set.ass.freq = ass.freq = 4
first.ass.yr <- total.yrs - project.yrs - 1
end.catch = first.ass.yr + 25
ass.yr = seq(first.ass.yr, total.yrs, ass.freq)
new.ass.num = length(ass.yr)
ass.num = new.ass.num

med.ssb         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ry          = array(0, dim = c(length(ds.list), total.yrs, 3))
med.depl        = array(0, dim = c(length(ds.list), total.yrs, 3))
med.catch       = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ofl         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.acl         = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ssb.est     = array(0, dim = c(length(ds.list), total.yrs, ass.num, 3))
med.ry.est      = array(0, dim = c(length(ds.list), total.yrs, ass.num, 3))
med.depl.est    = array(0, dim = c(length(ds.list), total.yrs, ass.num, 3))
med.acl.est     = array(0, dim = c(length(ds.list), total.yrs, 3))
med.ofl.est     = array(0, dim = c(length(ds.list), total.yrs, 3))
med.m.est       = array(0, dim = c(length(ds.list), ass.num, 3))
med.s.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
med.f.selex.est = array(0, dim = c(length(ds.list), 2, ass.num, 3))
m.est.all       = array(0, dim = c(length(ds.list), ass.num, max.sim))
s.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, max.sim))
f.selex.est.all = array(0, dim = c(length(ds.list), 6, ass.num, max.sim))
re.depl         = array(NA,dim = c(length(ds.list), ass.num, total.yrs, max.sim))
re.ssb          = array(NA,dim = c(length(ds.list), ass.num, total.yrs, max.sim))
re.ssb0         = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.m            = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.h            = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.k            = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.lmin         = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.lmax         = array(NA,dim = c(length(ds.list), ass.num, max.sim))
re.catch        = array(NA,dim = c(length(ds.list), project.yrs + 2 , max.sim))
re.ofl          = array(NA,dim = c(length(ds.list), project.yrs + 2 , max.sim))
re.s.selex      = array(0, dim = c(length(ds.list), 2, ass.num, max.sim))
re.f.selex      = array(0, dim = c(length(ds.list), 4, ass.num, max.sim))
re.f.selex.adj  = array(0, dim = c(length(ds.list), 1, ass.num, max.sim))
acl.min         = array(NA,dim = c(length(ds.list), total.yrs, max.sim))
rmse.sb0        = array(NA,dim = c(length(ds.list), ass.num))
rmse.depl       = array(NA,dim = c(length(ds.list), ass.num))
rmse.catch      = array(NA,dim = c(length(ds.list), ass.num))
rmse.ssb.ass    = array(NA,dim = c(length(ds.list), ass.num))

re.time.over          = matrix(0, length(ds.list), max.sim)
catch.median          = matrix(0, length(ds.list), max.sim)

#==============================================================================================
failed.to.detect.over.all <- array(0, dim = c(length(ds.list), ass.num))
failed.to.detect.rec.all  <- array(0, dim = c(length(ds.list), ass.num))
incorrect.rebuild.all     <- array(0, dim = c(length(ds.list), ass.num))

yrs.declared.rec.early.all<- array(0, dim = c(length(ds.list), max.sim))
yrs.declared.rec.late.all <- array(0, dim = c(length(ds.list), max.sim))
yrs.declared.all          <- array(0, dim = c(length(ds.list), max.sim))

aav                       <- array(0, dim = c(length(ds.list), max.sim))

#operating model values storage arrays ============================================================
ssb   = array(0, dim = c(length(ds.list), total.yrs, max.sim))
ry    = array(0, dim = c(length(ds.list), total.yrs, max.sim))
depl  = array(0, dim = c(length(ds.list), total.yrs, max.sim))
catch = array(0, dim = c(length(ds.list), total.yrs, max.sim))
ofl   = array(0, dim = c(length(ds.list), total.yrs, max.sim))
acl   = array(0, dim = c(length(ds.list), total.yrs, max.sim))
f.lens= array(0, dim = c(length(ds.list), total.yrs, max.sim))
s.lens= array(0, dim = c(length(ds.list), total.yrs, max.sim))
f.ages= array(0, dim = c(length(ds.list), total.yrs, max.sim))
s.ages= array(0, dim = c(length(ds.list), total.yrs, max.sim))
peak  = array(0, dim=  c(length(ds.list), total.yrs, max.sim))
dome  = array(0, dim=  c(length(ds.list), total.yrs, max.sim))
recovery.yr = array(0, dim=  c(length(ds.list), 1,   max.sim))
om.time.over= matrix(0, length(ds.list), max.sim)

#estimation values storage arrays  ================================================================
ssb.est     =  array(0, dim = c(length(ds.list), total.yrs, ass.num, max.sim))
ssb0.est    =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
ry.est      =  array(0, dim = c(length(ds.list), total.yrs, ass.num, max.sim))
depl.est    =  array(0, dim = c(length(ds.list), total.yrs, ass.num, max.sim))
acl.est     =  array(0, dim = c(length(ds.list), total.yrs,          max.sim))
ofl.est     =  array(0, dim = c(length(ds.list), total.yrs,          max.sim))
m.est       =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
h.est       =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
k.est       =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
lmin.est    =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
lmax.est    =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
cv.young.est=  array(0, dim = c(length(ds.list),            ass.num, max.sim))
cv.old.est  =  array(0, dim = c(length(ds.list),            ass.num, max.sim))
s.selex.est =  array(0, dim = c(length(ds.list), 6,         ass.num, max.sim))
f.selex.est =  array(0, dim = c(length(ds.list), 6,         ass.num, max.sim))
f.selex.adj.est =  array(0, dim = c(length(ds.list), 1,     ass.num, max.sim))

ave.over.catch  = matrix(0, length(ds.list), max.sim)
over.catch      = matrix(0, length(ds.list), max.sim)
yr.decl.est     = array(0, dim = c(length(ds.list), 3, max.sim))
yr.recr.est     = array(0, dim = c(length(ds.list), 3, max.sim))


n.overfished = matrix(0, length(ds.list), ass.num)
om.n.overfished = matrix(0, length(ds.list), ass.num)
n.overfished.split = matrix(0, length(ds.list), ass.num)
time.over    = matrix(0, length(ds.list), max.sim)

filt.re.time.over = list()

for (spec in 1:length(ds.list))
{
  j = order[spec]
  data.scenario = ds.list[j]

  #dir = paste(drive,"/PhD/Chapter3/", run.name ,LH,"_",data.scenario,"_multinom_boot_AE_TRUE_",
  #      sim.range[1],"_",sim.range[2],
  #      "/save/", sep = "")
  dir = paste(drive,"/PhD/Chapter3/", run.name ,"/",LH,"_",data.scenario,"_multinom_boot_AE_",AE,"_",
        sim.range[1],"_", sim.range[2],
        "/save/", sep = "")
  
  for (i in sim.range[1]:max.sim) {
      dat = paste(dir, "om_proj_",i,sep ="")
      load(dat)
      ssb[j,,i]   = Proj$SSB[1:total.yrs]
      ry[j,,i]    = Proj$Ry[1:total.yrs]*2
      depl[j,,i]  = Proj$Depl[1:total.yrs]
      acl[j,,i]   = Proj$acl[1:total.yrs]
      ofl[j,,i]   = Proj$ofl.true[1:total.yrs]
      f.lens[j,,i]= Proj$f.len.samp
      s.lens[j,,i]= Proj$s.len.sam
      f.ages[j,,i]= Proj$f.age.samp
      s.ages[j,,i]= Proj$s.age.samp
      dome[j,,i]  = Proj$dome
      peak[j,,i]  = Proj$peak
      ind = Proj$recovered.om > 0
      recovery.yr[j,,i] = ifelse(sum(ind) != 0, Proj$recovered.om[ind], 0)
      om.time.over[j,i] = max(Proj$recovered.om) - first.ass.yr
      if(om.time.over[j,i] < 0) { om.time.over[j,i] = ifelse(LH == "rockfish", 101, 51) }
  }

  #Save as an output file 
  om.all <- list()
  om.out <- paste(drive,"/PhD/Chapter3/", run.name, "/output/",LH,"_om_all", sep = "")
  om.all$ssb  <- ssb
  om.all$ry   <- ry
  om.all$depl <- depl
  om.all$catch<- catch
  om.all$ofl  <- ofl
  om.all$acl  <- acl
  om.all$f.lens   <- f.lens
  om.all$s.lens   <- s.lens
  om.all$f.ages   <- f.ages
  om.all$s.ages   <- s.ages
  om.all$peak     <- peak
  om.all$dome     <- dome
  om.all$om.time.over <- om.time.over
  save (om.all, file = om.out)

  #save.index = save.index.ov = 1:sim.range[2]
  
  for (i in sim.range[1]:max.sim){
    est = paste(dir,"ss_ests_",i,sep="")
    load(est)
  
    ssb.est[j,,,i]     = Est$SB[,1:ass.num]
    ry.est[j,,,i]      = Est$Recruit[,1:ass.num]
    depl.est[j,,,i]    = Est$Bratio[,1:ass.num]
    acl.est[j,,i]      = Est$ACL[1:total.yrs]
    ofl.est[j,,i]      = Est$OFL[1:total.yrs]
    m.est[j,,i]        = Est$M.store[1,1:ass.num]
    h.est[j,,i]        = Est$h[1,1:ass.num]
    k.est[j,,i]        = Est$k.store[1,1:ass.num]
    lmin.est[j,,i]     = Est$Lmin.store[1,1:ass.num]
    lmax.est[j,,i]     = Est$Lmax.store[1,1:ass.num]
    s.selex.est[j,,,i] = Est$S.selex[,1:ass.num]
    f.selex.est[j,,,i] = Est$F.selex[,1:ass.num]
    f.selex.adj.est[j,,,i] = Est$fsp2.est[,1:ass.num]
    ssb0.est[j,,i]     = Est$SB[1,1:ass.num]/Est$Bratio[1,1:ass.num]
    ind = Est$recovered.est > 0 
    index = Est$recovered.est[ind]
    yr.decl.est[j,,i]  = c(index[1], ifelse(length(index)> 2, index[3],0), ifelse(length(index)> 2, index[5],0))
    yr.recr.est[j,,i]  = c(index[2], ifelse(length(index)> 2, index[4],0), ifelse(length(index)> 2, index[6],0))

    #if (sum(Est$recovered.est) != 0){
    #  ind = Est$recovered.est>0
    #  values = unique(sort(Est$recovered.est[ind]))
    #  time.over[j,i] = values[2] -values[1] + 1
    #}
    #if (length(values) == 1) { time.over[j,i] = -101 }
#
    #ind = ifelse(is.na(yr.recr.est[j,1,i]), first.ass.yr + 100, yr.recr.est[j,1,i])
    #over.catch[j,i] = sum(Est$ACL[first.ass.yr:ind])
    #ave.over.catch[j,i] = over.catch[j,i]/ind
  } 

  #Save as an output file 
  est.all <- list()
  est.out <- paste(drive,"/PhD/Chapter3/", run.name,"/output/",LH,"_est_all", sep = "")
  est.all$ssb.est  <- ssb.est
  est.all$ry.est   <- ry.est
  est.all$depl.est <- depl.est
  est.all$acl.est  <- acl.est
  est.all$ofl.est  <- ofl.est
  est.all$m.est    <- m.est
  est.all$h.est    <- h.est
  est.all$k.est    <- k.est
  est.all$lmin.est <- lmin.est
  est.all$lmax.est <- lmax.est
  est.all$s.selex.est <- s.selex.est
  est.all$f.selex.est <- f.selex.est
  est.all$f.selex.adj.est <- f.selex.adj.est
  est.all$ssb0.est    <- ssb0.est
  est.all$yr.decl.est <- yr.decl.est
  est.all$yr.recr.est <- yr.recr.est
  est.all$time.over   <- time.over
  est.all$over.catch  <- over.catch
  est.all$ave.over.catch <- ave.over.catch
  save(est.all, file = est.out)

    for (a in 1:new.ass.num){
    re.depl[j,a,,] <- (depl.est[j,,a,] - depl[j,1:total.yrs,]) / depl[j,1:total.yrs,]
    re.ssb[j,a,,]  <- (ssb.est[j,,a,]  - ssb[j, 1:total.yrs,]) / ssb[j, 1:total.yrs,]
    re.ssb0[j,a,]  <- (ssb0.est[j,a,]  - ssb[j,1,])/ ssb[j,1,]
    re.m[j,a,]     <- (m.est[j,a,] - m) / m
    re.h[j,a,]     <- (h.est[j,a,] - steep) / steep
    re.k[j,a,]     <- (k.est[j,a,] - kf) / kf
    re.lmin[j,a,]  <- (lmin.est[j,a,] - L1) / L1
    re.lmax[j,a,]  <- (lmax.est[j,a,] - L2f) / L2f
  }
}

wd = paste0(drive, "/PhD/Chapter3/", run.name, "/",run.name,"_plots.pdf")
pdf(file = wd)
   ymin = -1; ymax = 1
   par(mfrow = c(2,2))
   boxplot(cbind(re.ssb[1,,120,], re.ssb[2,,120,]), ylim = c(ymin,ymax), ylab = "RE SSB")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))

   boxplot(cbind(re.depl[1,,120,], re.depl[2,,120,]),  ylim = c(ymin,ymax), ylab = "RE DEPL")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))

   boxplot(cbind(re.ssb0[1,,], re.ssb0[2,,]),  ylim = c(ymin,ymax), ylab = "RE SSB0")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))

   par(mfrow = c(2,2))
   boxplot(cbind(re.h[1,,], re.h[2,,]),  ylim = c(ymin,ymax), ylab = "RE h")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))

   boxplot(cbind(re.k[1,,], re.k[2,,]),  ylim = c(ymin,ymax), ylab = "RE k")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))

   boxplot(cbind(re.lmin[1,,], re.lmin[2,,]),  ylim = c(ymin,ymax), ylab = "RE lmin")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))

   boxplot(cbind(re.lmax[1,,], re.lmax[2,,]),  ylim = c(ymin,ymax), ylab = "RE lmax")
   abline(h = 0)
   legend('topleft', bty = 'n', legend = c("Time-invariant"))
   legend('topright', bty = 'n', legend = c("Time-Varying"))
dev.off()
