#########################################################
##            Reads Report Files                       ##
##      and pulls out required quantities              ##
##         Written by:  Chantel Wetzel                 ##
##              Date: 4-18-2014                        ##
#########################################################


Rep_Summary<- function(rep.new, y)
{
  tot.yrs <- 1:y 

  ofl.yrs   <- (tot.yrs[length(tot.yrs)]):(tot.yrs[length(tot.yrs)] + ass.freq - 1)
  OFL       <- mapply(function(x) OFL = as.numeric(strsplit(rep.new[grep(paste("OFLCatch_",x,sep=""),rep.new)], " ")[[1]][3]), 
                      x = ofl.yrs)
  ACL       <- mapply(function(x) OFL = as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",x,sep=""),rep.new)], " ")[[1]][3]), 
                      x = ofl.yrs)
  ForeCatch <- mapply(function(x) ForeCatch = as.numeric(strsplit(rep.new[grep(paste("ForeCatch_",x,sep=""),rep.new)], " ")[[1]][3]), 
                      x = ofl.yrs)
  SB        <- mapply(function(x) SB = as.numeric(strsplit(rep.new[grep(paste("SPB_",x,sep=""),rep.new)]," ")[[1]][3]), 
                      x = tot.yrs)
  SB.virgin <- as.numeric(strsplit(rep.new[grep("SPB_Virgin",rep.new)]," ")[[1]][3])
  
  Recruits  <- mapply(function(x) TotBio = as.numeric(strsplit(rep.new[grep(paste(1, x,"TIME",sep=" "),rep.new)]," ")[[1]][8]),
                      x = 1:tot.yrs[length(tot.yrs)-1])

  FSPR      <- as.numeric(strsplit(rep.new[grep("Fstd_SPR",rep.new)], " ")[[1]][3])
  CrashPen  <- as.numeric(strsplit(rep.new[grep("TOTAL",rep.new)+8], " ")[[1]][2])
  R0        <- as.numeric(strsplit(rep.new[grep("R0",rep.new)], " ")[[1]][3])
  M.f       <- as.numeric(strsplit(rep.new[grep("NatM_p_1_Fem_GP",rep.new)], " ")[[1]][3])
  M.m       <- as.numeric(strsplit(rep.new[grep("NatM_p_1_Mal_GP",rep.new)], " ")[[1]][3])
  Lmin.f    <- as.numeric(strsplit(rep.new[grep("L_at_Amin_Fem_GP",rep.new)], " ")[[1]][3])
  Lmax.f    <- as.numeric(strsplit(rep.new[grep("L_at_Amax_Fem_GP",rep.new)], " ")[[1]][3])
  Lmin.m    <- as.numeric(strsplit(rep.new[grep("L_at_Amin_Mal_GP",rep.new)], " ")[[1]][3])
  Lmax.m    <- as.numeric(strsplit(rep.new[grep("L_at_Amax_Mal_GP",rep.new)], " ")[[1]][3])
  k.f       <- as.numeric(strsplit(rep.new[grep("VonBert_K_Fem_GP",rep.new)], " ")[[1]][3])
  k.m       <- as.numeric(strsplit(rep.new[grep("VonBert_K_Mal_GP",rep.new)], " ")[[1]][3])
  cv.young.f<- as.numeric(strsplit(rep.new[grep("CV_young",rep.new)], " ")[[1]][3])
  cv.old.f  <- as.numeric(strsplit(rep.new[grep("CV_old",rep.new)], " ")[[1]][3 ])
  cv.young.m<- as.numeric(strsplit(rep.new[grep("CV_young_Mal",rep.new)], " ")[[1]][3])
  cv.old.m  <- as.numeric(strsplit(rep.new[grep("CV_old_Mal",rep.new)], " ")[[1]][3 ])

  # Females
  if (f.fleets > 1){
    f.selex.1 <- c(as.numeric(strsplit(rep.new[grep("SizeSel_1P_1_Fishery1",rep.new)], " ")[[1]][3]),
                   as.numeric(strsplit(rep.new[grep("SizeSel_2P_1_Fishery2",rep.new)], " ")[[1]][3]) )
    f.selex.2 <- c(as.numeric(strsplit(rep.new[grep("SizeSel_1P_2_Fishery1",rep.new)], " ")[[1]][3]),
                   as.numeric(strsplit(rep.new[grep("SizeSel_2P_2_Fishery2",rep.new)], " ")[[1]][3]))
    f.selex.3 <- c(as.numeric(strsplit(rep.new[grep("SizeSel_1P_3_Fishery1",rep.new)], " ")[[1]][3]),
                   as.numeric(strsplit(rep.new[grep("SizeSel_2P_3_Fishery2",rep.new)], " ")[[1]][3]) )
    f.selex.4 <- c(as.numeric(strsplit(rep.new[grep("SizeSel_1P_4_Fishery1",rep.new)], " ")[[1]][3]),
                   as.numeric(strsplit(rep.new[grep("SizeSel_2P_4_Fishery2",rep.new)], " ")[[1]][3]))
    f.selex.5 <- c(as.numeric(strsplit(rep.new[grep("SizeSel_1P_5_Fishery1",rep.new)], " ")[[1]][3]),
                   as.numeric(strsplit(rep.new[grep("SizeSel_2P_5_Fishery2",rep.new)], " ")[[1]][3]))
    f.selex.6 <- c(as.numeric(strsplit(rep.new[grep("SizeSel_1P_6_Fishery1",rep.new)], " ")[[1]][3]),
                   as.numeric(strsplit(rep.new[grep("SizeSel_2P_6_Fishery2",rep.new)], " ")[[1]][3]))
  }

  # Males
  if (f.fleets > 1){
    f.selex.m.1 <- c(as.numeric(strsplit(rep.new[grep("SzSel_1Male_Peak_Fishery1",rep.new)], " ")[[1]][3]),
                     as.numeric(strsplit(rep.new[grep("SzSel_2Male_Peak_Fishery2",rep.new)], " ")[[1]][3]) )
    f.selex.m.3 <- c(as.numeric(strsplit(rep.new[grep("SzSel_1Male_Ascend_Fishery1",rep.new)], " ")[[1]][3]),
                     as.numeric(strsplit(rep.new[grep("SzSel_2Male_Ascend_Fishery2",rep.new)], " ")[[1]][3]))
    f.selex.m.4 <- c(as.numeric(strsplit(rep.new[grep("SzSel_1Male_Descend_Fishery1",rep.new)], " ")[[1]][3]),
                     as.numeric(strsplit(rep.new[grep("SzSel_2Male_Descend_Fishery2",rep.new)], " ")[[1]][3]) )    
    f.selex.m.6 <- c(as.numeric(strsplit(rep.new[grep("SzSel_1Male_Final_Fishery1",rep.new)], " ")[[1]][3]),
                     as.numeric(strsplit(rep.new[grep("SzSel_2Male_Final_Fishery2",rep.new)], " ")[[1]][3]))
    f.selex.m.2 <- c("NA", "NA")
    f.selex.m.5 <- c("NA", "NA")
  }
  
  s.selex.1 <- as.numeric(strsplit(rep.new[grep("SizeSel_3P_1_Survey",rep.new)], " ")[[1]][3])
  s.selex.2 <- as.numeric(strsplit(rep.new[grep("SizeSel_3P_2_Survey",rep.new)], " ")[[1]][3])
  s.selex.3 <- as.numeric(strsplit(rep.new[grep("SizeSel_3P_3_Survey",rep.new)], " ")[[1]][3])
  s.selex.4 <- as.numeric(strsplit(rep.new[grep("SizeSel_3P_4_Survey",rep.new)], " ")[[1]][3])
  s.selex.5 <- as.numeric(strsplit(rep.new[grep("SizeSel_3P_5_Survey",rep.new)], " ")[[1]][3])
  s.selex.6 <- as.numeric(strsplit(rep.new[grep("SizeSel_3P_6_Survey",rep.new)], " ")[[1]][3])

  s.selex.m.1 <- as.numeric(strsplit(rep.new[grep("SzSel_3Male_Peak_Survey",rep.new)], " ")[[1]][3])
  s.selex.m.3 <- as.numeric(strsplit(rep.new[grep("SzSel_3Male_Ascend_Survey",rep.new)], " ")[[1]][3])
  s.selex.m.4 <- as.numeric(strsplit(rep.new[grep("SzSel_3Male_Descend_Survey",rep.new)], " ")[[1]][3])
  s.selex.m.6 <- as.numeric(strsplit(rep.new[grep("SzSel_3Male_Final_Survey",rep.new)], " ")[[1]][3])
  s.selex.m.2 <- "NA"
  s.selex.m.5 <- "NA"
  
  Depl = SB/SB.virgin
  
  RepSummary           <- list()
  RepSummary$SB        <- SB
  RepSummary$SB.virgin <- SB.virgin
  RepSummary$OFL       <- OFL
  RepSummary$ACL       <- ACL
  RepSummary$ForeCatch <- ForeCatch
  RepSummary$Depl      <- Depl
  RepSummary$FSPR      <- FSPR
  RepSummary$f.selex   <- rbind(f.selex.1, 
                                f.selex.2, 
                                f.selex.3, 
                                f.selex.4, 
                                f.selex.5, 
                                f.selex.6)
  RepSummary$f.selex.m <- rbind(f.selex.m.1, 
                                f.selex.m.2, 
                                f.selex.m.3, 
                                f.selex.m.4, 
                                f.selex.m.5, 
                                f.selex.m.6)
  RepSummary$R0        <- R0
  RepSummary$s.selex   <- c(s.selex.1, 
                            s.selex.2, 
                            s.selex.3, 
                            s.selex.4, 
                            s.selex.5, 
                            s.selex.6)
  RepSummary$s.selex.m <- c(s.selex.m.1, 
                            s.selex.m.2, 
                            s.selex.m.3, 
                            s.selex.m.4, 
                            s.selex.m.5, 
                            s.selex.m.6)
  RepSummary$M          <- cbind(M.f,M.m)
  RepSummary$Recruits   <- Recruits
  RepSummary$Lmin       <- cbind(Lmin.f, Lmin.m)
  RepSummary$Lmax       <- cbind(Lmax.f, Lmax.m)
  RepSummary$k          <- cbind(k.f, k.m)
  RepSummary$cv.young   <- cbind(cv.young.f, cv.young.m)
  RepSummary$cv.old     <- cbind(cv.old.f, cv.old.f)
  
  return(RepSummary)
}
