writeCtl <- function (ctl,y) 
{   
    fishery.selec.mat <- matrix(c(    
    25,     60,  fsp[1,1,1],  fsp[1,1,1],   -1,     5,   1,    0,   0,           0,          0,          0.2,        0,        0,   "#Peak",
    -5,     5,   fsp[1,1,2],  fsp[1,1,2],   -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Top (Width)",                            
    -10,    10,  fsp[1,1,3],  fsp[1,1,3],   -1,     5,   2,    0,   0,           0,          0,          0.5,        0,        0,   "#Asc_Width" ,                         
    -2,     20,  fsp[1,1,4],  fsp[1,1,4],   -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Desc_Width",                         
    -999,  100,  -999,        -999,         -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Init",                           
    -999,  100,  -999,        -999,         -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Final",
    #Male selectivity 
    -15,    15,  fsp[1,2,1] - fsp[1,1,1],  fsp[1,2,1] - fsp[1,1,1],   -1,     5,   4,    0,   0,           0,          0,         0.5,        0,        0,   "#Peak",                         
    -15,    15,  fsp[1,2,3] - fsp[1,1,3],  fsp[1,2,3] - fsp[1,1,3],   -1,     5,   5,    0,   0,           0,          0,         0.5,        0,        0,   "#Asc_Width" ,                         
    -15,    15,  fsp[1,2,4] - fsp[1,1,4],  fsp[1,2,4] - fsp[1,1,4],   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Desc_Width",                                                
    -15,    15,  fsp[1,2,6] - fsp[1,1,6],  fsp[1,2,6] - fsp[1,1,6],   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Final", 
    -15,    15,                        1,                        0,   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Scale", 

    #Fleet 2
    25,     60,  fsp[2,1,1],  fsp[2,1,1],   -1,     5,   1,    0,   0,           0,          0,          0.2,        0,        0,   "#Peak",
    -5,     5,   fsp[2,1,2],  fsp[2,1,2],   -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Top (Width)",                            
    -10,    10,  fsp[2,1,3],  fsp[2,1,3],   -1,     5,   2,    0,   0,           0,          0,          0.5,        0,        0,   "#Asc_Width" ,                         
    -2,     20,  fsp[2,1,4],  fsp[2,1,4],   -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Desc_Width",                         
    -999,  100,  -999,        -999,         -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Init",                           
    -999,  100,  -999,        -999,         -1,     5,  -9,    0,   0,           0,          0,          0.5,        0,        0,   "#Final",
    #Male selectivity 
    -15,    15,  fsp[2,2,1] - fsp[2,1,1],  fsp[2,2,1] - fsp[2,1,1],   -1,     5,   4,    0,   0,           0,          0,         0.5,        0,        0,   "#Peak",                         
    -15,    15,  fsp[2,2,3] - fsp[2,1,3],  fsp[2,2,3] - fsp[2,1,3],   -1,     5,   5,    0,   0,           0,          0,         0.5,        0,        0,   "#Asc_Width" ,                         
    -15,    15,  fsp[2,2,4] - fsp[2,1,4],  fsp[2,2,4] - fsp[2,1,4],   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Desc_Width",                                                
    -15,    15,  fsp[2,2,6] - fsp[2,1,6],  fsp[2,2,6] - fsp[2,1,6],   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Final", 
    -15,    15,                        1,                        0,   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Scale"), 
    ncol=15, byrow=T) 
    
    # Survey    
    survey.selec.mat <- matrix(c(                  
    25,     60,  ssp[1,1],  ssp[1,1],   -1,     5,   1,   0,   0,           0,          0,          0.5,        0,        0,   "#Peak",                           
    -5,     5,   ssp[1,2],  ssp[1,2],   -1,     5,  -9,   0,   0,           0,          0,          0.5,        0,        0,   "#Top (Width)",                            
    -10,    10,  ssp[1,3],  ssp[1,3],   -1,     5,   1,   0,   0,           0,          0,          0.5,        0,        0,   "#Asc_Width",                          
    -2,     20,  ssp[1,4],  ssp[1,4],   -1,     5,  -9,   0,   0,           0,          0,          0.5,        0,        0,   "#Desc_Width",                         
    -999,  100,  -999,          -999,   -1,     5,  -9,   0,   0,           0,          0,          0.5,        0,        0,   "#Init",                           
    -999,  100,  -999,          -999,   -1,     5,  -9,   0,   0,           0,          0,          0.5,        0,        0,   "#Final",
    #Male selectivity 
    -15,   15,  ssp[2,1] - ssp[1,1],  ssp[2,1] - ssp[1,1],   -1,     5,   4,    0,   0,           0,          0,         0.5,        0,        0,   "#Peak",                         
    -15,   15,  ssp[2,3] - ssp[1,3],  ssp[2,3] - ssp[1,3],   -1,     5,   5,    0,   0,           0,          0,         0.5,        0,        0,   "#Asc_Width" ,                         
    -15,   15,  ssp[2,4] - ssp[1,4],  ssp[2,4] - ssp[1,4],   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Desc_Width",                                                
    -15,   15,  ssp[2,6] - ssp[1,6],  ssp[2,6] - ssp[1,6],   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Final", 
    -15,   15,                    1,                    0,   -1,     5,  -9,    0,   0,           0,          0,         0.5,        0,        0,   "#Scale"),     
    ncol=15, byrow=T) 

    ageselec.mat<- matrix(c(
    #_LO    HI        INIT    PRIOR         R_type   SD       PHASE       env-var use_dev dev_minyr   dev_maxyr   dev_stddev  Block   Block_Fxn    
    0,      0,         0,     0,            0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MIN AGE", 
    0,      ages+10,   ages,  ages,         0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MAX AGE",
    0,      0,         0,     0,            0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MIN AGE", 
    0,      ages+10,   ages,  ages,         0,       50,      -3,        c(rep(0,4), 0.5, 0, 0), "#MAX AGE"),
    byrow=T, ncol=15)

    bio.mat <-matrix(c( 
    #_LO    HI          INIT          PRIOR                 PR_type    SD    PHASE      env-var use_dev dev_minyr   dev_maxyr   dev_stddev  Block   Block_Fxn
    0.01,   0.6,        m.f.mean,     round(log(m.f.mean),3),3,        .20,      1,       c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Fem_GP_1",
    3,      35,         L1f.mean,     L1f.mean,             -1,         10,      3,       c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Fem_GP_1_",
    45,     70,         L2f.mean,     L2f.mean,             -1,         10,      3,       c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Fem_GP_1_",
    0.00,   0.3,        kf.mean,      kf.mean,              -1,        .25,      2,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Fem_GP_1_",
    0.01,   0.2,        CV1,          CV1,                  -1,        .20,     -4,       c(rep(0,4), 0.5, 0,0), "#CV_young_Fem_GP_1_",
    0.01,   0.1,        CV2,          CV2,                  -1,        .20,     -4,       c(rep(0,4), 0.5, 0,0), "#CV_old_Fem_GP_1_",
    0.01,   0.60,       m.m.mean,     round(log(m.m.mean),3),3,        .20,      1,       c(rep(0,4), 0.5, 0,0), "#NatM_p_1_Mal_GP_1",
    3,      35,         L1m.mean,     L1m.mean,             -1,         99,      3,       c(rep(0,4), 0.5, 0,0), "#L_at_Amin_Mal_GP_1_",
    35,     70,         L2m.mean,     L2m.mean,             -1,         99,      3,       c(rep(0,4), 0.5, 0,0), "#L_at_Amax_Mal_GP_1_",
    0.01,   0.3,        km.mean,      km.mean,              -1,         99,      2,       c(rep(0,4), 0.5, 0,0), "#VonBert_K_Mal_GP_1_",
    0.01,   0.2,        CV1,          CV1,                  -1,         99,     -4,       c(rep(0,4), 0.5, 0,0), "#CV_young_Mal_GP_1_",
    0.01,   0.1,        CV2,          CV2,                  -1,         99,     -4,       c(rep(0,4), 0.5, 0,0), "#CV_old_Mal_GP_1_",
    0,      0.1,        wght.coef.f,  wght.coef.f,          -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Wtlen1_Fem",
    0,      4,          wght.exp.f,   wght.exp.f,           -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Wtlen2_Fem",
    20,     40,         ohm4,         ohm4,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0,0), "# Mat50_Fem",
    -3,     3,          ohm3,         ohm3,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "#Mat_slope_Fem",
    0,      3,          ohm5,         ohm5,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Eggs1_Fem",
    0,      3,          ohm6,         ohm6,                 -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Eggs2_Fem",
    0,      0.1,        wght.coef.m,  wght.coef.m,          -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# Wtlen1_Mal",
    0,      4,          wght.exp.m,   wght.exp.m,           -1,         99,    -99,       c(rep(0,4), 0.5, 0,0),  "# Wtlen2_Mal",
    0,      1,          1,            1,                    -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_GP_1_",
    0,      1,          1,            1,                    -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_Area_1_",
    0,      1,          1,            1,                    -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# RecrDist_Seas_1_",
    0,      1,          0,            0,                    -1,         99,    -99,       c(rep(0,4), 0.5, 0, 0), "# CohortGrowDev"), 
    ncol=15, byrow=T)

    sigma.set = ifelse(sigmaR == 0, 0.01, sigmaR)
    rec.mat <- matrix(c(
    #_LO    HI     INIT                 PRIOR       PR_type      SD       PHASE
    2,      15,    round(log(R0),3),    round(log(R0),3),    -1,         10,      1, "# log(R0)",  
    0.20,   1,     steep.set,           steep.set,    1,       0.09,     -2, "# SR_steep ",
    0,      1.5,   sigmaR.set,          sigmaR.set, -1,         99,     -99,"#SR_sigmaR",
    -5,     5,     0,                   0,          -1,         99,     -99,"# SR_envlink",
    -5,     5,     0,                   0,          -1,         99,     -99,"# SR_R1_offset",
    0,   0.99,     0,                   0,          -1,         99,     -99,"# SR_autocorr"), 
    ncol=8, byrow=T)
                
    
    cat("#Stock Synthesis\n",
    "#\n",
    1,          " #Number of Growth Morphs\n",
    1,          " #Number of Sub-Morphs\n",
    file =ctl, sep = " ")

    cat(
    0,          " #Number of Block Patterns\n",
    file = ctl, append = T, sep = " ")
    
    cat(
    0.5,        " #Fraction Female\n",
    0,          " #Natural Mortality Method\n",
    1,          " #Growth Method\n",
    a3,         " #Growth Amin\n",
    a4,         " #Growth Amax\n",
    0,          " #SD add to LAA\n",
    0,          " #CV pattern (CV=f(LAA))\n",
    1,          " #Maturity Method (1=length logistic)\n",
    mat.age,    " #First Mature Age\n",
    1,          " #Fecundity Option (fecundity=wt*(a+b*wt) with a=1 and b=0)\n",
    0,          " #Hermaphroditism option\n",
    1,          " #Offset Method (Direct Assignment, 2 = Males are an offset of females)\n",
    1,          " #Time-varying adjustment\n",
    file=ctl,append = T, sep=" ")
    
    cat("#Maturity & Growth Parameters\n",
    "#LO   HI   INIT   PRIOR   PRIOR_TYPE   SD   PHASE   ENV\n",
    file=ctl, append=T, sep="")
        
    write.table(bio.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
   
    cat("#Seasonal Effects\n",
    rep(0,10), " #\n",
    "#Recruitment\n",
    3, " #Recruitment Method, 3= standard BH\n",
    "#LO   HI   INIT   PRIOR   PRIOR_TYPE   SD   PHASE\n",
    file=ctl, append=T)

    write.table(rec.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    if (determ == TRUE){
        cat(0, " #SR env link\n", 
        0,     " #SR env target\n",
        0,     " #Do rec dev (0=none, 1=devvector, 2= simple dev)\n",
        1,     " #main recr dev begin yr\n",
        y,     " #main recr devs end yr\n",
        -2,    " #main recr dev phas\n",
        0,     " #advanced options (0=default values)ALL SET AT DEFAULT VALUES\n",   
        file=ctl,append=T)  
    }

    if (determ == FALSE){
        cat(0, " #SR env link\n", 
        0,                      " #SR env target\n",
        1,                      " #Do rec dev (0=none, 1=devvector, 2= simple dev)\n",
        main.rec.start,         " #main recr dev begin yr\n",
        main.rec.end,           " #main recr devs end yr\n",
        3,                      " #main recr dev phas\n",
        1,                      " #advanced options (0=default values)ALL SET AT DEFAULT VALUES\n",                                                 
        start.devs,         " #_recdev_early_start    (0=none;    neg value   makes   relative    to  recdev_start)\n",    
        pre.dev.phase,      " #_recdev_early_phase\n",                                                                                
        0,                  " #_forecast_recruitment  phase   (incl.  late    recr)   (0  value   resets  to  maxphase+1)\n",                                         
        1000,               " #_lambda    for prior_fore_recr occurring   before  endyr+1\n",                                                         
        start.bias,         " #_last_early_yr_nobias_adj_in_MPD\n",                                                                               
        full.bias,          " #_first_yr_fullbias_adj_in_MPD\n",                                                                              
        last.bias,          " #_last_yr_fullbias_adj_in_MPD\n",                                                                               
        last.no.bias,       " #_first_recent_yr_nobias_adj_in_MPD\n",                                                                             
        max.bias.adj,       " #_max_bias_adj_in_MPD\n",                                                                               
        0,                  " #period of cyle in recruitment\n",                                                             
        -15,                 " #min    rec_dev \n",                                                                        
        15,                  " #max    rec_dev \n",  
        0,             " #_read_recdevs\n",
        " #end of advanced options\n",
        file=ctl,append=T)   
    }
                                                            
    
    cat(
    " #Fishing Mortaltity\n",
    0.04,       " #\n",
    -99,        " #\n",
    1,          " #F Method (1=discrete pope's)\n",
    0.9,        " #Max F\n",
    #4,          " #N iterations for tuning hybrid F\n",
    file=ctl, append=T)
    
    cat(
    0, 4, 0, 0, 0, 99, -1, " #Initial F setup\n", 
    0, 4, 0, 0, 0, 99, -1, " #Initial F setup\n", 
    file=ctl,append=T) 
    
    cat(" #Catchability Spec\n",
    "# Q_type options:  <0=mirror, 0/1=float, 2=parameter, 3=parm_w_random_dev, 4=parm_w_randwalk)\n",
    "# A: Do_Power, B: Do_Env_Link, C: Do_extra_sd, D: Q type\n",
    0, 0, 0, 0,  " #Fishery1\n",
    0, 0, 0, 0,  " #Fishery2\n",
    0, 0, 0, 0,  " #Survey\n",
    file = ctl, append = T)

    cat(" #Size Selectivity Spec\n",
    24, 0, 3, 0, " #Dbl Normal Fishery1\n",
    24, 0, 3, 0, " #Dbl Normal Fishery2\n",
    24, 0, 3, 0, " #Dbl Normal Survey\n",
    file = ctl, append = T)

    cat(" #Age Selectivity Spec\n",
    10, 0, 0, 0, " #Fishery1\n",
    10, 0, 0, 0, " #Fishery2\n",
    10, 0, 0, 0, " #Survey\n",
    file = ctl, append = T)

    cat(" #Selectivity Parameters\n",
    "#LO HI INIT PRIOR PRIOR_TYPE SD PHASE\n",
    file=ctl, append=T)

    write.table(fishery.selec.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    write.table(survey.selec.mat,file=ctl, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    cat(
    0,      " #No Tag Parameters\n",
    1,      " #Variance Adjustments\n",
        0,    0,  0,        "#_add_to_survey_CV\n",
        0,    0,  0,        "#_add_to_discard_stddev\n",
        0,    0,  0,        "#_add_to_bodywt_CV\n",
        1,    1,  1,        "#_mult_by_lencomp_N\n",
        f.age.wght,    f.age.wght, s.age.wght,        "#_mult_by_agecomp_N\n",
        1,    1,  1,      "#_mult_by_size-at-age_N\n",
    2,      " #Max_lambda_phase\n",
    1,      " #sd_offset\n",
    n.lambda,      " #Number Lambda Changes\n",
    "#Place holder for lambdas\n",
    file = ctl, append = T)
    #5, 1, 1, 0, 1, "#Fishery Ages\n",
    #4, 1, 1, 0, 1, "#Fishery Lengths\n",
    if(!use.len.data) {
        cat(
        data.type, 1, 1, 0, 1, "#Fishery Lengths\n",
        data.type, 2, 1, 0, 1, "#Fishery Lengths\n", 
        data.type, 3, 1, 0, 1, "#Survey Lengths\n", file = ctl, append = T)
    }
    cat(
    0,      " #Option for Variance Estimates\n",
    999, file=ctl, append=T)
}    
        
#------Starter File----------------------------------------------------------------------------------------------------------------------------
writeStarter <- function (starter)
{
    cat("#\n",
    "#Stock Synthesis Version 3.0.11\n",
    "#\n",
    dat,    "\n",
    ctl,    "\n",
    0,      " #0=use init values in ctl file, 1= use ss3.par\n",
    0,      " #run display detail (0,1,2)\n",
    0,      " #detailed age-structured reports in REPORT.SSO (0,1)\n",
    0,      " #write detailed checkup.sso file (0,1)\n",
    0,      " #write parm values to ParmTrace.SSO\n",
    0,      " #report level in CUMREPORT.SSO (0,1,2)\n",
    0,      " #include prior_like for non-estimated parameteres (0,1)\n",
    1,      " #use soft boundaries to aid convergence\n",
    3,      " #number of boostrap datafiles to produce\n",
    20,     " #turn off estimation for parameters entering after this phase\n",
    1,      " #MCMC burn interval\n",
    1,      " #MCMC thin interval\n",
    0,      " #jitter initial parm value by this fraction\n",
    -1,     " #min yr for sdreport outputs (-1 for styr)\n",
    -2,     " #max yr for sdreport outputs (-1 for endyr; -2 for endyr+Nforecastyrs\n",
    0,      " #N individual STD years\n",
    0.0001, " #final convergence criteria\n",
    0,      " #retrospective year relative to end year\n",
    1,      " #min age for calc of summary biomass\n",
    1,      " #Depletion basis:  denom is: 0=skip; 1=rel X*B0; 2=rel X*Bmsy; 3=rel X*B_styr\n",
    1,      " #Fraction (X) for Depletion denominator\n",
    1,      " #(1-SPR)_reporting:  0=skip; 1=rel(1-SPR); 2=rel(1-SPR_MSY); 3=rel(1-SPR_Btarget); 4=notrel\n",    
    1,      " #F_std reporting: 0=skip; 1=exploit(Bio); 2=exploit(Num); 3=sum(frates)0 # F_report_basis: 0=raw; 1=rel Fspr; 2=rel Fmsy ; 3=rel Fbtgt\n",
    0,      " #F_report_basis: 0=raw; 1=rel Fspr; 2=rel Fmsy ; 3=rel Fbtgt\n",
    999, append=FALSE, file=starter, sep="")
}

#-----------------Forcast File--------------------------------------------------------------------------------------

writeForecast <- function (forecast,y)
{
    cat(
    "#\n",
    "#Stock Synthesis\n",
    "#\n",
    1,                " # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy\n", 
    2,                " # MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt); 4=set to F(endyr)\n",
    spr.target,       " #  SPR target (e.g. 0.40)\n",
    bio.target,       " # Biomass target (e.g. 0.40)\n",
    0, 0, 0, 0, 0, 0, " # Benchmark years: beg_bio, end_bio, beg_selex, end_selex, bef_relF, end_relF\n",
    1,                " # Bmark_relF_Basis: 1= use year range; 2= set relF same as forecast below\n",
    1,                " # Forecast: 0=none; 1=F(SPR); 2=F(MSY) 3=F(Btgt); 4=F(endyr); 5=Ave F (enter yrs); 6=read Fmult\n",
    10,               " # N forecast years\n",
    1,                " # F scalar (only used for Do_Forecast == 5)\n",
    0, 0, 0, 0,       " # Fcast_years:  beg_selex, end_selex, beg_relF, end_relF\n",
    1,                " # Control rule method (1=west coast adjust catch; 2=adjust F)\n",
    ctl.rule.tgt,     " # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40)\n",
    ctl.rule.thres,   " # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10)\n",
    buffer,           " # Control rule fraction of Flimit (e.g. 0.75)\n",
    3,                " # N forecast loops (1=OFL only; 2=ABC; 3=get F from forecast ABC catch with allocations applied)\n",
    3,                " # First forecast loop with stochastic recruitment\n",
    0,                " # Forecast loop control #3 (reserved for future bells&whistles)\n", 
    0,                " # Forecast loop control #4 (reserved for future bells&whistles)\n", 
    0,                " # Forecast loop control #5 (reserved for future bells&whistles)\n", 
    999,              " # FirstYear for caps and allocations (should be after years with fixed inputs)\n", 
    0,                " # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)\n",
    0,                " # Do West Coast gfish rebuilder output (0/1)\n", 
    -1,               " # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)\n",
    -1,               " # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)\n",
    1,                " # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below\n",
    "# Note that fleet allocation is used directly as average F if Do_Forecast=4\n", 
    2,                " # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)\n",
    " # Conditional input if relative F choice = 2\n",
    " # Fleet relative F:  rows are seasons, columns are fleets\n",
    " # max totalcatch by fleet (-1 to have no max) must enter value for each fleet\n",
    rep(-1, f.fleets),         "\n",
    " # max totalcatch by area (-1 to have no max); must enter value for each fleet\n", 
    -1,         "\n",
    " # fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)\n",
    rep(0, f.fleets),          "\n", 
    append = FALSE, file=forecast, sep=" ") 

    cat(
    0,      " # Number of forecast catch levels to input (else calc catch from forecast F)\n", 
    2,     " # basis for input Fcast catch:  2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)\n",
    999, append=TRUE, file=forecast, sep=" ")
}

#--------.dat file ---------------------------------------------------------------------------------------------------------------------------------
writeDat<-function(dat, y, fore.catch)
{

    landings <- cbind(
        #Value of Catch                         #Year                              #Fleet          
        #fore.catch[( pre.fishery.yrs + 1 ): (y - 1)], ( pre.fishery.yrs + 1):(y - 1),    rep(1,(y-1-pre.fishery.yrs)) )
        round(catch.wght.values[( pre.fishery.yrs + 1 ): (y - 1),],2), 
        ( pre.fishery.yrs + 1):(y - 1), 
        rep(1, (y-1-pre.fishery.yrs)) )


    survey.data.yrs = seq(start.s.data, y - 1, 1)
    ss.survey.data = cbind(
        survey.data.yrs, rep(1, length(survey.data.yrs)), rep(f.fleets + 1, length(survey.data.yrs)), 
        round(index.expect[survey.data.yrs],0), rep(ss.survey.cv, length(survey.data.yrs)))
        
    cat(
    "#\n",
    "#Stock Synthesis\n",
    "#\n",
    1,                     " #Start Year\n",
    y-1,                   " #End Year\n",
    1,                     " #Number Seasons per Year\n",
    12,                    " #Months per Season/season\n",
    1,                     " #Spawning Season\n",
    f.fleets,              " #Nfleet\n",
    1,                     " #Nsurveys\n",
    1,                     " #Nareas\n",
    "Fishery1%Fishery2%Survey",   " \n",
    rep(0.50, f.fleets),    " #Fleet Timing\n",
    0.50,                  " #Survey Timing\n",
    rep(1, f.fleets+1),    " #_area_assignments_for_each_fishery_and_survey\n",
    rep(1, f.fleets),       " #Catch Units (Biomass=1)\n",
    rep(0.0001, f.fleets), " #SE of log(catch)\n", 
    2,                     " #Ngenders\n",
    max.age,               " #Nages\n",
    rep(0, f.fleets),      " #Initial Equilibrium Catch\n",
    dim(landings)[1],      " #Number of Catch Observations\n",
    "#catch year season\n", file=dat,sep=" ") 
    
    write.table(landings, file=dat, append=T, row.names=FALSE, col.names=FALSE)
    
    cat("#\n",
    dim(ss.survey.data)[1], " #Number of Survey Observations\n",
    "#_Units:  0=numbers; 1=biomass; 2=F\n",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
    "#_Errtype:  -1=normal; 0=lognormal; >0=T\n",                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    "#_Fleet Units   Errtype\n",
    1, 1, 0, "#Fishery1\n",
    2, 1, 0, "#Fishery2\n",
    3, 1, 0, "#Survey\n",
    file = dat, append = T, sep = " ")

    write.table(ss.survey.data, file = dat, append = T,row.names = FALSE, col.names = FALSE)
    
    cat("#\n",
    0,                  " #N_fleets with discard\n", 
    0,                  " #Number of Discard Observations\n",
    0,                  " #Number of Mean Body Weight Observations\n",
    30,                 " #DF_meanwt\n",
    "#Population Length Structure\n",
    2,                  " #length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector\n",
    2,                  " #bin width in cm\n",
    4,                  " #minimum bin size in cm\n",
    78,                 " #maximum bin size in cm\n",
    #3,                  " #length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector\n",
    #length(len.step),   " #Number of Population Length Bins\n",
    file=dat,append=T,sep=" ")
    
    #cat(len.step,"\n",
    #file=dat,append=T,sep=" ")
    
    cat(-1,             " #Compress tails\n",
    0.0001,             " #Constant added to all proportions\n", 
    0,                  " #Combine males into females at or below this bin number\n",
    length(data.len.step),   " #_N_LengthBins\n",
    file=dat,append=T,sep="")
    
    cat(data.len.step,"\n",
    file=dat,append=T,sep=" ")
    
    if (!skip.len){
        cat(n.length.obs,  " #Number of Length Observations\n", 
        "#Year Seas Fleet Gender Partition Nsamp\n",
        file=dat,append=T,sep=" ") 
        
        write.table(fishery.length.data,file=dat, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE)  
        write.table(survey.length.data, file=dat, append=T,row.names=FALSE, col.names=FALSE, quote=FALSE) 
    }

    if (skip.len){
        cat(0,  " #Number of Length Observations\n", 
        "#Year Seas Fleet Gender Partition Nsamp\n",
        file=dat,append=T,sep=" ") 
    }
 
    cat("#\n",
    max.age,             " #Number of Ages\n",
    seq(1,max.age,1),    " #Age Bins\n",
    1,                   " #Number of Ageing Error Sets\n",
    "#Age Error Matrix\n",
    rep(-1, max.age + 1),      " #True Ages\n",
    (1:(max.age+1)-0.5)*0.10,  " #Age Error StDev\n", 
    #rep(0.02, max.age + 1),    " #Age Error StDev\n", 
    "#\n",
    n.age.obs,          " #Number Age Observations\n",
    3,                  " #Age-Length Bin Option: 1=poplenbins; 2=datalenbins; 3=lengths\n",
    0,                  " #Combine males into females at or below this bin number\n",
    "#Year Seas Fleet Gender Partition Ageerr Lbinlo Lbinhi Nsamp\n",  
    file=dat,append=T,sep=" ")
    
    write.table(fishery.age.data, file=dat, append=T, row.names=FALSE, col.names=FALSE, quote=FALSE) 
    write.table(survey.age.data,  file=dat, append=T, row.names=FALSE, col.names=FALSE, quote=FALSE) 

    cat("#\n", 
    0, " #Number of Mean Size at Age Observations\n",
    0, " #Number of Environmental Variables \n",
    0, " #Number of Environmental Observations\n",
    0, " #Number of Weight Frequency Observations\n",
    0, " #Number of Taggindg Data Observations\n", 
    0, " #Number of Morph Composition Observation\n", 
    999,"\n",
    file=dat, append=T,sep="")
 
}
