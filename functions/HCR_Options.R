# HCR Options ===================================================================================

# The current HCR
if (hcr.opt == "hcr_25_5" ){
	spr.target     <- 0.30         
	bio.target     <- 0.25
	ctl.rule.tgt   <- 0.25
	ctl.rule.thres <- 0.05
	over.thres     <- 0.125
}

# Classic HCR Option
if (hcr.opt == "hcr_40_10" ){
	spr.target     <- 0.40         
	bio.target     <- 0.40
	ctl.rule.tgt   <- 0.40
	ctl.rule.thres <- 0.10
	over.thres     <- 0.25
}

# Alternative 1
if (hcr.opt == "hcr_30_10" ){
	spr.target     <- 0.35         
	bio.target     <- 0.30
	ctl.rule.tgt   <- 0.30
	ctl.rule.thres <- 0.15
	over.thres     <- 0.10
}

# Alternative 2
if (hcr.opt == "hcr_20_05" ){
	spr.target     <- 0.25         
	bio.target     <- 0.20
	ctl.rule.tgt   <- 0.20
	ctl.rule.thres <- 0.10
	over.thres     <- 0.05
}