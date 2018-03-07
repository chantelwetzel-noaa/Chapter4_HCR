low = 0.05; high = 0.25
step = 0.05
bio<- seq(0,100,5)
depl<-bio/bio[length(bio)]	


yield <- function(low, high, f){
	
	z1 = length(seq(0.05, low, step))+1 ; z2 = length(seq(0.05, high, step))+1
	f.vec  <- rep(f,12)
	ofl    <- f*bio
	buffer <- 0.85 #Value used to make plotting better
	abc    <- ofl*buffer
	
	f.adj<-(f*(seq(low, high, step)-low)*high)/((high - low)*seq(low, high, step))
	#ofl[z1:z2] <- f.adj*bio[z1:z2]
	abc.hcr <- f.adj*bio[z1:z2]*buffer
	ofl.hcr <- f.adj*bio[z1:z2]

	out <-NULL
	out$ofl <- ofl
	out$abc <- abc
	out$abc.hcr <- abc.hcr
	out$ofl.hcr <- ofl.hcr
	out$z1  <- z1
	out$z2  <- z2
	return(out)
}


fspr<-expression(F[SPR])
bproxy <- expression(B[PROXY])
bthres <- expression(B[THRES])
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

setwd("C:/PhD/Chapter4/Poster")
png(filename = "HCR_wMMST.png", width = 6.7, height = 6, units = 'in', res = 256)
par(mfrow = c(2,2), mar = rep(0.50, 4), oma = c(3,3,3,3))
ofl.col = 1; abc.col = 'grey50' ; t.col = "grey63"; acl.col = 1
# 20-05 ==========================================================
low = 0.05; high = 0.20; f = 0.25
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i", lwd=2, cex.axis=1.2, col = ofl.col)
box()

lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 1, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)
# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)
lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)
print.letter(xy = c(0.19, 0.20), bthres)
print.letter(xy = c(0.28, 0.30), "MSST")
print.letter(xy = c(0.50, 0.40), bproxy)

print.letter(label = "(a)", xy = c(0.05, 0.92), cex = 1)
mtext(side=1,"Relative biomass",line=2, cex=1, outer = T)
legend("topright", bty ='n', lty = c(1,1, 2), col = c(ofl.col, abc.col, acl.col), lwd = c(2,2,2), cex = 1,
	legend = c(expression("OFL ="~F[MSY]~"*SB"), "ABC = OFL * Buffer", "ACL"))

# 25-5 ==========================================================
low = 0.05; high = 0.25; f=0.30
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col=ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 1, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 1, lwd = 2, col = abc.col)

lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)
# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)

print.letter(xy = c(0.19, 0.23), bthres)
print.letter(xy = c(0.33, 0.32), "MSST")
print.letter(xy = c(0.60, 0.55), bproxy)
print.letter(label = "(b)", xy = c(0.05, 0.92), cex = 1)

# 30-10 ==========================================================
low = 0.10; high = 0.30; f = 0.35
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col = ofl.col)
box()

lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 1, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)
# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)
print.letter(xy = c(0.28, 0.35), bthres)
print.letter(xy = c(0.37, 0.42), "MSST")
print.letter(xy = c(0.70, 0.71), bproxy)
axis(side=1,at=seq(0,0.4,.1), labels=c("0.00", "0.10", "0.20", "0.30", "0.40"), cex.axis = 1)
mtext(side=2,"ACL",line=1, cex=1, outer = T)
print.letter(label = "(d)", xy = c(0.05, 0.92), cex = 1)

# 40-10 ==========================================================
low = 0.10; high = 0.40; f = 0.40
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col = ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 1, lwd = 2, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 1, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high, 2), c(0, 20), lty = 3, col = t.col)
lines(rep(high/2, 2), c(0, 20), lty = 3, col = t.col)
# acl 
lines(seq(high,1,step), out$abc[z2:length(out$abc)]-0.10, lty = 2, lwd = 2, col = acl.col)
lines(seq(low,high,step), out$abc.hcr, lty = 2, lwd = 2, col = acl.col)
print.letter(xy = c(0.28, 0.39), bthres)
print.letter(xy = c(0.47, 0.55), "MSST")
print.letter(xy = c(0.89, 0.65), bproxy)
axis(side=1,at=seq(0,0.40,.1), labels=c("0.00","0.10", "0.20", "0.30", "0.40"), cex.axis = 1)
print.letter(label = "(c)", xy = c(0.05, 0.92), cex = 1)

dev.off()






