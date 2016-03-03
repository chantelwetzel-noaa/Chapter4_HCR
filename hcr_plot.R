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

	out <-NULL
	out$ofl <- ofl
	out$abc <- abc
	out$abc.hcr <- abc.hcr
	out$z1  <- z1
	out$z2  <- z2
	return(out)
}


fspr<-expression(F[SPR])
print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}


par(mfrow = c(2,2), mar = rep(1.25, 4), oma = c(4,4,4,4))
ofl.col = "grey71"; abc.col = 1 ; t.col = "grey63"
# 25-5 ==========================================================
low = 0.05; high = 0.25; f=0.30
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col=ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 4, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 2, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 4, lwd = 1, col = abc.col)
lines(seq(low,high,step), out$abc.hcr, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, out$ofl[z1]), lty = 3, col = t.col)
lines(rep(high, 2), c(0, out$ofl[z2]), lty = 3, col = t.col)
axis(side=1,at=seq(0.1,0.40,.1), labels=c("0.10", "0.20", "0.30", "0.40"), cex.axis = 1.2)
#abline(v = high, lty = 3, col = t.col); abline(v = low, lty = 3, col = t.col)
print.letter(label = "a)", xy = c(0.05, 0.92), cex = 1.2)
# 40-10 ==========================================================
low = 0.10; high = 0.40; f = 0.40
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col = ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 4, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 2, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 4, lwd = 1, col = abc.col)
lines(seq(low,high,step), out$abc.hcr, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, out$ofl[z1]), lty = 3, col = t.col)
lines(rep(high, 2), c(0, out$ofl[z2]), lty = 3, col = t.col)
axis(side=1,at=seq(0.1,0.40,.1), labels=c("0.10", "0.20", "0.30", "0.40"), cex.axis = 1.2)
#abline(v = high, lty = 3, col = t.col); abline(v = low, lty = 3, col = t.col)
print.letter(label = "b)", xy = c(0.05, 0.92), cex = 1.2)step)

# 30-10 ==========================================================
low = 0.10; high = 0.30; f = 0.35
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2, col = ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 4, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 2, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 4, lwd = 1, col = abc.col)
lines(seq(low,high,step), out$abc.hcr, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, out$ofl[z1]), lty = 3, col = t.col)
lines(rep(high, 2), c(0, out$ofl[z2]), lty = 3, col = t.col)
#abline(v = high, lty = 3, col = t.col); abline(v = low, lty = 3, col = t.col)
axis(side=1,at=seq(0.1,0.4,.1), labels=c("0.10", "0.20", "0.30", "0.40"), cex.axis = 1.2)
mtext(side=2,"Catch",line=1, cex=1.1, outer = T)
print.letter(label = "c)", xy = c(0.05, 0.92), cex = 1.2)
# 20-05 ==========================================================
low = 0.05; high = 0.20; f = 0.25
out <- yield(low, high, f); z1 = out$z1 ; z2 = out$z2
plot(seq(high,1, step), out$ofl[z2:length(out$ofl)],,type='l',ylim=c(0, 20),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i", lwd=2, cex.axis=1.2, col = ofl.col)
box()
lines(seq(0,1,step), out$ofl, lty = 4, col = ofl.col)
# abc
lines(seq(high,1,step), out$abc[z2:length(out$abc)], lty = 1, lwd = 2, col = abc.col)
lines(seq(0,1,step), out$abc, lty = 4, lwd = 1, col = abc.col)
lines(seq(low,high,step), out$abc.hcr, lty = 1, lwd = 2, col = abc.col)
lines(rep(low, 2), c(0, out$ofl[z1]), lty = 3, col = t.col)
lines(rep(high, 2), c(0, out$ofl[z2]), lty = 3, col = t.col)
#abline(v = high, lty = 3, col = t.col); abline(v = low, lty = 3, col = t.col)
axis(side=1,at=seq(0.1,0.40,.1), labels=c("0.10", "0.20", "0.30", "0.40"), cex.axis = 1.2)
print.letter(label = "d)", xy = c(0.05, 0.92), cex = 1.2)
mtext(side=1,"Relative Spawning Biomass",line=2, cex=1.1, outer = T)
legend("topright", bty ='n', lty = c(1,1), col = c(ofl.col, abc.col), lwd = c(2,2), cex = 1.1,
	legend = c("OFL", "ABC = OFL * Buffer"))






