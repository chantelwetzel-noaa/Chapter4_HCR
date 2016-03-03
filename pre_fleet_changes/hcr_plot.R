bio<- seq(0,100,5)
depl<-bio/bio[length(bio)]
f=0.25
step = 0.05
low = 0.05; high = 0.25
z1 = length(seq(0.05, low, step))+1 ; z2 = length(seq(0.05, high, step))+1
f.vec=rep(f,12)
ofl <- f*bio
buffer <- 0.95
#abc <- f*bio*buffer

f4010<-(f*(seq(low, high, step)-low)*high)/((high - low)*seq(low, high, step))
ofl4010<-f4010*bio[z1:z2]
#abc4010<- f4010*bio[3:9]*buffer
fspr<-expression(F[SPR])

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

#setwd("E:\\PhD\\")
#png("HCR_plot_color.png",width=7,height=7,res=300,units='in')
par(mfrow = c(1,1))
 plot(seq(high,1, step),ofl[z2:length(ofl)],,type='l',ylim=c(0,15),xlim=c(0,0.50),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2)
box()
lines(seq(0, high, step),ofl[1:z2],lty=2,lwd=2,col="black")
lines(seq(low, high, step),ofl4010,,lty=1,col="black",lwd=2)
axis(side=1,at=seq(0.1,1,.1),labels=c("0.10", "0.20", "0.30", "0.40", "0.50", "0.60", "0.70", "0.80", 
	"0.90", "1.0"))
mtext(side=1,"Relative Spawning Biomass",line=3,cex=1.5)
mtext(side=2,"Catch",line=1,cex=1.5)
abline(v = high, lty = 3); abline(v = low, lty = 3)






par(oma=c(2,2,2,2))
 plot(seq(0.40,1,0.05),ofl[9:length(ofl)],,type='l',ylim=c(0,max(ofl)),xlim=c(0,1),
     axes=F,ylab="",xlab="",xaxs="i",yaxs="i",lwd=2,cex.axis=1.2)
     
   
#trans.red <- rgb(1,0,0,1)  ; xx=c(0,0.25,0.25,0); yy= c(rep(max(ofl),2),0,0) 
#polygon(xx,yy,col=trans.red,lty=0)
#trans.yellow <- rgb(1,1,0,1); xx=c(0.25,0.40,0.40,0.25); yy= c(rep(max(ofl),2),0,0)
#polygon(xx,yy,col=trans.yellow,lty=0)
#trans.green <- rgb(0,1,0,1); xx=c(0.40,1,1,0.40); yy= c(rep(max(ofl),2),0,0)
#polygon(xx,yy,col=colors()[614],lty=0)

box(col="grey80")
#abline(v=0.40,lty=3,col="black") ; abline(v=0.25,lty=3,col="black")
lines(seq(0, high, 0.05),ofl[1:9],lty=2,lwd=2,col="black")
lines(seq(low, high, 0.05),ofl4010,,lty=1,col="black",lwd=2)
lines(seq(high, 1, 0.05),abc[9:length(abc)],col='white',lwd=2)
lines(seq(low, high, 0.05),abc4010,lty=1,lwd=2,col='white')
#lines(depl[3:length(depl)],c(f4010,f.vec)*40,col='blue',lwd=2)
axis(side=1,at=seq(0.1,1,.1),labels=seq(0.10, 1, 0.10),col="gray80")
#mtext(side=3,"U.S. West Coast Groundfish Harvest Control Rule",line=1,cex=1.5)
mtext(side=1,"Relative Spawning Biomass",line=3,cex=1.5)
mtext(side=2,"Catch",line=1,cex=1.5,las=1)
#mtext(side=4,fspr,line=1,cex=1.5,las=1)

#lines(seq(0.40,1,0.05),ofl[9:length(ofl)],lty=1,lwd=2)
#print.letter(label=expression(paste("OFL=",F[SPR],"*SB",sep="")),xy=c(0.55,0.70),cex=1.25,col="black")
#print.letter(label=expression(paste("ABC=Buffer*",F[SPR],"*SB",sep="")),xy=c(0.80,0.5),cex=1.25,col="white")
#print.letter(label=expression(F[SPR]),xy=c(0.85,0.35),cex=1.25,col="blue")

#dev.off()
