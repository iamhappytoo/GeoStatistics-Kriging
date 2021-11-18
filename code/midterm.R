##Problem 1##
library(geoR)
library(gstat)
library(sp)
data(parana)
help(parana)
points(parana)
names(parana)
qq <- lm(parana$data~parana$coords[,1]+parana$coords[,2])
parana$data=qq$residuals
plot(variogram1)
lines.variomodel(cov.model="gaussian",cov.pars=c(1000,200),nug=500,max.dist=620)
variogram1 <- variog(parana,max.dist=620)
plot(variogram1)
lines.variomodel(cov.model="gaussian",cov.pars=c(6300,250),nug=0,max.dist=620)
q <- summary(parana)
x <- seq(q$borders.summary[1,1],q$borders.summary[2,1],by=1)
y <- seq(q$borders.summary[1,2],q$borders.summary[2,2],by=1)
xv=rep(x,q$borders.summary[2,2]-q$borders.summary[1,2]+1)
yv=rep(y,each=q$borders.summary[2,1]-q$borders.summary[1,1]+1)
in_mat <- as.matrix(cbind(xv,yv))
plot(in_mat)
krig <- ksline(parana,cov.model="exp",cov.pars=c(10,3.33),nugget = 0,locations=in_mat)
imageori(krig,val=krig$predict)
imageori(krig,val=krig$krige.var)

length(b4$coords[,1])
variogram1 <- variog(parana,max.dist=620)
plot(variogram1)
lines.variomodel(cov.model="gaussian",cov.pars=c(6300,250),nug=0,max.dist=620)
a <- as.data.frame(cbind(parana$coords[,1], parana$coords[,2], parana$data))
names(a) <- c("x", "y", "data")
imageori <-image
library(gstat)
coordinates(a) <- ~x+y
a <- as.data.frame(cbind(parana$coords[,1], parana$coords[,2], parana$data))
names(a) <- c("x", "y", "data")
coordinates(a) <- ~x+y
qq <- hscat(data~1, a, c(0,20,40,60,80,100,120,140,160,180))
plot(qq, main="h-scatterplots")

q$coords.summary[1,2]
q <- summary(parana)
x <- seq(q$borders.summary[1,1],q$borders.summary[2,1],by=1)
y <- seq(q$borders.summary[1,2],q$borders.summary[2,2],by=1)
xv=rep(x,q$borders.summary[2,2]-q$borders.summary[1,2]+1)
yv=rep(y,each=q$borders.summary[2,1]-q$borders.summary[1,1]+1)
in_mat <- as.matrix(cbind(xv,yv))
plot(in_mat)
krig <- ksline(parana,cov.model="exp",cov.pars=c(10,3.33),nugget = 0,locations=in_mat)
imageori(krig,val=krig$predict)
image(krig,val=krig$krige.var)

imageori <- image
library(geoR)
library(gstat)
grd <- expand.grid(x,y)
coordinates(grd)~x+y
idw.out <- idw(data~1, locations=~x+y,a,grd)
class(idw.out)

data
head(data,10)

points(ld)
c1





c2
  c2variogram3 <- variog(ld,dir=0,max.dist=4440,tol=pi/4)
plot(variogram3)
initial.values1 <- expand.grid(seq(200000, 300000, by=100), seq(2000, 3000, by=100))
par(mfrow=c(1,2))
plot(variogram1, main="semivariogram for lead")
plot(variogram2, main="semivariogram for zinc")
initial.values2 <- expand.grid(seq(200000, 300000, by=100), seq(2000, 3000, by=100))
fit4 <- variofit(variogram2, cov.model="sph", ini.cov.pars=initial.values2, fix.nugget=FALSE, nugget=45000, wei="equal")
fit5 <- variofit(variogram2, cov.model="sph", ini.cov.pars=initial.values2, fix.nugget=FALSE, nugget=45000, wei="npairs")
fit6 <- variofit(variogram2, cov.model="sph", ini.cov.pars=initial.values2, fix.nugget=FALSE, nugget=45000, wei="cressie")



head(a)
mod <- lm(elevation~y,data=a)
mod
summary(mod)

ols

plot(var1)
lines.variomodel(cov.model="sph",cov.par=c(1,15),nugget=1,max.dist=24)
krig <- ksline(b,cov.model="exp",cov.pars=c(1,15),nugget = 1,locations=grd)
imageori(krig,val=krig$predict)
imageori(krig,val=krig$krige.var)


data=read.table("selected dataset.txt",header=TRUE)
b1 <- as.geodata(cbind(data[,1:2],data$expt))
b2 <- as.geodata(cbind(data[,1:2],data$elev))
b3 <- as.geodata(cbind(data[,1:2],data$quartz))
b4 <- as.geodata(cbind(data[,1:2],data$Ksat))
b5 <- as.geodata(cbind(data[,1:2],data$bubble))
b6 <- as.geodata(cbind(data[,1:2],data$moist_s1))
library(maps)
par(mfrow=c(1,1))
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5)
map("county","ca",add=TRUE)
points(b2,main="Elevation")
points(b3,main="Quartz")
points(b4,main="Ksat")
points(b5,main="Bubble")
points(b6,main="1st layer soil moisture")

plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5,"n")
map("county","ca",add=TRUE)
points(data$lon,data$lat,main="Expt",cex=data$expt)
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5,"n")
map("county","ca",add=TRUE)
points(b2,main="Elevation")
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5,"n")
map("county","ca",add=TRUE)
points(b3,main="Quartz")
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5,"n")
map("county","ca",add=TRUE)
points(b4,main="Ksat")
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5,"n")
map("county","ca",add=TRUE)
points(b5,main="Bubble")
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5,"n")
map("county","ca",add=TRUE)
points(b6,main="1st layer soil moisture")
summary(d1)

variogram1 <- variog(ld,dir=0,max.dist=4440,tol=pi/8)
variogram2 <- variog(zc,dir=0,max.dist=4440,tol=pi/8)
plot(variogram1, main="semivariogram for lead")
plot(variogram2, main="semivariogram for zinc")

###Fit by eye
vgf0 <- variog(d1)
plot(vgf0)
lines.variomodel(cov.model="sph",cov.pars=c(1500000,10),nug=1000000,max.dist=13,lty=2)
lines.variomodel(cov.model="exp",cov.pars=c(1500000,5),nug=1000000,max.dist=13,lty=3)
lines.variomodel(cov.model="gaussian",cov.pars=c(1500000,6),nug=1000000,max.dist=13,lty=4)

vgf1 <- variog(d1,dir=0,max.dist=10,tol=pi/3)
plot(vgf1)
lines.variomodel(cov.model="sph",cov.pars=c(1000000,7),nug=1000000,max.dist=10,lty=2)
lines.variomodel(cov.model="exp",cov.pars=c(1000000,3),nug=1000000,max.dist=10,lty=3)
lines.variomodel(cov.model="gaussian",cov.pars=c(1200000,6),nug=1000000,max.dist=10,lty=4)
legend("topleft",legend=c("sph","exp","gaussian"),lty=c(2,3,4))

initial.values1 <- expand.grid(seq(15000, 30000, by=10), seq(800, 2500, by=10))
fit1 <- variofit(variogram1, cov.model="sph", ini.cov.pars=initial.values1, fix.nugget=FALSE, nugget=0.000001, wei="equal")
fit2 <- variofit(variogram1, cov.model="sph", ini.cov.pars=initial.values1, fix.nugget=FALSE, nugget=0.000001, wei="npairs")
fit3 <- variofit(variogram1, cov.model="sph", ini.cov.pars=initial.values1, fix.nugget=FALSE, nugget=0.000001, wei="cressie")
initial.values2 <- expand.grid(seq(200000, 300000, by=100), seq(2000, 3000, by=100))
fit4 <- variofit(variogram2, cov.model="sph", ini.cov.pars=initial.values2, fix.nugget=FALSE, nugget=45000, wei="equal")
fit5 <- variofit(variogram2, cov.model="sph", ini.cov.pars=initial.values2, fix.nugget=FALSE, nugget=45000, wei="npairs")
fit6 <- variofit(variogram2, cov.model="sph", ini.cov.pars=initial.values2, fix.nugget=FALSE, nugget=45000, wei="cressie")
plot(variogram1) 
lines(fit1, type="l", lty=2) 
lines(fit2, type="l", lty=3) 
lines(fit3, type="l") 
legend("topleft", legend=c("Equal weights", "Number of pairs", "Weights by Cressie"), lty=c(2,3,1))
plot(variogram2) 
lines(fit4, type="l", lty=2) 
lines(fit5, type="l", lty=3) 
lines(fit6, type="l") 
legend("topleft", legend=c("Equal weights", "Number of pairs", "Weights by Cressie"), lty=c(2,3,1))



