setwd("C:/Users/BANZH/Downloads/")
data=read.table("selected dataset.txt",header=TRUE)
##Map the location of the data
library(maps)
par(mfrow=c(1,1))
plot(data$lon,data$lat,xlim=c(-125,-114),ylim=c(32,43),xlab="Longitude",ylab="Latitude",main="locations",cex=0.5)
map("county"",ca",add=TRUE)
##plot the points relative to their value
library(geoR)
b1 <- as.geodata(cbind(data[,1:2],data$expt))
b2 <- as.geodata(cbind(data[,1:2],data$Ksat))
b3 <- as.geodata(cbind(data[,1:2],data$moist_s1))
b4 <- as.geodata(cbind(data[,1:2],data$moist_s2))
b5 <- as.geodata(cbind(data[,1:2],data$moist_s3))
b6 <- as.geodata(cbind(data[,1:2],data$elev))
b7 <- as.geodata(cbind(data[,1:2],data$avg_T))
b8 <- as.geodata(cbind(data[,1:2],data$bubble))
b9 <- as.geodata(cbind(data[,1:2],data$quartz))
b10 <- as.geodata(cbind(data[,1:2],data$bulk_density))
b11 <- as.geodata(cbind(data[,1:2],data$Wcr_FRACT))
b12 <- as.geodata(cbind(data[,1:2],data$Wpwp_FRACT))
par(mfrow=c(3,4))
points(b1,main="Expt",xlab="Longitude",ylab="Latitude")
points(b2,main="Ksat",xlab="Longitude",ylab="Latitude")
points(b3,main="moist_s1",xlab="Longitude",ylab="Latitude")
points(b4,main="moist_s2",xlab="Longitude",ylab="Latitude")
points(b5,main="moist_s3",xlab="Longitude",ylab="Latitude")
points(b6,main="Elevation",xlab="Longitude",ylab="Latitude")
points(b7,main="avg_T",xlab="Longitude",ylab="Latitude")
points(b8,main="bubble",xlab="Longitude",ylab="Latitude")
points(b9,main="quartz",xlab="Longitude",ylab="Latitude")
points(b10,main="bulk density",xlab="Longitude",ylab="Latitude")
points(b11,main="wcr_fract",xlab="Longitude",ylab="Latitude")
points(b12,main="wpwp_fract",xlab="Longitude",ylab="Latitude")
library(scatterplot3d)
par(mfrow=c(2,3))
scatterplot3d(data$lon,data$lat,data$Ksat, xlab="South to North", ylab="West to East", zlab="Ksat")
scatterplot3d(data$lon,data$lat,data$moist_s3, xlab="South to North", ylab="West to East", zlab="3rd Layer soil moisture")
scatterplot3d(data$lon,data$lat,data$elev, xlab="South to North", ylab="West to East", zlab="elevation")
scatterplot3d(data$lon,data$lat,data$bubble, xlab="South to North", ylab="West to East", zlab="Bubble pressure")
scatterplot3d(data$lon,data$lat,data$bulk_density, xlab="South to North", ylab="West to East", zlab="Bulk density")
scatterplot3d(data$lon,data$lat,data$Wpwp_FRACT, xlab="South to North", ylab="West to East", zlab="wpwp fraction")
q1 <- lm(data$Ksat~data$lon+data$lat) 
q2 <- lm(data$moist_s3~data$lon+data$lat) 
q3 <- lm(data$elev~data$lon+data$lat) 
q4 <- lm(data$bubble~data$lon+data$lat) 
q5 <- lm(data$bulk_density~data$lon+data$lat) 
q6 <- lm(data$Wpwp_FRACT~data$lon+data$lat) 
c1 <- as.data.frame(cbind(data$lon,data$lat,q1$res)) 
c2 <- as.data.frame(cbind(data$lon,data$lat,q2$res)) 
c3 <- as.data.frame(cbind(data$lon,data$lat,q3$res)) 
c4 <- as.data.frame(cbind(data$lon,data$lat,q4$res)) 
c5 <- as.data.frame(cbind(data$lon,data$lat,q5$res)) 
c6 <- as.data.frame(cbind(data$lon,data$lat,q6$res)) 
names(c1) <- c("x","y","detKsat") 
names(c2) <- c("x","y","detmoist_s3") 
names(c3) <- c("x","y","detelev") 
names(c4) <- c("x","y","detbubble") 
names(c5) <- c("x","y","detbulk_dens") 
names(c6) <- c("x","y","detwpwp_fract")
d1 <- as.geodata(c1) 
d2 <- as.geodata(c2)
d3 <- as.geodata(c3) 
d4 <- as.geodata(c4)
d5 <- as.geodata(c5) 
d6 <- as.geodata(c6)
imageori <- image
library(gstat)
coordinates(c1) <- ~x+y 
coordinates(c2) <- ~x+y 
coordinates(c3) <- ~x+y 
coordinates(c4) <- ~x+y 
coordinates(c5) <- ~x+y 
coordinates(c6) <- ~x+y 
plt1 <- hscat(detKsat~1, c1, seq(1,13,by=1))
plt2 <- hscat(detmoist_s3~1, c2, seq(1,13,by=1)) 
plt3 <- hscat(detelev~1, c3, seq(1,13,by=1))
plt4 <- hscat(detbubble~1, c4, seq(1,13,by=1)) 
plt5 <- hscat(detbulk_dens~1, c5, seq(1,13,by=1))
plt6 <- hscat(detwpwp_fract~1, c6, seq(1,13,by=1)) 
plot(plt1) 
plot(plt2) 
plot(plt3) 
plot(plt4) 
plot(plt5) 
plot(plt6,main="wpwp_fract") 
par(mfrow=c(1,2)) 
varig1 <- variog(d1,dir=0,max.dist=10,tol=pi/6,option="cloud") 
plot(varig1, main="Ksat") 
varig1_rob <- variog(d1,dir=0,max.dist=10,tol=pi/6,option="cloud",estimator.type = "modulus") 
plot(varig1_rob, main="Ksat, robust estimator") 
varig2 <- variog(d2,dir=0,max.dist=10,tol=pi/6,option="cloud") 
plot(varig2, main="moist_s3") 
varig2_rob <- variog(d2,dir=0,max.dist=10,tol=pi/6,option="cloud",estimator.type = "modulus") 
plot(varig2_rob, main="moist_s3, robust estimator") 
varig3 <- variog(d3,dir=0,max.dist=10,tol=pi/6,option="cloud") 
plot(varig3, main="elevation") 
varig3_rob <- variog(d3,dir=0,max.dist=10,tol=pi/6,option="cloud",estimator.type = "modulus") 
plot(varig3_rob, main="elevation, robust estimator") 
varig4 <- variog(d4,dir=0,max.dist=10,tol=pi/6,option="cloud") 
plot(varig4, main="bubble pressure") 
varig4_rob <- variog(d4,dir=0,max.dist=10,tol=pi/6,option="cloud",estimator.type = "modulus") 
plot(varig4_rob, main="bubble pressure, robust estimator") 
varig5 <- variog(d5,dir=0,max.dist=10,tol=pi/6,option="cloud") 
plot(varig5, main="bulk density") 
varig5_rob <- variog(d5,dir=0,max.dist=10,tol=pi/6,option="cloud",estimator.type = "modulus") 
plot(varig5_rob, main="bulk density, robust estimator") 
varig6 <- variog(d6,dir=0,max.dist=10,tol=pi/6,option="cloud") 
plot(varig6, main="wpwp fraction") 
varig6_rob <- variog(d6,dir=0,max.dist=10,tol=pi/6,option="cloud",estimator.type = "modulus") 
plot(varig6_rob, main="wpwp fraction, robust estimator") 
cloud1 <- variog(d1, bin.cloud=TRUE, max.dist=10,option="cloud") 
cloud1_rob <- variog(d1, bin.cloud=T, estimator.type="modulus",max.dist=10,option="cloud") 
cloud2<- variog(d2, bin.cloud=T, max.dist=10,option="cloud") 
cloud2_rob <- variog(d2, bin.cloud=T, estimator.type="modulus",max.dist=10,option="cloud") 
cloud3 <- variog(d3, bin.cloud=T, max.dist=10,option="cloud") 
cloud3_rob <- variog(d3, bin.cloud=T, estimator.type="modulus",max.dist=10,option="cloud") 
cloud4<- variog(d4, bin.cloud=T, max.dist=10,option="cloud") 
cloud4_rob <- variog(d4, bin.cloud=T, estimator.type="modulus",max.dist=10,option="cloud") 
cloud5<- variog(d5, bin.cloud=T, max.dist=10,option="cloud") 
cloud5_rob <- variog(d5, bin.cloud=T, estimator.type="modulus",max.dist=10,option="cloud") 
cloud6<- variog(d6, bin.cloud=T, max.dist=10,option="cloud") 
cloud6_rob <- variog(d6, bin.cloud=T, estimator.type="modulus",max.dist=10,option="cloud") 
plot(cloud1,bin.cloud=TRUE) 
plot(cloud1_rob,bin.cloud=T) 
plot(cloud2,bin.cloud=T) 
plot(cloud2_rob,bin.cloud=T) 
plot(cloud3,bin.cloud=T) 
plot(cloud3_rob,bin.cloud=T) 
plot(cloud4,bin.cloud=T) 
plot(cloud4_rob,bin.cloud=T) 
plot(cloud5,bin.cloud=T) 
plot(cloud5_rob,bin.cloud=T) 
plot(cloud6,bin.cloud=T) 
plot(cloud6_rob,bin.cloud=T) 
vg1 <- variog4(d1,max.dist=10,tol=pi/6) 
vg2 <- variog4(d1,max.dist=10,tol=pi/4) 
vg3 <- variog4(d1,max,dist=10,tol=pi/3) 
vg11 <- variog4(d1,max.dist=10,tol=pi/6,estimator.type = "modulus") 
vg21 <- variog4(d1,max.dist=10,tol=pi/4,estimator.type = "modulus") 
vg31 <- variog4(d1,max,dist=10,tol=pi/3,estimator.type = "modulus") 
par(mfrow=c(2,3)) 
plot(vg1) 
title(main="Ksat, tol=pi/6") 
plot(vg2) 
title(main="Ksat, tol=pi/4") 
plot(vg3) 
title(main="Ksat, tol=pi/3") 
plot(vg11) 
title(main="Ksat, tol=pi/6, estimator=robust") 
plot(vg21) 
title(main="Ksat, tol=pi/4, estimator=robust") 
plot(vg31) 
title(main="Ksat, tol=pi/3, estimator=robust") 

par(mfrow=c(1,1))
vgf0 <- variog(d1,dir=pi/2,tol=pi/6,estimator.type="modulus") 
plot(vgf0) 
lines.variomodel(cov.model="sph",cov.pars=c(1500000,13),nug=500000,max.dist=13,lty=2) 
lines.variomodel(cov.model="exp",cov.pars=c(1500000,13),nug=500000,max.dist=13,lty=3) 
lines.variomodel(cov.model="gaussian",cov.pars=c(1500000,10),nug=500000,max.dist=13,lty=4) 
lines.variomodel(cov.model="power",cov.pars=c(25000,1.8),nug=500000,max.dist=13, lty=1) 
legend("topleft",legend=c("sph","exp","gaussian","power"),lty=c(2,3,4,1)) 
title(main="fit by eye, dir=pi/2, tol=pi/6") #variogram at particular direction and tolerance and fit by eye with different models 
initial.values1 <- expand.grid(seq(20000, 30000, by=1000), seq(1.5, 1.9, by=0.01)) 
fit1 <- variofit(vgf0, cov.model="power", ini.cov.pars=initial.values1, fix.nugget=FALSE, nugget=1200000, wei="equal") 
fit2 <- variofit(vgf0, cov.model="power", ini.cov.pars=initial.values1, fix.nugget=FALSE, nugget=1200000, wei="npairs") 
fit3 <- variofit(vgf0, cov.model="power", ini.cov.pars=initial.values1, fix.nugget=FALSE, nugget=1200000, wei="cressie") 
plot(vgf1) 
lines(fit1, type="l", lty=2)  
lines(fit2, type="l", lty=3)  
lines(fit3, type="l", lty=1)  
legend("bottomright", legend=c("Equal weights", "Number of pairs", "Weights by Cressie"), lty=c(2,3,1)) 

datach <- as.data.frame(cbind(data$lon,data$lat,d1$data)) 
names(datach) <- c("x","y","resKsat") 
coord <- as.matrix(cbind(datach$x,datach$y))
x1 <- rep(rep(0,nrow(datach)),nrow(datach)) 
dist <- matrix(x1,nrow=nrow(datach),ncol=nrow(datach)) 
for(i in 1:nrow(datach)){   
  for (j in 1:nrow(datach)){     
    dist[i,j]=((coord[i,1]-coord[j,1])^2+(coord[i,2]-coord[j,2])^2)^0.5   
  } } 
c0 <- 0 
c1 <- 0.05 
alpha <- 3 
x1 <- rep(rep(0,nrow(datach)),nrow(datach)) 
C2 <- matrix(x1,nrow=nrow(datach),ncol=nrow(datach)) 
for(i in 1:nrow(datach)){   
  for(j in 1:nrow(datach)){     
    C2[i,j]=c1-c1*(1.5*dist[i,j]/alpha-0.5*(dist[i,j]/alpha)^3)     
    if(dist[i,j]==0){C2[i,j]=c0+c1}     
    if(dist[i,j]>3){C2[i,j]=0}   } } 
L <- chol(C2) 
e <- rnorm(nrow(datach)) 
mu <- 0.1 
data1 <- mu + t(L) %*% e 
aa <- as.data.frame(cbind(datach[,1:2],data1)) 
bb <- as.geodata(aa) 
points(bb)

var1 <- variog(bb) 
plot(var1) 
lines.variomodel(cov.model="sph",cov.pars=c(0.05,3.2),nugget=0,lwd=2) 
lines.variomodel(cov.model="sph",cov.pars=c(0.05,3),nugget=0,lwd=2,col="red") 
legend("bottomright",c("fit by eye","theoretical"),col=c("black","red"), lwd=2) 


x1 <- rep(rep(0,nrow(datach)),nrow(datach)) 
dist <- matrix(x1,nrow=nrow(datach),ncol=nrow(datach)) 
for(i in 1:nrow(datach)){   
  for (j in 1:nrow(datach)){     
    dist[i,j]=((coord[i,1]-coord[j,1])^2+(coord[i,2]-coord[j,2])^2)^0.5   
  } } 
###Create distance matrix 
c0 <- 0 
c1 <- 0.05 
alpha <- 2.8 
x1 <- rep(rep(0,nrow(datach)),nrow(datach)) 
C1 <- matrix(x1,nrow=nrow(datach),ncol=nrow(datach)) 
for(i in 1:nrow(datach)){   
  for(j in 1:nrow(datach)){     
    C1[i,j]=c1-c1*(1.5*dist[i,j]/alpha-0.5*dist[i,j]^3/alpha^3)     
    if(i==j){C1[i,j]=c0+c1} 
    if(dist[i,j]>3){
      C1[i,j]=0
      }   
  }
} 
r=eigen(C1,symmetric = TRUE) 
val <- r$values 
L <- diag(val) 
p <- r$vectors 
C1_2=p%*%sqrt(L)%*%t(p) 
e <- rnorm(nrow(datach))
mu <- 0.1 
data1 <- mu + C1_2 %*% e 
data2 <- cbind(datach[,c(1,2)],data1) 
b1 <- as.geodata(data2) 
points(b1) 

var1 <- variog(b1) 
plot(var1) 
lines.variomodel(cov.model="sph",cov.pars=c(0.05,3.2),nugget=0,lwd=2) 
lines.variomodel(cov.model="sph",cov.pars=c(0.05,2.8),nugget=0,lwd=2,col="red") 
legend("bottomright",c("fit by eye","theoretical"),col=c("black","red"), lwd=2) 

theta <- c(0, pi/9, pi/4.5, pi/3, pi/2.25, pi/18, pi/6, pi/3.6, pi/2.571)  
range <- c(7, 2, 6, 1.7, 7, 7, 7, 7, 5.9)  
x1 <- cos(theta[1:5])*range[1:5]  
y1 <- sin(theta[1:5])*range[1:5] 
x2 <- range[6:9]*sin(theta[6:9])  
y2 <- -range[6:9]*cos(theta[6:9]) 
x11 <- -x1  
y11 <- -y1 
x22 <- -x2  
y22 <- -y2 
plot(x1,y1, xlim=c(-10,10), ylim=c(-10,10), xaxt="n", yaxt="n", ylab="y", xlab="x") 
points(x11,y11)  
points(x2,y2)  
points(x22,y22) 
segments(x1,y1, x11, y11)  
segments(x2,y2, x22, y22) 
segments(0, 10, 0, -10,lty=2)  
segments(-10, 0, 10, 0, lty=2) 
#Rotation theta matrix: 
a <- cos(pi/3) 
b <- sin(pi/3) 
xxx <- c(a,b,-b,a) 
#Create the rotation theta matrix: 
Rtheta <- matrix(xxx,nrow=2,ncol=2,byrow=TRUE) 
#Ratio of the two major axes: 
l <- 7/1.7 
yyy <- c(1,0,0,l) 
#Create the scaling matrix: 
Rl <- matrix(yyy, nrow=2, ncol=2, byrow=TRUE) 
#New coordinates: 
loc=cbind(data$lon,data$lat)
loc2 <- t(Rl%*%Rtheta%*%t(loc)) 
data2 <- cbind(loc2,d1$data) 
b2<- as.geodata(data2) 
points(b2)
#Idw prediction of target variable Ksat
q <- summary(d1)
grd <-expand.grid(x=seq(q$coords.summary[1,1], q$coords.summary[2,1], by=0.0625), y=seq(q$coords.summary[1,2], q$coords.summary[2,2],by=0.0625)) 
coordinates(grd)~x+y 
dataidw <- as.data.frame(cbind(data$lon,data$lat,d1$data)) 
names(dataidw)=c("x","y","data") 
idw.out <- idw(data~1, locations=~x+y,dataidw,grd) 
idwbtksat=q1$coefficients[1]+q1$coefficients[2]*grd[,1]+q1$coefficients[3]*grd[,2]+idw.out$var1.pred
idw.out$btksat=idwbtksat
vec.2.Mx <- function( xvec, nrow, ncol ) { Mx.out <- matrix(0, nrow, ncol ) 
for(j in 1:ncol) { 
  for(i in 1:nrow) { 
    Mx.out[i, j] <- xvec[ (j-1)*nrow + i ] } } 
return( Mx.out ) }
qqq <- vec.2.Mx( idw.out$var1.pred, length(seq(q$coords.summary[1,1],q$coords.summary[2,1],by=0.0625)), length(seq(q$coords.summary[1,2],q$coords.summary[2,2],by=0.0625)))  
library(lattice) 
library(raster)
library(rasterVis)
library(mapdata)
library(maptools)
library(rgdal)
boundaries <- readOGR("states_21basic/states.shp")
levelplot(idw.out$btksat~x+y,idw.out, aspect ="iso", main="idw predicted Ksat with trend added",at=c(0,1000,2000,3000,4000,5000))+ layer(sp.polygons(boundaries))
persp(seq(q$coords.summary[1,1],q$coords.summary[2,1],by=0.0625),seq(q$coords.summary[1,2], q$coords.summary[2,2],by=0.0625),qqq,xlab="West to East",ylab="South to North")
#Ordinary kriging prediction of target variable Ksat
x <- seq(q$coords.summary[1,1],q$coords.summary[2,1],by=0.0625) 
y <- seq(q$coords.summary[1,2],q$coords.summary[2,2],by=0.0625) 
lx=length(x)
ly=length(y)
xv=rep(x,ly) 
yv=rep(y,each=lx)
in_mat <- as.matrix(cbind(xv,yv)) 
plot(in_mat) 
#krig <- ksline(d1,cov.model="exp",cov.pars=c(1500000,13),nugget = 500000,locations=in_mat) 
krig <- ksline(d1,cov.model="gau",cov.pars=c(1500000,13),nugget = 0,locations=in_mat) 
okbtksat=q1$coefficients[1]+q1$coefficients[2]*in_mat[,1]+q1$coefficients[3]*in_mat[,2]+krig$predict
krig$btksat=okbtksat
levelplot(krig$btksat~locations[,1]+locations[,2],krig, aspect ="iso", main="ok predicted Ksat with trend added",at=c(0,1000,2000,3000,4000,5000))+ layer(sp.polygons(boundaries))
levelplot(krig$krige.var~locations[,1]+locations[,2],krig, aspect ="iso", main="ok predicted Ksat variance")+ layer(sp.polygons(boundaries))
persp(x,y,matrix(krig$btksat,length(x),length(y)),xlab="x coordinate", ylab="y coordinate", zlab="Predicted values of z", main="Predicted values with trend added") 
persp(x,y,matrix(sqrt(krig$krige.var),length(x),length(y)),xlab="x coordinate", ylab="y coordinate", zlab="Predicted values of z", main="Perspective plot of the standard errors")
#Universal kriging
#krig <- ksline(d1,cov.model="exp",cov.pars=c(1500000,13),nugget = 0,locations=in_mat, m0="kt", trend=1) 
krig <- ksline(d1,cov.model="gau",cov.pars=c(2000000,8),nugget = 1500000,locations=in_mat, m0="kt", trend=1) 
ukbtksat=q1$coefficients[1]+q1$coefficients[2]*in_mat[,1]+q1$coefficients[3]*in_mat[,2]+krig$predict
krig$btksat=ukbtksat
levelplot(krig$btksat~locations[,1]+locations[,2],krig, aspect ="iso", main="uk predicted Ksat with trend added",at=c(0,1000,2000,3000,4000,5000))+ layer(sp.polygons(boundaries))
levelplot(krig$krige.var~locations[,1]+locations[,2],krig, aspect ="iso", main="uk predicted Ksat variance")+ layer(sp.polygons(boundaries))
persp(x,y,matrix(krig$btksat,length(x),length(y)),xlab="x coordinate", ylab="y coordinate", zlab="Predicted values of z", main="Predicted values with trend added") 
persp(x,y,matrix(sqrt(krig$krige.var),length(x),length(y)),xlab="x coordinate", ylab="y coordinate", zlab="Predicted values of z", main="Perspective plot of the standard errors")


######################################
#Cokriging with other co-located variable
######################################
#Begin with the target variable Ksat:
data1 <- as.data.frame(cbind(data$lon,data$lat,c1$detKsat,c2$detmoist_s3,c3$detelev,c4$detbubble,c5$detbulk_dens,c6$detwpwp_fract))
colnames(data1)=c("x","y","detKsat","detmoist_s3","detelev","detbubble","detbulk_dens","detwpwp_fract")
g1 <- gstat(id="detKsat", formula = detKsat~1, locations = ~x+y, data = data1) 
#Append detmoist_s3:
g1 <- gstat(g1,id="detmoist_s3", formula = detmoist_s3~1, locations = ~x+y, data = data1)
#Append detelev:
g1 <- gstat(g1,id="detelev", formula = detelev~1, locations = ~x+y, data = data1)
g1 <- gstat(g1,id="detbubble", formula = detbubble~1, locations = ~x+y, data = data1)
g1 <- gstat(g1,id="detbulk_dens", formula = detbulk_dens~1, locations = ~x+y, data = data1)
g1 <- gstat(g1,id="detwpwp_fract", formula = detwpwp_fract~1, locations = ~x+y, data = data1)
plot(variogram(g1))
#==================================================????
#Use only target variable to model the variogram:
g <- gstat(id="detKsat", formula = detKsat~1, locations = ~x+y, data = data1) 
#Plot the variogram of the target variable:
plot(variogram(g,cutoff=13))
#Fit a model to the sample variogram:
#v.fit <- vgm(15000,"Pow",1.95,1500000)
v.fit <- vgm(2000000,"Gau",8,1500000)
v.fit <- vgm(1500000,"Exp",13,500000)
#v.fit <- fit.variogram(variogram(g,cutoff=13), vgm(15000,"Pow",1.95,1500000),fit.method=2)
#Plot the model variogram:
plot(variogram(g,cutoff=13),v.fit)
#==================================================????
#Now go back the gstat object with the three variables (target variable and two co-located variables):
#Fit a model variogram to all the variograms:
vm <- variogram(g1) 
vm.fit <- fit.lmc(vm, g1, model=v.fit)
#Plot the fitted variograms to all the sample variograms:
plot(vm,vm.fit)
#Cokriging predictions:
#Create the grid for predictions:
x.range <- as.integer(range(data1[,1])) 
y.range <- as.integer(range(data1[,2])) 
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.0625), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.0625)) 
ck <- predict(vm.fit, grd) 
######################################
#Cross validation: compare ordinary kriging with cokriging
######################################
#Read the data:
#Universal KRIGING:
uk1 <- krige(id="detKsat",detKsat~x+y, locations=~x+y, 
            model=v.fit, 
            data=data1, newdata=grd) 
#Cross validation:
cv_uk<- krige.cv(detKsat~x+y,data=data1, locations=~x+y, 
                 model=v.fit,nfold=nrow(data1)) 
#CO-KRIGING:
ck <- predict(vm.fit, grd) 
#Perform cross-validation:
cv_ck <- gstat.cv(vm.fit)
#COMPARE ORDINARY KRIGING WITH CO-KRIGING:
sum(cv_uk$residual^2)/nrow(data1)
sum(cv_ck$residual^2)/nrow(data1)

dataori=read.table("all_soilparamCA.txt")
dataori=dataori[dataori[,3]<=42,]
dataori=dataori[abs(dataori[,4])>=114,]
x.rangeori <- as.integer(range(dataori[,4]))
y.rangeori <- as.integer(range(dataori[,3]))
Ksat=dataori[,c(4,3,13)]
colnames(Ksat)=c("x","y","ksat")
levelplot(Ksat$ksat~x+y,Ksat, aspect ="iso", main="Ksat origin value",at=c(0,1000,2000,3000,4000))+ layer(sp.polygons(boundaries))
##Back transformation
ukbtksat=q1$coefficients[1]+q1$coefficients[2]*grd[,1]+q1$coefficients[3]*grd[,2]+uk$detKsat.pred
uk$btksat=ukbtksat
levelplot(uk$btksat~x+y,uk, aspect ="iso", main="Uk predictions added trend back",at=c(0,1000,2000,3000,4000))+ layer(sp.polygons(boundaries))
levelplot(ck$detKsat.pred~x+y,ck, aspect ="iso", main="Cokriging predictions")
ckbtksat=q1$coefficients[1]+q1$coefficients[2]*grd[,1]+q1$coefficients[3]*grd[,2]+ck$detKsat.pred
uk$btksat=ukbtksat
levelplot(uk$btksat~x+y,uk, aspect ="iso", main="Uk predictions added trend back",at=c(0,1000,2000,3000,4000))+ layer(sp.polygons(boundaries))
levelplot(ck$detKsat.pred~x+y,ck, aspect ="iso", main="Cokriging predictions")
