library(ncdf4)
nc <- nc_open("pres.sfc.mon.mean.nc")
nc
summary(nc)
library(raster)
LonIdx <- which( nc$dim$lon$vals > 235 & nc$dim$lon$vals < 293)
LatIdx <- which( nc$dim$lat$vals > 0 & nc$dim$lat$vals < 50)
lon=nc$dim$lon$vals[LonIdx]
lat=nc$dim$lat$vals[LatIdx]
test <- ncvar_get(nc,"pres")
pres <- ncvar_get( nc, "pres")[ LonIdx, LatIdx,]
nc_close(nc)
dim=dim(pres)
rmannual=array(0,dim=dim)
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    yr=rep(1871:2012,each=12)
    mon=rep(c(1:12),2012-1871+1)
    time=cbind(yr,mon)
    prestmp=pres[i,j,]
    preswd=cbind(time,prestmp)
    presclim=rep(0,12)
    for(k in 1:12){
      presclim[k]=mean(preswd[which(preswd[,2]==k),3])
    }
    presclimnew=rep(presclim,2012-1871+1)
    presnew=prestmp-presclimnew
    rmannual[i,j,]=presnew
  }
}
datanew=array(0,dim=c(dim[1]*dim[2],dim[3]))
for(i in 1:dim[1]){
  for(j in 1:dim[2]){
    datanew[(i-1)*dim[2]+j,]=rmannual[i,j,]
  }
}
C=(datanew)%*%t(datanew)/dim[3]
eig <- eigen(C)
sorteig <- sort(eig$values,decreasing = TRUE)
variance=sorteig/sum(sorteig)
m=length(datanew[,1])
meff=(m*mean(sorteig))^2/(m*mean(sorteig^2))
sigmalam=sorteig*sqrt(2/meff)
library(ggplot2)
x=c(1:10)
y=as.numeric(sorteig[1:10])
sd=sigmalam[1:10]
qplot(x,y,xlab="index",ylab="lambda")+geom_errorbar(aes(x=x, ymin=y-sd, ymax=y+sd), width=0.25)
#plot (x, y)
#segments(x,y-sd,x,y+sd)
#epsilon <- 0.02
#segments(x-epsilon,y-sd,x+epsilon,y-sd)
#segments(x-epsilon,y+sd,x+epsilon,y+sd)
i=1
summ=sum(variance[1:i])
while(summ<0.8){
  i=i+1
  summ=sum(variance[1:i])
}
i
#For example choose the first 5
eigvec <- eig$vectors[,1:5]
times <- t(eigvec)%*%datanew
vec2mx <- function(vec){
  mat <- array(0,dim=c(dim[1],dim[2]))
  for(i in 1:dim[1]){
    for(j in 1:dim[2]){
      mat[i,j]=vec[(i-1)*dim[2]+j]
    }
  }
  return(mat)
}
mat1 <- vec2mx(eigvec[,1])
mat2 <- vec2mx(eigvec[,2])
mat3 <- vec2mx(eigvec[,3])
mat4 <- vec2mx(eigvec[,4])
mat5 <- vec2mx(eigvec[,5])
rotate <- function(x) {t(apply(x, 2, rev))}
library(lattice)
library(raster)
library(rasterVis)
library(maps)
library(mapdata)
library(maptools)
## raster
plotmat <- function(mat,x){
  r <-raster(rotate(mat1)[,24:1],  xmn=-124, xmx=-68, ymn=2, ymx=48, 
             crs=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )
  myRaster <- raster(xmn=-124, xmx=-68, ymn=2, ymx=48, ncol=29,nrow=24)
  myRaster <- init(myRaster, run)
  ## polygon shapefile
  ext <- as.vector(extent(myRaster))
  boundaries <- map('worldHires', fill=TRUE,
                    xlim=ext[1:2], ylim=ext[3:4],
                    plot=FALSE)
  ## read the map2SpatialPolygons help page for details
  IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
  bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                               proj4string=CRS(projection(myRaster)))
  levelplot(r,margin=FALSE,main=paste0("EOF",x,"SLP", var=",round(variance[x],3")) + layer(sp.polygons(bPols))
}
plot(times[1,],type='l',xlab="Months since Jan 1871",ylab=NA,main="PC1TS")
par(mfrow=c(2,5))
plotmat(mat1,2)
run