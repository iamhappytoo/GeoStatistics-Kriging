##(a)##
setwd("C:/Users/BANZH/Downloads/")
data=read.table("tmax_01feb2019.csv",sep=",",header=TRUE,stringsAsFactors = FALSE)
data=data[which(!is.na(data[,6])),]
#Choose the location at north California
data1=data[which((abs(data[,4])>=119)&(abs(data[,4])<=122)),]  
data2=data1[which((data1[,3]>=38.9)&(data1[,3]<=42)),]
length(data2[,2])
##(b)
aa <- as.data.frame(cbind(data2$lat,data2$lon,data2$TMAX))
test_ind=sample(1:100,20)
test_data=aa[test_ind,]
use_data=aa[-test_ind,]
library(geoR)
b <- as.geodata(use_data)
summary(b)
vario=variog(b)
plot(vario)
v.fit <- variofit(vario,ini.cov.pars=c(13,2),cov.model="sph",fix.nugget=FALSE,nugget=0)
v.fit
lines(v.fit)
##So the fitted parameter for the spherical variogram: nugget is c0=2.2164, partial sill is c1=10.2971, 
#range is alpha=0.6797
getvario <- function(h){
  alpha=0.6797
  c0=2.2164
  c1=10.2971
  if(h==0){
   gam=0
  }else if((h>0) & (h<=alpha)){
   gam=c0+c1*(1.5*(h/alpha)-0.5*(h/alpha)^3)
  }else if(h>=alpha){
   gam=c0+c1
  }
  return(gam)
}
Z=rep(0,20)
varuk=rep(0,20)
for(cnt in 1:20){
  coor=rbind(test_data[cnt,1:2],use_data[,1:2])
  distmat=array(0,dim=c(81,81))
  for(i in 1:81){
    for(j in 1:81){
      distmat[i,j]=sqrt(sum((coor[i,1:2]-coor[j,1:2])^2))
    }
  }
  gammat=array(0,dim=c(83,83))
  for(i in 1:80){
    for(j in 1:80){
      gammat[i,j]=getvario(distmat[(i+1),(j+1)])
    }
  }
  gammat[1:80,81]=use_data[,2]
  gammat[1:80,82]=use_data[,1]
  gammat[1:80,83]=rep(1,80)
  gammat[81,1:80]=use_data[,2]
  gammat[82,1:80]=use_data[,1]
  gammat[83,1:80]=rep(1,80)
  gammat[81:83,81:83]=0
  distvec=array(0,dim=c(80,1))
  gamvec=array(0,dim=c(83,1))
  for(i in 1:80){
    distvec[i]=sqrt(sum((coor[1,]-coor[i+1,])^2))
    gamvec[i]=getvario(distvec[i])
  }
  gamvec[81]=coor[1,2]
  gamvec[82]=coor[1,1]
  gamvec[83]=1
  W=solve(gammat)%*%gamvec
  Z[cnt]=sum(W[1:80]*use_data[,3])
  varuk[cnt]=sum(W[1:80]*gamvec[1:80])+W[83]+W[82]*gamvec[82]+W[81]*gamvec[81]
}
