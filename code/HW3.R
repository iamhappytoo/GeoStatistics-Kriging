##1a##
setwd("C:/Users/BANZH/Downloads")
Tmax=read.table("USW00023174_TMAX_clean.csv",sep=",",header=T)
Tmin=read.table("USW00023174_TMIN_clean.csv",sep=",",header=T)
ind1 <- which(Tmax[,5]!=-999.9)
ind2 <- which(Tmin[,5]!=-999.9)
ind <- intersect(ind1,ind2)
Tmax=Tmax[ind,]
Tmin=Tmin[ind,]
Tmax_DJF <- Tmax[which((Tmax[,2]==12)|(Tmax[,2]==1)|(Tmax[,2]==2)),5]
Tmin_DJF <- Tmin[which((Tmin[,2]==12)|(Tmin[,2]==1)|(Tmin[,2]==2)),5]
Date_DJF <- Tmax[which((Tmax[,2]==12)|(Tmax[,2]==1)|(Tmax[,2]==2)),1:3]
for(i in 1:length(HDD)){
  HDD[i]=max(15-0.5*(Tmax_DJF[i]+Tmin_DJF[i]),0)
}
HDD_wd <- cbind(Date_DJF,HDD)
lDJF <- length(unique(Date_DJF[,1]))-1
seas_HDD <- rep(0,lDJF)
for(i in 1:lDJF){
  D <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i]))*100+12),]
  J <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i+1])*100+1)),]
  F <- HDD_wd[which((HDD_wd[,1]*100+HDD_wd[,2])==((unique(Date[,1])[i+1])*100+2)),]
  seas_HDD[i]=mean(c(D[,4],J[,4],F[,4]))
}###The winter HDD is stored in seas_HDD
###Download climate indices
nyr=2018-1944+1
month=rep(seq(1,12),nyr)
NINO3.4=read.table("NINO3.4.txt",skip=40)
NINO3.4=NINO3.4[which(NINO3.4[,1]>=1944),]
year=c(floor(NINO3.4[,1]),2018)
NINO3.4=cbind(year[1:length(NINO3.4[,2])],month[1:length(NINO3.4[,2])],NINO3.4[,2])
NINO4=read.table("NINO4.txt",skip=40)
NINO4=NINO4[which(NINO4[,1]>=1944),]
NINO4=cbind(year[1:length(NINO4[,2])],month[1:length(NINO4[,2])],NINO4[,2])
SOI=read.table("SOI Jones.txt",skip=40)
getindex <- function(index){
  index1=index[which(index[,1]>=1944),]
  tmp=rep(0,12*nyr)
  for(i in 1:nyr){
    tmp[((i-1)*12+1):(i*12)]=as.numeric(index1[i,2:13])
  }
  index2=cbind(year[1:length(tmp)],month[1:length(tmp)],tmp)
}
SOI=getindex(SOI)
PDO=read.table("PDO.txt",skip=40)
PDO=getindex(PDO)
NAO=read.table("NAO.txt",skip=40)
NAO=getindex(NAO)
AMO=read.table("AMO.txt",skip=40)
AMO=getindex(AMO)
landT=read.table("landtemp.txt",skip=40)
landT=landT[which(landT[,1]>=1944),]
###create predictors for winter HDD###
getSON <- function(index){
  index=index[which(index[,3]!=-999.9),]
  tmp=index[which((index[,2]==9)|(index[,2]==10)|(index[,2]==11)),]
  l=length(unique(tmp[,1]))-1
  out=rep(0,l)
  for(i in 1:l){
    out[i]=mean(tmp[which(tmp[,1]==unique(tmp[,1])[i]),3])
  }
  out1 <- cbind(unique(tmp[,1])[2:(l+1)],out)
  return(out1)
}
NINO3.4SON=getSON(NINO3.4)
NINO4SON=getSON(NINO4)
SOISON=getSON(SOI)
NAOSON=getSON(NAO)
PDOSON=getSON(PDO)
AMOSON=getSON(AMO)
landTSON=getSON(landT)
seas_HDD=cbind(landTSON[,1],seas_HDD)
Tmin_SON=getSON(Tmin[c(1,2,5)])
Tmax_SON=getSON(Tmax[c(1,2,5)])
SON <- cbind(NINO3.4SON,NINO4SON[,2],SOISON[,2],NAOSON[,2],PDOSON[,2],AMOSON[,2],landTSON[,2],Tmin_SON[,2],Tmax_SON[,2])
k=ncol(SON)
n=nrow(SON)
tmpSON=array(0,dim=c(n,k))
pdSON=array(0,dim=c(n,k+1))
for(i in 1:k){
  tmpSON[,i]=(SON[,i]-mean(SON[,i]))/sd(SON[,i])
}
pdSON=as.data.frame(cbind(seas_HDD[,2],tmpSON))
names(pdSON)=c("HDD","time","NINO3.4","NINO4","SOI","NAO","PDO","AMO","landT","Tmin","Tmax")
##Question 1b##
library(glmnet)
xminus <- seas_HDD[1:73,2]
xplus <- seas_HDD[2:74,2]
r1 <- sum((xminus-mean(xminus))*(xplus-mean(xplus)))/((sqrt(sum((xminus-mean(xminus))^2)))*(sqrt(sum((xminus-mean(xminus))^2))))
n1 <- n*(1-r1)/(1+r1)
expon <- 2/3*(1-n1/n) ###Now the Wilks 5.36 becomes L=(101-L)^(expon)
fL <- function(L,expon){
  n=100
  result <- L-(n-L+1)^expon
  return(result)
}
##Check the approximate location of root of fL##
L=seq(-100,100,0.1)
plot(L-(n-L+1)^expon,type='l') ##root of fL is between 5 and 7
eps=1e-7
max=max(L)
min=min(L)
tmp=0.5*(max+min)
check <- fL(tmp,expon)
while(abs(check)>eps){
  if(check<0){
    min=tmp
    tmp=0.5*(max+min)
  }else{
    max=tmp
    tmp=0.5*(max+min)
  }    
  check <- fL(tmp,expon)
}
L <- round(tmp) 
###moving window cross validation with overlap###
nblock <- floor(74/L)
lambda=seq(0,5,by=0.01)
MSE=array(0,dim=c(length(lambda),nblock))
meanMSE=rep(0,length(lambda))
for(i in 1:length(lambda)){
  for(j in 1:nblock){
    if(j==1){
      xtest=pdSON[1:L,]
      xtrain=pdSON[(L+1):74,]
    }else if(j>1){
      xtest=pdSON[((j-1)*L+1):(j*L),]
      xtrain=rbind(pdSON[1:((j-1)*L),],pdSON[(j*L+1):74,])
    }
    ridge_mod = glmnet(as.matrix(xtrain[,2:11]), xtrain[,1], alpha=0, lambda = lambda[i], nlambda=1)
    ridge_pred = predict(ridge_mod, newx = as.matrix(xtest[,2:11]))
    MSE[i,j]=sqrt(mean((ridge_pred - xtest[,1])^2))
  }
  meanMSE[i]=mean(MSE[i,])
}
lam=lambda[which(meanMSE==min(meanMSE))]
###1c###
plot(x=lambda,y=meanMSE,type='l',col="green",lwd=2)
###1d###
ridge_mod = glmnet(as.matrix(pdSON[,2:11]), pdSON[,1], alpha=0, lambda = lam, nlambda=1)
ridge_mod$beta



###Question 2a###



