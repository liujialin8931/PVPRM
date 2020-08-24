################ R codes for VPRM_SIF fitting

########developed by Jia-lin Liu

################# Simulation
####  original codes
library(modelr)

tempRLow = 5
Topt = 20
Tmax = 40
Tmin = 0

Tsc <- function(TairC){
  tsc = ((TairC-Tmin)*(TairC-Tmax))/
    (((TairC-Tmin)*(TairC-Tmax))-(TairC-Topt)^2)
  tsc[tsc<0] <- 0 
  return(tsc)}

TairR = xxx$tair
if(!is.na(tempRLow))TairR[which(TairR<tempRLow)]<-tempRLow
Tscale = Tsc(TairC=xxx$tair) 


NEEOBS <-xxx$nee
PAR <- xxx$par
SIF <- xxx$sif
GPP <- Tscale*SIF*PAR
RSP1 <- TairR

PARscale<-function(PAR,PAR0){(PAR0/(PAR0+PAR))}

lambda_init = -0.08
par0_init = 620
alpha_init = 0.01
beta_init = 0

lna<-!is.na(NEEOBS+GPP+RSP1+PAR) 
dataframe.0<-data.frame(NEEOBS,GPP,RSP1,PAR)

nee.nls<-nls(NEEOBS~lambda*GPP*PARscale(PAR,PAR0)+(alpha*RSP1+beta),
             data=dataframe.0,trace=T,subset=lna,
             start=list(PAR0=par0_init,lambda=lambda_init,
                        alpha=alpha_init,beta=beta_init ))

dataframe.new<-data.frame(GPP,RSP1,PAR)
NEEVPRM.0<-predict(nee.nls,newdata=dataframe.new)
plot(NEEOBS)
points(NEEVPRM.0,col='blue')

rmse(nee.nls,NEEOBS)
rsquare(nee.nls,NEEOBS)

##################### Calculation


library(modelr)

tempRLow = 5
Topt = 20
Tmax = 40
Tmin = 0

Tsc <- function(TairC){
  tsc = ((TairC-Tmin)*(TairC-Tmax))/
    (((TairC-Tmin)*(TairC-Tmax))-(TairC-Topt)^2)
  tsc[tsc<0] <- 0 
  return(tsc)}

TairR = xxx$tair
if(!is.na(tempRLow))TairR[which(TairR<tempRLow)]<-tempRLow
Tscale = Tsc(TairC=xxx$tair) 

PAR <- xxx$par
SIF <- xxx$sif
GPP <- Tscale*SIF*PAR
RSP1 <- TairR

PARscale<-function(PAR,PAR0){(PAR0/(PAR0+PAR))}

NEEVPRM<--0.02*GPP*PARscale(PAR,756.88)+(0.086*RSP1+0.33)

Reco<-0.086*RSP1+0.33

dataa<-data.frame(NEEVPRM,Reco)

write.csv(dataa,"dddd.csv")


########### DS season

TairR = xxx$tsoil
NEEOBS <-xxx$nee
RSP1 <- TairR

alpha_init = 0.01
beta_init = 0

lna<-!is.na(NEEOBS) 
dataframe.0<-data.frame(NEEOBS)

nee.nls<-nls(NEEOBS~alpha*RSP1+beta,
             data=dataframe.0,trace=T,subset=lna,
             start=list(PAR0=par0_init,lambda=lambda_init,
                        alpha=alpha_init,beta=beta_init ))

dataframe.new<-data.frame(GPP,RSP1,PAR)
NEEVPRM.0<-predict(nee.nls,newdata=dataframe.new)
plot(NEEOBS)
points(NEEVPRM.0,col='blue')

rmse(nee.nls,NEEOBS)
rsquare(nee.nls,NEEOBS)