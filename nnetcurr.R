

# USDCOP

setSymbolLookup(USDCOP=list(name='USD/COP',src='oanda',from=as.Date('2015-05-11')))
getSymbols(c('USDCOP'))
#functions


myATR <- function(x) ATR((x))[,'atr']
mySMI <- function(x) SMI((x))[,'SMI']
myADX <- function(x) ADX((x))[,'ADX']
myBB <- function(x) BBands((x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x))
myCLV <- function(x) EMA(((x)))
myMACD <- function(x) MACD((x))
myVolat <- function(x) volatility((x),calc="garman")
T.ind<-function(x){Next(x,k=8)}

library(randomForest)
data.model <- specifyModel(T.ind(USDCOP) ~   
RSI(USDCOP)+volatility(USDCOP)+aroon(USDCOP)+MACD(USDCOP)+runMean(USDCOP)+runSD(USDCOP))  
                              
                                 
set.seed(1234)



Tdata.train <- as.data.frame(modelData(data.model,
                                       data.window=c('2015-05-11','2016-04-07')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                                              data.window=c('2000-01-01','2009-09-15'))))
Tform <- as.formula('T.ind.USDCOP ~ .')

#nnet
library(nnet)
library(DMwR)

save<-NA
for (i in 1:200) {
  
norm.data <- scale(Tdata.train)
nn <- nnet(Tform,norm.data[1:292,],size=10,decay=0.01,maxit=1000,linout=T,trace=F)
norm.preds <- predict(nn,norm.data[292:293,])
preds <- unscale(norm.preds,norm.data)
save[i]<-preds[2]

}

prediction<-as.ts(preds)
preds
plot(USDCOP)




