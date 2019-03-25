library(quantmod)
getSymbols("2383.TW") ##來自yahoo finance
TSMC<-get("2383.TW")  ##令為字串，不然直接打2330.TW會被認為是數字

chartSeries(TSMC["2008-01-01/2015-11-01"],theme="white",type=c("line"))  ##畫圖

#######################################################移動平均策略
ma_20<-runMean(TSMC[,4],n=20)   
ma_60<-runMean(TSMC[,4],n=60)
addTA(ma_20,on=1,col="blue",lwd=2)
addTA(ma_60,on=1,col="red",lwd=2)

position<-Lag(ifelse(ma_20>ma_60, 1,0))     
return<-ROC(Cl(TSMC))*position
return<-return['2008-01-02/2015-11-13']
return<-exp(cumsum(return))
plot(return)


########################################加上MACD
chartSeries(TSMC["2012-01::2012-06"],theme="white",type=c("candlesticks"))

addMACD()

###############################加上Bollinger
addBBands()

######################################MACD策略
library(quantmod)
getSymbols("2330.TW") ##來自yahoo finance
TSMC<-get("2330.TW")  ##令為字串，不然直接打2330.TW會被認為是數字

chartSeries(TSMC['2007-01-02/2015-11-13'],theme="white",type=c("candlesticks"))
addMACD()



ema_12<-EMA(TSMC[,4],n=12)
ema_26<-EMA(TSMC[,4],26)
DIF<-ema_12-ema_26
MACD<-EMA(DIF,9)

##position<-Lag(ifelse(DIF>(MACD*1.1),1,ifelse(DIF>MACD,1,0)))
position<-Lag(ifelse(DIF>MACD,1,0))

return<-ROC(Cl(TSMC))*position
return<-return['2008-01-02/2015-11-13']
return<-exp(cumsum(return))
plot(return)

########################DIF超過超過MACD10% 持續兩天才買
library(quantmod)
getSymbols("2330.TW") ##來自yahoo finance
TSMC<-get("2330.TW")  ##令為字串，不然直接打2330.TW會被認為是數字

ema_12<-EMA(TSMC[,4],n=12)
ema_26<-EMA(TSMC[,4],n=26)
DIF<-ema_12-ema_26
MACD<-EMA(DIF,9)

v=(DIF-MACD)/MACD
vv_abs<-abs(v)
position<-Lag(ifelse((DIF>MACD) & ( vv_abs>0.1) ,1,ifelse((DIF<MACD) & (vv_abs>0.1),0,1)))
##position<-Lag(ifelse((DIF>MACD) &( vv_abs>0.1) ,1,0))
##position<-Lag(ifelse(DIF>MACD,1,0))

return<-ROC(Cl(TSMC))*position
return<-return['2008-01-02/2015-11-13']
return<-exp(cumsum(return))
plot(return)

############################################################Bollinger Bands
library(TTR)
library(quantmod)

getSymbols("9941.TW")
ACC<-get("9941.TW")

close<-Cl(ACC)
ma_20<-SMA(Cl(ACC),n=20)
sd<-runSD(close,n=20)
up<-ma_20+2*sd
down<-ma_20-2*sd
par(mai=c(1,1,1,1))
chartSeries(yrange = c(60,90),ACC["2015-01-01/2015-10-30"],type=c("candlesticks"),theme="white")
addTA(up,on=1)
addTA(down,on=1)

####addBBands() 內建bollinger
###zooom() 可選取放大看

#漲破up買，跌破down放空###position<-Lag(ifelse(close>up,1,ifelse(close<down,-1,0)))
position<-Lag(ifelse(close>up,1,0))  #漲破up買#
return<-ROC(Cl(ACC))*position
return<-return['2008-01/2015-10']
return<-exp(cumsum(return))
par(mai=c(1,1,0.5,1))
plot(return,type = "l")


#######################################################Stochastic Oscillator 
library(TTR)
library(quantmod)

getSymbols("9941.TW")
ACC<-get("9941.TW")
max_9<-runMax(Hi(ACC),9)
min_9<-runMin(Lo(ACC),9)
RSV<-(Cl(ACC)-min_9)/(max_9-min_9)*100
#############################################要用迴圈寫
for(i in 9:length(RSV)-8){
    if(i==9){
   K<-2/3*50+1/3*RSV[9]
   aa<-K
   }
  else{
    K<-2/3*K+1/3*RSV[i]
     bb<-K
     aa<-rbind(aa,bb)
     }
  }


K<-K*2/3+RSV*1/3
D<-50
D<-Lag(D)*2/3+K*1/3


chartSeries(Cl(ACC)["2015-05/2015-11"],theme="white")  
add
addTA(K,col="red")
addTA(D,on=2)
  

