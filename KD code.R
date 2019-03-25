library(quantmod)
getSymbols("2330.TW")
stock<-get("2330.TW")
close<-as.numeric(Cl(stock))
low<-as.numeric(Lo(stock))
high<-as.numeric(Hi(stock))


KD<-function(t,high,low,close){
  for(i in t:length(close)){
    rsv <- ((close[i] - min(low[(i-t+1):i])) / (max(high[(i-t+1):i]) - min(low[(i-t+1):i])) )* 100
    if(i == t){
      k <- (2/3)*50+(1/3)*rsv
      d <- (2/3)*50+(1/3)*k
    }else{
      k <- rbind(k, (2/3)*k[i-t]+(1/3)*rsv)
      d <- rbind(d, (2/3)*d[i-t]+(1/3)*k[i-t+1])
    }
  }
  out <- cbind(c(rep(NA,t-1), k), c(rep(NA,t-1), d))
  out
}
