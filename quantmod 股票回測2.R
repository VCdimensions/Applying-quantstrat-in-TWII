####################
#####簡單繪圖
####################
library(quantmod)
library(TTR)
#getSymbols("YHOO",src="google")#預設用YAHOO，改src="google" 用google #要改在google上代號
getSymbols("^TWII")
has.OHLC(TWII) #是否有開盤最高最低收盤資料
has.Vo(TWII) #是否有成交量
chartSeries(Cl(TWII))
chartSeries(Cl(TWII),TA = NULL) #不畫成交量
addTA(runMin(TWII,30),on=1,col="red") #on=1畫在第一張圖
chartSeries(last(TWII,400)) #近四百天
addSAR()
OpCl(TWII) #每天開盤價到收盤價變動百分比
HiCl(TWII) #每天最高價到收盤價變動百分比
dailyReturn(TWII) #每日報酬 今天收盤價和作天收盤價變動率  也可以用ClCl(TWII)算
weeklyReturn(TWII) ; barplot(weeklyReturn(TWII)) #周報變動率畫圖
barplot(monthlyReturn(TWII))

barplot(OpCl(TWII)) #畫每日開盤收盤變動率
barplot(HiCl(TWII)) #畫每日最高價和收盤價變動率 

Lag(Cl(TWII)) #落後一期
Lag(Cl(TWII),c(1,2,3)) #製造出落後12
Next(OpCl(TWII)) #往前一期

############畫移動平均 和 平均標準差
getSymbols("IVV",from="2013-08-01")
chartSeries(IVV)
IVV_mean_6 <- runMean(IVV[,4],n=6,cumulative = F)
IVV_mean_12 <- runMean(IVV[,4],n=12,cumulative = F)
IVV_mean_24 <- runMean(IVV[,4],n=24,cumulative = F)
addTA(IVV_mean_6,on=1,type="l",col=3)
addTA(IVV_mean_12,on=1,type="l",col=4)
addTA(IVV_mean_24,on=1,type="l",col=5)

IVV_Min <- runMin(IVV[,4],n=6,cumulative = F)
addTA(IVV_Min,on=1,type="l",col=6)
IVV_Max <- runMax(IVV[,4],n=6,cumulative = F)
addTA(IVV_Max,on=1,type="l",col=7)
IVV_SD <- runSD(IVV[,4],n=6,cumulative = F)

##########更簡單畫移動平均
addSMA(n=6)
addSMA(n=12)
addSMA(n=24)
####SAR
addSAR(col="white")
######bollinger band
addBBands()
IVV_Close_BBand <- BBands(IVV[,4])
head(IVV_Close_BBand,21)  ##dn 為下緣 mavg 為移動平均 20天 up 為上緣 pctB為

longposition <- which(diff(sign(IVV[,4] - IVV_Close_BBand[,3]))==2)  #買進點 
shortposition <- which(diff(sign(IVV[,4] - IVV_Close_BBand[,1]))==-2) #賣出點

#事件標記
addTA(IVV[longposition,4],on=1,type="p",col=5,pch=24)
addTA(IVV[shortposition,4],on=1,type="p",col=6,pch=24)

##########################
######抓取資料
##########################
#先查quantmod包中要查的資料在哪個網站 EX.匯率oanda
example("getSymbols.oanda")  #從裡面的example知道查詢格式
getSymbols("USD/EUR",src="oanda")
getSymbols("USD/EUR",src="oanda",from="2011-01-01")

getSymbols("USD/TWD",src="oanda",from="2011-01-01")
chartSeries(USDTWD) #日線
chartSeries(to.weekly(USDTWD)) #變周線

##############################
########策略回測 - runmin
#############################
library(quantmod)
library(TTR)
getSymbols("^TWII",from="2013-01-01",to="2014-12-31") #可直接取指定期間
chartSeries(TWII)
addTA(runMin(TWII,30),col="red",on=1)

signals <- cbind(Cl(TWII),runMin(TWII,30)) #兩個xts bind起來也是xts格式
signals <- cbind(signals,signals[,1]-signals[,2])
colnames(signals) <- c("price","runmin","diff")
sign(signals[,3]) #price 大於runmin 為1 price小於runmin 為-1
diff(sign(signals[,3])) #全部由1和-1組成 取差值不為零為price 和runmin crossover點
signals <- cbind(signals,diff(sign(signals[,3])))
colnames(signals) <- c("price","runmin","diff","sig.")
which(signals[,4]!=0) #哪些位置非零
rownames(as.matrix(signals)) #xts 不能用rownames 要轉為matrix
#cross_date <- rownames(as.matrix(signals))[which(signals[,4]!=0)] #這些日期有crossover
crossdown_date <- rownames(as.matrix(signals))[which(signals[,4] == -2)] #price從上向下穿越runMin的日期
crossup_date <- rownames(as.matrix(signals))[which(signals[,4] == 2)] #price 從下向上穿越runMin的日期
#addTA(signals[cross_date,1],on=1,pch=24,col=5,type="p") #在出現crossover 的日期的股價上點上訊號點
addTA(signals[crossdown_date,1],on=1,pch=24,col=5,type="p") #在出現price從上向下穿越runMin的crossover 的日期的股價上點上訊號點
addTA(signals[crossup_date,1],on=1,pch=23,col="blue",type="p") #在出現price 從下向上穿越runMin的crossover 的日期的股價上點上訊號點



zoomChart("2013-06") #特寫2013六月的圖



