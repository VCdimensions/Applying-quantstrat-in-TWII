###########################
####將時間格式轉為數字格式
############################
TimeToNumber <- function(Time)
{
  T<-as.numeric(substring(Time,1,2))*360000+as.numeric(substring(Time,3,4))*6000+
    as.numeric(substring(Time,5,6))*100+as.numeric(substring(Time,7,8))
  return(T)
}

NumberToTime <- function(T)
{
  T1<-sprintf("%02d",T%%100)
  TT<-trunc(T/100)
  T2<-TT%%60
  TT<-trunc(TT/60)
  T3<-TT%%60
  T4<-trunc(TT/60)
  
  T2<-sprintf("%02d",T2)
  T3<-sprintf("%02d",T3)
  T4<-sprintf("%02d",T4)
  return(paste(T4,T3,T2,T1,sep=''))
}

###########################
####最新價格
############################
LatestPrice <- function(A,DDate,current_time)
{
  colnames(A)<-c("Date","Time","Name","Price","Amount")
  AA<-subset(A,select=c(Time,Price,Amount),subset=(Date == DDate & Time >= 08450000 & Time <=as.numeric(current_time)))
  
  if (is.na(AA[1,2]))
  {
    P<-0
  } else {
    P<-AA[nrow(AA),2]
  }
  return(P)
}

#############################
####n日中的開盤最高最低收盤價
#############################


nTickToOHLC <- function(A,DDate,init_time,final_time)
{
  colnames(A)<-c("Date","Time","Name","Price","Amount")
  AA<-subset(A,select=c(Time,Price,Amount),subset=(Date == DDate & Time >= as.numeric(init_time) & Time <=as.numeric(final_time)))
  
  if (is.na(AA[1,2]))
  {
    AA<-c(-1,-1,-1)
    dim(AA)<-c(1,3)
  }
  O=AA[1,2]
  H=O
  L=O
  Amounts=0
  #       print((is.na(AA[1,2])))
  for (i in 1:nrow(AA))
  {
    C=AA[i,2]
    Amounts=Amounts+AA[i,3]
    if (AA[i,2]>H)
      H=AA[i,2]
    if (AA[i,2]<L)
      L=AA[i,2]
  }
  output<-list(O,H,L,C,Amounts)
  return(output)
}

#############################
####移動平均
#############################

MA <- function(NUM,period,A,DDate,time1)
{
  colnames(A)<-c("Date","Time","Name","Price","Amount")
  MA_sum<-0
  i<-1
  j<-0
  while (i <=NUM) {
    Price<-0
    mm<-TimeToNumber(time1)+as.numeric(period)*100*(j-1)
    ntime1<-as.numeric(NumberToTime(mm))
    mm<-TimeToNumber(time1)+as.numeric(period)*100*j
    ntime2<-as.numeric(NumberToTime(mm))
    AA<-subset(A,select=c(Time,Price,Amount),subset=(Date == DDate & Time >= ntime1 & Time <= ntime2))
    
    if ( !is.na(AA[1,2]) ) {
      Price<-AA[nrow(AA),2]
      MA_sum<-MA_sum+Price
      i<-i+1
    }
    #cat(ntime2,Price,"\n")
    j<-j-1
  }
  return(MA_sum/NUM)
}


#########################將時間轉換為POSIXct格式
Time<-as.POSIXct(strptime(paste(A[,1],sprintf("%08d",A[,2])), "%Y-%m-%d %H%M%S"))
#########################繪出圖形
plot(Time,A[,5]-A[,7],type='l')
Titlename<-paste0(Date," Bull Volume-Bear Volume")
title(main = list(Titlename, cex = 1.5,col = "blue", font = 3))



##################################
TT<-paste0(substring(E_time[i],1,2),":",substring(E_time[i],3,4),":",substring(E_time[i],5,6))
MM<-cbind(MM,paste0(Date," ",TT,",",nOHLC(A,Date,S_time[i],E_time[i])))

require(quantmod)
titles<-paste0(time_step/100," seconds K-line on ",Date)
z<-read.zoo(text=MM,sep=",",header=F,tz=' ',format="%Y-%m-%d %H:%M:%S")
colnames(z)<-c("Open","High","Low","Close","Volume","Adjusted")
chartSeries(z, theme = chartTheme("white", up.col='red',dn.col='green'),name=titles,show.grid = TRUE)






