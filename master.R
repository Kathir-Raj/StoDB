# Momentum Trading concept developed from Traderlounge
# Position need to be taken month on month
# Code Developed by Kathir Rajalingam on November 21, 2017
# Last updated on  Nov 21, 2017
# Database creation can be used everywhere
# Before Execuction
# Step1: Change the Nifty list of stocks in INDX folder
# Step2: Create the folder in the month name, inside that input, output,stockwise and monthwise
# Step3: Create the month name in the format shown in mnt
# Step4: Change the month name in the folder structure , any mistake would throw the error
#========================================================================================
#Set Work directory
# Database Creation from Bhav file
dirpath="C:/Data/R_Work/MasterDB/"
setwd("C:/Data/R_Work/MasterDB/csv")
mypath=getwd()

#declare paths
mnt="Nov 2017"# Input for every month

# Create master db
# Need to be edited for split and bonus

library(data.table)  
files=list.files(path = mypath,pattern = ".csv")
temp=lapply(files, fread, sep=",")
db=rbindlist(temp,fill=T)
db1=db[,-14]
db1$TIMESTAMP = as.Date(db1$TIMESTAMP, format="%d-%b-%Y")
db1=db1[order(db1$TIMESTAMP),]
db1=subset(db1,SERIES=="EQ")
#db1=subset(db1,TIMESTAMP <=as.Date("2017-10-31",format="%Y-%m-%d"))
masterDB=db1                # master DB Created and stored in Master DB directory
write.csv(db1,"C:/Data/R_Work/MasterDB/dbb.csv")

#Adjust stocks for splits and bonus

adjust=function(xp,sym,xdate,ratio){
  model=xp
  datefrom=xdate
  ratio=ratio
  exdate=as.Date(datefrom,format="%d-%m-%y")
  model.adj1=subset(model,SYMBOL==sym)
  model.adj=subset(model.adj1,TIMESTAMP < exdate)
  model.adj$OPEN=model.adj$OPEN*ratio
  model.adj$HIGH=model.adj$HIGH*ratio
  model.adj$LOW=model.adj$LOW*ratio
  model.adj$LAST=model.adj$LAST*ratio
  model.adj$CLOSE=model.adj$CLOSE*ratio
  model.adj$PREVCLOSE=model.adj$PREVCLOSE*ratio
  model.adj$TOTTRDQTY=model.adj$TOTTRDQTY/ratio
  exmodel.adj=subset(model.adj1,TIMESTAMP >= exdate)
  newmodel=rbind(model.adj,exmodel.adj)
}

spl=read.csv("C:/Data/R_Work/MasterDB/split.csv")
rcnt=nrow(spl)
for(i in 1:rcnt){
S11=as.character(spl$S1[i])
ed1=spl$ED[i]
rat1=spl$RAT[i]
new.model=adjust(masterDB,S11,ed1,rat1)
masterDB=subset(masterDB,masterDB$SYMBOL!=S11)
masterDB=rbind(masterDB,new.model)
}

#bonus shares
adjbonus=function(xp,sym,xdate,ratio){
model=xp
exdate=xdate
ratio=ratio
model.adj1=subset(model,SYMBOL==sym)
model.adj=subset(model.adj1,TIMESTAMP < exdate)
model.adj$OPEN=model.adj$OPEN*ratio
model.adj$HIGH=model.adj$HIGH*ratio
model.adj$LOW=model.adj$LOW*ratio
model.adj$LAST=model.adj$LAST*ratio
model.adj$CLOSE=model.adj$CLOSE*ratio
model.adj$PREVCLOSE=model.adj$PREVCLOSE*ratio
model.adj$TOTTRDQTY=model.adj$TOTTRDQTY/ratio
exmodel.adj=subset(model.adj1,TIMESTAMP >= exdate)
newmodel=rbind(model.adj,exmodel.adj)
}

bns=read.csv("C:/Data/R_Work/MasterDB/bonus.csv")
bns=bns[,-2]
bns$rat=bns$Dr/(bns$Nr+bns$Dr)
bns$xbd=as.Date(bns$xbd, format="%d-%m-%y")
#bns$rd=as.Date(bns$rd, format="%d-%m-%y")
rcnt=nrow(bns)
#i=38
for(i in 1:rcnt){
  S11=as.character(bns$stock[i])
  ed1=bns$xbd[i]
  rat1=bns$rat[i]
  new.model=adjbonus(masterDB,S11,ed1,rat1)
  masterDB=subset(masterDB,masterDB$SYMBOL!=S11)
  masterDB=rbind(masterDB,new.model)
}
write.csv(masterDB,"C:/Data/R_Work/MasterDB/masterdb.csv")
db1=masterDB
#========================================================================================

# Filter Stocks based on Equities in Nifty 50 list and create seperate dataframe
#library(LearnBayes)

setwd("C:/Data/R_Work/nov2017/input/")
mypath=getwd()
library(TTR)
library(dplyr)
n50=read.csv("C:/Data/R_Work/indx/n500.csv",header=T)
eq50=data.frame()
eq1=data.frame()
i=1
cnt=0.0
for(i in 1:501){
  eq1=n50$SYMBOL[i]
  eqx=filter(db1,db1$SYMBOL==eq1)
  cnt[i]=nrow(eqx)
  if(cnt[i]>=433)
    write.table(eq1,"n5002.csv", row.names=F,na="NA",append=T, quote= FALSE, sep=",",col.names=F)
  #write_csv(eq1,"n5002.csv", na="NA",append=T)
}

library(readr)
n500s=read.csv2("C:/Data/R_Work/nov2017/input/n5002.csv",header=F)
file.remove("C:/Data/R_Work/nov2017/input/n5002.csv")
colnames(n500s)="SYMBOL"
cnt_n500=0.0
cnt_n500=nrow(n500s)
set.seed(123)
library(xts)
j=0
ll=data.frame()
i=2
Path="C:/Data/R_Work/nov2017/stockwise/"
for(i in 1:cnt_n500){
  eq1=as.character(n500s$SYMBOL[i])
  eq50=filter(masterDB,masterDB$SYMBOL == eq1)
  eq50$RSI14=RSI(eq50$CLOSE,14)
  eq50$EMA20=EMA(eq50$CLOSE,20,ratio = 2/(1+20))
  eq50$EMA50=EMA(eq50$CLOSE,50,ratio = 2/(1+50))
  eq50$EMA200=EMA(eq50$CLOSE,200,ratio = 2/(1+200))
  eq50$ROC=ROC(eq50$CLOSE,12)
  newpath1=paste(Path,eq1,".csv")
  newpath1=gsub('\\s+', '', newpath1)
  write.csv(eq50,newpath1)
  eq50$TIMESTAMP=as.Date(x=eq50$TIMESTAMP, format="%d-%m-%Y")
  xts2=xts(eq50[,4:9],order.by = eq50$TIMESTAMP)
  data2=to.monthly(xts2, indexAt = 'yearmon',name=NULL)
  nr=nrow(data2)
  data3=data.frame(data2)
  data3$SYMBOL=eq1
  data3$ROC=ROC(data3[,"Close"],12)*100
  data3$PChg=0.0
  for(j in 2:nr){
  data3$PChg[j]=log(data3$Close[j]/data3$Close[j-1])
  }
  data3$ACCL=0.0
  nrr=nr-12
  for(k in 13:nr){
    data3$ACCL[k]=round(((data3$Close[k]/data3$Close[k-12])-1)*100,0)
  }
  newpath=as.character(paste("C:/Data/R_Work/nov2017/monthwise/",eq1,".csv"))
  newpath=gsub('\\s+', '', newpath)# To Remove white spaces
  write.csv(data3,newpath)
}

#===================================================

# Read all CSV files and report the technical parameters
# Rank the stocks based on ROC -list at Rank_ROC file
# Calculate based on Month returns
#Find the open in the First (min)date of the month in the time stamp
#Find the close in the last (max) date of the month in the time stamp
#Find the RoR based (open-close)/open for every month
#Add 1+RoR for everymonth
#product all the months and substract one from them
#Label as MRoC and rank them based on that

setwd("C:/Data/R_Work/nov2017/monthwise/")
mypath=getwd()
nr=nrow(data3)
files=list.files(path = mypath,pattern = ".csv")
temp=lapply(files, fread,sep=",",skip=nr-12)    # Skip Rows till the end in the Database
CL=rbindlist(temp,fill=T)
colnames(CL)=c("MONTH","OPEN","HIGH","LOW","CLOSE","SYMBOL","ROC","PCHG","ACCL")
CL1=CL[order(CL$ACCL,decreasing = T)]
CL1=subset(CL1,MONTH==mnt)
CL1=CL1[,-c(7,8)]
write.csv(CL1,"C:/Data/R_Work/nov2017/output/N500_Momentum.csv")
N500_top10=head(CL1,15)

# Collated - Rank Ordered Momentum table
# Read all CSV files and report the technical parameters
# Rank the stocks based on ROC -list at Rank_ROC file
# Filter Stocks based on Equities in Nifty 200 list and create seperate dataframe

library(TTR)
n200=read.csv("C:/Data/R_Work/indx/n200.csv",header=T)
eq2=data.frame()
n2001=data.frame()
n201=data.frame()
i=1
cnt=0.0
CL200=CL1

library(dplyr)
library(readr)
for(i in 1:201){
  eq2=as.character(n200$SYMBOL[i])
  eq200=filter(CL200,CL200$SYMBOL == eq2)
  n2001=rbind(eq200,n2001)
}
n2001=n2001[order(n2001$ACCL,decreasing = T),]
write.csv(n2001,"C:/Data/R_Work/nov2017/output/N200_Momentum.csv")
N200_top10=head(n2001,15)

# Filter Stocks based on Equities in Nifty 50 list and create seperate dataframe

library(TTR)
n50=read.csv("C:/Data/R_Work/indx/n50.csv",header=T)
eq2=data.frame()
n501=data.frame()
n201=data.frame()
i=1
cnt=0.0
CL50=CL1

library(dplyr)
library(readr)
for(i in 1:51){
  eq2=as.character(n50$SYMBOL[i])
  eq200=filter(CL50,CL50$SYMBOL == eq2)
  n501=rbind(eq200,n501)
}
#n501$ROC=as.numeric(n501$ROC)
n501=n501[order(n501$ACCL,decreasing = T),]
write.csv(n501,"C:/Data/R_Work/nov2017/output/N50_Momentum.csv")
N50_LS=rbind(head(n501,3),tail(n501,3))

# Filter Stocks based on Equities in Nifty 100 list and create seperate dataframe

library(TTR)
n100=read.csv("C:/Data/R_Work/indx/n100.csv",header=T)
eq2=data.frame()
n101=data.frame()
n201=data.frame()
i=1
cnt=0.0
CL100=CL1

library(dplyr)
library(readr)
for(i in 1:101){
  eq2=as.character(n100$SYMBOL[i])
  eq200=filter(CL100,CL100$SYMBOL == eq2)
  n101=rbind(eq200,n101)
}
#n101$ROC=as.numeric(n101$ROC)
n101=n101[order(n101$ACCL,decreasing = T),]
write.csv(n101,"C:/Data/R_Work/nov2017/output/N100_Momentum.csv")
N100_LS=rbind(head(n101,3),tail(n101,3))

print("Nifty 500 stocks -Top 15 long")
N500_top10
print("Nifty 200 stocks -Top 15 long")
N200_top10
print("Nifty 100 stocks -Top 3 long and bottom 3 Short")
N50_LS
print("Nifty 100 stocks -Top 3 long and bottom 3 Short")
N100_LS

##==============================================================================
                    ############# Completed##################
#===============================================================================


