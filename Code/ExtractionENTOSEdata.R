library(XML)
library(RCurl)
library(zoo)
library(xts)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(dygraphs)
library(taRifx)


##########################################################################################################################
#  1  ############ FUNCTION for creating dataframe df - has to be repeated for each country (2015 default) ###############
##########################################################################################################################
#USER UPDATE: date (under from and to) 

df_country<-function(df2,dfexp,dfimp){  
  #### year selection ###
  from<-as.POSIXct(ISOdate(2014,12,31,23, tz="UTC")) #to 22 since i put it as being UTC and we are actually 23h at UTC+1
  #length <-(ncol(df2)+1-(which(colnames(df2)=="Period.Point.position")))/2
  to<-as.POSIXct(ISOdate(2015,12,31,23,tz="UTC"))
  t<-c(seq.POSIXt(from,to,by="15 min",tz="UTC")) 
  
  
  ### column content selection ###
  name_col<-c("time","B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","Export","Import")
  
  ###creation of empty dataframe###
  country<-data.frame(matrix(NA,nrow=length(t),ncol=length(name_col)))
  colnames(country)<-name_col
  country[["time"]]<-t
  
  
  ### creation of list of vectors with #observation per TimeSeries ###
  
  df2$Period.timeInterval.start<-gsub("Z",":00",df2$Period.timeInterval.start)
  df2$Period.timeInterval.end<-gsub("Z",":00",df2$Period.timeInterval.end)
  dfexp$Period.timeInterval.end<-gsub("Z",":00",dfexp$Period.timeInterval.end)
  dfexp$Period.timeInterval.start<-gsub("Z",":00",dfexp$Period.timeInterval.start)
  dfimp$Period.timeInterval.end<-gsub("Z",":00",dfimp$Period.timeInterval.end)
  dfimp$Period.timeInterval.start<-gsub("Z",":00",dfimp$Period.timeInterval.start)
  
  df2$Period.timeInterval.start<-gsub("T"," ",df2$Period.timeInterval.start)
  df2$Period.timeInterval.end<-gsub("T"," ",df2$Period.timeInterval.end)
  dfexp$Period.timeInterval.start<-gsub("T"," ",dfexp$Period.timeInterval.start)
  dfexp$Period.timeInterval.end<-gsub("T"," ",dfexp$Period.timeInterval.end)
  dfimp$Period.timeInterval.start<-gsub("T"," ",dfimp$Period.timeInterval.start)
  dfimp$Period.timeInterval.end<-gsub("T"," ",dfimp$Period.timeInterval.end)
  
  df2$Period.timeInterval.start<-as.POSIXct(as.numeric(ymd_hms(df2$Period.timeInterval.start,tz="UTC")),origin="1970-01-01")
  df2$Period.timeInterval.end<-as.POSIXct(as.numeric(ymd_hms(df2$Period.timeInterval.end,tz="UTC")),origin="1970-01-01")
  dfexp$Period.timeInterval.start<-as.POSIXct(as.numeric(ymd_hms(dfexp$Period.timeInterval.start,tz="UTC")),origin="1970-01-01")
  dfexp$Period.timeInterval.end<-as.POSIXct(as.numeric(ymd_hms(dfexp$Period.timeInterval.end,tz="UTC")),origin="1970-01-01")
  dfimp$Period.timeInterval.start<-as.POSIXct(as.numeric(ymd_hms(dfimp$Period.timeInterval.start,tz="UTC")),origin="1970-01-01")
  dfimp$Period.timeInterval.end<-as.POSIXct(as.numeric(ymd_hms(dfimp$Period.timeInterval.end,tz="UTC")),origin="1970-01-01")
  
  list_pos<-list()
  ########################HERE: don't know why, if total time interval > length too big, but if not total : lengt alright: I guess it's from the entsoe data...
  for (i in (1:nrow(df2))){
    list_pos[[i]]<-c(seq.POSIXt(from=df2[i,"Period.timeInterval.start"],to=df2[i,"Period.timeInterval.end"],by="15 min",tz="UTC"))
    if (length(list_pos[[i]])>2045){list_pos[[i]]<-c(seq.POSIXt(from=df2[i,"Period.timeInterval.start"],to=df2[i,"Period.timeInterval.end"],by="15 min",tz="UTC"))}
  }
  ### attention seulemnet periode de 60 min pour export import!!
  for (i in (nrow(df2)+1):(nrow(df2)+nrow(dfexp))) {list_pos[[i]]<-c(seq.POSIXt(from=as.POSIXct(dfexp[(i-nrow(df2)),"Period.timeInterval.start"]),to=as.POSIXct(dfexp[(i-nrow(df2)),"Period.timeInterval.end"])-3600,by="hour",tz="UTC"))}
  for (i in ((nrow(df2)+nrow(dfexp))+1):(nrow(df2)+nrow(dfexp)+nrow(dfimp))) {list_pos[[i]]<-c(seq.POSIXt(from=as.POSIXct(dfimp[(i-nrow(df2)-nrow(dfexp)),"Period.timeInterval.start"]),to=as.POSIXct(dfimp[(i-nrow(df2)-nrow(dfexp)),"Period.timeInterval.end"])-3600,by="hour",tz="UTC"))}
  
  ### creation of list with quantity vectors per Time Series ###
  list_QT<-list()
  for (i in 1:(nrow(df2))) {deb<-which(colnames(df2)=="Period.Point.position");fin<-which(colnames(df2)=="Period.Point.position")+2*length(list_pos[[i]])-1
  QT<-c(df2[i,deb:fin])
  list_QT[[i]]<-QT[c(FALSE,TRUE)]}
  for (i in ((nrow(df2)+1):(nrow(df2)+nrow(dfexp)))) {deb<-which(colnames(dfexp)=="Period.Point.position"); fin<-which(colnames(dfexp)=="Period.Point.position")+2*length(list_pos[[i]])-1
  QT<-c(dfexp[(i-nrow(df2)),(deb:fin)])
  list_QT[[i]]<-QT[c(FALSE,TRUE)]}
  for (i in ((nrow(df2)+nrow(dfexp)+1)):(nrow(df2)+nrow(dfexp)+nrow(dfimp))) {deb<-which(colnames(dfimp)=="Period.Point.position"); fin<-which(colnames(dfimp)=="Period.Point.position")+2*length(list_pos[[i]])-1
  QT<-c(dfimp[(i-nrow(df2)-nrow(dfexp)),deb:fin])
  list_QT[[i]]<-QT[c(FALSE,TRUE)]}
  
  #inserting QT in country's dataframe if time and psrType are matching > considering that e(length(list_pos[[i]]))xport import have only one time series!
  for (i in 1:nrow(df2)){index_col<-which(colnames(country)==as.character(df2[i,"psrType"])) 
  if (length(list_pos[[i]])>0){end<-length(list_pos[[i]])}else{end<-1}
  for (j in 1:end){
    index_row<-which(country[,1]==list_pos[[i]][[j]])
    country[index_row,index_col]<-as.numeric(as.character(list_QT[[i]][[j]]))}}
  
  for (i in (nrow(df2)+1):(nrow(df2)+nrow(dfexp))){index_col<-which(colnames(country)==as.character("Export"))
  for (j in 1:length(list_pos[[i]])){
    index_row<-which(country[,1]==list_pos[[i]][[j]])
    country[index_row,index_col]<-as.numeric(as.character(list_QT[[i]][[j]]))}}
  
  for (i in ((nrow(df2)+nrow(dfexp)+1)):(nrow(df2)+nrow(dfexp)+nrow(dfimp))){ index_col<-which(colnames(country)==as.character("Import"))
  for (j in 1:length(list_pos[[i]])){
    index_row<-which(country[,1]==list_pos[[i]][[j]])
    country[index_row,index_col]<-as.numeric(as.character(list_QT[[i]][[j]]))}}
  
  return (country)
}


##########################################################################################################################
#  2  ################## PREPARATION FOR FUNCTION - since takes ages: one country at a time ##############################
##########################################################################################################################

load("C:\\Users\\Alice\\Documents\\Rstuff\\dfAT20151_clean.Rda")
df2<-df2at1_clean

### extraction XML exchange data###
xml.fileexp<-"https://transparency.entsoe.eu/api?securityToken=197d2d06-a66c-48a4-b8fe-71ba60826243&documentType=A11&in_Domain=10YAT-APG------L&out_Domain=10YCH-SWISSGRIDZ&periodStart=201412312300&periodEnd=201512312300"
xml.urlexp<-getURL(xml.fileexp)
doc<-xmlParse(xml.urlexp)
df <- ldply(xmlToList(doc), data.frame)
dfexp<-df[df$.id=="TimeSeries",]  #selecting only rows from node TimeSeries

xml.fileimp<-"https://transparency.entsoe.eu/api?securityToken=197d2d06-a66c-48a4-b8fe-71ba60826243&documentType=A11&in_Domain=10YCH-SWISSGRIDZ&out_Domain=10YAT-APG------L&periodStart=201412312300&periodEnd=201512312300"
xml.urlimp<-getURL(xml.fileimp)
doc<-xmlParse(xml.urlimp)
df <- ldply(xmlToList(doc), data.frame)
dfimp<-df[df$.id=="TimeSeries",]  #selecting only rows from node TimeSeries 



##########################################################################################################################
#  3  ################## SAVING THE FINAL DATAFRAME > CHANGE COUNTRY NAME ! #############################################
##########################################################################################################################
#when changing: careful time interval, period

at20151<-df_country(df2,dfexp,dfimp)
save(at20151, file="AT20151.Rda")



##################### FOR DATA WHO HAVE TO BE DOWNLOADED IN TWO PARTS #########################################
#combining code in multiple parts
###############################################################################################################


load(file="C:\\Users\\Alice\\Documents\\Rstuff\\AT20151.Rda")
load(file="C:\\Users\\Alice\\Documents\\Rstuff\\AT20152.Rda")
at2015<-rbind(at20151[1:14496,],at20152[14497:35041,])
at2015[,1]<-with_tz(at2015[,1],tz="UTC")
save(at2015,file="AT2015.Rda")
load(file="AT2015.Rda")

load(file="C:\\Users\\Alice\\Documents\\Rstuff\\DE20151.Rda")
load(file="C:\\Users\\Alice\\Documents\\Rstuff\\DE20152.Rda")
de2015<-rbind(de20151[1:14496,],de20152[14497:35041,]) 
de2015[,1]<-with_tz(de2015[,1],tz="UTC")
save(de2015,file="DE2015.Rda")
load(file="DE2015.Rda")
###################################################################################################################
##### XTS to get a range over 1 hour instead of 15 min ############################################################

### t=tstart (always four positions! NICE!)
seq<-seq.POSIXt(from=as.POSIXct(ISOdate(2014,12,31,23, tz="UTC")), to=as.POSIXct(ISOdate(2015,12,31,23, tz="UTC")), by="15 min")
align.time.down=function(x,n){index(x)=index(x)-n;align.time(x,n)}

at2015h<-xts(at2015[,2:23],order.by=seq) #column type double
at2015h<-period.apply(at2015h,endpoints(at2015h,on="hours"),colSums,na.rm=TRUE)
at2015h<-at2015h/4*1 #MWh
# [00, 15, 30 , 45] sum is given at 45 and I want it to be given at 00


de2015h<-xts(de2015[,2:23],order.by=seq) #column type double
de2015h<-period.apply(de2015h,endpoints(de2015h,on="hours"),colSums,na.rm=TRUE)
de2015h<-de2015h/4*1 #MWh

save(at2015h,file="AT2015h.Rda")
save(de2015h,file="DE2015h.Rda")
