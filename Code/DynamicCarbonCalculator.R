library(XML)
library(RCurl)
library(zoo)
library(xts)
library(tidyr)
library(plyr)
library(dplyr)
library(lubridate)
library(dygraphs)
library(readxl)
library(quantmod)
library(lattice)

### Follow the instructions


###: directory should be adapted to own laptop
setwd("C:\\Users\\Alice\\Documents\\Rstuff")

#loading data
load("IT2015.Rda")
load("FR2015.Rda")
load("CH2015.Rda")
load("DE2015h.Rda")
load("AT2015h.Rda")

# last preparation of data frame
country<-with_tz(country,tz="UTC")
fr2015<-with_tz(fr2015,tz="UTC")
it2015<-with_tz(it2015,tz="UTC")
CHe<-as.data.frame(xts(country[,2:23],order.by =country[,1]))
FRe<-as.data.frame(xts(fr2015[,2:23],order.by = fr2015[,1]))
ITe<-as.data.frame(xts(it2015[,2:23],order.by = it2015[,1]))
DEe<-as.data.frame(de2015h)
ATe<-as.data.frame(at2015h)


##########################################################################################################################
#  1  ##########  FAGG : need df with rownames=tstar, B01.... B20: returns dataframe "fagg" with rownames=tstart##########
##########################################################################################################################
#one per country 

fagg<-function(df){
  
  df<-df[,order(colnames(df))]
  s<-which(colnames(df)=="B01")
  e<-which(colnames(df)=="B20")
  prodtot<-apply(df[,s:e],1,sum,na.rm=TRUE)
  
  ef<-t(data.frame(read_excel("CO2factors.xlsx", sheet=1, col_names=FALSE, skip=0)))
  colnames(ef)<-colnames(df[,s:e])
  fagg<-c()
  for (i in 1:nrow(df)){ 
    if (prodtot[i]==0) {fagg[i]=0
    }else{
      dfnew<-rbind(df[i,s:e],as.numeric(ef[2,]))
      fagg[i]<-sum(apply(dfnew,2,prod, na.rm=FALSE),na.rm=TRUE)/prodtot[i]}
  }
  fagg<-data.frame(fagg)
  rownames(fagg)<-rownames(df)
  
  return (fagg)
}

faggch<-fagg(CHe); faggch[1:4345,]<-25
faggat<-fagg(ATe)
faggde<-fagg(DEe)
faggfr<-fagg(FRe)
faggit<-data.frame(matrix(475,ncol=1,nrow=8761)); rownames(faggit)<-rownames(ITe) #fagg IT has to be taken constant, lack of data
faggfin<-data.frame(faggch,faggat,faggde,faggfr,faggit)
colnames(faggfin)<-c("faggch","faggat","faggde","faggfr","faggit")

###############################################################################################################################################
#  2  ###Preparation of dataframe for FO (in this case the Swissgrid database is used)
###############################################################################################################################################
# First: create usable df: import and export must contain "imp" or "exp" !!

##### WORKING WITH ENTSOE
#s<-which(colnames(CHe)=="B01")
#e<-which(colnames(CHe)=="B20")
#CHe[["prodtot"]]<-apply(CHe[,s:e],1,sum,na.rm=TRUE)
#df<-data.frame(CHe$prodtot,ATe$Import,ATe$Export,DEe$Import,DEe$Export,FRe$Import,FRe$Export,ITe$Import,ITe$Export)
##consider CH/FR/IT 23:00 beginning UTC, AT/DE: 23:45 end UTC > I want to use tstart
#rownames(df)<-rownames(CHe)
#colnames(df)<-c("prodtot","ATimp","ATexp","DEimp","DEexp","FRimp","FRexp","ITimp","ITexp")

### WORKING WITH SWISSGRID
Swissgrid2015 <- read_excel("CH_2015.xlsx", sheet=3, col_names=TRUE, skip=0)
names(Swissgrid2015)[1]<-"t_end"
Swissgrid2015$t_end <- as.POSIXct(round.POSIXt(Swissgrid2015$t_end),tz="Europe/Berlin", usetz=TRUE)
names(Swissgrid2015) <- c("t_end",
                          "Cons_Total_Enduse_Swiss_ControlBlock",
                          "Prod_Total_Swiss_ControlBlock",
                          "Cons_Total_Swiss_ControlBlock",
                          "Net_Outflow_Swiss_Transm_Grid",
                          "Grid_FeedIn_Swiss_Transm_Grid",
                          "Control_Energy_Pos_Sec",
                          "Control_Energy_Neg_Sec",
                          "Control_Energy_Pos_Ter",
                          "Control_Energy_Neg_Ter",
                          "Cross_Border_Exchange_CH_AT",
                          "Cross_Border_Exchange_AT_CH",
                          "Cross_Border_Exchange_CH_DE",
                          "Cross_Border_Exchange_DE_CH",
                          "Cross_Border_Exchange_CH_FR",
                          "Cross_Border_Exchange_FR_CH",
                          "Cross_Border_Exchange_CH_IT",
                          "Cross_Border_Exchange_IT_CH",
                          "Transit",
                          "Import",
                          "Export",
                          "Prices_Avg_Pos_Sec_Control_Energy",
                          "Prices_Avg_Neg_Sec_Control_Energy",
                          "Prices_Avg_Pos_Ter_Control_Energy",
                          "Prices_Avg_Neg_Ter_Control_Energy",
                          "Prod_AG","Cons_AG", "Prod_FR", "Cons_FR","Prod_GL","Cons_GL", "Prod_GR", "Cons_GR", "Prod_LU","Cons_LU",
                          "Prod_NE","Cons_NE", "Prod_SO","Cons_SO", "Prod_SG","Cons_SG", "Prod_TI","Cons_TI", "Prod_TG","Cons_TG",
                          "Prod_VS","Cons_VS","Prod_AI,AR","Cons_AI,AR","Prod_BL,BS","Cons_BL,BS", "Prod_BE,JU","Cons_BE,JU","Prod_SZ,ZG",
                          "Cons_SZ,ZG","Prod_OW,NW,UR","Cons_OW,NW,UR", "Prod_GE,VD","Cons_GE,VD","Prod_SH,ZH", "Cons_SH,ZH","Prod_all_CANTONS",
                          "Cons_all_CANTONS",
                          "Prod_control_area_foreign_ter",
                          "Cons_control_area_foreign_ter"
)
#working with UTC: Sinan's function
target <- which(duplicated(Swissgrid2015$t_end))
if(length(target)>0){
  Swissgrid2015$t_end[seq(min(target)-6,by=1,length.out = 10)] <-  seq.POSIXt(Swissgrid2015$t_end[min(target)-6],by="15 min",length.out = 10)
}
Swissgrid2015$t_end <- as.POSIXct(round.POSIXt(Swissgrid2015$t_end),tz="Europe/Zurich",usetz=T)
Swissgrid2015$t_end <- with_tz(Swissgrid2015$t_end,tz="UTC")
fodf2015<-data.frame(cbind(Swissgrid2015$Cons_Total_Swiss_ControlBlock,Swissgrid2015$Cross_Border_Exchange_CH_AT,Swissgrid2015$Cross_Border_Exchange_AT_CH,
                           Swissgrid2015$Cross_Border_Exchange_CH_DE,Swissgrid2015$Cross_Border_Exchange_DE_CH,
                           Swissgrid2015$Cross_Border_Exchange_CH_FR,Swissgrid2015$Cross_Border_Exchange_FR_CH,
                           Swissgrid2015$Cross_Border_Exchange_CH_IT,Swissgrid2015$Cross_Border_Exchange_IT_CH))
colnames(fodf2015)<-c("prodtot","ATexp","ATimp","DEexp","DEimp","FRexp","FRimp","ITexp","ITimp")

fodf2015<-xts(fodf2015, order.by=Swissgrid2015$t_end)
fodf2015<-period.apply(fodf2015, endpoints(fodf2015, on="hours", k=1), colSums)
seq<-seq(from=ISOdate(2014,12,31,23),length.out = 8761, by="hours", tz="UTC")
fodf2015<-xts(fodf2015[-8762,], order.by=seq)#tstart
fodf2015<-data.frame(fodf2015)

###############################################################################################################################################
#  3  ###FO: need df with rownames=import and export for each country + prod tot ch: RETURN df foa/foc/fob for countries c and rownames=tstart
###############################################################################################################################################
fo<-function(df){
  tot_exp<-apply(df[,grepl("exp",names(df))],1,sum, na.rm=TRUE)
  tot_imp<-apply(df[,grepl("imp",names(df))],1,sum, na.rm=TRUE) 
  
  control<-tot_imp-tot_exp+df$prodtot
  ### VARIANT A ###
  #################
  controla<-df$prodtot-tot_exp
  foa_ch<-ifelse(control<=0,NA,ifelse(controla<0,0,((df$prodtot-tot_exp)/(df$prodtot-tot_exp+tot_imp)) ))
  foa_at<-ifelse(control<=0,NA,ifelse(controla<0,(df$ATimp/tot_imp),(df$ATimp/(df$prodtot-tot_exp+tot_imp))))
  foa_de<-ifelse(control<=0,NA,ifelse(controla<0,(df$DEimp/tot_imp),(df$DEimp/(df$prodtot-tot_exp+tot_imp))))
  foa_fr<-ifelse(control<=0,NA,ifelse(controla<0,(df$FRimp/tot_imp),(df$FRimp/(df$prodtot-tot_exp+tot_imp))))
  foa_it<-ifelse(control<=0,NA,ifelse(controla<0,(df$ITimp/tot_imp),(df$ITimp/(df$prodtot-tot_exp+tot_imp))))
  
  ### VARIANT B ###
  #################
  controlb<-tot_imp-tot_exp
  fob_ch<-ifelse(control<=0,NA,ifelse(controlb<0,1.0, df$prodtot/(df$prodtot+controlb)))
  fob_at<-ifelse(control<=0,NA, ifelse(controlb<0, 0,(df$ATimp*(1-tot_exp/tot_imp)/(df$prodtot-tot_exp+tot_imp))))
  fob_de<-ifelse(control<=0,NA, ifelse(controlb<0, 0,(df$DEimp*(1-tot_exp/tot_imp)/(df$prodtot-tot_exp+tot_imp))))
  fob_fr<-ifelse(control<=0,NA, ifelse(controlb<0, 0,(df$FRimp*(1-tot_exp/tot_imp)/(df$prodtot-tot_exp+tot_imp))))
  fob_it<-ifelse(control<=0,NA, ifelse(controlb<0, 0,(df$ITimp*(1-tot_exp/tot_imp)/(df$prodtot-tot_exp+tot_imp))))
  
  ### VARIANT C ###
  #################
  foc_ch<-ifelse(control<=0,NA,df$prodtot*(1-(tot_exp/(tot_imp+df$prodtot)))/(df$prodtot-tot_exp+tot_imp))
  foc_at<-ifelse(control<=0,NA,df$ATimp*(1-(tot_exp/(tot_imp+df$prodtot)))/(df$prodtot-tot_exp+tot_imp))
  foc_de<-ifelse(control<=0,NA,df$DEimp*(1-(tot_exp/(tot_imp+df$prodtot)))/(df$prodtot-tot_exp+tot_imp))
  foc_fr<-ifelse(control<=0,NA,df$FRimp*(1-(tot_exp/(tot_imp+df$prodtot)))/(df$prodtot-tot_exp+tot_imp))
  foc_it<-ifelse(control<=0,NA,df$ITimp*(1-(tot_exp/(tot_imp+df$prodtot)))/(df$prodtot-tot_exp+tot_imp))
  
  ### VARIANT D###
  ################# 
  controld<-tot_imp+df$prodtot/2-tot_exp
  fod_ch<-ifelse(control<=0,NA,ifelse(controld<0,1,df$prodtot*(1-(tot_exp/(2*tot_imp+df$prodtot)))/(df$prodtot-tot_exp+tot_imp)))
  fod_at<-ifelse(control<=0,NA,ifelse(controld<0,0,df$ATimp*(1-(tot_exp/(tot_imp+df$prodtot/2)))/(df$prodtot-tot_exp+tot_imp)))
  fod_de<-ifelse(control<=0,NA,ifelse(controld<0,0,df$DEimp*(1-(tot_exp/(tot_imp+df$prodtot/2)))/(df$prodtot-tot_exp+tot_imp)))
  fod_fr<-ifelse(control<=0,NA,ifelse(controld<0,0,df$FRimp*(1-(tot_exp/(tot_imp+df$prodtot/2)))/(df$prodtot-tot_exp+tot_imp)))
  fod_it<-ifelse(control<=0,NA,ifelse(controld<0,0,df$ITimp*(1-(tot_exp/(tot_imp+df$prodtot/2)))/(df$prodtot-tot_exp+tot_imp)))
  
  
  fo<-data.frame(foa_ch,fob_ch,foc_ch,fod_ch,
                 foa_at,fob_at,foc_at,fod_at,
                 foa_de,fob_de,foc_de,fod_de,
                 foa_fr,fob_fr,foc_fr,fod_fr,
                 foa_it,fob_it,foc_it,fod_it)
  return(fo)
}

fo<-fo(fodf2015)

###############################################################################################################################################
#  4  ### TEF (with a df with all fagg and fo(already contains all fo))
###############################################################################################################################################

TEF<-function(faggfin,fo){
  a<-cbind(fo$foa_ch*faggfin$faggch+fo$foa_at*faggfin$faggat+fo$foa_de*faggfin$faggde+fo$foa_fr*faggfin$faggfr+fo$foa_it*faggfin$faggit)
  b<-cbind(fo$fob_ch*faggfin$faggch+fo$fob_at*faggfin$faggat+fo$fob_de*faggfin$faggde+fo$fob_fr*faggfin$faggfr+fo$fob_it*faggfin$faggit)
  c<-cbind(fo$foc_ch*faggfin$faggch+fo$foc_at*faggfin$faggat+fo$foc_de*faggfin$faggde+fo$foc_fr*faggfin$faggfr+fo$foc_it*faggfin$faggit)
  d<-cbind(fo$fod_ch*faggfin$faggch+fo$fod_at*faggfin$faggat+fo$fod_de*faggfin$faggde+fo$fod_fr*faggfin$faggfr+fo$fod_it*faggfin$faggit)
  TEFa<-apply(a,1,sum,na.rm=FALSE)
  TEFb<-apply(b,1,sum,na.rm=FALSE)
  TEFc<-apply(c,1,sum,na.rm=FALSE)
  TEFd<-apply(d,1,sum,na.rm=FALSE)
  TEF<-data.frame(TEFa,TEFb,TEFc,TEFd)
  row.names(TEF)<-row.names(fo)
  return (TEF)
}


TEF<-TEF(faggfin,fo)

###############################################################################################################################################
#  3  ### DYGRAPHS
###############################################################################################################################################
save(TEF,file="TEF.Rda")
save(faggfin,file="faggfin.Rda")
save(fo,file="fo.Rda")
load("TEF.Rda")
load("fo.Rda")
load("faggfin.Rda")



dyCrosshair <- function(dygraph, 
                        direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("examples/plugins/crosshair.js", 
                       package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

seq<-as.POSIXct(rownames(TEF),tz="UTC")
dat<-xts(TEF,order.by = seq)
dygraph(dat, main="Emission factor of the Swiss consumption mix")%>% 
  dySeries("TEFa",label="TEF calculation variant a",color = "red",strokeWidth = 3)%>%
  dySeries("TEFb",label="TEF calculation variant b",color = "green",strokeWidth = 3)%>%
  dySeries("TEFc",label="TEF calculation variant c",color = "orange",strokeWidth = 3)%>%
  dySeries("TEFd",label="TEF calculation variant d",color = "blue",strokeWidth = 3)%>%
  #dyEvent("2015-8-10", "KKL Revision start", labelLoc = "top", color="red") %>%
  #dyEvent("2015-9-17", "KKL Revision finish", labelLoc = "top", color="blue") %>%
  #dyEvent("2015-10-17", "KKL Stop", labelLoc = "top", color="red") %>%
  #dyEvent("2015-11-02", "KKL Start", labelLoc = "top", color="blue") %>%
  #dyEvent("2015-6-30", "KKBI stop", labelLoc = "top", color="red") %>%
  #dyEvent("2015-8-14", "KKBII stop", labelLoc = "top", color="red") %>%
  #dyEvent("2015-12-23", "KKBII start", labelLoc = "top", color="blue") %>%
  #dyEvent("2015-6-06", "KKG Revision start", labelLoc = "top", color="red") %>%
  #dyEvent("2015-7-06", "KKG Revision end", labelLoc = "top", color="blue") %>%
  #dyEvent("2015-8-02", "KKM stop", labelLoc = "top", color="red") %>%
  #dyEvent("2015-10-02", "KKM start", labelLoc = "top", color="blue") %>%
#dyLimit(91.5, color = "dark blue")%>%
#dyLimit(101.5, color = "dark blue")%>%
dyRangeSelector() %>%
  dyAxis("x", label = "Datetime UTC", drawGrid = T) %>%
  dyAxis("y", label = "g-CO2eq/kWh for the Swiss electricity mix") %>%
  dyCrosshair(direction="vertical") %>%
  dyOptions(stackedGraph=F,stepPlot=T,useDataTimezone=T)

##DYGRAPH fagg
faggfin[,1][faggfin[,1]==0]<-NA
faggfin[,3][faggfin[,3]==0]<-NA
seqfagg<-as.POSIXct(rownames(faggfin),tz="UTC")
datfagg<-xts(faggfin,order.by = seqfagg)
dygraph(datfagg, main="Fp: production emission factor ")%>% 
  dySeries("faggch",label="Switzerland",color = "red",strokeWidth = 3)%>%
  dySeries("faggat",label="Austria",color = "purple",strokeWidth = 3)%>%
  dySeries("faggde",label="Germany",color = "orange",strokeWidth = 3)%>%
  dySeries("faggfr",label="France",color = "blue",strokeWidth = 3)%>%
  dySeries("faggit",label="Italy",color = "green",strokeWidth = 3)%>%
  dyEvent("2015-1-02", "EF for IT and CH constant", labelLoc = "top", color="blue") %>%
  dyEvent("2015-7-01", "EF for CH after ENTSOE data", labelLoc = "top", color="blue") %>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Datetime UTC", drawGrid = T) %>%
  dyAxis("y", label = "g-CO2eq/kWh") %>%
  dyCrosshair(direction="vertical") %>%
  dyOptions(stackedGraph=F,stepPlot=T,useDataTimezone=T)

fr<-xts(fr2015[2:21], order.by=fr2015[,1])
fr<-fr[,4]
dygraph(fr)  %>% dyRangeSelector() 
## DYGRAPH fo
seqfo<-as.POSIXct(rownames(fo),tz="UTC")
#A
foa<-data.frame(fo$foa_ch,fo$foa_at,fo$foa_de,fo$foa_fr,fo$foa_it)
datfoa<-xts(foa,order.by = seqfo)
dygraph(datfoa, main="Origin factors: Variant A")%>% 
  dySeries("fo.foa_ch",label="Swiss production",color = "red",strokeWidth = 3)%>%
  dySeries("fo.foa_at",label="Imports from Austria",color = "purple",strokeWidth = 3)%>%
  dySeries("fo.foa_de",label="Imports from Germany",color = "blue",strokeWidth = 3)%>%
  dySeries("fo.foa_fr",label="Imports from France",color = "orange",strokeWidth = 3)%>%
  dySeries("fo.foa_it",label="Imports from Italy",color = "green",strokeWidth = 3)%>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Datetime UTC", drawGrid = T) %>%
  dyAxis("y", label = "percentage in consumption mix") %>%
  dyCrosshair(direction="vertical") %>%
  dyOptions(stackedGraph=T,stepPlot=T,useDataTimezone=T)
#B
fob<-data.frame(fo$fob_ch,fo$fob_at,fo$fob_de,fo$fob_fr,fo$fob_it)
datfob<-xts(fob,order.by = seqfo)
dygraph(datfob, main="Origin factors:Variant B")%>% 
  dySeries("fo.fob_ch",label="Swiss production",color = "red",strokeWidth = 3)%>%
  dySeries("fo.fob_at",label="Imports from Austria",color = "purple",strokeWidth = 3)%>%
  dySeries("fo.fob_de",label="Imports from Germany",color = "blue",strokeWidth = 3)%>%
  dySeries("fo.fob_fr",label="Imports from France",color = "orange",strokeWidth = 3)%>%
  dySeries("fo.fob_it",label="Imports from Italy",color = "green",strokeWidth = 3)%>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Datetime UTC", drawGrid = T) %>%
  dyAxis("y", label = "percentage in consumption mix") %>%
  dyCrosshair(direction="vertical") %>%
  dyOptions(stackedGraph=T,stepPlot=T,useDataTimezone=T)
#C
foc<-data.frame(fo$foc_ch,fo$foc_at,fo$foc_de,fo$foc_fr,fo$foc_it)
datfoc<-xts(foc,order.by = seqfo)
dygraph(datfoc, main="Origin factors: Variant C")%>% 
  dySeries("fo.foc_ch",label="Swiss production",color = "red",strokeWidth = 3)%>%
  dySeries("fo.foc_at",label="Imports from Austria",color = "purple",strokeWidth = 3)%>%
  dySeries("fo.foc_de",label="Imports from Germany",color = "blue",strokeWidth = 3)%>%
  dySeries("fo.foc_fr",label="Imports from France",color = "orange",strokeWidth = 3)%>%
  dySeries("fo.foc_it",label="Imports from Italy",color = "green",strokeWidth = 3)%>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Datetime UTC", drawGrid = T) %>%
  dyAxis("y", label = "percentage in consumption mix") %>%
  dyCrosshair(direction="vertical") %>%
  dyOptions(stackedGraph=T,stepPlot=T,useDataTimezone=T)

#D
fod<-data.frame(fo$fod_ch,fo$fod_at,fo$fod_de,fo$fod_fr,fo$fod_it)
datfod<-xts(fod,order.by = seqfo)
dygraph(datfod, main="Origin factors: Variant D")%>% 
  dySeries("fo.fod_ch",label="Swiss production",color = "red",strokeWidth = 3)%>%
  dySeries("fo.fod_at",label="Imports from Austria",color = "purple",strokeWidth = 3)%>%
  dySeries("fo.fod_de",label="Imports from Germany",color = "blue",strokeWidth = 3)%>%
  dySeries("fo.fod_fr",label="Imports from France",color = "orange",strokeWidth = 3)%>%
  dySeries("fo.fod_it",label="Imports from Italy",color = "green",strokeWidth = 3)%>%
  dyRangeSelector() %>%
  dyAxis("x", label = "Datetime UTC", drawGrid = T) %>%
  dyAxis("y", label = "percentage in consumption mix") %>%
  dyCrosshair(direction="vertical") %>%
  dyOptions(stackedGraph=T,stepPlot=T,useDataTimezone=T)