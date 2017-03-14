#SCRIPT TO EXTRACT THE DYNAMIC CARBON INTENSITY DATA IN CSV FORM

setwd('C://Users//boa//Documents//Repositories_Github//dynamic-carbon-factor-tool//Code')
load("TEF.Rda")
load("fo.Rda")
load("faggfin.Rda")

write.csv(TEF,file="carbon_intensity_CH_with_variants.csv",col.names=TRUE,row.names=TRUE)
write.csv(faggfin,file="carbon_intensity_imports_DE_FR_IT_AT.csv",col.names=TRUE,row.names=TRUE)