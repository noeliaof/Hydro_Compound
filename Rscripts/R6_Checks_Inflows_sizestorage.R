# Analysis of CH2018 and reference

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();


# Set -up the ouput
df_mau <- read.csv('/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Revision_Mar23/ClassicalModels_only_discharge/Kraftwerke Mauvoisin AG.csv')
df_mau$date <- as.Date(as.character(df_mau$date))
df_mau_red <- df_mau%>%dplyr::filter(format(date,"%Y")<2010)%>%dplyr::select(c("date","PREVAH"))

mau_mon <- df_mau_red%>%group_by(mon=format(date,"%m"))%>%dplyr::summarise(mon_dis = mean(PREVAH), dis_sum = sum(PREVAH)/28)
# convert to QS

Area_m2 <- 297327604.8

factor <- (1000*24*3600)

mau_mon$Dis_Qm3 <- (mau_mon$dis_sum*Area_m2)/factor
