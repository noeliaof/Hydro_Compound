# Data selection Leventina and Blenio
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

#Load energy 

load("../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2015_2021.Rda")
# Filter production type: only reservoir and runof-river
df_enerp_s <- day_generation_2015_2021%>%dplyr::filter(ProductionTypeName!="Hydro Pumped Storage" )
n_entsoe <- length(unique(df_enerp_s$PowerSystemResourceName))
# Load metadata 
metafinal <- read.csv("../../data/Output_data/csv/metadata_final.csv")

# Filter only the selected-stations
entsoe_final <- df_enerp_s%>%dplyr::filter(eic_g%in%metafinal$eic_g)%>%
  dplyr::select(date,eic_g , PowerSystemResourceName, name_g,name_p, ActualGenerationOutput,InstalledGenCapacity,capacity_p )%>%
  dplyr::mutate(generation=ActualGenerationOutput/24)

l_entsoe <- split(entsoe_final, f=entsoe_final$name_p)

# Load discharge
sel_pw <- read.csv("../../data/Output_data/csv/IDS_with_Ticcino_UP.csv")

metafinal <- read.csv("../../data/Output_data/csv/metadata_final.csv")
raw_env <- read.table("../../data/Envidat/ch_500_eval.RGS_version_bilanz_81_21")


# Need to rename the columns V+1
names(raw_env)[1] <- "Date"
names(raw_env) <- c("Date",paste("V",seq(1,307), sep=""))

name_hp    <- unique(sel_pw$name_p)
hps <- c("Blenio (OFIBLE)","AET Leventina")

l_pw <- list()

for(i in 1:length(hps)) {
  
  dfent <-  l_entsoe[names(l_entsoe)==hps[i]][[1]]
  l_pw[[i]] <- get_prevah_select_subcatch(raw_env, sel_pw, hps[i])
  names(l_pw[[i]]) <- c("date","PREVAH")
  l_pw[[i]] <- merge(dfent, l_pw[[i]], id="date", all=TRUE)
  l_pw[[i]] <- l_pw[[i]]%>%dplyr::filter(format(date,"%Y")>"1980")
  l_pw[[i]]$date <- as.Date(l_pw[[i]]$date)
  # Add the window-moving average as for the observed discharge
  l_pw[[i]] <- l_pw[[i]]%>%dplyr::mutate(PREVAH7D=rollapply(PREVAH, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                    by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  l_pw[[i]] <- l_pw[[i]]%>%dplyr::mutate(PREVAH15D=rollapply(PREVAH, width=15, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                     by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  l_pw[[i]] <- l_pw[[i]]%>%dplyr::mutate(PREVAH30D=rollapply(PREVAH, width=30, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                     by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  
  
}
names(l_pw) <- hps

# Quick check!
# lev <- read.csv("../../data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/AET Leventina.csv")
# lev$date <- as.Date(lev$date)
# m_lev <- lev%>%group_by(mon=format(date,"%m"))%>%dplyr::summarise(dis=mean(PREVAH, na.rm=TRUE))
# m_l_pw <- l_pw$`AET Leventina`%>%group_by(mon=format(date,"%m"))%>%dplyr::summarise(dis=mean(PREVAH, na.rm=TRUE))
# ggplot() + geom_line(data=m_lev, aes(mon,dis,color="PREVAH(orig)"), group=1) + geom_line(data =m_l_pw, aes(mon, dis, color="PREVAH(new)"),group=1) +
#   labs(x = "Mon",y = "(mm/d)",color = "Catchm") +
#   scale_color_manual(values = c("blue","red"))+
#   theme_bw()
# # not much difference
# 
# bl <- read.csv("../../data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/Blenio (OFIBLE).csv")
# bl$date <- as.Date(bl$date)
# m_bl <- bl%>%group_by(mon=format(date,"%m"))%>%dplyr::summarise(dis=mean(PREVAH, na.rm=TRUE))
# m_l_bl <- l_pw$`Blenio (OFIBLE)`%>%group_by(mon=format(date,"%m"))%>%dplyr::summarise(dis=mean(PREVAH, na.rm=TRUE))
# ggplot() + geom_line(data=m_bl, aes(mon,dis, color="PREVAH(orig)"), group=1) +
#   geom_line(data =m_l_bl, aes(mon, dis, color="PREVAH(new)"), group=1) +
#   labs(x = "Mon",y = "(mm/d)",color = "Catchm") +
#   scale_color_manual(values = c("blue","red")) + theme_bw()
# not much difference

################################
# Read meteo, merge and prepare 
###############################

# Leventina
lev_t2m    <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Leventina/t2m_lev_197901_202112.csv")
# rename columns
names(lev_t2m) <- c("date","t2m")
lev_t2m$date <- as.Date(lev_t2m$date)
lev_t2mmax <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Leventina/t2max_lev_197901_202112.csv")
names(lev_t2mmax) <- c("date","t2mmax")
lev_t2mmax$date <- as.Date(lev_t2mmax$date)
lev_pr     <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Leventina/tp_lev_197901_202112.csv")
lev_pr$date <- as.Date(lev_pr$date)
names(lev_pr) <- c("date","tp")

# Merge with the discharge and energy data
df_lev_met <- Reduce(function(x, y) merge(x, y, all=TRUE), list(l_pw$`AET Leventina`, lev_t2m, lev_t2mmax, lev_pr))%>%dplyr::filter(format(date,"%Y")>1980)
# Add the index
df_met_lev_ind <- add_SCE_SPEI_todata(df_lev_met)
names(df_met_lev_ind)[names(df_met_lev_ind)=="discharge"] <- "PREVAH"

write.csv(df_met_lev_ind, file= paste('/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/mergedata_final/mergedata_with_index/',unique(df_met_lev_ind$name_p)[2], ".csv",sep=""), row.names = FALSE)

########### Blenio
bl_t2m    <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Blenio/t2m_bl_197901_202112.csv")
# rename columns
names(bl_t2m) <- c("date","t2m")
bl_t2m$date <- as.Date(bl_t2m$date)
bl_t2mmax <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Blenio/t2max_bl_197901_202112.csv")
names(bl_t2mmax) <- c("date","t2mmax")
bl_t2mmax$date <- as.Date(bl_t2mmax$date)
bl_pr     <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Blenio/tp_bl_197901_202112.csv")
bl_pr$date <- as.Date(bl_pr$date)
names(bl_pr) <- c("date","tp")

# Merge with the discharge and energy data
df_bl_met <- Reduce(function(x, y) merge(x, y, all=TRUE), list(l_pw$`Blenio (OFIBLE)`, bl_t2m, bl_t2mmax, bl_pr))%>%dplyr::filter(format(date,"%Y")>1980)
# Add the index
df_met_bl_ind <- add_SCE_SPEI_todata(df_bl_met)
names(df_met_bl_ind)[names(df_met_bl_ind)=="discharge"] <- "PREVAH"

write.csv(df_met_bl_ind, file= paste('/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/mergedata_final/mergedata_with_index/',unique(df_met_bl_ind$name_p)[2], ".csv",sep=""),row.names = FALSE)
