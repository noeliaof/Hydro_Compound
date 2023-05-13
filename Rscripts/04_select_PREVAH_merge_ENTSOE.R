# Select data from ENVIDAT

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();


# read the selected catchments
# sel_pw <- read.csv("../../data/Output_data/csv/IDS_envidat.csv")
# Update catchment for Mauvoison
# sel_pw <- read.csv("../../data/Output_data/csv/IDS_updated_Mauv_envidat.csv")
# New Update using Rhein!
sel_pw <- read.csv("../../data/Output_data/csv/IDS_with_Rhein_UP.csv")

metafinal <- read.csv("../../data/Output_data/csv/metadata_final.csv")
raw_env <- read.table("../../data/Envidat/ch_500_eval.RGS_version_bilanz_81_21")


# Need to rename the columns V+1
names(raw_env)[1] <- "Date"
names(raw_env) <- c("Date",paste("V",seq(1,307), sep=""))

dis_prevah <- get_prevah_UP_subcatch(raw_env, sel_pw)


# filter from 1979
ll_discharge <- lapply(dis_prevah, function(x)x%>%dplyr::filter(format(date,"%Y")>="1979"))
# rename discharge
ll_discharge <- lapply(dis_prevah, function(x){colnames(x) <-  c("PREVAH", "date")
x})

# Load ENTSOE long term data
load("../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2015_2021.Rda")
# Filter production type: only reservoir and runof-river
df_enerp_s <- day_generation_2015_2021%>%dplyr::filter(ProductionTypeName!="Hydro Pumped Storage" )
n_entsoe <- length(unique(df_enerp_s$PowerSystemResourceName))

# Filter only the selected-stations
entsoe_final <- df_enerp_s%>%dplyr::filter(eic_g%in%metafinal$eic_g)%>%
  dplyr::select(date,eic_g , PowerSystemResourceName, name_g,name_p, ActualGenerationOutput,InstalledGenCapacity,capacity_p )%>%
  dplyr::mutate(generation=ActualGenerationOutput/24)

l_entsoe <- split(entsoe_final, f=entsoe_final$name_p)



# rename discharge
dout <-  "../../data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/"
dir.create(dout, showWarnings = F)
data_extended <- list()
for ( i in 1:length(l_entsoe)){
  idx <- which(names(l_entsoe)[[i]]== names(ll_discharge))
  
  data_extended[[i]] <- merge(l_entsoe[[i]], ll_discharge[[idx]], id="date", all=TRUE)
  # remove 1980
  data_extended[[i]] <- data_extended[[i]]%>%dplyr::filter(format(date,"%Y")>"1980")
  # remove discharge_mmd (need to fix it)
  data_extended[[i]]$discharge_mmd <- NULL
  # Add the window-moving average as for the observed discharge
  data_extended[[i]] <- data_extended[[i]]%>%dplyr::mutate(PREVAH7D=rollapply(PREVAH, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                              by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_extended[[i]] <- data_extended[[i]]%>%dplyr::mutate(PREVAH15D=rollapply(PREVAH, width=15, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                               by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_extended[[i]] <- data_extended[[i]]%>%dplyr::mutate(PREVAH30D=rollapply(PREVAH, width=30, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                               by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  
  
  # save the new data
  write.csv(data_extended[[i]], file=paste(dout, unique(data_extended[[i]]$name_p[!is.na(data_extended[[i]]$name_p)]), ".csv",sep = ""))
}













