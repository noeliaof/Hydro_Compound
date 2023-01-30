# Prepare data to model with PREVAH

# Data prepare ENTSOE and Discharge to model 
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# This data uses the meteorological variables are from the same catchement where the observed discharge was selected (from BAFU)
mdir <- "../../data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/mergedata_final/"
llf <- paste(mdir, list.files(mdir, pattern = "*csv"), sep="")
data_pw <- lapply(1:length(llf), function(i) read.csv(llf[i]))
data_pw <- lapply(data_pw, function(x) {x$date <- as.Date(x$date)
x})


data_merged_index <- lapply(1:length(data_pw), function(i) add_SCE_SPEI_todata(data_pw[[i]]))



outdir <- "../../data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/mergedata_final/mergedata_with_index/"
dir.create(outdir)
# names_stat <- lapply(data_merged_index, function(x) unique(x$name_p)[!is.na(unique(x$name_p)) &  unique(x$name_p)!=""])
names_stat <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(llf))

names(data_merged_index) <- names_stat
# Rename the last element of the list, as I added Rhineau 
# names(data_merged_index)[7] <- "Rhineau"
#Rename again the name discharge for consistency
for(i in 1:length(data_merged_index)){
  cat(i, "\n")
  
  names(data_merged_index[[i]])[names(data_merged_index[[i]])=="discharge"] <- "PREVAH"
  write.csv(data_merged_index[[i]], file = paste(outdir, names(data_merged_index)[i],".csv",sep=""), row.names = FALSE)
}

