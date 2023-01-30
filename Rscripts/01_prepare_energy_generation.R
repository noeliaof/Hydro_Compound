setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

tot_cap     <- read.csv("/Users/noeliaotero/Documents/OCCR/data/ENTSOE_TP/national_generation_capacity_stacked.csv")
dir_perunit <- "/Users/noeliaotero/Documents/OCCR/data/ENTSOE_TP/ActualGenerationOutputPerUnit/"
years_f     <- lapply(1:length(list.files(dir_perunit)),function(i) strsplit(list.files(dir_perunit)[i],"_")[[1]][1])
ff_unit     <- paste(dir_perunit,list.files(dir_perunit), sep="")
dir_pertype <- "/Users/noeliaotero/Documents/OCCR/data/ENTSOE_TP/AggregatedGenerationPerType/"
ff_pertype     <- paste(dir_pertype,list.files(dir_pertype), sep="")


#  ------------get hydropower info plants from JRC connected to ENTSOE---------------------
jfile <- "../../data/JRC-PPDB-OPEN.ver0.91/JRC_OPEN_UNITS.csv"
hp_jrc <- get_info_HP_jrc(jfile)
# Extract energy data
# CTY - AreaTypeCode= Switerzland
num_year <- c("2015","2016","2017","2018")
nworkers <- detectCores()
# Select Swiss areacode
areacode <- "10YCH-SWISSGRIDZ"
cl <- makeForkCluster(nworkers)
day_prod_yy <- parLapply(cl, 1:length(num_year), function(i) {
  get_energydata(dir_perunit,"generation" , num_year[i], "daily", areacode)})

stopCluster(cl)

df_ener <- melt(day_prod_yy, id=names(day_prod_yy[[1]]))
df_ener$L1 <- NULL

hp_metadata <- hp_jrc$meta_hp[,c("eic_p","eic_g","name_g","capacity_p", "capacity_g","type_g","lat","lon")]
names(hp_metadata)[names(hp_metadata)=="eic_g"] <- "GenerationUnitEIC"

df_enerp <- merge(df_ener, hp_metadata, id="GenerationUnitEIC")
df_enerp <- df_enerp[order(df_enerp$GenerationUnitEIC, df_enerp$date),]
# NOTE Units are MW
save(df_enerp, file="../data/Output_data/daily_generationunit_eic_entsoe.Rda")
