setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

########################################
# First visualize the shapefile
########################################
sh_file <- "/Users/noeliaotero/Documents/OCCR/data/Shapefiles_powerplants/Mauvoisin_Update/catchMauvoisin_region.shp"
shp     <- readOGR(dsn = sh_file, stringsAsFactors = F)
proj_env <- CRS("+proj=somerc +lat_0=46.95240555555556 
  +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
  +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
raster::crs(shp) <- proj_env
# Original 
centralp_orig <- lapply(1:length(shp@polygons), function(i) shp@polygons[[i]]@Polygons[[1]]@labpt)
# crs is empty
prj.LatLong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
shp_transf  <- spTransform(shp, prj.LatLong)
# save the transformed shapes
writeOGR(shp_transf,  "Mau_trans_wgs84.shp",layer = "ID", driver = "ESRI Shapefile")

############################
# Historical conditions
############################
mau_area_km2 <- shp$AREA
mydir <- "../../data/Envidat/Mauvoisin/CTRL_1981_2016_g73/Mou200/"
dtable <- read.table("../../data/Envidat/Mauvoisin/CTRL_1981_2016_g73/Mou200/Mou200.mit",fill = TRUE , header = FALSE)

# Data with subcatchments
raw_data <- read.table("../../data/Envidat/Mauvoisin/CTRL_1981_2016_g73/Mou200/Mou200_eval.RGS",fill = TRUE , header = FALSE)
names(raw_data)[1] <- "date"
names(raw_data) <- c("date",paste("V",seq(1,12), sep=""))
raw_data$date <- as.Date(as.character(raw_data$date),format= "%Y%m%d")

# Here I only need the discharge over the domain (Mauvoisin)

dis_raw      <- dtable[-c(1,2),c("V1","V2","V3","V14")]
dis_raw$date <- as.Date(paste(dis_raw$V1,dis_raw$V2,dis_raw$V3, sep="-"))
dis_81_21    <- dis_raw%>%dplyr::select(c("date","V14"))%>%dplyr::filter(format(date,"%Y")>1980)%>%dplyr::rename("PREVAH"="V14")
dis_81_21$PREVAH <- as.numeric(dis_81_21$PREVAH)
# QUICK CHECK
#df_mau_red <- dis_80_21%>%dplyr::filter(format(date,"%Y")<2010)%>%dplyr::select(c("date","PREVAH"))
#mau_mon <- df_mau_red%>%group_by(mon=format(date,"%m"))%>%dplyr::summarise(mon_dis = mean(as.numeric(PREVAH)), dis_sum = sum(as.numeric(PREVAH))/29)

# Merge with the ENTSO-E
# Load ENTSOE long term data
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

# ONLY NEED MAUVOISON
data_extended <- merge(l_entsoe$`Kraftwerke Mauvoisin AG`, dis_81_21, id="date", all=TRUE)
# remove 1980
data_extended <- data_extended%>%dplyr::filter(format(date,"%Y")>"1980")

# Add the window-moving average as for the observed discharge
data_extended <- data_extended%>%dplyr::mutate(PREVAH7D=rollapply(PREVAH, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                            by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
data_extended <- data_extended%>%dplyr::mutate(PREVAH15D=rollapply(PREVAH, width=15, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                             by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
data_extended <- data_extended%>%dplyr::mutate(PREVAH30D=rollapply(PREVAH, width=30, FUN=function(x) mean(x, na.rm=TRUE), 
                                                                             by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))

##########################################
# Now add the meteorological variables
#########################################
mau_t2m    <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Mauvoison/t2m_mau_197901_202112.csv")
# rename columns
names(mau_t2m) <- c("date","t2m")
mau_t2m$date <- as.Date(mau_t2m$date)
mau_t2mmax <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Mauvoison/t2max_mau_197901_202112.csv")
names(mau_t2mmax) <- c("date","t2mmax")
mau_t2mmax$date <- as.Date(mau_t2mmax$date)
mau_pr     <- read.csv("/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/Observed_discharge_entsoe_powerplants/meteovars_avg_finalpowerplants/Mauvoison/tp_mau_197901_202112.csv")
mau_pr$date <- as.Date(mau_pr$date)
names(mau_pr) <- c("date","tp")

# Merge with the discharge and energy data
df_met <- Reduce(function(x, y) merge(x, y, all=TRUE), list(data_extended, mau_t2m, mau_t2mmax, mau_pr))%>%dplyr::filter(format(date,"%Y")>1980)
# Add the index
df_met_ind <- add_SCE_SPEI_todata(df_met)
names(df_met_ind)[names(df_met_ind)=="discharge"] <- "PREVAH"
# Save the data
outdir <- "../../data/Output_data/csv/PREVAH_discharge_entsoe_powerplants/mergedata_final/mergedata_with_index/"
write.csv(df_met_ind, file=paste(outdir, unique(df_met_ind$name_p[!is.na(df_met_ind$name_p)]), ".csv",sep = ""), row.names = FALSE)

