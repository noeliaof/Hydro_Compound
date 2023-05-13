# Catchment selection Leventina and Blenio


ID_Lev_valley <- c(83,274)

ID_Blenio <- c(81,1)

# Select the catchments for future scenarios
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();


# Select the catchments for future scenarios
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# ----------------Open discharge Envidat->PREVAH 
sh_file <- "/Users/noeliaotero/Documents/OCCR/data/Envidat/catchment_shapes/ch500_trans_wgs84.shp"
nc <- st_make_valid(st_read(sh_file, quiet=TRUE))

jfile <- "../../data/JRC-PPDB-OPEN.ver0.91/JRC_OPEN_UNITS.csv"
# load the selected catchments:
final_pw <- read.csv("../../data/Output_data/csv/metadata_final.csv")


########################################################################
# For the revised version we need to update the Rheinfelden catchment
# Leventina and Blenio catchment: UPdates: use the subcatchments!  
########################################################################

IDs_prev <- read.csv("../../data/Output_data/csv/IDS_with_Rhein_UP.csv")

names_p <- IDs_prev$name_p
# Remove Blenio and Leventina for updating
IDs_prev <- IDs_prev[!IDs_prev$IDS_up_rhein%in%c(ID_Lev_valley, ID_Blenio,225),]
names(IDs_prev) <- c("IDS","Area","name_p")
# # select the area as well
ids_tic <-  nc[nc$OBJECTID%in%c(ID_Lev_valley,ID_Blenio),]
ids_tic$OBJECTID <- as.integer(ids_tic$OBJECTID)
df_IDS_tic <- data.frame("IDS"=c(ids_tic$OBJECTID))

# # add more lines for the subchat

for( i in 1:length(df_IDS_tic$ID)){
  ix <- which(nc$OBJECTID%in%df_IDS_tic$ID[i])
  df_IDS_tic[i,"Area"] <- nc[ix, "Shape_Area"]$Shape_Area
}

df_IDS_tic$name_p <- c("Blenio (OFIBLE)","Blenio (OFIBLE)","AET Leventina","AET Leventina")
df_tic <- rbind(IDs_prev, df_IDS_tic)

write.csv(df_tic, file="../../data/Output_data/csv/IDS_with_Ticcino_UP.csv",row.names=FALSE)





# Extract the shapefileBalmer


proj_env <- CRS("+proj=somerc +lat_0=46.95240555555556                              
  +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
  +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
# WGS84--regular--
prj.LatLong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")



ezg_balmer <- readOGR("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region.shp")
balm_nc <- st_read("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region.shp")
ezg_sh <- readOGR("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerShort_region.shp")

raster::crs(ezg_balmer) <- proj_env
ezg_trans <- spTransform(ezg_balmer, prj.LatLong)
st_crs(balm_nc) <- proj_env
new_ezg_trans <-  st_transform(balm_nc, prj.LatLong) 
st_centroid(new_ezg_trans)


# read files
scheme_names <- readxl::read_xlsx("../../data/fromBettina/BalmerDatabase/SchemesNames_Balmer.xlsx")
# Check which ID (FROM BALMER DB):
Blenio_ID <- scheme_names[grep("Blenio",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID
Leventina_ID <- c(224.1, 224.2, 224.3, 224.4) #scheme_names[grep("Stalvedro",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID


dir_sh <- "/Users/noeliaotero/Documents/OCCR/data/Shapefiles_powerplants/"
blenio_sh <- new_ezg_trans[new_ezg_trans$SCHEME_ID%in%Blenio_ID,]
st_write(blenio_sh,  paste(dir_sh,"Blenio/Blenio_Balmer_wgs84.shp",sep=""))


Leventina_sh <- new_ezg_trans[new_ezg_trans$SCHEME_ID%in%Leventina_ID,]
st_write(Leventina_sh,  paste(dir_sh,"Leventina/Leventina_Balmer_wgs84.shp",sep=""))

