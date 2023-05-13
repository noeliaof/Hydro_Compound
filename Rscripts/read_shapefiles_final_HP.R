setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

proj_env <- CRS("+proj=somerc +lat_0=46.95240555555556                              
  +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
  +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
# WGS84--regular--
prj.LatLong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


sh_Tic <-readOGR("../../data/EsriShapes/catchmentsTicinoCOrr_region.shp")
sh_Mau <-readOGR("../../data/EsriShapes/catchsMauvEmoMassa_region.shp")
sh_Rhein <-readOGR("../../data/EsriShapes/RheinfeldenBAFU2091_partoutsideCH_region.shp")



ezg_balmer <- readOGR("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region.shp")
balm_nc <- st_read("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region.shp")
ezg_sh <- readOGR("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerShort_region.shp")
# swisstopo
lakes <- st_read("../../data/fromBettina/BalmerDatabase/SwissTopo/lacs_region.shp")
kantons <- st_read("../../data/fromBettina/BalmerDatabase/SwissTopo/cantons_region.shp")

raster::crs(ezg_balmer) <- proj_env
ezg_trans <- spTransform(ezg_balmer, prj.LatLong)
st_crs(balm_nc) <- proj_env
new_ezg_trans <-  st_transform(balm_nc, prj.LatLong) 
st_centroid(new_ezg_trans)

# Convert projection 
sh_Rhein_wg84 <-st_read("../../data/Shapefiles_powerplants/Rheinfelden/142378.shp")

rein_trans <- st_transform(sh_Rhein_wg84, proj_env)
st_write(rein_trans, "../../data/Shapefiles_powerplants/Rheinfelden/Rhein_proj_env.shp")

