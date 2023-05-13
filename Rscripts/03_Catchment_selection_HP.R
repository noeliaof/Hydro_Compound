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

# Updates: need to add the other catchments?
# Add Envidat
# OPen FOEN statistik
bf_file <- "../../data/BFE_stats/annual_stats/Statistics_2018.xlsx"
dat_bfu <- readxl::read_xlsx(bf_file)
cnames <- c("ZE-Nr","ZE-Name","WKA-Name","WKA-Typ","Prod. ohne Umwälzbetrieb - W.","Prod. ohne Umwälzbetrieb - S.","Prod. ohne Umwälzbetrieb - J.","QTurbine [m3/sec]","ZE-Kanton","ZE-Kote","QPumpe [m3/sec]","Inst. Turbinenleistung",
            "Max. Leistung ab Generator","ZE-Koordinaten unscharf (Ost)","ZE-Koordinaten unscharf (Nord)","Proz. Anteil CH")
dat_sel <- data.frame(dat_bfu[,cnames])

dat_sel$lon_reg <- CH.to.WGS.lng(dat_sel$ZE.Koordinaten.unscharf..Ost.,dat_sel$ZE.Koordinaten.unscharf..Nord.)
dat_sel$lat_reg <- CH.to.WGS.lat(dat_sel$ZE.Koordinaten.unscharf..Ost.,dat_sel$ZE.Koordinaten.unscharf..Nord.)
# mauv <- dat_sel[dat_sel$WKA.Name%in%c("Fionnay","Chanrion","Champsec","Riddes"),]
fionnay <- dat_sel[grepl("Fionnay",dat_sel$WKA.Name),]
Chanrion <- dat_sel[grepl("Chanrion",dat_sel$WKA.Name),]
Champsec <- dat_sel[grepl("Champsec",dat_sel$WKA.Name),]
Riddes <- dat_sel[dat_sel$WKA.Name=="Riddes",]
mauv <- rbind(fionnay, Chanrion, Champsec, Riddes)

lev_ritom <- dat_sel[dat_sel$WKA.Name=="Ritom",]
lev_Tremorgio <- dat_sel[dat_sel$WKA.Name=="Tremorgio",]
lev_Bias <- dat_sel[dat_sel$WKA.Name=="Nuova Biaschina",]
lev_Stal <- dat_sel[dat_sel$WKA.Name=="Stalvedro (AET)",]
lev_Piot <- dat_sel[dat_sel$WKA.Name=="Piottino",]
lev <- rbind(lev_Bias, lev_Tremorgio, lev_Stal, lev_Piot)
# need to add Rhineau:
Rheinau <- dat_sel[grepl("Rheinau",dat_sel$WKA.Name),]

hp_jrc <- get_info_HP_jrc(jfile)
meta_final <- hp_jrc$meta_hp[hp_jrc$meta_hp$eic_p%in%final_pw$eic_p,]

ggplot2::ggplot(st_geometry(nc)) +  
  geom_sf(fill="grey") +
  geom_point(data=hp_jrc$meta_hp, aes(x=lon, y=lat), col="blue", size=2) + 
  theme_bw()

pts <- data.frame(meta_final[,c("lat","lon")])

st_crs(nc) <- 4326
points.sf <- st_as_sf(pts, coords = c("lon", "lat"))
st_crs(points.sf) <- st_crs(4326) 
xx <- st_distance(x = points.sf, y = nc)

IDS <- array()
for (i in 1:length(pts$lat)){
  IDS[i] <- which.min(xx[i,])
}


# I checked this manually and the catchment for the rest of Mauvoisin will be 33
# Change Mauvoisin catchment! Now, take 33 instead 167
IDS_up <- IDS
IDS_up[IDS_up==167] <- 33
# only catch
m_swiss <- ggplot2::ggplot(st_geometry(nc)) +  
  geom_sf(fill=ifelse(nc$OBJECTID%in%IDS_up,"lightblue","white"), lwd=0.8) + 
  xlab("") + ylab("")+
  theme_void()
# ggsave(m_swiss, file="../../Results/Hydro_project/map_CH_shapes.png", width = 12, height = 8)
#######################
# MAP SUBMISSION 1
#########################
mp <- ggplot2::ggplot(st_geometry(nc)) +  
  geom_sf(fill=ifelse(nc$OBJECTID%in%IDS_up,"lightblue","white"), lwd=0.2) +
  geom_point(data=meta_final, aes(x=lon, y=lat), col="black", size=3, shape=8) + 
  geom_point(data=final_pw, aes(x=lon_reg, y=lat_reg), col="red", size=3, shape =16, alpha=0.6) + 
  geom_text_repel(data=meta_final,aes(x=lon, y=lat, label=name_p),min.segment.length = Inf, seed = 42, box.padding = 0.5, size=5) + 
  xlab("") + ylab("")+
  theme_void()

ggsave(mp, file="../../Results/Hydro_project/map_shapes_update_Mauvoison.png", width = 12, height = 8)



# Add Mauvoisin and Leventina

mp_extra <- ggplot2::ggplot(st_geometry(nc)) +  
  geom_sf(fill=ifelse(nc$OBJECTID%in%c(IDS,33),"lightblue","white"), lwd=0.2) +
  geom_point(data=meta_final, aes(x=lon, y=lat), col="black", size=3, shape=8) + 
  geom_point(data=final_pw, aes(x=lon_reg, y=lat_reg), col="red", size=3, shape =16, alpha=0.6) +
  geom_point(data=mauv, aes(x=lon_reg, y=lat_reg), col="green", size=3, shape =16, alpha=0.6) +
  geom_point(data=lev, aes(x=lon_reg, y=lat_reg), col="green", size=3, shape =16, alpha=0.6) +
  geom_text_repel(data=meta_final,aes(x=lon, y=lat, label=name_p),min.segment.length = Inf, seed = 42, box.padding = 0.5, size=5) + 
  xlab("") + ylab("")+
  theme_void()


########################################################################
# For the revised version we need to update the Rheinfelden catchment
# Rhein catchment: UPdates: use the subcatchments!  
########################################################################

f_sh_rhein <-  "../../../OCCR/data/Rhein_shapefiles/142378.shp"
sh_rein <- st_read(f_sh_rhein)
centroides <- sf::st_centroid(nc)

id_int <-   sf::st_intersection(sh_rein, centroides) %>% 
       sf::st_set_geometry(NULL)

IDs_rhein <- id_int$OBJECTID

# check 
ggplot() + 
  geom_sf(data=nc, fill=ifelse(nc$OBJECTID%in%c(id_int$OBJECTID),"lightblue","white"), color="black") +
  # overlay the whole Rhein 
  geom_sf(data=sh_rein, fill="light yellow", color="red", alpha=0.2) + theme_bw()


IDS_up_rhein <- IDS_up
IDS_up_rhein <- IDS_up_rhein[IDS_up_rhein!=150] # remove the old Rheinfelden ID
IDS_up_rhein <- c(IDS_up_rhein,IDs_rhein)

# # select the area as well
ids <-  nc[nc$OBJECTID%in%IDs_rhein,]
ids$OBJECTID <- as.integer(ids$OBJECTID)
df_IDS <- data.frame(IDS_up_rhein)
#  Need to adapt the names from the metadata
new_metadata_with_rhein <- meta_final[meta_final$name_p != "KW Rheinfelden CH",]

# # Add the data from BFU
new_metadata_with_rhein[6,] <- meta_final[meta_final$name_p == "KW Rheinfelden CH",]
# # add more lines for the subchat

for( i in 1:length(df_IDS$IDS_up_rhein)){
  ix <- which(nc$OBJECTID%in%df_IDS$IDS_up_rhein[i])
  df_IDS[i,"Area"] <- nc[ix, "Shape_Area"]$Shape_Area
}

df_IDS$name_p <- NA
df_IDS$name_p[1:5] <- new_metadata_with_rhein$name_p[1:5]
df_IDS$name_p[6:length(df_IDS$IDS_up_rhein)] <- new_metadata_with_rhein$name_p[6]

final_pw <- merge(final_pw, df_IDS, id="name_p")
write.csv(df_IDS, file="../../data/Output_data/csv/IDS_with_Rhein_UP.csv",row.names=FALSE)

###########################################
# Create shapefiles of each catchement
############################################
dir_sh <- "/Users/noeliaotero/Documents/OCCR/data/Shapefiles_powerplants/"


Rheinfelden_sh <- nc[nc$OBJECTID%in%c(id_int$OBJECTID),] 
st_write(Rheinfelden_sh,  paste(dir_sh,"Rheinfelden/Rheinfelden_wgs84.shp",sep=""))

Blenio_sh <- nc[nc$OBJECTID%in%c(81,1),] 
st_write(Blenio_sh,  paste(dir_sh,"Blenio/Blenio_new_wgs84.shp",sep=""))

ElectraMassa <- nc[nc$OBJECTID%in%c(260),] 
st_write(ElectraMassa,  paste(dir_sh,"Electra-Massa/Electra-Massa_wgs84.shp",sep=""))

Leventina <- nc[nc$OBJECTID%in%c(83,274),] 
st_write(Leventina,  paste(dir_sh,"Leventina/Leventina_new_wgs84.shp",sep=""))

Emosson <- nc[nc$OBJECTID%in%c(100),] 
st_write(Emosson,  paste(dir_sh,"Emosson/Emosson_wgs84.shp",sep=""))
#######
# plot
######
# ggplot() + 
#   geom_sf(data=nc, fill=ifelse(Emosson$OBJECTID, Blenio_sh$OBJECTID, Leventina$OBJECTID, ElectraMassa$OBJECTID, Rheinfelden_sh$ ,"lightblue","white"), color="black") +
#   # overlay the whole Rhein 
#   geom_sf(data=sh_rein, fill="light yellow", color="red", alpha=0.2) + theme_bw()


######################################
# Double-check with Balmer database 
######################################
proj_env <- CRS("+proj=somerc +lat_0=46.95240555555556                              
  +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
  +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
# WGS84--regular--
prj.LatLong <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# read files
scheme_names <- readxl::read_xlsx("../../data/fromBettina/BalmerDatabase/SchemesNames_Balmer.xlsx")
# Check which ID (FROM BALMER DB):
mauvoisin_ID <- scheme_names[grep("Mauv",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID
Emosson_ID <- scheme_names[grep("Emosson",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID
ElectraMassa_ID <- scheme_names[grep("Electra Massa",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID
Blenio_ID <- scheme_names[grep("Blenio",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID
Leventina_ID <- c(224.1, 224.2, 224.3, 224.4) #scheme_names[grep("Stalvedro",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID
Rheinfelden_ID <- 65.1 #scheme_names[grep("Rheinfelden",scheme_names$SCHEME_NAM),"SCHEME_ID"]$SCHEME_ID

IDs_Balmer <- c(mauvoisin_ID, Emosson_ID, ElectraMassa_ID, Blenio_ID, Leventina_ID, Rheinfelden_ID)

balm_nc <- st_read("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region.shp")
st_crs(balm_nc) <- proj_env
new_ezg_trans <-  st_transform(balm_nc, prj.LatLong) 
st_centroid(new_ezg_trans)
# 
ezg_balmer <- readOGR("../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region.shp")
raster::crs(ezg_balmer) <- proj_env
ezg_trans <- spTransform(ezg_balmer, prj.LatLong)

plot(ezg_trans, col=ifelse(ezg_trans$SCHEME_ID%in%IDs_Balmer,"yellow","white"), axes=TRUE)
points(meta_final$lon, meta_final$lat,col="red", pch=8, cex=1)


st_write(new_ezg_trans, dsn = "../../data/fromBettina/BalmerDatabase/HydroGIS_Bettina/EZG_BalmerDischarge_region_W84.shp", layer = "ID", driver = "ESRI Shapefile")






##### Save the shapefile of Rhine within PREVAH
Rhein_shapefiles <- nc[nc$OBJECTID%in%c(id_int$OBJECTID),]
file_sh <-  "../../data/Envidat/st_Rhein.shp"
st_write(Rhein_shapefiles, dsn = file_sh, layer = "ID", driver = "ESRI Shapefile")
# test open
test <- st_read(file_sh)

####################################################################################################
###################This part is not needed anymore##################################################
# Open Rheineu subcatchments
#
# rhein_subcatch <- readxl::read_xlsx("../../data/Envidat/PREVAH_subcatchments_Rheinau.xlsx")
# # Remove all Rheinfelden 150
# 
# IDS_up_rheineau <- IDS_up
# IDS_up_rheineau <- IDS_up_rheineau[IDS_up_rheineau!=150] 
# IDS_up_rheineau <- c(IDS_up_rheineau,rhein_subcatch$OBJECTID)
# # select the area as well
# ids <-  nc[nc$OBJECTID%in%IDS_up_rheineau,]
# ids$OBJECTID <- as.integer(ids$OBJECTID)
# 
# df_IDS <- data.frame(IDS_up_rheineau)
# # Need to adapt the names from the metadata
# new_metadata_with_rhein <- meta_final[meta_final$name_p != "KW Rheinfelden CH",]
# 
# # Add the data from BFU
# new_metadata_with_rhein[6,] <- meta_final[meta_final$name_p == "KW Rheinfelden CH",]
# # add more lines for the subchat
# 
# for( i in 1:length(ids$OBJECTID)){
#   ix <- which(ids$OBJECTID%in%df_IDS$IDS_up_rheineau[i])
#   df_IDS[i,"Area"] <- ids[ix, "Shape_Area"]
# }
# 
# df_IDS$name_p <- NA
# df_IDS$name_p[1:5] <- new_metadata_with_rhein$name_p[1:5]
# df_IDS$name_p[6:length(df_IDS$IDS_up_rheineau)] <- new_metadata_with_rhein$name_p[6]
# 
# final_pw <- merge(final_pw, df_IDS, id="name_p")
# 
# ggplot2::ggplot(st_geometry(nc)) +  
#   geom_sf(fill=ifelse(nc$OBJECTID%in%df_IDS$IDS_up_rheineau,"grey","white")) +
#   geom_point(data=meta_final, aes(x=lon, y=lat), col="blue", size=2) + 
#   geom_point(data=final_pw, aes(x=lon_reg, y=lat_reg), col="red", size=2) + 
#   geom_point(data=Rheinau,aes(x=lon_reg,y= lat_reg), col="green") + 
#   geom_text_repel(data=meta_final,aes(x=lon, y=lat, label=name_p),box.padding = 0.5, max.overlaps = Inf) 
#   theme_bw()
# 
# 
# names(df_IDS) <- c("IDS", "Area",  "name_p")
# write.csv(df_IDS, file="../../data/Output_data/csv/IDS_updated_with_rhein_subcatchments.csv",row.names=FALSE)
# 
# 
# # Check with the shapefiles from the power plants
# l_shp <- list.dirs("../../data/Shapefiles_powerplants/")[-1]
# 
# l_rds <- lapply(1:5, function(i) st_read(l_shp[i]))
# df_rs <- sf::st_as_sf(data.table::rbindlist(l_rds))
# 
# # extract rhineu catch
# 
# st_rhineau <- nc  %>%dplyr::filter(OBJECTID%in% rhein_subcatch$OBJECTID)
# st_rhineau$OBJECTID <- as.character(st_rhineau$OBJECTID)
# st_write(st_rhineau, "../../data/Envidat/st_rhineau.shp", driver = "ESRI Shapefile")
