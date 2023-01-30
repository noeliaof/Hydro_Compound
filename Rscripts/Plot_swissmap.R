setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();


# Read one file from SFON files and copy to PREVAH
# open prevah
sh_file <- "/Users/noeliaotero/Documents/OCCR/data/Envidat/catchment_shapes/ch500.shp"
shp     <- readOGR(dsn = sh_file, stringsAsFactors = F)
summary(shp@data)
swiss_pj <- CRS("+proj=somerc +lat_0=46.9524055555556 +lon_0=7.43958333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel
  +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
proj_env <- CRS("+proj=somerc +lat_0=46.95240555555556 
  +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
  +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")
raster::crs(shp) <- proj_env
# Transform
shp_transf  <- spTransform(shp, swiss_pj)

# Save new shapefiles
writeOGR(shp_transf,  "ch500_trans_swiss_pj.shp",layer = "ID", driver = "ESRI Shapefile")

library(bfsMaps)

options(bfsMaps.base="/Users/noeliaotero/Documents/OCCR/data/Themakart2022/")

RequireMap(c("polg.map", "bezk.map", "kant.map", "stkt.pnt","prevah.map"))

# Plot municipalities
#plot(tkart$polg.map, border="grey85" )
#plot(tkart$bezk.map, border="grey55")
plot(tkart$prevah.map, border="grey55", lwd =1)
plot(tkart$kant.map, border="black", lwd=1, add=TRUE )

AddLakes()
AddRivers()


sh_id <- read.csv("../../data/Output_data/csv/IDS_updated_Mauv_envidat.csv")

plot(tkart$prevah.map[as.integer(tkart$prevah.map$OBJECTID)%in%sh_id$IDS,],col="gray94", add=TRUE )

########################################
# Add Envidat
# OPen FOEN statistik
# Add the Hydropower plants
jfile <- "../../data/JRC-PPDB-OPEN.ver0.91/JRC_OPEN_UNITS.csv"
# load the selected catchments:
final_pw <- read.csv("../../data/Output_data/csv/metadata_final.csv")
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

hp_jrc     <- get_info_HP_jrc(jfile)
meta_final <- hp_jrc$meta_hp[hp_jrc$meta_hp$eic_p%in%final_pw$eic_p,]

########################
# Adding points
########################

mydf   <- data.frame(meta_final[,c("lon","lat")])
names(mydf) <- c("longitude","latitude")
points <- st_as_sf(mydf, coords = c("longitude", "latitude"), crs = 4326)
points <- st_transform(points, crs = swiss_pj)
dff <- as(points, 'Spatial')
points(dff@coords, pch=21, col="grey",size=4, bg="red")





######################################################
# Information power plants: for the supplement table
#####################################################
jfile <- "../../data/JRC-PPDB-OPEN.ver0.91/JRC_OPEN_UNITS.csv"
# Information Wasta
wasta_file <- "../../data/BFE_stats/2021  Statistik der Wasserkraftanlagen der Schweiz 1.1.2022.xlsx"
shp_file <- "/Users/noeliaotero/Documents/OCCR/data/9339-Statistik_2018/ch.bfe.statistik-wasserkraftanlagen /HydropowerPlant.shp"
# OPen FOEN statistik

dat_bfu <- readxl::read_xlsx(wasta_file)
cnames <- c("ZE-Nr","ZE-Name","WKA-Name","WKA-Typ","Prod. ohne Umwälzbetrieb - W.","Prod. ohne Umwälzbetrieb - S.","Prod. ohne Umwälzbetrieb - J.","QTurbine [m3/sec]","ZE-Kanton","ZE-Kote","QPumpe [m3/sec]","Inst. Turbinenleistung",
            "Max. Leistung ab Generator","ZE-Koordinaten unscharf (Ost)","Maxim. Nettofallhöhe [m]","ZE-Koordinaten unscharf (Nord)","Proz. Anteil CH")
dat_sel <- data.frame(dat_bfu[,cnames])


# Leventina
lev_ritom <- dat_sel[dat_sel$WKA.Name=="Ritom",]
lev_Tremorgio <- dat_sel[dat_sel$WKA.Name=="Tremorgio",]
lev_Bias <- dat_sel[dat_sel$WKA.Name=="Nuova Biaschina",]
lev_Stal <- dat_sel[dat_sel$WKA.Name=="Stalvedro (AET)",]
lev_Piot <- dat_sel[dat_sel$WKA.Name=="Piottino",]
lev <- rbind(lev_Bias, lev_Tremorgio, lev_Stal, lev_Piot)
# Blenio
Blenio <- dat_sel[grepl("Biasca",dat_sel$WKA.Name),]

rheinfelden <- dat_sel[dat_sel$WKA.Name == "Rheinfelden",]

# Mauvoisin
fionnay <- dat_sel[grepl("Fionnay",dat_sel$WKA.Name),]
Chanrion <- dat_sel[grepl("Chanrion",dat_sel$WKA.Name),]
Champsec <- dat_sel[grepl("Champsec",dat_sel$WKA.Name),]
Riddes <- dat_sel[dat_sel$WKA.Name=="Riddes",]
mauv <- rbind(fionnay, Chanrion, Champsec, Riddes)
# Massa
electramassa <- dat_sel[grepl("Bitsch",dat_sel$WKA.Name),]

# Emosson
emosson <- dat_sel[grepl("Vallorcine",dat_sel$ZE.Name),]

list_pw <- list("Kraftwerke Mauvoisin AG"=c("Fionnay", "Chanrion", "Champsec", "Riddes"), "AET Leventina"=c( "Tremorgio", "Nuova Biaschina", "Stalvedro (AET)", "Piottino"), 
                "Blenio (OFIBLE)"= dat_sel[dat_sel$name_p=="Blenio (OFIBLE)","WKA.Name"],"Emosson (ESA)"= info_wasta[info_wasta$name_p=="Emosson (ESA)","WKA.Name"] , 
                "Electra-Massa (EM)" =info_wasta[info_wasta$name_p=="Electra-Massa (EM)","WKA.Name"], "KW Rheinfelden CH"= c("Rheinfelden"), "Rheinau"= c("Rheinau"))



