#######################################
# Process ENVIDAT CH2018: TICCINO
#######################################
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();


sel_pw <- read.csv("../../data/Output_data/csv/IDS_with_Ticcino_UP.csv")
hps <- c("Blenio (OFIBLE)" , "AET Leventina")

metafinal   <- read.csv("../../data/Output_data/csv/metadata_final.csv")
total_names <- metafinal$name_p
# Adding Rhineau to metafinal
# total_names <- c(metafinal$name_p, "Rhineau")


# read shapefiles
sh_file <- "/Users/noeliaotero/Documents/OCCR/data/Envidat/catchment_shapes/ch500_trans_wgs84.shp"
nc <- st_make_valid(st_read(sh_file, quiet=TRUE))

discharge_dir <- "../../data/Envidat/climate_simulations/Discharge/"
nfiles  <- list.files(discharge_dir)
d_dis_Bl   <- lapply(1:length(nfiles), function(i) select_disCH18_subcatch(paste(discharge_dir, nfiles[i],sep=""), sel_pw,name_plant = hps[1] ))
d_dis_Lev   <- lapply(1:length(nfiles), function(i) select_disCH18_subcatch(paste(discharge_dir, nfiles[i],sep=""), sel_pw,name_plant = hps[2] ))

# extract model names
m_names <- lapply(nfiles, function(x) strsplit(x,"_")[[1]][4])
names(d_dis_Bl) <- names(d_dis_Lev) <- m_names

############################################
############ CHANGE THIS FOR BLENIO
############################################- 
# open pr and temperature files
pr_ch <- "/Users/noeliaotero/Documents/OCCR/data/meteoswiss/CH2018_selected_catch/Leventina/pr/"
tas_ch <- "../../data/meteoswiss/CH2018_selected_catch/Leventina/tas/"
tasmax_ch <- "../../data/meteoswiss/CH2018_selected_catch/Leventina/tasmax/"


# extract model names for each meteo
pr_dat <- read_ch2018_met(pr_ch)
tas_dat <- read_ch2018_met(tas_ch)
tasmax_dat <- read_ch2018_met(tasmax_ch)


# extract model names for each meteo
pr_dat <- read_ch2018_met(pr_ch)
tas_dat <- read_ch2018_met(tas_ch)
tasmax_dat <- read_ch2018_met(tasmax_ch)

# check missing values and fill it up with moving-window na avg
# the model SMHI-RCA-HADGEM-EUR11-RCP has missing values

pr_dat <- fun_check_NA(pr_dat)
tas_dat <- fun_check_NA(tas_dat)
tasmax_dat <- fun_check_NA(tasmax_dat)
#####################################################
############ CHANGE THIS FOR BLENIO
dis_ch2018 <- d_dis_Lev

pr_dat <- lapply(pr_dat, function(x) {  names(x) <- c("date","tp")
x})
# Filter model names (select those with discharge)
pr_dat <- pr_dat[names(pr_dat)%in%names(dis_ch2018)]

tas_dat <- lapply(tas_dat, function(x) {  names(x) <- c("date","t2m")
x})
tas_dat <- tas_dat[names(tas_dat)%in%names(dis_ch2018)]

tasmax_dat <- lapply(tasmax_dat, function(x) {  names(x) <- c("date","t2mmax")
x})
tasmax_dat <- tasmax_dat[names(tasmax_dat)%in%names(dis_ch2018)]

# Merge variables
hp_models <- lapply(1:length(tas_dat), function(i) { list(pr_dat[[i]], tas_dat[[i]], tasmax_dat[[i]], dis_ch2018[[i]])%>%
    reduce(left_join, by = "date")})
names(hp_models) <- names(dis_ch2018)
# Prepare the data: reverse list
l_hp <- list(hp_models)
names(l_hp) <- hps[2]
new_hp <- fun_list(l_hp)
new_hp <- new_hp[names(new_hp)%in%names(dis_ch2018)]

# Add the rest of variables
metafinal   <- read.csv("../../data/Output_data/csv/metadata_final.csv")
sel_pw <- read.csv("../../data/Output_data/csv/IDS_with_Ticcino_UP.csv")
sel_pw <- sel_pw[sel_pw$name_p==hps[2],]

hp_metadata <- metafinal%>%dplyr::filter(name_p==unique(sel_pw$name_p))%>%dplyr::select(c("eic_p","eic_g","name_p"))
dd_pw <- dd_final <- list()
for (i in 1:length(new_hp)){
  dd_final[[i]] <- add_SCE_SPEI_todata(new_hp[[i]][[1]])
  dd_final[[i]] <- merge(dd_final[[i]],hp_metadata)
  # change the name!
  # names(dd_final[[i]])[names(dd_final[[i]])=="discharge"] <- "PREVAH"
}

names(dd_final) <- names(dis_ch2018)



dout <- paste('/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/data_CH2018_tomodel/',hp_metadata$name_p,sep="")
dir.create(dout)

for (i in 1:length(dd_final)) {
  write.csv(dd_final[[i]], file= paste(dout,"/", names(dd_final[i]), ".csv",sep=""))
}
