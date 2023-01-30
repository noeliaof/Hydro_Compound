###########################
# Process ENVIDAT CH2018
##########################
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# read the selected catchments
# sel_pw <- read.csv("../../data/Output_data/csv/IDS_envidat.csv")

sel_pw <- read.csv("../../data/Output_data/csv/IDS_updated_Mauv_envidat.csv")
metafinal <- read.csv("../../data/Output_data/csv/metadata_final.csv")
# Adding Rhineau to metafinal
total_names <- c(metafinal$name_p, "Rhineau")


# read shapefiles
sh_file <- "/Users/noeliaotero/Documents/OCCR/data/Envidat/catchment_shapes/ch500_trans_wgs84.shp"
nc <- st_make_valid(st_read(sh_file, quiet=TRUE))

discharge_dir <- "../../data/Envidat/climate_simulations/Discharge/"
nfiles  <- list.files(discharge_dir)
d_dis   <- lapply(1:length(nfiles), function(i) select_dis_UP_subcatch(paste(discharge_dir, nfiles[i],sep=""), sel_pw))
# extract model names
m_names <- lapply(nfiles, function(x) strsplit(x,"_")[[1]][4])
names(d_dis) <- m_names
######################################################################
# Note: Rheinfelden is Rhineau!!!!!!!!!!! I won't rename it for now.
#####################################################################

#### Need to add the proper variables --- 
# open pr and temperature files
pr_ch <- "../../data/meteoswiss/CH2018_selected_catch/pr/used/"
tas_ch <- "../../data/meteoswiss/CH2018_selected_catch/tas/used/"
tasmax_ch <- "../../data/meteoswiss/CH2018_selected_catch/tasmax/used/"


# extract model names for each meteo
pr_dat <- read_ch2018_met(pr_ch)
tas_dat <- read_ch2018_met(tas_ch)
tasmax_dat <- read_ch2018_met(tasmax_ch)

# check missing values and fill it up with moving-window na avg
# the model SMHI-RCA-HADGEM-EUR11-RCP has missing values

pr_dat <- fun_check_NA(pr_dat)
tas_dat <- fun_check_NA(tas_dat)
tasmax_dat <- fun_check_NA(tasmax_dat)

# Load Rhineau
Rhineau_pr_ch <- "../../data/meteoswiss/CH2018_selected_catch/Rhineau/pr/"
Rhineau_tas_ch <- "../../data/meteoswiss/CH2018_selected_catch/Rhineau/tas/"
Rhineau_tasmax_ch <- "../../data/meteoswiss/CH2018_selected_catch/Rhineau/tasmax/"

Rhineau_pr_dat <- read_ch2018_met(Rhineau_pr_ch)
Rhineau_pr_dat <- lapply(Rhineau_pr_dat, function(x) {  names(x) <- c("date","tp")
                  x})
Rhineau_pr_dat <- Rhineau_pr_dat[names(Rhineau_pr_dat)%in%names(dat_models)]
Rhineau_tas_dat <- read_ch2018_met(Rhineau_tas_ch)
Rhineau_tas_dat <- lapply(Rhineau_tas_dat, function(x) {  names(x) <- c("date","t2m")
x})
Rhineau_tas_dat <- Rhineau_tas_dat[names(Rhineau_tas_dat)%in%names(dat_models)]
Rhineau_tasmax_dat <- read_ch2018_met(Rhineau_tasmax_ch)
Rhineau_tasmax_dat <- lapply(Rhineau_tasmax_dat, function(x) {  names(x) <- c("date","t2mmax")
                    x})
Rhineau_tasmax_dat <- Rhineau_tasmax_dat[names(Rhineau_tasmax_dat)%in%names(dat_models)]

# Need to add discharge from the new-update Rheinfelden

Rhineau_discharge_dat <- lapply(d_dis, function(x) x[,c("date", "KW Rheinfelden CH")])
Rhineau_discharge_dat <- lapply(Rhineau_discharge_dat, function(x) {  names(x) <- c("date","discharge")
x})

###############
# merge files #
###############
# apply to each model
dat_models        <- lapply(1:length(pr_dat), function(i) merge_variables(dis = d_dis[[i]], pr = pr_dat[[i]], tas = tas_dat[[i]], tasmax = tasmax_dat[[i]], metafinal))
names(dat_models) <- names(pr_dat)
# Prepare Rhienau and merge

rhineau_models <- lapply(1:length(Rhineau_tas_dat), function(i) { list(Rhineau_pr_dat[[i]], Rhineau_tas_dat[[i]], Rhineau_tasmax_dat[[i]], Rhineau_discharge_dat[[i]])%>%
                           reduce(left_join, id = "date")})
names(rhineau_models) <- names(Rhineau_tas_dat)

l_rhineau <- list("Rhineau"= rhineau_models)

new_rhineau <- fun_list(l_rhineau)
new_rhineau <- new_rhineau[names(new_rhineau)%in%names(dat_models)]

# Now concatenate to dat_models
dat_tot_models <- mapply(append, dat_models, new_rhineau, SIMPLIFY = F)

# create lags --this takes a while
dd_pw <- dd_final <- list()
for (i in 1:length(dat_models)){
  for (j in 1:length(total_names)){
    # dd_pw[[j]] <- add_SCE_SPEI_todata(dat_models[[i]][[j]])
    dd_pw[[j]] <- add_SCE_SPEI_todata(dat_tot_models[[i]][[j]])
  }
  
  dd_final[[i]] <- dd_pw
  names(dd_final[[i]]) <- total_names
}
names(dd_final) <- names(dat_models)

dd_rev <- fun_rev(dd_final)


#
###################
# save data
dout <-  "../../data/Output_data/csv/data_CH2018_tomodel/"

for( i in 1:length(total_names)){
  d <- paste(dout,total_names[i], sep="")
  dir.create(d, showWarnings = FALSE)
  l_pw <- dd_rev[[which(names(dd_rev)==total_names[i])]]
  lapply(1:length(l_pw), function(i) write.csv(l_pw[[i]], file = paste(d, "/",names(l_pw[i]), ".csv", sep="")))
  
}


# No need this part
#################
# Add daily STI
#################
dd_tmp <- dd_all <- list()
for (i in 1:length(dd_rev)){
  dd_pw <- dd_rev[[i]]
  for(j in 1:length(dd_pw)){
    dd_tmp[[j]] <-  add_daily_STI(dd_pw[[j]])
  }
  names(dd_tmp) <- total_names
  dd_all[[i]] <- dd_tmp
}

names(dd_all) <- names(dd_rev)

#Make some checks with hist
sel_pw <- read.csv("../../data/Output_data/csv/IDS_updated_Mauv_envidat.csv")
metafinal <- read.csv("../../data/Output_data/csv/metadata_final.csv")

raw_env <- read.table("../../data/Envidat/ch_500_eval.RGS_version_bilanz_81_21")

# Check if re-naming the columns V+1
names(raw_env)[1] <- "Date"
names(raw_env) <- c("Date",paste("V",seq(1,307), sep=""))


his_81_21 <- get_prevah(raw_env, sel_pw)
mau <- his_81_21$`Kraftwerke Mauvoisin AG`%>%group_by(month=format(date,"%m"))%>%dplyr::summarise(D=mean(discharge))
mod_mau <- dd_final$`Kraftwerke Mauvoisin AG`$`DMI-HIRHAM-ECEARTH-EUR11-RCP26`%>%group_by(month=format(date,"%m"))%>%dplyr::summarise(D=mean(discharge))

p <- ggplot(mau, aes(x=month, D, gruop=1)) + geom_line() 

# save data
dout <-  "../../data/Output_data/csv/data_CH2018_tomodel/"

for( i in 1:length(metafinal$name_p)){
  pw <- metafinal$name_p[i]
  d <- paste(dout,pw, sep="")
  dir.create(d, showWarnings = FALSE)
  l_pw <- dd_rev[[which(names(dd_rev)==pw)]]
  lapply(1:length(l_pw), function(i) write.csv(l_pw[[i]], file = paste(d, "/",names(l_pw[i]), ".csv", sep="")))
  
}



