##############################################
# Process New Catchment of Mauvoisin CH2018
############################################
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();
#################
# Get dircharge
################
dir <- '/Users/noeliaotero/Documents/OCCR/data/Envidat/Mauvoisin/CH2018/'

# extract model names for each meteo
f_nam <- (lapply(list.files(dir), function(x) strsplit(x,"_")[[1]][[1]]))
dat_catch <- lapply(paste(dir, list.files(dir), '/Mou200/Mou200.mit', sep=""), 
                      function(x) read.table(x, fill=TRUE, header=FALSE))
  
dis_raw      <- lapply(dat_catch, function(x) x[-c(1,2),c("V1","V2","V3","V14")])
fun_preproc <- function(x){
  x$date <- as.Date(paste(x$V1,x$V2,x$V3, sep="-"))
  dis    <- x%>%dplyr::select(c("date","V14"))%>%dplyr::filter(format(date,"%Y")>1980)%>%dplyr::rename("discharge"="V14")
  dis$discharge <- as.numeric(dis$discharge)
    
  return(dis)
}
dis_ch2018 <- lapply(dis_raw, fun_preproc)
names(dis_ch2018) <- f_nam


#### Need to add the proper variables --- 
# open pr and temperature files
pr_ch <- "../../data/meteoswiss/CH2018_selected_catch/Mauvoisin/pr/"
tas_ch <- "../../data/meteoswiss/CH2018_selected_catch/Mauvoisin/tas/"
tasmax_ch <- "../../data/meteoswiss/CH2018_selected_catch/Mauvoisin/tasmax/"


# extract model names for each meteo
pr_dat <- read_ch2018_met(pr_ch)
tas_dat <- read_ch2018_met(tas_ch)
tasmax_dat <- read_ch2018_met(tasmax_ch)
# check missing values and fill it up with moving-window na avg
# the model SMHI-RCA-HADGEM-EUR11-RCP has missing values

pr_dat <- fun_check_NA(pr_dat)
tas_dat <- fun_check_NA(tas_dat)
tasmax_dat <- fun_check_NA(tasmax_dat)


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
mau_models <- lapply(1:length(tas_dat), function(i) { list(pr_dat[[i]], tas_dat[[i]], tasmax_dat[[i]], dis_ch2018[[i]])%>%
                            reduce(left_join, by = "date")})
names(mau_models) <- names(dis_ch2018)
# Prepare the data: reverse list
l_mauvoisin <- list("Mauvoisin"= mau_models)
new_mauvoisin <- fun_list(l_mauvoisin)
new_mauvoisin <- new_mauvoisin[names(new_mauvoisin)%in%names(dis_ch2018)]

# Add the rest of variables
metafinal   <- read.csv("../../data/Output_data/csv/metadata_final.csv")
sel_pw <- read.csv("../../data/Output_data/csv/IDS_with_Rhein_UP.csv")

mau_metadata <- metafinal%>%dplyr::filter(name_p=="Kraftwerke Mauvoisin AG")%>%dplyr::select(c("eic_p","eic_g","name_p"))
dd_pw <- dd_final <- list()
for (i in 1:length(new_mauvoisin)){
  dd_final[[i]] <- add_SCE_SPEI_todata(new_mauvoisin[[i]]$Mauvoisin)
  dd_final[[i]] <- merge(dd_final[[i]],mau_metadata)
  # change the name!
 # names(dd_final[[i]])[names(dd_final[[i]])=="discharge"] <- "PREVAH"
}

names(dd_final) <- names(dis_ch2018)



dout <- paste('/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/data_CH2018_tomodel/',mau_metadata$name_p,sep="")

for (i in 1:length(dd_final)) {
  write.csv(dd_final[[i]], file= paste(dout,"/", names(dd_final[i]), ".csv",sep=""))
}
