######################################################################################
# Data preparation for publish
# The original data is larger
# To upload the data for reproducibility I will prepare accordingly the dataset
# NOF: May 23
#####################################################################################
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# Set -up the ouput
dir_recons     <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Revision_Mar23/ClassicalModels_only_discharge/'
dir_rcp_recons <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Revision_Mar23/climate_rcp_simulations/ClassicalModels_only_discharge/'

dirout <- "/Users/noeliaotero/Documents/OCCR/data/HydropowerData/Historical/"
dir.create(dirout, showWarnings = FALSE)


# use ENTSO-E
load("/Users/noeliaotero/Documents/OCCR/data/Output_data/daily_generationunit_eic_entsoe.Rda")


# Historical Data
l_files <- list.files(dir_recons, pattern = "csv")
station_name <- tools::file_path_sans_ext(l_files)

r_data <- lapply(1:length(l_files), function(i) read.csv(paste(dir_recons, l_files[i], sep="")))
# fix name_p
s_columns <- c("date","generation","PREVAH",  "PREVAH7D",  "PREVAH15D", "PREVAH30D" ,"pred_rf_Predefinesplit","spi_3","STI_1")

r_data_fix <- list()
for(i in 1:length(r_data)){
  r_data_fix[[i]] <- r_data[[i]]
  r_data_fix[[i]]$name_p <- unique(r_data_fix[[i]]$name_p)[(unique(r_data_fix[[i]]$name_p)!="")]
  r_data_fix[[i]] <- r_data_fix[[i]]%>%dplyr::select(s_columns)
  # rename
  names(r_data_fix[[i]]) <- c("date","Generation", "Predictions", "PREVAH",  "PREVAH7D",  "PREVAH15D", "PREVAH30D" ,"spi_3", "STI_1")
}
names(r_data_fix) <- station_name
# make sure date is class as.Date
r_data_fix <- lapply(r_data_fix, function(x) {x$date <- as.Date(x$date) 
x})

####################
# Write the output
###################

for (i in 1:length(r_data_fix)) {
  write.csv(r_data_fix[[i]], file= paste(dirout,"/", names(r_data_fix[i]), ".csv",sep=""), row.names = FALSE)
}


##################################
# For the future projections
#################################
# load projections
rcps <- c("RCP26","RCP45","RCP85")
s_columns <- c("date","PREVAH",  "PREVAH7D",  "PREVAH15D", "PREVAH30D" ,"predictions","spi_3","STI_1")

data_rcp <- read_all_pw(dir_rcp_recons, station_name, rcps)
rcp_new_l <- list()

for (i in seq_along(data_rcp)) { # loop over Stations
  rcp_new_l[[i]] <- list() # initialize the nested list
  
  for (j in seq_along(data_rcp[[i]])) { # loop over RCP
    rcp_new_l[[i]][[j]] <- list() # initialize the nested list
    
    for (l in seq_along(data_rcp[[i]][[j]])) { # loop over models
      rcp_new_l[[i]][[j]][[l]] <- data_rcp[[i]][[j]][[l]][, s_columns, drop = FALSE]
      # subset and rename
      names(rcp_new_l[[i]][[j]][[l]]) <- c("date", "Predictions", "PREVAH", "PREVAH7D", "PREVAH15D", "PREVAH30D", "spi_3", "STI_1")
    }
    
    # set the names of the second-level nested list to match those of data_rcp
    names(rcp_new_l[[i]][[j]]) <- names(data_rcp[[i]][[j]])
  }
  
  # set the names of the first-level nested list to match those of data_rcp
  names(rcp_new_l[[i]]) <- names(data_rcp[[i]])
}

# set the names of the outer list to match those of data_rcp
names(rcp_new_l) <- names(data_rcp)

#####################
# Save the files
####################


dirout <- "/Users/noeliaotero/Documents/OCCR/data/HydropowerData/data_CH2018/"
dir.create(dirout, showWarnings = FALSE)

for (i in seq_along(rcp_new_l)) { # loop over Stations
  for (j in seq_along(rcp_new_l[[i]])) { # loop over RCP
    for (l in seq_along(rcp_new_l[[i]][[j]])) { # loop over models
      
      # create the file name based on the station, RCP, and model
      file_name <- paste0(names(rcp_new_l)[i], "_", names(rcp_new_l[[i]])[j], "_", names(rcp_new_l[[i]][[j]])[l], ".csv")
      cat("save",file_name,"\n")
      # set the full path to the file
      file_path <- file.path(dirout, file_name)
      
      # save the rcp_new_l to the file
      write.csv(rcp_new_l[[i]][[j]][[l]], file = file_path, row.names = FALSE)
    }
  }
}
