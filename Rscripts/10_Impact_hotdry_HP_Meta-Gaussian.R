# Impact hot-dry-HP 
# Copula approach
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# --------Observations-----------
#load data
# dir_recons <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/outmodels/observed_discharge/Classical/meteo_HPcatch/daily_complete/'
#dir_recons <- "/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Nov22/simulations_PREVAH/ClassicalModels/"
dir_recons <- "/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Revision_Mar23/ClassicalModels_only_discharge/"

# dir_month_recons <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/outmodels/Classical/meteo_HPcatch/monthly/'

# ---------Using PreVAH-----------
# dir_recons <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/outmodels/simulations_PREVAH//Classical/'

l_files <- list.files(dir_recons, pattern = "csv")
station_name <- tools::file_path_sans_ext(l_files)

r_data <- lapply(1:length(l_files), function(i) read.csv(paste(dir_recons, l_files[i], sep="")))
names(r_data) <- station_name

r_data <- lapply(r_data, function(x) x%>%dplyr::select(c("date","season","eic_g","name_p","generation","pred_rf_Predefinesplit","PREVAH","t2mmax","prec","spi_3","spei_3","STI_1")))
dff    <- reshape2::melt(r_data, id=names(r_data[[1]]))
names(dff) <- c(names(r_data[[1]]),"Station")
dff$date <- as.Date(dff$date)
dff <- create_extended_season(dff)

# rename predictions
names(dff)[which(names(dff)=="pred_rf_Predefinesplit")] <- "predictions"

# s_vars <- c("predictions","discharge","t2mmax","prec","spei_1","spei_3","spei_6","STI_1","SSI1")
s_vars <- c("predictions","PREVAH","t2mmax","prec","spi_3","spei_3","STI_1")
# Get monthly data
dff_mon <- dff%>%group_by(date=format(date,"%Y-%m"), season, extended_seas, Station)%>%summarise_at(vars(s_vars), mean, na.rm=T)


#############################################
#-----------Meta-Gaussian model-------------
############################################
# adapted from Hao et al.
# Inputs---Montly scale
# specify input variables
# NQT transformation
# plot PDF 
l_cases <- c("Dryhot", "DryCold")
#i_vars <- c("pred_randomforest","SPI3","STI")
# i_vars <- c("predictions","SPI3","STI_1")
# nvars <- c("SPI3","STI", "Prob")

# test SPEI
i_vars <- c("predictions","spi_3","STI_1")
nvars <- c("SPI3","STI", "Prob")

cat ("Run for", i_vars)

# Daily basis (prec30, tmax)
# split into extended seasons
l_mon <- split(dff_mon, f=dff_mon$Station)
r_data_summer <- lapply(l_mon, function(x) x%>%dplyr::filter(extended_seas=="AMJJAS"))
# r_data_winter <- lapply(l_mon, function(x) x%>%dplyr::filter(extended_seas=="ONDJFM"))

#dout_s <- "../../Results/Hydro_project/Analysis_Updates_Nov22/ClassicalModels_2predictors/MetaGaussian/Discharge/AMJJAS/mon_SPI3_STI/"
#############UPDATES
dout_s <- "../../Results/Hydro_project/hydro_outputmodels_Revision_Mar23/ClassicalModels_only_discharge/MetaGaussian/Discharge/AMJJAS/mon_spi_3_STI//"

dir.create(dout_s, recursive = T, showWarnings = F)
# dout_s <- "../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/Observed/AMJJAS/mon_SPI3_STI/"
# dout_w <- "../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/Observed/ONDJFM/mon_SPI3_STI/"

# ------ for summer DryHot ----------
run_MG_cases(r_data_summer, "DryHot", i_vars, nvars, dout_s)
# run_MG_cases(r_data_winter, "DryCold", i_vars, nvars, dout_w)

# not for now
# ---------Repeat the analysis using SSI
# i_vars <- c("predictions","SSI1","STI_1")
# # i_vars <- c("pred","spei_3","STI_1")
# nvars <- c("SSI","STI", "Prob")
# dout <- "../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/PREVAH/AMJJAS/mon_SSI_STI/"
# run_MG_cases(r_data_summer, "DryHot", i_vars, nvars, dout)




#####----------------------Need to be revised----------------
##########################################
# Individual impacts
##########################################

run_MG2D <- function(r_mon_data, ind_cases, nvar, dout){
  #Main function that runs MG-model and plots the results
  #' @r_mon_data: list of data
  #' @ind_cases: type of case
  #' @nvar: name of variables for plotting: SPEI/STI or SSI/STI (what we show)
  #' @dout: path for the outputs
  
  # check and create path
  dir.create(dout, showWarnings = FALSE)
  
  out <- list()
  for(i in 1:length(r_mon_data)){
    cat(i,"\n")
    out[[i]] <-  main_metaG_2D(r_mon_data[[i]], i_vars, ind_cases)
  }
  names(out) <- names(r_mon_data)
  
  # Extract data: mat1(and) and mat2(or)
  mat_high <- lapply(out, function(x) x$mat_cases$high)
  mat_low <- lapply(out, function(x) x$mat_cases$low)
  
  dff_high <- reshape2::melt(mat_high, id=names(mat_high[[1]]))
  dff_low <- reshape2::melt(mat_low, id=names(mat_low[[1]]))
  names(dff_high) <- names(dff_low) <- c("case","prob_and", "prob_cond", "station")
  dff_high$case <- as.numeric(dff_high$case)
  dff_low$case <- as.numeric(dff_low$case)
  dff_low[,c("prob_and","prob_cond")] <- sapply(dff_low[,c("prob_and","prob_cond")],as.numeric)
  dff_high[,c("prob_and","prob_cond")] <- sapply(dff_high[,c("prob_and","prob_cond")],as.numeric)
  ## Extract the case and plot
  dff_all <- list("high"=dff_high, "low" = dff_low)
  dff_all <- reshape2::melt(dff_all, id=names(dff_all[[1]]))
  names(dff_all) <- c(names(dff_all[[1]]), "Type")
  p_h <- ggplot2::ggplot(dff_high, aes(x=case, y=prob_cond, fill=station)) + geom_bar(stat="identity", position="dodge") + 
    scale_fill_brewer(palette = "Dark2") +  scale_x_continuous(breaks=dff_high$case, labels=dff_high$case)  +
     theme_bw()
  p_l <- ggplot2::ggplot(dff_low, aes(x=case, y=prob_cond, fill=station)) + geom_bar(stat="identity", position="dodge") + 
    scale_fill_brewer(palette = "Dark2") +  scale_x_continuous(breaks=dff_low$case, labels=dff_low$case)  +
     theme_bw()
  plot_grid(p_l, p_h, labels=c(paste(nvar,"<", sep=""),paste(nvar,">", sep="")))
  # plot PDF
  # extract params
  l_params <- lapply(out, function(x) x$l_params)
  df_params <- setNames(reshape2::melt(l_params, id=c("mu","sig")), c("mu","sig","compound","type","station"))
  
  l_pdfs <- lapply(out, function(x) x$l_pdf)
  l_dff <- lapply(l_pdfs, function(x) setNames(reshape2::melt(x, id=names(x[[1]][[1]])), c(names(x[[1]][[1]]),"compound","type")))
  all_dff <- reshape2::melt(l_dff, id=names(l_dff[[1]]))
  names(all_dff) <- c(names(l_dff[[1]]), "station")
  all_dff <- merge(all_dff, df_params, by=c("compound","station","type"))
  
  types <- unique(all_dff$compound)
  # example
  # HotDry
  l_types <- list("low"=c("-1.9","-1.6","-1.3","-0.8"), "high" =c("1.9","1.6","1.3","0.8"))
  
  
  for (i in 1:length(l_types)){
    
    p <- all_dff%>%dplyr::filter(compound%in%l_types[[i]])%>%
      ggplot() + geom_density(aes(C_PDF, color=compound, fill=compound), alpha=0.1)+ geom_density(aes(PDF), linetype="dashed", size=0.3)  + 
      geom_vline(aes(xintercept=mu, color=compound), linetype="dashed", size=0.3) + 
      facet_wrap(~station, ncol=3) + 
      scale_fill_brewer(palette = "Dark2", name="")+
      scale_color_brewer(palette = "Dark2", name="")+
      theme_bw()
    
    ggsave(p, file=paste(dout, names(l_types[i]), "_PDF_cond.png", sep=""), width = 12, height = 8)
    
  }
  
}


ind_cases <- c("high", "low")
dout <- "../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/SPEI3/"
run_MG2D(r_mon_data, ind_cases, "spei_3", dout)
ind_cases <- c("high", "low")
dout <- "../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/STI1/"
run_MG2D(r_mon_data, ind_cases, "STI_1", dout)
