# Check annual statistics model-BFE

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();


dir_recons <- "/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Nov22/simulations_PREVAH/ClassicalModels/"
l_files <- list.files(dir_recons, pattern = "csv")
station_name <- tools::file_path_sans_ext(l_files)

r_data <- lapply(1:length(l_files), function(i) read.csv(paste(dir_recons, l_files[i], sep="")))
names(r_data) <- station_name

# Define list of selected Hydro PW
load("../../data/BFE_stats/info_wasta_selected.Rda")
list_pw <- list("Kraftwerke Mauvoisin AG"=c("Fionnay", "Chanrion", "Champsec", "Riddes"), "AET Leventina"=c( "Tremorgio", "Nuova Biaschina", "Stalvedro (AET)", "Piottino"), 
                "Blenio (OFIBLE)"= info_wasta[info_wasta$name_p=="Blenio (OFIBLE)","WKA.Name"],"Emosson (ESA)"= info_wasta[info_wasta$name_p=="Emosson (ESA)","WKA.Name"] , 
                "Electra-Massa (EM)" =info_wasta[info_wasta$name_p=="Electra-Massa (EM)","WKA.Name"], "KW Rheinfelden CH"= c("Rheinau"))


# OPen FOEN statistik
dir_f <- "../../data/BFE_stats/annual_stats/"
yy_bfu_data <-  lapply(1:length(list.files(dir_f)), function(i) get_stats_BFE(paste(dir_f, list.files(dir_f), sep="")[i], list_pw))
names(yy_bfu_data) <- seq("2013", "2021", by=1)
df_bfu <- reshape2::melt(yy_bfu_data, id=names(yy_bfu_data[[1]]))
names(df_bfu) <- c(names(yy_bfu_data[[1]]), "year")
# prepare the data to merge after
df_bfu <- df_bfu%>%pivot_longer(c(ONDJFM, AMJJAS, annual), names_to="extended_seas", values_to="value")

# prepare my-data
r_data <- lapply(r_data, function(x) {x$date <- as.Date(x$date) 
x})
r_data_sel <- lapply(r_data, function(x) x%>%dplyr::filter(format(date,"%Y")>"2012"))
df_ss <- reshape2::melt(r_data_sel, id=names(r_data_sel[[1]]))
names(df_ss) <- c(names(r_data[[1]]),"Station")
df_ss <- create_extended_season(df_ss)
df_ss_stats <- df_ss%>%group_by(Station, format(date,"%Y"), extended_seas)%>%dplyr::summarise(true=sum(ActualGenerationOutput)/1000, pred=sum(pred_randomforest*24)/1000 )
df_ss_ann <- df_ss%>%group_by(Station,format(date,"%Y"))%>%dplyr::summarise(true=sum(ActualGenerationOutput)/1000, pred=sum(pred_randomforest*24)/1000 )

df_ss_ann$extended_seas <- "annual"
df_ss_all <- reshape2::melt(list(df_ss_stats, df_ss_ann))
df_ss_all$L1 <- NULL

# NOt need#
#Rheinfelden_Rheinau <- df_ss_all%>%dplyr::filter(Station=="KW Rheinfelden CH")
#Rheinfelden_Rheinau$Station <- "Rheinfelden_Rheinau"

names(df_ss_all) <- c("Station", "year", "extended_seas", "variable", "value")
df_with_entsoe <- reshape2::melt(list("WASTA"=df_bfu, "ENTSOE"=df_ss_all%>%dplyr::filter(variable=="true")))

names(df_with_entsoe) <- c("Station","year","season", "variable", "Generation", "Data")

df_with_ML <- reshape2::melt(list("WASTA"=df_bfu, "ENTSOE"=df_ss_all%>%dplyr::filter(variable=="pred")))
names(df_with_ML) <- c("Station","year", "season", "variable", "Generation", "Data")


pp <- df_with_ML%>%ggplot2::ggplot(aes(x=year, y=Generation, fill=Data )) + geom_bar(stat="identity", position = "dodge") + 
  facet_grid(~Station ~season) +   
  scale_fill_brewer(palette = "Set2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Geneation(GWh)")
