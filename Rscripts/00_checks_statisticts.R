# Check annual statistics model-BFE

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();



#---------HP generation ENTSOE-----------
load("../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2015_2021.Rda")

dff_enerp <- day_generation_2015_2021%>%dplyr::select(-c(lon, lat,AreaTypeCode,L1,name_p,capacity_p, type_g))%>%mutate(generation=ActualGenerationOutput/24)
names(dff_enerp)[names(dff_enerp)=="ProductionTypeName"] <- "type_g"

# Information Wasta
wasta_file <- "../../data/BFE_stats/9339-Statistik der Wasserkraftanlagen der Schweiz 1.1.2018/Statistik der Wasserkraftanlagen der Schweiz 1.1.2018.xlsx"
shp_file <- "/Users/noeliaotero/Documents/OCCR/data/9339-Statistik_2018/ch.bfe.statistik-wasserkraftanlagen /HydropowerPlant.shp"
inf_wasta <- process_WASTA(wasta_file, shp_file)
df_w <- data.frame(inf_wasta[[2]])

# find closest to JRC (ENTSOE)
jfile <- "../../data/JRC-PPDB-OPEN.ver0.91/JRC_OPEN_UNITS.csv"
hp_jrc <- get_info_HP_jrc(jfile)
info_merged_metadata <-  find_wastahp_JRC(inf_wasta[[1]], hp_jrc$meta_hp)
# remove Hydro Pumped Storage
info_hp <- info_merged_metadata%>%dplyr::filter(type_g!="Hydro Pumped Storage")
rheinfelden_num <- df_w[df_w$WASTANumbe==109400,]


load("../../data/BFE_stats/info_wasta_selected.Rda")
list_pw <- list("Kraftwerke Mauvoisin AG"=c("Fionnay", "Chanrion", "Champsec", "Riddes"), "AET Leventina"=c( "Tremorgio", "Nuova Biaschina", "Stalvedro (AET)", "Piottino"), 
                "Blenio (OFIBLE)"= info_wasta[info_wasta$name_p=="Blenio (OFIBLE)","WKA.Name"],"Emosson (ESA)"= info_wasta[info_wasta$name_p=="Emosson (ESA)","WKA.Name"] , 
                "Electra-Massa (EM)" =info_wasta[info_wasta$name_p=="Electra-Massa (EM)","WKA.Name"], "KW Rheinfelden CH"= c("Rheinfelden"), "Rheinau"= c("Rheinau"))



# OPen FOEN statistik
dir_f <- "../../data/BFE_stats/annual_stats/"
yy_bfu_data <-  lapply(1:length(list.files(dir_f)), function(i) get_stats_BFE(paste(dir_f, list.files(dir_f), sep="")[i], list_pw))
names(yy_bfu_data) <- seq("2013", "2021", by=1)
df_bfu <- reshape2::melt(yy_bfu_data, id=names(yy_bfu_data[[1]]))
names(df_bfu) <- c(names(yy_bfu_data[[1]]), "year")
# prepare the data to merge after
df_bfu <- df_bfu%>%pivot_longer(c(ONDJFM, AMJJAS, annual), names_to="extended_seas", values_to="value")
#dat_bfu[grepl("Rhein",dat_bfu$ZE.Name),]


dir_recons <- "/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Nov22/simulations_PREVAH/ClassicalModels/"
l_files <- list.files(dir_recons, pattern = "csv")
station_name <- tools::file_path_sans_ext(l_files)

r_data <- lapply(1:length(l_files), function(i) read.csv(paste(dir_recons, l_files[i], sep="")))
r_data <- lapply(r_data, function(x) { x$date <- as.Date(x$date)
          x})
names(r_data) <- station_name

r_data_sel <- lapply(r_data, function(x) x%>%dplyr::filter(format(date,"%Y")>"2012"))
df_all <- reshape2::melt(r_data, id=names(r_data[[1]]))
names(df_all) <- c(names(r_data[[1]]),"Station")

df_ss <- reshape2::melt(r_data_sel, id=names(r_data_sel[[1]]))
names(r_data_sel) <- station_name
names(df_ss) <- c(names(r_data_sel[[1]]),"Station")

df_ss <- create_extended_season(df_ss)
df_ss_stats <- df_ss%>%group_by(Station, year=format(date,"%Y"), extended_seas)%>%dplyr::summarise(true=sum(ActualGenerationOutput, na.rm=T)/1000, pred=sum(pred_randomforest*24)/1000 )
df_ss_ann <- df_ss%>%group_by(Station,year=format(date,"%Y"))%>%dplyr::summarise(true=sum(ActualGenerationOutput,na.rm=T)/1000, pred=sum(pred_randomforest*24)/1000 )

df_ss_ann$extended_seas <- "annual"
df_ss_all <- reshape2::melt(list(df_ss_stats, df_ss_ann))
df_ss_all$L1 <- NULL

#Monthly 
# df_ss%>%dplyr::filter(format(date,"%Y")>"2015")%>%
df_all%>%
  group_by(Station, month=format(date,"%m"))%>%
  dplyr::summarise(true=mean(ActualGenerationOutput, na.rm=T)/1000, pred=mean(pred_randomforest*24, na.rm=T)/1000)%>%
  pivot_longer(cols=c("true","pred"), names_to = "data", values_to = "production")%>%
  ggplot2::ggplot(aes(month,production, color=data, group=data)) + geom_line() + facet_wrap(~Station, ncol=3, scales = "free" ) + 
  scale_color_manual(values=c("blue","black"), labels=c("pred (1981-2021)", "true(2016-2021)")) + ylab("Production(GWh)") + theme_bw()


############################################
### Compare statistics from BFU
###########################################




df_with_entsoe <- reshape2::melt(list("WASTA"=df_bfu, "ENTSOE"=df_ss_all%>%dplyr::filter(variable=="true")))

names(df_with_entsoe) <- c("Station","year","season", "variable", "Generation", "Data")
df_with_entsoe$season <- as.factor(df_with_entsoe$season)
df_with_entsoe$season <- factor(df_with_entsoe$season, levels=c("ONDJFM", "AMJJAS", "annual"))
df_with_entsoe%>%dplyr::filter(year>"2015")%>%
  ggplot2::ggplot(aes(x=Station, y=Generation, fill=Data )) + geom_bar (stat="identity", position = "dodge")+ 
  facet_grid(~year ~season) +   
  scale_fill_brewer(palette = "Set2") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  ylab("Geneation(GWh)")




############ Extract characteristics from Barmer database
load("../../data/Output_data/df_metadata_balmer_wasta.Rda")

total_info <- df_balwasta[df_balwasta$ZE.Name%in%info_wasta$ZE.Name,]

