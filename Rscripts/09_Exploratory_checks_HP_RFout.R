# Analysis of CH2018 and reference

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# Set -up the ouput
dir_recons <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Nov22/simulations_PREVAH/ClassicalModels/'
dir_rcp_recons <- '/Users/noeliaotero/Documents/OCCR/data/Output_data/csv/hydro_outputmodels_Nov22/simulations_PREVAH/climate_rcp_simulations/ClassicalModels//'
dirout <- "../../Results/Hydro_project/Analysis_Updates_Nov22/ClassicalModels_2predictors//"
dir.create(dirout, showWarnings = FALSE)


# use ENTSO-E
load("/Users/noeliaotero/Documents/OCCR/data/Output_data/daily_generationunit_eic_entsoe.Rda")

out <- load_MLOutput(dir_recons, dir_rcp_recons)

data_all <- out$RCP
data_all$Unnamed..0 <- NULL
data_all_1981_2021 <- out$RCP%>%dplyr::filter(format(date,"%Y")<"2022")

c_names <- names(data_all_1981_2021)
# Slect the same columns
cnames <- names(out$REF)[names(out$REF)%in%c_names]
df_entsose <- out$REF[,c(cnames, "ActualGenerationOutput")]
names(df_entsose) <- c(cnames, "generation") #note that these are TRUE!! but just for plotting
df_entsose$generation <- (df_entsose$generation)/(1000)

dff <- out$REF[,c(cnames, "pred_rf_Predefinesplit")]
names(dff) <- c(cnames, "generation")
dff$RCP <- "Historical"
dff$model <- "Historical"
dff$generation <- (dff$generation*24)/(1000)
# Add true:entsoe
df_entsose$RCP <- "Observed"
df_entsose$model <- "ENTSO-E" 


names(data_all)[names(data_all)=="predictions"] <- "generation"
names(data_all_1981_2021)[names(data_all_1981_2021)=="predictions"] <- "generation"
data_all$generation <- (data_all$generation*24)/(1000)
data_all_1981_2021$generation <- (data_all_1981_2021$generation*24)/(1000)

data_with_ref <- reshape2::melt(list("Observed" = df_entsose, "Historical" = dff, "rcps" = data_all[,names(dff)]), id=names(dff))
data_with_ref$L1 <- NULL

data_withref_1981_2021 <- reshape2::melt(list("Observed" = df_entsose, "Historical" = dff, "rcps" = data_all_1981_2021[,names(dff)]), id=names(dff))
data_withref_1981_2021$L1 <- NULL

# Plot hydropower 
# Apply some conversions to get GWh
data_withref_1981_2021 <- create_extended_season(data_withref_1981_2021)


# Annual production
annual_production <- data_withref_1981_2021%>%group_by(Station, year=format(date,"%Y"), RCP, model)%>%dplyr::summarise(production=sum(generation, na.rm=TRUE))
annual_production$extended_seas <- "annual"
seas_production <-   data_withref_1981_2021%>%group_by(Station, year=format(date,"%Y"), extended_seas, RCP, model)%>%dplyr::summarise(production=sum(generation, na.rm=TRUE))
df_summary_production <- reshape2::melt(list("annual"=annual_production, "seas"=seas_production), id=names(seas_production))
df_summary_production$L1 <- NULL
df_summary_production$year <- as.numeric(df_summary_production$year)

# Re-order and re-name
new_names <- c("Leventina", "Blenio", "Rheinfelden", "Electra-Massa", "Emosson", "Mauvoisin" )

df_summary_production$Station <- as.factor(df_summary_production$Station)
df_summary_production$Station <- factor(df_summary_production$Station , levels=c("AET Leventina","Blenio (OFIBLE)","KW Rheinfelden CH",
                                                                               "Electra-Massa (EM)", "Emosson (ESA)", "Kraftwerke Mauvoisin AG" ))
levels(df_summary_production$Station) <- new_names
  
pl_yr <- df_summary_production%>%dplyr::filter(RCP%in%c("Historical","Observed") & extended_seas == "annual")%>%ggplot2::ggplot(aes(year, production, fill=RCP)) + 
  geom_bar(stat = "identity",position="dodge") +
  facet_wrap(~Station, ncol=3, scales="free") +
  scale_fill_brewer(palette = "Set2", name="data", label =c("Predicted","Observed")) +
  ylab("Hydropower production [GWh]")+
  theme_bw()

ggsave(pl_yr, file=paste(dirout,"annual_means.png",sep=""),width = 14, height = 8)
# Seasons
pl_seas <- data_withref_1981_2021%>%group_by(Station, season, RCP, model)%>%
  dplyr::summarise(production=mean(generation))%>%dplyr::filter(RCP%in%c("Historical"))%>%
  ggplot2::ggplot(aes(season, production, fill=RCP)) + 
  geom_bar(stat = "identity",position="dodge") +
  facet_wrap(~Station, ncol=3, scales="free") +
  scale_fill_brewer(palette = "Set2", name="data") +
  ylab("Seasonal mean of modelled hydropower production [GWh] (1981-2021)")+
  theme_bw()
ggsave(pl_seas, file=paste(dirout,"seasonal_means.png",sep=""),width = 14, height = 8)

# For the same period!

pl_seas_2016 <- data_withref_1981_2021%>% dplyr::filter(format(date,"%Y")>"2015")%>%
  group_by(Station, season, RCP, model)%>%
  dplyr::summarise(production=mean(generation, na.rm=T))%>%dplyr::filter(RCP%in%c("Historical" , "Observed"))%>%
  ggplot2::ggplot(aes(season, production, fill=RCP)) + 
  geom_bar(stat = "identity",position="dodge") +
  facet_wrap(~Station, ncol=3) +
  scale_fill_brewer(palette = "Set2", name="data") +
  ylab("Seasonal mean of hydropower production [GWh] (2016-2021)")+
  theme_bw()
ggsave(pl_seas_2016, file="../../Results/Hydro_project/Analysis_Updates_June/Exploratory/seasonal_means_2016_2021.png",width = 14, height = 8)


# Plot
df_sum_prod_rcp <- df_summary_production%>%dplyr::filter(RCP!="Historical")
df_sum_His_rcp <- df_summary_production%>%dplyr::filter(RCP=="Historical")%>%group_by(Station, extended_seas)%>%dplyr::summarise(production=mean(production))
df_sum_His_rcp$extended_seas <- as.factor(df_sum_His_rcp$extended_seas)
df_sum_His_rcp$extended_seas <- factor(df_sum_His_rcp$extended_seas, levels=c("ONDJFM","AMJJAS","annual"))
levels(df_sum_His_rcp$extended_seas) <- c("winter","summer", "annual")
df_sum_prod_rcp$extended_seas <- as.factor(df_sum_prod_rcp$extended_seas)
df_sum_prod_rcp$extended_seas <- factor(df_sum_prod_rcp$extended_seas, levels=c("ONDJFM","AMJJAS","annual"))
levels(df_sum_prod_rcp$extended_seas) <- c("winter","summer", "annual")

p_stat <- df_sum_prod_rcp%>%ggplot2::ggplot(aes(y=production, x=RCP, fill = RCP )) + geom_boxplot() +
  facet_grid(~extended_seas~Station) +
  scale_fill_manual(values=rcp_colors) + 
  ylab("[GWh]")+
  theme_bw()
  
p_stat + geom_hline(data= df_sum_His_rcp, aes(yintercept=production), colour="black", linetype="dashed")

data_withref_1981_2021%>%group_by(Station, M= format(date, "%m"), RCP)%>%ggplot2::ggplot(aes(x=M, y=generation, fill= RCP)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") + 
  ylab("[GWh]")+
  facet_wrap(~Station, ncol=3)

# Plot reference

data_withref_1981_2021%>%dplyr::filter(RCP=="Historical")%>%group_by(Station, M= format(date, "%m"))%>%ggplot2::ggplot(aes(x=M, y=predictions, fill= RCP)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") + 
  ylab("[GWh]")+
  facet_wrap(~Station, ncol=3)

  
######################
# Predictors checks 
#####################
# Historical period

data_withref_1981_2021$Station <- as.factor(data_withref_1981_2021$Station)
data_withref_1981_2021$Station <- factor(data_withref_1981_2021$Station , levels=c("AET Leventina","Blenio (OFIBLE)","KW Rheinfelden CH",
                                                                                 "Electra-Massa (EM)", "Emosson (ESA)", "Kraftwerke Mauvoisin AG" ))
levels(data_withref_1981_2021$Station) <- new_names

hp <- data_withref_1981_2021%>%dplyr::filter(RCP=="Historical" & format(date, "%Y") > "2015")%>%group_by(Station, M= format(date, "%m"))%>%
  ggplot(aes(x=M, y=generation)) + geom_boxplot(outlier.size = 0, outlier.stroke = 0, fill="grey80", color="grey50") +  
  theme_classic() +   theme(axis.text = element_text(size =12), 
                            axis.title = element_text(size=12),
                            strip.text = element_text(size = 12)) + xlab("") + 
  ylab("[GWh]") + facet_wrap(~Station, scales="free") 

m <- data_withref_1981_2021%>%dplyr::filter(RCP=="Observed" & format(date, "%Y") > "2015")%>%
  dplyr::group_by(Station, month=format(date,"%m"), model)%>%
  dplyr::summarise(m = mean(generation, na.rm=T), ms= mean(scale(generation), na.rm=T), D= mean(PREVAH, na.rm=T), DS= mean(scale(PREVAH)))

hp <- hp + geom_line(data=m, aes(x=month, y=m, group=Station), color="black", linetype="dashed", size=1.)
#ggsave(hp, file=paste(dirout,"monthly_hp_production.png",sep=""),width = 14, height = 8)

hp <- hp + geom_line(data=m, aes(x=month, y=D, group=Station), color="blue", linetype = "dotdash", size =1)
ggsave(hp, file=paste(dirout,"monthly_hp_production_with_discharge.png",sep=""),width = 14, height = 8)

# Normalize everything 
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

pred_normalised <- data_withref_1981_2021%>%dplyr::filter(RCP%in%c("Historical", "Observed") & format(date, "%Y") > "2015")%>%
  dplyr::group_by(Station, month=format(date,"%m"))%>%
  dplyr::mutate(norm_generation = min_max_norm(generation),  norm_discharge = min_max_norm(PREVAH))
pred_sum_normalised <-  pred_normalised%>%dplyr::group_by(Station, RCP, month=format(date,"%m"))%>%
  dplyr::summarise(norm_generation = mean(norm_generation, na.rm=T), norm_discharge= mean(norm_discharge))



hp_normalised <-  pivot_wider(pred_sum_normalised, names_from = RCP, values_from = norm_generation)%>%ggplot2::ggplot() +
  geom_line(aes(x = month, y=Observed, group=Station, colour = "Hydropower production (ML)")) +
  geom_point(aes(x = month, y=Observed, group=Station, colour = "Hydropower production (ML)")) +
  geom_line(aes(x = month, y=Historical, group=Station), linetype = "dashed", color="black", size = 0.8) +
 # geom_point(aes(x = month, y=Historical, group=Station, colour = "Hydropower production (ENTSO-E)")) +
  geom_line( aes(x=month, y=norm_discharge, group=Station, colour = "Discharge")) +
  geom_point( aes(x=month, y=norm_discharge, group=Station, colour = "Discharge")) +
  facet_wrap(~Station, scales="free") + 
  scale_colour_manual(name = " ", values = c("Hydropower production (ML)" = "red", "Discharge" = "blue")) + 
  theme_classic() +   theme(axis.text = element_text(size =12), 
                            axis.title = element_text(size=12),
                            strip.text = element_text(size = 12)) + xlab("") + ylab("Normalized production, Normalized discharge")

ggsave(hp_normalised, file=paste(dirout,"monthly_hp_production_with_discharge_normalised.png",sep=""),width = 14, height = 8)

  t2m <- dff%>%group_by(Station, M= format(date, "%m"))%>%dplyr::summarize(T2m=mean(t2m))%>%
  ggplot(aes(x=M, y=T2m, color=Station, group=Station)) + geom_line(size=0.6) + scale_color_brewer(palette = "Set1", name="") +   
  theme_classic() +   theme(axis.text = element_text(size =12), axis.title = element_text(size=12)) + xlab("") + ylab("[ºC]")

prec <- dff%>%group_by(Station, M= format(date, "%m"))%>%dplyr::summarize(prec=mean(prec))%>%
  ggplot(aes(x=M, y=prec, color=Station, group=Station)) + geom_line(size=0.6) + scale_color_brewer(palette = "Set1", name="") +   
  theme_classic() +   theme(axis.text = element_text(size =12), axis.title = element_text(size=12)) + xlab("") + ylab("[mm/day]")
                                                                                                 

dis <- dff%>%group_by(Station, M= format(date, "%m"))%>%dplyr::summarize(PREVAH=mean(PREVAH))%>%
  ggplot(aes(x=M, y=PREVAH, color=Station, group=Station)) + geom_line(size=0.6) + scale_color_brewer(palette = "Set1", name="") +
  theme_classic() +   theme(axis.text = element_text(size =12), axis.title = element_text(size=12)) + xlab("") + ylab("[mm/day]") 


dis_fc <-  dff%>%group_by(Station, M= format(date, "%m"))%>%dplyr::summarize(PREVAH=mean(PREVAH))%>%
  ggplot(aes(x=M, y=PREVAH, group=Station)) + geom_line(size=0.6, color="black") +  
  facet_wrap(~Station, scales="free") +
  theme_classic() +   theme(axis.text = element_text(size =12), axis.title = element_text(size=12)) + xlab("") + ylab("[mm/day]") 

prec_fc <-  dff%>%group_by(Station, M= format(date, "%m"))%>%dplyr::summarize(prec=mean(prec))%>%
  ggplot(aes(x=M, y=prec, group=Station)) + geom_line(size=0.6, color="black") +  
  facet_wrap(~Station, scales="free") +
  theme_classic() +   theme(axis.text = element_text(size =12), axis.title = element_text(size=12)) + xlab("") + ylab("[mm/day]") 

ggsave(hp, file=paste(dirout,"HP_montly_training_period.png", sep=""), width = 10, height = 6)
ggsave(t2m, file=paste(dirout, "T2m_montly.png", sep=""), width = 10, height = 6)
ggsave(prec, file=paste(dirout, "prec_montly.png", sep=""), width = 10, height = 6)
ggsave(dis, file=paste(dirout, "dis_montly.png",sep=""), width = 10, height = 6)




data_withref_1981_2021%>%dplyr::filter(RCP=="Historical")%>%group_by(Station, M= format(date, "%m"))%>%dplyr::summarize(T2m=mean(t2m))%>%
  ggplot(aes(x=M, y=T2m, color=Station)) + geom_line() + scale_color_brewer(palette = "Set2") 


data_withref_1981_2021%>%group_by(Station, M= format(date, "%m"), RCP)%>%ggplot2::ggplot(aes(x=M, y=t2mmax, fill= RCP)) + geom_boxplot() +
  facet_wrap(~Station, ncol=3)

data_withref_1981_2021%>%group_by(Station, M= format(date, "%m"), RCP)%>%ggplot2::ggplot(aes(x=M, y=t2m, fill= RCP)) + geom_boxplot() +
  facet_wrap(~Station, ncol=3)

data_withref_1981_2021%>%group_by(Station, M= format(date, "%m"), RCP)%>%dplyr::summarise(mean=mean(prec, na.rm=T), sd = sd(prec, na.rm=T))%>%
ggplot2::ggplot(aes(x=M, y=mean, color= RCP, group=RCP)) + 
  geom_line() +
  # geom_ribbon(aes(y = mean, ymin = mean - sd, ymax = mean + sd, fill = RCP), alpha = .2) +
  facet_wrap(~Station, ncol=3)



data_withref_1981_2021%>%group_by(Station, M= format(date, "%m"), RCP)%>%dplyr::summarise(mean=mean(PREVAH, na.rm=T), sd=sd(PREVAH, na.rm=T))%>%
  ggplot2::ggplot(aes(x=M, y=mean, color= RCP, group=RCP)) + geom_line() +
  facet_wrap(~Station, ncol=3, scales="free")



# select the periods and 
fun_period_checks <- function(dff, data_all, TT, vplot, typeP="line", myleg){
  
  if (any(names(data_all) == "generation")){data_all <- data_all%>%dplyr::rename("production"="generation")}
  
  if (any(names(dff) == "generation")){dff <- dff%>%dplyr::rename("production"="generation")}
  
  data_with_ref <- reshape2::melt(list("Historical" = dff, "rcps" = data_all[,names(data_all)%in%names(dff)]), id=names(dff))
  # data_with_ref <- data_with_ref%>%dplyr::mutate(production= predictions * 24/(1000))
  data_with_ref$L1 <- NULL
 

  datT_rcp <- data_all[,names(dff)]%>%dplyr::filter(format(date,"%Y")>=TT[1] & format(date,"%Y")<=TT[2])
  # %>% dplyr::mutate(production= predictions * 24/(1000))
  
  tname <- paste(TT[1],TT[2],sep="-")
  all <- list("Ref"=dff, tname=datT_rcp)
  df_tot <- reshape2::melt(all, id=names(dff))
  names(df_tot) <- c(names(dff), "T")
  
  df_ref_mean <- dff%>%group_by(Station, M= format(date, "%m"), RCP)%>%
    # dplyr::mutate(production= predictions * 24/(1000))%>%
  dplyr::summarise_at(vplot, list(mean, sd))%>%
     dplyr::mutate(Date=as.Date(paste0("2000-", M, "-01","%Y-%m-%d")))
  names(df_ref_mean) <- c("Station", "M", "RCP", "mean", "SD", "Date")
  rr <- "RCP"; m <- "M"; ss <- "SD"; mn <- "mean"; dd <- "Date"; mod <- "model"
  int_var <- paste0("interaction(", paste0(c("RCP", "model"),collapse =  ", "), ")")
  # mcolors <-  c("#00AFBB", "#E7B800", "#FC4E07")
  
  if (typeP == "line"){
    # add all models
    p <- datT_rcp%>%group_by(Station, M= format(date, "%m"), RCP, model)%>%dplyr::summarise_at(vplot, mean)%>%
      dplyr::mutate(Date=as.Date(paste0("2000-", M, "-01","%Y-%m-%d")))%>%
      ggplot2::ggplot(aes_string(x=dd, y=vplot, color= rr, group=int_var)) + geom_line() +
      facet_wrap(~Station, ncol=3, scales="free")+
      xlab("") +
      ylab(myleg) + 
      scale_fill_manual(values=rcp_colors) +
      scale_color_manual(values=rcp_colors) +
      # scale_color_viridis(discrete = TRUE, option = "D")+
      # scale_fill_viridis(discrete = TRUE) +
      scale_x_date(labels = date_format("%b"))+
      theme_bw()
    p <- p+  stat_summary(data= df_ref_mean, fun=median, geom="line", aes(group=factor(RCP)), size=0.5, color="black", linetype="dashed") 
    p <- p + ggtitle(paste(TT[1], TT[2],sep="-"))
    
 }else if(typeP=="range"){
   
   xm <- datT_rcp%>%group_by(Station, M= format(date, "%m"), RCP, model)%>%dplyr::summarise_at(vplot,mean)%>%
     dplyr::mutate(Date=as.Date(paste0("2000-", M, "-01","%Y-%m-%d")))
   xx <- data.frame(xm)%>%group_by(Date, Station, M, RCP)%>%dplyr::summarise_at(vplot, list(mean, max, min))
   names(xx) <- c("Date","Station", "M", "RCP", "mean", "max", "min")
   p <- ggplot2::ggplot(xx, aes(x=Date, y=mean, color= RCP, group=RCP)) + geom_line() +
     facet_wrap(~Station, ncol=3, scales="free")+
     geom_ribbon(aes(ymax=max, ymin=min, fill= RCP), color="grey90",alpha=0.5) +
     xlab("") +
     ylab(myleg) + 
     scale_fill_manual(values=rcp_colors) +
     scale_color_manual(values=rcp_colors) +
     scale_x_date(labels = date_format("%b"))+
     theme_classic() +   theme(axis.text.x = element_text(size =12),
                               axis.text.y = element_text(size =12),
                               strip.text  = element_text(size=12),
                               axis.title.y = element_text(size =12),
                               axis.title.x = element_text(size =12), 
                               legend.key.width = unit(0.6, "cm"),
                               legend.key.height = unit(0.6, "cm"))
   
   p <- p+  geom_line(data= df_ref_mean, aes(x=Date, y=mean, group=RCP), color="black", linetype="dashed")
   p <- p + ggtitle(paste(TT[1], TT[2],sep="-"))
   
   
   }else{
   # Add shaded
   x <- datT_rcp%>%group_by(Station, M= format(date, "%m"), RCP)%>%dplyr::summarise_at(vplot, list(mean, sd))%>%
     dplyr::mutate(Date=as.Date(paste0("2000-", M, "-01","%Y-%m-%d")))
    names(x) <- c("Station", "M", "RCP", "mean", "SD", "Date")
    p <- ggplot2::ggplot(x, aes(x=Date, y=mean, color= RCP, group=RCP)) + geom_line() +
      facet_wrap(~Station, ncol=3, scales="free")+
      geom_ribbon(aes(ymax=mean+SD, ymin=mean-SD, fill= RCP), color="grey95",alpha=0.2) +
      xlab("") +
      ylab(myleg) + 
      scale_fill_manual(values=rcp_colors) +
      scale_color_manual(values=rcp_colors) +
      # scale_color_viridis(discrete = TRUE, option = "D")+
      # scale_fill_viridis(discrete = TRUE) +
      scale_x_date(labels = date_format("%b"))+
      theme_classic() +   theme(axis.text.x = element_text(size =12),
                           axis.text.y = element_text(size =12),
                           strip.text  = element_text(size=12),
                           axis.title.y = element_text(size =12),
                           axis.title.x = element_text(size =12), 
                           legend.key.width = unit(0.8, "cm"),
                           legend.key.height = unit(2, "cm"))
    
    p <- p+  geom_line(data= df_ref_mean, aes(x=Date, y=mean, group=RCP), color="black", linetype="dashed")
    p <- p + ggtitle(paste(TT[1], TT[2],sep="-"))
  }
  
  ggsave(p, file=paste(dirout, vplot, "_", typeP,TT[1],"_",TT[2], ".png",sep=""), width = 12, height = 7)
  return(p)
}


# select the periods and 
fun_plot_yy_periods <- function(dff, data_all, TT, vplot, typeP="line", myleg){
  
  data_all <- data_all%>%dplyr::mutate(generation= predictions * 24/(1000))

  # d_yy <- data_all%>%group_by(Station, RCP, season, year=format(date, "%Y"))%>%dplyr::summarise_at(vplot, list(mean, sd))%>%
  #   dplyr::filter(year>="2020")%>%dplyr::mutate(Date=as.Date(paste0(year,"-01-01","%Y-%m-%d")))
  # names(d_yy) <- c("Station", "RCP","season", "year", "mean", "SD","Date")
  
  xm <- data_all%>%group_by(Station, year=format(date, "%Y"), RCP, model)%>%dplyr::summarise_at(vplot,mean)%>%
    dplyr::filter(year>="2020")%>%dplyr::mutate(Date=as.Date(paste0(year,"-01-01","%Y-%m-%d")))
  xx <- data.frame(xm)%>%group_by(Date, Station, year, RCP)%>%dplyr::summarise_at(vplot, list(mean, max, min))
  names(xx) <- c("Date","Station", "year", "RCP", "mean", "max", "min")
  
 p <-  ggplot2::ggplot(xx, aes(x=Date, y=mean, color= RCP, group=RCP)) + geom_line() +
   facet_wrap(~Station, ncol=3, scales="free")+
   geom_ribbon(aes(ymax=max, ymin=min, fill= RCP), color="grey90",alpha=0.5) +
   xlab("") +
   # ylab(myleg) + 
   # scale_fill_brewer(palette = "Set2") + 
   scale_fill_manual(values=rcp_colors) +
   scale_color_manual(values=rcp_colors) +
   scale_x_date(labels = date_format("%Y"))+
   theme_bw()
  
  return(p)
 
}


# Re-order station names
dff$Station <- as.factor(dff$Station)
dff$Station <- factor(dff$Station , levels=c("AET Leventina","Blenio (OFIBLE)","KW Rheinfelden CH",
                                                                                   "Electra-Massa (EM)", "Emosson (ESA)", "Kraftwerke Mauvoisin AG" ))
levels(dff$Station) <- new_names

data_all$Station <- as.factor(data_all$Station)
data_all$Station <- factor(data_all$Station , levels=c("AET Leventina","Blenio (OFIBLE)","KW Rheinfelden CH",
                                             "Electra-Massa (EM)", "Emosson (ESA)", "Kraftwerke Mauvoisin AG" ))
levels(data_all$Station) <- new_names

  
p_dis_t1 <- fun_period_checks(dff,data_all, T1, "PREVAH", "range", "[mm/day]")
p_dis_t2 <- fun_period_checks(dff,data_all, T2, "PREVAH", "range", "[mm/day]")
p_dis_t3 <- fun_period_checks(dff,data_all, T3, "PREVAH", "range", "[mm/day]")

# Production
p_hp_t1 <- fun_period_checks(dff,data_all, T1, "production", "range", "[GWh]")
p_hp_t2 <- fun_period_checks(dff,data_all, T2, "production", "range", "[GWh]")
p_hp_t3 <- fun_period_checks(dff,data_all, T3, "production", "range", "[GWh]")

# temperature
p_tmax_t1 <- fun_period_checks(dff,data_all, T1, "t2mmax", "range", "[ºC]")
p_tmax_t2 <- fun_period_checks(dff,data_all, T2, "t2mmax", "range", "[ºC]")
p_tmax_t3 <- fun_period_checks(dff,data_all, T3, "t2mmax", "range", "[ºC]")


# precipitation
p_prec_t1 <- fun_period_checks(dff,data_all, T1, "prec", "range", "[mm]")
p_prec_t2 <- fun_period_checks(dff,data_all, T2, "prec", "range", "[mm]")
p_prec_t3 <- fun_period_checks(dff,data_all, T3, "prec", "range", "[mm]")



## -----------Explore correlations---------------------
# 1. monthly scale

examine_cor <- function(dd){
  
  X <- dd%>%group_by(Station, season,  extended_seas,RCP, date=format(date,"%Y-%m"))%>%dplyr::summarise(predictions=mean(predictions, na.rm=T), spei_1=mean(spei_1, na.rm=T), spei_3=mean(spei_3, na.rm=T), 
                                                                    SSI1=mean(SSI1, na.rm=T), STI_1=mean(STI_1, na.rm=T))
  X%>%group_by(Station, season, RCP)%>%
    dplyr::summarise(cor_hp_spei3 = cor(predictions, spei_3), cor_hp_ssi = cor(predictions, SSI1),cor_hp_sti = cor(predictions, STI_1),cor_hp_sti = cor(predictions, STI_1))%>%
    pivot_longer(cols=c(cor_hp_spei3, cor_hp_ssi,cor_hp_sti), names_to="variable", values_to="value")%>%
    ggplot(aes(season, value, fill=variable)) + 
    geom_bar(stat="identity", position="dodge") + facet_grid(~RCP~Station)   
  
  
}


# Plot total HP -average periods

