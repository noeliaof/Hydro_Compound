# Probabilistic modeling#

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# Set -up the ouput


out <- load_MLOutput(dir_recons, dir_rcp_recons)
data_all <- out$RCP
c_names <- names(data_all)
data_all <- create_extended_season(data_all)
# Slect the same columns
cnames <- names(out$REF)[names(out$REF)%in%c_names]
dff <- out$REF[,c(cnames, "pred_rf_Predefinesplit")]
names(dff) <- c(cnames, "predictions")
dff <- create_extended_season(dff)
dff$RCP   <- "Historical"
dff$model <- "Historical"

# apply the probabilistic model to each RCP, model 
prepare_input <- function(dd, TT){

  s_vars <- c("predictions","PREVAH","t2mmax","prec","spei_1","spei_3","spei_6","STI_1")
  
  
  #filter the years of the selected period
  dd_yy <- dd%>%dplyr::filter(format(date,"%Y")>=TT[1] & format(date,"%Y")<=TT[2])

  # Get monthly data
  dd_mon <- dd_yy%>%group_by(Station, model, RCP, season,extended_seas, date=format(date, "%Y-%m"))%>%
    dplyr::summarise_at(vars(s_vars), mean, na.rm=T)
  
  # split the data into model and station
  l_stat <- split(dd_mon, f=dd_mon$Station)
  l_rcp_stat <- lapply(l_stat ,function(x) split(x, f=x$RCP))
  l_rcp_stat_mon <- lapply(l_rcp_stat, function(x) lapply(x, function(y) split(y, f=y$model)))
  
  # reverse the order of the list
  
  X <- rev_list(l_rcp_stat_mon)
  XX <- lapply(X, function(x) { x <- rev_list(x)
  x})
  
  return(XX)
  
}

# Historical period
l_historical <- prepare_input(dff, T1)
l_rcp_t1 <- prepare_input(data_all, T1)
l_rcp_t2 <- prepare_input(data_all, T2)
l_rcp_t3 <- prepare_input(data_all, T3)



run_all <- function(l_rcp_tt, TT, Fdir, i_vars, nvars, cases, SS){
  
  mdir <- paste0(Fdir,paste("RCP","_",TT[1],"-", TT[2], "/",sep=""))
  out_all <- out_mod <- list()
  for (i in 1:length(l_rcp_tt)){ # loop for rcp
    
    for(j in 1:length(l_rcp_tt[[i]])){ #loop for model
  
       if (SS %in%c("AMJJAS", "ONDJFM")){
         r_data_seas <- lapply(l_rcp_tt[[i]][[j]], function(x) data.frame(x%>%dplyr::filter(extended_seas==SS)))
       }else{
         r_data_seas <- lapply(l_rcp_tt[[i]][[j]], function(x) data.frame(x%>%dplyr::filter(season==SS)))
       }
       rcp_nam <- unique(r_data_seas[[1]]$RCP)
       mod_nam <- unique(r_data_seas[[1]]$model)
       dout <- paste0(mdir, rcp_nam, "_", mod_nam,"/")
       dir.create(dout, showWarnings = FALSE)
       out_mod[[j]] <- run_MG_cases(r_data_seas, "DryHot", i_vars, nvars, dout)
  
    }
      names(out_mod) <- names(l_rcp_tt[[i]])
      out_all[[i]] <- out_mod
    
  }
  
  names(out_all) <- names(l_rcp_tt)
  
  return(out_all)
}



# Extra process ---
plot_pdfs_rcp <- function(out, out_hist, name_com, TT){
  
  # extract the data
  l_pdfs  <- lapply(out, function(x) lapply(x, function(y) y$df_pdfs))
  l_hist <- lapply(out_hist, function(x) lapply(x, function(y) y$df_pdfs))
  df_pdfs <- reshape2::melt(l_pdfs, id=names(l_pdfs[[1]][[1]]))
  df_hist <- reshape2::melt(l_hist, id=names(l_hist[[1]][[1]]))
  names(df_pdfs) <- names(df_hist) <-  c(names(l_pdfs[[1]][[1]]), "model", "RCP")
  
  # compute means
  #df_rcp_mm <- df_pdfs%>%group_by( station,compound, type, RCP)%>%dplyr::summarise(m_mu=mean(mu), m_sig=mean(sig), m_PDF=mean(PDF), m_condPDF=mean(C_PDF))
  #df_hist_mm <- df_hist%>%group_by( station,compound, type, RCP)%>%dplyr::summarise(m_mu=mean(mu), m_sig=mean(sig), m_PDF=mean(PDF), m_condPDF=mean(C_PDF))
  
  df_all <- reshape2::melt(list("RCP"=df_pdfs, "Hist"=df_hist), id=names(df_pdfs))
  names(df_all) <- c(names(df_pdfs), "data")
  
  
  l_types <- c("-1.9/1.9","-1.6/1.6","-1.3/1.3","-0.8/0.8")
  new_legend <- c("Mild", "Moderate", "Severe", "Extreme")
  
  # df_all$station[df_all$station =="Kraftwerke Mauvoisin AG"] <- "Mauvoisin AG"
  # df_all$station[df_all$station =="KW Rheinfelden CH"] <- "Rheineau"
  
  p <-  df_all%>%dplyr::filter(compound%in%l_types)%>%
   # group_by(station, compound, RCP)%>%
   # dplyr::summarise(C_PDF = mean(C_PDF, na.rm=TRUE), PDF = mean(PDF, na.rm=TRUE), mu = mean(mu), sig = mean(sig))%>%
    ggplot2::ggplot(aes(C_PDF, y=RCP, color=compound, fill=compound))+
    geom_density_ridges(alpha=0.15)  + facet_wrap( ~station, ncol=3) + 
    scale_fill_manual(values = c( "#009E73", "#F0E442", "#F46D43","#9E0142"), name=name_com, labels = new_legend) +
    scale_color_manual(values = c("#009E73", "#F0E442", "#F46D43", "#9E0142"),  name=name_com, labels = new_legend) + 
    ylab("") +
    xlab("SHPI") +
    ggtitle(TT) + 
    theme(strip.text = element_text(size=12), axis.text = element_text(size=10)) + 
    theme_bw()
  
  return(p)
}


### compare distributions #####

compare_distributions <- function(out, out_hist){
  
  l_types <- c("-1.9/1.9","-1.6/1.6","-1.3/1.3","-0.8/0.8")
  func <- function(x){
    xx <- merge(x[[1]], x[[2]], by = c("station", "compound"))
    xx <- xx[,c("compound","station","C_PDF.x","C_PDF.y")]
    xx$Means<- xx%>%dplyr::select_if(is.numeric)%>%rowMeans()
    return(xx[,c("compound","station","Means")])
    }
  
  # extract the data
  l_pdfs  <- lapply(out, function(x) lapply(x, function(y) y$df_pdfs))
  l_pdfs <- lapply(l_pdfs, function(x) lapply(x, function(y) y%>%dplyr::filter(compound%in%l_types)))
  rcp_pdfs_means <- lapply(l_pdfs, func)
  
  l_hist <- lapply(out_hist, function(x) lapply(x, function(y) y$df_pdfs))
  l_hist <- lapply(l_hist, function(x) lapply(x, function(y) y%>%dplyr::filter(compound%in%l_types)))
  
  dfhist <- l_hist$Historical$Historical[,c("compound","station","C_PDF")]
  names(dfhist) <- c("compound","station", "historical")
  # change the names
  names(rcp_pdfs_means$RCP26) <- c("compound","station","RCP26")
  names(rcp_pdfs_means$RCP45) <- c("compound","station","RCP45")
  names(rcp_pdfs_means$RCP85) <- c("compound","station","RCP85")
  dfs <- cbind(rcp_pdfs_means$RCP26,"RCP45" = rcp_pdfs_means$RCP45$RCP45, "RCP85" = rcp_pdfs_means$RCP85$RCP85, "historical" = dfhist$historical)
  
  pvals <- dfs%>%group_by(compound, station)%>%dplyr::summarise(p_value1 = t.test(RCP26, RCP45)$p.value,
                                                             p_value26h = t.test(RCP26, historical)$p.value, 
                                                             p_value2 = t.test(RCP26, RCP85)$p.value, 
                                                             p_value45h = t.test(RCP45, historical)$p.value, 
                                                             p_value3 = t.test(RCP45,RCP85)$p.value,
                                                             p_value85h = t.test(RCP85, historical)$p.value)
  
  return(pvals)

  
}
  
# run the model for each case
l_cases <- c("DryHot", "DryCold")
#i_vars <- c("pred_randomforest","SPI3","STI")
i_vars <- c("predictions","spei_3","STI_1")
nvars <- c("SPI3","STI", "Prob")

Fdir <- "../../Results/Hydro_project/Analysis_Updates_Nov22/ClassicalModels_2predictors/MetaGaussian/RCP/AMJJAS/SPI3_STI1/"
SS <- "AMJJAS"
out_hist <- run_all(l_historical, T1, Fdir, i_vars, nvars, "DryHot", SS) 
out_t1 <- run_all(l_rcp_t1, T1, Fdir, i_vars, nvars, "DryHot", SS) 
out_t2 <- run_all(l_rcp_t2, T2, Fdir, i_vars, nvars, "DryHot", SS)
out_t3 <- run_all(l_rcp_t3, T3, Fdir, i_vars, nvars, "DryHot", SS)

p1_spei <- plot_pdfs_rcp(out_t1, out_hist, "SPI3/STI", paste(T1[1], T1[2],sep="-"))
ggsave(p1_spei, file=paste0(Fdir, "PDFS_1981-2021.png", sep=""), width = 10, height = 6)
p2_spei <- plot_pdfs_rcp(out_t2, out_hist, "SPI3/STI", paste(T2[1], T2[2],sep="-"))
ggsave(p2_spei, file=paste0(Fdir, "PDFS_2031-2070.png", sep=""), width = 10, height = 6)
p3_spei <- plot_pdfs_rcp(out_t3, out_hist, "SPI3/STI", paste(T3[1], T3[2],sep="-"))
ggsave(p3_spei, file=paste0(Fdir, "PDFS_2059-2099.png", sep=""), width = 10, height = 6)




# Using SSI
Fdir <- "../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/RCP/AMJJAS/SSI1_STI1/"
dir.create(Fdir)
SS <- "AMJJAS"
i_vars <- c("predictions","SSI1","STI_1")
nvars <- c("SSI","STI", "Prob")
out_ssi_hist <- run_all(l_historical, T1, Fdir, i_vars, nvars, "DryHot",SS) 
out_ssi_t1 <- run_all(l_rcp_t1, T1, Fdir, i_vars, nvars, "DryHot",SS) 
out_ssi_t2 <- run_all(l_rcp_t2, T2, Fdir, i_vars, nvars, "DryHot",SS)
out_ssi_t3 <- run_all(l_rcp_t3, T3, Fdir, i_vars, nvars, "DryHot",SS)

p1_ssi <- plot_pdfs_rcp(out_ssi_t1, out_ssi_hist, "SSI1/STI", paste(T1[1], T1[2],sep="-"))
ggsave(p1_ssi, file=paste0(Fdir, "PDFS_1981-2021.png", sep=""), width = 10, height = 6)
p2_ssi <- plot_pdfs_rcp(out_ssi_t2, out_ssi_hist, "SSI1/STI", paste(T2[1], T2[2],sep="-"))
ggsave(p2_ssi, file=paste0(Fdir, "PDFS_2031-2070.png", sep=""), width = 10, height = 6)
p3_ssi <- plot_pdfs_rcp(out_ssi_t3, out_ssi_hist, "SSI1/STI", paste(T3[1], T3[2],sep="-"))
ggsave(p3_ssi, file=paste0(Fdir, "PDFS_2059-2099.png", sep=""), width = 10, height = 6)




#######################################################
# Plot probabilities
process_probs <- function(out, outH, nvars){
  
  mat <- lapply(out, function(x) lapply(x, function(y) y$mat_or))
  matH <- lapply(outH, function(x) lapply(x, function(y) y$mat_or))
  # melt
  df_mat <- reshape2::melt(mat, id=names(mat[[1]][[1]]))
  dfH_mat <- reshape2::melt(matH, id=names(matH[[1]][[1]]))
  
  names(df_mat) <- names(dfH_mat) <- c(nvars[1], nvars[2], "Prob","Station", "model", "RCP")
  df_all <- reshape2::melt(list("HIST"=dfH_mat, "RCP"=df_mat), id=names(df_mat))
  df_all$L1 <- NULL
  
  df_mat$Station[df_mat$Station =="Kraftwerke Mauvoisin AG"] <- "Mauvoisin AG"
  df_mat$Station[df_mat$Station =="KW Rheinfelden CH"] <- "Rheinfelden"
  # Select compounds
  v1 <- c("-1.9/1.9", "-1.6/1.6", "-0.8/0.8")
  df_mat$compound <- paste(df_mat[,nvars[1]], df_mat[,nvars[2]], sep="/")
  p <- df_mat%>%dplyr::filter(compound%in%v1)%>%ggplot2::ggplot(aes(x=RCP, y=Prob, fill=RCP)) + geom_boxplot(alpha=0.8) + 
    facet_grid( ~compound~Station) + scale_fill_manual(values = rcp_colors) + ylab("") + xlab("") +
    theme_bw() + theme(strip.text = element_text(size=12), axis.text = element_text(size=10))
  
  # add historical 
  dfH_mat$Station[dfH_mat$Station =="Kraftwerke Mauvoisin AG"] <- "Mauvoisin AG"
  dfH_mat$Station[dfH_mat$Station =="KW Rheinfelden CH"] <- "Rheinfelden"
  dfH_mat$compound <- paste(dfH_mat[,nvars[1]], dfH_mat[,nvars[2]], sep="/")
  dfcom_his <- dfH_mat%>%dplyr::filter(compound%in%v1)
  p <- p + geom_hline(data= dfcom_his, aes(yintercept=Prob), colour="black", linetype="dashed")
  
  return(p)
}


p_T2 <- process_probs(out_t2, out_hist, nvars=c("SPI3", "STI"))
p_T2 <- p_T2 + ggtitle(paste(T2[1], T2[2], sep="-"))

p_T3 <- process_probs(out_t3, out_hist, nvars=c("SPI3", "STI"))
p_T3 <- p_T3 + ggtitle(paste(T3[1], T3[2], sep="-"))

ggsave(p_T2, file="../../Results/Hydro_project/Analysis_Updates_June/MetaGaussian/RCP/AMJJAS/SPI3_STI1/Probs_boxplot_T2.png", width = 12, height = 6)
ggsave(p_T3, file="../../Results/Hydro_project/Analysis_Updates_June/MetaGaussian/RCP/AMJJAS/SPI3_STI1/Probs_boxplot_T3.png", width = 12, height = 6)


p_ssi_T2 <- process_probs(out_ssi_t2, out_hist, nvars=c("SSI", "STI"))
p_ssi_T2 <- p_ssi_T2 + ggtitle(paste(T2[1], T2[2], sep="-"))

p_ssi_T3 <- process_probs(out_ssi_t3, out_hist, nvars=c("SSI", "STI"))
p_ssi_T3 <- p_ssi_T3 + ggtitle(paste(T3[1], T3[2], sep="-"))


ggsave(p_ssi_T2, file="../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/RCP/Probs_boxplot_SSI_T2.png", width = 12, height = 6)
ggsave(p_ssi_T3, file="../../Results/Hydro_project/Analaysis_Feb22/MetaGaussian/RCP/Probs_boxplot_SSI_T3.png", width = 12, height = 6)
