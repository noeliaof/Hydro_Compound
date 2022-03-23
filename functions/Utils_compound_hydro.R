# Assess compounds 
# 1. Create compound hot-dry monthly scale
# 2. Create CE at daily scale
create_rollsum_hotdry <- function(dat, tmax_dat){
  
  dd     <- data.frame("date"=as.Date(as.character(tmax_data$V1),format= "%Y%m%d"), "t2max"=tmax_data[,which(grepl(n_obj, colnames(tmax_data)))])
  # convert to ºC
  dd$t2max <- dd$t2max-273.15
  new_df <- merge(dat, dd, id="date")
  new_df <- create_season_UP(new_df, NULL)
  # rollsum prec
  dat_r <- new_df%>%dplyr::mutate(prec7D=zoo::rollsum(prec, k = 7, fill = NA), DD7=zoo::rollsum(prec, k = 7, fill = NA))
  DD7_90 <- avg_index(dat_r, "DD7", 90, "DD7_90", "high", "1980", "2018")
  dat_tot <- merge(dat_r, DD7_90, id="date")
  
}


Adding_hotdry_information <- function(dat, tmax_data){
  # This function create some index to account for hor-dry conditions
  #@dat is an element of the list list_entsoe_re or list_ror
  #
  # check the colnames
  if(any(colnames(dat)=="tp") || any(colnames(dat) == "t2mmax")){
    colnames(dat)[(colnames(dat)=="tp")] <- "prec"
    colnames(dat)[(colnames(dat)=="t2mmax")] <- "t2max"
  }
  
  x_spei <- get_SPEI(dat[,c("date","prec","t2m")])
  x_spi  <- get_SPEI(dat[,c("date","prec","t2m")])
  n_obj  <- unique(dat$OBJECTID)
  
  if (!is.null(tmax_data)){
    dd     <- data.frame("date"=as.Date(as.character(tmax_data$V1),format= "%Y%m%d"), 
                         "t2max"=tmax_data[,colnames(tmax_data)[which(grepl(paste("\\bV",n_obj,"\\b",sep=""), colnames(tmax_data)))]])
    new_df <- merge(dat, dd, id="date")
  }else{
    new_df <- dat
  }
  # Add moving sum for prec. (7 days)
  new_df <- new_df%>%dplyr::mutate(prec7D=zoo::rollsum(prec, k = 7, fill = NA))
  new_df <- create_season_UP(new_df, NULL)
  # create T2max index
  # create index
  t90_ix <- avg_index(new_df, "t2max", 90, "T90", "high", "1980","2018")
  t90_ix_mon <- t90_ix%>%group_by(date=format(date,"%Y-%m"))%>%dplyr::summarise(nt=sum(T90))%>%
    mutate(date=as.Date(paste(date,"-01",sep="")))
  t90_ix_mon <- create_season_UP(t90_ix_mon, NULL)
  df_month <- merge(x_spei, t90_ix_mon, id="date")
  df_month <- create_season_UP(df_month, NULL)
  
  # Create daily data (merge spei and t90)
  # Merge
  day_tmp_dat <-  merge( new_df,x_spei, by = "date", all = TRUE)
  # I will set the first row of spei_3 and spei_6 to 0 to create daily data
  day_dat <- day_tmp_dat
  day_dat[1,c("spei_3")] <- day_dat[1,c("spei_6")] <- day_dat[1,c("spei_12")] <- 0 
  # Fill up NA's
  day_dat <-  transform(day_dat, spei_1 = na.locf(spei_1), spei_3 = na.locf(spei_3), spei_6 = na.locf(spei_6), spei_12 = na.locf(spei_12))
  day_dat <-  merge(day_dat,t90_ix, id="date" ) 
  #  Add HW
  df_HW <- define_Temperature_Waves(day_dat, ivar="T90", wlen=5)
  day_dat <- merge(day_dat, df_HW[,c("date","TW")])
 
  return(day_dat)
  
}


# 
# # plot SPEI
# p <- ggplot(x_spei, aes(x=date,y=spei_1)) +
#   geom_ribbon(aes(ymin=pmin(spei_1,0), ymax=0), fill="blue", col="grey80", alpha=0.5) +
#   geom_ribbon(aes(ymin=0, ymax=pmax(spei_1,0)), fill="red", col="grey80", alpha=0.5) +
#   geom_hline(aes(yintercept=0), color="grey") 
# 
# p + geom_line(data=day_dat, aes(x=date, y=scale(hp)), color="black", alpha=0.5) + ylab("Scaled-HP (MB)")
#   



calculate_JP_t2p <-  function(dat, seas, vars){
  #' @vars: variables used to fit the copula (e.g. prec, t2m)
  # fit a copula to get JP 
  # analyse separetely each season
  Nr <- 1000
  station <- unique(dat$WKA.Name)
  if (!is.null(seas)){
    dat_s <- dat%>%dplyr::filter(season==seas)
  }else{
    dat_s <- dat
  }
  # analyse kendal, tau cor
  cor(dat_s[,vars])
  # transform to pseudo-obs
  dd <- dat_s[,vars]
  m <- pobs(as.matrix(dd))
  pairs(as.copuladata(m))
  
  selectedCopula   <-  BiCopSelect(m[,1], m[,2], family=c(1,2,3,4,5,6), selectioncrit = "AIC", rotations=FALSE)
  fname_cop   <- selectedCopula$familyname
  cat ( "Selected copula:",fname_cop,"\n")
  # --- Estimate copula parameters
  cop_model <- namecop(selectedCopula)
  # --- fit the model
  cop_fit  <-  tryCatch({ fitCopula(cop_model, m, method = 'ml')
  }, error=function(e){ 
    if( grepl("non-finite",conditionMessage(e))) {
      fitCopula(cop_model, m, method = 'itau')
    }else{cat("ERROR :",station,conditionMessage(e), "\n")}
  })
  
  # check <- BiCopGofTest(m[,1],m[,2],family=selectedCopula$family)
  check <- gofCopula(cop_model, m,simulation = "mult", estim.method = "mpl")
  
  if (selectedCopula$family==0) {
    my_cop <- indepCopula(dim=2)
  }else if (selectedCopula$family==1) {
    my_cop <- normalCopula(param=coef(cop_fit)[[1]])
  }else if(selectedCopula$family==2){
    my_cop <- tCopula(param=coef(cop_fit)[[1]])
  }else if(selectedCopula$family==3){
    my_cop <- claytonCopula(param=coef(cop_fit)[[1]])
  }else if(selectedCopula$family==4){
    my_cop <- gumbelCopula(param=coef(cop_fit)[[1]])
  }else if(selectedCopula$family==5){
    my_cop <- frankCopula(param=coef(cop_fit)[[1]])
  }else if(selectedCopula$family==6){
    my_cop <- joeCopula(param=coef(cop_fit)[[1]])
  }else if(selectedCopula$family==13 | selectedCopula$family==23 | selectedCopula$family==33){
    my_cop <- rotCopula(claytonCopula(param=coef(cop_fit)[[1]]))
  }else if(selectedCopula$family==16 | selectedCopula$family==26){
    my_cop <- rotCopula(joeCopula(param=coef(cop_fit)[[1]]))
    # } else if(selectedCopula$family==26){
    # my_cop <- r90JoeBiCopula(param=coef(cop_fit)[[1]])
  } else if(selectedCopula$family==14 | selectedCopula$family==24){
    my_cop <- rotCopula(gumbelCopula(param=coef(cop_fit)[[1]]))
  } else if(selectedCopula$family==104 | selectedCopula$family==204){
    my_cop <- tawnCopula(param=coef(cop_fit)[[1]])
  }
  # get tables-data.frame for coupla information
  
  tabl_info<- data.frame("familiy"=selectedCopula$familyname, "param"=cop_fit@estimate, "AIC"=selectedCopula$AIC,
                         "pval"= check$p.value, "sn"=check$statistic)
  
  
  # Create a contour plot of the selected copula
  contour(my_cop, dCopula, main = fname_cop)
  # Create a perspective plot and a scatter plot for the Gaussian Copula
  persp(my_cop,dCopula, phi = 15, theta = 30)
  u1 <-  rCopula(Nr, my_cop) # sample for the selected copula
  plot(u1[,1],u1[,2],col='blue', main = fname_cop,xlab = "u1", ylab = "u2")
  cat("correlation sample", "\n")
  cor(u1,method='spearman') # 
  cat("correlation my data","\n")
  cor(dd, method= "spearman", use = "complete")
  
  # Fitting marginals
  u_fit <- myfits(dd[,1], "none", th=NULL) # duration
  v_fit <- myfits(dd[,2], "none", th=NULL) # severity
  
  
  
}


add_SCE_SPEI_todata <- function(data_s){
  
  data_s <- data_s%>%dplyr::mutate(prec7D=rollapply(tp, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                    by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_s <- data_s%>%dplyr::mutate(prec15D=rollapply(tp, width=15, FUN=function(x) mean(x, na.rm=TRUE), 
                                                    by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_s <- data_s%>%dplyr::mutate(prec30D=rollapply(tp, width=30, FUN=function(x) mean(x, na.rm=TRUE), 
                                                    by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  
  # temperature
  data_s <- data_s%>%dplyr::mutate(t2max7D=rollapply(t2mmax, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                    by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_s <- data_s%>%dplyr::mutate(t2max15D=rollapply(t2mmax, width=15, FUN=function(x) mean(x, na.rm=TRUE), 
                                                     by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_s <- data_s%>%dplyr::mutate(t2max30D=rollapply(t2mmax, width=30, FUN=function(x) mean(x, na.rm=TRUE), 
                                                     by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  # add also the lag of discharge
  data_s <- data_s%>%dplyr::mutate(dis7D=rollapply(discharge, width=7, FUN=function(x) mean(x, na.rm=TRUE), 
                                                     by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_s <- data_s%>%dplyr::mutate(dis15D=rollapply(discharge, width=15, FUN=function(x) mean(x, na.rm=TRUE), 
                                                   by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
  data_s <- data_s%>%dplyr::mutate(dis30D=rollapply(discharge, width=30, FUN=function(x) mean(x, na.rm=TRUE), 
                                                   by=1, by.column=TRUE, partial=TRUE, fill=NA, align="center"))
 
 
  
  data_s <- create_season_UP(data_s,NULL)
  names(data_s)[names(data_s)=="tp"] <- "prec"
  x_spei <- get_SPEI(data_s[,c("date","prec","t2m")])
  x_spi  <- get_SPEI(data_s[,c("date","prec","t2m")])
  
  # add sti
  x_sti <- get_STI(data_s[,c("date","t2mmax")])
  x_sti <- x_sti%>%dplyr::select(-month, -TMAX)
  
  # add SSU-streamflow
  x_ssi <- get_SSI(data_s[,c("date","discharge")])

  
  # get the index
  seasons <- unique(data_s$season)
  xvars <- c("prec","t2mmax")
  seas_index <- list()
  
  # I will skip this part...
  # for (iseas in 1:length(seasons)){
  #   data_seas <- data_s%>%dplyr::filter(season==seasons[iseas])
  #   out_index <- Get_index_JP_HD(data_seas[,c("date",xvars)], xvars, wka_name = unique(data_seas$name_p), mtype = "none", dout = m_dout)
  #   seas_index[[iseas]] <- out_index
  # }
  # names(seas_index) <- seasons
  # 
  # df_seas_index <- setNames(melt(seas_index, id=names(seas_index[[1]])), c(names(seas_index[[1]]),"season"))
  # merge information
  # data_merged <- merge(data_s, df_seas_index, id=c("date","season"))
  
  data_merged <- data_s
  day_tmp_dat <-  merge( data_merged,x_spei, by = "date", all = TRUE)
  day_tmp_dat_ext <- merge(day_tmp_dat, x_sti, by="date", all=TRUE)
  # day_tmp_dat_ext_2 <- merge(day_tmp_dat_ext, x_ssi, by="date", all.y=TRUE)
  # I will set the first row of spei_3 and spei_6 to 0 to create daily data
  day_dat <- day_tmp_dat_ext
  day_dat[1,c("spei_3")] <- day_dat[1,c("spei_6")] <- day_dat[1,c("spei_12")] <- 0 
  day_dat[1,c("STI_2")] <- day_dat[1,c("STI_3")] <- 0
  # day_dat[1,c("ssi_3")] <- day_dat[1,c("ssi_6")] <- 0
  # Fill up NA's
  day_dat <-  transform(day_dat, spei_1 = na.locf(spei_1), spei_3 = na.locf(spei_3), spei_6 = na.locf(spei_6), spei_12 = na.locf(spei_12),
                        STI_1 = na.locf(STI_1),STI_2 = na.locf(STI_2),STI_3 = na.locf(STI_3))
  
  return(day_dat)
}

