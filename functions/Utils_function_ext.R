# Several functions to get extremes indices
library(SPEI)
get_SPI <- function(metval){
 
  # metval <- met_tot[,c("date", "pr_mm")]
  # I will take prec from envidat as well , change units to mm
  names(metval)[grepl("pr",names(metval))] <- "pr_mm"
  dat_mon <-  metval%>%group_by(date=format(date,"%Y-%m"))%>%dplyr::summarise(tmonp=sum(pr_mm,na.rm=T))%>%
    mutate(date=as.Date(paste(date,"-01",sep="")), month=format(date,"%m"))
  spi_1 <- spi(dat_mon[,c("tmonp")],1, distribution = "Gamma")
  spi_3 <- spi(dat_mon[,c("tmonp")],3, distribution = "Gamma")
  spi_6 <- spi(dat_mon[,c("tmonp")],6, distribution = "Gamma")
  spi_12 <- spi(dat_mon[,c("tmonp")],12, distribution = "Gamma")
  
  #  Now I need to re-organize the data
  
  dat_spi1 <- convert_todf(spi_1$fitted, dat_mon$date)
  names(dat_spi1) <- c("date","spi_1")
  dat_spi3 <- convert_todf(spi_3$fitted, dat_mon$date)
  names(dat_spi3) <- c("date","spi_3")
  dat_spi6 <- convert_todf(spi_6$fitted, dat_mon$date)
  names(dat_spi6) <- c("date","spi_6")
  dat_spi12 <- convert_todf(spi_12$fitted, dat_mon$date)
  names(dat_spi12) <- c("date","spi_12")
  df_spi <- list(dat_spi1, dat_spi3, dat_spi6, dat_spi12) %>% reduce(left_join, by = "date")
  
  return(df_spi) 
}

get_SPEI <- function(metval){
  library(SPEI)
  # metval <- met_tot[,c("date", "pr_mm", "t2m")]
  # I will take prec from envidat as well , change units to mm
  names(metval)[grepl("pr",names(metval))] <- "pr_mm"
  dat_mon <-  metval%>%group_by(date=format(date,"%Y-%m"))%>%dplyr::summarise(PRCP=sum(pr_mm, na.rm=T), TMED=mean(t2m, na.rm=T))%>%
    mutate(date=as.Date(paste(date,"-01",sep="")), month=format(date,"%m"))
  # in case there is NA
  dat_mon <- na.omit(dat_mon)
  
  # Compute potential evapotranspiration (PET) and climatic water balance (BAL)
  dat_mon$PET <- thornthwaite(dat_mon$TMED, 37.6475)
  dat_mon$BAL <- dat_mon$PRCP-dat_mon$PET
  
  # One and tvelwe-months SPEI
  spei1 <- spei(dat_mon[,'BAL'], 1)
  spei3 <- spei(dat_mon[,'BAL'], 3)
  spei6 <- spei(dat_mon[,'BAL'], 6)
  spei12 <- spei(dat_mon[,'BAL'], 12)
  class(spei1)
  
  #  Now I need to re-organize the data

  dat_spei1 <- convert_todf(spei1$fitted, dat_mon$date)
  names(dat_spei1) <- c("date","spei_1")
  dat_spei3 <- convert_todf(spei3$fitted, dat_mon$date)
  names(dat_spei3) <- c("date","spei_3")
  dat_spei6 <- convert_todf(spei6$fitted, dat_mon$date)
  names(dat_spei6) <- c("date","spei_6")
  dat_spei12 <- convert_todf(spei12$fitted, dat_mon$date)
  names(dat_spei12) <- c("date","spei_12")
  df_spei <- list(dat_spei1, dat_spei3, dat_spei6, dat_spei12) %>% reduce(left_join, by = "date")
  
  return(df_spei) 
}


get_STI <- function(metval) {
  
  dat_mon <-  metval%>%group_by(date=format(date,"%Y-%m"))%>%dplyr::summarise(TMAX=mean(t2mmax, na.rm=T))%>%
    mutate(date=as.Date(paste(date,"-01",sep="")), month=format(date,"%m"))
  
  dat_mon$STI_1 <- sti(dat_mon$TMAX,1)
  dat_mon$STI_2 <- sti(dat_mon$TMAX,2)
  dat_mon$STI_3 <- sti(dat_mon$TMAX,3)

  return(dat_mon)
  
}

get_SSI <- function(metval) {
  
  dat_mon <-  metval%>%group_by(date=format(date,"%Y-%m"))%>%dplyr::summarise(dis=mean(discharge, na.rm=T))%>%
    mutate(date=as.Date(paste(date,"-01",sep="")), month=format(date,"%m"))
  # in case there is NA
  dat_mon <- na.omit(dat_mon)
  ssi1 <- spi(dat_mon[,'dis'], 1)
  ssi3 <- spi(dat_mon[,'dis'], 3)
  ssi6 <- spi(dat_mon[,'dis'], 6)
  
  
  dat_ssi1 <- convert_todf(ssi1$fitted, dat_mon$date)
  names(dat_ssi1) <- c("date","ssi_1")
  dat_ssi3 <- convert_todf(ssi3$fitted, dat_mon$date)
  names(dat_ssi3) <- c("date","ssi_3")
  dat_ssi6 <- convert_todf(ssi6$fitted, dat_mon$date)
  names(dat_ssi6) <- c("date","ssi_6")
 
  df_ssi <- list(dat_ssi1, dat_ssi3, dat_ssi6) %>% reduce(left_join, by = "date")
  
  return(df_ssi)
  
}

convert_todf <- function(ind, mydate){
  # ind is spi$fitted
  cnam <- colnames(ind)
  dat_spi     <- data.frame(Y=as.matrix(ind), date=as.Date(time(ind)))
  dat_spi$date <- mydate
  dat_spi <- dat_spi[,c("date",cnam)]
  return(dat_spi)
}


calculate_index <- function(dat_temp, y1, y2, pth, var, indType, ww = 5){
  require(heatwaveR)
  # Problem!!!!!!!!!!!I need to complete the temperature data sets!! 
  # ww is my windown, by defatult 5days
  # pth my percentile
  # indType if is high, pth 90,95 etc..
  #               low, pth 10,5 etc..
  dimsT <- dim(dat_temp) 
  # !pls update new dates
  times <- seq.Date(as.Date(paste(y1, "01-01", sep="-")), as.Date(paste(y2, "12-31", sep="-")), by ="day")
  # reference year I don't need this anymore...I'm using another function
  # yTref_end <- yTref +31 
  # id_st <- which(times==as.Date(paste(yTref,"01-01",sep="-")))
  # id_en <- which(times==as.Date(paste(yTref_end,"12-31",sep="-")))
  # mat_Tref  <- dat_temp[,,id_st:id_en]
  # calculate the threshold for each grid point
  mat_threshold <- mat_ex <- mat_log <-  array(NA, dim=c(dimsT))
  cat("get extremes for", indType, "percentile", pth, "\n")
  for (ilo in 1:dimsT[1]){
    for(ila in 1:dimsT[2]){
    
      df <- data.frame(t=times, temp=dat_temp[ilo,ila,])
      if (var == "t2m"){
        
        res <- ts2clm(df, climatologyPeriod = c("1980-01-01", "2011-12-31"), pctile = pth, windowHalfWidth = ww)
      }else{
        q   <- quantile(df$temp, probs=pth/100)
        res <- df
        res$thresh <- q[[1]]
      }
      if (indType =="high"){
        res$ex <- as.numeric(res$temp>res$thresh)
        mat_threshold[ilo,ila, ] <- res$thresh
        mat_ex[ilo, ila, ] <- res$ex
        mat_log[ilo, ila, ] <- res$temp>res$thresh
      }else if (indType=="low"){
        res$ex <- as.numeric(res$temp<res$thresh)
        mat_threshold[ilo,ila, ] <- res$thresh
        mat_ex[ilo, ila, ] <- res$ex
        mat_log[ilo, ila, ] <- res$temp<res$thresh
        # # threshold[ilo,ila,] <- mvThreshold(yTref, (yTref+31), (yTref+1), (yTref+30), mat_Tref[ilo,ila,], pth, 15)
        
      }
    }
  }
  
  return(list(mat_threshold, mat_ex, mat_log))
  
}

get_stagnation <- function(wind, th=3.2){
  
  dimsT  <- dim(wind)
  mat_ex  <-  array(NA, dim=c(dimsT)) 
  
  for (ilo in 1:dimsT[1]){
    for(ila in 1:dimsT[2]){
        mat_ex[ilo, ila, ] <- as.numeric(wind[ilo,ila,] < th)
    }
  }
  return(mat_ex)
}

getmat_spi <- function(matprec,y1,y2){
  # matprec <- metvars$TP
  times <- seq.Date(as.Date(paste(y1, "01-01", sep="-")), as.Date(paste(y2, "12-31", sep="-")), by ="day")
  months <- paste(unique(format(times, "%Y-%m")),"-01",sep="")
  dims =dim(matprec)
  mat_spi1 <- mat_spi3 <- mat_spi6 <- array(NA, dim=c(dims[1],dims[2], length(months)))
  
  for( i in 1:dims[1]){
    for(j in 1:dims[2]){
      metval <- data.frame(date=times, pr_mm=matprec[i,j,])
      temp <- get_spei(metval)
      mat_spi1[i, j, ] <- temp$spi_1
      mat_spi3[i, j, ] <- temp$spi_3
      mat_spi6[i, j, ] <- temp$spi_6
    }
  }
  
  return(list(mat_spi1, mat_spi3, mat_spi6))
}  

get_mat_compound <- function(mat1, mat2, shift, ...){
  # check dimension
  if(any(dim(mat1))!= any(dim(mat2))) 
    stop("Error: Elements of your input vector do not have the same length!")
  
  
  mdims <- dim(mat1)
  comp_mat <- array(NA, dim=mdims)
  # Note: I will loop over latitude-1 and longitude-1, because of the shifting , but even
  # if shift is FALSE, it won't affect the results (as I want the countries afterall)
  for(ilo in 1:(mdims[1]-1)){
    for(ila in 1:(mdims[2]-1)){
      comp_mat[ilo, ila, ] <- ifelse(mat1[ilo,ila,]==1 & mat2[ilo,ila,]==1, 1, 0)
        # shif 1 grid lon/lat
      if( shift ){
        grid1        <- mat1[ilo,ila,]*mat2[ilo,ila,]
        grid_pl_la1  <- mat1[ilo,ila,]*mat2[ilo,ila+ 1,]
        grid_pl_lo1  <- mat1[ilo,ila,]*mat2[ilo+1,ila,]
        grid_pl_lalo <- mat1[ilo,ila,]*mat2[ilo+1,ila+1,]
        
        g_temp <- ifelse(rowSums(cbind(grid1, grid_pl_la1, grid_pl_lo1, grid_pl_lalo))>=1,1,0)
        
        if ( ilo >1 ){
          grid_sub_lo1  <- mat1[ilo,ila,]*mat2[ilo-1,ila,]
          grid_sub_la1lo <- mat1[ilo,ila,]*mat2[ilo-1,ila+1,]
          g_temp <- ifelse(rowSums(cbind(grid_sub_lo1,grid_sub_la1lo, g_temp ))>=1,1,0)
        }
        if ( ila > 1){
          grid_sub_la1  <- mat1[ilo,ila,]*mat2[ilo,ila-1,]
          grid_sub_lalo1 <- mat1[ilo,ila,]*mat2[ilo+1,ila-1,]
          g_temp <-  ifelse(rowSums(cbind(grid_sub_la1,grid_sub_lalo1, g_temp ))>=1,1,0)
        }
        if ( ilo>1 & ila >1){
          grid_sub_la1lo1 <- mat1[ilo,ila,]*mat2[ilo-1,ila-1,]
          g_temp <- ifelse(rowSums(cbind(grid_sub_la1lo1, g_temp ))>=1,1,0)
        }
        
        comp_mat[ilo, ila, ] <- g_temp
      }
      
    }
  }
  
  return(comp_mat)
}


get_energy_ex <- function(e_country, mvar, pth, type){
  
  # rename variables
  exvar <- paste("ex_",mvar, sep="")
  Dvar  <- paste("D_", mvar, sep="")
 
 if ( all(is.na(e_country[,mvar])) ){
    new_df     <- e_country[,c("date","season",mvar)]
    # return empty values 
    new_df[,exvar] <- NA
    new_df[,Dvar] <- NA
  }else{
    
    e_country <- data.frame(e_country)
    qval       <- quantile(e_country[,mvar],probs=pth,na.rm=T)
    new_df     <- e_country[,c("date","season",mvar)]
    if ( type == "high" ){
      new_df[,exvar] <- as.numeric(new_df[,mvar]>qval[[1]])
    }else if ( type== "low" ){
      new_df[,exvar] <- as.numeric(new_df[,mvar]<qval[[1]])
    }
    
    new_df[,Dvar] <-  ave(new_df[,exvar], cumsum(new_df[,exvar] == 0), FUN = cumsum)
    max_indexes <-  which(diff(  sign(diff( c(0,new_df[,Dvar])))) == -2)
    # Get the lenght of the period (duration of extreme)
    #  get maximum duration each year
    # df_yy <- new_df%>%group_by(year=format(date, "%Y"))%>%dplyr::summarise(Dmax=max(D))
    
  }
  return(new_df)
  
}

define_events <- function(e_country, mvar, pth, type, len_w){
  
  
  # Get the lenght of the period (duration of extreme)
  event_in <- getInfo_event(e_country, mvar, pth, type, len_w)
  df_info      <- event_in$dfS
  # Severity
  if ( type == "high"){
    out <- df_info[,c("year","max","cumS","D", "fday", "lday")]
  }else if( type == "low"){
    out <-df_info[,c("year","min","cumS","D", "fday", "lday")]
  }
  
  names(out) <- c("year","P","S","D","fday","lday")
  
  return(list("df_ev"=out, "info_ev"=event_in$newD))
  
}

getInfo_event_withdecluster <- function(e_country, mvar, pth, type, N, TH_ref){
  # this function is testing...use the older one with len_w=2
  # UPdate: the severity is now defined as SUM(E-E_Threshold)
  # AS is the cumulative sum of standarised anomalies of power (or RL)
  if (!all(is.na(e_country[,mvar]))){
    e_country  <- data.frame(e_country)
    if (TH_ref){
      qval <- pth
    }else{
      qval       <- quantile(e_country[,mvar],probs=pth,na.rm=T)
    }
 
    df     <- e_country
    
    exvar <- paste("ex_",mvar, sep="")
    Dvar  <- paste("D_", mvar, sep="")
    
    if ( type == "high"){
      df[,exvar] <- as.numeric(df[,mvar]>qval[[1]])
      exc   <- df[,mvar][df[,mvar]>qval[[1]]]
      times <- df[,"date"][df[,mvar]>qval[[1]]]
    }else if (type== "low"){
      df[,exvar] <- as.numeric(df[,mvar]<qval[[1]])
      exc <- df[,mvar][df[,mvar]<qval[[1]]]
      times <- df[,"date"][df[,mvar]<qval[[1]]]
      
    }
    
   # estimate anomalies
    anom_v <- paste("anom_",mvar,sep="")
    df[,anom_v] <- (df[,mvar]-mean(df[,mvar]))/sd(df[,mvar])
    
    # excess of power , i.e power - power_thershold
    excess_v <- paste("excess_",mvar,sep="")
    df[,excess_v] <- (df[,mvar]-qval[[1]])
    
    # ------ get index times of ecents
    attr(exc, "times") <- times
    #  Add to the original data and check the index
    data_dec <- evir::decluster(exc, run=N)
    ind_time <- attributes(data_dec)
  
    # get duration 
    #  Using the whole data set
    df[,Dvar] <-  ave(df[,exvar], cumsum(df[,exvar] == 0), FUN = cumsum)
    # Now filter with the declustered data
    df_filt <- df[df$date%in%ind_time$times,] 
    # get the information
    metv <- c("t2mmax","t2mmean","t2mmin","wsmax","wsmin","wsmean","tp", "ssrd", "w100")
    test <-   merge(df_filt, df[,c("date",mvar,anom_v,excess_v, metv)], all=T)
    subD <- sumD <-  list()
    for ( i in 1:length(ind_time$times)){
      
      itime <- which(test$date==ind_time$times[[i]])
      
      if (test[itime,Dvar] >1){
        
        ind2 <- itime
        ind0 <- ind2-(test[ind2,Dvar]-1)
        subD[[i]] <- test[ind0:ind2, ]
        sinfo  <-  subD[[i]] %>% 
          dplyr::summarise_at(vars(mvar), funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm=T), cumS = sum(., na.rm=T)))
        
        sinfo_2 <- subD[[i]] %>% 
          dplyr::summarise_at(vars(excess_v, anom_v), funs(abs(sum(., na.rm=T))))%>%dplyr::rename("ES"=excess_v,"AS"=anom_v)
        sinfo_2 <- sinfo_2%>%dplyr::mutate("ES_std" = ES/sd(df[,mvar]))
        sinfo_met  <-  subD[[i]] %>% 
          dplyr::summarise_at(vars(metv), funs(mean(., na.rm = TRUE)))
        sinfo[,Dvar]    <- test[itime,Dvar]
        sinfo[,"sdate"] <-  test[ind0,"date"] # start date
        sinfo[,"edate"] <-  test[ind2,"date"] # end date
        
        sinfo_t <- merge(sinfo, sinfo_met)
        sinfo_tot <- merge(sinfo_t, sinfo_2)
        sumD[[i]]       <- data.frame(sinfo_tot)
        
      }else{
        
        subD[[i]] <- test[itime,]
        sinfo  <-  subD[[i]] %>% 
         dplyr::summarise_at(vars(mvar), funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm=T), cumS = sum(., na.rm=T)))
        sinfo_2 <- subD[[i]] %>% 
          dplyr::summarise_at(vars(excess_v, anom_v), funs(abs(sum(., na.rm=T))))%>%dplyr::rename("ES"=excess_v,"AS"=anom_v)
        sinfo_2 <- sinfo_2%>%dplyr::mutate("ES_std" = ES/sd(df[,mvar]))
        sinfo[,Dvar]    <- test[itime,Dvar]
        sinfo[,"sdate"] <- sinfo[,"edate"] <-  test[itime,"date"]
        sinfo_met  <-  subD[[i]] %>% 
          dplyr::summarise_at(vars(metv), funs(mean(., na.rm = TRUE)))
        sinfo_t <- merge(sinfo, sinfo_met)
        sinfo_tot <- merge(sinfo_t, sinfo_2)
        sumD[[i]]       <- data.frame(sinfo_tot)
      }
     
    }
  
    out <- melt(sumD, id=names(sumD[[1]]))
    out$L1 <- NULL
    
    if (type == "high"){
      
      out <- out[,c( "max", "cumS", "ES", "ES_std","AS",Dvar, "sdate","edate",metv)]
      
    }else if (type=="low"){
      
      out <- out[,c( "min", "cumS","ES","ES_std","AS",Dvar, "sdate","edate",metv)]
      
    }
    names(out) <- c("P","S","ES", "ES_std","AS","D","sdate", "edate",metv)
    
    return(list(out, subD))
  }
}


getInfo_event <- function(e_country, mvar, pth, type, len_w, fgap, ...){
  # test data 
  # new_df <- test_pv_cs$AT10001
  # len_w <- length of "wave" (e.g. 3 for temperature or ozone, 5 for blocking)
  # xvar must be the cumulative value of the variable (not just 0 or 1)
  # len_w <- 3
  # xvar <- "value_csum"
  # nvar <- c("O3_ex", "Tmax_ex" ,"TxO", "TxBl", "BlO", "coh")
  # var2 <- "O3_ex" or "Tmax_ex"
  # Get index where there are more than 3 (5)-days
  # Find "local" maximum values indices
  
  exvar <- paste("ex_",mvar, sep="")
  Dvar  <- paste("D_", mvar, sep="")
  
  stopifnot(!all(is.na(e_country[,mvar])))
  
  new_df  <- data.frame(e_country)
  qval       <- quantile(e_country[,mvar],probs=pth,na.rm=T)
  #new_df     <- e_country[,c("date","season",mvar)]
  
  if ( type == "high" ){
    new_df[,exvar] <- as.numeric(e_country[,mvar]>qval[[1]])
  }else if ( type== "low" ){
    new_df[,exvar] <- as.numeric(e_country[,mvar]<qval[[1]])
  }
  
  new_df[,Dvar] <-  ave(new_df[,exvar], cumsum(new_df[,exvar] == 0), FUN = cumsum)
  max_indexes   <-  which(diff(  sign(diff( c(0,new_df[,Dvar])))) == -2)
  local_w       <- new_df[max_indexes,Dvar]
  # Select the periods that meet the criteria (> len_w)
  # Allow +-1 lag-day between extremes?
  xnew <-  list()
  ind1 <- ind2 <- array(NA)
  subD <- list()
  sumD <- list()
  num.ev <- 1
  if( length(max_indexes)>0){
    
    for(i in 1:length(max_indexes)){
      
      if( new_df[max_indexes[i],Dvar] >= len_w) {
        # Get properties of each event  3/longer-days
        nind  <- max_indexes[i]
        lenw  <- new_df[nind,Dvar]
        ns    <- nind-local_w[i]+1
        subD[[i]]  <- new_df[ns:nind,] 
        subD[[i]]$n <- num.ev
        fday <-  subD[[i]]$date[1]
        lday <- subD[[i]]$date[length(subD[[i]]$date)]
        # maximum length of event
        sinfo  <-  subD[[i]] %>% group_by(year=format(date,"%Y"))%>%
          dplyr::summarise_at(vars(mvar), funs(min(., na.rm = TRUE), mean(., na.rm = TRUE), max(., na.rm=T), cumS = sum(., na.rm=T)))
        # Get the number of extremes of the other variable (e.g if I analyse O3, get the number of Tx during pollution)
        sumD[[i]] <- data.frame(sinfo)
        sumD[[i]]$fday <- fday
        sumD[[i]]$lday <- lday
        sumD[[i]]$D    <- new_df[max_indexes[i],Dvar]
        num.ev <- num.ev + 1
      }
    }
    # Remove NULL elements
    subD <- Filter(length, subD)
    sumD <- Filter(length, sumD)
  }
  
  if(!is_empty(subD)){
    # Get full information about all events
    newD <- data.frame(do.call(rbind, subD))
    # dfS contains mean 
    dfS  <- data.frame(do.call(rbind, sumD))

    return(list("dfS"=dfS, "newD"=newD))
    
  }else {
    
    return(list("dfS"=NULL, "newD" = NULL))
  } 
  
}  

avg_index <- function(x, v1, th1, indname, type, y1, y2){
  # y1 <- "1979";y2 <- "2019"
  times  <- seq.Date(as.Date(paste(y1, "01-01", sep="-")), as.Date(paste(y2, "12-31", sep="-")), by ="day")
  dd     <- data.frame(t=times, temp=x[,v1])
  if ( v1 %in% c("Tx90","T10", "DD7")){
    res    <- ts2clm(dd, climatologyPeriod = c("1980-01-01", "2011-12-31"), pctile = th1, windowHalfWidth = 5)
  }else{
    res <- dd
    q <- quantile(dd$temp, probs=th1/100)
    res$thresh <- q[[1]]
  }
  if (type == "high"){
    res$ex <- as.numeric(res$temp>res$thresh)
  }else{
    res$ex <- as.numeric(res$temp<res$thresh)
  }
  
  df.ex <- data.frame(date=res$t)
  df.ex[,indname] <- res$ex
  return(df.ex)
}


merge_metvalues_events <- function(list_xx,  y){
  # list_xx energy data events (subD from getInfo_event)
  # xx is the summary of events
  # y meteorological data
  l_met <- list()
  #  get info met
  for ( i in 1:length(list_xx)){
    subdates <- list_xx[[i]]$date
    dfmet_info <- y%>%dplyr::filter(date%in%subdates)
    l_met[[i]] <- merge(list_xx[[i]], dfmet_info, id="date")
  }
  
  return(l_met)
}

merge_metpth_events <- function(list_xx, xx,  y){
  # list_xx energy data events (subD from getInfo_event)
  # xx is the summary of events
  # y meteorological data
  l_met <- list()
  #  get info met
  vvar <- names(y)[!names(y)%in%c("date","country")]
  vv_ecdf <- lapply(1:length(vvar), function(i) ecdf(y[,vvar[[i]]][[1]]))
  vv_eu_ecdf <-  lapply(1:length(vvar), function(i) ecdf(df_eu[,vvar[[i]]][[1]]))
  
  local_pth <- eu_pth <- l_met <-  l_met_eu <-val_met <-  list()
  for ( i in 1:length(list_xx)){
    
    subdates <- list_xx[[i]]$date
    dfmet_info <- y%>%dplyr::filter(date%in%subdates)
    info_ev <-   xx%>%dplyr::filter(sdate%in%subdates) #get info event
    
    for (ivar in 1:length(vvar)){
      # work with the mean values
      mean_val <- mean(dfmet_info[,vvar[ivar]][[1]], na.rm=T)
      local_pth[[ivar]] <- vv_ecdf[[ivar]](mean_val)*100
      eu_pth[[ivar]] <- vv_eu_ecdf[[ivar]](mean_val)*100
    }
    names(local_pth) <- names(eu_pth) <- vvar
    df_met_th <- do.call(cbind.data.frame, local_pth)
    df_met_eu_th <- do.call(cbind.data.frame, eu_pth)
    
    dd <- cbind(info_ev,df_met_th)
    dd_eu <- cbind(info_ev,df_met_eu_th)
    l_met[[i]] <- dd
    l_met_eu[[i]] <- dd_eu
    mean_event <- colMeans(dfmet_info[,-c(1:2)])
    mean_event <- data.frame(t(mean_event))
    mean_event$sdate <- info_ev$sdate
    mean_event$edate <- info_ev$edate
    mean_event$P <- info_ev$P
    mean_event$S <- info_ev$S
    mean_event$D <- info_ev$D
    val_met[[i]] <- mean_event
  }
  
  df_out <- melt(l_met, id=names(l_met[[1]]))
  df_values <- melt(val_met, id=names(val_met[[1]]))
  df_eu_out <- melt(l_met, id=names(l_met_eu[[1]]))
  df_values$L1 <- df_out$L1 <- df_eu_out$L1 <- NULL
  return(list("local_pth"=df_out,"EU_pth" = df_eu_out, "df_values" =df_values))
  
}




avg_CE <- function(dd, v1, v2, ce_var){
  
  y1 <- dd[,v1][[1]]
  y2 <- dd[,v2][[1]]
  out <- dd[,"date"]
  out[,ce_var] <- y1*y2
  
  return(out)
  
}

process_avg_index_ex <- function(ll){
  l_ind_avg          <- lapply(ll, function(x) setNames(melt(x, id="date"), c("date","variable","value","country")))
  df_ind_avg <- melt(l_ind_avg, id=names(l_ind_avg[[1]]))  
  df_ind_avg$L1 <- NULL
  dftot_ind         <-  df_ind_avg%>%pivot_wider(names_from= variable, values_from=value)
  return(dftot_ind)
}


processTw <- function(TW, nvar){
  ddw <- setNames(melt(TW, id=names(TW[[1]])), c(names(TW[[1]]), "country"))
  ddw <- ddw[,c("date","country","TW")]
  names(ddw) <- c("date","country",nvar)
  return(ddw)
  
}

define_Temperature_Waves <- function(dat_index, ivar, wlen=3){
  # select temperature indices
  xvar <- "Tex_csum"; 
  dd <- data.frame(dat_index[,c("date",ivar)])
  dd[,xvar] <-  ave(dd[,ivar], cumsum(dd[,ivar] == 0), FUN = cumsum)
  max_indexes <-  which(diff(  sign(diff( c(0,dd[,xvar])))) == -2)
  # Get the lenght of the period (duration of extreme)
  local_w <- dd[max_indexes,xvar]
  # initical positions for each event
  pos_i   <- max_indexes-local_w+1
  # Get positions of events
  xi <- sapply(1:length(pos_i), function(i) pos_i[i]:max_indexes[i], simplify = TRUE)
  # initizalize TW *Temperature-Wave
  dd["TW"] <- 0
  
  if( length(max_indexes)>0){
    
    for(i in 1:length(max_indexes)){
      
      if( dd[max_indexes[i],xvar] >= wlen) {
        # Get properties of each event  3/longer-days
        nind  <- max_indexes[i]
        lenw  <- dd[nind,xvar]
        ns    <- nind-local_w[i]+1
        dd[ns:nind,"TW"] <- 1 
      }else{
        # reset to 0
        nind  <- max_indexes[i]
        dd[nind,"TW"] <- 0
      }
    }
  }
  return(dd)
}


