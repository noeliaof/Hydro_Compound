plot_TS_simobs <- function(sim, obs, dout){ 
  
  yy  <- unique(format(sim$date, "%Y"))[unique(format(sim$date, "%Y"))%in%unique(format(obs$date, "%Y"))]
  obs_yy <- obs%>%dplyr::filter(format(date,"%Y")%in%yy)
  obs_yy <- obs_yy[,c("date","runoff_mmd")]
  sim_yy <- sim%>%dplyr::filter(format(date,"%Y")%in%yy)
  sim_yy <- sim_yy[,c("date",names(sim)[grepl("dis",names(sim))])]
  sim_yy <- sim_yy[,c(names(sim_yy)[!grepl("m3s",names(sim_yy))])]
  dfm <- merge(sim_yy, obs_yy, id="date", all=T)
  dfmm <- melt(dfm, id="date")
  p <- ggplot2::ggplot(dfmm, aes(x=date, y=value, color=variable)) +
    geom_line() + 
    ylab("(mm/d)") +
    scale_color_brewer(palette = "Set1",labels=c("prevah","obs"))+
    theme_bw() 
  
  ggsave(p, filename = paste(dout,unique(obs$place), ".png", sep=""))
  
}

# keep the top-10 and do composites from metevar
get_metcompos <- function(top_events, metvars, num, mettimes){
  
  # top_events for each country!
  # e.g. top_events <- top_demand[[1]]
  #@ num is the number of events I want to look at (the first one, 2 ...)
  # select only the first 10
  topn    <- top_events[1:num,]
  dates_n <- as.Date(topn$date)
  # filter dates 
  ind_n <- which(mettimes%in%dates_n)
  # filter metvars
  l_compos <- setNames(lapply(1:length(metvars), function(i) metvars[[i]][,,ind_n]), names(metvars))
  
  return(l_compos)
  
}


country_compos <- function(l_events, lon, lat, num, dirplot){
  # num =1  nuber of events
  info_count <- lapply(1:length(l_events), function(i) get_metcompos(l_events[[i]], metvars, num, mettimes))
  # I also need to extact the corresponding dates
  dates_num <- lapply(1:length(l_events), function(i) l_events[[i]]$date[1:num])
  names(info_count) <- names(dates_num) <-  names(l_events)
  # plot 
  lonlat <- expand.grid(lon=lon,lat=lat)
  #as lonlat was a row/column name, it is character, that's why we convert it into numeric
  lonlat <- apply(lonlat,2,as.numeric)
  
  convert_todf <- function(ll, lonlat){
    
    mvar <- names(ll)
    l_df <- lapply(1:length(mvar), function(ivar) data.frame(lonlat, value=as.vector(ll[[mvar[ivar]]])))
    names(l_df) <-  c("Tmax","Tmean","Tmin","Windmax","Windmin","TP")
    dft <- melt(l_df, id=names(l_df[[1]]))
    names(dft) <- c("lon","lat","value","variable")
    return(dft)
  }
  
  # check if info_count is empty
  id_prob <- lapply(info_count, function(x) length(x[[1]]))
  info_count <- info_count[id_prob!=0]
  f_country <- n_country[id_prob!=0]
  dates_new <- dates_num[id_prob!=0]
  dft <- lapply(1:length(info_count), function(i) convert_todf(info_count[[i]], lonlat))
  # Need to plot the variable separately 
  colbr <- brewer.pal(11,"RdBu")
  myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")), space="Lab")
  colw <- brewer.pal(6,"Blues")
  #import the countries limits
  limit <- st_read("../../data/EU_shapefiles/CNTR_RG_03M_2020_4326.shp/CNTR_RG_03M_2020_4326.shp")
  
  # ---------plot ----------------
  for (ic in 1:length(f_country)){
    cat("plot for", f_country[ic],"\n")
    
    temp_p <- dft[[ic]]%>%dplyr::filter(variable%in%c("Tmax","Tmean","Tmin"))%>% ggplot2::ggplot()+
      geom_tile(aes(lon,lat,fill=value))+ #temperature data
      geom_sf(data=limit,fill=NA,size=.5)+ #limits
      scale_fill_gradientn(colours=rev(colbr), name="(ºC)")+
      coord_sf(ylim=c(lat[1],lat[length(lat)]),xlim=c(lon[1],lon[length(lon)]))+
      scale_x_continuous(breaks=seq(lon[1],lon[length(lon)],10),expand=c(0,0))+
      scale_y_continuous(breaks=seq(lat[1],lat[length(lat)],10),expand=c(0,0))+
      facet_grid(~variable)+
      #plot panels by month
      theme_bw()
    
    wind_p <- dft[[ic]]%>%dplyr::filter(variable%in%c("Windmax","Windmin"))%>% ggplot2::ggplot()+
      geom_tile(aes(lon,lat,fill=value))+ #temperature data
      geom_sf(data=limit,fill=NA,size=.5)+ #limits
      scale_fill_distiller(palette = "RdYlGn", name="(m/sg)") + 
      coord_sf(ylim=c(lat[1],lat[length(lat)]),xlim=c(lon[1],lon[length(lon)]))+
      scale_x_continuous(breaks=seq(lon[1],lon[length(lon)],10),expand=c(0,0))+
      scale_y_continuous(breaks=seq(lat[1],lat[length(lat)],10),expand=c(0,0))+
      facet_grid(~variable)+
      #plot panels by month
      theme_bw()
    
    prec_p <- dft[[ic]]%>%dplyr::filter(variable=="TP")%>% ggplot2::ggplot()+
      geom_tile(aes(lon,lat,fill=value))+ #temperature data
      geom_sf(data=limit,fill=NA,size=.5)+ #limits
      scale_fill_gradientn(colours=colw, name=("mm/day"))+
      coord_sf(ylim=c(lat[1],lat[length(lat)]),xlim=c(lon[1],lon[length(lon)]))+
      scale_x_continuous(breaks=seq(lon[1],lon[length(lon)],10),expand=c(0,0))+
      scale_y_continuous(breaks=seq(lat[1],lat[length(lat)],10),expand=c(0,0))+
      facet_grid(~variable)+
      #plot panels by month
      theme_bw()
    
    
    pt <- plot_grid(temp_p, wind_p, prec_p, nrow=3)
    
    fout <-  paste(dirplot,f_country[ic],"_",dates_new[[ic]],".png",sep="" )
    ggsave(filename = fout, pt, width = 12, height = 8)
    
  }
  
  
}


plot_country_monthInfo <- function(yy_country, mvar){
  require(hrbrthemes)
  
  get_nameleg <- function(mvar){ 
    if (mvar =="WD"){
      mleg <- " Demand (GW)"
    }else if( mvar == "res_load_WS"){
      mleg <- "DNR (RE=wind+solar)(GW)"
    }else if( mvar == "res_load_tot"){
      mleg <- "DNR (RE=wind+solar+HP)(GW)"
    }else if( mvar == "pw_wind"){
      mleg <- "Wind production(GW)"
    }else if( mvar == "pw_solar"){
      mleg <- "Solar production (GW)"
    }else if( mvar == "pw_hy_ror"){
      mleg <- "Hydro-ROR production (GW)"
    }else if( mvar == "pw_hy_re"){
      mleg <- "Hydro_RE production (GW)"
    }
    return(mleg)
    
  }
  
  mleg <- get_nameleg(mvar)
  yy_country <- Filter(length,yy_country)
  df <- melt(yy_country, id=names(yy_country[[1]]))
  
  cc <- "country"; mm <- "month"
  p <-  df%>%group_by(country, month=format(date,"%m"))%>%dplyr::summarise_at(vars(mvar),mean, na.rm=T)%>%
    ggplot2::ggplot(aes_string(y=cc, x= mm, fill=mvar)) +  geom_tile() +
    scale_fill_distiller(palette = "Spectral", name= mleg)+
    theme_ipsum() +
    theme(legend.position="bottom")
  
  return(p)
  
}


visualice_Drought_event_declusEv <- function(e_country, cvars, lowP, highD,yy, type, mdir){
  # get 90th of demand and residual
  # same for production
  # e_country if obtained from dft 
  # cvars <- c("WD", "PWS")
  # probs <- c(0.90, 0.10)
  # lowP <- low_pws_country$AT[[1]]
  # highD <- high_demand_country$AT[[1]]
  
  # select event yy
  # extract event information
  event_lp <- lowP[[1]]
  event_hd <- highD[[1]]
  
 
  # Extract dates
  event_lp <- event_lp[,c("sdate","edate","D")]
  event_hd <- event_hd[,c("sdate","edate","D")]
  # select variable
  df_country <- e_country%>%dplyr::select(c(date,cvars))
  df_scale   <- df_country%>%dplyr::filter(format(date,"%Y")%in%yy)%>%dplyr::mutate_at(cvars, (scale))   
  nvars <- c(paste("scale", cvars,sep=""))
  names(df_scale) <- c("date",nvars)
  dfp        <- merge(df_country, df_scale, id="date")
  

  if ( type=="normalised" ){
    xx1 <- nvars[1]; xx2 <- nvars[2]; dd <- "date" 
    p <- ggplot2::ggplot(dfp) + geom_line(aes_string(dd, xx1), color="red")  +
      geom_line(aes_string(dd,xx2), color="blue", linetype = "dashed")
    ymin <- min(dfp[,xx1]); ymax <- max(dfp[,xx1])
  }else{
    xx1 <- cvars[1]; xx2 <- cvars[2]; dd <- "date" 
    p <- ggplot2::ggplot(dfp) + geom_line(aes_string(dd, xx1), color="red")  
    ymin <- min(dfp[,xx1]); ymax <- max(dfp[,xx1])
  }
  
  
  df_yy_event <- event_hd%>%dplyr::filter(format(sdate,"%Y")%in%yy)
  dp_yy_event <- event_lp%>%dplyr::filter(format(sdate,"%Y")%in%yy)
  
  p <- p + geom_rect(data=df_yy_event, inherit.aes=FALSE,
                aes(xmin=as.Date(sdate),
                    xmax=as.Date(edate),
                    ymin=ymin,ymax=ymax),alpha=0.4, fill="red") +
      geom_rect(data=dp_yy_event, inherit.aes=FALSE,
              aes(xmin=as.Date(sdate),
                  xmax=as.Date(edate),
                  ymin=ymin,ymax=ymax), alpha=0.5,fill="blue", color="blue") +
    scale_fill_discrete(name = " ") + theme_bw() + ggtitle(yy)
  
  ggsave(p, filename = paste(mdir, unique(e_country$country), "_",yy,".png", sep=""), width = 12, height = 6)
  
}




visualice_Drought_event <- function(e_country, yy, cvars, type, mdir){
  # get 90th of demand and residual
  # same for production
  # probs <- c(0.90, 0.10)

  event_in <- getInfo_event(e_country, mvar=cvars[1], 0.9, "high", 1)[[1]]
  low_prod <- getInfo_event(e_country, mvar=cvars[2], 0.1, "low", 1)[[1]]

  # select variable
  # select variable
  df_country <- e_country%>%dplyr::select(c(date,cvars))
  df_scale   <- df_country%>%dplyr::filter(format(date,"%Y")%in%yy)%>%dplyr::mutate_at(cvars, (scale))   
  nvars <- c(paste("scale", cvars,sep=""))
  names(df_scale) <- c("date",nvars)
  dfp        <- merge(df_country, df_scale, id="date")
  
  if ( type == "normalised" ){
    xx1 <- nvars[1]; xx2 <- nvars[2]; dd <- "date" 
    xv <- cvars[1];xv2 <- cvars[2]
    p <- ggplot2::ggplot(dfp) + geom_line(mapping=(aes(date, scaleWD, color="WD")))  +
      geom_line(mapping=(aes(date,scalePWS,color="PWS"))) + ylab(paste("Scale")) +
      scale_color_manual(name = " ", values = c("WD" = "darkblue", "PWS" = "orange"))
    ymin <- min(dfp[,xx1]); ymax <- max(dfp[,xx1])
  }else{
    xx1 <- cvars[1]; xx2 <- cvars[2]; dd <- "date" 
    p <- ggplot2::ggplot(dfp) + geom_line(aes(date, WD), color="orange")  
    ymin <- min(dfp[,xx1]); ymax <- max(dfp[,xx1])
  }


  df_yy_event <- event_in%>%dplyr::filter(year%in%yy)
  dp_yy_event <- low_prod%>%dplyr::filter(year%in%yy)

  p <- p + geom_rect(data=df_yy_event, inherit.aes=FALSE,
                aes(xmin=as.Date(df_yy_event$fday),
                    xmax=as.Date(df_yy_event$lday),
                    ymin=ymin,ymax=ymax),alpha=0.3, fill="darkblue") +
    geom_rect(data=dp_yy_event, inherit.aes=FALSE,
              aes(xmin=as.Date(dp_yy_event$fday),
                  xmax=as.Date(dp_yy_event$lday),
                  ymin=ymin,ymax=ymax),alpha=0.4, fill="orange") +
    scale_fill_discrete(name = " ") + 
    theme_bw() + ggtitle(yy)
  ggsave(p, filename = paste(mdir,"_", unique(e_country$country), "_",yy,"_",type,".png", sep=""), width = 12, height = 6)

}


plot_heatmaps <- function(mm){
  
  dff_mat <- reshape2::melt(mm)
  names(dff_mat) <- c("SPEI","STI","Prob")
  p <- ggplot2::ggplot(dff_mat, aes(x=SPEI, y=STI, fill=Prob)) + geom_tile() +  
    geom_text(aes(label = round(Prob, 3))) +
    scale_fill_gradient(low = "white", high = "red") + theme_bw()
  return(p)
}