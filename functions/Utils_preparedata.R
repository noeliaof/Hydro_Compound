organize_mon_prodxls <- function(hydro_prod){
  tmp1   <- hydro_prod%>% separate("Year-Month", c("Year-Month","temp"), sep = "(/)")%>%dplyr::select(-(temp))
  sname  <- strsplit(tmp1[[1]],"-")
  # identify years
  i_name     <- lapply(sname, function(x) length(x))
  ind_yy <- which((unlist(i_name))>1)
  yy <- as.numeric(unlist(lapply(sname[ind_yy], function(x) x[[1]])))
  ny <- length(yy)
  # add dec2020
  tmp1[480,] <- NA
  l_yy <- split(tmp1,rep(1:ny,each=12))
  # rename months
  mon <- c("January","February","March","April", "May", "June", "July", "August", "September", "October", "November", "December")
  l_syy <- lapply(l_yy, function(x,y) {x[,1] <- y
  x}, mon)
  l_syy <- lapply(l_syy, function(x) setNames(x,c("month","ror","re","total")))
  names(l_syy) <- yy
  # now, melt everything again
  df_yy <- setNames(reshape2::melt(l_syy, id=names(l_syy[[1]])), c(names(l_syy[[1]]),"year"))
  # create a date
  num_m <- lubridate::days_in_month( as.Date(paste(df_yy$year,df_yy$month,"01",sep="-"), "%Y-%b-%d"))
  df_yy$date <- as.Date(paste(df_yy$year,df_yy$month,num_m,sep="-"), "%Y-%b-%d")
  
  return(df_yy)
}

load_met <- function(dirmet){
  # load and process nc
  
  mvars <- c("T2MMAX","T2MMEAN","T2MMIN","WSmax","ws","WSmin","TP")
  l_metvars <- lapply(1:length(mvars), function(i) get_nc(paste(metdir, list.files(metdir),sep="")[i], mvars[i]))
  # extract lat & lon
  names(l_metvars) <- mvars
  lon <- l_metvars[[1]]$lon
  lat <- l_metvars[[2]]$lat
  metvars <- lapply(l_metvars, function(x) x[[1]])
  # convert temperature to degree celsius
  metvars[1:3] <- lapply(metvars[1:3], function(x) { x <- x-273.15
  x})
  return(list(metvars, lon, lat))
  
}
get_c3s_meteo <- function(dir_cs3, country){
  # dir_cs3 <- "/Users/noeliaotero/Documents/OCCR/data/CS3/"
  
  met_var         <- c("t2m","totpr","wind10")
  fil_country     <-   list.files(paste(dir_cs3,"meteo_country/", sep=""), pattern = "*csv")
  fil_subcountry  <- list.files(paste(dir_cs3,"meteo_subcountry/", sep=""), pattern = "*csv")
  data_country    <- lapply(1:length(met_var), function(i) read.csv(paste(dir_cs3,"meteo_country/", fil_country, sep="")[i], skip=52))
  data_subcountry <- lapply(1:length(met_var), function(i) read.csv(paste(dir_cs3,"meteo_subcountry/", fil_subcountry, sep="")[i], skip=52))
  names(data_country) <- names(data_subcountry) <- met_var
  if ( !is.null(country) ){
  # elect by country: CH and re-organize the data
    met_list_ch  <- lapply(data_country, function(x) x[,c("Date",country)])
    met_tmp      <- melt(met_list_ch,id=names(met_list_ch[[1]]))
    met_ch       <- met_tmp%>%pivot_wider(names_from = L1, values_from = country)
    met_ch$Date  <- as.POSIXct(met_ch$Date, tz="UTC")
  # merge with load
  
  # Create daily average data
    met_chD <- met_ch%>%dplyr::group_by(date=as.Date(format(Date, "%Y-%m-%d")))%>%summarise_at(vars(t2m, wind10), mean, na.rm=T)
    met_tp <- met_ch%>%dplyr::group_by(date=as.Date(format(Date, "%Y-%m-%d")))%>%summarise_at(vars(totpr), sum, na.rm=T)%>%
      mutate(pr_mm=totpr*1000)
    met_tot <- merge(met_chD, met_tp, id="date")
  }
    return(met_tot)
  
}


merge_ext_country <- function(i_country){
  # function for organice extremes of energy data
  dfm <- melt(i_country, id=c("date", "season"))
  dfm$L1 <- NULL
  df_out <- dfm%>%group_by(variable) %>%
    mutate(row = row_number()) %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) 
  return(df_out)
  
}

merge_ce_country <- function(d_country){
  # this function will re-organize the data of extremes and compound
  myfun <- function(x){
    dd <- x[[1]][,c("date","CE")]
    names(dd) <- c("date",names(x))
    return(dd)
  }
  dc <- lapply(1:length(d_country), function(i) myfun(d_country[i]))
  df <- melt(dc, id="date") ;df$L1 <- NULL                        
  df <- df%>%pivot_wider(names_from=variable, values_from=value)
  return(df)
}



merge_energy_compound_ex <- function(ener_country, ext_country){
  # function to merge in the same data frame energy and extreme data
  
  df <- merge(ener_country, ext_country, id="date")
  
  
}


melt_matrix <- function(mat_c, mettimes, avg=FALSE){
  
  if (avg==TRUE) {
    tmp <- apply(mat_c, 3, mean)
    dx <- data.frame("time"=mettimes, "value" = tmp)
  }else{
    x <- melt(mat_c)
    x$Var3 <- mettimes
    dx <- x%>%dplyr::select(Var3, value)%>%dplyr::rename("time"="Var3")
  }
  
  return(dx)  
  
}


read_WR_KIT <- function(ff, type_file, daily=TRUE){
  if (type_file == "proj"){
    # I need to do a weird artifact to handle this!
    cnames <- c("HourS","times","EOF_atr","max_WR", "AT", "ZO", "ScTr", "AR","EuBL", "ScBL", "GL")
  }else{             # AT ZO ScTr AR EuBL ScBL GL
    cnames <- c("HourS","times", "hour","EOF_atr","max_WR", "LC")
  }
  r_dat <- read_csv(ff,quote = ",", skip=5)
  names(r_dat) <- "all"
  if ( type_file == "proj"){
    n_dat <- r_dat %>%
      separate(1, into = cnames, sep = " {1,}")
    n_dat <- n_dat%>%separate(times, into=c("times","hour"))
    n_dat <- n_dat%>%dplyr:: mutate_at(vars(c("AT", "ZO", "ScTr", "AR","EuBL", "ScBL", "GL")),funs(as.numeric))
  }else{
    n_dat <- r_dat%>%separate(all, into=cnames)
  }
  # transform time
  n_dat$date <- as.POSIXct(paste(n_dat$times, n_dat$hour), format = "%Y%m%d %H", tz = "UTC") 

  if( daily ){
    # get the daily values: I assume I need to take the LC
      if ( type_file == "proj" ){
      dat_out <- n_dat%>%group_by(date=format(date,"%Y-%m-%d"))%>%
        dplyr::summarise_at(vars(c("AT", "ZO", "ScTr", "AR","EuBL", "ScBL", "GL")), mean, na.rm=TRUE)
    
      }else{
        # take the maximum number
        temp <- n_dat%>%group_by(date=as.Date(format(date,"%Y-%m-%d")),LC)%>%dplyr::summarise(nWR=n())
        temp2 <- temp%>%group_by(date)%>%dplyr::mutate(WR = ifelse(max(nWR)==4,LC[LC!=0], LC[which.max(nWR)] ))
        dat_out <- temp2%>%group_by(date=as.Date(format(date,"%Y-%m-%d")))%>%dplyr::summarise(WR=as.factor(unique(WR)))
        # dat_out <- n_dat%>%group_by(date=as.Date(format(date,"%Y-%m-%d")))%>%dplyr::summarise(WR= as.factor(unique(LC)))
        dat_out$type <- dat_out$WR
        dat_out <- data.frame(change_numWR_toname(dat_out))
      }
   }

  dat_out$date <- as.Date(dat_out$date)
  
  return(dat_out)
  
}

get_wr_index <- function(w_index){
  
  n_index <- c("AT", "ZO", "ScTr", "AR" ,"EuBL", "ScBL", "GL" ,"no")
  ii <- c(1,6,7,4,5,3,0)
  
  if( w_index == 0){
    new_names <- "no"
  }else if(w_index == 1){
    new_names <- "AT"
  }else if(w_index == 6){
    new_names <- "ZO"
  }else if(w_index == 7){
    new_names <- "ScTr"
  }else if(w_index == 4){
    new_names <- "AR"
  }else if(w_index == 5){
    new_names <- "EuBL"
  }else if(w_index == 3){
      new_names <- "GL"
  }
  
  return(new_names)
  
}
