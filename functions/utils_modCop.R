# Utils copula

myfits <- function(xx, mtype, th, ...){
  
  require(fitdistrplus)
  
  
  if (mtype%in%c("gev","pareto")){
    if (mtype=="gev"){
      ff <- gev.fit(xx)
      params <- ff$mle
     }else if(mtype=="pareto"){
      thx <- quantile(xx, probs=th)
      ff <- gpd.fit(xx, thx[[1]])
      params <- c(ff$threshold, ff$mle)
    }
    nam.mar <- mtype
    estimates <- params  
    gf <- gofTest(xx, distribution = mtype, test = "ks")
    
    
  }else{
    
    if (mtype!="none"){
      distributions <- mtype
    }else{
      distributions <-  c("norm", "lnorm", "gamma", "weibull", "exp")
    }
    
 
    
    distr_aic <-  list()
    distr_fit <-  list()
    ks.lis    <- list()
    gf.list <- list()
  

    for (distribution in distributions) {
      
      distr_fit[[distribution]] <-  tryCatch({fitdist(data=xx, distribution)}, error=function(e) {cat(distribution)}) 
      
      if ( !is.null(distr_fit[[distribution]] )){
        distr_aic[[distribution]] <-  distr_fit[[distribution]]$aic
        pdis <- paste("p",distribution, sep="")
        gf.list[[distribution]] <- gofstat(distr_fit[[distribution]])
        if (distribution == "norm"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, mean= distr_fit[[distribution]]$estimate["mean"],
                                            sd = distr_fit[[distribution]]$estimate["sd"])
        }else if (distribution == "lnorm"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, mean= distr_fit[[distribution]]$estimate["meanlog"],
                                            sd = distr_fit[[distribution]]$estimate["sdlog"])
        }else if (distribution == "gamma"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, shape= distr_fit[[distribution]]$estimate["shape"],
                                            rate = distr_fit[[distribution]]$estimate["rate"])
        }else if (distribution == "weibull"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, shape= distr_fit[[distribution]]$estimate["shape"],
                                            scale = distr_fit[[distribution]]$estimate["scale"])
         }else if (distribution == "exp"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, rate= distr_fit[[distribution]]$estimate["rate"])
        }
      }
    }
    
    # ind.min <- which.min(distr_aic)
    # nam.mar <- names(distr_aic[ind.min])
    # ks.pval <- ks.lis[[ind.min]]$p.value
    # 
    df.bic <- setNames(melt(lapply(gf.list, function(x) x$bic)), c("value", "name"))
    df.aic <- setNames(melt(lapply(gf.list, function(x) x$aic)), c("value", "name"))
    df.kspval <- setNames(melt(lapply(gf.list, function(x) x$ks)), c("value", "name"))
    df.cvpval <- setNames(melt(lapply(gf.list, function(x) x$cvm)), c("value", "name"))
    # select based on the minimum aic and compatible with pval >0.05
    ind <- which.min(df.aic$value)
      if (df.kspval$value[ind]>0.05 | length(df.kspval$value)==1){
        # I will keep ind, otherwise I look for the next lower aic
        nam.mar <- names(distr_aic[ind])
        ks.pval <- df.kspval$value[ind]
        estimates <- distr_fit[[ind]]$estimate
        
      }else{
        # find another index
         new_ind <- order(df.aic$value)[-1] #excluding the first
         ind.ks <-  which(df.kspval$value[new_ind] >0.05)
         
         if ( length(ind.ks)>1 ){
           # take the largest pvalue
           ind.ks    <- order(df.kspval$value[new_ind], decreasing = T)[1] 
           ks.pval   <- df.kspval$value[ind.ks]
           estimates <- distr_fit[[ind.ks]]$estimate
           nam.mar   <- names(distr_aic[ind.ks])
           
          }else{
           
           new.ind.ks    <- order(df.kspval$value[new_ind], decreasing = T)[1] 
           ks.pval   <- df.kspval$value[new.ind.ks]
           estimates <- distr_fit[[new.ind.ks]]$estimate
           nam.mar   <- names(distr_aic[new.ind.ks])
           
          }
      }
     # cdfcomp(distr_fit)
       
    }
     mm <- eval(parse(text=(paste("p",nam.mar, sep=""))))
     if (length(estimates)>2){
       p_non <- mm(xx, estimates[1],estimates[2], estimates[3])
     }else{
       p_non <- mm(xx, estimates[1],estimates[2])
     }
     
    if (mtype%in%c("gev","pareto")){
      
      return(list(data.frame(nam.mar=mtype, pval=gf$p.value), "estimates"=params, "pnon"=p_non))
    }else{
      return(list(data.frame(nam.mar,ks.pval), "estimates"=estimates, "pnon"=p_non))
      
    }
     
}


# univariate analysis 
univar_f <- function(x, dat){
  
  # extract name  
  mm <- eval(parse(text=(paste("p",x[[1]]$nam.mar, sep=""))))
  # probability of non-exceedances
  if (  x[[1]]$nam.mar == "nbinom"){
    p_non <- mm(dat, size = x$estimates[1], mu= x$estimates[2])
  }else{
    
    if (length(x$estimates)>2 ){
      p_non <- mm(dat, x$estimates[1],x$estimates[2], x$estimates[3])
    }else{
      p_non <- mm(dat, x$estimates[1],x$estimates[2])
    }
  }
  
  # probability of exceedances
  p_ex=1-p_non
  rp=1/p_ex
  return(data.frame(p_non, p_ex, rp))
}

join_bivariate_analysis_nonparametric <- function(dd, mcountry, type_var, dout){
# In this case I am not assuming any marginal type
  Nr <- 1000
  
  dd <- data.frame(dd)
  nevent <- length(dd[,1])
  nyears <- 41
  db <- na.omit(dd) 
  # convert pseudo-obs
  # m  <- pobs(as.matrix(db)) 
  ties_met <- "random"
  m <- pobs(as.matrix(db), ties.method = ties_met)
  # select copula
  selectedCopula   <- BiCopSelect(m[,1], m[,2], familyset = c(1, 2, 3, 4, 6), selectioncrit = "AIC", rotations = F)
  #check <- BiCopGofTest(m[,1],m[,2],family=selectedCopula$family)
  #  if pvalue >0.05 we accept copula (if pval<0.05 we can reject that the empirical copula is significantly represented
  #  by the parametric copula )
  fname_cop       <- selectedCopula$familyname
  cat ( "Selected copula:",fname_cop,"\n")
  # --- Estimate copula parameters
  cop_model <- namecop(selectedCopula)
  # --- fit the model
  cop_fit  <-  tryCatch({ fitCopula(cop_model, m, method = 'ml')
  }, error=function(e){ 
    if( grepl("non-finite",conditionMessage(e))) {
      fitCopula(cop_model, m, method = 'itau')
    }else{cat("ERROR :",mcountry,conditionMessage(e), "\n")}
  })
  
  check <- tryCatch({ gofCopula(cop_model, m,simulation = "mult", estim.method = "mpl")
  }, error=function(e){ 
    if( grepl("NA/NaN/Inf",conditionMessage(e))) {
      gofCopula(cop_model, m)
    }else{cat("ERROR :",mcountry,conditionMessage(e), "\n")}
  })
  
  # Not sure if include rotated or not...
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
  
  # Create a contour plot of the selected copula
  contour(my_cop, dCopula, main = fname_cop)
  # Create a perspective plot and a scatter plot for the Gaussian Copula
  persp(my_cop,dCopula, phi = 15, theta = 30)
  u1 <-  rCopula(Nr, my_cop) # sample for the selected copula
  plot(u1[,1],u1[,2],col='blue', main = fname_cop,xlab = "u1", ylab = "u2")
  cat("correlation sample", "\n")
  cor(u1,method='spearman') # 
  cat("correlation my data","\n")
  cor(db, method= "spearman", use = "complete")
  
  comparing_empirical_with_mycops(m, cop_fit, selectedCopula, mcountry, dout)
  plotC_nonparametric(db, my_cop, dout , mcountry, fname_cop)

  #  simulate distribution
  int_arr <- nyears/nevent
  # probability of exceedances
  uu <- 0.95; dim <- 2
  sim_cop  <- rCopula(Nr, my_cop)
  
  # use kernel estimation to convert sim_cop to original scale
  # sim_cop
  
  ex_sim_prob <- mean(sim_cop[,1]>uu &  sim_cop[,2]>uu)
  Fu_ex95 <- mean(sim_cop[,1]>uu)
  Fv_ex95 <- mean(sim_cop[,2]>uu)

  ex.prob <- 1- 0.95 - 0.95 + pCopula(c(0.95,0.95), my_cop)
  
  if (length(dd[,1])> nyears){
    # to get the jointreturn period I'm considering the interarrival time
    jp_rp <- int_arr/ex.prob
  }else if (length(dd[,1]) == nyears){
    jp_rp <- 1/ex.prob
  }
  
  # Defining different types of ED 
  # Weak: 2D and S>75th
  # Moderate: 4D and S>90th
  # Severe: 7D and S>95th
  thu <- quantile(dd[,1], probs=c(0.75,0.90,0.95,0.99))
  #  duration: 4, 7 and 9
  thu_fixed <- c(2,4,7,9)
  thv <- quantile(m[,2], probs=c(0.75,0.90,0.95,0.99))
 
  # return copula info
  df_cop <- data.frame("familiy" = selectedCopula$familyname, "param" = my_cop@parameters, "check"=check$p.value)
  
  return(list("df_cop"= df_cop,"check"=check, data.frame("rp"=c("95th"),"jp"=c(ex.prob),"jp_rp"=c(jp_rp),  
                                                            "Fu"=rbind(Fu_ex95), "Fv"=rbind(Fv_ex95))))
}

comparing_empirical_with_mycops <- function(U, cop_fit, selectedCopula,mcountry, mdir){
  # U=m
  library(latticeExtra)
  if (selectedCopula$family == 2){
    cpG <- contourplot2(tCopula(cop_fit@estimate[1]), FUN = pCopula, region = FALSE,
                        key = list(corner = c(0.04, 0.04),
                                   lines = list(col = 1:2, lwd = 2),
                                   text = list(c(selectedCopula$familyname,
                                                 "Empirical copula"))))
  }else{
    cpG <- contourplot2(cop_fit@copula, FUN = pCopula, region = FALSE,
                        key = list(corner = c(0.04, 0.04),
                                   lines = list(col = 1:2, lwd = 2),
                                   text = list(c(selectedCopula$familyname,
                                                 "Empirical copula"))))
  }
  # empirical
  u <- seq(0, 1, length.out = 10)
  grid <- as.matrix(expand.grid(u1 = u, u2 = u))
  val <- cbind(grid, z = C.n(grid, X = U))
  cpCn <- contourplot2(val, region = FALSE, labels = FALSE, col = 2)
  gridAB  <- grid.arrange(cpG + cpCn)
  ggsave(gridAB, file=paste(mdir, "empirical_fitcop_", mcountry,".png",sep=""))
  
}


join_bivariate_analysis <- function(dd, mtype, mcountry, type_var, dout){
  # dd is data.frame/matrix with two variables
  Nr <- 1000
  
  dd <- data.frame(dd)
  nevent <- length(dd[,1])
  nyears <- 41
  # get marginals, I try GEV, but need to check other distributions
  # ----- Univariate ---------
  if ( mtype == "pareto"){
    u_fit <- myfits(dd[,1], mtype, th=0.9)
    v_fit <- myfits(dd[,2], mtype, th=0.1)
  }else{
    u_fit <- myfits(dd[,1], mtype, th=NULL) # duration
    v_fit <- myfits(dd[,2], mtype, th=NULL) # severity
  }

  # to save marginal information
  info_mar <- rbind.data.frame("umar"=u_fit[[1]],"vmar"=v_fit[[1]])
  
  param_u   <- u_fit[[2]] # Duration
  param_v   <- v_fit[[2]] # Severity
  
  # parametric marginals
  u_d <- u_fit[[3]]
  v_s <- v_fit[[3]]
  m <- as.matrix(cbind(u_d, v_s)) # parametrically
  # --------start copula------------
  if (mtype == "pareto"){
    cat("fitting pareto")
    # in the case of using only exceedances
    q1 <- quantile(dd[,1], probs=0.9)
    q2 <- quantile(dd[,2], probs=0.1)
 
    d1<- dd[,1][dd[,1]>q1]
    d2 <- dd[,2][dd[,2]<q2]  # Need to check this!!!!!!!!!!!!!!!!!!1
    db <- cbind(d1,d2)
  }else{
    
    db <- dd

  }
  
  # Update: note:
  # for some countries for annual maxima Clayton copula gives problem when using the BiCopSelect, 
  # I will exclude familiy=5 for those countries (still doing the AIC selection)
  # Updates: for simplicity I am going to remove Clayton for all processes! 
  if ((mcountry=="Norway" & type_var=="pw_solar") | (mcountry=="Italy" & type_var=="res_solar")){
    selectedCopula   <- BiCopSelect(m[,1], m[,2], familyset = 4)
  }else if (mcountry=="Sweden" & type_var == "res_load" ){
    selectedCopula   <- BiCopSelect(m[,1], m[,2], familyset = 1, rotations = F)
  }else{
    selectedCopula   <- BiCopSelect(m[,1], m[,2], familyset = c(1, 2, 3, 4, 6), selectioncrit = "AIC", rotations = F)
  #   selectedCopula   <- BiCopSelect(m[,1], m[,2], familyset = c(1, 2, 3, 5 ,4, 6), selectioncrit = "AIC", rotations = F)
  }
  # 
   #  if pvalue >0.05 we accept copula (if pval<0.05 we can reject that the empirical copula is significantly represented
  #  by the parametric copula )
  fname_cop   <- selectedCopula$familyname
  cat ( "Selected copula:",fname_cop,"\n")
  # --- Estimate copula parameters
  cop_model <- namecop(selectedCopula)
  # --- fit the model
  cop_fit  <-  tryCatch({ fitCopula(cop_model, m, method = 'ml')
  }, error=function(e){ 
    if( grepl("non-finite",conditionMessage(e))) {
      fitCopula(cop_model, m, method = 'itau')
    }else{cat("ERROR :",mcountry,conditionMessage(e), "\n")}
  })
  
  # check <- BiCopGofTest(m[,1],m[,2],family=selectedCopula$family)
  #check <- gofCopula(cop_model, m,simulation = "mult", estim.method = "mpl")
  check <- tryCatch({ gofCopula(cop_model, m,simulation = "mult", estim.method = "mpl")
  }, error=function(e){ 
    if( grepl("NA/NaN/Inf",conditionMessage(e))) {
      gofCopula(cop_model, m)
    }else{cat("ERROR :",mcountry,conditionMessage(e), "\n")}
  })
  
  # Not sure if include rotated or not...
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
  cor(db, method= "spearman", use = "complete")

    # ------------model for margins (I chose GPD)-------------------------------
    if ( mtype== "gev"){
      # xi is the shape, mu=loc; beta=scale
      my.dis <- mvdc(my_cop, margins = c("gev", "gev"),
                     paramMargins =  list(list(mu=u_fit$estimates[[1]],beta=u_fit$estimates[[2]],xi=u_fit$estimates[[3]]),
                                          list(mu=v_fit$estimates[[1]],beta=v_fit$estimates[[2]],xi=v_fit$estimates[[3]])))
        
    }else if (mtype == "gpd" | mtype == "pareto" ){
      
      my.dis <- mvdc(my_cop, margins = c("gpd", "gpd"),
                     paramMargins =  list(list(loc=u_fit$estimates[[1]],scale=u_fit$estimates[[2]],shape=u_fit$estimates[[3]]),
                                            list(loc=v_fit$estimates[[1]],scale=v_fit$estimates[[2]],shape=v_fit$estimates[[3]])))
    }else{
        # my.dis <- mvdc(my_cop, margins = c(u_fit[[1]]$nam.mar, v_fit[[1]]$nam.mar),
        #                paramMargins = list(list(u_fit$estimates[[1]],u_fit$estimates[[2]]),
        #                                    list(v_fit$estimates[[1]],v_fit$estimates[[2]])))
        my.dis <- mvdc(my_cop, margins = c(u_fit[[1]]$nam.mar, v_fit[[1]]$nam.mar),
                       paramMargins = list(list(u_fit$estimates),
                                           list(v_fit$estimates)))
        
    }
    #  simulate distribution
    sim  <- rMvdc(Nr, my.dis)
    # Compute the density
    pdf_mvd <- dMvdc(sim, my.dis)
    # Compute the CDF
    cdf_mvd <- pMvdc(sim, my.dis)
    
    # Plot curves
    # I need to work on this part (addapt to each distribution)
    plotC(db, u_fit, v_fit, my.dis, my_cop, dout , mcountry, fname_cop)
    comparing_empirical_with_mycops(m, cop_fit, selectedCopula, mcountry, dout)
    
    # Return period lines plot
    plot_lines_RP_conditional_SD(dd, u_fit, v_fit, my_cop, mcountry, dout)
    
    thu <- quantile(dd[,1], probs=c(0.75,0.90,0.95,0.99))
    #  duration: 4, 7 and 9
    thu_fixed <- c(2,4,7,9)
    thv <- quantile(dd[,2], probs=c(0.75,0.90,0.95,0.99))
  
    # Univariate RP
    # Need to implement this properly!!!!!!!!!!!!!!
    # NON exceedances
    Fu_exD1 <-  univar_f(u_fit, dat=thu_fixed[1])[[1]]
    # Fu_ex90 <-  univar_f(u_fit, dat=thu[1])[[1]]
    Fu_exD2 <-  univar_f(u_fit, dat=thu_fixed[2])[[1]]
    Fu_exD3 <-  univar_f(u_fit, dat=thu_fixed[3])[[1]]
    Fu_exD4 <-  univar_f(u_fit, dat=thu_fixed[4])[[1]]
   # for severity
   
    Fv_ex75 <-  univar_f(v_fit, dat=thv[1])[[1]] 
    Fv_ex90 <-  univar_f(v_fit, dat=thv[2])[[1]]
    Fv_ex95 <-  univar_f(v_fit, dat=thv[3])[[1]]
    Fv_ex99 <-  univar_f(v_fit, dat=thv[4])[[1]]
    
    # probability of exceedances
    dim <- 2
    C75 <- pCopula(rep(0.75, dim), copula = my_cop)
    C90 <- pCopula(rep(0.90, dim), copula = my_cop)
    C95 <- pCopula(rep(0.95, dim), copula = my_cop)
    C99 <- pCopula(rep(0.99, dim), copula = my_cop)
    # For fixed Duration
    Cf75 <- pCopula(c(Fu_exD1, Fv_ex75), copula = my_cop)
    Cf90 <- pCopula(c(Fu_exD2, Fv_ex90), copula = my_cop)
    Cf95 <- pCopula(c(Fu_exD3, Fv_ex95), copula = my_cop)
    Cf99 <- pCopula(c(Fu_exD4, Fv_ex99), copula = my_cop)

    # AND _JOIN prob
    p75 <- 1 - 0.75 - 0.75 + C90 
    p90 <- 1 - 0.90 - 0.90 + C90 
    p95 <- 1 - 0.95 - 0.95 + C95 
    p99 <- 1 - 0.99 - 0.99 + C99 
    
    # Fixed duration
    pD1S75 <- 1 - Fu_exD1 - Fv_ex75 + Cf75
    pD2S90 <- 1 - Fu_exD2 - Fv_ex90 + Cf90 
    pD3S95 <- 1 - Fu_exD3 - Fv_ex95 + Cf95 
    pD4S99 <- 1 - Fu_exD4 - Fv_ex99 + Cf99 
    
    get_prob_fromSim <- function(thD, thS){
      out_l <- data.frame("pD1S75"=NA, "pD2S90"=NA, "pD3S95"=NA, "pD4S99"=NA)
      for(i in 1:length(thv)){
        out_l[,i] <- mean(sim[,1]>=thD[i] & sim[,2]>=thS[i])
      }
      
      out <- data.frame("D"=c("75th","90th","95th","99th"),"S"=c("75th","90th","95th","99th"),"jp"=t(out_l), "rp"=1/t(out_l))
      rownames(out) <- NULL
      return(out)
    }
    
    out_simDS <- get_prob_fromSim(thu_fixed, thv)
    out_sim <- get_prob_fromSim(thu, thv)
    
    if (length(dd[,1])> nyears){
      # to get the jointreturn period I'm considering the interarrival time
      rp75 <- int_arr/p75 ;  rp_D1S75 <- int_arr/pD1S75
      rp90 <- int_arr/p90 ;  rp_D2S90 <- int_arr/pD2S90
      rp95 <- int_arr/p95 ;  rp_D3S95 <- int_arr/pD3S95
      rp99 <- int_arr/p99 ;  rp_D4S99 <- int_arr/pD4S99
    }else if (length(dd[,1]) == nyears){
      rp75 <- 1/p75 ;  rp_D1S75 <- 1/pD1S75
      rp90 <- 1/p90 ;  rp_D2S90 <- 1/pD2S90
      rp95 <- 1/p95 ;  rp_D3S95 <- 1/pD3S95
      rp99 <- 1/p99 ;  rp_D4S99 <- 1/pD4S99
    }
    
    dat_prob_th <- data.frame("D"=c("75th","90th","95th","99th"),"S"=c("75th","90th","95th","99th"),"jp"=c(p75,p90,p95,p99),"rp"=c(rp75,rp90,rp95,rp99))
    dat_prob_DSth <- data.frame("D"= thu_fixed, "S" =c("75th","90th","95th","99th"),"jp"=c(pD1S75,pD2S90,pD3S95,pD4S99),"rp"=c(rp_D1S75,rp_D2S90,rp_D3S95,rp_D4S99))
    
    return(list("tabl_info"=tabl_info,"info_mar"=info_mar, "dat_prob_th"=dat_prob_th, "dat_prob_DSth" = dat_prob_DSth, "out_sim"=out_sim, "out_sim_DS"=out_simDS))
    
}
  

plotC_nonparametric <- function(db, my_cop, dir, mcountry, fname_cop){
  # get example from 
  # https://stackoverflow.com/questions/50356312/how-to-get-ggplot2-geom-contour-to-replicate-base-graphics-contour
  Nr <- 1000
  #  this also can be done
  # non-Parametrically
  m <- pobs(db)
  ff <- as.matrix(pobs(db))

  # Adding this new part
  # -------compute copula distribution------
  # f_cop_non <- pCopula(ff, my_cop) 
  # Using ranked pseudo-obs
  f_cop_non <- pCopula(ff, my_cop) 
  
  f_cop <- 1-f_cop_non
  
  rp <- 1/f_cop
  # to plot
  Cop.vd <-  as.matrix(cbind(m[,1],m[,2],f_cop_non,f_cop,rp))
  fSim <- seq(0.05, 0.99998, length.out = 1000)
  # simulate from copula
  sim  <- rCopula(Nr, my_cop) 
  qD <- sim[,1][order(sim[,1])]
  qM <- sim[,2][order(sim[,2])]
 
  eDM <- cbind(expand.grid(fSim, fSim)$Var1, expand.grid(fSim,fSim)$Var2)
  simPred <- pCopula(eDM, my_cop)
  simPredMat <- matrix(simPred, 1000, 1000)
  return.level <-  c(1.25,2,5,10,20,50,100,200,500)
  prob.level  <-  T2prob(return.level)
  
  
  # --------plot-----------
  var1 <- names(db)[1]
  var2 <- names(db)[2]
  png(file=paste(dir,mcountry,".png",sep=""))
  par(mfrow=c(1,1))
  par("lwd"=2)
  par("cex"=1)
  plot(sim[,1] ,sim[,2],col="light grey",cex=0.5,xlab=var1,ylab=var2,main=fname_cop)
  points(Cop.vd[,1],Cop.vd[,2],col="red",cex=0.5)
  # contour(s1,d1,simPredMat,levels = prob.level,labels=return.level,xaxs='i',yaxs='i'
  #         ,labcex=0.6,lwd=1,col="black",add=TRUE,method = "flattest", vfont = c("sans serif",
  #                                                                               "plain"))
  contour(qD,qM,simPredMat,levels = prob.level,labels=return.level,xaxs='i',yaxs='i'
          ,labcex=1.5,lwd=1,col="black",add=TRUE)
  legend(min(sim[,1]),max(sim[,2]), c("Simulated","Observed","Return Period"),lwd=c("","",1),col=c(
    "light grey","red","black"),pch=c(15,15,NA),box.col="white",bg="white")
  
  dev.off()
  
}

plotC <- function(db, u_fit, v_fit, my.dis, my_cop, dir, mcountry, fname_cop){
  # get example from 
  # https://stackoverflow.com/questions/50356312/how-to-get-ggplot2-geom-contour-to-replicate-base-graphics-contour
  Nr <- 100
  #  this also can be done
  # Parametrically
  u_d <- univar_f(u_fit, db[,1]) # duration
  v_s <- univar_f(v_fit, db[,2]) # severity
  ff <- as.matrix(cbind(u_d$p_non, v_s$p_non))
  

  # Adding this new part
  # -------compute copula distribution------
  f_cop_non <- pCopula(ff, my_cop) 
  f_cop <- 1-f_cop_non
  rp <- 1/f_cop
  # to plot
  Cop.vd <-  as.matrix(cbind(db[,1],db[,2],f_cop_non,f_cop,rp))
  fSim <- seq(0.05, 0.99998, length.out = Nr)
  sim  <- rMvdc(Nr, my.dis)
  qD <- sim[,1][order(sim[,1])]
  qM <- sim[,2][order(sim[,2])]
  # get quantiles 
  
  if ( u_fit[[1]]$nam.mar  == "gev" ){
    
    d1   <- extRemes::qevd(fSim, u_fit$estimates[1], u_fit$estimates[2], u_fit$estimates[3])
    s1   <- extRemes::qevd(fSim, v_fit$estimates[1], v_fit$estimates[2], u_fit$estimates[3])
    
  }else{
    qq_d <- eval(parse(text=(paste("q",u_fit[[1]]$nam.mar, sep=""))))
    qq_s <- eval(parse(text=(paste("q",v_fit[[1]]$nam.mar, sep=""))))
    
    d1   <- qq_d(fSim, u_fit$estimates[1], u_fit$estimates[2])
    s1   <- qq_s(fSim, v_fit$estimates[1], v_fit$estimates[2])
    
  }
 
  eDM <- cbind(expand.grid(fSim, fSim)$Var1, expand.grid(fSim,fSim)$Var2)
  simPred <- pCopula(eDM, my_cop)
  simPredMat <- matrix(simPred, Nr, Nr)
  simDF <- data.frame(  simD = qD, simM = qM, simPredMat)
  return.level <-  c(1.25,2,5,10,20,50,100,200,500)
  prob.level  <-  T2prob(return.level)
  
  
  # --------plot-----------
  var1 <- names(db)[1]
  var2 <- names(db)[2]
  png(file=paste(dir,mcountry,".png",sep=""))
  par(mfrow=c(1,1))
  par("lwd"=2)
  par("cex"=1)
  plot(sim[,1] ,sim[,2],col="light grey",cex=0.5,xlab=var1,ylab=var2,main=fname_cop)
  points(Cop.vd[,1],Cop.vd[,2],col="red",cex=0.5)
  # contour(s1,d1,simPredMat,levels = prob.level,labels=return.level,xaxs='i',yaxs='i'
  #         ,labcex=0.6,lwd=1,col="black",add=TRUE,method = "flattest", vfont = c("sans serif",
  #                                                                               "plain"))
  contour(d1,s1,simPredMat,levels = prob.level,labels=return.level,xaxs='i',yaxs='i'
           ,labcex=1.5,lwd=1,col="black",add=TRUE)
  legend(min(sim[,1]),max(sim[,2]), c("Simulated","Observed","Return Period"),lwd=c("","",1),col=c(
    "light grey","red","black"),pch=c(15,15,NA),box.col="white",bg="white")
  
  dev.off()
  
}



plot_lines_RP_conditional_SD <- function(dd, u_fit, v_fit, my_cop, mcountry, dout){
  
  qd <- quantile(dd[,1], probs=c(0.10, 0.25, 0.50,0.75,0.90, 0.95))
  qs <- round(quantile(dd[,2], probs=c(0.10, 0.25, 0.50,0.75,0.90, 0.95)),2)

  D <- seq(range(dd[,1])[1], range(dd[,1])[2], length.out = 20)
  S <- seq(range(dd[,2])[1], range(dd[,2])[2], length.out = 20)
  
  l1_dat <- l2_dat <-  list()
  for (i in 1:length(qd)){
    #---------------------------
    # Conditional to duration
    Fu <-  univar_f(u_fit, dat=qd[i])[[1]] #non-exceedances
    Fd <-rep(Fu,length(S))
    Fs <-univar_f(v_fit, dat=S)[[1]]
    # non-exceedances
    Ts <- 1/(1-Fs)
    Td <- 1/(1-Fd)
    C<-pCopula(cbind(Fd,Fs),my_cop)
    #  Shiau 2006
    # Conditional to duration
    Tsc_d <- (1/(1-Fd))*(1/(1-Fd-Fs+C))
    #---------------------------
    # Conditional to Severity
    Fv <-  univar_f(v_fit, dat=qs[i])[[1]] #non-exceedances
    Fss <-rep(Fv,length(D))
    Fdd <-univar_f(u_fit, dat=D)[[1]]
    # non-exceedances
    Tss <- 1/(1-Fss)
    Tdd <- 1/(1-Fdd)
    CC<-pCopula(cbind(Fdd,Fss),my_cop)
    #  Shiau 2006
    # Conditional to duration
    Tdc_s <- (1/(1-Fdd))*(1/(1-Fdd-Fss+CC))
    # P<-(Fs-C)/(1-Fd)
    
    l1_dat[[i]] <- data.frame(Duration=qd[[i]],Severity=S, RP_d=Tsc_d)
    l2_dat[[i]] <- data.frame(Duration=D,Severity=qs[[i]], RP_s=Tdc_s)
  }
  
  new_D <- melt(l1_dat, id=names(l1_dat[[1]]))
  new_D$L1 <- NULL
  new_D$Duration <- as.factor(new_D$Duration)
  #
  new_S <- melt(l2_dat, id=names(l2_dat[[1]]))
  new_S$L1 <- NULL
  new_S$Severity <- as.factor(new_S$Severity)
  pd <- ggplot(new_D, aes(Severity, RP_d, group=Duration, color=Duration, linetype=Duration)) + 
    geom_line(size=1.2) + ylim(0,300) + 
    scale_color_brewer(palette = "Dark2") +
    ylab("Return Period") +
    theme_bw() + 
    theme(legend.position = c(.94, .86))
  ps <- ggplot(new_S, aes(Duration, RP_s, group=Severity, color=Severity,linetype=Severity), size=1.5) + 
    geom_line(size=1.2) + ylim(0,300) + theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    ylab("Return Period") +
    theme_bw() + 
    theme(legend.position = c(.94, .84))
  pp <- pd + ps
  ggsave(pp, file= paste(dout, mcountry, "_RP_lines.png", sep=""), width = 16, height = 6)
}



univarite_AM <- function(dx, type){
  # select annual max/min for severity and duration
  if( type == "high"){
    y_val <- data.frame(dx%>%group_by(year)%>%dplyr::summarise(S=max(S), D=max(D)))
  }else{
    y_val <- data.frame(dx%>%group_by(year)%>%dplyr::summarise(S=min(S), D=max(D)))
  }
  
  if( type == "high"){
    s_fit <- fevd(y_val$S, type = "GEV")
  }else if(type=="low"){
    s_fit <- fevd(-y_val$S, type = "GEV")
  }

  s_loc <- s_fit$results$par[[1]] ;s_scale <- s_fit$results$par[[2]];s_shape <- s_fit$results$par[[3]]
  d_fit <-  fevd(y_val$D, type = "GEV")
  d_loc <- d_fit$results$par[[1]];d_scale <-  d_fit$results$par[[3]];d_shape <-  d_fit$results$par[[3]]
  
  # Apply KS to check the distribution; if pval>0.05, distribution not refused
  gf.d <- gofTest(y_val$D, distribution = "gev",test = "ks")
  gf.s <- gofTest(y_val$S, distribution = "gev",test = "ks")
  rp <- c(5,10,20,50,100,200)
  rlev_s <- return.level(s_fit, rp)
  rlev_d <- return.level(d_fit, rp)
  # If I want to calculate the RP, as the inverse of the probability
  # I will select the 0.95, 0.98, 0.99 events or the top 3 
  top3_s <- sort(y_val$S, decreasing = T)[1:3]
  top3_d <- unique(sort(y_val$D, decreasing = T))[1:3]
  
  rpD_top3 <- 1/(1-pextRemes(d_fit, top3_d))
  rpS_top3 <- 1/(1-pextRemes(s_fit, top3_s))
  
}





namecop <- function(myselectcop){
  
  numfam <- myselectcop$family
  if (numfam == 0 ){
    cop_model  <- indepCopula(dim = 2)
  }else if (numfam == 1 ){
    cop_model  <- normalCopula(dim = 2)
  }else if (numfam == 2 ){
    # cop_model  <- tCopula(dim = 2)
    cop_model <- tCopula(dim = 2, dispstr = "un", df.fixed = TRUE)
  }else if (numfam == 3 ){
    cop_model  <- claytonCopula(dim = 2)
  }else if (numfam == 4 ){
    cop_model  <- gumbelCopula(dim = 2)
  }else if (numfam == 5 ){
    cop_model  <- frankCopula(dim = 2)
  }else if (numfam == 6 ){
    cop_model  <- joeCopula(dim = 2)
  }else if (numfam == 23 | numfam == 13 ){
    cop_model  <- rotCopula(claytonCopula(dim = 2))
  }else if (numfam == 26 | numfam == 36){
    cop_model  <- rotCopula(joeCopula(dim = 2))
  }else if (numfam == 34| numfam == 24 ){
    cop_model  <- rotCopula(gumbelCopula(dim = 2))
  }else if (numfam == 104 | numfam == 204 ){
    cop_model  <- tawnCopula()
  }
  
  return(cop_model)
}

getCop <- function(cop_fit, selectedCopula){
  
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
  }else if(selectedCopula$family==16 | selectedCopula$family==26  | selectedCopula$family==36){
    my_cop <- rotCopula(joeCopula(param=coef(cop_fit)[[1]]))
    # } else if(selectedCopula$family==26){
    # my_cop <- r90JoeBiCopula(param=coef(cop_fit)[[1]])
  } else if(selectedCopula$family==14 | selectedCopula$family==24){
    my_cop <- rotCopula(gumbelCopula(param=coef(cop_fit)[[1]]))
  } else if(selectedCopula$family==104 | selectedCopula$family==204){
    my_cop <- tawnCopula(param=coef(cop_fit)[[1]])
  }
  
  return(my_cop)
}



#######################notes to plot curves given a duration
# 
# 
# D <- 10
# S <-seq(min(dd[,2]), max(dd[,2]),by=0.1)
# Fu <-  univar_f(u_fit, dat=D)[[1]]
# Fd<-rep(Fu,length(S))
# Fs<-univar_f(v_fit, dat=S)[[1]]
# data=cbind(Fd,Fs)
# C<-pCopula(cbind(Fd,Fs),my_cop)
# P<-(Fs-C)/(1-Fd)
# plot(S,P,pch=0,xlab="severity",ylab="probability")
# lines(S,P)
# 
# # Return periods
# plot(S,1/P,pch=0,xlab="Severity",ylab="RP")
# lines(S,1/P)
