## Exploratory analysis to see how proceed with the compound analysis
# load and prepare env.
# Impact hot-dry HP 
# Meta-Gaussian approach
setwd("~/Documents/OCCR/Hydro_Compound")
source("config/ConfigureEnv.R")
ConfigureEnv();

#load reconstructed data
# Daily
dir_recons <- '/Users/noeliaotero/Documents/OCCR/Hydro_Compound/data/Classical/' 
l_files <- list.files(dir_recons, pattern = "csv")
station_name <- tools::file_path_sans_ext(l_files)
r_data <- lapply(1:length(l_files), function(i) read.csv(paste(dir_recons, l_files[i], sep="")))
names(r_data) <- station_name

## read monthly
dir_month_recons <- '/Users/noeliaotero/Documents/OCCR/Hydro_Compound/data/Classical/monthly/'
l_mon_files <- list.files(dir_month_recons, pattern = "csv")
r_mon_data <- lapply(1:length(l_mon_files), function(i) read.csv(paste(dir_month_recons, l_mon_files[i], sep="")))
names(r_mon_data) <- station_name

# I will skip the station Gde-Dixence (GD), bad data (I should remove it from the ML) r_mon_data[[5]]


#############################################
#-----------Meta-Gaussian model-------------
############################################
# adapted from Hao et al.
# Inputs---Montly scale
# specify input variables


NQT <- function(X){
  # NQT transformation
  n  <- length(X)
  EP <-ecdf(X)
  P <- EP(X)*n/(n+1)
  SI <- qnorm(P,0,1)
  return(SI)
}


meta_gaussian_model <- function(dat, u0, ug, impact_var){
  library(mvtnorm)
  # adapted from Hao et al. 2018
  # CRAN CompoundEvents

  num_vars <- length(dat)
  n <-  length(dat[,num_vars[1]])
  Nr <- 10000 # number of samples
  v_names <- names(dat)
  # Transform all variables into standardized variables
  
  # for HP
  X_nqt <- array(NA, dim=c(n,num_vars))
  for ( i in 1:num_vars){
    cat(i,"\n")
    X_nqt[,i] <- NQT(dat[,i])
  }
 
  colnames(X_nqt) <- v_names
  # Build the meta-Gaussian model
  # define the covariance based on the impact variable

  if ( num_vars == 2){
    cat("meta-G for two variables")
    xd <- cbind(X_nqt[,1])
    yd <- cbind(X_nqt[,2])
    colnames(xd) <- names(dat)[1]
    colnames(yd) <- names(dat)[2]
    cv<-cov(cbind(xd,yd))
    
    my=mean(yd)
    
    sig12 <-cv[1,2];
    mx <- mean(xd)
    mu <- my+sig12%*%solve(cov(xd))%*%t((u0 - as.vector(mx)))
    sig<-cv[num_vars,num_vars]-sig12%*%solve(cov(xd))%*%t(sig12)
    
    #simulate 
    X <- dat[,1]
    n  <- length(X)
    EP <-ecdf(X)
    P <- EP(X)*n/(n+1)
    res_pdf <- qnorm(P, mean = mu, sd=sig)
    orig_pdf <- qnorm(P, mean = my, sd=1)
    
    df_pdf <- data.frame("PDF"=orig_pdf, "C_PDF"=res_pdf)
    
    sim <- mvrnorm(n = Nr, c(mx, mu), cv)
    # x <- rmvnorm(1000, sigma = cv)
  
  }else{
    
    xd <- cbind(X_nqt[,which(v_names!=impact_var)])
    yd <- cbind(X_nqt[,which(v_names==impact_var)])
    colnames(xd) <- v_names[v_names!=impact_var]
    colnames(yd) <- impact_var
    cv<-cov(cbind(xd,yd))
    
    my=mean(yd)
    
    sig12<-cv[1:num_vars-1,num_vars];
    sig21<-cv[num_vars,1:num_vars-1];
    
    sig12=matrix(data=sig12, nrow=2,ncol = 1)
    sig21=matrix(data=sig21, nrow=1,ncol = 2)
    mx=cbind(mean(xd[,1]),mean(xd[,2]))
    
    # Get the conditional mean and variance based on the conditions -u0
    mu<-my+sig21%*%solve(cov(xd))%*%t((u0-mx))
    sig<-cv[num_vars,num_vars]-sig21%*%solve(cov(xd))%*%t(sig21)
    

    X <- dat[,impact_var]
    n  <- length(X)
    EP <-ecdf(X)
    P <- EP(X)*n/(n+1)
    res_pdf <- qnorm(P, mean = mu, sd=sig)
    orig_pdf <- qnorm(P, mean = my, sd=1)
    df_pdf <- data.frame("PDF"=orig_pdf, "C_PDF"=res_pdf)
    
    #simulate 
    sim <- mvrnorm(n = Nr, c(mx, my), cv)
    
    # pCopula(c(0.1,t2[k],Inf), copula=my_cop) - pCopula(c(0.1,t2[k],t3[w]), copula = my_cop)
    # Joint prob P( Generation < 0, SPI<u1, STI>u2) = P( X1 < x1, X2<x2, X3>x3) = P(X1<x1,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3)
    # P( X1 < x1, X2<x2, X3<x3):
    if ( !is.null(ug)){
      
      co  <- cor(cbind(xd,yd))
      co_SPI <- cor(cbind(xd[,1],yd))
      co_STI <- cor(cbind(xd[,2],yd))
      co12 <- cor(xd)
      
      # check if it's hot-dry/ wet-cold
      if( u0[1] <= 0 &  u0[2] > 0){
        cat("hot-dry","\n")
        # P( X1 < x1, X2>x2, X3<x3) = P(X1<x1,X3<x3 )-  P(X1<x1,X2<x2, X3<x3 )
        # NOT mu , use my
        p_13  <- pmvnorm(mean=c(mx[,1], my), co_SPI, lower=rep(-Inf, 2), upper=c(u0[1], ug))[1]
        p_123 <- pmvnorm(mean=c(mx, my), co, lower=rep(-Inf, num_vars), upper=c(u0, ug))[1]
        
        Pr <- p_13 - p_123
        # compare with simulated
        # mean(sim[,1]<u0[1] & sim[,2]>u0[2] & sim[,3]<0)
        # conditional 
        # P( X1 < x1| X2<x2, X3>x3) = P(X1<x1,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3) /P(X2<x2)- P(X2<x2, X3<x3)
        p_3 = pnorm(u0[1], mx[,1], 1)
        p_4 =  pmvnorm(mean=c(mx[,1], mx[,2]), co12, lower=rep(-Inf, 2), upper=c(u0))[1]
        P_or <- (p_13 -p_123)/(p_3 -p_4)
        
      }else if (u0[1] > 0 &  u0[2] <= 0){
        cat("cold-wet","\n")
        # P(X1>x1, X2<x2, X3<x3 ) = P(X2<x2, X3<x3) - P( X1 < x1, X2<x2, X3<x3)
        p_23  <- pmvnorm(mean=c(mx[,2], my), co_STI, lower=rep(-Inf, 2), upper=c(u0[2], ug))[1]
        p_123 <- pmvnorm(mean=c(mx, my), co, lower=rep(-Inf, num_vars), upper=c(u0, ug))[1]
        Pr <- p_23 - p_123
        # compare with simulated
        # mean(sim[,1]<u0[1] & sim[,2]>u0[2] & sim[,3]<0)
        # conditional 
        # P( X3 < x3| X2<x2, X1>x1) = P(X3<x3,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3) /P(X2<x2)- P(X2<x2, X3<x3)
        p_3 = pnorm(u0[2], mx[,2], 1)
        p_4 =  pmvnorm(mean=c(mx[,1], mx[,2]), co12, lower=rep(-Inf, 2), upper=c(u0))[1]
        P_or <- (p_23 - p_123)/(p_3 - p_4)
        
      }else if(u0[1] >=0 & u0[2] >=0){
        cat("wet-hot")
        # P(X1>x1, X2>x2, X3<x3 ) = P(X3<x3)- P(X2<x2, X3<x3) - P(X2<x2, X3<x3) + P( X1 < x1, X2<x2, X3<x3)
        p_0 <-  pnorm(0, mu, 1) #P(X3<x3)
        p_13 <- pmvnorm(mean=c(mx[,1], my), co_SPI, lower=rep(-Inf, 2), upper=c(u0[1], ug))[1]
        p_23 <- pmvnorm(mean=c(mx[,2], my), co_STI, lower=rep(-Inf, 2), upper=c(u0[2], ug))[1]
        p_123 <- pmvnorm(mean=c(mx, my), co, lower=rep(-Inf, num_vars), upper=c(u0, ug))[1]
        Pr <- p_0 - p_13 - p_23 + p_123 
        # compare with simulated
        # mean(sim[,1]<u0[1] & sim[,2]>u0[2] & sim[,3]<0)
        # conditional 
        # P( X1 < x1| X2>x2, X3>x3) = P(X1<x1,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3) /P(X2<x2)- P(X2<x2, X3<x3)
        p_3 = pnorm(u0[1], mx[,1], 1)
        p_4 =  pnorm(u0[2], mx[,2], 1)
        
        P_or <- Pr/(p_3+p_4-pmvnorm(mean=c(mx[,1], mx[,2]), co12, lower=rep(-Inf, 2), upper=c(u0))[1])
        
      }else if(u0[1] <= 0 & u0[2] <= 0){
        
        cat("cold-dry","\n")
        p_123 <- pmvnorm(mean=c(mx, my), co, lower=rep(-Inf, num_vars), upper=c(u0, ug))[1]
        Pr <- p_123
        P_or <- 1 -p_123
      
      }
     # Check the P-or! it is not well calculated
      
      table_pb <- data.frame("P_and"=Pr, "P_or" = P_or)
      
    }
    
  }
 

  res<-cbind(mu,sig)
 
  return(list("params"=res, "pdf"=df_pdf, "sim" = sim, "table_pb" = table_pb))

}

############### Start the analysis 
# define the values for SPEI and STI
u_spei <- seq(-2.0, 2, 0.5)
u_sti <- seq(-2.0, 2.0, 0.5)
# Pred: hydro-generation (predictions)
# SPEI_3: precipiation
# STI_1: temperature

i_vars <- c("pred","spei_3","STI_1")

main_metaG <- function(df_case_mon, i_vars, u_spei, u_sti){
  # Main function to call the meta-gaussian model for each powerplant
  dat_i <- data.frame(df_case_mon[,i_vars])
  # remove NAN
  dat_i <- na.omit(dat_i)
  mat_prob = matrix(0, nrow = length(u_spei), ncol = length(u_sti))
  rownames(mat_prob) <- u_spei
  colnames(mat_prob) <- u_sti
  mat_prob_cond <- mat_prob
  l_pdf <- name_l <- l_params <-  list()
  k <- 0
  for( i in 1:length(u_spei)){
    for(j in 1:length(u_sti)){
      k <- k + 1
      cat('model for spei',u_spei[i],"\n")
      cat('model for sti',u_sti[j],"\n")
      out <- meta_gaussian_model(dat_i, c(u_spei[i], u_sti[j]), ug=0 , impact_var = "pred")
      mat_prob[i,j] <- out$table_pb[,1]
      mat_prob_cond[i,j] <- out$table_pb[,2]
      l_pdf[[k]] <- out$pdf
      l_params[[k]] <- setNames(data.frame(out$params),c("mu","sig"))
      name_l[[k]] <- paste(u_spei[i],"/",u_sti[j], sep="")
    }
  }
  
  names(l_pdf) <- names(l_params) <- name_l
  return(list("mat_and" = mat_prob, "mat_or" = mat_prob_cond, "l_pdf" = l_pdf,"l_params" = l_params))
  
}

## Run the main script for each power plant
out <- list()
for(i in 1:length(r_mon_data)){
  cat(i,"\n")
  out[[i]] <-  main_metaG(r_mon_data[[i]], i_vars, u_spei, u_sti)
}
names(out) <- names(r_mon_data)

# Extract data: mat1(and) and mat2(or)
mat1 <- lapply(out, function(x) x$mat_and)
mat2 <- lapply(out, function(x) x$mat_or)



## Plot the restults



pm1 <- lapply(1:length(mat1), function(i) plot_heatmaps(mat1[[i]]))
pm2 <- lapply(1:length(mat2), function(i) plot_heatmaps(mat2[[i]]))

plot_grid(plotlist = pm1, ncol=3)
plot_grid(plotlist = pm2, ncol=3)

########################
# plot PDF
# extract params
#######################
l_params <- lapply(out, function(x) x$l_params)
df_params <- setNames(reshape2::melt(l_params, id=c("mu","sig")), c("mu","sig","compound","station"))

l_pdfs <- lapply(out, function(x) x$l_pdf)
l_dff <- lapply(l_pdfs, function(x) setNames(reshape2::melt(x, id=names(x[[1]])), c(names(x[[1]]),"compound")))
all_dff <- reshape2::melt(l_dff, id=names(l_dff[[1]]))
names(all_dff) <- c(names(l_dff[[1]]), "station")
all_dff <- merge(all_dff, df_params, by=c("compound","station"))

types <- unique(all_dff$compound)
# example
i_type <- c("-1.5/1.5","-0.5/0.5","1.5/-1.5")
all_dff%>%dplyr::filter(compound%in%i_type)%>%
  ggplot() + geom_density(aes(C_PDF, color=compound, fill=compound), alpha=0.1)+geom_density(aes(PDF), linetype="dashed")  + 
  geom_vline(aes(xintercept=mu, color=compound), linetype="dashed", size=0.3) + 
  facet_wrap(~station, ncol=3) + theme_bw()


