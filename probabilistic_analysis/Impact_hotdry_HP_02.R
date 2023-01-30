# Impact hot-dry-HP 
# Copula approach
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

#load data
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


####################
# Copula analysis
###################
# ---------------------------------------------
# Analysis with reconstructed hydro-generation
mvars <- c("pred","spei_3","STI_1") # try tmax

build_3cop <- function(df_case_mon, mvars, th=0.2, u_spei, u_sti ){
  # Function to fit tri-variate copula
  #' @df_case_mon: monthly data for one power plant
  #' @mvars: names of the variables (pred, spei_3, STI_1)
  #' @th: threshold for the hydropower
  #' @u_spei: threshold for spei
  #' @u_sti: threshold for sti
  
  dd <- data.frame(df_case_mon[,mvars])
  # remove na
  dd <- na.omit(dd)
  # check dependence between variables
  
  # transform to univariate marginals
  U <- pobs(dd)
  pairs(as.copuladata(U))
  # parametric marginals
  
  # for generation I will use gamma
  
  m1 <-  myfits(dd[,1], "gamma", NULL)
  m2 <-  myfits(dd[,2], "none", NULL)
  m3 <-  myfits(dd[,3], "none", NULL)
  
  param1   <- m1[[2]] 
  param2   <- m2[[2]] 
  param3  <- m3[[2]]
  
  # parametric marginals
  f1 <- m1[[3]]
  f2 <- m2[[3]]
  f3 <- m3[[3]]
  
  # keep margins information
  info_mar <- rbind.data.frame("marg1"=m1[[1]],"marg2"=m2[[1]], "marg3" = m3[[1]])
  
  ## A trivariate normal copula
  cop_model <- normalCopula( dim = 3, dispstr = "un")
  cop_fit   <-  fitCopula(cop_model, U, method = 'ml')
  my_cop     <- normalCopula(param=coef(cop_fit), dim=3, dispstr = "un")
  
  
  check <- gofCopula(cop_model, U,simulation = "mult", estim.method = "mpl")
  cat("the pvalue for the copula is:",check$p.value)
  if(check$p.value<0.05){cat("not a good fit")}
  
  df_cop <- data.frame( "param" = my_cop@parameters, "check"=check$p.value)
  
  Nr <- 10000
  sim <-  rCopula(Nr, my_cop) # sample for the selected copula
  par(mfrow=c(1,3))
  plot(sim[,1],sim[,2],col='blue', main = "Gaussian",xlab = "u1", ylab = "u2")
  plot(sim[,2],sim[,3],col='red', main = "Gaussian",xlab = "u2", ylab = "u3")
  plot(sim[,1],sim[,3],col='grey', main = "Gaussian",xlab = "u1", ylab = "u3")
  cat("correlation sample", "\n")
  cor(sim,method='spearman') # 
  cat("correlation my data","\n")
  cor(dd, method= "spearman", use = "complete")
  
  # simulate distribution based on marginals
  my.dis <- mvdc(my_cop, margins = c(info_mar$nam.mar[1], info_mar$nam.mar[2],info_mar$nam.mar[3] ),
                 paramMargins = list(list(param1), list(param2), list(param3)))
  my.sim <- rMvdc(Nr, my.dis)
  # ----------define thresholds--------
  
  
  trsh_u1 = quantile(U[,1], probs = th)
  #trsh_u1 <- 0
  # Fixed threshold---comparison RF

  
  tf1 <- quantile(dd[,1],0.2)
  mp  <- m_sim <- m_emp <- m_cond <-     matrix(0, nrow = length(u_spei), ncol = length(u_spei))
  
  rownames(mp) <- rownames(m_cond) <-  rownames(m_emp) <- u_spei
  colnames(mp) <- colnames(m_cond) <-  rownames(m_emp) <- u_sti
  
  for(i in 1:(length(u_spei))){
    for(j in 1:(length(u_sti))){
      
      Fs <-  univar_f(m2, dat=u_spei[i])[[1]]
      Ft <- univar_f(m3,  dat=u_sti[j])[[1]]
      cat ("probability for", u_spei[i],":",Fs,"and",u_sti[j],"Ft", Ft,"\n")
      
      if( u_spei[i] < 0 &  u_sti[j] > 0){
        cat("hot-dry","\n")
        # P(X1<x1, X2>x2, X3<x3) =  P(X1<x1, X3<x3) - P(X1<x1, X2<x2, X3<x3) = C13-C123
        mp[i,j] <- pCopula(c(0.2,Fs,Inf), copula=my_cop) - pCopula(c(0.2,Fs,Ft), copula = my_cop)
        m_sim[i,j] <- mean(my.sim[,1]< tf1 & my.sim[,2]< (Fs) & my.sim[,3]> (Ft))
        # P( X1 < x1| X2<x2, X3>x3) = P(X1<x1,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3) /P(X2<x2)- P(X2<x2, X3<x3)
        m_cond[i,j] <- (pCopula(c(0.2,Fs,Inf), copula=my_cop) - pCopula(c(0.2,Fs,Ft), copula = my_cop))/(Fs-pCopula(c(-Inf,Fs,Ft), copula = my_cop))
        
        
      }else if(u_spei[i] > 0 &  u_sti[j] < 0){
        cat("cold-wet","\n")
        # P(X1>x1, X2<x2, X3<x3) = P(X3<x3, X2<x2) - P( X1 < x1, X2<x2, X3<x3) ????
        # Not working...
        # mp[i,j] <- pCopula(c(0.2, Inf, Ft), copula=my_cop) - pCopula(c(0.2,Fs,Ft), copula = my_cop)
        mp[i,j] <- pCopula(c(0.2, -Inf, Ft), copula=my_cop) - pCopula(c(0.2,Fs,Ft), copula = my_cop)
        m_sim[i,j] <- mean(my.sim[,1]< tf1 & my.sim[,2] > (Fs) & my.sim[,3] < (Ft))
        # P( X1 < x1| X2<x2, X3>x3) = P(X1<x1,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3) /P(X2<x2)- P(X2<x2, X3<x3)
        m_cond[i,j] <- (pCopula(c(0.2,-Inf, Ft), copula=my_cop) - pCopula(c(0.2,Fs,Ft), copula = my_cop))/(Ft-pCopula(c(-Inf,Fs,Ft), copula = my_cop))
        
      }else if(u_spei[i] < 0 &u_sti[j] < 0){
        cat("cold-dry","\n")
        mp[i,j]    <- pCopula(c(0.2,Fs,Ft), copula = my_cop)
        m_sim[i,j] <- mean(my.sim[,1]< tf1 & my.sim[,2] < (Fs) & my.sim[,3] < (Ft))
        # P( X1 < x1| X2<x2, X3>x3) = P(X1<x1,X2<x2 ) - P( X1 < x1, X2<x2, X3<x3) /P(X2<x2)- P(X2<x2, X3<x3)
        m_cond[i,j] <- 1-pCopula(c(0.2,Fs,Ft), copula = my_cop)
        
      }else if(u_spei[i] > 0 &u_sti[j] > 0){
        cat("wet-hot","\n")
        # P(X1>x1, X2>x2, X3<x3) = P(X3<x3) - P(X3<x3, X1<x1) - P(X3<x3, X2<x2) + P( X1 < x1, X2<x2, X3<x3)
        mp[i,j]    <- 0.2 - pCopula(c(0.2, -Inf, Ft), copula=my_cop) - pCopula(c(0.2, Fs, Inf), copula=my_cop) + pCopula(c(0.2,Fs,Ft), copula = my_cop)
        m_sim[i,j] <- mean(my.sim[,1]< tf1 & my.sim[,2] > (Fs) & my.sim[,3] > (Ft))
       #  need to check this
        m_cond[i,j] <- 1-pCopula(c(0.2,Fs,Ft), copula = my_cop)
        
      }
      
      
      # m_emp[i,j] <- sum(dd[,1]<tf1 & dd[,2] < (Fs) & dd[,3] > (Ft)) /(length(dd[,1]))
    }
  }
  
  
  return(list("mat_and" = mp, "mat_or" = m_cond, "check" = df_cop))
  

  
}


# Apply the cop. for each power plant
out <- list()
for(i in 1:length(r_mon_data)){
  df_case_mon <- r_mon_data[[i]]
  u_spei <- seq(-2.0, 2, 0.5)
  u_sti <- seq(-2, 2.0, 0.5)
  out[[i]] <- build_3cop(df_case_mon, mvars, th=0.2, u_spei, u_sti)
}

mat1 <- lapply(out, function(x) x$mat_and)
mat2 <- lapply(out, function(x) x$mat_or)


pm1 <- lapply(1:length(mat1), function(i) plot_heatmaps(mat1[[i]]))
pm2 <- lapply(1:length(mat2), function(i) plot_heatmaps(mat2[[i]]))

plot_grid(plotlist = pm1, ncol=3)
plot_grid(plotlist = pm2, ncol=3)










# Plot

dff_mat <- reshape2::melt(m_sim)
names(dff_mat) <- c("SPEI","STI","Prob")
ggplot2::ggplot(dff_mat, aes(x=SPEI, y=STI, fill=Prob)) + geom_tile() +  
  geom_text(aes(label = round(Prob, 3))) +
  scale_fill_gradient(low = "white", high = "red")




#-----
#P = C(u,vv)- C(u,vv,zz)
mm2 = matrix(0, nrow = length(probs), ncol = length(probs))
t2 <- probs
t3 <- seq(0.5,0.95,0.05)
rownames(mm2) <- t2
colnames(mm2) <- t3
for(k in 1:(length(t2))){
  for(w in 1:(length(t3))){
    mm2[k,w] <-  pCopula(c(0.1,t2[k],Inf), copula=my_cop) - pCopula(c(0.1,t2[k],t3[w]), copula = my_cop)
  }
}


dff_mat <- reshape2::melt(mm2)
names(dff_mat) <- c("SPEI","STI","Prob")
ggplot2::ggplot(dff_mat, aes(x=SPEI, y=STI, fill=Prob)) + geom_tile() +  scale_fill_fermenter(n.breaks = 9, palette = "Oranges")



# ---test--
tf1 <- quantile(dd[,1],0.2)
tf2 <- quantile(dd[,2],0.1)
tf3 <- quantile(dd[,3],0.9)

mean(dd[,1]<tf1 & dd[,2]<tf2 & dd[,3]>tf3)
mean(my.sim[,1]<tf1 & my.sim[,2]<tf2 & my.sim[,3]>tf3)



probs <- sort(seq(0.05,0.5,0.05),decreasing=T)
trsh_u2 = quantile(U[,2], probs = sort(seq(0.05,0.5,0.05),decreasing=T)) # precipiation
trsh_u3 = quantile(U[,3], probs = seq(0.5,1,0.05)) # temperature


mm = matrix(0, nrow = length(probs), ncol = length(probs))


for(k in 1:(length(probs))){
  for(w in 1:(length(probs))){
    idx = which(sim[,2]<= trsh_u2[k] & sim[,3]<=trsh_u3[w])
    
    x = sim[idx,1]
    ecdf_f = ecdf(x)
    mm[k,w] = ecdf_f(trsh_u1)
    
  }}

require(fields)
image(mm,xaxt="n",yaxt="n")



