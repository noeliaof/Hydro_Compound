###############
# Model set-up:
# I define some terms that I need to run the model
# Including the list with the models to check
# the full (starting model) includes the predictors to capture the main physical processes
# related to the O3 formation: 
# f1(VPD, O3t-1) to caputure dry deposition
# f2(lag24 (MDA8), BLH): to capture mixing processes within the BLH
# f3(tas, nox)
# I need to check the interactions f1, f2, are they linear? or not..?

# st.types <- c("rural","urban","suburban")
seas       <- "JAS"
# inputY <- "value"
inputY   <- "DO3"
# Fixed threshol
namfil <- "sunmax"
thr_ws  <- 3.2
# maindir <- paste("../fit_GAM/sunmax/plots_fixed_ws/", "O3","/", sep="")
# define models
# assuming in principle 3 non-linear relationship among predictors
# f1, f2, and f3
# But need to test what happens in linear cases (adding variables individually)
# Let's define the covariates (also including interaction)
# add "s" into VPD and blh_pct
v_param   <- c("lag","s(VPD)","lag:VPD","lag24","s(blh_pct)","lag24:blh_pct")
vnl_param <- c("te(lag,VPD)","te(lag24,blh_pct)")
# another try, less possibilities
basic_form  <- as.formula(DO3 ~ te(tas,log(nox+1)))
#with by=period
basic_form  <- as.formula(DO3 ~ period + te(tas,log(nox+1), by=period))

final_form     <- as.formula(DO3 ~ te(tas,log(nox+1))+ te(lag,VPD) + te(lag24,blh_pct))
final_form_s   <- as.formula(DO3 ~ te(tas,log(nox+1))+ s(VPD) + s(blh_pct))
# period_form  <- as.formula(DO3 ~ te(tas,log(nox+1))+ te(lag,VPD) + te(lag24,blh_pct) + period)
period_form    <- as.formula(DO3 ~ period + te(tas,log(nox+1),by=period)+ te(lag,VPD) + te(lag24,blh_pct))
period_form_s  <- as.formula(DO3 ~ period + te(tas,log(nox+1),by=period)+ s(lag) + s(VPD) + s(lag24) + s(blh_pct))

period_form_O3    <- as.formula(value ~ lag + period + te(tas,log(nox+1), by=period)+ te(lag,VPD) + te(lag24,Dblh))


ytrend_form_sy <- as.formula(DO3 ~ s(year) + te(tas,log(nox+1), year) + te(lag,VPD) + te(lag24,blh_pct))
ytrend_form    <- as.formula(DO3 ~ te(tas,log(nox+1), year) + te(lag,VPD) + te(lag24,blh_pct))
# ytrend_form <- as.formula(DO3 ~  te(tas,log(nox+1),by= year) + te(lag,VPD) + te(lag24,blh_pct))

period_form_dec <- as.formula(DO3 ~ period + ti(tas) + ti(log(nox + 1)) + ti(tas, log(nox + 1), by = period) + 
                                ti(lag) + ti(VPD) + ti( VPD, lag) + ti(lag24) + ti(blh_pct) + ti(blh_pct, lag24))

final_form_dec <- as.formula(DO3 ~ ti(tas) + ti(log(nox + 1)) + ti(tas, log(nox + 1)) + 
                               ti(lag) + ti(VPD) + ti(VPD,lag) + ti(lag24) + ti(blh_pct) + ti( blh_pct, lag24))


test_form   <-  as.formula(value ~ lag + te(tas,log(nox+1))+ te(lag,VPD) + te(lag24,blh_pct))

s_param     <- c("s(lag)","s(VPD)","s(lag24)","s(blh_pct)")
int_param   <- c("te(lag,VPD)","te(lag24,blh_pct)")
int_param2   <- c("te(lag,VPD)","te(lag24,blh_pct)","te(lag24,VPD)","te(lag,blh_pct)")
# dirout
# Dirouts     <- list("rural"="../plot_final_GAM/rural/",
#                   "urban"="../plot_final_GAM/urban/",
#                 "suburban"="../plot_final_GAM/suburban/")


dout <- list("rural"="../out_GAM_v2/Byperiods/plot_codes/rural/",
             "suburban"="../out_GAM_v2/Byperiods/plot_codes/suburban/",
             "urban"="../out_GAM_v2/Byperiods/plot_codes/urban/")


# for plot
leg <-  expression(paste(Delta, "O3 (ugrm-3/h)"))