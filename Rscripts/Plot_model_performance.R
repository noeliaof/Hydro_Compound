## Plot score models####
setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

df_classical <- read.csv("../../Results/Hydro_project/Analysis_Updates_Nov22/ClassicalModels_2predictors/ClassicalM_table_perf.csv")
df_classical$X <- NULL
df_nn <- read.csv("../../Results/Hydro_project/Analysis_Updates_Nov22/LSTM_2predictors/LSTM_PREVAH_table_perf_PREVAH.csv")
df_nn$X <- NULL
df_all <- rbind(df_classical[,names(df_nn)], df_nn)

df_all <- df_all%>%dplyr::filter(model != "randomforest")
df_all$model[df_all$model=="LSTM_v1"] <- "LSTM"
df_all$model[df_all$model=="ann1"] <- "ANN"
df_all$model[df_all$model=="linear"] <- "MLR"
df_all$model[df_all$model=="rf_Predefinesplit"] <- "RF"

# plot scores

df_train <- df_all[,c("station", "model", "train_rmse", "train_mae", "cor_train")]
names(df_train) <- c("station", "model", "RMSE", "MAE", "COR")
df_test <- df_all[,c("station", "model", "test_rmse", "test_mae", "cor_test")]
names(df_test) <- c("station", "model", "RMSE", "MAE", "COR")
df_train <- df_train%>%pivot_longer(c(RMSE, MAE, COR), names_to = "metric", values_to = "value")
df_test <- df_test%>%pivot_longer(c(RMSE, MAE, COR), names_to = "metric", values_to = "value")

dff <- reshape2::melt(list("Train" = df_train, "Test" = df_test), id=names(df_test))
names(dff) <- c(names(df_train), "data")
dff$station[dff$station == "Kraftwerke Mauvoisin AG"] <- "Mauvoisin AG"
dff$station[dff$station == "KW Rheinfelden CH"] <- "Rheinfelden CH"

dff$data <- as.factor(dff$data)
dff$data <- factor(dff$data, levels=c("Train", "Test"))
m1 <- dff%>%dplyr::filter(metric!="COR")%>%
  ggplot2::ggplot(aes(x=metric, y = value, fill= model)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_grid(~data~station) +
  xlab("") + ylab("") + scale_fill_brewer(palette= "Blues", name="") + theme_minimal_hgrid()
m2 <- dff%>%dplyr::filter(metric=="COR")%>%
  ggplot2::ggplot(aes(x=metric, y = value, fill= model)) + 
  geom_bar(stat="identity", position = "dodge") + 
  facet_grid(~data~station) +
  xlab("") + ylab("") + scale_fill_brewer(palette= "Blues", name="") +theme_minimal_hgrid()

mp <- ggarrange(m1, m2, ncol=1)

ggsave(mp, file="../../Results/Hydro_project/Analysis_Updates_Nov22/plot_metric_model.png", width = 12, height = 8)
