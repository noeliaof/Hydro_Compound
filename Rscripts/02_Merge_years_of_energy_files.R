# Prepare energy entsoe generation long-time series
# Note: I needed to process separately years because the format slightly changed!!!!!!!!

setwd("~/Documents/OCCR/scripts/R")
source("./config/ConfigureEnv.R")
ConfigureEnv();

# Daily
file_2015_2020 <- load("../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2015_2020.Rda")
day_2015_2020 <- get(file_2015_2020)
file_2021 <- load("../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2021.Rda")
day_2021 <- get(file_2021)


load("../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2021_2.Rda")
# check dates
day_2021%>%group_by(name_p, ProductionType)%>%dplyr::summarise(n())

# Hourly
fileH_2015_2020 <- load("../../data/Output_data/entsoe/entsoe/hourly_CH_enerationunit_eic_entsoe_2015_2020.Rda")
hour_2015_2020 <- get(fileH_2015_2020)
fileH_2021 <- load("../../data/Output_data/entsoe/entsoe/hourly_CH_enerationunit_eic_entsoe_2021.Rda")
hour_2021 <- get(fileH_2021)


names(day_2021)[names(day_2021)=="ProductionType"] <- "ProductionTypeName"
names(hour_2021)[names(hour_2021)=="ProductionType"] <- "ProductionTypeName"

# Merge years
day_generation_2015_2021 <- rbind(day_2015_2020, day_2021)

# Hourly
cnames1 <- names(hour_2015_2020)
cnames2 <- names(hour_2021)
cnames <- cnames1[cnames1%in%cnames2]
hour_2015_2020 <- hour_2015_2020%>%dplyr::select
hour_generation_2015_2021 <- rbind(hour_2015_2020[,cnames], hour_2021[,cnames])

hour_generation_2015_2021 <- hour_generation_2015_2021[order(hour_generation_2015_2021$PowerSystemResourceName,hour_generation_2015_2021$DateTime),]

save(day_generation_2015_2021, file="../../data/Output_data/entsoe/entsoe/daily_generationunit_eic_entsoe_2015_2021.Rda")
save(hour_generation_2015_2021, file="../../data/Output_data/entsoe/entsoe/hourly_CH_enerationunit_eic_entsoe_2015_2021.Rda")
