pacman::p_load(tidyverse, vars, zoo)

df <- rio::import("lag_data.txt") %>% tibble
df

class(df)
df_st <- as.ts(df)

df %>%
  add_row(ID = 11, Time = as.Date("1948-11-01")) %>%
  mutate(Air_Temperature_lagged = dplyr::lag(Air_Temperature, 1),
         Relative_Humidity_lagged = dplyr::lag(Relative_Humidity, 1)) -> df.withlags
df.withlags

precip.model <- lm(Precipitation ~ Air_Temperature_lagged + Relative_Humidity_lagged, 
                   df.withlags)
anova(precip.model)
precip.model

plot(predict(precip.model, newdata = df.withlags))
