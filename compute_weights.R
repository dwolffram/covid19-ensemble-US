setwd("covid19-ensemble-US")

source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/data_loading.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/scoring.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_methods.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_functions.R")

library(doParallel)

add_truth <- function(df){
  target_dict = list("inc case" = "Incident Cases",
                     "inc death" = "Incident Deaths",
                     "cum death" = "Cumulative Deaths")
  
  df$merge_target <- str_sub(df$target, start=12)
  targets <- unique(df$merge_target)
  
  truth_df <- data.frame()
  
  for (target in targets){
    truth <- load_truth(target_dict[[target]]) %>%
      rename(truth = value) %>%
      mutate(merge_target = target)
    
    truth_df <- bind_rows(truth_df, truth)
  }
  
  df <- df %>%
    left_join(truth_df, by=c("merge_target"="merge_target", "target_end_date"="date", "location"="location")) %>% 
    select(- merge_target)
  
  return(df)
}


forecast_dates <- substr(list.files("data/", "df_train"), 10, 19)

df = data.frame()

dplyr.summarize.inform = FALSE

for (forecast_date in forecast_dates[1:2]){
  print(forecast_date)
  
  df_train <- read_csv(paste0("data/df_train_", forecast_date, ".csv"), col_types = cols(
                forecast_date = col_date(format = ""),
                target = col_character(),
                target_end_date = col_date(format = ""),
                location = col_character(),
                type = col_character(),
                quantile = col_double(),
                value = col_double()
              )) %>%
              filter(location != 'US') %>%
              as.data.frame()
  
  df_train <- add_truth(df_train)
  
  
  for (t in unique(df_train$target)){
    
    print(t)
    train <- df_train %>%
      filter(target == t)
    
    
    p <- v3_iter_fit(train)
    print(p)
    
    temp <- data.frame(p)
    temp$target <- t
    temp$forecast_date <- forecast_date

    df <- bind_rows(df, temp)
  }
}



no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  



df_all <- foreach(forecast_date=forecast_dates[1:2], .combine=rbind, .packages=c("tidyverse")) %dopar% {
  print(forecast_date)
  
  df_train <- read_csv(paste0("data/df_train_", forecast_date, ".csv"), col_types = cols(
    forecast_date = col_date(format = ""),
    target = col_character(),
    target_end_date = col_date(format = ""),
    location = col_character(),
    type = col_character(),
    quantile = col_double(),
    value = col_double()
  )) %>%
    filter(location != 'US') %>%
    as.data.frame()
  
  df_train <- add_truth(df_train)
  
  df <- data.frame()
  
  for (t in unique(df_train$target)){
    
    print(t)
    train <- df_train %>%
      filter(target == t)
    
    p <- v3_iter_fit(train)
    print(p)
    
    temp <- data.frame(p)
    temp$target <- t
    
    df <- bind_rows(df, temp)
  }
  df$forecast_date <- forecast_date
  df
}



















df_ensemble <- df_ensemble %>%
  select(-truth)




file_name <- paste0("data/df_pred_", forecast_date, ".csv")
file_name <- paste0("data/df_pred_US_", forecast_date, ".csv")

write.csv(df_ensemble, file_name, row.names=FALSE)


# PLOT
Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_ALL", "C")

df <- read_csv(paste0("data/df_pred_", forecast_date, ".csv"))
df2 <- read_csv(paste0("data/df_pred_US_", forecast_date, ".csv"))
df <- bind_rows(df, df2)

df <- read_csv("data/ensemble_data/submissions/2021-04-26-KITmetricslab-select_ensemble.csv")
df <- read_csv("data/ensemble_data/submissions/2021-05-03-KITmetricslab-select_ensemble.csv")
df <- read_csv("data/ensemble_data/submissions/2021-05-10-KITmetricslab-select_ensemble.csv")
df <- read_csv("data/ensemble_data/submissions/2021-05-17-KITmetricslab-select_ensemble.csv")
df <- read_csv("data/ensemble_data/submissions/2021-05-24-KITmetricslab-select_ensemble.csv")
df <- read_csv("data/ensemble_data/submissions/2021-05-31-KITmetricslab-select_ensemble.csv")

df <- read_csv(paste0("submissions/", forecast_date, "-KITmetricslab-select_ensemble.csv"))


plot_submission <- function(df, target="inc death", start_date="2020-11-01"){
  cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1]
  
  if (str_detect(target, 'cum death')){
    title <- 'Cumulative Deaths'
  } else if (str_detect(target, 'inc death')){
    title <- 'Incident Deaths'
  } else if (str_detect(target, 'cum case')){
    title <- 'Cumulative Cases'
  } else if (str_detect(target, 'inc case')){
    title <- 'Incident Cases'
  }
  
  df <- df %>%
    filter(str_detect(target, !!target)) %>%
    filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)
  
  exclude_locations <- c("60", "66", "69", "72", "74", "78")
  
  truth <- load_truth(title, min(df$target_end_date) - 5) %>%
    #filter(location != 'US' & !(location %in% exclude_locations)) %>%
    filter(!(location %in% exclude_locations)) %>%
    rename(target_end_date = date, truth = value)
  
  df <- bind_rows(truth, df)
  
  df <- df %>% 
    mutate(across(starts_with('value'), ~ coalesce(., truth))) %>%
    filter(target_end_date >= start_date)
  
  df <- add_location_names(df)
  

  
  ggplot(df, aes(x=target_end_date, y=value)) +
    facet_wrap('location_name', scales='free_y') +
    geom_smooth(aes(y = value.0.5, ymin = value.0.025, ymax = value.0.975), 
                linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
                linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
    geom_line(aes(y = truth)) +
    geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
    labs(title=title, x=NULL, y=NULL) +
    theme_grey(base_size=14)
}

plot_submission(df, "inc death")
plot_submission(df, "cum death")

plot_submission(df, "inc case")


ggsave('data/ensemble_data/inc_death_2021-05-03.png', width=40, height=25, dpi=500, unit='cm', device='png')
ggsave('data/ensemble_data/cum_death_2021-05-03.png', width=40, height=25, dpi=500, unit='cm', device='png')
ggsave('data/ensemble_data/inc_case_2021-05-03.png', width=40, height=25, dpi=500, unit='cm', device='png')


### FORMAT SUBMISSION

df$type <- 'quantile'

df_point <- df %>%
  filter(quantile == 0.5) %>%
  mutate(type = 'point', quantile = NA)

df <- bind_rows(df, df_point)

df$forecast_date <- as.Date(forecast_date)

df$value <- pmax(df$value, 0)

df <- df %>%
  select(forecast_date, target, target_end_date, location, type, quantile, value)

write.csv(df, paste0("submissions/", forecast_date, "-KITmetricslab-select_ensemble.csv"), row.names=FALSE)




truth <- load_truth('Incident Deaths')




df <- df %>%
  filter(str_detect(target, 'inc death')) %>%
  filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))

df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)


cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1] # length(levels_coverage) + 1

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

truth <- load_truth("Incident Deaths", min(df$target_end_date) - 5) %>%
  filter(!(location %in% exclude_locations)) %>%
  rename(target_end_date = date, truth = value)



a <- bind_rows(truth, df)

a <- a %>% 
  mutate(across(starts_with('value'), ~ coalesce(., truth)))

a <- a %>%
  filter(target_end_date >= '2020-11-01')

a <- add_location_names(a)

truth <- load_truth('Incident Deaths')
truth <- add_location_names(truth)%>%
  filter(date == '2021-04-17' & !(location %in% exclude_locations))

ggplot(a, aes(x=target_end_date, y=value)) +
  facet_wrap('location_name', scales='free_y') +
  geom_smooth(aes(y = value.0.5, ymin = value.0.025, ymax = value.0.975), 
              linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
              linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
  geom_line(aes(y = truth)) +
  geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
  labs(title='Incident Deaths', x=NULL, y=NULL) +
  geom_point(data = truth, aes(x=date, y=value), pch = 4, size=0.7) +
  facet_wrap('location_name', scales='free_y')

ggsave('data/ensemble_data/inc_death_obs1.png', width=40, height=25, dpi=500, unit='cm', device='png')


### Compare to individual models

df_individual <- load_forecasts(targets=c("1 wk ahead inc death"),
                     exclude_locations=exclude_locations, start_date="2021-04-17")

df_individual <- df_individual %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

df_individual <- df_individual %>%
  select(- location_name)

df <- df %>%
  filter(target == '1 wk ahead inc death')

df$model <- 'V3_iter'

df_all <- bind_rows(df, df_individual) %>%
  as.data.frame()

scores_all <- score_forecasts(df_all)

s <- scores_all %>%
  group_by(model) %>%
  summarize(meanWIS = mean(wis), meanAE = mean(ae))
