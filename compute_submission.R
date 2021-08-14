setwd("covid19-ensemble-US")

source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/data_loading.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/scoring.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_methods.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_functions.R")


load_train_test <- function(forecast_date, national_level = FALSE){
  df_train <- read_csv(paste0("data/df_train_", forecast_date, ".csv"), 
                       col_types = cols(forecast_date = col_date(format = ""),
                                        target = col_character(),
                                        target_end_date = col_date(format = ""),
                                        location = col_character(),
                                        type = col_character(),
                                        quantile = col_double(),
                                        value = col_double())) %>%    
    filter(if (!national_level) location != "US" else TRUE) %>%
    as.data.frame()
  
  
  df_test <- read_csv(paste0("data/df_test_", forecast_date, ".csv"), 
                      col_types = cols(forecast_date = col_date(format = ""),
                                       target = col_character(),
                                       target_end_date = col_date(format = ""),
                                       location = col_character(),
                                       type = col_character(),
                                       quantile = col_double(),
                                       value = col_double())) %>%
    filter(if (!national_level) {location != "US"} else {location == "US"}) %>%
    as.data.frame()
  
  return(list(df_train=df_train, df_test=df_test))
}

compute_forecasts <- function(forecast_date, national_level=FALSE){
  dfs <- load_train_test(forecast_date, national_level)
  df_train <- dfs$df_train
  df_test <- dfs$df_test
  
  df_ensemble <- data.frame()
  
  for (t in unique(df_test$target)){
    print(t)
    train <- df_train %>%
      filter(target == t)
    test <- df_test %>%
      filter(target == t)
    
    train <- add_truth(train)
    test$truth <- 0
    
    p <- v3_iter_fit(train)
    print(p)
    ensemble <- V3(test, params=p$params, models=p$models)
    ensemble <- sort_quantiles(ensemble)
    df_ensemble <- bind_rows(df_ensemble, ensemble)
  }
  
  df_ensemble <- df_ensemble %>%
    select(-truth)
  
  return(df_ensemble)
}

compute_submission <- function(forecast_date){
  print("Compute state level ensemble forecasts...")
  df_ensemble <- compute_forecasts(forecast_date, national_level = FALSE)
  write.csv(df_ensemble, paste0("data/df_pred_", forecast_date, ".csv"), row.names=FALSE)
  
  print("Compute national level ensemble forecasts...")
  df_ensemble <- compute_forecasts(forecast_date, national_level = TRUE)
  write.csv(df_ensemble, paste0("data/df_pred_US_", forecast_date, ".csv"), row.names=FALSE)
  
  df <- read_csv(paste0("data/df_pred_", forecast_date, ".csv"))
  df2 <- read_csv(paste0("data/df_pred_US_", forecast_date, ".csv"))
  df <- bind_rows(df, df2)
  
  ### FORMAT SUBMISSION
  print("Reformat submission...")
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
  print("Done.")
}


plot_submission <- function(df, target="inc death", start_date="2020-11-01", locations){
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
    filter(!(location %in% exclude_locations)) %>%
    rename(target_end_date = date, truth = value)
  
  df <- bind_rows(truth, df)
  
  df <- df %>% 
    mutate(across(starts_with('value'), ~ coalesce(., truth))) %>%
    filter(target_end_date >= start_date)
  
  df <- add_location_names(df)
  
  if(!missing(locations)){
    df <- df %>%
      filter(location %in% locations)
  }
  
  
  
  ggplot(df, aes(x=target_end_date, y=value)) +
    facet_wrap('location_name', scales='free_y') +
    geom_smooth(aes(y = value.0.5, ymin = value.0.025, ymax = value.0.975), 
                linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
                linetype=3, size=0.4, colour="white", fill=cols[1], alpha=1, stat = "identity") +
    geom_line(aes(y = truth)) +
    geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
    labs(title=title, x=NULL, y=NULL) +
    theme_grey(base_size=16)
}

forecast_date <- '2021-08-09'
compute_submission(forecast_date)


df <- read_csv(paste0("submissions/", forecast_date, "-KITmetricslab-select_ensemble.csv"))

plot_submission(df, "inc case")
plot_submission(df, "inc death")
plot_submission(df, "cum death")


