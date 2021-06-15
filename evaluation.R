setwd("covid19-ensemble-US")

source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/data_loading.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/scoring.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_methods.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_functions.R")

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")



df <- load_forecasts(targets = c("1 wk ahead inc case", "2 wk ahead inc case"), 
                     exclude_locations=exclude_locations,
                     start_date="2021-04-24")

get_all_models()
