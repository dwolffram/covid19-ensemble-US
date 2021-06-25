setwd("covid19-ensemble-US")

source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/data_loading.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/scoring.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_methods.R")
source("https://raw.githubusercontent.com/dwolffram/covid19-ensembles/master/ensemble_functions.R")

library(viridis)
library(tidytext)

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(targets = c(paste(1:4, "wk ahead inc case"),  
                                 paste(1:4, "wk ahead inc death")), 
                     exclude_locations=exclude_locations,
                     start_date="2021-04-24")



write.csv(df, "evaluation/2021-06-19_df.csv", row.names=FALSE)


# get_all_models()
# 
# plot_availability(df, target = "1 wk ahead inc case")


KIT <- df %>%
  filter(model == "KITmetricslab-select_ensemble") %>% 
  group_by(target) %>%
  summarize(start_date = min(target_end_date),
            end_date = max(target_end_date),
            n = n_distinct(target_end_date))

locs <- df %>% 
  filter(model == "KITmetricslab-select_ensemble") %>% 
  distinct(location) %>% 
  pull()


temp <- df %>% 
  left_join(KIT, by = "target")

temp <- temp %>%
  filter(target_end_date >= start_date &
           target_end_date <= end_date &
           location %in% locs) %>% 
  group_by(target, model, target_end_date) %>%
  mutate(n_locs = n_distinct(location)) %>%
  filter(n_locs == length(locs)) %>% 
  group_by(target, model) %>%
  mutate(n_dates = n_distinct(target_end_date)) %>%
  filter(n_dates == n) %>%
  select(-c(start_date, end_date, n, n_dates, n_locs)) %>%
  as.data.frame()



# temp <- df %>%
#   filter(target == "1 wk ahead inc case")
# 
# KIT <- temp %>% 
#   filter(model == "KITmetricslab-select_ensemble")
# 
# start_date = min(KIT$target_end_date)
# end_date = max(KIT$target_end_date)
# n = n_distinct(KIT$target_end_date)
# locs = unique(KIT$location)
# 
# temp <- temp %>%
#   filter(target_end_date >= start_date &
#          target_end_date <= end_date &
#          location %in% locs) %>% 
#   group_by(model, target_end_date) %>%
#   mutate(n_locs = n_distinct(location)) %>%
#   filter(n_locs == length(locs)) %>% 
#   group_by(model) %>%
#   mutate(n_dates = n_distinct(target_end_date)) %>%
#   filter(n_dates == n) %>%
#   select(-c(n_dates, n_locs)) %>%
#   as.data.frame()



temp <- temp %>%
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

temp <- temp %>%
  filter(target_end_date <= "2021-06-19")

temp <- temp %>% 
  filter(!model %in% c("CU-scenario_high", "CU-scenario_mid", "CU-scenario_low", "CU-nochange"))


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

temp <- add_truth(temp)

write.csv(temp, "evaluation/2021-06-19_df_processed.csv", row.names=FALSE)


df_case <- temp %>%
  filter(str_detect(target, "case"))

df_death <- temp %>%
  filter(str_detect(target, "death"))

# scores <- score_forecasts(temp)

scores_case <- score_forecasts(df_case)
scores_death <- score_forecasts(df_death)
scores <- bind_rows(scores_case, scores_death)


write.csv(scores_case, "scores/2021-06-19_scores_cases.csv", row.names=FALSE)
write.csv(scores_death, "scores/2021-06-19_scores_deaths.csv", row.names=FALSE)
write.csv(scores, "scores/2021-06-19_scores.csv", row.names=FALSE)


mean_wis <- scores %>%
  filter(location != "US") %>%
  group_by(target, model) %>%
  summarize(mean_WIS = mean(wis))



wis_by_date <- scores %>%
  filter(!model %in% c("CU-scenario_high", "CU-scenario_mid", "CU-scenario_low", "CU-nochange")) %>%
  filter(location != "US") %>%
  group_by(target, model, target_end_date) %>%
  summarize(mean_WIS = mean(wis))
  
ggplot(wis_by_date, aes(x=target_end_date, y=mean_WIS, group=model)) +
  facet_wrap("target") +
  geom_line(aes(color=model)) +
  geom_line(data = subset(wis_by_date,  model == "KITmetricslab-select_ensemble"), color = "black", size=0.8)+
  geom_line(data = subset(wis_by_date,  model == "COVIDhub-trained_ensemble"), size=0.8, color="red")+
  geom_line(data = subset(wis_by_date,  model == "COVIDhub-ensemble"), size=0.8, color="orange")+
  geom_line(data = subset(wis_by_date,  model == "LNQ-ens1"), size=0.8, color="blue")


p <- s %>%
  filter(!model %in% c("CU-scenario_high", "CU-scenario_mid", "CU-scenario_low", "CU-nochange")) %>%
  filter(location != "US") %>%
  group_by(target, model, location_name) %>%
  summarize(mean_WIS = mean(wis))

ggplot(p, aes(x=location_name, y=mean_WIS, group=model)) +
  facet_wrap("target") +
  geom_line(aes(color=model)) +
  geom_line(data = subset(p,  model == "KITmetricslab-select_ensemble"), color = "black", size=0.8)+
  geom_line(data = subset(p,  model == "COVIDhub-trained_ensemble"), size=0.8, color="red")+
  geom_line(data = subset(p,  model == "COVIDhub-ensemble"), size=0.8, color="orange")+
  geom_line(data = subset(p,  model == "LNQ-ens1"), size=0.8, color="blue")


scores <- load_scores("scores/2021-06-19_scores_cases.csv", long_format=TRUE) %>%
  filter(!model %in% c("CU-scenario_high", "CU-scenario_mid", "CU-scenario_low", "CU-nochange")) %>%
  filter(location != "US")

ggplot(subset(scores, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap("target") +
  #facet_wrap("location_name", ncol=2, scales="free") +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = c(0.9, 0), 
        legend.justification = c(1, 0)) +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS")

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) parse(text=gsub(reg, "", x)), ...)
}

ggplot(subset(scores, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder_within(model, value, target), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  scale_x_reordered() +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  facet_wrap("target", scales="free", drop=TRUE) +
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  labs(x = NULL,
       y = "Mean WIS")
