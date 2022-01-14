library(tidyverse)

Sys.setlocale("LC_ALL", "C")

eval_date <- '2022-01-03'

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols())
df_wide <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)

df_ens <- df %>%
  filter(model == "COVIDhub-ensemble",
         str_detect(target, "death"),
         location == "US")

cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1]

ggplot(df_ens, aes(x=target_end_date, y=truth)) +
  facet_wrap("target", scales="free_y", ncol=2) +
  geom_smooth(aes(y = value.0.5, ymin = value.0.025, ymax = value.0.975), 
              linetype=3, size=0.7, colour="white", fill=cols[2], alpha=0.8, stat = "identity") +
  geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
              linetype=3, size=0.7, colour="white", fill=cols[1], alpha=0.8, stat = "identity") +
  geom_line() +
  geom_point(pch = 4, size=3) +
  geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=3) +
  theme_bw() +
  labs(title="National level forecasts - KITmetricslab-select_ensemble",
       x = NULL,
       y = "Deaths") +
  theme_bw(base_size=16) +
  theme(strip.text.y = element_text(size = 8))



df2 <- df %>%
  filter(model == "COVIDhub-ensemble",
         target == "1 wk ahead inc death",
         location == "US",
         !quantile %in% c(0.01, 0.5, 0.99) )

df2_wide <- df_wide %>%
  filter(model == "COVIDhub-ensemble",
         target == "1 wk ahead inc death",
         location == "US")

ggplot(df2_wide, aes(x=target_end_date)) +
  geom_point(data=df2, aes(x=target_end_date, y=value), 
             shape='-', size=3, col='deepskyblue3', alpha=0.9) +
  geom_crossbar(aes(y=value.0.5, ymin=value.0.01, ymax=value.0.99), fatten=1.5,
                width=3, size=0.5, col='deepskyblue4', fill='gray', alpha=0.3) +
  geom_line(aes(y=truth), size=1, col='darkred') +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%Y") +
  xlab(NULL) +
  ylab('Incident deaths') +
  labs(title = 'One-week-ahead ensemble forecasts') +
  theme_bw(base_size = 11)

ggsave("figures/ensemble_forecast.pdf", width=180, height=100, unit="mm", device = "pdf", dpi=300)


### Multiple models

unique(df$model)

models <- c('COVIDhub-baseline', 'COVIDhub-ensemble', 'KITmetricslab-select_ensemble')

df2 <- df %>%
  filter(model %in% models,
         target == "1 wk ahead inc death",
         location == "US",
         !quantile %in% c(0.01, 0.5, 0.99) )

df2_wide <- df_wide %>%
  filter(model %in% models,
         target == "1 wk ahead inc death",
         location == "US")

ggplot(df2_wide, aes(x=target_end_date)) +
  facet_wrap("model", ncol = 1) +
  geom_point(data=df2, aes(x=target_end_date, y=value), 
             shape='-', size=1.8, col='darkslategray4', alpha=0.9) +
  geom_crossbar(aes(y=value.0.5, ymin=value.0.01, ymax=value.0.99), fatten=4,
                width=2, size=0.1, col='deepskyblue4', fill='gray', alpha=0.3) +
  geom_line(aes(y=truth), size=0.4, col='darkred') +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  xlab(NULL) +
  ylab('Incident deaths') +
  labs(title = 'One-week-ahead forecasts') +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05))

ggsave("figures/ensemble_forecast2.pdf", width=160, height=180, unit="mm", device = "pdf", dpi=500)

