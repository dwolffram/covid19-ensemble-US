library(tidyverse)

Sys.setlocale("LC_ALL", "C")

eval_date <- '2022-01-03'

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location == "US")


models=c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

df_temp <- df %>%
  filter(target == "1 wk ahead inc death",
           model %in% models)
df_temp$l <- df_temp$truth < floor(df_temp$value)
df_temp$u <- df_temp$truth <= floor(df_temp$value)

df_temp <- df_temp %>%
  group_by(target, model, quantile) %>%
  summarize(l = mean(l), u=mean(u))

ggplot(df_temp) +
  facet_wrap("model", ncol=3) +
  geom_segment(aes(x=0,xend=1,y=0,yend=1), size=0.2, linetype="solid", colour="grey70")+
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.0125, size=0.3,
                data=df_temp, colour="black") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  xlab('Quantile') +
  ylab(NULL) +
  # labs(title="Coverage of death forecasts") +
  theme_bw(base_size=11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05)) +
  coord_fixed()

ggsave("figures/coverage_national.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)
