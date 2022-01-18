library(tidyverse)

Sys.setlocale("LC_ALL", "C")

eval_date <- '2022-01-03'

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location != "US")


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
  geom_line(aes(x = quantile, y = l)) +
  geom_line(aes(x = quantile, y = u)) +
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

#ggsave("figures/coverage_national.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

####

df_temp <- df %>%
  filter(target == "1 wk ahead inc death",
         model %in% models)

df_temp <- df %>%
  filter(target == "1 wk ahead inc death",
         model %in% models,
         quantile %in% c(0.1, 0.5, 0.9))

df_temp <- df %>%
  filter(target == "1 wk ahead inc death",
         quantile == 0.5,
         model == "COVIDhub-ensemble")

coverage <- function(x, y){
  l <- mean(y < floor(x))
  u <- mean(y <= floor(x))
  c(l, u)
}

coverage(df_temp$value, df_temp$truth)

df_temp %>%
  group_by(model, quantile) %>%
  summarize(coverage = coverage(value, truth))

df_temp$l <- df_temp$truth < floor(df_temp$value)
df_temp$u <- df_temp$truth <= floor(df_temp$value)

df_temp <- df_temp %>%
  group_by(target, model, quantile) %>%
  summarize(l = mean(l), u=mean(u))

library(boot)

coverage <- function(data, indices){
  if(!missing(indices)){
    data <- data[indices, ]
  }
  l <- mean(data$truth < floor(data$value))
  u <- mean(data$truth <= floor(data$value))
  c(l, u)
}

results <- boot(df_temp, statistic=coverage, R=1000)

ci <- boot.ci(results, type="perc")
ci$perc[c(4,5)]

ci <- boot.ci(results, index = 2, type="perc")
ci$perc[c(4,5)]

df_temp %>%
  group_by(model, quantile) %>%
  summarize(ci = boot.ci(boot(., statistic=coverage, R=1000), type="perc")$perc[c(4, 5)])

df_temp %>%
  group_by(model, quantile) %>%
  summarize(boot(., statistic=coverage, R=1000))


boot_values <- function(data){
  boot_out <- boot(data, statistic=coverage, R=1000)
  sample_coverage <- boot_out$t0
  ci_l <- boot.ci(boot_out, index=1, type="perc")$perc[c(4, 5)]
  ci_u <- boot.ci(boot_out, index=2, type="perc")$perc[c(4, 5)]
  
  tibble("l" := sample_coverage[1],
         "u" := sample_coverage[2],
         "l_l" := ci_l[1],
         "l_u" := ci_l[2],
         "u_l" := ci_u[1],
         "u_u" := ci_u[2])
}

r <- df_temp %>%
  group_by(model, quantile) %>%
  summarize(boot_values(cur_data()))


ggplot(r) +
  facet_wrap("model", ncol=3) +
  geom_segment(aes(x=0,xend=1,y=0,yend=1), size=0.2, linetype="solid", colour="grey70")+
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.0125, size=0.3,
                data=r, colour="black") +
  # geom_line(aes(x = quantile, y = l_l)) +
  # geom_line(aes(x = quantile, y = l_u)) +
  # geom_line(aes(x = quantile, y = u_l)) +
  # geom_line(aes(x = quantile, y = u_u)) +
  geom_smooth(aes(x = quantile, y = l, ymin = l_l, ymax = l_u), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_smooth(aes(x = quantile, y = u, ymin = u_l, ymax = u_u), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
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

ggsave("figures/coverage_new.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

