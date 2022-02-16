library(tidyverse)
library(boot)

Sys.setlocale("LC_ALL", "C")

eval_date <- '2022-01-03'

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location != "US")

# df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
#   filter(location == "US")

models=c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

df_temp <- df %>%
  filter(target == "1 wk ahead inc death",
         model %in% models)

coverage <- function(data, indices){
  if(!missing(indices)){
    data <- data[indices, ]
  }
  l <- mean(data$truth < floor(data$value))
  u <- mean(data$truth <= floor(data$value))
  c(l, u)
}

boot_values <- function(data, R=1000){
  # stratify by location if more than one is present
  if(length(unique(df$location)) > 1){
    boot_out <- boot(data, strata=data$location, statistic=coverage, R=R)
  }
  else{
    boot_out <- boot(data, statistic=coverage, R=R)
  }
  
  sample_coverage <- boot_out$t0
  ci_50_l <- boot.ci(boot_out, index=1, type="perc", conf=c(0.5, 0.95))$perc[1, 4]
  ci_50_u <- boot.ci(boot_out, index=2, type="perc", conf=c(0.5, 0.95))$perc[1, 5]
  ci_95_l <- boot.ci(boot_out, index=1, type="perc", conf=c(0.5, 0.95))$perc[2, 4]
  ci_95_u <- boot.ci(boot_out, index=2, type="perc", conf=c(0.5, 0.95))$perc[2, 5]
  
  tibble("l" := sample_coverage[1],
         "u" := sample_coverage[2],
         "ci_50_l" := ci_50_l,
         "ci_50_u" := ci_50_u,
         "ci_95_l" := ci_95_l,
         "ci_95_u" := ci_95_u)
}

r <- df_temp %>%
  group_by(model, quantile) %>%
  summarize(boot_values(cur_data()))



ggplot(r) +
  facet_wrap("model", ncol=3) +
  geom_segment(aes(x=0,xend=1,y=0,yend=1), size=0.2, linetype="solid", colour="grey70")+
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.0125, size=0.3,
                data=r, colour="black") +
  geom_smooth(aes(x = quantile, y = u, ymin = ci_50_l, ymax = ci_50_u), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_smooth(aes(x = quantile, y = u, ymin = ci_95_l, ymax = ci_95_u), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
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

# ggsave("figures/coverage2.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)



### DIFFERENCE

r_diff <- r %>% 
  mutate_at(vars("l", "u", "ci_50_l", "ci_50_u", "ci_95_l", "ci_95_u"), list(~ . - quantile))

ggplot(r_diff) +
  facet_wrap("model", ncol=3) +
  geom_hline(yintercept = 0, size = 0.3, linetype = "solid", color = "darkgray") +
  geom_smooth(aes(x = quantile, y = u, ymin = ci_50_l, ymax = ci_50_u), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_smooth(aes(x = quantile, y = u, ymin = ci_95_l, ymax = ci_95_u), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.0125, size=0.3,
                data=r_diff, colour="black") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  xlab('Quantile') +
  ylab(NULL) +
  # labs(title="Coverage of death forecasts") +
  theme_bw(base_size=11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05))

# ggsave("figures/coverage_diff2.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)


### NEW BOOTSTRAP

eval_date <- '2022-01-03'
models <- c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")


df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location != "US")

df <- df %>%
  filter(target == "1 wk ahead inc death",
         model %in% models) %>% 
  select(- target)

dates <- unique(df$target_end_date)
df$residual = df$truth - df$value

# ggplot(subset(df_res, location <= 10), aes(x=target_end_date, y=residual)) +
#   geom_point()+
#   facet_grid(location ~ model)

# df <- df %>% 
#   select(c(target_end_date, location, model, quantile, residual))

# all(df_res$location == df_temp$location)




dates_resampled <- sample(dates, replace = TRUE)

df_resampled <- lapply(dates_resampled, function(x){df %>% filter(target_end_date == x)}) %>%
  bind_rows()

all(df_resampled$location == df$location)

df_temp <- df
df_temp$value <- df_temp$value + df_resampled$residual

df_temp


### SUBSAMPLING
coverage <- function(df){
  df$l <- df$truth < floor(df$value)
  df$u <- df$truth <= floor(df$value)
  
  df <- df %>%
    group_by(model, quantile) %>%
    summarize(l = mean(l), u=mean(u))
  
  return(df)
}

dates <- as.Date(unique(df$target_end_date))
n <- length(dates)
b <- 16
n_start <- n - b + 1

i <- sample(n_start, 1)
start <- dates[i]
end <- dates[i + b - 1]

df_window <- df %>%
  filter(between(target_end_date, start, end))

coverage(df_window)

sample_window <- function(df, b=16){
  dates <- as.Date(unique(df$target_end_date))
  n <- length(dates)
  n_start <- n - b + 1
  
  i <- sample(n_start, 1)
  start <- dates[i]
  end <- dates[i + b - 1]
  
  df_window <- df %>%
    filter(between(target_end_date, start, end))
  
  return(df_window)
}


dates <- as.Date(unique(df$target_end_date))
n <- length(dates)
b <- 16
n_start <- n - b + 1

coverage_df = data.frame()  

for(i in 1:n_start){
  start <- dates[i]
  end <- dates[i + b - 1]
  
  df_window <- df %>%
    filter(between(target_end_date, start, end))
  
  coverage_window <- coverage(df_window)
  
  coverage_df <- bind_rows(coverage_df, coverage_window)
}

t <- 2
results <- coverage_df %>%
  group_by(model, quantile) %>%
  summarize(l_5 = quantile(l, 0.05, type=t),
            # l_95 = quantile(l, 0.95),
            l_25 = quantile(l, 0.25, type=t),
            # l_75 = quantile(l, 0.75),
            # u_5 = quantile(u, 0.05),
            u_95 = quantile(u, 0.95, type=t),
            # u_25 = quantile(u, 0.25),
            u_75 = quantile(u, 0.75, type=t))

d <- coverage(df)

results <- results %>%
  left_join(d)


ggplot(results) +
  facet_wrap("model", ncol=3) +
  geom_segment(aes(x=0,xend=1,y=0,yend=1), size=0.2, linetype="solid", colour="grey70")+
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.0125, size=0.3,
                data=results, colour="black") +
  geom_smooth(aes(x = quantile, y = u, ymin = l_25, ymax = u_75), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_smooth(aes(x = quantile, y = u, ymin = l_5, ymax = u_95), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
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


r_diff <- results %>% 
  mutate_at(vars("l", "u", "l_5", "l_25", "u_75", "u_95"), list(~ . - quantile))

ggplot(r_diff) +
  facet_wrap("model", ncol=3) +
  geom_hline(yintercept = 0, size = 0.3, linetype = "solid", color = "darkgray") +
  geom_smooth(aes(x = quantile, y = u, ymin = l_25, ymax = u_75), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_smooth(aes(x = quantile, y = u, ymin = l_5, ymax = u_95), col = NA, stat = "identity", fill = "darkred", alpha = 0.2) +
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.0125, size=0.3,
                data=r_diff, colour="black") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  xlab('Quantile') +
  ylab(NULL) +
  # labs(title="Coverage of death forecasts") +
  theme_bw(base_size=11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05))




df %>%
  filter(location == "01") %>% 
  group_by(target_end_date) %>%
  summarize(truth = first(truth)) %>% 
  ggplot(aes(x = target_end_date, y = truth)) +
  geom_line()
