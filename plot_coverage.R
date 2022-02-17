library(tidyverse)
Sys.setlocale("LC_ALL", "C")

eval_date <- "2022-01-03"
models <- c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location != "US")

df <- df %>%
  filter(target == "1 wk ahead inc death",
         model %in% models) %>% 
  select(- target)

coverage <- function(df){
  df$l <- df$truth < floor(df$value)
  df$u <- df$truth <= floor(df$value)
  
  df <- df %>%
    group_by(model, quantile) %>%
    summarize(l = mean(l), u=mean(u), .groups = "drop")
  
  return(df)
}

plot_coverage <- function(df, date_column = target_end_date, B = 1000, difference = FALSE){
  date_column <- enquo(date_column)
  dates <- df %>% distinct(!!date_column) %>% pull()
  
  # compute coverage on all bootstrap samples
  coverage_df = data.frame()  
  for(i in 1:B){
    dates_resampled <- sample(dates, replace = TRUE)
    
    # simple filter doesn't work because the same date can occur multiple times
    df_resampled <- lapply(dates_resampled, function(x){df %>% filter(target_end_date == x)}) %>%
      bind_rows()
    
    coverage_sample <- coverage(df_resampled)
    
    coverage_df <- bind_rows(coverage_df, coverage_sample)
  }
  
  # compute CIs from bootstrapped coverage, we only use the lower (upper) bound for the lower (upper) value
  results <- coverage_df %>%
    group_by(model, quantile) %>%
    summarize(l_5 = quantile(l, 0.05),
              # l_95 = quantile(l, 0.95),
              l_25 = quantile(l, 0.25),
              # l_75 = quantile(l, 0.75),
              # u_5 = quantile(u, 0.05),
              u_95 = quantile(u, 0.95),
              # u_25 = quantile(u, 0.25),
              u_75 = quantile(u, 0.75), 
              .groups = "drop")
  
  # compute coverage on full sample
  coverage_full <- coverage(df)
  
  results <- results %>%
    left_join(coverage_full, by = c("model", "quantile"))
  
  if (!difference){
    g <- ggplot(results) +
      facet_wrap("model", ncol = 3) +
      geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), size = 0.2, linetype = "solid", colour = "grey70")+
      geom_ribbon(aes(x = quantile, ymin = l_25, ymax = u_75), fill = "darkred", alpha = 0.2) +
      geom_ribbon(aes(x = quantile, ymin = l_5, ymax = u_95), fill = "darkred", alpha = 0.2) +
      geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
      scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = function(x) ifelse(x == 0, "0", x)) +
      scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
      xlab("Quantile") +
      ylab(NULL) +
      theme_bw(base_size = 11) +
      theme(panel.grid.major = element_line(size = 0.05), 
            panel.grid.minor = element_line(size = 0.05)) +
      coord_fixed()
    
    print(g)
    invisible(results)
  }
  else{
    results_diff <- results %>% 
      mutate_at(vars("l", "u", "l_5", "l_25", "u_75", "u_95"), list(~ . - quantile))
    
    g <- ggplot(results_diff) +
      facet_wrap("model") +
      geom_hline(yintercept = 0, size = 0.3, linetype = "solid", color = "darkgray") +
      geom_ribbon(aes(x = quantile, ymin = l_25, ymax = u_75), fill = "darkred", alpha = 0.2) +
      geom_ribbon(aes(x = quantile, ymin = l_5, ymax = u_95), fill = "darkred", alpha = 0.2) +
      geom_errorbar(aes(x=quantile, ymin = l, ymax = u), width = 0.0125, size = 0.3, colour = "black") +
      scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = function(x) ifelse(x == 0, "0", x)) +
      scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
      xlab("Quantile") +
      ylab(NULL) +
      theme_bw(base_size = 11) +
      theme(panel.grid.major = element_line(size = 0.05), 
            panel.grid.minor = element_line(size = 0.05))
    
    print(g)
    invisible(results_diff)
  }
}

results <- plot_coverage(df, B = 1000)

ggsave("figures/coverage_states2.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

results_diff <- plot_coverage(df, B = 1000, difference = TRUE)

ggsave("figures/coverage_diff_states2.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)
