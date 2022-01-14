library(tidyverse)

Sys.setlocale("LC_ALL", "C")

elementary_quantile_score <- function(y_true, y_pred, theta, alpha){
  ((y_true < y_pred) - alpha) * ((theta < y_pred) - (theta < y_true))
}

get_thetas <- function(df, n=1001){
  tmp <- c(df$value, df$truth)
  thetas <- seq(from = min(tmp) - 1, to = max(tmp) + 1, length.out = n)
  return(thetas)
}

murphy_diagram <- function(df, models, target, 
                                       quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 0.01, 0.05, 0.15, 0.2, 
                                                     0.3, 0.35, 0.4, 0.45, 0.55, 0.6, 0.65, 0.7, 0.8, 0.85, 0.95, 0.99)){
  df <- df %>%
    filter(model %in% models,
           target == !!target,
           quantile %in% quantiles)
  
  df <- df %>% left_join(df %>%
                           group_by(quantile) %>%
                           do(crossing(theta = get_thetas(., 150))), by = "quantile")
  
  df <- df %>%
    mutate(score = elementary_quantile_score(truth, value, theta, quantile))
  
  df <- df %>%
    group_by(model, quantile, theta) %>%
    summarize(mean_score = mean(score), .groups="keep")
  
  ggplot(df, aes(x=theta, y=mean_score, color=model)) +
    geom_line(size=0.5) +
    facet_wrap("quantile", scales="free") +
    # labs(title = paste("Murphy diagram:", target), color = "Model") +
    xlab(expression(paste("Parameter ", theta))) +
    ylab(NULL) +
    theme_bw(base_size = 11) +
    theme(aspect.ratio = 1, legend.position = "bottom", legend.title = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(color = "black"),
          panel.grid.major = element_line(size = 0.05), 
          panel.grid.minor = element_line(size = 0.05))
}


eval_date <- '2022-01-03'

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location != "US")


models=c("KITmetricslab-select_ensemble", "COVIDhub-ensemble", "COVIDhub-baseline")

murphy_diagram(df, models, '1 wk ahead inc death', quantiles = c(0.05, 0.5, 0.95))

ggsave("figures/murphy_states.pdf", width=210, height=100, unit="mm", device = "pdf", dpi=300)
