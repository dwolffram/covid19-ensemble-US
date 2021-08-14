library(murphydiagram)
library(viridis)
library(tidyverse)

df <- read.csv("evaluation/2021-06-12_df_processed.csv") %>%
  filter(location != "US")

#df$value <- pmax(df$value, 0)
#df$value <- round(df$value, 0)




murphy <- function(df, model1, model2, alpha = 0.5, diff = FALSE){
  
  df1 <- df %>%
    filter(quantile == 0.5)
  
  f1 <- subset(df1, model == model1)$value
  f2 <- subset(df1, model == model2)$value
  y <- subset(df1, model == "COVIDhub-baseline")$truth
  
  if(diff){
    murphydiagram_diff(f1, f2, y, functional="quantile", alpha=alpha)
  }
  else{
    murphydiagram(f1, f2, y, functional="quantile", alpha=alpha, labels=c(model1, model2))
  }
}


murphy_all <- function(df, model1, model2, diff = FALSE){
  par(mfrow=c(4, 2))
  
  qs = c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  
  for(alpha in qs){
    df1 <- df %>%
     filter(quantile == alpha)
  
    f1 <- subset(df1, model == model1)$value
    f2 <- subset(df1, model == model2)$value
    y <- subset(df1, model == "COVIDhub-baseline")$truth
    
    outer = TRUE
    line = -2
    cex = 2
    adj  = 0.025
    
    
    if(diff){
      murphydiagram_diff(f1, f2, y, functional="quantile", alpha=alpha)
    }
    else{
      murphydiagram(f1, f2, y, functional="quantile", alpha=alpha, labels=c(model1, model2))
    }
    #title(outer=outer,adj=adj,main=alpha,cex.main=cex,col="black",font=2,line=line)
    title(main=alpha, font=2)
  }
}

murphy_all(df, "KITmetricslab-select_ensemble", "COVIDhub-baseline")


murphy(df, "KITmetricslab-select_ensemble", "COVIDhub-baseline", 0.5)
murphy(df, "KITmetricslab-select_ensemble", "COVIDhub-baseline", 0.5, diff=TRUE)


murphydiagram(subset(df1, model=="KITmetricslab-select_ensemble")$value, subset(df1, model=="COVIDhub-baseline")$value, 
              y=subset(df1, model=="KITmetricslab-select_ensemble")$truth, functional="quantile", alpha=0.5)

murphydiagram_diff(subset(df1, model=="KITmetricslab-select_ensemble")$value, subset(df1, model=="COVIDhub-baseline")$value, 
              y=subset(df1, model=="KITmetricslab-select_ensemble")$truth, functional="quantile", alpha=0.5)


a <- subset(df1, model=="KITmetricslab-select_ensemble")



# implementation with ggplot

elementary_quantile_score <- function(y_true, y_pred, theta, alpha){
  ((y_true < y_pred) - alpha) * ((theta < y_pred) - (theta < y_true))
}

model1 = "KITmetricslab-select_ensemble"
model2 = "COVIDhub-baseline"

murphy_scores <- function(y_true, y_pred, thetas, alpha){
  scores <- list()
  for(t in thetas){
    scores[[paste0("s", t)]] <- elementary_quantile_score(y_true, y_pred, t, alpha)
  }
  return(scores)
}

df2 <- df %>%
  filter((model == model1 | model == model2) & target == "1 wk ahead inc death")


tmp <- c(df2$value, df2$truth)
thetas <- seq(from = min(tmp) - 1, to = max(tmp) + 1, length.out = 501)

get_thetas <- function(df){
  tmp <- c(df$value, df$truth)
  thetas <- seq(from = min(tmp) - 1, to = max(tmp) + 1, length.out = 501)
  return(thetas)
}


df2 <- df2 %>%
  crossing(theta = thetas)

df2 <- df2 %>%
  group_by(quantile) %>%
  crossing(theta = seq(from = min(min(.$value), min(.$truth)) - 1, 
                       to = max(max(.$value), max(.$truth)) + 1, 
                       length.out = 501))

df2 <- df2 %>%
  group_by(quantile) %>%
  crossing(theta = seq(from = min(min(value), min(truth)) - 1, 
                       to = max(max(value), max(truth)) + 1, 
                       length.out = 501))

df2 <- df2 %>% left_join(df2 %>%
                           group_by(quantile) %>%
                           do(crossing(theta = get_thetas(.))))
  

# df2 <- df2 %>%
#   group_by(quantile) %>%
#   crossing(theta = get_thetas(cur_data()))
# 
# df2 <- df2 %>%
#   group_by(quantile) %>%
#   summarize(theta = get_thetas(.))



df2 <- df2 %>%
  mutate(score = elementary_quantile_score(truth, value, theta, quantile))

df3 <- df2 %>%
  group_by(model, quantile, theta) %>%
  summarize(mean_score = mean(score))

ggplot(df3, aes(x=theta, y=mean_score, color=model)) +
  geom_line() +
  facet_wrap("quantile", scales="free") +
  labs(color = "Model") +
  xlab(expression(paste("Parameter ", theta))) +
  ylab(NULL)


df3 %>%
  group_by(quantile) %>%
  summarize(m = max(max(theta), 100))

df2 %>%
  group_by(quantile) %>%
  summarize(m = max(max(value), 500))

df2 %>%
  group_by(quantile) %>%
  crossing(m = max(value))



d <- data.frame(a= c(1, 1, 1, 1, 1), b = c(2, 2, 2, 2, 2))
d %>%
  mutate(m = min(min(a), min(b)))
