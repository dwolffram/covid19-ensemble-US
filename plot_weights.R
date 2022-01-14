library(tidyverse)
Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_ALL", "C")

df <- read_csv("evaluation/2021-08-02_model_weights.csv")

df <- df %>%
  filter(params != 0 & !str_detect(target, "cum")) 

df$models <- as.factor(df$models)

df[df == 0] <- NA

df <- df %>%
  group_by(target) %>%
  complete(nesting(target, models), forecast_date)

df <- df %>%
  filter(!str_detect(target, "cum")) 

df_count <- df %>%
  group_by(target, forecast_date) %>%
  summarize(n_models = n_distinct(models))

ggplot(df_count, aes(x=forecast_date, y = n_models)) +
  facet_wrap("target", ncol=2) +
  geom_line() +
  xlab("Forecast date") +
  ylab("Number of models")

library(RColorBrewer)
palette_Dark2 <- colorRampPalette(brewer.pal(20, "Dark2"))

library(randomcoloR)
n <- 20
palette <- distinctColorPalette(n)
my_cols <- randomColor(n)

my_cols <- c("#89ffac", "#ffbccd", "#74ed7a", "#d8f783", "#bc17ea", "#a964c9", 
  "#4fc3ea", "#960bc4", "#e5ce4e", "#aaffe1", "#2044c9", "#988bd6", 
  "#2cd117", "#db0d10", "#c46cd8", "#59eae3", "#f7adb8", "#ff6870", 
  "orange", "#ffc8ad")

ggplot(subset(df, str_detect(target, "case")), aes(x=forecast_date, y = params, color=models)) +
  facet_wrap("target", ncol=2, scales="free") +
  geom_line(size = 1) +
  geom_point() +
  xlab("Forecast date") +
  ylab("Model weight") +
  theme_gray(base_size = 16) +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values=my_cols)

ggsave('plots/weights_cases.png', width=30, height=16, dpi=500, unit='cm', device='png')
