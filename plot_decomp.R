library(tidyverse)
library(geomtextpath)
library(ggrepel)


df <- read.csv("evaluation/2021-07-10_df_processed.csv") %>%
  filter(location == "US")

eval_date <- '2022-01-03'

df <- read_csv(paste0("evaluation/", eval_date, "_df_processed.csv"), col_types = cols()) %>%
  filter(location != "US")

reldiag = function(x, y, alpha = 0.5, n_resamples = 999, digits = 3, region_level = 0.9){
  
  require(isotone)
  pava = function(x, y){
    # In case of ties,  gpava assigns mean instead of quantile, e.g.,
    # gpava(c(-1,-1,-1),c(-1,0,0),solver = weighted.median,ties = "secondary")
    # the following step replaces y values with the respective quantile in case of ties
    y = unlist(lapply(split(y, x),function(y) rep(quantile(y, alpha, type = 1), length(y))), use.names = FALSE)
    return(gpava(x, y, solver = weighted.fractile, p = alpha, ties = "secondary")$x)
  }
  score = function(x, y) mean(2*(as.numeric(x >= y) - alpha)*(x-y))
  marg = function(x) quantile(x, alpha, type = 1)
  identif = function(x, y) as.numeric(x > y) - alpha
  score_label = "QS "
  
  ord_x = order(x)
  x = x[ord_x]
  y = y[ord_x]
  
  x_rc = pava(x,y)
  
  res = y - x
  
  s = score(x,y)
  c_rc_ucond = optim(par = 0,fn = function(c) score(x+c,y),method = "Brent",lower = min(res),upper = max(res))$par
  s_rc_ucond = score(x + c_rc_ucond,y)
  s_rc = score(x_rc,y)
  s_mg = score(marg(y),y)
  
  mcb = s - s_rc
  umcb = s - s_rc_ucond
  cmcb = s_rc_ucond - s_rc
  dsc = s_mg - s_rc
  unc = s_mg
  
  # test: mean identification zero? (t-test)
  v = identif(x,y)
  t = sqrt(length(v)) * mean(v)/sd(v)
  pval_ucond = 1 - abs(pt(t,length(v)-1) - 0.5)*2
  
  n_samples = n_resamples + 1 # total number of samples including observed sample
  low = floor(n_samples * (1-region_level)/2)
  up = n_samples - low
  pval_digits = ceiling(log(n_samples,10))
  
  resamples = sapply(1:n_resamples,function(i) x + sample(res,length(y),replace = TRUE)) 
  
  x_rc_resamples = apply(resamples, 2, function(y) pava(x,y))
  x_rc_resamples_sorted = apply(cbind(x_rc,x_rc_resamples),1,sort) - marg(res) # includes observed values + bias corrected (shifted by mean residual)
  
  ran_x = range(x)
  
  mcb_resamples = sapply(1:n_resamples,function(i) score(x,resamples[,i]) - score(x_rc_resamples[,i],resamples[,i]))
  mcb_bounds = sort(c(mcb,mcb_resamples))[c(low,up)]
  
  rank_obs = tail(rank(c(mcb_resamples,mcb)),1)
  pval = 1 - (rank_obs - 1)/(n_resamples + 1)
  
  results <- data.frame(quantile = alpha, x = x, y = y, x_rc = x_rc,
                        lower = x_rc_resamples_sorted[low,],
                        upper = x_rc_resamples_sorted[up,],
                        score = s,
                        umcb = umcb, cmcb = cmcb, mcb = mcb, dsc = dsc, unc = unc,
                        pval_cond = pval, pval_ucond = pval_ucond)
}

df2 <- df %>%
  filter(quantile == 0.5 & target == "1 wk ahead inc case" & model != "USC-SI_kJalpha")

df2 <- df %>%
  filter(target == "1 wk ahead inc case" & model != "USC-SI_kJalpha")

df2 <- df %>%
  filter(quantile %in% c(0.05, 0.5, 0.95),
         target == "1 wk ahead inc death",
         model != "USC-SI_kJalpha")

# df_med %>%
#   drop_na() %>%
#   group_by(model) %>%
#   summarize(n = n())

results <- df2 %>%
  group_by(model, quantile) %>%
  do(reldiag(.$value, .$truth, alpha = unique(.$quantile), n_resamples = 99))

# summarize scores and create labels
scores <- results %>%
  group_by(quantile, model) %>%
  distinct(across(score:pval_ucond))

write.csv(scores, 'evaluation/2022-01-03_score_decomp2.csv', row.names=FALSE)

scores <- read.csv("evaluation/2022-01-03_score_decomp2.csv")

scores <- scores %>%
  filter(!model %in% c("COVIDhub-4_week_ensemble", "COVIDhub-ensemble_CDC"))
  
scores$quantile <- as.factor(scores$quantile)

scores$model <- str_replace(scores$model, "COVIDhub-baseline", "Baseline")
scores$model <- as.character(lapply(strsplit(as.character(scores$model), "-"), '[[', 1))
scores$model <- str_replace(scores$model, "COVIDhub", "COVIDhub-ensemble")




df.abline <- data.frame(slope=1,
                        intercept=seq(-max(scores$mcb),
                                      max(scores$dsc),
                                      length.out=10))

ggplot(data=scores) +
  theme_classic() +
  xlim(c(0.9*min(scores$mcb), 1.1*max(scores$mcb))) +
  ylim(c(0.9*min(scores$dsc), 1*max(scores$dsc))) +
  geom_point(aes(x=mcb, y=dsc, color=quantile)) +
  geom_text(aes(x=mcb, y=dsc, label=model), size=4, vjust=-0.1, hjust=-0.1, 
            position = position_dodge()) +
  geom_abline(data=df.abline, aes(intercept=intercept, slope=slope, color=intercept)) +
  scale_colour_gradient(low = "white", high = "black", name="UNC - Brier Score", guide = "colourbar") +
  guides(color = guide_colourbar(barwidth = 20, barheight = 0.5)) +
  theme(legend.position = "bottom")


ggplot(data=scores) +
  xlim(c(0.9*min(scores$mcb), 1.1*max(scores$mcb))) +
  ylim(c(0.9*min(scores$dsc), 1*max(scores$dsc))) +
  geom_point(aes(x=mcb, y=dsc, fill=quantile)) +
  geom_text(aes(x=mcb, y=dsc, label=model), size=4, vjust=-0.1, hjust=-0.1, 
            position = position_dodge()) +
  geom_abline(data=df.abline, aes(intercept=intercept, slope=slope, color=intercept)) +
  #scale_colour_gradient(low = "white", high = "black", name="UNC - Brier Score", guide = "colourbar") +
  #guides(color = guide_colourbar(barwidth = 20, barheight = 0.5)) +
  theme(legend.position = "bottom")

library(Rcpp)
library(ggrepel)

scores_med <- subset(scores, quantile == 0.5)

df.abline <- data.frame(slope=1,
                        intercept=seq(-ceiling(max(scores$mcb)),
                                      ceiling(max(scores$dsc)),
                                      length.out=10),
                        label = 3)

scores <- scores %>%
  filter(!model %in% c("COVIDhub-4_week_ensemble", "COVIDhub-ensemble_CDC"))

iso <- scores %>%
  group_by(quantile) %>%
  summarize(intercept = seq(-ceiling(max(mcb)), ceiling(max(dsc)), length.out=10), slope = 1)

iso <- scores %>%
  group_by(quantile) %>%
  summarize(intercept = seq(max(dsc), -max(mcb), by = -(max(dsc) - min(dsc))/4), slope = 1, unc = unique(unc))

s <- subset(scores, quantile %in% c(0.05, 0.5, 0.95))
i <- subset(iso, quantile %in% c(0.05, 0.5, 0.95))

ggplot(data=s) +
  facet_wrap('quantile', scales = "free", ncol = 3) +
  geom_point(aes(x=mcb, y=dsc, color=model), size = 0.4) +
  geom_text_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
                  direction = "both", segment.color = "transparent", box.padding = 0.1, force = 1, point.padding = 0) +
  geom_abline(data=i, aes(intercept=intercept, slope=slope), linetype = "solid", alpha = 0.3,
              size = 0.2) +
  #geom_label_repel(data=i, aes(label = QS)) +
  theme(legend.position = "None", aspect.ratio = 1) +
  xlab("MCB") +
  ylab("DSC") +
  #labs(title = "1 wk ahead inc case") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.position = "none",
        # axis.text.x=element_blank(),
        # axis.ticks.x=element_blank(),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank()
        )


library(geomtextpath)

ggplot(data=s) +
  facet_wrap('quantile', scales = "free", ncol = 3) +
  # geom_abline(data=i, aes(intercept=intercept, slope=slope), linetype = "solid", alpha = 0.3,
  #             size = 0.2) +
  geom_textabline(data = i, aes(intercept=intercept, slope=slope, label=round(unc - intercept, 1)), 
                  hjust=0.875, size=4*0.36, linewidth=0.2, linecolour="gray") +
  # geom_label_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
  #                 direction = "both", segment.color = "transparent",
  #                 label.size=NA) +
  geom_point(aes(x=mcb, y=dsc, color=model), size = 0.4) +
  geom_text_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
                  direction = "both", segment.color = "transparent", box.padding = 0.1, force = 1, point.padding = 0) +
  xlab("MCB") +
  ylab("DSC") +
  #labs(title = "1 wk ahead inc case") +
  theme_bw(base_size = 10) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

ggsave("figures/decomposition3.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)



### LABEL MIT WEISSEM HINTERGRUND
ggplot(data=s) +
  facet_wrap('quantile', scales = "free", ncol = 3) +
  # geom_abline(data=i, aes(intercept=intercept, slope=slope), linetype = "solid", alpha = 0.3,
  #             size = 0.2) +
  # geom_labelabline(data = i, aes(intercept=intercept, slope=slope, label=round(unc - intercept, 1)), 
  #                  hjust=0.85, size=4*0.36, text_only=TRUE, boxcolour=NA, halign="right") +
  geom_textabline(data = i, aes(intercept=intercept, slope=slope, label=round(unc - intercept, 1)), 
                  hjust=0.85, size=4*0.36, linewidth=0.2, linecolour="gray") +
  geom_label_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
                  direction = "both", segment.color = "transparent",
                  box.padding = 0, label.padding = 0.15,
                  label.size=NA) +
  geom_point(aes(x=mcb, y=dsc, color=model), size = 0.4) +
  xlab("MCB") +
  ylab("DSC") +
  #labs(title = "1 wk ahead inc case") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )


ggsave("figures/decomposition4.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

### MIT LEGENDE

ggplot(data=s) +
  facet_wrap('quantile', scales = "free", ncol = 3) +
  geom_abline(data=i, aes(intercept=intercept, slope=slope), linetype = "solid", alpha = 0.3,
              size = 0.2) +
  geom_labelabline(data = i, aes(intercept=intercept, slope=slope, label=round(unc - intercept, 1)), 
                   hjust=0.8, size=4*0.36, text_only=TRUE, fill="white", boxcolour=NA,
                   label.r = unit(0.05, "lines"),
                   label.padding = unit(0.1, "lines"),) +
  geom_point(aes(x=mcb, y=dsc, color=model), size = 0.4) +
  # geom_label_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
  #                 direction = "both", segment.color = "transparent",
  #                 label.size=NA) +
  # geom_text_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
  #                 direction = "both", segment.color = "transparent", box.padding = 0.1, force = 1, point.padding = 0) +
  #theme(legend.position = "None", aspect.ratio = 1) +
  xlab("MCB") +
  ylab("DSC") +
  #labs(title = "1 wk ahead inc case") +
  theme_bw(base_size = 10) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        #legend.position = "none",
        legend.position = "bottom",
        legend.text = element_text(size=7),
        legend.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

ggsave("figures/decomposition_legend.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)


### REMOVE LABELS MANUALLY

scores <- read.csv("evaluation/2022-01-03_score_decomp2.csv")

scores <- scores %>%
  filter(!model %in% c("COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble"))

scores$quantile <- as.factor(scores$quantile)
scores$model <- str_replace(scores$model, "COVIDhub-baseline", "Baseline")
scores$model <- as.character(lapply(strsplit(as.character(scores$model), "-"), '[[', 1))
scores$model <- str_replace(scores$model, "COVIDhub", "COVIDhub-ensemble")


# iso <- scores %>%
#   group_by(quantile) %>%
#   summarize(m=(max(dsc)-min(dsc))/(max(mcb)-min(mcb)), d = sqrt(max(dsc)^2 + max(mcb)^2),
#             step_size = 1/10 * d/m,
#             intercept = seq(ceiling(max(dsc)) + first(unc)%%1 - ceiling(min(mcb)), # add decimal part of unc to ensure integer valued scores on isolines
#                             -(ceiling(max(mcb))+ first(unc)%%1 - ceiling(min(mcb))), 
#                             # by = -step_size), 
#                             by = -1/m * 1/4 * (max(dsc) - min(dsc))), 
#             slope = 1, 
#             unc = unique(unc)) %>%
#   mutate(score = round(unc - intercept, 1), label = score)



iso <- scores %>%
  group_by(quantile) %>%
  summarize(intercept = seq(ceiling(max(dsc)) + first(unc)%%1 - ceiling(min(mcb)), # add decimal part of unc to ensure integer valued scores on isolines
                            -(ceiling(max(mcb))+ first(unc)%%1 - ceiling(min(mcb))), 
                            by = -round((max(dsc) - min(dsc))/8)), 
                            # by = -round((max(mcb)-min(mcb))/(max(dsc)-min(dsc))*(max(dsc) - min(dsc))/4)), 
                            # by = -((max(mcb)-min(mcb))/(max(dsc)-min(dsc)))), 
            slope = 1, 
            unc = unique(unc)) %>%
  mutate(score = round(unc - intercept, 1), label = score)

s <- subset(scores, quantile %in% c(0.05, 0.5, 0.95))
i <- subset(iso, quantile %in% c(0.05, 0.5, 0.95))

# i$label[c(3, 4, 5, 14, 15, 19, 21, 24, 42, 48)] <- NA
# i$label[c(2, 3, 10, 17, 34, 40)] <- NA
i$label[c(1, 2, 3, 12, 13, 14, 15, 18, 19, 24, 57, 58, 59, 62, 63, 70)] <- NA

ggplot(data=s) +
  facet_wrap('quantile', scales = "free", ncol = 3) +
  geom_abline(data=i, aes(intercept=intercept, slope=slope), color="lightgray", alpha=0.5,
              size = 0.2) +
  geom_labelabline(data = i, aes(intercept=intercept, slope=slope, label=label), color="gray50",
                   hjust=0.85, size=4*0.36, text_only=TRUE, boxcolour=NA, straight=TRUE) +
  # geom_textabline(data = i, aes(intercept=intercept, slope=slope, label=label), 
  #                 hjust=0.875, size=4*0.36, linewidth=0.2, linecolour="gray") +

  geom_point(aes(x=mcb, y=dsc, color=model), size = 0.4) +
  geom_text_repel(aes(x=mcb, y=dsc, label=model), max.overlaps=NA, size = 6*0.36, nudge_x = 0.1,
                  direction = "both", segment.color = "transparent", box.padding = 0.1, force = 1, point.padding = 0) +
  xlab("MCB") +
  ylab("DSC") +
  #labs(title = "1 wk ahead inc case") +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        aspect.ratio = 1,
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
  )

ggsave("figures/decomposition7.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

