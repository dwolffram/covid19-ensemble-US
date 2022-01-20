library(tidyverse)

Sys.setlocale("LC_ALL", "C")

reldiag = function(x, y, alpha = 0.5, n_resamples = 999, digits = 3, region_level = 0.9){
  
  require(isotone)
  pava = function(x,y){
    # In case of ties, isotone::gpava uses the conditional mean instead of quantile, try e.g.,
    # gpava(c(-1,-1,-1),c(-1,0,0),solver = weighted.median,ties = "secondary")
    
    # Wrong fix: The following step replaces y values with the respective quantile in case of ties
    # y = unlist(lapply(split(y,x),function(y) rep(quantile(y,alpha,type = 1),length(y))),use.names = FALSE)
    
    # New fix: Use ranking of predictor values and break ties by ordering the corresponding instances in order of decreasing observations
    ranking = match(1:length(x),order(x,y,decreasing = c(FALSE,TRUE)))
    
    return(gpava(ranking,y,solver = weighted.fractile,p = alpha,ties = "secondary")$x)
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

reliability_diagram <- function(df, model = "COVIDhub-baseline",
                                target = "1 wk ahead inc case", 
                                quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
                                n_resamples = 999){
  df <- df %>%
    filter(model == !!model & target == !!target & quantile %in% quantiles)
  
  # compute recalibration, consistency band and score decomposition
  results <- df %>%
    group_by(quantile) %>%
    do(reldiag(.$value, .$truth, alpha = unique(.$quantile), n_resamples = n_resamples))
  
  # summarize scores and create labels
  scores <- results %>%
    group_by(quantile) %>%
    distinct(across(score:pval_ucond)) %>%
    mutate(label = paste0(c("QS ", "uMCB ","cMCB ","DSC ","UNC "),
                          round(c(score, umcb, cmcb, dsc, unc), digits = 3),
                          c("", paste0(" [p = ", round(pval_ucond, digits = 3),"]"), "", "", ""),
                          c("", "", paste0(" [p = ", round(pval_cond, digits = 3),"]"), "", ""),
                          collapse = "\n"))
  
  # needed to ensure square facets with equal x and y limits
  facet_lims <- results %>%
    group_by(quantile) %>%
    summarize(mn = min(c_across(c(x, x_rc, lower, upper))), 
              mx = max(c_across(c(x, x_rc, lower, upper))))
  
  options(repr.plot.width=15, repr.plot.height=5*ceiling(length(quantiles)/3), repr.plot.res = 400)
  
  if(length(quantiles) == 1){
    options(repr.plot.width=6, repr.plot.height=6, repr.plot.res = 200)
  }
  
  ggplot(results, aes(x, x_rc)) +
    facet_wrap("quantile", scales="free", ncol=3) +
    #geom_point(aes(x, y), alpha=0.2) +
    geom_abline(intercept = 0 , slope = 1) +
    geom_point(color = "red") +
    geom_step(color = "red", direction = "vh") +    
    geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
    geom_rug(sides = "b", alpha = 0.2) +
    geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
    geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
    xlab("Forecast value") +
    ylab("Conditional quantile") +
    labs(title = paste0(model, ":\n", target))  +
    geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
               size = 4, hjust = 0, vjust = 1, label.size = NA, alpha=0, label.padding = unit(1, "lines")) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_gray(base_size = 18)    
}

eval_date <- '2022-01-03'

df <- read.csv(paste0("evaluation/", eval_date, "_df_processed.csv")) %>%
  filter(location == "US")

reliability_diagram(df, model = "KITmetricslab-select_ensemble", target = "1 wk ahead inc death", 
                    quantiles = c(0.5), n_resamples = 99) + theme_bw()

reliability_diagram <- function(df, models = c("COVIDhub-baseline"),
                                target = "1 wk ahead inc death", 
                                quantile = 0.5,
                                n_resamples = 999){
  df <- df %>%
    filter(model %in% models,
           target == !!target,
           quantile == !!quantile)
  
  # compute recalibration, consistency band and score decomposition
  results <- df %>%
    group_by(model) %>%
    do(reldiag(.$value, .$truth, alpha = quantile, n_resamples = n_resamples))
  
  # summarize scores and create labels
  scores <- results %>%
    group_by(model) %>%
    distinct(across(score:pval_ucond)) %>%
    mutate(label = paste0(c("QS ", "uMCB ","cMCB ","DSC ","UNC "),
                          round(c(score, umcb, cmcb, dsc, unc), digits = 3),
                          c("", paste0(" [p = ", round(pval_ucond, digits = 3),"]"), "", "", ""),
                          c("", "", paste0(" [p = ", round(pval_cond, digits = 3),"]"), "", ""),
                          collapse = "\n"))
  
  # needed to ensure square facets with equal x and y limits
  facet_lims <- results %>%
    group_by(model) %>%
    summarize(mn = min(c_across(c(x, x_rc, lower, upper))), 
              mx = max(c_across(c(x, x_rc, lower, upper))))
  
  # options(repr.plot.width=15, repr.plot.height=5*ceiling(length(models)/3), repr.plot.res = 400)
  
  # if(length(models) == 1){
  #   options(repr.plot.width=6, repr.plot.height=6, repr.plot.res = 200)
  # }
  
  ggplot(results, aes(x, x_rc)) +
    facet_wrap("model", ncol=3) +
    #geom_point(aes(x, y), alpha=0.2) +
    geom_abline(intercept = 0 , slope = 1, colour="grey70") +
    geom_point(color = "red", size=0.5) +
    # geom_step(color = "red", direction = "vh") +    
    geom_line(color = "red") + 
    geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
    geom_rug(sides = "b", alpha = 0.2, size = 0.25) +
    geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
    geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
    xlab("Forecast value") +
    ylab("Conditional quantile") +
    #labs(title = paste0(model, ":\n", target))  +
    geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
               size = 6*0.36, hjust = 0, vjust = 1, label.size = NA, alpha=0, label.padding = unit(1, "lines")) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw(base_size = 11) +
    theme(panel.grid.major = element_line(size = 0.05), 
          panel.grid.minor = element_line(size = 0.05)) +
    coord_fixed()
}



reliability_diagram(df, models = c("COVIDhub-baseline", "COVIDhub-ensemble", 
                                   "KITmetricslab-select_ensemble"), 
                    target = "1 wk ahead inc death", 
                    quantile = 0.5, n_resamples = 99)  

#ggsave("figures/rel_diag_national.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

# State level

df <- read.csv(paste0("evaluation/", eval_date, "_df_processed.csv")) %>%
  filter(location != "US")

reliability_diagram(df, models = c("COVIDhub-baseline", "COVIDhub-ensemble", 
                                   "KITmetricslab-select_ensemble"), 
                    target = "1 wk ahead inc death", 
                    quantile = 0.5, n_resamples = 99) 

#ggsave("figures/rel_diag_states.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)


#### INSET

models = c("COVIDhub-baseline", "COVIDhub-ensemble", "KITmetricslab-select_ensemble")
target = "1 wk ahead inc death"
quantile = 0.5
n_resamples = 99

df <- df %>%
  filter(model %in% models,
         target == !!target,
         quantile == !!quantile)

# compute recalibration, consistency band and score decomposition
results <- df %>%
  group_by(model) %>%
  do(reldiag(.$value, .$truth, alpha = quantile, n_resamples = n_resamples))

# summarize scores and create labels
scores <- results %>%
  group_by(model) %>%
  distinct(across(score:pval_ucond)) %>%
  mutate(label = paste0(c("QS ", "uMCB ","cMCB ","DSC ","UNC "),
                        round(c(score, umcb, cmcb, dsc, unc), digits = 3),
                        c("", paste0(" [p = ", round(pval_ucond, digits = 3),"]"), "", "", ""),
                        c("", "", paste0(" [p = ", round(pval_cond, digits = 3),"]"), "", ""),
                        collapse = "\n"))

# needed to ensure square facets with equal x and y limits
facet_lims <- results %>%
  group_by(model) %>%
  summarize(mn = min(c_across(c(x, x_rc, lower, upper))), 
            mx = max(c_across(c(x, x_rc, lower, upper))))


p <- ggplot(results, aes(x, x_rc)) +
  facet_wrap("model", ncol=3) +
  #geom_point(aes(x, y), alpha=0.2) +
  geom_abline(intercept = 0 , slope = 1, colour="grey70") +
  geom_point(color = "red", size=0.5) +
  # geom_step(color = "red", direction = "vh") +    
  geom_line(color = "red") + 
  geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
  geom_rug(sides = "b", alpha = 0.2, size = 0.25) +
  geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
  geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
  xlab("Forecast value") +
  ylab("Conditional quantile") +
  #labs(title = paste0(model, ":\n", target))  +
  geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
             size = 6*0.36, hjust = 0, vjust = 1, label.size = NA, alpha=0, label.padding = unit(1, "lines")) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05)) +
  coord_fixed()

inset <- ggplot(results, aes(x)) +
  facet_wrap("model", ncol=3) +
  geom_histogram(fill=NA, col="black") +
  theme_classic() +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())

ggdraw(p) +
  draw_plot(inset, .45, .45, .5, .5)


p <- ggplot(results, aes(x, x_rc, group=model)) +
  facet_wrap(~model, ncol=3) +
  #geom_point(aes(x, y), alpha=0.2) +
  geom_abline(intercept = 0 , slope = 1, colour="grey70") +
  #geom_point(color = "red", size=0.5) +
  # geom_step(color = "red", direction = "vh") +    
  geom_line(color = "red") + 
  geom_smooth(aes(ymin = lower, ymax = upper), linetype = 0, stat = "identity", fill = "skyblue3") +
  # geom_rug(sides = "b", alpha = 0.2, size = 0.25) +
  geom_blank(data = facet_lims, aes(x = mx, y = mx)) +
  geom_blank(data = facet_lims, aes(x = mn, y = mn)) +
  xlab("Forecast value") +
  ylab("Conditional quantile") +
  #labs(title = paste0(model, ":\n", target))  +
  geom_label(data = scores, mapping = aes(x = -Inf, y = Inf, label = label),
             size = 6*0.36, hjust = 0, vjust = 1, label.size = NA, alpha=0, label.padding = unit(1, "lines")) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_line(size = 0.05), 
        panel.grid.minor = element_line(size = 0.05)) +
  coord_fixed()

get_inset <- function(df, ...){
  ggplot(df, aes(x)) +
    geom_histogram(fill="gray", col="black", size=0.2, bins = 8) +
    theme_classic( base_size=5.5) +
    theme(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE))
}

# get_inset(subset(results, model == "COVIDhub-ensemble"))

annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) 
{
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

inset_plot <- get_inset(subset(results, model == "COVIDhub-ensemble"))

p + annotation_custom2(grob=ggplotGrob(inset_plot), 
                     data = subset(results, model == "COVIDhub-ensemble"),
                     ymin = -4000, ymax=2500, xmin=10000, xmax=15000)




get_annotation <- function(df, model){
  inset_plot <- get_inset(df)
  annotation_custom2(grob=ggplotGrob(inset_plot), 
                     data = subset(df, model == unique(df$model)),
                     ymin = 0, ymax=750, xmin=1500, xmax=2750)
                     #ymin = -3000, ymax=2500, xmin=10000, xmax=15000)
}

insets <- results %>%
  group_by(model) %>%
  group_map(get_annotation, .keep=TRUE)

         
p + insets    

ggsave("figures/rel_diag_inset_states.pdf", width=180, height=70, unit="mm", device = "pdf", dpi=300)

