# Analysis of the visual analogy task - correct RT
# Matt Kmiecik
# 30 June 2022

source("r-prep.R") # prepares R workspace

# Loads data ----
all_data <- read_rds("output/analogy-data.rds") # includes all vars
vis_data <- read_rds("output/raw-visual-data.rds") # only raw vis data

##############
#            #
# CORRECT RT #
#            #
##############

# establishes df
corRT_data <- 
  vis_data %>% 
  filter(acc == 1) # excludes incorrect trials (i.e., selects correct RTs)

# summarize by subject
corRT_data_ss <-
  corRT_data %>%
  group_by(ss, rc, inhib, validity) %>%
  summarise(m = mean(rt), n = n()) %>%
  ungroup()

# histograms
ggplot(corRT_data_ss %>% filter(validity == "valid"), aes(m)) +
  geom_histogram(binwidth = 250) +
  labs(x = "Correct RT (ms)", y = "Frequency", title = "Valid Trials") +
  facet_grid(rc~inhib)

ggplot(corRT_data_ss %>% filter(validity == "invalid"), aes(m)) +
  geom_histogram(binwidth = 250) +
  labs(x = "Correct RT (ms)", y = "Frequency", title = "Invalid Trials") +
  facet_wrap(~rc)

# group-wise
corRT_data_sum <-
  corRT_data_ss %>%
  group_by(rc, inhib, validity) %>%
  summarise(
    M = mean(m), 
    SD = sd(m), 
    N = n(), 
    SEM = SD/sqrt(N), 
    LL = quantile(m, .025, na.rm = TRUE),
    UL = quantile(m, .975, na.rm = TRUE)
  ) %>%
  ungroup() 

# plot
pj <- position_jitter(width = .2)
ggplot(corRT_data_sum, aes(factor(rc), M, group = inhib, color = inhib)) +
  geom_point(
    data = corRT_data_ss, 
    aes(y=m), 
    shape = 1, 
    alpha = 1/2,
    position = pj
  ) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=M-SEM, ymax=M+SEM), width = .1) +
  theme_minimal() +
  facet_wrap(~validity) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium[c(2,4)]) +
  theme(legend.position = "bottom")

# Linear mixed effects modeling - - - -

# preps data
mod_data <- 
  corRT_data %>% 
  filter(validity == "valid") %>% # filters out invalid
  mutate(
    rc = factor(rc),
    inhib = factor(inhib)
  )

# sets contrasts
contrasts(mod_data$rc) <- cbind(rc = c(-.5, .5)) # 2 > 1 rt
contrasts(mod_data$inhib) <- cbind(inhib = c(-.5, .5)) # yes > no rt

# MINIMAL MODEL
min_mod <-
  lmer(
    rt ~ 1 + rc*inhib + (1 | ss), 
    data = mod_data, 
    REML = TRUE
  )
summary(min_mod) # model summary

# MEDIUM COMPLEXITY MODEL
# - individual slopes for rc and inhib
mid_mod <-
  lmer(
    rt ~ 1 + rc*inhib + (1 + rc + inhib | ss), 
    data = mod_data, 
    REML = TRUE
  )
summary(mid_mod) # model summary

# MAXIMUM MODEL
# - individual slopse for rc and inhib, as well as their interaction
max_mod <-
  lmer(
    rt ~ 1 + rc*inhib + (1 + rc*inhib | ss), 
    data = mod_data, 
    REML = TRUE
  )
summary(max_mod) # model summary

# Model Comparison
anova(min_mod, mid_mod, max_mod) # best model is mid mod
