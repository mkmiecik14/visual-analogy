# Analysis of the visual analogy task - accuracy
# Matt Kmiecik
# 30 June 2022

source("r-prep.R") # prepares R workspace

# Loads data ----
all_data <- read_rds("output/analogy-data.rds") # includes all vars
vis_data <- read_rds("output/raw-visual-data.rds") # only raw vis data

# Looking at accuracy ----
# subject-wise
vis_acc_ss <- 
  vis_data %>%
  group_by(ss, rc, inhib, validity) %>%
  summarise(m = mean(acc), n = n()) %>%
  ungroup()

ggplot(vis_acc_ss %>% filter(validity == "valid"), aes(m)) +
  geom_histogram(binwidth = .04) +
  coord_cartesian(xlim = c(0,1)) +
  facet_grid(rc~inhib)

# group-wise
vis_acc_sum <-
  vis_acc_ss %>%
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
ggplot(vis_acc_sum, aes(factor(rc), M, group = inhib, color = inhib)) +
  geom_point(
    data = vis_acc_ss, 
    aes(y=m), 
    shape = 1, 
    alpha = 1/2,
    position = pj
  ) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=M-SEM, ymax=M+SEM), width = .2) +
  theme_minimal() +
  facet_wrap(~validity) +
  labs(
    x = "Relational Complexity", 
    y = "Mean Prop. Correct", 
    caption = "SEM error bars."
  ) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium[c(2,4)]) +
  theme(legend.position = "bottom")

# Linear Mixed-Effect Modeling - - - -

# maximal model
vis_acc_data <- 
  vis_data %>% 
  filter(validity == "valid") %>% # filters out invalid
  mutate(
    rc = factor(rc),
    inhib = factor(inhib)
  )

# sets contrasts
contrasts(vis_acc_data$rc) <- cbind(rc = c(.5, -.5)) # acc 1 > 2
contrasts(vis_acc_data$inhib) <- cbind(inhib = c(.5, -.5)) # acc no > yes

# MINIMAL MODEL
acc_min_mod <-
  lmer(
    acc ~ 1 + rc*inhib + (1 | ss), 
    data = vis_acc_data, 
    REML = TRUE
  )
summary(acc_min_mod) # model summary

# MEDIUM COMPLEXITY MODEL
# - individual slopes for rc and inhib
acc_2_mod <-
  lmer(
    acc ~ 1 + rc*inhib + (1 + rc + inhib | ss), 
    data = vis_acc_data, 
    REML = TRUE
  )
summary(acc_2_mod) # model summary

# MAXIMUM MODEL
# - individual slopse for rc and inhib, as well as their interaction
acc_max_mod <-
  lmer(
    acc ~ 1 + rc*inhib + (1 + rc*inhib | ss), 
    data = vis_acc_data, 
    REML = TRUE
  )
summary(acc_max_mod) # model summary

# Model Comparison
anova(acc_min_mod, acc_2_mod, acc_max_mod) # best model is acc_2_mod

# Prepping cognitive component data - - - -
all_data %>%
  select(
    ss, 
    sex, 
    age, 
    starts_with("BP"), 
    OSpan, 
    RSpan, 
    NumberSeries, 
    RAPM, 
    GenKnow, 
    TextComp, 
    ShipleyVocab
    )
