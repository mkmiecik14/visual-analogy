# Data Exploration
# Matt Kmiecik
# 22 June 2021

# Purpose: get a handle on the data

source("r-prep.R") # prepares R workspace
analogy_data <- read_rds("output/analogy-data.rds") # reads in data

# Exploring visual analogy data ----
vis_data <- 
  analogy_data %>% 
  select(ss, contains("rc")) %>%
  pivot_longer(-ss) %>%
  separate(name, into = c("task", "rc", "inhib", "meas", "extra"))

# Filtering down to accuracy data across the four conditions
vis_acc_data <- 
  vis_data %>% 
  filter(meas == "ACC", rc %in% c("1rc", "2rc")) %>% 
  select(-extra)

ggplot(vis_acc_data, aes(value)) +
  geom_histogram(binwidth = .04) +
  theme_minimal() +
  labs(x = "Proportion Correct", y = "Frequency") +
  facet_grid(rc~inhib)

# Summary
vis_acc_sum <-
  vis_acc_data %>%
  filter(complete.cases(value)) %>%
  group_by(rc, inhib) %>%
  summarise(
    M = mean(value), 
    SD = sd(value), 
    N = n(), 
    SEM = SD/sqrt(N),
    LL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[1]),
    UL = as.numeric(t.test(value, conf.level = 0.95)$conf.int[2])
    ) %>%
  ungroup()

# Summary plot
pj <- position_jitter(width = .1)
pn <- position_nudge(x = .3, y = 0)
ggplot(vis_acc_sum, aes(rc, M, group = inhib, color = inhib)) +
  geom_point(data = vis_acc_data, aes(y = value), alpha = 1/3, position = pj) +
  geom_point(position = pn) +
  geom_errorbar(aes(ymin = LL, ymax = UL), width = .2, position = pn) +
  geom_line(position = pn) +
  labs(x = "Relations", "Proportion Corect", caption = "95% CI error bars.") +
  scale_color_manual(values = ghibli_palettes$MononokeMedium) +
  theme_classic() +
  theme(legend.position = "bottom")

# Running multilevel modeling
vis_acc_data_wide <- 
  vis_acc_data %>%
  pivot_wider(id_cols = ss, names_from = c(inhib, rc), values_from = value) %>%
  filter(complete.cases(.)) # deleting listwise

lvl1_data <-
  vis_acc_data_wide %>%
  pivot_longer(-ss) %>%
  separate(name, into = c("inhib", "rc")) %>%
  mutate(inhib = as.factor(inhib), rc = as.factor(rc))

# setting contrasts
contrasts(lvl1_data$inhib)  <- cbind(c1 = c(-.5, .5)) # noninhib > inhib
contrasts(lvl1_data$rc)     <- cbind(c1 = c(5, -.5))  # 1rc > 2rc

# Level 1 models
lvl1_mod <- 
  lvl1_data %>%
  nest_by(ss) %>%
  mutate(mod = list(lm(value ~ 1 + inhib*rc, data = data)))

# Level 1 estimates
lvl1_est <-
  lvl1_mod %>%
  summarise(broom::tidy(mod)) %>%
  ungroup() %>%
  mutate(
    term = gsub("[\\(\\)]", "", term),
    term = gsub("c1", "", term)
    )

# Level 2 data
# prepping between-subject variables

## Inhibition
# Brown-Peterson Intrusions
# Brown-Peterson Index

## Working Memory
# Operation Span
# Reading Span

## Crystallized Knowledge
# General Knowledge
# Text Completion
# Shipley Vocabulary

## Fluid Intelligence
# Number Series
# Raven’s Advanced Progressive Matrices (RAPM)

cog_meas <- 
  analogy_data %>%
  select(
    ss,
    bpindex = BPIndex,
    bpint = BP_Intrusions, 
    ospan = OSpan, 
    rspan = RSpan, 
    numser = NumberSeries, 
    rapm = RAPM, 
    genknow = GenKnow,
    textcomp = TextComp, 
    shipvocab = ShipleyVocab
    )

# Scales the measures and combines z scores into components  
cog_meas_scaled <- 
  cog_meas %>%
  filter(complete.cases(.)) %>%
  mutate(across(.cols = -ss, ~as.numeric(scale(.x)))) %>%
  mutate(
    ic = bpindex * -1, # to put measure in the same direction 
    wm = ospan + rspan,
    k = genknow + textcomp + shipvocab,
    gf = numser + rapm
    )

# Full level 2 data
lvl2_data <- 
  left_join(lvl1_est, cog_meas_scaled, by = "ss") %>%
  select(-std.error, -statistic, -p.value) %>%
  filter(complete.cases(.)) # elimiates ss with vis data, but missing components

# Level 2 models
lvl2_mod <- 
  lvl2_data %>%
  nest_by(term) %>%
  mutate(mod = list(lm(estimate ~ 1 + ic + wm + k + gf, data = data)))

# Level 2 estimates
lvl2_mod %>%
  summarise(broom::tidy(mod, conf.int = TRUE, conf.level = 0.95)) %>%
  ungroup() %>%
  mutate(source = rep(lvl2_mod$term, each = 5)) %>%
  mutate(term = gsub("[\\(\\)]", "", term)) %>%
  select(source, term:conf.high)

# next step is to model d'