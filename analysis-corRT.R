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

########################
#                      #
# Cognitive Components #
#                      #
########################

load("output/cog-data.rda") # brings in cognitive components

length(unique(mod_data$ss)) # 213 subjects in analyses above

# drops to 208 due to missing data (will drop further after z-scoring etc.)
cog_data %>% filter(ss %in% unique(mod_data$ss)) 

# prepares with z-scores and composite variables
cog_data_z <- 
  cog_data %>% 
  filter(ss %in% unique(mod_data$ss)) %>% # keeps only ss with vis analogy data 
  mutate(
    across(
      .cols = c(bp_index:shipley_vocab), # excludes sex and age
      .fns = ~as.numeric(scale(.x)), # z-scores columns
      .names = "z_{.col}") # names them with a leading "z_"
  ) %>%
  mutate(
    wm = z_ospan + z_rspan, # working memory (greater scores = better wm)
    gf = z_num_series + z_rapm, # fluid intelligence (greater scores = higher IQ)
    gc = z_gen_know + z_text_comp + z_shipley_vocab, # crys intel (greater scores = higher IQ)
    ic = z_bp_index + z_verbgen_ir # interference control (greater scores = worse IC!)
  )

# visualization of the variables - - - -

# conversion to long format
cog_data_z_long <- cog_data_z %>% select(-sex, -age) %>% pivot_longer(-ss)

# Raw values - variables for plotting
these_vars <- 
  c(
    "bp_index", "verbgen_ir", "ospan", "rspan", "num_series", "rapm", 
    "gen_know", "text_comp", "shipley_vocab"
  )
pj <- position_jitter(width = .1)
ggplot(
  cog_data_z_long %>% filter(name %in% these_vars), 
  aes(name, value)
) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(width = .2, position = position_nudge(x = .3)) +
  facet_wrap(~name, scales = "free") +
  theme_bw()

# Z-scores - variables for plotting
these_vars <- 
  c(
    "z_bp_index", "z_verbgen_ir", "z_ospan", "z_rspan", "z_num_series", "z_rapm", 
    "z_gen_know", "z_text_comp", "z_shipley_vocab"
  )
pj <- position_jitter(width = .1)
ggplot(
  cog_data_z_long %>% filter(name %in% these_vars), 
  aes(name, value)
) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(width = .2, position = position_nudge(x = .3)) +
  theme_bw()


# Composite variables - variables for plotting
these_vars <- c("wm", "ic", "gc", "gf")
pj <- position_jitter(width = .1)
ggplot(
  cog_data_z_long %>% filter(name %in% these_vars), 
  aes(name, value)
) +
  geom_point(position = pj, alpha = 1/3) +
  geom_boxplot(width = .2, position = position_nudge(x = .3)) +
  labs(x = "Composite Variable", y = "Summated Z-Score") +
  theme_bw()

# Linear mixed modeling with cognitive variables - - - -

# narrows down to composites
cog_composites <- cog_data_z %>% select(ss, age, wm, gf, gc, ic)

# combines vis rt data with cog data
mod_cog_data <- 
  mod_data %>% 
  left_join(., cog_composites, by = "ss") %>% # should be 179 ss
  mutate(ic = ic*-1) # flips IC so that greater scores = better IC
  
contrasts(mod_cog_data$rc) # proof that contrasts are set
contrasts(mod_cog_data$inhib) # proof that contrasts are set
unique(mod_cog_data$validity) # proof that only valid trials are analyzed

# MINIMAL MODEL
cog_min_mod <-
  lmer(
    rt ~ 1 + age + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 | ss), 
    data = mod_cog_data, 
    REML = TRUE
  )
summary(cog_min_mod) # model summary

# WM * RC
interact_plot(
  cog_min_mod, 
  pred = wm, 
  modx = rc, 
  plot.points = FALSE
)

# GF * RC
interact_plot(
  cog_min_mod, 
  pred = gf, 
  modx = rc, 
  plot.points = FALSE
)

# GC * RC
interact_plot(
  cog_min_mod, 
  pred = gc, 
  modx = rc, 
  plot.points = FALSE
)

# MEDIUM COMPLEXITY MODEL
# - individual slopes for rc and inhib
cog_2_mod <-
  lmer(
    rt ~ 1 + age + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + rc + inhib | ss), 
    data = mod_cog_data, 
    REML = TRUE
  )
summary(cog_2_mod) # model summary

# MAXIMUM MODEL
# - individual slopes for rc and inhib, as well as their interaction
cog_max_mod <-
  lmer(
    rt ~ 1 + age + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + rc*inhib | ss), 
    data = mod_cog_data, 
    REML = TRUE
  )
summary(cog_max_mod) # model summary
performance::check_model(cog_max_mod)

# Model Comparison
anova(cog_min_mod, cog_2_mod, cog_max_mod) # best model is middle mod

# WM * inhibition
interact_plot(
  cog_2_mod, 
  pred = gc, 
  modx = rc, 
  plot.points = FALSE
)

# Bootstrapped zero-order correlations - - - -
# Zero order correlations
set.seed(14) # sets seed for reproducible boostrapping

# preps data
corRT_grand <- 
  mod_cog_data %>% 
  group_by(ss) %>%
  summarise(
    va_corRT = mean(rt),
    trials = n()
  ) %>%
  ungroup() %>%
  left_join(., cog_composites, by = "ss") %>%
  filter(complete.cases(.)) # ends with n=179
corRT_grand # all will have different trials bc corRT

# Computes bootstrapped correlations
zero_order_cors <-
  psych::corr.test(
    corRT_grand %>% select(-ss, -trials),
    use = "pairwise",
    method = "pearson", 
    adjust = "none",
    ci = TRUE,
    minlength = 100 # extends the abrreviations
  )
zero_order_cors$ci # the results

# correlation results as tibble for plotting
cor_res <- 
  as_tibble(zero_order_cors$ci, rownames = "var") %>%
  separate(var, into = c("var1", "var2"), sep = "-") %>%
  mutate(var2 = fct_relevel(var2, c("wm", "ic", "gc", "gf")))

# correlation plot
ggplot(cor_res %>% filter(var1 == "va_corRT"), aes(var2, r)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  labs(
    x = "Predictors", 
    y = "r (correlation with total visual correct RT)", 
    caption = "95% CI error bars."
  ) +
  theme_classic() +
  theme(legend.position = "none")
