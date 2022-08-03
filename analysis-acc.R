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

########################
#                      #
# Cognitive Components #
#                      #
########################

load("output/cog-data.rda") # brings in cognitive components

length(unique(vis_acc_data$ss)) # 213 subjects in analyses above

# drops to 208 due to missing data (will drop further after z-scoring etc.)
cog_data %>% filter(ss %in% unique(vis_acc_data$ss)) 

# prepares with z-scores and composite variables
cog_data_z <- 
  cog_data %>% 
  filter(ss %in% unique(vis_acc_data$ss)) %>% # keeps only ss with vis analogy data 
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

# combines vis acc data with cog data
vis_acc_cog_data <- 
  vis_acc_data %>% 
  left_join(., cog_composites, by = "ss") %>% # should be 179 ss
  mutate(ic = ic*-1) # flips IC so that greater scores = better IC
  
contrasts(vis_acc_cog_data$rc) # proof that contrasts are set
contrasts(vis_acc_cog_data$inhib) # proof that contrasts are set
unique(vis_acc_cog_data$validity) # proof that only valid trials are analyzed

# vis analogy accuracy summary (subject-wise)
acc_model_ss <- 
  cog_composites %>%
  filter(complete.cases(.)) %>% # includes only ss with complete cog data
  left_join(., vis_acc_ss, by = "ss") 

# vis analogy accuracy summary (group-wise)
acc_model_sum <- 
  acc_model_ss %>%
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

# valid only plot (bar plot version)
pd <- position_dodge(width = .5)
ggplot(
  acc_model_sum %>% filter(validity == "valid"), 
  aes(factor(rc), M, group = inhib, fill = inhib)
  ) +
  geom_bar(stat = "identity", position = pd, color = "black", width = .5) +
  geom_errorbar(aes(ymin=M-SEM, ymax = M+SEM), width = .1, position = pd) +
  scale_fill_manual(values = ghibli_palettes$PonyoLight[3:4]) +
  coord_cartesian(ylim = c(.5, 1)) +
  scale_y_continuous(breaks = seq(.5, 1, .1)) +
  labs(
    x = "Relational Numerosity", 
    y = "Proportion Correct", 
    caption = "SEM error bars."
    ) +
  theme_bw() +
  theme(legend.position = "bottom")

# vis analogy acc plot point version 
pn <- position_nudge(x = .2, y = 0)
pj <- position_jitter(width = .1)
ggplot(
  acc_model_sum %>% filter(validity == "valid"), 
  aes(factor(rc), M, group = inhib, color = inhib)
) +
  geom_point(data = acc_model_ss, aes(y = m), position = pj, alpha = 1/3, shape = 16) +
  geom_point(position = pn) +
  geom_errorbar(aes(ymin=M-SEM, ymax = M+SEM), width = .1, position = pn) +
  geom_line(position = pn) +
  scale_color_manual(values = ghibli_palettes$PonyoMedium[3:4]) +
  scale_y_continuous(breaks = seq(0, 1, .2)) +
  labs(
    x = "Relational Numerosity", 
    y = "Proportion Correct", 
    caption = "SEM error bars."
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# MINIMAL MODEL
acc_cog_min_mod <-
  lmer(
    acc ~ 1 + age + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(acc_cog_min_mod) # model summary

# MEDIUM COMPLEXITY MODEL
# - individual slopes for rc and inhib
acc_cog_2_mod <-
  lmer(
    acc ~ 1 + age + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + rc + inhib | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(acc_cog_2_mod) # model summary

# Extracting estimates for table
acc_cog_2_mod_ests <- broom::tidy(acc_cog_2_mod)

acc_cog_2_mod_ests %>% 
  filter(effect == "fixed") %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "rcrc" ~ "rn",
      term == "inhibinhib" ~ "inhib",
      term == "rcrc:inhibinhib" ~ "rn * inhib",
      term == "rcrc:wm"
      TRUE ~ term
      # CONTINUE WITH TABLE
      
    )
    )
  
  

# MAXIMUM MODEL
# - individual slopes for rc and inhib, as well as their interaction
acc_cog_max_mod <-
  lmer(
    acc ~ 
      1 + age + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + rc*inhib | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(acc_cog_max_mod) # model summary
performance::check_model(acc_cog_max_mod)

# Model Comparison
model_anova <- 
  anova(acc_cog_min_mod, acc_cog_2_mod, acc_cog_max_mod) # best model is max_mod
model_anova_tidy <- broom::tidy(model_anova) # converts to df

# saves out for reporting
# uncomment to save out
#write_csv(model_anova_tidy, file = "output/acc-model-anova-tidy.csv")

# WM * inhibition
interact_plot(
  acc_cog_2_mod, 
  pred = wm, 
  modx = inhib, 
  plot.points = FALSE
)

# Bootstrapped zero-order correlations - - - -
# Zero order correlations
set.seed(14) # sets seed for reproducible boostrapping

# preps data
vis_acc_grand <- 
  vis_acc_cog_data %>% 
  group_by(ss) %>%
  summarise(
    va_acc = mean(acc),
    trials = n()
  ) %>%
  ungroup() %>%
  left_join(., cog_composites, by = "ss") %>%
  filter(complete.cases(.)) # ends with n=179
vis_acc_grand %>% filter(trials != 96) # all have 96 trials

# Computes bootstrapped correlations
zero_order_cors <-
  psych::corr.test(
    vis_acc_grand %>% select(-ss, -trials),
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
ggplot(cor_res %>% filter(var1 == "va_acc"), aes(var2, r)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  labs(
    x = "Predictors", 
    y = "r (correlation with total visual analogy acc)", 
    caption = "95% CI error bars."
  ) +
  theme_classic() +
  theme(legend.position = "none")
