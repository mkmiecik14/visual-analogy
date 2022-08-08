# Analysis of the visual analogy task - signal detection theory
# Matt Kmiecik
# 30 June 2022

source("r-prep.R") # prepares R workspace

# Loads data ----
all_data <- read_rds("output/analogy-data.rds") # includes all vars
vis_data <- read_rds("output/raw-visual-data.rds") # only raw vis data

######
#    #
# d' #
#    #
######

# see the psycho::dprime()
# https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html

#* note:
#* d' can only be calculated from the inhibition trials (exclude the non-inhib)
#* this is because a comparison must be made between valid and invalid trials
#* given that invalid trials must contain inhibition, it would not be possible
#* to calculate d' from this.
#* 
#* 1 relational complexity
#* 
#*                      Signal
#*                      Present | Absent  |
#*            Response   VALID  | INVALID |
#*                  YES |  HIT  |   FA    |
#*                  NO  | MISS  |   CR    |
#*                  
#*  repeat ^ for 2 relations
#*                        

# calculates necessary metrics for SDT
sdt_calc <- 
  vis_data %>% 
  filter(inhib == "yes") %>%
  group_by(ss, rc) %>%
  count(validity, acc) %>%
  ungroup() %>%
  mutate(
    sdt = case_when(
      validity == "invalid" & acc == 0 ~ "fa", # false alarm
      validity == "invalid" & acc == 1 ~ "cr", # correct rejection
      validity == "valid" & acc == 0 ~ "miss", # miss
      validity == "valid" & acc == 1 ~ "hit" # hit
    )
  )

# vis_data %>% 
#   filter(ss == 1325) %>% 
#   filter(inhib == "yes") %>% 
#   group_by(rc) %>% 
#   count(validity,acc)

# wide format
sdt_data <- 
  sdt_calc %>%
  pivot_wider(id_cols = c(ss, rc), names_from = sdt, values_from = n) %>%
  # replace all NAs with 0 as participants that didn't have a trial that qualified
  # got accidentally replaced with NAs (see above step with the case_when)
  mutate(across(c(fa, cr, miss, hit), ~replace_na(., 0))) %>%
  mutate(trials = fa+cr+miss+hit)

# as a check, all participants have 48 trials per row
unique(sdt_data$trials)

# d' calculation here
dprimes <- 
  psycho::dprime(
    sdt_data$hit, 
    sdt_data$fa, 
    sdt_data$miss, 
    sdt_data$cr, 
    adjusted = TRUE
  )

# combines dprime estimates with the rest of the data
sdt_res <- 
  as_tibble(cbind(sdt_data, dprimes)) %>%
  mutate(rc = factor(rc))

# SAVES OUT FOR LARA
vis_dprime_ss <- 
  sdt_res %>% 
  pivot_wider(
    id_cols = ss, 
    names_prefix = "rc_", 
    names_from = rc, 
    values_from = dprime
  ) %>%
  mutate(total_dprime = (rc_1+rc_2)/2)
# saves out
# uncomment to save out
# write_csv(vis_dprime_ss, file = "output/vis-dprime-ss.csv")

# summary measures
sdt_res_sum <-
  sdt_res %>%
  select(ss, rc, dprime, beta, aprime, bppd, c) %>%
  pivot_longer(cols = c(-ss, -rc)) %>%
  group_by(rc, name) %>%
  summarise(
    M = mean(value),
    SD = sd(value),
    N = n(),
    SEM = SD/sqrt(N),
    LL = quantile(value, .025, na.rm = TRUE),
    UL = quantile(value, .975, na.rm = TRUE)
  ) %>%
  ungroup()

# summary measures
ggplot(sdt_res_sum, aes(factor(rc), M, group = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin=M-SEM, ymax = M+SEM), width = .2) +
  geom_line() +
  theme_minimal() +
  facet_wrap(~name, scales = "free")

pj <- position_jitter(width = .1)
pn <- position_nudge(x = .3)
ggplot(sdt_res_sum %>% filter(name == "dprime"), aes(factor(rc), M, group = 1)) +
  geom_point(
    data = sdt_res, 
    aes(factor(rc), dprime), 
    alpha = 1/3,
    position = pj) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(position = pn) +
  geom_errorbar(aes(ymin=M-SEM, ymax = M+SEM), width = .1, position = pn) +
  geom_line(position = pn) +
  labs(x = "Relational Complexity", y = "d'", caption = "SEM error bars") +
  theme_minimal()

# d' linear mixed effects modeling

# sets contrast
contrasts(sdt_res$rc) <- cbind(rc = c(.5, -.5)) # acc 1 > 2

# MINIMAL MODEL
sdt_min_mod <-
  lmer(
    dprime ~ 1 + rc + (1 | ss), 
    data = sdt_res, 
    REML = TRUE
  )
summary(sdt_min_mod) # model summary
performance::check_model(sdt_min_mod)

# MAXIMAL MODEL
# is too complex to compute because the number of random effects is = to ss
# sdt_max_mod <-
#   lmer(
#     dprime ~ 1 + rc + (1 + rc | ss),
#     data = sdt_res,
#     REML = TRUE
#   )
# summary(sdt_max_mod) # model summary

########################
#                      #
# Cognitive Components #
#                      #
########################

load("output/cog-data.rda") # brings in cognitive components

mod_data <- sdt_res # helps with copy/paste from previous script

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

# combines vis acc data with cog data
mod_cog_data <- 
  mod_data %>% 
  left_join(., cog_composites, by = "ss") %>% # should be 179 ss
  mutate(ic = ic*-1) # flips IC so that greater scores = better IC
  
contrasts(mod_cog_data$rc) # proof that contrasts are set

# vis analogy corRT summary (subject-wise)
dp_model_ss <- 
  cog_composites %>%
  filter(complete.cases(.)) %>% # includes only ss with complete cog data
  left_join(., sdt_res, by = "ss") 

# vis analogy corRT summary (group-wise)
dp_model_sum <- 
  dp_model_ss %>%
  group_by(rc) %>%
  summarise(
    M = mean(dprime), 
    SD = sd(dprime), 
    N = n(), 
    SEM = SD/sqrt(N), 
    LL = quantile(dprime, .025, na.rm = TRUE),
    UL = quantile(dprime, .975, na.rm = TRUE)
  ) %>%
  ungroup()

# valid only plot (bar plot version)
pd <- position_dodge(width = .5)
ggplot(
  dp_model_sum, 
  aes(rc, M)
) +
  geom_bar(
    stat = "identity", 
    position = pd, 
    color = "black", 
    width = .5,
    fill = "lightgrey"
    ) +
  geom_errorbar(
    aes(ymin=M-SEM, ymax = M+SEM), 
    width = .1, 
    position = pd
    ) +
  scale_y_continuous(minor_breaks = NULL) +
  labs(
    x = "Relational Numerosity", 
    y = "d'", 
    caption = "SEM error bars."
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# vis analogy corRT plot point version 
pn <- position_nudge(x = .2, y = 0)
pj <- position_jitter(width = .1)
ggplot(dp_model_sum, aes(rc, M, group = 1)) +
  geom_point(data = dp_model_ss, aes(y = dprime), position = pj, alpha = 1/3, shape = 16) +
  geom_point(position = pn) +
  geom_errorbar(aes(ymin=M-SEM, ymax = M+SEM), width = .1, position = pn) +
  geom_line(position = pn) +
  labs(
    x = "Relational Numerosity", 
    y = "d'", 
    caption = "SEM error bars."
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# MINIMAL MODEL
cog_min_mod <-
  lmer(
    dprime ~ 1 + age + rc*wm + rc*gf + rc*gc + rc*ic + (1 | ss), 
    data = mod_cog_data, 
    REML = TRUE
  )
summary(cog_min_mod) # model summary
performance::check_model(cog_min_mod)

# Extracting estimates for table
cog_min_mod_ests <- 
  broom::tidy(cog_min_mod, conf.int = TRUE, conf.level = 0.95)

# fixed effects table
cog_min_mod_fixed <- 
  cog_min_mod_ests %>% 
  filter(effect == "fixed") %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "rcrc" ~ "rn",
      term == "rcrc:wm" ~ "rn * wm",
      term == "rcrc:gf" ~ "rn * gf",
      term == "rcrc:gc" ~ "rn * gc",
      term == "rcrc:ic" ~ "rn * ic",
      TRUE ~ term
    )
  ) %>%
  # reorders for nicer table
  select(
    effect, 
    term, 
    b = estimate, 
    LL = conf.low, 
    UL = conf.high, 
    SE = std.error, 
    t = statistic, 
    df, 
    p = p.value
  )
# writes out to csv
# uncomment to save out
#write_csv(cog_min_mod_fixed, file = "output/dprime-model-fixed-effects.csv")

# random effects
cog_min_mod_random <- 
  cog_min_mod_ests %>% 
  filter(effect == "ran_pars") %>%
  mutate(effect = "random") %>%
  separate(term, into = c("stat", "terms"), sep = "__") %>%
  mutate(
    terms = case_when(
      terms == "(Intercept)" ~ "Intercept",
      TRUE ~ terms
    )
  ) %>%
  select(effect:estimate)
# writes out to csv
# uncomment to save out
#write_csv(cog_min_mod_random, file = "output/dprime-model-rand-effects.csv")

# Model does not have enough df to model random effect of rc
# cog_min_mod2 <-
#   lmer(
#     dprime ~ 1 + age + rc*wm + rc*gf + rc*gc + rc*ic + (1 + rc | ss), 
#     data = mod_cog_data, 
#     REML = TRUE
#   )
# summary(cog_min_mod2) # model summary
# performance::check_model(cog_min_mod)

# model comparison (not possible here)
# anova(cog_min_mod, cog_min_mod2)

# Bootstrapped zero-order correlations - - - -
# Zero order correlations
set.seed(14) # sets seed for reproducible boostrapping

# preps data
dprime_grand <- 
  mod_cog_data %>% 
  group_by(ss) %>%
  summarise(
    va_dprime = mean(dprime),
    trials = n()
  ) %>%
  ungroup() %>%
  left_join(., cog_composites, by = "ss") %>%
  filter(complete.cases(.)) # ends with n=179
dprime_grand %>% filter(trials != 2) # all will have 2 trials (i.e., conditions)

# Computes bootstrapped correlations
zero_order_cors <-
  psych::corr.test(
    dprime_grand %>% select(-ss, -trials),
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
ggplot(cor_res %>% filter(var1 == "va_dprime"), aes(var2, r)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_hline(yintercept = 0, alpha = 1/3, linetype = 2) +
  labs(
    x = "Predictors", 
    y = "r (correlation with total visual d')", 
    caption = "95% CI error bars."
  ) +
  theme_classic() +
  theme(legend.position = "none")
