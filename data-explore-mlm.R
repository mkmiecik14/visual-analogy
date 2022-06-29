# Data Exploration - raw data
# Matt Kmiecik
# 28 June 2022

# Purpose: get a handle on the data from the newly processed RAW visual analogy 
# data for linear mixed effects modeling

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

# MAXIMAL MODEL
# is too complex to compute because the number of random effects is = to ss
# sdt_max_mod <-
#   lmer(
#     dprime ~ 1 + rc + (1 + rc | ss), 
#     data = sdt_res, 
#     REML = TRUE
#   )
# summary(sdt_max_mod) # model summary


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

# Linear mixed effects modeling here for correct RT





