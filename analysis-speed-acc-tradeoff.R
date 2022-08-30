# Analysis of the visual analogy task - speed accuracy tradeoff
# Matt Kmiecik
# 30 August 2022

source("r-prep.R") # prepares R workspace

# Loads data - - - -
all_data <- read_rds("output/analogy-data.rds") # includes all vars
vis_data <- read_rds("output/raw-visual-data.rds") # only raw vis data

# Organizes
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

# Linear mixed modeling with cognitive variables - - - -

# narrows down to composites
cog_composites <- cog_data_z %>% select(ss, age, wm, gf, gc, ic)

# combines vis acc data with cog data
vis_acc_cog_data <- 
  vis_acc_data %>% 
  left_join(., cog_composites, by = "ss") %>% # should be 179 ss
  mutate(ic = ic*-1) %>% # flips IC so that greater scores = better IC
  mutate(rt = rt/1000) # rt is in seconds now

contrasts(vis_acc_cog_data$rc) # proof that contrasts are set
contrasts(vis_acc_cog_data$inhib) # proof that contrasts are set
unique(vis_acc_cog_data$validity) # proof that only valid trials are analyzed

# MINIMAL MODEL
mod_1 <-
  lmer(
    acc ~ 1 + rt + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(mod_1) # model summary

mod_2 <-
  lmer(
    acc ~ 1 + rt + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + inhib | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(mod_2) # model summary

mod_2_test <-
  lmer(
    acc ~ 1 + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + inhib | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
summary(mod_2_test) # model summary

mod_3 <-
  lmer(
    acc ~ 1 + rt + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + inhib + rc | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(mod_3) # model summary

mod_4 <-
  lmer(
    acc ~ 1 + rt + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + inhib + rc + rt | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE
  )
summary(mod_4) # model summary
mod_4_allfit <- allFit(mod_4)
mod_4_allfit_OK <- mod_4_allfit[sapply(mod_4_allfit, is, "merMod")]
lapply(mod_4_allfit_OK, function(x) x@optinfo$conv$lme4$messages)

mod_4_bobyqa <-
  lmer(
    acc ~ 1 + rt + rc*inhib*wm + rc*inhib*gf + rc*inhib*gc + rc*inhib*ic + (1 + inhib + rc + rt | ss), 
    data = vis_acc_cog_data, 
    REML = TRUE,
    control = lmerControl(optimizer = "bobyqa")
  )
summary(mod_4_bobyqa) # model summary

mod_4_bobyqa_aug <- augment(mod_4_bobyqa)

ggplot(mod_4_bobyqa_aug %>% filter(ss %in% unique(mod_4_bobyqa_aug$ss)[1:20]), aes(rt, acc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(aes(y = .fitted), linetype = 2, method = "lm", se = FALSE, color = "red") +
  theme_bw() +
  facet_wrap(~ss)

# see: https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
# https://joshua-nugent.github.io/allFit/#STILL_no_convergence

tt <- getME(mod_4, "theta")
ll <- getME(mod_4, "lower")
min(tt[ll==0])

anova(mod_1, mod_2, mod_3, mod_4_bobyqa)
