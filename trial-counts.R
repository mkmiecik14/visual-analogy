# Task design
# Matt Kmiecik
# 5 July 2022

# Purpose: estimates trial numbers/condition for each participant

source("r-prep.R") # prepares R workspace

# Loads data - - - -
all_data <- read_rds("output/analogy-data.rds") # includes all vars
vis_data <- read_rds("output/raw-visual-data.rds") # only raw vis data

trial_counts <- vis_data %>% count(ss, rc, inhib, validity)
6*24 # total trials per participant

vis_data %>% count(ss, inhib)
