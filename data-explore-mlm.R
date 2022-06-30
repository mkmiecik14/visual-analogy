# Data Exploration - raw data
# Matt Kmiecik
# 28 June 2022

# Purpose: get a handle on the data from the newly processed RAW visual analogy 
# data for linear mixed effects modeling

source("r-prep.R") # prepares R workspace

# Loads data ----
all_data <- read_rds("output/analogy-data.rds") # includes all vars
vis_data <- read_rds("output/raw-visual-data.rds") # only raw vis data

# code that used to be here was split into separate scripts