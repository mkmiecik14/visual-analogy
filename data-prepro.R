# Data preprocessing
# Matt Kmiecik
# Started 22 June 2021

# Purpose: this script brings in data from Excel and saves into RData and CSV

source("r-prep.R") # Prepares R Workspace

# Loads data ----
analogy_data <- read_excel("../data/analogy-data-may-2017.xlsx", sheet = "forR")

# Saves out data ----
saveRDS(analogy_data, file = "../output/analogy-data.rds")
write_csv(analogy_data, file = "../output/analogy-data.csv")

# Clean up scrip objects ----
rm(analogy_data)

