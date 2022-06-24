# Data preprocessing
# Matt Kmiecik
# Started 22 June 2021

# Purpose: this script brings in data from Excel and saves into RData and CSV

source("r-prep.R") # Prepares R Workspace

# Loads data ----
# This is the cleaned up data in excel
analogy_data <- read_excel("data/analogy-data-may-2017.xlsx", sheet = "forR")

# to better model these data using linear mixed models, I also pulled the raw
# trial by trial data:
raw_data_files <- 
  list.files(path = "data/", pattern = "visual-analogy-data*")

# reads in files and cleans
raw_vis_analogy_data <- 
  raw_data_files %>%
  map(~read_excel(path = paste0("data/", .x), sheet = "raw-data")) %>%
    # CLEANING
    # rc (1=1 relation; 2=2 relations)
    # inhib (1=yes, 2=no)
    # valid (1=valid, 2=invalid)
  map_dfr(
    ~ .x %>% 
      select(
        ss = Subject,
        proc = `Running[Block]`,
        rc_ = stimulus_rc,
        inhib_ = stimulus_inhibition,
        valid_ = stimulus_validity,
        code = stimulus_code,
        acc = stimulus.ACC,
        rt = stimulus.RT
        ) %>%
      filter(proc == "VisualAnalogy") %>% # filters out practice
      rowwise() %>%
      unite(code_check, rc_, inhib_, valid_, sep = "", remove = FALSE) %>%
      mutate(
        code_check = as.numeric(code_check),
        inhib = ifelse(inhib_ == 1, "yes", "no"),
        validity = ifelse(valid_ == 1, "valid", "invalid"),
        same = code==code_check # checks to make sure the order matched
        )
    )

raw_vis_analogy_data %>% filter(same == FALSE) # all matched

# final touches
vis_analogy_data <- 
  raw_vis_analogy_data %>% select(ss, proc, rc = rc_, inhib, validity, acc, rt)

# Saves out data ----
# from already cleaned excel
saveRDS(analogy_data, file = "output/analogy-data.rds")
write_csv(analogy_data, file = "/output/analogy-data.csv")

# from raw visual analogy data
saveRDS(vis_analogy_data, file = "output/raw-visual-data.rds")
write_csv(vis_analogy_data, file = "output/raw-visual-data.csv")

# Clean up scrip objects ----
rm(analogy_data, vis_analogy_data, raw_vis_analogy_data, raw_data_files)

