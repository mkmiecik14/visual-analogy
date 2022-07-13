# Preprocessing of cognitive components data
# Matt Kmiecik
# 13 July 2022

# Purpose: to preprocess and prepare the external congitive component data and
# demographics

source("r-prep.R") # Prepares R Workspace

# Loads data - - - -
# This is the cleaned up data in excel
analogy_data <- read_excel("data/analogy-data-may-2017.xlsx", sheet = "forR")

# preparing cognitive data
cog_data <- 
  analogy_data %>% 
  select(
    ss, # subject number
    sex, # sex (is 1 male?)
    age, # age (in years)
    bp_index = BPIndex, # interference (greater scores indicate more interference)
    bp_trial1 = BP_Trial1, # interference
    bp_intrusions = BP_Intrusions, # interference
    verbgen_ir = VerbGen_IR, # in milliseconds (interference); higher IR = less able to resolve interference
    ospan = OSpan, # working memory
    rspan = RSpan, # working memory
    num_series = NumberSeries, # fluid intelligence
    rapm = RAPM, # fluid intelligence
    gen_know = GenKnow, # crystallized intelligence
    text_comp = TextComp, # crystallized intelligence 
    shipley_vocab = ShipleyVocab # crystallized intelligence
    ) # z-scoring must be completed in analysis scripts due to attrition

# saving out data
save(cog_data, file = "output/cog-data.rda") # rda
write_csv(cog_data, file = "output/cog-data.csv") # csv

# cleaning up workspace
rm(analogy_data, cog_data)
