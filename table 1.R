##### The data
source("ignore/data/get imputed data.R")

# library(tidyverse)
library(dplyr)
library(finalfit)

# Select variables.
dvt.s <- dvt.i %>% select(dvt, sex, malign, surg, notraum, vein, calfdif3, age, durat, ddimdich, studyid)
dvt.s$dvt <- as.factor(dvt.s$dvt)

table1 <- dvt.s %>%
    summary_factorlist(dependent = "dvt", 
                       explanatory = c("sex", "malign", "surg", "notraum", "vein", "calfdif3", "ddimdich", "age", "durat"),
                       add_dependent_label=T,
                       total_col = T)
table(dvt.s$sex) # For which is which see start.R.

# write.csv(table1, file = "C:/Users/vjong3/surfdrive/Research/GenPred/GenPred-dvt/Tables/table1.csv")

# Alternatively, a summary stratified by study can be obtained by:
# stratified_summary_factorlist <- function(s) {
#     dvt.s[dvt.s$studyid == s, ] %>%
#         summary_factorlist(dependent = "dvt", 
#                            explanatory = c("sex", "malign", "surg", "notraum", "vein", "calfdif3", "ddimdich", "age", "durat"),
#                            add_dependent_label=T,
#                            total_col = T)}
# stratified_table1 <- mapply(stratified_summary_factorlist, s = sort(unique(dvt.s$studyid)), SIMPLIFY = F)