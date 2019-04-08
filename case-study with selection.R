remove(list = ls())

##### Functions
# metamisc version of 5 April 2019 or later is necessary.
# install.packages("metamisc", repos="http://R-Forge.R-project.org")
library(metamisc)
library(splines)
source("functions.R")

##### The data
source("ignore/data/get imputed data.R")

##### Model validation
f <- dvt ~ malign + calfdif3 + surg
m0 <- metapred(data = dvt.i,
               strata = "studyid",
               formula = f,
               scope = f,
               estFUN = "logistfirth",
               family = binomial,
               perfFUN = list("mse", "bin.cal.int", "cal.slope", "auc"),
               genFUN = list("abs.mean", "rema.tau"))

##### Model development
# Candidate predictor functions
s <- dvt ~ sex + malign + surg + ddimdich +
    notraum  + vein + calfdif3 + I(log(durat)) + 
    age25 + I(age25^2)  + surg:notraum + age25:malign + ddimdich:sex
    
### Strategy 1: Ignoring heterogeneity
# Model 1: Ignoring heterogeneity
m1 <- update(m0, 
             scope = s,
             genFUN = list("abs.mean", "rema.beta", "rema.tau"))

### Strategy 2: Weighted Meta-Analysis
# Model 2a: Weighted Meta-Analysis: Mean effect
m2a <- update(m1, genFUN = list("rema", "rema.beta", "rema.tau"), lambda = 1)

# Model 2b: Weighted Meta-Analysis: Mean effect + heterogeneity
m2b <- update(m1, genFUN = list("rema", "rema.beta", "rema.tau"), lambda = 1/2)

# Model 2c: Weighted Meta-Analysis: Heterogeneity
m2c <- update(m1, genFUN = list("rema", "rema.beta", "rema.tau"), lambda = 0)

### Strategy 3: Heterogeneity only
# Model 3: Heterogeneity only: parametric
m3a <- update(m1, genFUN = list("coef.var", "rema.beta", "rema.tau"))

# Model 3: Heterogeneity only: non-parametric
m3b <- update(m1, genFUN = list("gmd", "rema.beta", "rema.tau"))

models <- list(m1, m2a, m2b, m2c, m3a, m3b)

source("ignore/models/save models.R")
source("ignore/models/load models.R")

##### Comparison.
### Coefficients
# Which variables are selected, in any model?
selected <- unique(unlist(lapply(lapply(models, coef), names))) 

# It is important that the ordering of the variables is maintained.
# Thus We select them as follows.
# Note that NA means that a variable was not selected in the model: it's coefficient is zero.
all_coefs <- sapply(lapply(models, coef), '[', selected)

# Some of the variables' names are not passed on, as they were not present in the first model.
rownames(all_coefs) <- selected
model_names <- c("Mean", "MA mean", "MA both", "MA heterogeneity", "Coefficient of variation", "Gini MD")
colnames(all_coefs) <- model_names

all_coefs_manuscript <- round(all_coefs, digits = 2)
all_coefs_manuscript[is.na(all_coefs_manuscript)] <- ""
all_coefs_manuscript
data.frame(all_coefs_manuscript, stringsAsFactors = TRUE)

write.csv(data.frame(all_coefs_manuscript, stringsAsFactors = TRUE), file = "ignore/tables/CS with selection - coefs.csv")


### Performance and Generalizability
# MSE
# get_summary(models, "mse", model_names = model_names, digits = 3)

# Calibration intercept
ints <- get_summary(models, "bin.cal.int", model_names = model_names)

# Calibration slope
slopes <- get_summary(models, "cal.slope", model_names = model_names)

# AUC
aucs <- get_summary(models, "auc", model_names = model_names)

# Combined
write.csv(rbind(aucs, ints, slopes), file =  "ignore/tables/CS with selection - ma of perfs.csv")

### Forest plots
# Calibration intercept
# invisible(mapply(forest_list, m = 1:6, stat_id = 2))
mapply(pdf_forest, m = 1:6, stat_id = 2)

# Calibration slope
# invisible(mapply(forest_list, m = 1:6, stat_id = 3))
mapply(pdf_forest, m = 1:6, stat_id = 3)

# AUC
# invisible(mapply(forest_list, m = 1:6, stat_id = 4))
mapply(pdf_forest, m = 1:6, stat_id = 4)

##### The End. ##### 