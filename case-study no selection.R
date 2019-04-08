remove(list = ls())

##### Functions
library(metamisc) # Version of December 2018 or later is necessary.
source("functions.R")

############ The data
source("ignore/data/get imputed data.R")

############ Model validation
f0 <- dvt ~ malign + calfdif3 + surg 
m0 <- metapred(data = dvt.i,
               strata = "studyid",
               formula = f0,
               scope = f0,
               estFUN = "logistfirth",
               family = binomial,
               perfFUN = list("mse", "bin.cal.int", "cal.slope", "auc"))

# forest(m0)
# forest(m0, perfFUN = "bin.cal.int")
# forest(m0, perfFUN = "cal.slope")
# forest(m0, perfFUN = "auc")


cv0 <- subset(m0)
cv0$stratified.fit
# metamisc:::rema.mp.cv.val(cv0)


m0.mse <- perf(cv0)
m0.auc <- perf(cv0, perfFUN = "auc")
m0.slo <- perf(cv0, perfFUN = "cal.slope")
m0.int <- perf(cv0, perfFUN = "bin.cal.int")

studylabels <- as.character(m0.mse$val.strata)

perf <- data.frame(study = studylabels,
                   MSE       = est.se(m0.mse$estimate, m0.mse$se),
                   AUC       = est.se(m0.auc$estimate, m0.auc$se),
                   Slope     = est.se(m0.slo$estimate, m0.slo$se),
                   Intercept = est.se(m0.int$estimate, m0.int$se))

write.csv(perf, file = "ignore/tables/CS no selection - perf.csv", row.names = F)

coef <- data.frame(study = studylabels,
              formatC(as.matrix(coef(cv0$stratified.fit)), format = "f", flag='0', digits = 2))

write.csv(coef, file = "ignore/tables/CS no selection - fit.csv", row.names = F)

############ the end. ############ 