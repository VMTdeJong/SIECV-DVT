remove(list = ls())

##### Functions
# metamisc version of 29 August 2019 or later is necessary.
# install.packages("metamisc", repos="http://R-Forge.R-project.org")
library(metamisc) 
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


### Performance estimates and standard errors
cv0 <- subset(m0)
m0.mse <- perf(cv0)
m0.auc <- perf(cv0, perfFUN = "auc")
m0.slo <- perf(cv0, perfFUN = "cal.slope")
m0.int <- perf(cv0, perfFUN = "bin.cal.int")
studylabels <- as.character(m0.mse$val.strata)
perf <- data.frame(Study = studylabels,
                   # MSE       = est_se(m0.mse$estimate, m0.mse$se),
                   Slope     = est_se(m0.slo$estimate, m0.slo$se),
                   Intercept = est_se(m0.int$estimate, m0.int$se),
                   AUC       = est_se(m0.auc$estimate, m0.auc$se))

ma.auc <- ma(m0.auc)
ma.slo <- ma(m0.slo)
ma.int <- ma(m0.int)
perf_summary_estimates <- c(ma.slo$est, ma.int$est, ma.auc$est)
perf_summary_errors    <- c(ma.slo$se,  ma.int$se,  ma.auc$se)
perf_summary_est_se <- data.frame(t(est_se(perf_summary_estimates, perf_summary_errors)))
colnames(perf_summary_est_se) <- c("Slope", "Intercept", "AUC")

# Intervals
perf_ci.lb <- c(ma.slo$ci.lb, ma.int$ci.lb, ma.auc$ci.lb)
perf_ci.ub <- c(ma.slo$ci.ub, ma.int$ci.ub, ma.auc$ci.ub)
perf_pi.lb <- c(ma.slo$pi.lb, ma.int$pi.lb, ma.auc$pi.lb)
perf_pi.ub <- c(ma.slo$pi.ub, ma.int$pi.ub, ma.auc$pi.ub)
perf_summary <- data.frame(est = perf_summary_estimates, ci.lb = perf_ci.lb, ci.ub = perf_ci.ub,
                           pi.lb = perf_pi.lb, pi.ub = perf_pi.ub)
perf_summary_pi <- format_summary(round(perf_summary, digits = 2))$pi
perf_summary_pi_df <- data.frame(matrix(as.character(perf_summary_pi), nrow = 1))
colnames(perf_summary_pi_df) <- c("Slope", "Intercept", "AUC")

# Combined
perf_summary2 <- rbind(perf, cbind(Study = "Summary effect", perf_summary_est_se), cbind(Study = "Prediction interval", perf_summary_pi_df))
# write.csv(perf_summary2, file = "ignore/tables/CS no selection - perf summary.csv", row.names = F)
# write.csv(perf, file = "ignore/tables/CS no selection - perf.csv", row.names = F)

### Coefficients
m0.ma <- ma(m0, select = "global", method = "REML")
coefs <- est_se(coef(cv0$stratified.fit), se(cv0$stratified.fit))
coefs_re_se <- data.frame(t(est_se(coef(m0.ma), se(m0.ma))))
colnames(coefs_re_se) <- colnames(coefs)

coefs_summary <- data.frame(est = m0.ma$coefficients, ci.lb = m0.ma$ci.lb, ci.ub = m0.ma$ci.ub, pi.lb = m0.ma$pi.lb, pi.ub = m0.ma$pi.ub)
coefs_re_pi <- format_summary(round(coefs_summary, digits = 2))$pi
coefs_re_pi_df <- data.frame(matrix(as.character(coefs_re_pi), nrow = 1))
colnames(coefs_re_pi_df) <- colnames(coefs)

coefs_summary_full <- rbind(coefs, coefs_re_se, coefs_re_pi_df)
rownames(coefs_summary_full) <- c(1:11, "Summary effect", "Prediction interval")

# write.csv(coefs, file = "ignore/tables/CS no selection - fit.csv", row.names = F)
# write.csv(coefs_summary_full, file = "ignore/tables/CS no selection - fit summary.csv", row.names = T)

############ the end. ############ 