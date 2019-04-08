remove(list = ls())
source("ignore/data/get unimputed data.R")
library(jomo)

# What's missing?
apply(DVT.to.imp[ , apply(DVT.to.imp, 2, anyNA)], 2, function(x) mean(is.na(x)))

# parameters
nburn = as.integer(10000);
nbetween = as.integer(1);
nimp = as.integer(2);

# data
# y = to impute
# z = for imputation, random effects
# x = for imputation, no random effects
# 1 = level 1 variable (patients...)
# 2 = level 2 variable (clusters, studies)
y1 <-       DVT.to.imp[ , apply(DVT.to.imp, 2, function(x) sum(is.na(x))) != 0]
y2 <- data.frame("typdim" = DVT.to.imp$typdim)
y1$typdim <- NULL
z <- cbind(DVT.to.imp[ , apply(DVT.to.imp, 2, function(x) sum(is.na(x))) == 0], 1)
z$studyid <- NULL # cluster variable should not be in x or z.
clus <- DVT.to.imp$studyid

# Random effects are specified by Z.
set.seed(2311)
DVT.imputed.and.unimputed  <- jomo(Y = y1, Y2 = y2, Z = z, clus = clus, nburn = nburn, nbetween = nbetween, nimp = 2) 

DVT.imputed <- DVT.imputed.and.unimputed[DVT.imputed.and.unimputed$Imputation == 2, ] # We use only one imputation.
DVT.imputed$Imputation <- NULL
DVT.imputed$X1 <- NULL # is an intercept.
DVT.imputed$X2.1 <- NULL # is an intercept.
DVT.imputed$`1`<- NULL # is an intercept.


if (any(DVT.imputed$dvt == 2)) # so that 1 = dvt, 0 = no dvt.
    DVT.imputed$dvt <- as.numeric(DVT.imputed$dvt) - 1

if (any(DVT.imputed$malign == 2)) # so that 1 = malign, 0 = no malign
    DVT.imputed$malign <- as.numeric(DVT.imputed$malign) - 1

if (any(DVT.imputed$par == 2)) # etc
    DVT.imputed$par <- as.numeric(DVT.imputed$par) - 1

if (any(DVT.imputed$surg == 2)) # etc
    DVT.imputed$surg <- as.numeric(DVT.imputed$surg) - 1

if (any(DVT.imputed$tend == 2)) # etc
    DVT.imputed$tend <- as.numeric(DVT.imputed$tend) - 1

if (any(DVT.imputed$leg == 2)) # etc
    DVT.imputed$leg <- as.numeric(DVT.imputed$leg) - 1

if (any(DVT.imputed$calfdif3 == 2)) # etc
    DVT.imputed$calfdif3 <- as.numeric(DVT.imputed$calfdif3) - 1

if (any(DVT.imputed$pit == 2))  # etc
    DVT.imputed$pit <- as.numeric(DVT.imputed$pit) - 1

if (any(DVT.imputed$vein == 2)) # etc
    DVT.imputed$vein <- as.numeric(DVT.imputed$vein) - 1

if (any(DVT.imputed$vein == 2)) # etc
    DVT.imputed$vein <- as.numeric(DVT.imputed$vein) - 1

if (any(DVT.imputed$vein == 2)) # etc
    DVT.imputed$vein <- as.numeric(DVT.imputed$vein) - 1

if (any(DVT.imputed$altdiagn == 2)) # etc
    DVT.imputed$altdiagn <- as.numeric(DVT.imputed$altdiagn) - 1

if (any(DVT.imputed$sex == 2)) # etc
    DVT.imputed$sex <- as.numeric(DVT.imputed$sex) - 1

DVT.imputed$studyid <- DVT.imputed$clus
DVT.imputed$clus <- NULL
DVT.imputed$studyname <- DVT.imputed$studyid
DVT.imputed$studyid <- as.numeric(DVT.imputed$studyname)

DVT.imputed$durat[DVT.imputed$durat < .5] <- .5 # because log(0 or smaller) does not make sense.

source("ignore/data/save imputed data.R")