library(wrapr)
library(Hmisc)

source("ignore/data/get raw data.R")
    
# Remove all variables with large amount of missing values 
# As imputing half the data set is impractical here.
# This includes the continuous version of d-dimer, for which 80% is missing.
DVT <- DVT.FULL[ , apply(DVT.FULL, 2, function(x) mean(is.na(x))) < .5]

# Non-linear effects can be handled in metapred, so not necessary here:
DVT$age2 <- NULL 
DVT$agecat <- NULL
DVT$wellsdich <- NULL

# We don't need these either
DVT$PID <- NULL
DVT$country <- NULL
DVT$care  <- NULL 

# care, vein and manag may lead to separation
with(DVT, table(studyid, care, dvt))
# with(DVT, table(studyid, vein, dvt)) # might be fixable with Firth's correction
# with(DVT, table(studyid, manag, dvt))

# These may be problematic variables as well:
with(DVT, table(studyid, malign, dvt))
with(DVT, table(studyid, par, dvt))
with(DVT, table(studyid, surg, dvt))

# And a few others we don't need:
DVT$wellsupdate  <- NULL 
DVT$wellsudich  <- NULL 
DVT$wellsprim  <- NULL 
DVT$wellshistdvt  <- NULL 
DVT$LPwells0  <- NULL    
DVT$predprobwells0  <- NULL    
DVT$predprobwells0cat  <- NULL    
DVT$LPadjWells  <- NULL    
DVT$predprobadjWells  <- NULL    
DVT$LPwellsu  <- NULL    
DVT$wellsdichtwo  <- NULL    

DVT$wellsmoddich  <- NULL
DVT$wellsmod  <- NULL
DVT$ageold  <- NULL
DVT$const  <- NULL
DVT$Bconst  <- NULL
DVT$wellstrichdim  <- NULL
DVT$duratdich  <- NULL
DVT$wellstrich  <- NULL
DVT$wellsdichdim  <- NULL
DVT$predprobwellsu  <- NULL

# Calf diff dichotimized at 3 cm cannot be negative. 
# It appears there is a coding error that can be fixed here:
# DVT$calfdif3[DVT$calfdif3 < 0] <- as.numeric(DVT.FULL$calfdif[DVT$calfdif3 < 0] > 3)
# In the new data file, it is suddenly a factor, so we use this instead:
DVT$calfdif3[as.numeric(DVT$calfdif3) < 5] <- NA
levels(DVT$calfdif3) # Impossible values removed. Impossible levels are still there.
DVT$calfdif3 <- droplevels(DVT$calfdif3)
levels(DVT$calfdif3) # Levels removed
table(DVT$calfdif3, as.numeric(DVT$calfdif3)) # 1 is <3cm, 2 is >= 3cm
DVT$calfdif3[is.na(DVT$calfdif3)] <- 
    c("calf difference < 3 cm", "calf difference >= 3 cm")[(DVT.FULL$calfdif[is.na(DVT$calfdif3)] > 3) + 1]


# This leaves some missing values:
apply(DVT, 2, function(x) mean(is.na(x)))

DVT[is.na(DVT$studyid), ] <- NULL
anyNA(DVT$studyid)

# Study 7 = study 13
DVT$studyid[DVT$studyid == "Additional patients from validation study Toll"] <- "Validation study Toll "
DVT$studyid <- droplevels(DVT$studyid)
table(DVT$studyid)
length(table(DVT$studyid)) # We have data from eleven studies.

DVT.to.imp <- # Derived variables we do not need
    DVT[ , -which(colnames(DVT) %in% Cs(wellsdichdim, agegroup, wellstrich, duratdich, wellstrichdim, agedepcutoff))]


attr(DVT$studyid, "value.labels")[1]

DVT.to.imp$dvt <- as.numeric(DVT.to.imp$dvt) - 1

with(DVT, table(ddimdich, studyid, useNA = "ifany"))

table(DVT.FULL$sex)# For table 1.

source("ignore/data/save unimputed data.R")