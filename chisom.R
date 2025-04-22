#this is in class activity - chisom

# install.packages(c("dplyr", "lattice", "psych", "psychTools"))

library(psychTools)
library(psych)
library(dplyr)
library(lattice)

data(sai)
force(sai)

# MERGE DATA INTO CAFFEINE VARIABLE = DRUG AND CONTROL/NORESPONSE GROUPS = PLACEBO

sai <- mutate(sai,grouped_study = recode(study,"AGES" = "DRUG","CITY"="DRUG","EMIT"="DRUG","SALT"="DRUG","XRAY"="DRUG","Cart" = "PLACEBO","Fast"="PLACEBO","Shed"="PLACEBO", "Raft"="PLACEBO","Shop"="PLACEBO"))

#FILTER DRUG AND PLACEBO VARIABLE INTO NEW VECTOR AND BY TIME ONE 

sai_filtered <- filter(sai, grouped_study %in% c("DRUG", "PLACEBO"), time %in% "1")


#SEPARATE INTO DATASET FOR DRUG AND PLACEBO VARIABLES 
sai_drug = filter(sai_filtered, grouped_study == "DRUG")
sai_placebo = filter(sai_filtered, grouped_study == "PLACEBO")


# find non-normally distributed variable - do we have to make code determining how we found this?

#### Anxiety ####

#inference from median difference and inference from mean difference 

# find medians 
med_drug = median(sai_drug$anxious, na.rm = TRUE)
med_placebo = median(sai_placebo$anxious, na.rm = TRUE)

# graph representation

hist(sai_drug$anxious)
abline(v = med_drug, col="blue")
hist(sai_placebo$anxious)
abline(v = med_placebo, col="red")

# median differences


# figuring out what new function in CRAN to introduce in our code



