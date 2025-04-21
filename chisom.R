#this is in class activity - chisom

install.packages(c("dplyr", "lattice", "psych", "psychTools"))
library(psychTools)
library(psych)
library(dplyr)
library(lattice)

data(sai)
force(sai)

# MERGE DATA INTO CAFFEINE VARIABLE = DRUG AND CONTROL/NORESPONSE GROUPS = PLACEBO

sai <- mutate(sai,grouped_study = recode(study,"AGES" = "DRUG","CITY"="DRUG","EMIT"="DRUG","SALT"="DRUG","XRAY"="DRUG","Cart" = "PLACEBO","Fast"="PLACEBO","Shed"="PLACEBO", "Raft"="PLACEBO","Shop"="PLACEBO"))

#FILTER DRUG AND PLACEBO VARIABLE INTO NEW VECTOR

sai_filtered <- filter(sai, grouped_study %in% c("DRUG", "PLACEBO"))

#FILTER TOWARDS TIME ONE 
sai_2filtered = filter(sai_filtered, time %in% "1")
                         
# find non-normally distributed variable 


# possible function for finding nonnormal data
summarise(group_by(filter(sai_2filtered, grouped_study == "DRUG"), grouped_study), shapiro_p = shapiro.test(anxious)$p.value, .groups = "drop")


# graph representation

hist(subset(sai_2filtered, grouped_study == "DRUG")$anxious, 
     main = "Histogram of Anxious (DRUG Group)", 
     xlab = "Anxious Scores", col = "lightblue")


# median differences

# figuring out what new function in CRAN to introduce in our code



