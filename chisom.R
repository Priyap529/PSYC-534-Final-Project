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

#### Anxiety Median ####

#inference from median difference and inference from mean difference 

# find medians 
med_drug = median(sai_drug$anxious, na.rm = TRUE)
med_placebo = median(sai_placebo$anxious, na.rm = TRUE)

# graph representation

hist(sai_drug$anxious)
abline(v = med_drug, col="blue")
hist(sai_placebo$anxious)
abline(v = med_placebo, col="red")

# difference score
anxious_diff <- sai_drug$anxious - sai_placebo$anxious

# median difference 
med_diff <- median(anxious_diff, na.rm = TRUE)

#bootstrapping 
boot_med_diff = matrix(,10000,1)

# discrimination bootstrapping

#omit NAs
drug_anxious <- na.omit(sai_drug$anxious)
placebo_anxious <- na.omit(sai_placebo$anxious)

boot_med_dscr <- matrix(,10000,1)

for (reps in 1:10000) {
  resample_drug <- sample(drug_anxious, replace = TRUE)
  resample_placebo <- sample(placebo_anxious, replace = TRUE)
  
  boot_med_diff[reps] <- median(resample_drug) - median(resample_placebo)
}

#histogram bootstrapping

hist(boot_med_diff, main = "Anxious BootStrapping",
     xlab = "Difference Amount", ylab = "Frequency")
abline(v=quantile(boot_med_diff, c(0.025, 0.975)), col="blue")
abline(v = med_diff, col="red")

#### Anxiety Mean ####

# find means 
mean_drug = mean(sai_drug$anxious, na.rm = TRUE)
mean_placebo = mean(sai_placebo$anxious, na.rm = TRUE)

# graph representation
hist(sai_drug$anxious)
abline(v = mean_drug, col="blue")
hist(sai_placebo$anxious)
abline(v = mean_placebo, col="red")


# mean differences
# difference score
anxious_diff <- sai_drug$anxious - sai_placebo$anxious

# median difference 
mean_diff <- mean(anxious_diff, na.rm = TRUE)
