######################
# 3b. attrition analyes GenR 
######################
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: Generation R (wave F9 and F13) 
# AIM OF SCRIPT: Compare on covs the sample with DTI & longi data vs the sample without & invited to the f9
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl), based on a script offered by Dr. Rosa Mulder


########
# Setting environment
########

rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)

dd <- readRDS("genr_main_data.rds")

comp <- readRDS("GenR_attrition_analyses_sample.rds")


# variables: puberty, sex, education, ethnicity 
# puberty & age continuous 
# sex and ethnicity nominal 
# education ordinal


#####
# Check for missingness
#####

missings <- lapply(dd[,c("puberty", "sex", "ethn", "mat_edu")], function(x) length(which(is.na(x))))

missings2 <- lapply(comp[,c("puberty", "sex", "ethn", "mat_edu")], function(x) length(which(is.na(x))))

cbind(missings, missings2) 


######
# Select rows which are not NAs
######

comp_clean <- comp[!is.na(comp$ethn) & !is.na(comp$puberty) & !is.na(comp$mat_edu) & !is.na(comp$sex), ]
dd_clean <- dd[!is.na(dd$ethn) & !is.na(dd$puberty) & !is.na(dd$mat_edu) & !is.na(dd$sex), c("id", "ethn", "sex", "mat_edu", "puberty")] 


######
# Select relevant variables & compare values
######

covs <- c("puberty", "sex", "ethn", "mat_edu")


compare_them <- function(data1,data2,vars) {
  
  names(vars) <- vars
  
  stat <- lapply(vars, function(x) { 
    if(is.numeric(data1[,x])){
      result <- as.numeric(unlist(t.test(data1[,x],data2[,x]))[1:3])
    }else if(is.factor(data1[,x])){
      d1 <- cbind(data1[,x],"d1")
      d2 <- cbind(data2[,x],"d2")
      d  <- as.data.frame(rbind(d1,d2))
      colnames(d) <- c("var","set")
      result <- as.numeric(unlist(chisq.test(d$var, d$set))[1:3])
    }else{
      result <- c(NA,NA,NA)                   #please check if NAs occur, should not happen
    }
  })
  names  <- vars
  save   <- as.data.frame(stat[1:length(stat)], c("t_or_X_stat","df","p"))
  save   <- tFrame(save)
  
}

tab1_subsample <- CreateTableOne(c(covs), data = dd_clean) # subsample

tab1_fullsample <- CreateTableOne(c(covs), data = comp_clean)

comparison <- compare_them(dd_clean, comp_clean, covs) 


#######
# Save
#######

comparison_table <- print(comparison, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE) # no diff.

sumstats_subsample <- print(tab1_subsample, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)

sumstats_fullsample <- print(tab1_fullsample, exact = "stage", quote = FALSE, noSpaces = TRUE, printToggle = FALSE)


## Save to a CSV file

write.csv2(comparison_table, paste0(res, "Attritionanalyses_proportioncomparison.csv"))

write.csv2(sumstats_subsample, paste0(res, "Attritionanalyses_proportions_subsample.csv"))

write.csv2(sumstats_fullsample, paste0(res, "Attritionanalyses_proportions_fullsample.csv"))

