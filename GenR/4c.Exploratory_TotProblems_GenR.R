#######################################
# 3c.Exploratory Total problems - GenR
#######################################
# Project: Directionality of brain - behaviour associations
# Project is preregistered on osf at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R at @9 and @13 (focus)
# Aim: run CLPMs for total problems with global FA and MD
# Author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)


####
# set environment
####

rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)

dd <- readRDS(paste0(indata, "genr_main_data.rds"))


####
# Pre model prep
####

### factor scaling ###
# this is needed so that the variables are more or less on the same order of magnitude
# FA/MD values are generally 100 times smaller than behavioural ones. --> use scale factor of 100 

scale_factor = 100

dd$md_t1 <- dd$md_t1*scale_factor

dd$md_t2 <- dd$md_t2*scale_factor


dd$fa_t1 <- dd$fa_t1*scale_factor

dd$fa_t2 <- dd$fa_t2*scale_factor



####
# Recoding of vars for lavaan
####
# Before running lavaan, variables need to be coded in a certain way if they are not continuous
# this varies depending on whether the var is exogeneous (ind) or endogeneous (dep)
# more info at https://lavaan.ugent.be/tutorial/cat.html


### Make ordered variables, continuous/numeric ###
# maternal education only here

dd$mat_edu_num <- as.numeric(dd$mat_edu)


### dummy coding for binary/nominal variables ### 

rownames(dd) <- dd$id
dd$id <- NULL


dd2 <- fastDummies::dummy_cols(dd) 


# delete reference vars 
# and dummies with NAs 

dd2$ethn_dutch <- NULL

dd2$sex_girl <- NULL

dd2$ethn_NA <- NULL


#####
# Model specification 
#####
# Specify the same models you used in the main analyses 


tot_fa <- '

# lagged and stability 
tot_t2 + fa_t2 ~ tot_t1 + fa_t1

# covariances
tot_t1 ~~ fa_t1
tot_t2 ~~ fa_t2

# autocorr
tot_t1 ~~ tot_t1
fa_t1 ~~ fa_t1
tot_t2 ~~ tot_t2
fa_t2 ~~ fa_t2


# covs
tot_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2
fa_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'



tot_md <- '

# lagged and stability 
tot_t2 + md_t2 ~ tot_t1 + md_t1

# covariances
tot_t1 ~~ md_t1
tot_t2 ~~ md_t2

# autocorr
tot_t1 ~~ tot_t1
md_t1 ~~ md_t1
tot_t2 ~~ tot_t2
md_t2 ~~ md_t2


# covs
tot_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'


####
# model identification 
####

# run the model
tot_fa_fit <- sem(tot_fa, data = dd2, missing = "fiml", fixed.x= F) 
tot_md_fit <- sem(tot_md, data = dd2, missing = "fiml", fixed.x= F) 

# check the fit

summary(tot_fa_fit, fit.measures = T, estimates = F) 
summary(tot_md_fit, fit.measures = T, estimates = F) 


####
# model estimates
#### 

est_fa <- standardizedSolution(tot_fa_fit)
est_md <- standardizedSolution(tot_md_fit)

est <- cbind(est_fa, est_md)

write.csv2(est, paste0(res, "totalproblems_summarystats.csv"))


est_fa <- as.data.frame(est_fa[2:3, ])
est_md <- as.data.frame(est_md[2:3, ])

est <- rbind(est_fa, est_md)

rownames(est) <- paste0(est$lhs, est$op, est$rhs)

cols <- c("lhs", "rhs", "op")

est <- est[ , !names(est) %in% cols]

est2 <- round(est, digits = 3)

write.csv2(est, paste0(res, "totalproblems_lagged.csv"))
write.csv2(est2, paste0(res, "totalproblems_lagged_rounded.csv"))

