###########################
# 6. Analyses by sex - GenR 
###########################
# Project: Directionality of brain - behaviour associations
# Project is preregistered on osf at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R at @9 and @13 (focus)
# Aim: rerun main CLPMs by sex
# Author: Lorenza Dall'Aglio (ldallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)
# based on https://lavaan.ugent.be/tutorial/groups.html 


#####
# Set environment
####

rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)

dd <- readRDS("genr_main_data.rds")


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


# NB here don't change anything for sex because we'll group by that! 

dd2$ethn_NA <- NULL


#####
# Model specification 
#####
# you need to test the following models
# 1. Iñternalising - FA
# 2. internalising - MD
# 3. Externalising - FA
# 4. Externalising - MD
# each model contains 2 lagged paths (e.g. from int to FA and from FA to int)
# NB here you need to take away sex because you'll group by that! 

#### Model 1: Internalising - FA #######

# model specification 

m_int_FA <- '

# lagged and stability 
int_t2 + fa_t2 ~ int_t1 + fa_t1

# covariances
int_t1 ~~ fa_t1
int_t2 ~~ fa_t2

# autocorr
int_t1 ~~ int_t1
fa_t1 ~~ fa_t1
int_t2 ~~ int_t2
fa_t2 ~~ fa_t2

# covs
int_t2 ~ ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2
fa_t1 ~ age_mri_t1 + age_diff_t1
'



### Model 2:  Internalising - MD ###

m_int_MD <- '

# lagged and stability 
int_t2 + md_t2 ~ int_t1 + md_t1

# covariances
int_t1 ~~ md_t1
int_t2 ~~ md_t2

# autocorr
int_t1 ~~ int_t1
md_t1 ~~ md_t1
int_t2 ~~ int_t2
md_t2 ~~ md_t2

# covs
int_t2 ~ ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1
'



### Model 3: externalising - FA ###

m_ext_FA <- '
# lagged and stability 
ext_t2 + fa_t2 ~ ext_t1 + fa_t1

# covariances
ext_t1 ~~ fa_t1
ext_t2 ~~ fa_t2

# autocorr
ext_t1 ~~ ext_t1
fa_t1 ~~ fa_t1
ext_t2 ~~ ext_t2
fa_t2 ~~ fa_t2

# covs
ext_t2 ~ ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2
fa_t1 ~ age_mri_t1 + age_diff_t1
'



### Model 4: Externalising - MD ###

m_ext_MD <- '

# lagged and stability 
ext_t2 + md_t2 ~ ext_t1 + md_t1

# covariances
ext_t1 ~~ md_t1
ext_t2 ~~ md_t2

# autocorr
ext_t1 ~~ ext_t1
md_t1 ~~ md_t1
ext_t2 ~~ ext_t2
md_t2 ~~ md_t2

# covs
ext_t2 ~ ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1
'



####
# Model identification 
####
# model identification 
# identify the model
# fixed.x = F because we want to estimate the covariances too to then estimate the lagged (as specified above in the model) 

### internalising - FA ###

m_int_FA_fit <- sem(m_int_FA, data= dd2, missing = "fiml", fixed.x= F, group = "sex")


### internalising - MD ###

m_int_MD_fit <- sem(m_int_MD, data=dd2, missing = "fiml", fixed.x = F, group = "sex")


### Externalising - FA ###

m_ext_FA_fit <- sem(m_ext_FA, data = dd2, missing = "fiml", fixed.x = F, group = "sex")



### Externalising - MD ###

m_ext_MD_fit <- sem(m_ext_MD, data = dd2,  missing = "fiml", fixed.x = F, group = "sex")


######## 
# Testing for measurement invariance
########

# int - FA

fit1 <- sem(m_int_FA, data=dd2, missing = "fiml", fixed.x = F, group = "sex")

fit2 <- sem(m_int_FA, data=dd2, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions")


out1 <- lavTestLRT(fit1, fit2)


# int - MD 

fit3 <- sem(m_int_MD, data=dd2, missing = "fiml", fixed.x = F, group = "sex")

fit4 <- sem(m_int_MD, data=dd2, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions")

out2 <- lavTestLRT(fit3, fit4)



# ext- FA

fit5 <- sem(m_ext_FA, data=dd2, missing = "fiml", fixed.x = F, group = "sex")

fit6 <- sem(m_ext_FA, data=dd2, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions")


out3 <- lavTestLRT(fit5, fit6)



# ext - MD 

fit7 <- sem(m_ext_MD, data=dd2, missing = "fiml", fixed.x = F, group = "sex")

fit8 <- sem(m_ext_MD, data=dd2, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions")


out4 <- lavTestLRT(fit7, fit8)



#####
# save output
#####


all <- rbind(out1, out2, out3, out4)

rownames(all) <- c("int_fa(base)", "int_fa(equal_reg)", "int_md(base)", "int_md(equal_reg)", 
                   "ext_fa(base)", "ext_fa(equal_reg)", "ext_md(base)", "ext_md(equal_reg)")

all


write.csv(all, paste0(res, "CLPM_by_sex_GenR.csv"))


##########
# Check the models which differ by sex (int fa and ext fa)
##########

summary(fit1) 

summary(fit5) 
