#####################################
# 6. Exploratory analyses: models by sex
#####################################
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: Check for differences in coefficients across sexes 
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

#####
# Set environment
#####

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

dd <- readRDS(paste0(indata, "abcd_main_data.Rds"))


####
# Pre model prep
####


### factor scaling ###
# to ensure the variables are on the same order of magnitude

scale_factor = 100

dd$FA_t1_scaled <- dd$FA_t1*scale_factor 

dd$MD_t1_scaled <- dd$MD_t1*scale_factor

dd$FA_t2_scaled <- dd$FA_t2*scale_factor 

dd$MD_t2_scaled <- dd$MD_t2*scale_factor


####
# Re-coding of vars for lavaan
####
# Before running lavaan, variables need to be coded in a certain way if they are not continuous
# more info at https://lavaan.ugent.be/tutorial/cat.html

source("0b.Source_file_CLPMs_ABCD_4.0.R")



#####
# Model specification 
#####
# Since we need to test whether there are sex differences,
# the model specification does not currently include sex. 
# The rest of the model is equal to those tested in the other CLPMs. 


#### Model 1: Internalising - FA #######

# model specification 

m_int_FA <- '

# lagged and stability 
int_t2 + FA_t2_scaled ~ int_t1 + FA_t1_scaled

# covariances
int_t1 ~~ FA_t1_scaled
int_t2 ~~ FA_t2_scaled

# autocorr
int_t1 ~~ int_t1
FA_t1_scaled ~~ FA_t1_scaled
int_t2 ~~ int_t2
FA_t2_scaled ~~ FA_t2_scaled

# covs
int_t2 ~  ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

FA_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

FA_t1_scaled ~ age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'



### Model 2:  Internalising - MD ###

m_int_MD <- '


# lagged and stability 
int_t2 + MD_t2_scaled ~ int_t1 + MD_t1_scaled

# covariances
int_t1 ~~ MD_t1_scaled
int_t2 ~~ MD_t2_scaled

# autocorr
int_t1 ~~ int_t1
MD_t1_scaled ~~ MD_t1_scaled
int_t2 ~~ int_t2
MD_t2_scaled ~~ MD_t2_scaled

# covs
int_t2 ~ ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

MD_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

MD_t1_scaled ~ age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'



### Model 3: externalising - FA ###

m_ext_FA <- '
# lagged and stability 
ext_t2 + FA_t2_scaled ~ ext_t1 + FA_t1_scaled

# covariances
ext_t1 ~~ FA_t1_scaled
ext_t2 ~~ FA_t2_scaled

# autocorr
ext_t1 ~~ ext_t1
FA_t1_scaled ~~ FA_t1_scaled
ext_t2 ~~ ext_t2
FA_t2_scaled ~~ FA_t2_scaled

# covs
ext_t2 ~ ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

FA_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

FA_t1_scaled ~ age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'



### Model 4: Externalising - MD ###

m_ext_MD <- '

# lagged and stability 
ext_t2 + MD_t2_scaled ~ ext_t1 + MD_t1_scaled

# covariances
ext_t1 ~~ MD_t1_scaled
ext_t2 ~~ MD_t2_scaled

# autocorr
ext_t1 ~~ ext_t1
MD_t1_scaled ~~ MD_t1_scaled
ext_t2 ~~ ext_t2
MD_t2_scaled ~~ MD_t2_scaled

# covs
ext_t2 ~ ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

MD_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

MD_t1_scaled ~ age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'


### check for sex differences ####

#1) estimate the same model for males and females (as based on sex assigned at birth) separately  (fit 1)
#2) check for sex differences by restricting to the same regression coeff. 
#4) Compare the fits of the models 
# When the LRT shows no sign. diff. across models, it means there are no sex differences
# otherwise there are and we need to check results by group


#####
# check for sex differences across all models 
#####


#### int - FA ###

fit1 <- sem(m_int_FA, data= dd, missing = "fiml", fixed.x = F, group = "sex")

fit2 <- sem(m_int_FA, data= dd, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions") 

out1 <- lavTestLRT(fit1, fit2) 

# no diff.


#### int - MD ####

fit3 <- sem(m_int_MD, data= dd, missing = "fiml", fixed.x = F, group = "sex")

fit4 <- sem(m_int_MD, data= dd, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions") 


out2 <- lavTestLRT(fit3, fit4) 
# none stat. sign 


#### ext - FA ####

fit5 <- sem(m_ext_FA, data=dd, missing = "fiml", fixed.x = F, group = "sex")

fit6 <- sem(m_ext_FA, data=dd, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions") 


out3 <- lavTestLRT(fit5, fit6) 
# as above


#### ext - MD #####

fit7 <- sem(m_ext_MD, data=dd, missing = "fiml", fixed.x = F, group = "sex")

fit8 <- sem(m_ext_MD, data=dd, missing = "fiml", fixed.x = F, group = "sex",
            group.equal = "regressions") 

out4 <- lavTestLRT(fit7, fit8) 
# as above 


## Overall this indicates that we do not have sex differences in the models

##########
# make table for text
#########

tab <- rbind(out1, out2, out3, out4)
tab

rownames(tab) <- c("int_fa(base)", "int_fa(equal_reg)", "int_md(base)", "int_md(equal_reg)", 
                   "ext_fa(base)", "ext_fa(equal_reg)", "ext_md(base)", "ext_md(equal_reg)")


tab


write.csv(tab, paste0(res, "CLPM_by_sex.csv"))




