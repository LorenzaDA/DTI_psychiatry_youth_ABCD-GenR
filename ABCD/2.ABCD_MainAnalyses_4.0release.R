###########################
# 1.  Main analyses: CLPM - ABCD
###########################
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: Run cross-lagged panel models for internalising / externalising problems with global FA and MD
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)


#####
# Set environment
#####

# load relevant source files and data

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

dd <- readRDS(paste0(indata, "abcd_main_data.Rds"))


####
# Pre model prep
####

### factor scaling ###
# this is needed so that the variables are more or less on the same order of magnitude
# FA/MD values are generally 100 times smaller than behavioural ones --> use scale factor of 100 

scale_factor = 100

dd$FA_t1_scaled <- dd$FA_t1*scale_factor 

dd$MD_t1_scaled <- dd$MD_t1*scale_factor

dd$FA_t2_scaled <- dd$FA_t2*scale_factor 

dd$MD_t2_scaled <- dd$MD_t2*scale_factor


### Recoding of vars for lavaan ###

# Before running lavaan, variables need to be coded in a certain way if they are not continuous
# this varies depending on whether the var is exogeneous (ind) or endogeneous (dep)
# more info at https://lavaan.ugent.be/tutorial/cat.html
# these steps are run in the other source file - load it 

source("0b.Source_file_CLPMs_ABCD_4.0.R")


### NB after dummyfying you don't need to put that var as factor type 
# check rationale here https://jslefche.github.io/sem_book/categorical-variables.html


#####
# Model specification 
#####
# you need to test the following models
# 1. Iñternalising - FA
# 2. internalising - MD
# 3. Externalising - FA
# 4. Externalising - MD
# each model contains 2 lagged paths (e.g. from int to FA and from FA to int)
# which we are interested in. Those paths will inform us on our hypotheses
# NB the models used in GenR and identified after MIs were applied to ABCD too


#### Model 1: Internalising - FA #######

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
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
FA_t2_scaled ~ puberty_num + age_t2 
FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
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
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
MD_t2_scaled ~ puberty_num + age_t2 
MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 + 
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 + 
site_t1_site15 + site_t1_site17 +  site_t1_site18 +
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
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
FA_t2_scaled ~ puberty_num + age_t2
FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15  + site_t1_site17 +  site_t1_site18 +
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
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
MD_t2_scaled ~ puberty_num + age_t2
MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'



####
# Model identification 
####
# identify the model you specified
# fixed.x = F because we want to estimate the covariance at baseline too

### internalising - FA ###

# run the model
m_int_FA_fit <- sem(m_int_FA, data = dd, missing = "fiml", fixed.x= F)

# check the fit without looking at estimates
summary(m_int_FA_fit, fit.measures = T, estimates = F) # fit not good enough



### internalising - MD ###

m_int_MD_fit <- sem(m_int_MD, data = dd, missing = "fiml", fixed.x = F) 

summary(m_int_MD_fit, fit.measures = T, estimates = F)


### Externalising - FA ###

m_ext_FA_fit <- sem(m_ext_FA, data = dd, missing = "fiml", fixed.x = F)

summary(m_ext_FA_fit, fit.measures = T, estimates = F) 


### Externalising - MD ###

m_ext_MD_fit <- sem(m_ext_MD, data = dd,  missing = "fiml", fixed.x = F)

summary(m_ext_MD_fit, fit.measures = T, estimates = F) 



### 
# Model comparison
####
# this includes testing of alternative models. 
# We pre-specified in the preregistration that our comparison models
# would be without lagged paths 

#### Internalising - FA ###

comp_int_FA <- '

# stability 
int_t2 ~ int_t1
FA_t2_scaled ~ FA_t1_scaled

# covariances
int_t1 ~~ FA_t1_scaled
int_t2 ~~ FA_t2_scaled

# autocorr
int_t1 ~~ int_t1
FA_t1_scaled ~~ FA_t1_scaled
int_t2 ~~ int_t2
FA_t2_scaled ~~ FA_t2_scaled

# covs
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
FA_t2_scaled ~ puberty_num + age_t2 
FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'

comp_int_FA_fit <- sem(comp_int_FA, data = dd, missing = "fiml", fixed.x = F)

summary(comp_int_FA_fit, fit.measures = T, estimates = F) 


# compare the two nested fits 

lavTestLRT(m_int_FA_fit, comp_int_FA_fit) # no sign. difference - there is model equivalence



#### Internalising - MD ####

comp_int_MD <- '

# stability 
int_t2 ~ int_t1
MD_t2_scaled ~ MD_t1_scaled

# covariances
int_t1 ~~ MD_t1_scaled
int_t2 ~~ MD_t2_scaled

# autocorr
int_t1 ~~ int_t1
MD_t1_scaled ~~ MD_t1_scaled
int_t2 ~~ int_t2
MD_t2_scaled ~~ MD_t2_scaled

# covs
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
MD_t2_scaled ~ puberty_num + age_t2
MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'

comp_int_MD_fit <- sem(comp_int_MD, data=dd, missing = "fiml", fixed.x = F)

# compare the two nested fits 

lavTestLRT(m_int_MD_fit, comp_int_MD_fit) 



### Externalising - FA ###

comp_ext_FA <- '

# stability 
ext_t2 ~ ext_t1
FA_t2_scaled ~ FA_t1_scaled

# covariances
ext_t1 ~~ FA_t1_scaled
ext_t2 ~~ FA_t2_scaled

# autocorr
ext_t1 ~~ ext_t1
FA_t1_scaled ~~ FA_t1_scaled
ext_t2 ~~ ext_t2
FA_t2_scaled ~~ FA_t2_scaled

# covs
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
FA_t2_scaled ~ puberty_num + age_t2
FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15  + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'


comp_ext_FA_fit <- sem(comp_ext_FA, data=dd,  missing = "fiml", fixed.x = F)

# compare the two nested fits 

lavTestLRT(m_ext_FA_fit, comp_ext_FA_fit) 



#### Externalising - MD ####

comp_ext_MD <- '

# stability 
ext_t2 ~ ext_t1
MD_t2_scaled ~ MD_t1_scaled

# covariances
ext_t1 ~~ MD_t1_scaled
ext_t2 ~~ MD_t2_scaled

# autocorr
ext_t1 ~~ ext_t1
MD_t1_scaled ~~ MD_t1_scaled
ext_t2 ~~ ext_t2
MD_t2_scaled ~~ MD_t2_scaled

# covs
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
MD_t2_scaled ~ puberty_num + age_t2 
MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15  + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21
'

comp_ext_MD_fit <- sem(comp_ext_MD, data=dd, missing = "fiml", fixed.x = F)


# compare the two nested fits 

lavTestLRT(m_ext_MD_fit, comp_ext_MD_fit) 

# Summary: Overall it seems that the models with the lagged paths are not 
# significantly better than the models without the lagged paths.
# This suggests that our lagged paths do not make a substantial difference 
# - there is model equivalence when the paths are set to 0 or are not



#####
# Modification indices (MIs)
#####
# Because our model fits were mostly below guidelines for good model fits, 
# we will check modification indeces to have suggestions on what could improve the 
# model fit

modindices(m_int_FA_fit, sort. = T, minimum.value = 10) 
modindices(m_int_MD_fit, sort. = T, minimum.value = 10) 
modindices(m_ext_FA_fit, sort. = T, minimum.value = 10) 
modindices(m_ext_MD_fit, sort. = T, minimum.value = 10) 

# Summary: all suggestions are site related (site loading on t2 DTI values)


######
# Modify models based on MIs 
######
# include the sites loading on t2 dti values


### int - fa ####
m_int_FA_site <- '

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
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
FA_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15  + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 +
site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14 +
site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21
'


m_int_FA_site_fit <- sem(m_int_FA_site, data=dd,  missing = "fiml", fixed.x = F)

summary(m_int_FA_site_fit, fit.measures = T, estimates = F) # fit becomes good! 

lavTestLRT(m_int_FA_site_fit, m_int_FA_fit) # model fit significantly better when compared to the model before MIs



#### int - MD ####


m_int_MD_site <- '


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
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
MD_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'

m_int_MD_site_fit <- sem(m_int_MD_site, data=dd,  missing = "fiml", fixed.x = F)

summary(m_int_MD_site_fit, fit.measures = T, estimates = F) # good model fit

lavTestLRT(m_int_MD_site_fit, m_int_MD_fit) # also sign. better


### ext - FA ### 

m_ext_FA_site <- '
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
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num
FA_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'


m_ext_FA_site_fit <- sem(m_ext_FA_site, data=dd,  missing = "fiml", fixed.x = F)

summary(m_ext_FA_site_fit, fit.measures = T, estimates = F) # good model fit

lavTestLRT(m_ext_FA_site_fit, m_ext_FA_fit)



### externalising - MD ####

m_ext_MD_site <- '

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
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

MD_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'

m_ext_MD_site_fit <- sem(m_ext_MD_site, data=dd,  missing = "fiml", fixed.x = F)

summary(m_ext_MD_site_fit, fit.measures = T, estimates = F)

lavTestLRT(m_ext_MD_fit, m_ext_MD_site_fit) # sign. better



##### 
# Some more checks
#####

# create plot to visualise std residuals for all vars

plot_matrix <- function(matrix_toplot){
  corrplot::corrplot(matrix_toplot, is.corr = FALSE,
                     type = 'lower',
                     order = "original",
                     tl.col='black', tl.cex=.75)
}


# depict under/over-prediction for int
# lavResiduals$cov.z gives the standardised residuals 
# large positive values = model under-predicts the relation 
# large negative values = model over-predicts the relation
# according to researchers |r>.1| is worth further considerations, according to others |r>.2| 
# book by Keith (Multiple regression and beyond) suggesting a better rule of 
# thumb is to focus on the biggest difference in case of model misfit 
# but we do have good model fits so we shouldn't worry

png(paste0(res, "residualsVarCovMatrix_std_int_FA_ABCD.png"))
plot_matrix(lavResiduals(m_int_FA_site_fit)$cov.z) 
dev.off()

png(paste0(res, "residualsVarCovMatrix_std_int_MD_ABCD.png"))
plot_matrix(lavResiduals(m_int_MD_site_fit)$cov.z) 
dev.off()


# depict for ext

png(paste0(res, "residualsVarCovMatrix_std_ext_FA_ABCD.png"))
plot_matrix(lavResiduals(m_ext_FA_site_fit)$cov.z) 
dev.off()

png(paste0(res, "residualsVarCovMatrix_std_ext_MD_ABCD.png"))
plot_matrix(lavResiduals(m_ext_MD_site_fit)$cov.z) 
dev.off()


# overall it seems that some of the relations of interest, as calculated in the model-implied,
# are under-estimated as compared to the observed relations. However, most of the 
# over or under-estimation is not in our main variables of interest and our model fits
# are good, which suggests we should not worry about these patterns. 


#####
# Get fits & estimates 
#####

### prepare a table with the model fits ###

# create a list with all final models 

fits_all <- list(m_int_FA_site_fit, m_int_MD_site_fit, m_ext_FA_site_fit, m_ext_MD_site_fit)

names(fits_all) <- c("int_fa", "int_md", "ext_fa", "ext_md")


# extract fit measures we are interested in 

table_fits <- sapply(fits_all, function(x) fitMeasures(x, c("cfi", "tli", "rmsea", "srmr"))) 

table_fits <- as.data.frame(t(table_fits)) # for saving, transform into data-frame format after transposing (so fit indeces are the cols and models the rows)

table_fits$model <- rownames(table_fits) # create new col with the model name 

table_fits <- table_fits[, c(5, 3, 4, 1, 2)] # change order of cols

table_fits

write.csv(table_fits, paste0(res, "model_fits_mainmodels_ABCD.csv"), row.names = F)


# save with rounded decimals too 

rownames(table_fits) <- table_fits$model # turn the col model into table_fits cause non numeric (cannot be rounded)

table_fits$model <- NULL

table_fits <- round(table_fits, digits = 3) # round to 3 decimals

table_fits

write.csv(table_fits, paste0(res, "model_fits_mainmodels_rounded_ABCD.csv"))



### get estimates and prepare a table for those ###

# get standardised estimates

output_std <- lapply(fits_all, function(x) standardizedSolution(x)) # get std estimates, ci and p vals info 

output_std2 <- as.data.frame(output_std) # change format for saving 

write.csv(output_std2, paste0(res, "summarystats_main_abcd.csv"), row.names = F) # save all summary stats for all the models 


# get lagged coefficients

output_std3 <- output_std2[2:3, ] 
output_std3

write.csv(output_std3, paste0(res, "laggedpaths_std_main_abcd.csv"), row.names = F)


# prepare an output table for the manuscript

output_std3$FDR <- as.numeric("NA")

names(output_std3)

output_table <- data.frame(model = 1:8, 
                           lhs = "NA",
                           op = "NA", 
                           rhs = "NA", 
                           est.std = as.numeric("NA"),
                           se = as.numeric("NA"), 
                           z = as.numeric("NA"), 
                           pvalue = as.numeric("NA"), 
                           ci.lower = as.numeric("NA"), 
                           ci.upper = as.numeric("NA"),
                           fdr = as.numeric("NA"))

output_table


# fill in the table with the coeff obtained

output_table$pvalue <- c(output_std3$int_fa.pvalue, output_std3$int_md.pvalue,
                         output_std3$ext_fa.pvalue, output_std3$ext_md.pvalue)

pvals_adj <- p.adjust(output_table$pvalue, method = "fdr") 

output_table$fdr <- pvals_adj

output_table$lhs <- c(output_std3$int_fa.lhs, output_std3$int_md.lhs,
                      output_std3$ext_fa.lhs, output_std3$ext_md.lhs)

output_table$rhs <- c(output_std3$int_fa.rhs, output_std3$int_md.rhs,
                      output_std3$ext_fa.rhs, output_std3$ext_md.rhs)

output_table$op <- "~"


output_table$est.std <- c(output_std3$int_fa.est.std, output_std3$int_md.est.std,
                          output_std3$ext_fa.est.std, output_std3$ext_md.est.std)


output_table$se <-  c(output_std3$int_fa.se, output_std3$int_md.se,
                      output_std3$ext_fa.se, output_std3$ext_md.se)

output_table$z <-  c(output_std3$int_fa.z, output_std3$int_md.z,
                     output_std3$ext_fa.z, output_std3$ext_md.z)

output_table$ci.lower <- c(output_std3$int_fa.ci.lower, output_std3$int_md.ci.lower,
                           output_std3$ext_fa.ci.lower, output_std3$ext_md.ci.lower)

output_table$ci.upper <- c(output_std3$int_fa.ci.upper, output_std3$int_md.ci.upper,
                           output_std3$ext_fa.ci.upper, output_std3$ext_md.ci.upper)

output_table$model <- paste0(output_table$lhs, " ", output_table$op, " ", output_table$rhs)

output_table


# clean up the table and save

out <- output_table[ , c(1, 5, 6, 9, 10, 8, 11)]  # reorder cols

rownames(out) <- out$model
out$model <- NULL

out

write.csv(out, paste0(res, "results_main_abcd.csv"))


# round coefficients 

out2 <- round(out, digits = 3)

out2

write.csv(out2, paste0(res, "results_main_abcd_roundedcoeff.csv"))



