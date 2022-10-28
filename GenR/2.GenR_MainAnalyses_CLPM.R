######################################################
# Confirmatory analyses GenR: CLPM for int/ext & FA/MD
#######################################################
# Project: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R (wave F9 and F13) 
# Aim of script: Run cross-lagged panel models for global FA and MD with
# internalizing and externalizing problems in the Generation R cohorts
# Author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

#####
# Set environment
####

rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)

dd <- readRDS("genr_main_data.rds")


####
# Pre model preparation
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
# Recoding of variables for lavaan
####
# Before running lavaan, variables need to be coded in a certain way if they are not continuous
# this varies depending on whether the var is exogeneous (ind) or endogeneous (dep)
# more info at https://lavaan.ugent.be/tutorial/cat.html


### Turn ordered variables into continuous/numeric ###
# in our case, this applies only to maternal education

dd$mat_edu_num <- as.numeric(dd$mat_edu)


### dummy coding for binary/nominal variables ### 
# to make dummy columns from factors or character variables, you can use dummy_cols()
# NB first check which vars are factors or characters
# id is chr - change it just temporarily so it is not transformed into a dummy 
# the only nominal vars here are ethnicity and sex

rownames(dd) <- dd$id
dd$id <- NULL

dd2 <- fastDummies::dummy_cols(dd) 
# NB the function dichotomizes everything w/o setting a reference category so you need to take care of that


# delete reference categories, which usually are the largest categories
# for ethn = dutch, for sex = girls
# also delete dummies with NAs 

dd2$ethn_dutch <- NULL
dd2$sex_girl <- NULL
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
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
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
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
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
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
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
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1
'



####
# Model identification 
####
# model identification 
# identify the model
# fixed.x = F because we want to estimate the covariances too to then estimate the lagged (as specified above in the model) 
# fixed.x = T if we want for these to be fixed to the observed values. 
# we are trying both to see which one is giving us a better fit. do not pick according to outcomes but to fit. 

### internalising - FA ###

m_int_FA_fit <- sem(m_int_FA, data= dd2, missing = "fiml", fixed.x= F)

summary(m_int_FA_fit, fit.measures = T, estimates = F) # already a good model fit  


### internalising - MD ###

m_int_MD_fit <- sem(m_int_MD, data = dd2, missing = "fiml", fixed.x = F)

summary(m_int_MD_fit, fit.measures = T, estimates = F) # adequate model fit


### Externalising - FA ###

m_ext_FA_fit <- sem(m_ext_FA, data = dd2, missing = "fiml", fixed.x = F)

summary(m_ext_FA_fit, fit.measures = T, estimates = F) # good model fit already 


### Externalising - MD ###

m_ext_MD_fit <- sem(m_ext_MD, data = dd2,  missing = "fiml", fixed.x = F)

summary(m_ext_MD_fit, fit.measures = T, estimates = F) # not sufficiently good model fit --> check MIs



### 
# Model comparison
####
# this includes testing of alternative models.
# We prespecified in the preregistration, our comparison models
# would be without lagged paths 

#### Internalising - FA ###

comp_int_FA <- '

# stability 
int_t2 ~ int_t1
fa_t2 ~ fa_t1

# covariances
int_t1 ~~ fa_t1
int_t2 ~~ fa_t2

# autocorr
int_t1 ~~ int_t1
fa_t1 ~~ fa_t1
int_t2 ~~ int_t2
fa_t2 ~~ fa_t2

# covs
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2
fa_t1 ~ age_mri_t1 + age_diff_t1
'

comp_int_FA_fit <- sem(comp_int_FA, data = dd2, missing = "fiml", fixed.x = F)


# compare the two nested fits 

lavTestLRT(m_int_FA_fit, comp_int_FA_fit) # the competing model is not sign. different from the hypothesised model
# so we go with the most parsimonious --> the competing model --> the lagged paths must be negligible


#### Internalising - MD ####

comp_int_MD <- '

# stability 
int_t2 ~ int_t1
md_t2 ~ md_t1

# covariances
int_t1 ~~ md_t1
int_t2 ~~ md_t2

# autocorr
int_t1 ~~ int_t1
md_t1 ~~ md_t1
int_t2 ~~ int_t2
md_t2 ~~ md_t2

# covs
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1
'

comp_int_MD_fit <- sem(comp_int_MD, data = dd2, missing = "fiml", fixed.x = F)


# compare the two nested fits 

lavTestLRT(m_int_MD_fit, comp_int_MD_fit) # as above 



### Externalising - FA ###

comp_ext_FA <- '

# stability 
ext_t2 ~ ext_t1
fa_t2 ~ fa_t1

# covariances
ext_t1 ~~ fa_t1
ext_t2 ~~ fa_t2

# autocorr
ext_t1 ~~ ext_t1
fa_t1 ~~ fa_t1
ext_t2 ~~ ext_t2
fa_t2 ~~ fa_t2

# covs
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2
fa_t1 ~ age_mri_t1 + age_diff_t1
'

comp_ext_FA_fit <- sem(comp_ext_FA, data = dd2,  missing = "fiml", fixed.x = F)

# compare the two nested fits 
lavTestLRT(m_ext_FA_fit, comp_ext_FA_fit) 
# also showing no diff. bw the two models --> keep most parsimonious (comp)



#### Externalising - MD ####

comp_ext_MD <- '

# stability 
ext_t2 ~ ext_t1
md_t2 ~ md_t1

# covariances
ext_t1 ~~ md_t1
ext_t2 ~~ md_t2

# autocorr
ext_t1 ~~ ext_t1
md_t1 ~~ md_t1
ext_t2 ~~ ext_t2
md_t2 ~~ md_t2

# covs
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1
'

comp_ext_MD_fit <- sem(comp_ext_MD, data = dd2, missing = "fiml", fixed.x = F)

# compare the two nested fits 

lavTestLRT(m_ext_MD_fit, comp_ext_MD_fit) 
# again the LRT showed that there is no sign differences bw the 2 models -->
# keep most parsimonious (comp)


# SUMMARY: Overall it seems that the models with the lagged paths are not 
# significantly better than the models without the lagged paths
# this means that the most parsimonious models should be used. 
# These are the competing models with no lagged paths. 

# NB the model fits are still not necessarily good 
# (the SRMR is good  but the RMSEAS are not).
# so we need to check the MIs, as prespecified in the preregistration 



#####
# Modification indices
#####

modindices(m_int_MD_fit, sort. = T, minimum.value = 10)
modindices(m_ext_MD_fit, sort. = T, minimum.value = 10)
# sex --> md t1


######
# Model respecification based on MIs 
######
# we need to consider one MI at a time, so let's start with the biggest issues
# model specification 

int_FA_mis <- '

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
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2 
fa_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy 
'



### Model 2:  Internalising - MD ###

int_MD_mis <- '

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
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'



### Model 3: externalising - FA ###

ext_FA_mis <- '
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
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2 
fa_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'



### Model 4: Externalising - MD ###

ext_MD_mis <- '

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
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2 
md_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'

int_MD_mis_fit <- sem(int_MD_mis, data = dd2,  missing = "fiml", fixed.x = F)

summary(int_MD_mis_fit, fit.measures = T, estimates = F) # fairly good fit! 

lavTestLRT(m_int_MD_fit, int_MD_mis_fit) # sign diff (the df diff is worth the 
# loss for the improve in fit)


int_FA_mis_fit <- sem(int_FA_mis, data = dd2,  missing = "fiml", fixed.x = F)

summary(int_FA_mis_fit, fit.measures = T, estimates = F) # also a good fit 

lavTestLRT(m_int_FA_fit, int_FA_mis_fit) # same as above


ext_MD_mis_fit <- sem(ext_MD_mis, data = dd2,  missing = "fiml", fixed.x = F)

summary(ext_MD_mis_fit, fit.measures = T, estimates = F) # fair fit

lavTestLRT(m_ext_MD_fit, ext_MD_mis_fit) #  still worth the difference in df

ext_FA_mis_fit <- sem(ext_FA_mis, data = dd2,  missing = "fiml", fixed.x = F)

summary(ext_FA_mis_fit, fit.measures = T, estimates = F) # this is a good fit! 

lavTestLRT(m_ext_FA_fit, ext_FA_mis_fit) # also worth the difference



######
# Fit indices 
######

fits_all <- list(int_FA_mis_fit, int_MD_mis_fit, ext_FA_mis_fit, ext_MD_mis_fit)

names(fits_all) <- c("int_fa", "int_md", "ext_fa", "ext_md")


# extract fit measures we are interested in 

table_fits <- sapply(fits_all, function(x) fitMeasures(x, c("cfi", "tli", "rmsea", "srmr"))) 

table_fits <- as.data.frame(t(table_fits)) # for saving, transform into 
# data-frame format after transposing (so fit indeces are the cols and models the rows)

table_fits$model <- rownames(table_fits) # create new col with the model name 

table_fits <- table_fits[, c(5, 3, 4, 1, 2)] # change order of cols

table_fits

write.csv(table_fits, paste0(res, "model_fits_mainmodels_GenR.csv"), row.names = F)


# save with rounded decimals too 

rownames(table_fits) <- table_fits$model # turn the col model into table_fits 
# cause non numeric (cannot be rounded)

table_fits$model <- NULL

table_fits <- round(table_fits, digits = 3) # round to 3 decimals

table_fits

write.csv(table_fits, paste0(res, "model_fits_mainmodels_rounded_GenR.csv"))



#######
# Plots of var cov matrix correspondence 
#######

#### observed var cov matrix ####


### plots ###

## residual plots

plot_matrix <- function(matrix_toplot){
  corrplot::corrplot(matrix_toplot, is.corr = FALSE,
                     type = 'lower',
                     order = "original",
                     tl.col='black', tl.cex=.75)
}


png(paste0(res, "/residualsVarCovMatrix_std_int_FA.png"))
plot_matrix(lavResiduals(int_FA_mis_fit)$cov.z) 
dev.off()


png(paste0(res, "/residualsVarCovMatrix_std_int_MD.png"))
plot_matrix(lavResiduals(int_MD_mis_fit)$cov.z) 
dev.off()



# depict for ext

png(paste0(res, "/residualsVarCovMatrix_std_ext_FA.png"))
plot_matrix(lavResiduals(ext_FA_mis_fit)$cov.z) 
dev.off()

png(paste0(res, "/residualsVarCovMatrix_std_ext_MD.png"))
plot_matrix(lavResiduals(ext_MD_mis_fit)$cov.z) 
dev.off()


######
# Get estimates
######
# NB we're looking at the ones with the lagged paths only
# because those are at the center of our hypotheses

output_std <- lapply(fits_all, function(x) standardizedSolution(x)) # get std estimates, ci and p vals info 

output_std2 <- as.data.frame(output_std) # change format for saving 


write.csv(output_std2, paste0(res, "summarystats_main_genr.csv"), row.names = F) # save all summary stats for all the models 


output_std3 <- output_std2[2:3, ] # get the lagged coefficients

output_std3

write.csv(output_std3, paste0(res, "laggedpaths_std_main_genr.csv"), row.names = F)

output_std3$FDR <- as.numeric("NA")

names(output_std3)

output_table <- data.frame(1:8, model = "NA", 
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

out <- output_table[ , c(2, 6, 7, 10, 11, 9, 12)] 

rownames(out) <- out$model

out$model <- NULL

out2 <- round(out, digits = 3)

write.csv(out, paste0(res, "results_main_genr.csv"))
write.csv(out2, paste0(res, "results_main_genr_rounded.csv"))

out2


