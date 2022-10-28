##########################################
# Exploratory analyses GenR - DTI tracts 
##########################################
# Project: Directionality of brain - behaviour associations
# Project is preregistered on osf at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R at @9 and @13 (focus)
# Aim: run CLPMs for each dti tract in assoc. with int & ext 
# Author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

######
# Set environment
###### 


rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)

dd <- readRDS("genr_main_data.rds")


#####
# factor scaling 
#####

scale_factor = 100

to_match <- c("_md_", "_fa_") 

tracts <- grep(paste0(to_match, collapse = "|"), names(dd), value = T)

# 40 tracts as expected - 10 for MD  * for two time points, 10 for FA * for two time points

dd[tracts] <- lapply(dd[tracts], function(x) x * scale_factor) # scale all tracts



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



#######
# Model specification
#######

# create general models 

m_int  <- '

# lagged and stability 
int_t2 + var_t2 ~ int_t1 + var_t1

# covariances
int_t1 ~~ var_t1
int_t2 ~~ var_t2

# autocorr
int_t1 ~~ int_t1
var_t1 ~~ var_t1
int_t2 ~~ int_t2
var_t2 ~~ var_t2

# covs
int_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
var_t2 ~ puberty + age_mri_t2 + age_diff_t2
var_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'



m_ext <- '
# lagged and stability 
ext_t2 + var_t2 ~ ext_t1 + var_t1

# covariances
ext_t1 ~~ var_t1
ext_t2 ~~ var_t2

# autocorr
ext_t1 ~~ ext_t1
var_t1 ~~ var_t1
ext_t2 ~~ ext_t2
var_t2 ~~ var_t2

# covs
ext_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
var_t2 ~ puberty + age_mri_t2 + age_diff_t2
var_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'


###### 
# Create specific model specifications
######


# create empty lists where to put the model specifications for each tract

specifications_int <- list()
specifications_ext <- list()

# create vector for the substitution of the var specifications into tract names 


names <- c("cgc_fa_", "cgh_fa_", "cst_fa_", "atr_fa_", "unc_fa_", "ilf_fa_", 
           "ifo_fa_", "slf_fa_", "fma_fa_", "fmi_fa_", "cgc_md_", "cgh_md_",
           "cst_md_", "atr_md_", "unc_md_", "ilf_md_", "ifo_md_",
           "slf_md_", "fma_md_", "fmi_md_")


# loop to create the various model specifications per syndrome scale 

for (i in names){
  specifications_int[i] <- gsub("var_", i, m_int) # gsub changes the "var_" string into every element of names in the m_int specification
  specifications_ext[i] <- gsub("var_", i, m_ext) # as above, but for ext specifications
}

specifications_int
specifications_ext


names(specifications_int) <- paste0(names(specifications_int), "int") # add to the title of each element of the list "int"
names(specifications_ext) <- paste0(names(specifications_ext), "ext")




#######
# Model evaluation - fit
#######

# fit the model 

fits_int <- sapply(specifications_int, function(x) sem(x, data = dd2, missing = "fiml", fixed.x = F)) # this will take a while! 

fits_ext <- sapply(specifications_ext, function(x) sem(x, data = dd2, missing = "fiml", fixed.x = F))

fits_all <- append(fits_int, fits_ext)

table_fits <- sapply(fits_all, function(x) fitMeasures(x, c("cfi", "tli", "rmsea", "srmr")))

table_fits <- as.data.frame(t(table_fits)) # for saving, transform into dataframe format after transposing (so fit indices are the cols and models the rows)

table_fits <- round(table_fits, digits = 3) # round to 3 decimals - this doesn't come off right in mac 

table_fits$model <- rownames(table_fits) # create new col with the model name 

table_fits <- table_fits[, c(5, 3, 4, 1, 2)] # change order of cols

table_fits # model fits are good!!

write.csv(table_fits, paste0(res, "model_fits_tracts.csv"), row.names = F)



#########
# Model estimates
#########


# get estimates 

output_std <- lapply(fits_all, function(x) standardizedSolution(x)) # get std estimates, ci and p vals info 

output_std2 <- as.data.frame(output_std) # change format for saving 

write.csv(output_std2, paste0(res, "summarystats_std_tracts.csv"), row.names = F) # save all summary stats for all the models 

output_std3 <- output_std2[2:3, ] # get the lagged coefficients
output_std3

write.csv(output_std3, paste0(res, "laggedpaths_std_tracts.csv"), row.names = F)

output_std3$FDR <- as.numeric("NA")

names(output_std3)

output_table <- data.frame(1:80, model = "NA", 
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







output_table$pvalue <- c(output_std3$cgc_fa_int.pvalue, output_std3$cgh_fa_int.pvalue, 
                         output_std3$cst_fa_int.pvalue, output_std3$atr_fa_int.pvalue, 
                         output_std3$unc_fa_int.pvalue, output_std3$ilf_fa_int.pvalue, 
                         output_std3$ifo_fa_int.pvalue, output_std3$slf_fa_int.pvalue, 
                         output_std3$fma_fa_int.pvalue, output_std3$fmi_fa_int.pvalue,
                         
                         
                         output_std3$cgc_md_int.pvalue, output_std3$cgh_md_int.pvalue, 
                         output_std3$cst_md_int.pvalue, output_std3$atr_md_int.pvalue, 
                         output_std3$unc_md_int.pvalue, output_std3$ilf_md_int.pvalue, 
                         output_std3$ifo_md_int.pvalue, output_std3$slf_md_int.pvalue, 
                         output_std3$fma_md_int.pvalue, output_std3$fmi_md_int.pvalue, 
                         
                         output_std3$cgc_fa_ext.pvalue, output_std3$cgh_fa_ext.pvalue, 
                         output_std3$cst_fa_ext.pvalue, output_std3$atr_fa_ext.pvalue, 
                         output_std3$unc_fa_ext.pvalue, output_std3$ilf_fa_ext.pvalue, 
                         output_std3$ifo_fa_ext.pvalue, output_std3$slf_fa_ext.pvalue, 
                         output_std3$fma_fa_ext.pvalue, output_std3$fmi_fa_ext.pvalue,
                         
                         
                         output_std3$cgc_md_ext.pvalue, output_std3$cgh_md_ext.pvalue, 
                         output_std3$cst_md_ext.pvalue, output_std3$atr_md_ext.pvalue, 
                         output_std3$unc_md_ext.pvalue, output_std3$ilf_md_ext.pvalue, 
                         output_std3$ifo_md_ext.pvalue, output_std3$slf_md_ext.pvalue, 
                         output_std3$fma_md_ext.pvalue, output_std3$fmi_md_ext.pvalue)


pvals_adj <- p.adjust(output_table$pvalue, method = "fdr") 

output_table$fdr <- pvals_adj






output_table$lhs <- c(output_std3$cgc_fa_int.lhs, output_std3$cgh_fa_int.lhs, 
                      output_std3$cst_fa_int.lhs, output_std3$atr_fa_int.lhs, 
                      output_std3$unc_fa_int.lhs, output_std3$ilf_fa_int.lhs, 
                      output_std3$ifo_fa_int.lhs, output_std3$slf_fa_int.lhs, 
                      output_std3$fma_fa_int.lhs, output_std3$fmi_fa_int.lhs,
                      
                      
                      output_std3$cgc_md_int.lhs, output_std3$cgh_md_int.lhs, 
                      output_std3$cst_md_int.lhs, output_std3$atr_md_int.lhs, 
                      output_std3$unc_md_int.lhs, output_std3$ilf_md_int.lhs, 
                      output_std3$ifo_md_int.lhs, output_std3$slf_md_int.lhs, 
                      output_std3$fma_md_int.lhs, output_std3$fmi_md_int.lhs, 
                      
                      output_std3$cgc_fa_ext.lhs, output_std3$cgh_fa_ext.lhs, 
                      output_std3$cst_fa_ext.lhs, output_std3$atr_fa_ext.lhs, 
                      output_std3$unc_fa_ext.lhs, output_std3$ilf_fa_ext.lhs, 
                      output_std3$ifo_fa_ext.lhs, output_std3$slf_fa_ext.lhs, 
                      output_std3$fma_fa_ext.lhs, output_std3$fmi_fa_ext.lhs,
                      
                      
                      output_std3$cgc_md_ext.lhs, output_std3$cgh_md_ext.lhs, 
                      output_std3$cst_md_ext.lhs, output_std3$atr_md_ext.lhs, 
                      output_std3$unc_md_ext.lhs, output_std3$ilf_md_ext.lhs, 
                      output_std3$ifo_md_ext.lhs, output_std3$slf_md_ext.lhs, 
                      output_std3$fma_md_ext.lhs, output_std3$fmi_md_ext.lhs)

output_table$rhs <- c(output_std3$cgc_fa_int.rhs, output_std3$cgh_fa_int.rhs, 
                      output_std3$cst_fa_int.rhs, output_std3$atr_fa_int.rhs, 
                      output_std3$unc_fa_int.rhs, output_std3$ilf_fa_int.rhs, 
                      output_std3$ifo_fa_int.rhs, output_std3$slf_fa_int.rhs, 
                      output_std3$fma_fa_int.rhs, output_std3$fmi_fa_int.rhs,
                      
                      
                      output_std3$cgc_md_int.rhs, output_std3$cgh_md_int.rhs, 
                      output_std3$cst_md_int.rhs, output_std3$atr_md_int.rhs, 
                      output_std3$unc_md_int.rhs, output_std3$ilf_md_int.rhs, 
                      output_std3$ifo_md_int.rhs, output_std3$slf_md_int.rhs, 
                      output_std3$fma_md_int.rhs, output_std3$fmi_md_int.rhs, 
                      
                      output_std3$cgc_fa_ext.rhs, output_std3$cgh_fa_ext.rhs, 
                      output_std3$cst_fa_ext.rhs, output_std3$atr_fa_ext.rhs, 
                      output_std3$unc_fa_ext.rhs, output_std3$ilf_fa_ext.rhs, 
                      output_std3$ifo_fa_ext.rhs, output_std3$slf_fa_ext.rhs, 
                      output_std3$fma_fa_ext.rhs, output_std3$fmi_fa_ext.rhs,
                      
                      
                      output_std3$cgc_md_ext.rhs, output_std3$cgh_md_ext.rhs, 
                      output_std3$cst_md_ext.rhs, output_std3$atr_md_ext.rhs, 
                      output_std3$unc_md_ext.rhs, output_std3$ilf_md_ext.rhs, 
                      output_std3$ifo_md_ext.rhs, output_std3$slf_md_ext.rhs, 
                      output_std3$fma_md_ext.rhs, output_std3$fmi_md_ext.rhs)


output_table$op <- "~"


output_table$est.std <- c(output_std3$cgc_fa_int.est.std, output_std3$cgh_fa_int.est.std, 
                          output_std3$cst_fa_int.est.std, output_std3$atr_fa_int.est.std, 
                          output_std3$unc_fa_int.est.std, output_std3$ilf_fa_int.est.std, 
                          output_std3$ifo_fa_int.est.std, output_std3$slf_fa_int.est.std, 
                          output_std3$fma_fa_int.est.std, output_std3$fmi_fa_int.est.std,
                          
                          
                          output_std3$cgc_md_int.est.std, output_std3$cgh_md_int.est.std, 
                          output_std3$cst_md_int.est.std, output_std3$atr_md_int.est.std, 
                          output_std3$unc_md_int.est.std, output_std3$ilf_md_int.est.std, 
                          output_std3$ifo_md_int.est.std, output_std3$slf_md_int.est.std, 
                          output_std3$fma_md_int.est.std, output_std3$fmi_md_int.est.std, 
                          
                          output_std3$cgc_fa_ext.est.std, output_std3$cgh_fa_ext.est.std, 
                          output_std3$cst_fa_ext.est.std, output_std3$atr_fa_ext.est.std, 
                          output_std3$unc_fa_ext.est.std, output_std3$ilf_fa_ext.est.std, 
                          output_std3$ifo_fa_ext.est.std, output_std3$slf_fa_ext.est.std, 
                          output_std3$fma_fa_ext.est.std, output_std3$fmi_fa_ext.est.std,
                          
                          
                          output_std3$cgc_md_ext.est.std, output_std3$cgh_md_ext.est.std, 
                          output_std3$cst_md_ext.est.std, output_std3$atr_md_ext.est.std, 
                          output_std3$unc_md_ext.est.std, output_std3$ilf_md_ext.est.std, 
                          output_std3$ifo_md_ext.est.std, output_std3$slf_md_ext.est.std, 
                          output_std3$fma_md_ext.est.std, output_std3$fmi_md_ext.est.std)


output_table$se <-  c(output_std3$cgc_fa_int.se, output_std3$cgh_fa_int.se, 
                      output_std3$cst_fa_int.se, output_std3$atr_fa_int.se, 
                      output_std3$unc_fa_int.se, output_std3$ilf_fa_int.se, 
                      output_std3$ifo_fa_int.se, output_std3$slf_fa_int.se, 
                      output_std3$fma_fa_int.se, output_std3$fmi_fa_int.se,
                      
                      
                      output_std3$cgc_md_int.se, output_std3$cgh_md_int.se, 
                      output_std3$cst_md_int.se, output_std3$atr_md_int.se, 
                      output_std3$unc_md_int.se, output_std3$ilf_md_int.se, 
                      output_std3$ifo_md_int.se, output_std3$slf_md_int.se, 
                      output_std3$fma_md_int.se, output_std3$fmi_md_int.se,
                      
                      output_std3$cgc_fa_ext.se, output_std3$cgh_fa_ext.se, 
                      output_std3$cst_fa_ext.se, output_std3$atr_fa_ext.se, 
                      output_std3$unc_fa_ext.se, output_std3$ilf_fa_ext.se, 
                      output_std3$ifo_fa_ext.se, output_std3$slf_fa_ext.se, 
                      output_std3$fma_fa_ext.se, output_std3$fmi_fa_ext.se,
                      
                      
                      output_std3$cgc_md_ext.se, output_std3$cgh_md_ext.se, 
                      output_std3$cst_md_ext.se, output_std3$atr_md_ext.se, 
                      output_std3$unc_md_ext.se, output_std3$ilf_md_ext.se, 
                      output_std3$ifo_md_ext.se, output_std3$slf_md_ext.se, 
                      output_std3$fma_md_ext.se, output_std3$fmi_md_ext.se)



output_table$z <-  c(output_std3$cgc_fa_int.z, output_std3$cgh_fa_int.z, 
                     output_std3$cst_fa_int.z, output_std3$atr_fa_int.z, 
                     output_std3$unc_fa_int.z, output_std3$ilf_fa_int.z, 
                     output_std3$ifo_fa_int.z, output_std3$slf_fa_int.z, 
                     output_std3$fma_fa_int.z, output_std3$fmi_fa_int.z,
                     
                     
                     output_std3$cgc_md_int.z, output_std3$cgh_md_int.z, 
                     output_std3$cst_md_int.z, output_std3$atr_md_int.z, 
                     output_std3$unc_md_int.z, output_std3$ilf_md_int.z, 
                     output_std3$ifo_md_int.z, output_std3$slf_md_int.z, 
                     output_std3$fma_md_int.z, output_std3$fmi_md_int.z, 
                     
                     output_std3$cgc_fa_ext.z, output_std3$cgh_fa_ext.z, 
                     output_std3$cst_fa_ext.z, output_std3$atr_fa_ext.z, 
                     output_std3$unc_fa_ext.z, output_std3$ilf_fa_ext.z, 
                     output_std3$ifo_fa_ext.z, output_std3$slf_fa_ext.z, 
                     output_std3$fma_fa_ext.z, output_std3$fmi_fa_ext.z,
                     
                     
                     output_std3$cgc_md_ext.z, output_std3$cgh_md_ext.z, 
                     output_std3$cst_md_ext.z, output_std3$atr_md_ext.z, 
                     output_std3$unc_md_ext.z, output_std3$ilf_md_ext.z, 
                     output_std3$ifo_md_ext.z, output_std3$slf_md_ext.z, 
                     output_std3$fma_md_ext.z, output_std3$fmi_md_ext.z)


output_table$ci.lower <- c(output_std3$cgc_fa_int.ci.lower, output_std3$cgh_fa_int.ci.lower, 
                           output_std3$cst_fa_int.ci.lower, output_std3$atr_fa_int.ci.lower, 
                           output_std3$unc_fa_int.ci.lower, output_std3$ilf_fa_int.ci.lower, 
                           output_std3$ifo_fa_int.ci.lower, output_std3$slf_fa_int.ci.lower, 
                           output_std3$fma_fa_int.ci.lower, output_std3$fmi_fa_int.ci.lower,
                           
                           
                           output_std3$cgc_md_int.ci.lower, output_std3$cgh_md_int.ci.lower, 
                           output_std3$cst_md_int.ci.lower, output_std3$atr_md_int.ci.lower, 
                           output_std3$unc_md_int.ci.lower, output_std3$ilf_md_int.ci.lower, 
                           output_std3$ifo_md_int.ci.lower, output_std3$slf_md_int.ci.lower, 
                           output_std3$fma_md_int.ci.lower, output_std3$fmi_md_int.ci.lower, 
                           
                           output_std3$cgc_fa_ext.ci.lower, output_std3$cgh_fa_ext.ci.lower, 
                           output_std3$cst_fa_ext.ci.lower, output_std3$atr_fa_ext.ci.lower, 
                           output_std3$unc_fa_ext.ci.lower, output_std3$ilf_fa_ext.ci.lower, 
                           output_std3$ifo_fa_ext.ci.lower, output_std3$slf_fa_ext.ci.lower, 
                           output_std3$fma_fa_ext.ci.lower, output_std3$fmi_fa_ext.ci.lower,
                           
                           
                           output_std3$cgc_md_ext.ci.lower, output_std3$cgh_md_ext.ci.lower, 
                           output_std3$cst_md_ext.ci.lower, output_std3$atr_md_ext.ci.lower, 
                           output_std3$unc_md_ext.ci.lower, output_std3$ilf_md_ext.ci.lower, 
                           output_std3$ifo_md_ext.ci.lower, output_std3$slf_md_ext.ci.lower, 
                           output_std3$fma_md_ext.ci.lower, output_std3$fmi_md_ext.ci.lower)

output_table$ci.upper <-c(output_std3$cgc_fa_int.ci.upper, output_std3$cgh_fa_int.ci.upper, 
                          output_std3$cst_fa_int.ci.upper, output_std3$atr_fa_int.ci.upper, 
                          output_std3$unc_fa_int.ci.upper, output_std3$ilf_fa_int.ci.upper, 
                          output_std3$ifo_fa_int.ci.upper, output_std3$slf_fa_int.ci.upper, 
                          output_std3$fma_fa_int.ci.upper, output_std3$fmi_fa_int.ci.upper,
                          
                          
                          output_std3$cgc_md_int.ci.upper, output_std3$cgh_md_int.ci.upper, 
                          output_std3$cst_md_int.ci.upper, output_std3$atr_md_int.ci.upper, 
                          output_std3$unc_md_int.ci.upper, output_std3$ilf_md_int.ci.upper, 
                          output_std3$ifo_md_int.ci.upper, output_std3$slf_md_int.ci.upper, 
                          output_std3$fma_md_int.ci.upper, output_std3$fmi_md_int.ci.upper, 
                          
                          output_std3$cgc_fa_ext.ci.upper, output_std3$cgh_fa_ext.ci.upper, 
                          output_std3$cst_fa_ext.ci.upper, output_std3$atr_fa_ext.ci.upper, 
                          output_std3$unc_fa_ext.ci.upper, output_std3$ilf_fa_ext.ci.upper, 
                          output_std3$ifo_fa_ext.ci.upper, output_std3$slf_fa_ext.ci.upper, 
                          output_std3$fma_fa_ext.ci.upper, output_std3$fmi_fa_ext.ci.upper,
                          
                          
                          output_std3$cgc_md_ext.ci.upper, output_std3$cgh_md_ext.ci.upper, 
                          output_std3$cst_md_ext.ci.upper, output_std3$atr_md_ext.ci.upper, 
                          output_std3$unc_md_ext.ci.upper, output_std3$ilf_md_ext.ci.upper, 
                          output_std3$ifo_md_ext.ci.upper, output_std3$slf_md_ext.ci.upper, 
                          output_std3$fma_md_ext.ci.upper, output_std3$fmi_md_ext.ci.upper)

output_table$model <- paste0(output_table$lhs, " ", output_table$op, " ", output_table$rhs)

output_table
out <- output_table[ , c(2, 6, 7, 10, 11, 9, 12)] 

write.csv(out, paste0(res, "results_tracts_genr.csv"))

rownames(out) <- out$model
out$model <- NULL

out2 <- round(out, digits = 3)

write.csv(out2, paste0(res, "results_tracts_genr_rounded.csv"))


# is there anything sign after fdr correction? 

out[out$fdr < 0.05, ] # none
out[out$pvalue < 0.05, ] # cgc fa to int, int to cgc md and fma md. ext to ifo md 

