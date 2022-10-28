#######################################
# 4b.Exploratory_analyses_tracts_ABCD
#######################################
# Exploratory analyses - specific tracts - ABCD
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: running exploratory analyses on DTI tracts in ABCD: CLPMs for each of 10 tracts with internalising and externalising problems
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)


###
# set environment
###

source("0a.Source_file_general_ABCD_4.0.R")
dd <- readRDS(paste0(indata, "abcd_main_data.Rds"))


#####
# factor scaling 
#####
# make all variables in the same order of magnitude. you need to scale by 100 all tracts

scale_factor = 10

to_match <- c("_fa_", "_md_") 

tracts <- grep(paste0(to_match, collapse = "|"), names(dd), value = T)
# 40 tracts as expected - 10 for MD 10 for FA * for two time points


# scale 

dd[tracts] <- lapply(dd[tracts], function(x) x * scale_factor)

summary(dd[tracts]) # transformation went well 



#####
# Vars recoding
#####

source("0b.Source_file_CLPMs_ABCD_4.0.R")

####
# CLPMs
####
# We test each tract in relation to internalising and externalising problems
# Apply the general model specifications form the source file to all tracts

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


names(specifications_int) <- paste0(names(specifications_int), "int") # add to the title of each element of the list "int"
names(specifications_ext) <- paste0(names(specifications_ext), "ext")


specifications <- append(specifications_int, specifications_ext) # put together specifications for int and ext models
specifications # list of 40 as expected


####
# Model evaluation - fit 
####

# fit the models 

fits_all <- sapply(specifications, function(x) sem(x, data = dd, missing = "fiml", fixed.x = F)) 


# get model fits - are they good enough? 
# get all model fit indeces you are interested in 

table_fits <- sapply(fits_all, function(x) fitMeasures(x, c("cfi", "tli", "rmsea", "srmr")))

table_fits <- as.data.frame(t(table_fits)) # for saving, transform into dataframe format after transposing (so fit indeces are the cols and models the rows)

#table_fits <- round(table_fits, digits = 3) # round to 3 decimals

table_fits$model <- rownames(table_fits) # create new col with the model name 

table_fits <- table_fits[, c(5, 3, 4, 1, 2)] # change order of cols

table_fits # model fits are good 

# save

write.csv(table_fits, paste0(res, "model_fits_tracts_ABCD.csv"), row.names = F)


# round coeff.

table_fits$model <- NULL

table_fits <- round(table_fits, digits = 3)

write.csv(table_fits, paste0(res, "model_fits_tracts_ABCD.csv"), row.names = F)


####
# Get estimates 
####

# get estimates 

output_std <- lapply(fits_all, function(x) standardizedSolution(x)) # get std estimates, ci and p vals info 

output_std2 <- as.data.frame(output_std) # change format for saving 

write.csv(output_std2, paste0(res, "summarystats_std_tracts_ABCD.csv"), row.names = F) # save all summary stats for all the models 


# focus on lagged paths 

output_std3 <- output_std2[2:3, ] # get the lagged coefficients information

write.csv(output_std3, paste0(res, "laggedpaths_std_tracts_ABCD.csv"), row.names = F)


# create an output table for in-text ms 

output_std3$FDR <- as.numeric("NA")

output_table <- data.frame(model = 1:80, 
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
                         output_std3$fma_md_ext.pvalue, output_std3$fmi_md_ext.pvalue
                         
                         )


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
                         output_std3$fma_md_ext.lhs, output_std3$fmi_md_ext.lhs
                      )

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
                         output_std3$fma_md_ext.rhs, output_std3$fmi_md_ext.rhs
                      
                      
                      )


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
                         output_std3$fma_md_ext.z, output_std3$fmi_md_ext.z
                     
                     )


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
                         output_std3$fma_md_ext.ci.lower, output_std3$fmi_md_ext.ci.lower
                         
                         
                         
                         )

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

out <- output_table[ , c(1, 5, 6, 9, 10, 8, 11)]  # re-order cols 


rownames(out) <- out$model
out$model <- NULL

write.csv(out, paste0(res, "results_tracts_ABCD.csv"))


out <- round(out, digits = 3)

write.csv(out, paste0(res, "results_tracts_ABCD_rounded.csv"))


