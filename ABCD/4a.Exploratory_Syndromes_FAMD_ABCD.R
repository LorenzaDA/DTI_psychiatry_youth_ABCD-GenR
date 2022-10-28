################
# 4a. ABCD - exploratory analyses - syndrome scales and global DTI
###############
  
####
# set environment 
####

source("0a.Source_file_general_ABCD_4.0.R")
dd <- readRDS(paste0(indata, "abcd_main_data.Rds"))

####
# Model prep 
####

### factor scaling ###

scale_factor = 100

dd$FA_t1_scaled <- dd$FA_t1*scale_factor 

dd$MD_t1_scaled <- dd$MD_t1*scale_factor

dd$FA_t2_scaled <- dd$FA_t2*scale_factor 

dd$MD_t2_scaled <- dd$MD_t2*scale_factor


#####
# Vars re-coding & model specifications
#####
# source file contains the pre model prep for CLPMs 
# & general model specifications where a general model obtained from script 2 (general #CLPMs) is specified

source("0b.Source_file_CLPMs_ABCD_4.0.R")


#####
# Run CLPMs 
#####

# create empty lists where to put the model specifications for each syndrome scale
# From the general model specifications, we create specific model specifications for each
# syndrome scale. So the m_fa and m_md from the source file 0b. will be used. 
# The "var" within the specification will be substituted with each syndrome scale

specifications_fa <- list()
specifications_md <- list()

# create vector for the substitution of the var specifications into syndrome scale names 

names <- c("anxdep_", "withdep_", "somatic_", "social_", "thought_", "att_", "agg_", "rulebr_")


# loop to create the various model specifications per syndrome scale 

for (i in names){
  specifications_fa[i] <- gsub("var_", i, m_fa) # gsub changes the "var_" string into every element of names in the m_fa specification
  specifications_md[i] <- gsub("var_", i, m_md) # as above, but for md specifications
}


names(specifications_fa) <- paste0(names(specifications_fa), "fa")
names(specifications_md) <- paste0(names(specifications_md), "md")

# put all model specifications together

specifications <- append(specifications_fa, specifications_md)
specifications



#########
# Model evaluation - fit
########

# fit all models

fits_all <- sapply(specifications, function(x) sem(x, data = dd2, missing = "fiml", fixed.x = F))


# get all model fit measures that you are interested in 

table_fits <- sapply(fits_all, function(x) fitMeasures(x, c("cfi", "tli", "rmsea", "srmr")))

table_fits <- as.data.frame(t(table_fits)) # for saving, transform into dataframe format after transposing (so fit indices are the cols and models the rows)


table_fits$model <- rownames(table_fits) # create new col with the model name 

table_fits <- table_fits[, c(5, 3, 4, 1, 2)] # change order of cols

write.csv(table_fits, paste0(res, "model_fits_syndromescales_ABCD.csv"), row.names = F)


table_fits$model <- NULL # for rounding we need to have all vars as numeric

table_fits <- round(table_fits, digits = 3) # round to 3 decimals

write.csv(table_fits, paste0(res, "model_fits_syndromes_ABCD_rounded.csv"))


########
# Get estimates 
########


# get standardised estimates 

output_std <- lapply(fits_all, function(x) standardizedSolution(x)) # get std estimates, ci and p vals info 

output_std2 <- as.data.frame(output_std) # change format for saving 

write.csv(output_std2, paste0(res, "summarystats_std_syndromescales_ABCD.csv"), row.names = F) # save all summary stats for all the models 


output_std3 <- output_std2[2:3, ] # get the lagged coefficients
output_std3

write.csv(output_std3, paste0(res, "laggedpaths_std_syndromescales_ABCD.csv"), row.names = F)


#### create output table for in-text ####

output_std3$FDR <- as.numeric("NA")

names(output_std3)

output_table <- data.frame(model = 1:32, 
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



output_table$pvalue <- c(output_std3$anxdep_fa.pvalue, output_std3$withdep_fa.pvalue,
                         output_std3$somatic_fa.pvalue, output_std3$social_fa.pvalue,
                         output_std3$thought_fa.pvalue, output_std3$att_fa.pvalue,
                         output_std3$agg_fa.pvalue, output_std3$rulebr_fa.pvalue, 
                         output_std3$anxdep_md.pvalue, output_std3$withdep_md.pvalue,
                         output_std3$somatic_md.pvalue, output_std3$social_md.pvalue,
                         output_std3$thought_md.pvalue, output_std3$att_md.pvalue,
                         output_std3$agg_md.pvalue, output_std3$rulebr_md.pvalue)

pvals_adj <- p.adjust(output_table$pvalue, method = "fdr") 

output_table$fdr <- pvals_adj

output_table$lhs <- c(output_std3$anxdep_fa.lhs, output_std3$withdep_fa.lhs,  
                      output_std3$somatic_fa.lhs, output_std3$social_fa.lhs,
                      output_std3$thought_fa.lhs, output_std3$att_fa.lhs, 
                      output_std3$agg_fa.lhs, output_std3$rulebr_fa.lhs, 
                      output_std3$anxdep_md.lhs, output_std3$withdep_md.lhs,  
                      output_std3$somatic_md.lhs, output_std3$social_md.lhs,
                      output_std3$thought_md.lhs, output_std3$att_md.lhs, 
                      output_std3$agg_md.lhs, output_std3$rulebr_md.lhs)

output_table$rhs <- c(output_std3$anxdep_fa.rhs, output_std3$withdep_fa.rhs,  
                      output_std3$somatic_fa.rhs, output_std3$social_fa.rhs,
                      output_std3$thought_fa.rhs, output_std3$att_fa.rhs, 
                      output_std3$agg_fa.rhs, output_std3$rulebr_fa.rhs,
                      output_std3$anxdep_md.rhs, output_std3$withdep_md.rhs,  
                      output_std3$somatic_md.rhs, output_std3$social_md.rhs,
                      output_std3$thought_md.rhs, output_std3$att_md.rhs, 
                      output_std3$agg_md.rhs, output_std3$rulebr_md.rhs)

output_table$op <- "~"


output_table$est.std <- c(output_std3$anxdep_fa.est.std, output_std3$withdep_fa.est.std,  
                          output_std3$somatic_fa.est.std, output_std3$social_fa.est.std,
                          output_std3$thought_fa.est.std, output_std3$att_fa.est.std, 
                          output_std3$agg_fa.est.std, output_std3$rulebr_fa.est.std,
                          output_std3$anxdep_md.est.std, output_std3$withdep_md.est.std,  
                          output_std3$somatic_md.est.std, output_std3$social_md.est.std,
                          output_std3$thought_md.est.std, output_std3$att_md.est.std, 
                          output_std3$agg_md.est.std, output_std3$rulebr_md.est.std)


output_table$se <-  c(output_std3$anxdep_fa.se, output_std3$withdep_fa.se,  
                      output_std3$somatic_fa.se, output_std3$social_fa.se,
                      output_std3$thought_fa.se, output_std3$att_fa.se, 
                      output_std3$agg_fa.se, output_std3$rulebr_fa.se,
                      output_std3$anxdep_md.se, output_std3$withdep_md.se,  
                      output_std3$somatic_md.se, output_std3$social_md.se,
                      output_std3$thought_md.se, output_std3$att_md.se, 
                      output_std3$agg_md.se, output_std3$rulebr_md.se)

output_table$z <-  c(output_std3$anxdep_fa.z, output_std3$withdep_fa.z,  
                     output_std3$somatic_fa.z, output_std3$social_fa.z,
                     output_std3$thought_fa.z, output_std3$att_fa.z, 
                     output_std3$agg_fa.z, output_std3$rulebr_fa.z, 
                     
                     output_std3$anxdep_md.z, output_std3$withdep_md.z,  
                     output_std3$somatic_md.z, output_std3$social_md.z,
                     output_std3$thought_md.z, output_std3$att_md.z, 
                     output_std3$agg_md.z, output_std3$rulebr_md.z)

output_table$ci.lower <- c(output_std3$anxdep_fa.ci.lower, output_std3$withdep_fa.ci.lower,  
                           output_std3$somatic_fa.ci.lower, output_std3$social_fa.ci.lower,
                           output_std3$thought_fa.ci.lower, output_std3$att_fa.ci.lower, 
                           output_std3$agg_fa.ci.lower, output_std3$rulebr_fa.ci.lower,
                           output_std3$anxdep_md.ci.lower, output_std3$withdep_md.ci.lower,  
                           output_std3$somatic_md.ci.lower, output_std3$social_md.ci.lower,
                           output_std3$thought_md.ci.lower, output_std3$att_md.ci.lower, 
                           output_std3$agg_md.ci.lower, output_std3$rulebr_md.ci.lower)

output_table$ci.upper <- c(output_std3$anxdep_fa.ci.upper,output_std3$withdep_fa.ci.upper,
                           output_std3$somatic_fa.ci.upper, output_std3$social_fa.ci.upper,
                           output_std3$thought_fa.ci.upper, output_std3$att_fa.ci.upper, 
                           output_std3$agg_fa.ci.upper, output_std3$rulebr_fa.ci.upper, 
                           output_std3$anxdep_md.ci.upper, output_std3$withdep_md.ci.upper,  
                           output_std3$somatic_md.ci.upper, output_std3$social_md.ci.upper,
                           output_std3$thought_md.ci.upper, output_std3$att_md.ci.upper, 
                           output_std3$agg_md.ci.upper, output_std3$rulebr_md.ci.upper)

output_table$model <- paste0(output_table$lhs, " ", output_table$op, " ", output_table$rhs)

output_table

out <- output_table[ , c(1, 5, 6, 9, 10, 8, 11)] 
out

rownames(out) <- out$model
out$model <- NULL


write.csv(out, paste0(res, "results_syndromescales_ABCD.csv"))

out <- round(out, digits = 3)

write.csv(out, paste0(res, "results_syndromescales_rounded_ABCD.csv"))



