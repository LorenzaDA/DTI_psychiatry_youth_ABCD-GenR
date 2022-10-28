##################################
# 4. Exploratory analyses - Syndrome scales with global FA/MD - GenR
##################################
# Project: Directionality of brain - behaviour associations
# Project is preregistered on osf at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R at @9 and @13 (focus)
# Author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

######
# Set environment
###### 


rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)


dd <- readRDS("genr_main_data.rds")



#######
# Other model prep
#######


### factor scaling ####

scale_factor = 100

dd$md_t1 <- dd$md_t1*scale_factor

dd$md_t2 <- dd$md_t2*scale_factor

dd$fa_t1 <- dd$fa_t1*scale_factor

dd$fa_t2 <- dd$fa_t2*scale_factor


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

m_fa  <- '
var_t2 + fa_t2 ~ var_t1 + fa_t1
var_t1 ~~ fa_t1
var_t2 ~~ fa_t2
var_t1 ~~ var_t1
fa_t1 ~~ fa_t1
var_t2 ~~ var_t2
fa_t2 ~~ fa_t2
var_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
fa_t2 ~ puberty + age_mri_t2 + age_diff_t2
fa_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'




m_md <- '
var_t2 + md_t2 ~ var_t1 + md_t1
var_t1 ~~ md_t1
var_t2 ~~ md_t2
var_t1 ~~ var_t1
md_t1 ~~ md_t1
var_t2 ~~ var_t2
md_t2 ~~ md_t2
var_t2 ~ sex_boy + ethn_other + ethn_surinames_antillian + ethn_turkish_moroccan + ethn_european_descent + mat_edu_num
md_t2 ~ puberty + age_mri_t2 + age_diff_t2
md_t1 ~ age_mri_t1 + age_diff_t1 + sex_boy
'

# create empty lists where to put the model specifications for each syndrome scale

specifications_fa <- list()
specifications_md <- list()


# create vector for the substitution of the var specifications into syndrome scale names 

names <- c("anxdep_", "withdep_", "somatic_", "social_", "thought_", "att_", "agg_", "rulebr_")


# loop to create the various model specifications per syndrome scale 

for (i in names){
  specifications_fa[i] <- gsub("var_", i, m_fa) # gsub changes the "var_" string into every element of names in the m_fa specification
  specifications_md[i] <- gsub("var_", i, m_md) # as above, but for md specifications
}

specifications_fa
specifications_md


names(specifications_fa) <- paste0(names(specifications_fa), "fa") # add names to the list elements 
names(specifications_md) <- paste0(names(specifications_md), "md")


specifications <- append(specifications_fa, specifications_md) # put all specifications together
specifications



#######
# Running models
#######


fits_all <- sapply(specifications, function(x) sem(x, data = dd2, missing = "fiml", fixed.x = F))

# get model fits - are they good enough? 

output_all <- sapply(fits_all, function(x) summary(x, fit.measures = T, estimates = F)) # list of 16 as expected
# fits are fine 


# get all model fits you are interested in and save the info 

table_fits <- sapply(fits_all, function(x) fitMeasures(x, c("cfi", "tli", "rmsea", "srmr"))) # fits we interested in 

table_fits <- as.data.frame(t(table_fits)) # for saving, transform into dataframe format after transposing (so fit indices are the cols and models the rows)

table_fits <- round(table_fits, digits = 3) # round to 3 decimals

table_fits$model <- rownames(table_fits) # create new col with the model name 


table_fits <- table_fits[, c(5, 3, 4, 1, 2)] # change order of cols

table_fits$model <- NULL

table_fits

write.table(table_fits, paste0(res, "model_fits_syndromescales.csv"), row.names = F, sep = ",") # save


#######
# Get estimates 
######

output_std <- lapply(fits_all, function(x) standardizedSolution(x)) # get std estimates, ci and p vals info - these are ALL standardised info

output_std2 <- as.data.frame(output_std) # change format for saving 

write.table(output_std2, paste0(res, "summarystats_std_syndromes_GenR.csv"), row.names = F, sep = ",") # save all summary stats for all the models 


output_std3 <- output_std2[2:3, ] # get the lagged coefficients
output_std3

write.table(output_std3, paste0(res, "laggedpaths_std.csv"), row.names = F, sep = ",")

output_std3$FDR <- as.numeric("NA")

names(output_std3)

output_table <- data.frame(1:32, model = "NA", 
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

output_table$ci.upper <- c(output_std3$anxdep_fa.ci.upper, output_std3$withdep_fa.ci.upper,  
                           output_std3$somatic_fa.ci.upper, output_std3$social_fa.ci.upper,
                           output_std3$thought_fa.ci.upper, output_std3$att_fa.ci.upper, 
                           output_std3$agg_fa.ci.upper, output_std3$rulebr_fa.ci.upper, 
                           output_std3$anxdep_md.ci.upper, output_std3$withdep_md.ci.upper,  
                           output_std3$somatic_md.ci.upper, output_std3$social_md.ci.upper,
                           output_std3$thought_md.ci.upper, output_std3$att_md.ci.upper, 
                           output_std3$agg_md.ci.upper, output_std3$rulebr_md.ci.upper)

output_table$model <- paste0(output_table$lhs, " ", output_table$op, " ", output_table$rhs)

output_table
out <- output_table[ , c(2, 6, 7, 10, 11, 9, 12)] 
out

rownames(out) <- out$model
out$model <- NULL

out2 <- round(out, digits = 3)


write.table(out, paste0(res, "results_syndromescales_genr.csv"), sep= ",")
write.table(out2, paste0(res, "results_syndromescales_genr_rounded.csv"), sep= ",")


#### make the tables ready for the supplement ###

# divide between direction: brain --> beh vs. beh --> brain
# divide FA and MD 

out2$model <- rownames(out2)

tab <- out2

## naming 

tab$model2 <- NA

tab$model2 <- gsub("anxdep", "Anxious/Depressed", 
                   gsub("withdep", "Withdrawn/Depressed", 
                        gsub("somatic", "Somatic Complaints",
                             gsub("social", "Social Problems",
                                  gsub("thought", "Thought Problems",
                                       gsub("att", "Attention Problems",
                                            gsub("agg", "Aggressive Behavior", 
                                                 gsub("rulebr", "Rule-Breaking Behavior",
                                                      gsub("fa", "FA", 
                                                           gsub("md", "MD", 
                                                                gsub("_t2", " T2", 
                                                                     gsub("_t1", " T1",                                                                                                                      
                                                                          tab$model))))))))))))


tab                                  







FA <- grep("fa", tab$model, value = T)
BrtoBeh <- grep("md_t1|fa_t1", tab$model, value = T)

FA_BrtoBeh <- tab[rownames(tab) %in% FA & rownames(tab) %in% BrtoBeh, ]
FA_BehtoBr <- tab[rownames(tab) %in% FA & !rownames(tab) %in% BrtoBeh, ]

MD_BrtoBeh <- tab[!rownames(tab) %in% FA & rownames(tab) %in% BrtoBeh, ]
MD_BehtoBr <- tab[!rownames(tab) %in% FA & !rownames(tab) %in% BrtoBeh, ]


## put it all together in the order you want 

tab <- rbind(FA_BrtoBeh, FA_BehtoBr, MD_BrtoBeh, MD_BehtoBr)

tab


## clean up 

rownames(tab) <- NULL
rownames(tab) <- tab$model2
tab$model <- NULL
tab$model2 <- NULL

write.csv(tab, paste0(res, "exploratory_syndromes.csv"))
