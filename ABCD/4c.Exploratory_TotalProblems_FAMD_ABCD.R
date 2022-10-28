####################################################
# 4c.Exploratory total problems and global FA and MD
####################################################

####  
# set environment
####

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

dd <- readRDS(paste0(indata, "abcd_main_data.Rds"))

####
# factor scaling
####
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

source("0b.Source_file_CLPMs_ABCD_4.0.R")


### NB after dummyfying you don't need to put that var as factor type 
# check rationale here https://jslefche.github.io/sem_book/categorical-variables.html

####
# Model specification 
####
# Specify the same models you used in the main analyses 

tot_fa  <- '
 
tot_t2 + FA_t2_scaled ~ tot_t1 + FA_t1_scaled

tot_t1 ~~ FA_t1_scaled
tot_t2 ~~ FA_t2_scaled

tot_t1 ~~ tot_t1
FA_t1_scaled ~~ FA_t1_scaled
tot_t2 ~~ tot_t2
FA_t2_scaled ~~ FA_t2_scaled

tot_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

FA_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21  

FA_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'



tot_md <- '
tot_t2 + MD_t2_scaled ~ tot_t1 + MD_t1_scaled

tot_t1 ~~ MD_t1_scaled
tot_t2 ~~ MD_t2_scaled

tot_t1 ~~ tot_t1
MD_t1_scaled ~~ MD_t1_scaled
tot_t2 ~~ tot_t2
MD_t2_scaled ~~ MD_t2_scaled

tot_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

MD_t2_scaled ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15  + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

MD_t1_scaled ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'

####
# model identification 
####

# run the model
tot_fa_fit <- sem(tot_fa, data = dd, missing = "fiml", fixed.x= F) 
tot_md_fit <- sem(tot_md, data = dd, missing = "fiml", fixed.x= F) 

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


