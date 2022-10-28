##################################
# 0b. Data prep GenR
##################################
# Project: Directionality of brain - behaviour associations
# Project is preregistered on osf at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R at @9 and @13 (focus)
# Aim of script: Data preparation of the Generation R sample
# Author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

#######
# Environment prep 
#######

rm(list=ls())

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

setwd(indata)


### Load in the data ###
# here you can easily load the data with loops  cause they are in wide format 
# NB previous step in SPSS: recoded all variables so that missing or missing system would be treated as missing systems
# this is for readability in R of the missings

data <- list.files(pattern = ".sav")


# read in all files into a list
data2 <- list()

for (i in data){
  data2[[i]] <- read.spss(i, to.data.frame = T)
}

data3 <- lapply(data2, function(x) {names(x) <- tolower(names(x)); x}) #make all variables lowercase 
data4 <- lapply(data3, as.data.table)


# load the datasets which are not in the spss format 

dti_9 <- readRDS("f09_GenR_MRI_eddy_dipy_wls_14Feb2022_autoPtx_dti_stats_inc_glob_measV1.rds") 
dti_13 <- readRDS("f13_GenR_MRI_eddy_dipy_wls_14Feb2022_autoPtx_dti_stats_inc_glob_measV1.rds")
core_dti <- readRDS("genr_mri_core_data_20220311.rds")


#######
# Merge the data
#######

### merge behavioral data and covariates ### 

data4 <- lapply(data4, function (x) {x[order(x$idc), ]}) # first order by participant ID

dd <- Reduce(function(d1, d2) merge(d1, d2, by = intersect(names(d1), names(d2)), all.x = TRUE), data4)  # merge 

dd2 <- as.data.frame(dd)


### merge with dti data ###

# order by participant id

dd2 <- dd2[order(dd2$idc), ] 
dti_9 <- dti_9[order(dti_9$idc), ] 
dti_13 <- dti_13[order(dti_13$idc), ] 

# merge

dd3 <- merge(dd2, dti_9, by = "idc", all.x = T) 
dd4 <- merge(dd3, dti_13, by = "idc", all.x = T)


### merge with core MRI data ###

# order by participant id

core_dti <- core_dti[order(core_dti$idc), ] 
dd4 <- dd4[order(dd4$idc), ] 

# merge

dd5 <- merge(dd4, core_dti, by = "idc", all.x = T) 


#######
# Create the dti vars - average hemisphere values
#######

#### t1 #####

# for FA per tract

dd5$cgc_fa_t1 <- (dd5$cgc_l_dti_dipy_wls_wavg_FA_f09 + dd5$cgc_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$cgh_fa_t1 <- (dd5$cgh_l_dti_dipy_wls_wavg_FA_f09 + dd5$cgh_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$cst_fa_t1 <- (dd5$cst_l_dti_dipy_wls_wavg_FA_f09 + dd5$cst_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$atr_fa_t1 <- (dd5$atr_l_dti_dipy_wls_wavg_FA_f09 + dd5$atr_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$unc_fa_t1 <- (dd5$unc_l_dti_dipy_wls_wavg_FA_f09 + dd5$unc_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$ilf_fa_t1 <- (dd5$ilf_l_dti_dipy_wls_wavg_FA_f09  + dd5$ilf_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$ifo_fa_t1 <- (dd5$ifo_l_dti_dipy_wls_wavg_FA_f09 + dd5$ifo_r_dti_dipy_wls_wavg_FA_f09)/2

dd5$slf_fa_t1 <- (dd5$slf_l_dti_dipy_wls_wavg_FA_f09 + dd5$slf_r_dti_dipy_wls_wavg_FA_f09)/2


# for MD per tract

dd5$cgc_md_t1 <- (dd5$cgc_l_dti_dipy_wls_wavg_MD_f09 + dd5$cgc_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$cgh_md_t1 <- (dd5$cgh_l_dti_dipy_wls_wavg_MD_f09 + dd5$cgh_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$cst_md_t1 <- (dd5$cst_l_dti_dipy_wls_wavg_MD_f09 + dd5$cst_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$atr_md_t1 <- (dd5$atr_l_dti_dipy_wls_wavg_MD_f09 + dd5$atr_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$unc_md_t1 <- (dd5$unc_l_dti_dipy_wls_wavg_MD_f09 + dd5$unc_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$ilf_md_t1 <- (dd5$ilf_l_dti_dipy_wls_wavg_MD_f09  + dd5$ilf_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$ifo_md_t1 <- (dd5$ifo_l_dti_dipy_wls_wavg_MD_f09 + dd5$ifo_r_dti_dipy_wls_wavg_MD_f09)/2

dd5$slf_md_t1 <- (dd5$slf_l_dti_dipy_wls_wavg_MD_f09 + dd5$slf_r_dti_dipy_wls_wavg_MD_f09)/2


# rename the fma and fmi (for which averaged values are not needed - no lh and rh)

dd5$fma_fa_t1 <- dd5$fma_dti_dipy_wls_wavg_FA_f09
dd5$fma_md_t1 <- dd5$fma_dti_dipy_wls_wavg_MD_f09

dd5$fmi_fa_t1 <- dd5$fmi_dti_dipy_wls_wavg_FA_f09 
dd5$fmi_md_t1 <- dd5$fmi_dti_dipy_wls_wavg_MD_f09



#### t2 #####

# for FA

dd5$cgc_fa_t2 <- (dd5$cgc_l_dti_dipy_wls_wavg_FA_f13 + dd5$cgc_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$cgh_fa_t2 <- (dd5$cgh_l_dti_dipy_wls_wavg_FA_f13 + dd5$cgh_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$cst_fa_t2 <- (dd5$cst_l_dti_dipy_wls_wavg_FA_f13 + dd5$cst_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$atr_fa_t2 <- (dd5$atr_l_dti_dipy_wls_wavg_FA_f13 + dd5$atr_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$unc_fa_t2 <- (dd5$unc_l_dti_dipy_wls_wavg_FA_f13 + dd5$unc_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$ilf_fa_t2 <- (dd5$ilf_l_dti_dipy_wls_wavg_FA_f13  + dd5$ilf_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$ifo_fa_t2 <- (dd5$ifo_l_dti_dipy_wls_wavg_FA_f13 + dd5$ifo_r_dti_dipy_wls_wavg_FA_f13)/2

dd5$slf_fa_t2 <- (dd5$slf_l_dti_dipy_wls_wavg_FA_f13 + dd5$slf_r_dti_dipy_wls_wavg_FA_f13)/2


# for MD

dd5$cgc_md_t2 <- (dd5$cgc_l_dti_dipy_wls_wavg_MD_f13 + dd5$cgc_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$cgh_md_t2 <- (dd5$cgh_l_dti_dipy_wls_wavg_MD_f13 + dd5$cgh_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$cst_md_t2 <- (dd5$cst_l_dti_dipy_wls_wavg_MD_f13 + dd5$cst_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$atr_md_t2 <- (dd5$atr_l_dti_dipy_wls_wavg_MD_f13 + dd5$atr_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$unc_md_t2 <- (dd5$unc_l_dti_dipy_wls_wavg_MD_f13 + dd5$unc_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$ilf_md_t2 <- (dd5$ilf_l_dti_dipy_wls_wavg_MD_f13  + dd5$ilf_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$ifo_md_t2 <- (dd5$ifo_l_dti_dipy_wls_wavg_MD_f13 + dd5$ifo_r_dti_dipy_wls_wavg_MD_f13)/2

dd5$slf_md_t2 <- (dd5$slf_l_dti_dipy_wls_wavg_MD_f13 + dd5$slf_r_dti_dipy_wls_wavg_MD_f13)/2


# rename the fma and fmi 

dd5$fma_fa_t2 <- dd5$fma_dti_dipy_wls_wavg_FA_f13
dd5$fma_md_t2 <- dd5$fma_dti_dipy_wls_wavg_MD_f13

dd5$fmi_fa_t2 <- dd5$fmi_dti_dipy_wls_wavg_FA_f13 
dd5$fmi_md_t2 <- dd5$fmi_dti_dipy_wls_wavg_MD_f13



#######
# Select the variables we need 
#######
# as based on the preregistration
# inclusion criteria: having longitudinal data on both brain and behaviour (DTI FA or MD & int or ext at both @ 9 and @ 13 - NB allowing for 25% missingness in global values computation) 
# exclusion criteria: failed DTI qc (this includes failed quality due to braces, and exclusion of repeat scans, too low n vols) & no incidental findings 
# for remaining twins/siblings only 1 randomly included 
# different scan type @9 as additional exclusion criteria 
# covariates: age (MRI, MRI-CBCL difference), sex, national origin/ethnicity, maternal education, pubertal stage
# other key variables: non-response, tract specific values, syndrome scales

vars <- c("idc", "idm", "mother", "sum_ext_9m", "sum_int_9m",  "sum_int_14", # main variables
          "sum_ext_14", "mean_FA_abcd_f09", "mean_MD_abcd_f09", # main variables
          "mean_FA_abcd_f13", "mean_MD_abcd_f13", 
          "ethninfv3", "gender", "educm5", "puberty13", # covariates
          "educm", "agechild_cbcl9m", "agechild_gr1093", "age_child_mri_f09", "age_child_mri_f13", # covariates
          "has_braces_mri_f09", "has_braces_mri_f13", "mri_consent_f09", "mri_consent_f13", # exclusion criteria
          "exclude_incidental_f09", "exclude_incidental_f13", # exclusion criteria
          "mr750_softwareversionshort_dicom", 
          "startfase3_9", # non-response
          "sum_anx_9m", "sum_wit_9m", "sum_som_9m", "sum_sop_9m","sum_tho_9m", "sum_att_9m", "sum_rul_9m", "sum_agg_9m", 
          "sum_anx_14", "sum_wit_14","sum_som_14", "sum_sop_14", "sum_tho_14", "sum_att_14", "sum_rul_14", "sum_agg_14", 
          "cbcl_sum_9m", "cbcl_sum_14", # syndrome scales
          "cgc_md_t1", "cgh_md_t1", "fma_md_t1", "fmi_md_t1", "cst_md_t1", "atr_md_t1", "unc_md_t1", 
          "ilf_md_t1", "ifo_md_t1", "slf_md_t1", "cgc_fa_t1", "cgh_fa_t1", 
          "fma_fa_t1", "fmi_fa_t1", "cst_fa_t1", "atr_fa_t1", "unc_fa_t1", 
          "ilf_fa_t1", "ifo_fa_t1", "slf_fa_t1", #tracts
          "cgc_md_t2", "cgh_md_t2", 
          "fma_md_t2", "fmi_md_t2", "cst_md_t2", "atr_md_t2", "unc_md_t2", 
          "ilf_md_t2", "ifo_md_t2", "slf_md_t2", "cgc_fa_t2", "cgh_fa_t2", 
          "fma_fa_t2", "fmi_fa_t2", "cst_fa_t2", "atr_fa_t2", "unc_fa_t2", 
          "ilf_fa_t2", "ifo_fa_t2", "slf_fa_t2", # tracts
          "missingness_abcd_f09", "missingness_abcd_f13", # inclusion criteria (has MRI data)
          "dti_overall_qc_f09", "dti_overall_qc_f13") # inclusion/exclusion: QC


# filter the df for the variables we need (specified in vars)

dd6 <- dd5[ , names(dd5) %in% vars] 



####
# Variable recoding & naming
####

dd7 <- dd6 %>% dplyr::mutate(educm5 = recode_factor(educm5, "no education finished" = "low_int", 
							"primary" = "low_int", "secondary, phase 1" = "low_int", 
							"secondary, phase 2" = "low_int", "higher, phase 1"= "high",
							"higher, phase 2" = "high"),
							educm = recode_factor(educm, "no education finished" = 'low_int', 
							"primary" = "low_int",  "secondary, phase 1" = "low_int", 
							"secondary, phase 2" = "low_int", "higher, phase 1"= "high",
							"higher, phase 2" = "high"),
							idc = as.character(idc),
							idm = as.character(idm), 
							mother = as.character(mother),
							
							ethninfv3 = recode_factor(ethninfv3, "Dutch" = "dutch", "Moroccan" = "turkish_moroccan", 
							"Indonesian" = "other", "Cape Verdian" = "other", "Dutch Antilles" = "surinames_antillian", 
							"Surinamese" = "surinames_antillian", "Turkish" = "turkish_moroccan", "Surinamese-Creole" = "surinames_antillian",
							"Surinamese-Hindustani" = "surinames_antillian", "Surinamese-Unspecified" = "surinames_antillian", 
							"African" = "other", "American,western" = "european_descent", "American, non western" = "other", "Asian, western" = "other",
							"Asian, non western" = "other", "European" = "european_descent", "Oceanie" = "european_descent")
							) %>% 
					
					# rename vars 
					rename(mat_edu = educm5, ethn = ethninfv3, sex = gender, age_cbcl_t1 = agechild_cbcl9m, 
							int_t1 = sum_int_9m, int_t2 = sum_int_14, ext_t1 = sum_ext_9m, ext_t2 = sum_ext_14, 
							age_cbcl_t2 = agechild_gr1093, age_mri_t1 = age_child_mri_f09, age_mri_t2 = age_child_mri_f13,
							puberty = puberty13, global_fa_t1 = mean_FA_abcd_f09, global_fa_t2 = mean_FA_abcd_f13,
							global_md_t1 = mean_MD_abcd_f09, global_md_t2 = mean_MD_abcd_f13) 



#####
# Other data prep. 
#####


# create age diff variables for behavior-DTI (need DTI measures first) 
dd7$age_diff_t1 <- dd7$age_mri_t1 - dd7$age_cbcl_t1 
dd7$age_diff_t2 <- dd7$age_mri_t2 - dd7$age_cbcl_t2


####
# Flow chart
#### 

final <- dd7 %>%
  subset(!is.na(int_t1) & !is.na(int_t2) & !is.na(ext_t1) & !is.na(ext_t2)) %>% # inclusion: psych symptoms
  subset(!is.na(global_fa_t1) & !is.na(global_fa_t2) & !is.na(global_md_t1) & !is.na(global_md_t2)) %>%   # inclusion: MRI data
  subset(missingness_abcd_f09 < 2.5 & missingness_abcd_f13 < 2.5) %>% # inclusion: MRI data (NB allowing for some missingness max)
  subset(is.na(dti_overall_qc_f09) & is.na(dti_overall_qc_f13)) %>% # passes qc 
  subset(exclude_incidental_f09 == "include" & exclude_incidental_f13 == "include") %>% # IF 
  subset(mr750_softwareversionshort_dicom == "v24") %>% # correct software version
  subset(!duplicated(mother)) #  include just one of the siblings


# final sample: 1095 children 


#### get comparison sample for attrition analyses ###

dd7b <- dd7[dd7$startfase3_9 == "Yes" & !is.na(dd7$startfase3_9), ] 

vars <- c("idc", "ethn", "sex", "mat_edu", 
          "puberty") 

dd7c <- dd7b[ , names(dd7b) %in% vars]

dd7c$mat_edu <- ordered(dd7c$mat_edu)

saveRDS(dd7c, "GenR_attrition_analyses_sample.rds")



######
# Clean up the dataframes
######

#### rename/recode vars  ###

final <- final %>% 
  mutate(mat_edu = ordered(mat_edu), 
         educm = ordered(educm)) %>%
  rename(id = idc,
         fa_t1 = global_fa_t1, 
         fa_t2 = global_fa_t2, 
         md_t1 = global_md_t1, 
         md_t2 = global_md_t2, 
         anxdep_t1 = sum_anx_9m,
         anxdep_t2 = sum_anx_14, 
         withdep_t1 = sum_wit_9m,
         withdep_t2 = sum_wit_14, 
         somatic_t1 = sum_som_9m, 
         somatic_t2 = sum_som_14, 
         social_t1 = sum_sop_9m, 
         social_t2 = sum_sop_14, 
         thought_t1 = sum_tho_9m, 
         thought_t2 = sum_tho_14, 
         att_t1 = sum_att_9m, 
         att_t2 = sum_att_14, 
         rulebr_t1 = sum_rul_9m, 
         rulebr_t2 = sum_rul_14, 
         agg_t1 = sum_agg_9m,
         agg_t2 = sum_agg_14,
         tot_t1 = cbcl_sum_9m,
         tot_t2 = cbcl_sum_14) %>%
  select(id,  # id vars
         ethn, educm, mat_edu, sex, puberty,
         age_cbcl_t1, age_cbcl_t2, age_mri_t1, age_mri_t2, 
         age_diff_t1, age_diff_t2, # covariates 
         int_t1, ext_t1, int_t2, ext_t2, # psych problems
         fa_t1, fa_t2, md_t1, md_t2, # MRI
         anxdep_t1, anxdep_t2, 
         withdep_t1, withdep_t2, 
         somatic_t1, somatic_t2,
         social_t1, social_t2, 
         thought_t1, thought_t2,
         att_t1, att_t2, 
         rulebr_t1, rulebr_t2, 
         agg_t1, agg_t2,  # 8 syndrome scales
         tot_t1, tot_t2, # 2 total scores
         cgc_md_t1, cgh_md_t1, fma_md_t1, fmi_md_t1, cst_md_t1, atr_md_t1, unc_md_t1, 
         ilf_md_t1, ifo_md_t1, slf_md_t1, # 10 tracts - MD t1
         cgc_fa_t1, cgh_fa_t1, fma_fa_t1, fmi_fa_t1, cst_fa_t1, atr_fa_t1, unc_fa_t1, 
         ilf_fa_t1, ifo_fa_t1, slf_fa_t1, # 10 tracts - FA t1
         cgc_md_t2, cgh_md_t2, fma_md_t2, fmi_md_t2, cst_md_t2, atr_md_t2, unc_md_t2, 
         ilf_md_t2, ifo_md_t2, slf_md_t2, # 10 tracts - MD t2
         cgc_fa_t2, cgh_fa_t2, fma_fa_t2, fmi_fa_t2, cst_fa_t2, atr_fa_t2, unc_fa_t2, 
         ilf_fa_t2, ifo_fa_t2, slf_fa_t2) # 10 tracts - FA t2 


# square root transformation of psychiatric problems

cols <- c("int_t1", "int_t2", 
          "ext_t1", "ext_t2",
          "anxdep_t1", "anxdep_t2", 
          "withdep_t1", "withdep_t2", 
          "somatic_t1", "somatic_t2",
          "social_t1", "social_t2", 
          "thought_t1", "thought_t2",
          "att_t1", "att_t2", 
          "rulebr_t1", "rulebr_t2", 
          "agg_t1", "agg_t2", 
          "tot_t1", "tot_t2")

final[cols] <- lapply(final[cols], sqrt)


### multiply MD by 1000 so it's on a more similar scale to FA ###

md_vars <- grep("md", names(final), value = T)

final[md_vars] <- lapply(final[md_vars], function(x) x*1000)


########
# Save datasets
########

saveRDS(final, "genr_main_data.rds")


#######
# Summary stats
#######

# get the vars 

out1 <- summary(final)

write.csv(out1, paste0(res, "summarystats_genr.csv"))


# get the SDs for numeric vars

temp <- final %>% dplyr::select(where(is.numeric)) 

out2 <- lapply(temp, function(x) sd(x, na.rm = T))

write.csv(out2, paste0(res, "sd_summarystats.csv"))

# rename

dd <- final


### table 1 ###

dd_temp <- dd

dd_temp <- dd[ , c("fa_t1", "md_t1", "fa_t2", "md_t2", "int_t1","int_t2", "ext_t1", "ext_t2",
                  "sex", "ethn",  "mat_edu", "puberty", 
                  "age_mri_t1", "age_mri_t2", "age_diff_t1", "age_diff_t2")]

names(dd_temp)

table_names <- c("Fractional anisotropy t1", "Mean diffusivity t1", "Fractional anisotropy t2", "Mean diffusivity t2", "Internalising problems t1", 
                 "Internalising problems t2", "Externalising problems t1", "Externalising problems t2", "Sex", "National origin",  
                 "Highest achieved maternal education", "Perceived pubertal score",
                 "Age MRI t1", "Age MRI t2", "Age difference between MRI and behavioural assessment t1", "Age difference between MRI and behavioural assessment t2" )

names(dd_temp) <- table_names

table1 <- tbl_summary(dd_temp, include = all_of(table_names)) %>%  modify_header(label = "**Variable**") %>% italicize_levels()


# save table as word doc

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(table1, path = paste0(res, "table1.docx"))


