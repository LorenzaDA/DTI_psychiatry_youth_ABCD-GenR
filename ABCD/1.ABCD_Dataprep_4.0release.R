#################################
# 0. DATA PREP FOR MAIN ANALYSES 
#################################
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: prepping the data before use for analyses 
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl)

######
# Environment prep
######

# load the relevant source file

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

### load in data ###

demo <- read.delim(paste0(indata, "abcd_lpds01.txt"), header = T, na.strings=c("","NA")) # demographics data 

cbcl <- read.delim(paste0(indata, "abcd_cbcls01.txt"), header = T, na.strings=c("","NA")) # psychiatric problems data

dti <- read.delim(paste0(indata, "abcd_dmdtifp101.txt"), header = T, na.strings=c("","NA")) # dti data

dti_incl <- read.delim(paste0(indata, "abcd_imgincl01.txt"), header = T, na.strings=c("","NA")) # recommended inclusion dti data

ethn_sib <- read.delim(paste0(indata, "acspsw03.txt"), header = T, na.strings=c("","NA")) # for ethnicity data & familiarity 

pub_child <- read.delim(paste0(indata, "abcd_ssphy01.txt"), header = T, na.strings=c("","NA")) # child rated perceived pubertal stage

site <- read.delim(paste0(indata, "abcd_lt01.txt"), header = T, na.strings=c("","NA")) # this has more rows than the others - there seem to be multiple follow-up waves not included in other datasets
# e.g. 6 / 18 months follow up

incfind <- read.delim(paste0(indata, "abcd_mrfindings02.txt"), header = T, na.strings=c("","NA")) # incidental findings

# NB most of these datasets have different n observations. 


######
# Merging
######

#### merge demo & dti data #####

# select relevant cols & merge demographics and DTI 

demo <- demo[ , c("sex", "demo_prnt_ed_v2_l", "src_subject_id", "eventname")]

dti <- dti[, c("dmdtifp1_38", "dmdtifp1_80", "src_subject_id", 
               "eventname", "interview_age", 
               # FA tracts
               "dmdtifp1_3", "dmdtifp1_5", "dmdtifp1_7", "dmdtifp1_9", 
               "dmdtifp1_11", "dmdtifp1_13", "dmdtifp1_15", "dmdtifp1_20",
               "dmdtifp1_17", "dmdtifp1_18", 
               "dmdtifp1_4", "dmdtifp1_6", "dmdtifp1_8", "dmdtifp1_10", 
               "dmdtifp1_12", "dmdtifp1_14", "dmdtifp1_16", "dmdtifp1_21", 
               # MD tracts
               "dmdtifp1_45", "dmdtifp1_47", "dmdtifp1_49", "dmdtifp1_51", 
               "dmdtifp1_53", "dmdtifp1_55", "dmdtifp1_57", "dmdtifp1_62", 
               "dmdtifp1_59", "dmdtifp1_60", 
               "dmdtifp1_46", "dmdtifp1_48", "dmdtifp1_50", "dmdtifp1_52",
               "dmdtifp1_54", "dmdtifp1_56", "dmdtifp1_58", "dmdtifp1_63"
               )]


# order

demo <- demo[with(demo, order(src_subject_id, eventname)), ] # order by id and time-period (bc we are working with longitudinal data - long format)
dti <- dti[with(dti, order(src_subject_id, eventname)), ]


# rename the interview age variable - other datasets have interview age as var.- this is to avoid confusion 

dti$interview_age.dti <- dti$interview_age
dti$interview_age <- NULL


# merge

merged1 <- merge(demo, dti, by = c("src_subject_id", "eventname"), all = T)



### merge  with cbcl ###

# select relevant columns 

cbcl <- cbcl[, c("src_subject_id", "eventname", "cbcl_scr_syn_internal_r", 
                 "cbcl_scr_syn_external_r", "interview_age",
                 "cbcl_scr_syn_anxdep_r", "cbcl_scr_syn_withdep_r", 
                 "cbcl_scr_syn_somatic_r", "cbcl_scr_syn_social_r",
                 "cbcl_scr_syn_thought_r", "cbcl_scr_syn_attention_r",
                 "cbcl_scr_syn_rulebreak_r", "cbcl_scr_syn_aggressive_r", 
                 "cbcl_scr_syn_totprob_r")]

cbcl$interview_age.cbcl <- cbcl$interview_age # change the name of interview age or you'll have duplicates
cbcl$interview_age <- NULL


# order

cbcl <- cbcl[with(cbcl, order(src_subject_id, eventname)), ] 
merged1 <- merged1[with(merged1, order(src_subject_id, eventname)), ]


# merge

merged2 <- merge(merged1, cbcl, by = c("src_subject_id", "eventname"), all = T) 



### merge with dti inclusion data ###

# select relevant vars 

dti_incl <- dti_incl[, c("imgincl_dmri_include", "src_subject_id", "eventname")]


# order 

merged2 <- merged2[with(merged2, order(src_subject_id, eventname)), ] 
dti_incl <- dti_incl[with(dti_incl, order(src_subject_id, eventname)), ]

# merge

merged3 <- merge(merged2, dti_incl, by = c("src_subject_id", "eventname"), all = T) 



### merge with ethnicity & family data ###

ethn_sib <- ethn_sib[ , c("src_subject_id", "eventname", "rel_family_id", "race_ethnicity")]

# order

ethn_sib <- ethn_sib[with(ethn_sib, order(src_subject_id, eventname)), ] 
merged3 <- merged3[with(merged3, order(src_subject_id, eventname)), ]

# merge

merged4 <- merge(merged3, ethn_sib, by = c("src_subject_id", "eventname"), all = T) 


### merge with puberty ###

# order 

merged4 <- merged4[with(merged4, order(src_subject_id, eventname)), ]
pub_child <- pub_child[with(pub_child, order(src_subject_id, eventname)), ]

# select vars

pub_child <- pub_child[ , c("src_subject_id", "eventname", "pds_y_ss_female_category_2", "pds_y_ss_male_cat_2")]

# merge 

merged5 <- merge(merged4, pub_child, by = c("src_subject_id", "eventname"), all = T) 



### merge with site ###

# order

merged5 <- merged5[with(merged5, order(src_subject_id, eventname)), ]
site <- site[with(site, order(src_subject_id, eventname)), ]

# select vars

site <- site[ , c("src_subject_id", "eventname", "site_id_l")]

# merge

merged6 <- merge(merged5, site, by = c("src_subject_id", "eventname"), all.x = T)
# here we specify all.x = T because the site dataset has many more eventnames than the other datasets



### merge with IF information ### 

# order

incfind <- incfind[with(incfind, order(src_subject_id, eventname)), ]
merged6 <- merged6[with(merged6, order(src_subject_id, eventname)), ]

# select vars 

names(incfind)
incfind <- incfind[ , c("src_subject_id", "eventname", "mrif_score")]

merged7 <- merge(merged6, incfind, by = c("src_subject_id", "eventname"), all = T)



# rename df for ease of use

dd <- merged7 



#####
# Data-frame checks
#####

head(dd)
tail(dd) # there is a line with description of the dataset
str(dd) # all vars are chr --> need to change that
names(dd)



# change variable type

dd2 <- dd %>% 
  # there's a row describing what the variable contains but not having values - need to filter for it! 
  filter(!sex=="Sex of subject at birth") %>%  
  # change variable type for each variable in the dataset
  mutate(t = factor(eventname, labels = c("1yFU", "t2", "3yFU", "t1")), # when you use levels(factor(dd$eventname)) it shows the order it uses is 1y FU, 2y, 3 and baseline.
         # we are interested in baseline and 2y so we call them t1 and t2
         sex = factor(sex),
         parent_edu = factor(demo_prnt_ed_v2_l, ordered = T),
         ethn = factor(race_ethnicity, levels = c(1,2,3,4,5), labels = c("white",  "black",  "hispanic", "asian", "other")),
         FA = as.numeric(dmdtifp1_38),
         MD = as.numeric(dmdtifp1_80),
         age_cbcl = as.numeric(interview_age.cbcl),
         age_dti = as.numeric(interview_age.dti),
         int = as.numeric(cbcl_scr_syn_internal_r),
         ext = as.numeric(cbcl_scr_syn_external_r),
         anxdep = as.numeric(cbcl_scr_syn_anxdep_r), 
         withdep = as.numeric(cbcl_scr_syn_withdep_r), 
         somatic = as.numeric(cbcl_scr_syn_somatic_r), 
         social = as.numeric(cbcl_scr_syn_social_r),
         thought = as.numeric(cbcl_scr_syn_thought_r), 
         att = as.numeric(cbcl_scr_syn_attention_r),
         rulebr = as.numeric(cbcl_scr_syn_rulebreak_r), 
         agg = as.numeric(cbcl_scr_syn_aggressive_r), 
         tot = as.numeric(cbcl_scr_syn_totprob_r),
         dti_incl = factor(imgincl_dmri_include),
         incfind = factor(mrif_score),
         pub_female = factor(pds_y_ss_female_category_2, levels = c(1,2,3,4,5),labels = c("pre-puberty", "early-puberty", "mid-puberty", "late-puberty", "post-puberty")),
         pub_male = factor(pds_y_ss_male_cat_2, levels = c(1,2,3,4,5), labels = c("pre-puberty", "early-puberty", "mid-puberty", "late-puberty", "post-puberty")),
         site = factor(site_id_l),
         id = src_subject_id,
         family_id = rel_family_id, 
         r_cgc_fa = as.numeric(dmdtifp1_3), 
         r_cgh_fa = as.numeric(dmdtifp1_5), 
         r_cst_fa = as.numeric(dmdtifp1_7),
         r_atr_fa = as.numeric(dmdtifp1_9), 
         r_unc_fa = as.numeric(dmdtifp1_11), 
         r_ilf_fa = as.numeric(dmdtifp1_13), 
         r_ifo_fa = as.numeric(dmdtifp1_15), 
         r_slf_fa = as.numeric(dmdtifp1_20), 
         fma_fa = as.numeric(dmdtifp1_17),  
         fmi_fa = as.numeric(dmdtifp1_18), 
         l_cgc_fa = as.numeric(dmdtifp1_4), 
         l_cgh_fa = as.numeric(dmdtifp1_6), 
         l_cst_fa = as.numeric(dmdtifp1_8), 
         l_atr_fa = as.numeric(dmdtifp1_10), 
         l_unc_fa = as.numeric(dmdtifp1_12), 
         l_ilf_fa = as.numeric(dmdtifp1_14), 
         l_ifo_fa = as.numeric(dmdtifp1_16), 
         l_slf_fa = as.numeric(dmdtifp1_21), 
         
         r_cgc_md = as.numeric(dmdtifp1_45), 
         r_cgh_md = as.numeric(dmdtifp1_47), 
         r_cst_md = as.numeric(dmdtifp1_49), 
         r_atr_md = as.numeric(dmdtifp1_51), 
         r_unc_md = as.numeric(dmdtifp1_53), 
         r_ilf_md = as.numeric(dmdtifp1_55),
         r_ifo_md = as.numeric(dmdtifp1_57), 
         r_slf_md = as.numeric(dmdtifp1_62), 
         
         fma_md = as.numeric(dmdtifp1_59), 
         fmi_md = as.numeric(dmdtifp1_60), 
         
         l_cgc_md = as.numeric(dmdtifp1_46), 
         l_cgh_md = as.numeric(dmdtifp1_48), 
         l_cst_md = as.numeric(dmdtifp1_50), 
         l_atr_md = as.numeric(dmdtifp1_52), 
         l_unc_md = as.numeric(dmdtifp1_54), 
         l_ilf_md = as.numeric(dmdtifp1_56),
         l_ifo_md = as.numeric(dmdtifp1_58), 
         l_slf_md = as.numeric(dmdtifp1_63)
         ) %>% 
  # select only columns we need
 select(t, sex, parent_edu, ethn, FA, MD, age_cbcl, age_dti, int, ext, dti_incl, incfind,
        pub_female, pub_male, site, id, family_id, anxdep, withdep, 
        somatic, social, thought, att, rulebr, agg, tot,
        r_cgc_fa, r_cgh_fa, r_cst_fa, r_atr_fa, r_unc_fa, r_ilf_fa, r_ifo_fa, 
        r_slf_fa, fma_fa, fmi_fa, l_cgc_fa, l_cgh_fa, l_cst_fa, l_atr_fa, 
        l_unc_fa, l_ilf_fa, l_ifo_fa, l_slf_fa, 
        r_cgc_md, r_cgh_md, r_cst_md, r_atr_md, r_unc_md, r_ilf_md, 
        r_ifo_md, r_slf_md, fma_md, fmi_md, 
        l_cgc_md, l_cgh_md, l_cst_md, l_atr_md, l_unc_md, 
        l_ilf_md, l_ifo_md, l_slf_md
        ) 



#####
# Put together left and right hemisphere information
#####
# per tract, per value (md or fa) we want to put together the lh and rh & get an average
# NB forceps major and minor are already ready for use


to_match <- c("_fa","_md") # to get all cols names with _fa and _md in the line below

all_tracts <- grep(paste0(to_match, collapse = "|"), names(dd2), value = T) # 36 as expected

exc <- c("fmi_fa", "fma_fa", "fmi_md", "fma_md") # set the variables 
#you want to exclude from all tracts because they do not need to be averaged bw lh and rh hemi

all_tracts <- all_tracts[!all_tracts %in% exc ] # this selects only tracts which are relevant to us 
# i.e., they are all tracts which have something like _fa or _md in their name, but excluding (exc) the fmi and fma
# tracts 


# get average FA per tract 

dd2$cgc_fa <- (dd2$l_cgc_fa + dd2$r_cgc_fa)/2

dd2$cgh_fa <- (dd2$l_cgh_fa + dd2$r_cgh_fa)/2

dd2$cst_fa <- (dd2$l_cst_fa + dd2$r_cst_fa)/2

dd2$atr_fa <- (dd2$l_atr_fa + dd2$r_atr_fa)/2

dd2$unc_fa <- (dd2$l_unc_fa + dd2$r_unc_fa)/2

dd2$ilf_fa <- (dd2$l_ilf_fa + dd2$r_ilf_fa)/2

dd2$ifo_fa <- (dd2$l_ifo_fa + dd2$r_ifo_fa)/2

dd2$slf_fa <- (dd2$l_slf_fa + dd2$r_slf_fa)/2


# get average MD per tract

dd2$cgc_md <- (dd2$l_cgc_md + dd2$r_cgc_md)/2

dd2$cgh_md <- (dd2$l_cgh_md + dd2$r_cgh_md)/2

dd2$cst_md <- (dd2$l_cst_md + dd2$r_cst_md)/2

dd2$atr_md <- (dd2$l_atr_md + dd2$r_atr_md)/2

dd2$unc_md <- (dd2$l_unc_md + dd2$r_unc_md)/2

dd2$ilf_md <- (dd2$l_ilf_md + dd2$r_ilf_md)/2

dd2$ifo_md <- (dd2$l_ifo_md + dd2$r_ifo_md)/2

dd2$slf_md <- (dd2$l_slf_md + dd2$r_slf_md)/2


dd2 <- dd2[, !names(dd2) %in% all_tracts] # exclude the vars which you 
# just averaged because you don't need the info by hemisphere anymore


names(dd2)


#####
# Fix site information - this was wrong for some children as specified in the ABCD release 4.0 notes
#####
# issues specified in release 4.0, document 3a. NDA 4.0 Changes and Known Issues (title demographics, subtitle incorrect site_id_l reported)
# this document is publicly available at https://nda.nih.gov/study.html?id=1299
# change incorrect values to correct ones as specified in the guide

dd2[dd2$id == "NDAR_INV2HYAENE6" & dd2$t == "t2", "site"] <-  "site08"
dd2[dd2$id == "NDAR_INV4DVGGJE9" & dd2$t == "t1", "site"] <- "site22"
dd2[dd2$id == "NDAR_INV6JF8WUYT" & dd2$t == "t1", "site"] <- "site21"
dd2[dd2$id == "NDAR_INV6WV9X2KM" & dd2$t == "t1", "site"] <- "site22"
dd2[dd2$id == "NDAR_INV6WV9X2KM" & dd2$t == "1yFU", "site"] <- "site21"

dd2[dd2$id == "NDAR_INV6WV9X2KM" & dd2$t == "t2", "site"] <- "site21"
dd2[dd2$id == "NDAR_INVC7P1CVEU" & dd2$t == "t1", "site"] <- "site17"
dd2[dd2$id == "NDAR_INVG19M2F39" & dd2$t == "t1", "site"] <- "site06"
dd2[dd2$id == "NDAR_INVGFCRX7YW" & dd2$t == "t1", "site"] <- "site17"
dd2[dd2$id == "NDAR_INVGVPPRTDN" & dd2$t == "t1", "site"] <- "site20"

dd2[dd2$id == "NDAR_INVHAP0JZTR" & dd2$t == "t1", "site"] <- "site05"
dd2[dd2$id == "NDAR_INVJHCBZTEX" & dd2$t == "t1", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVJHCBZTEX" & dd2$t == "1yFU", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVJHCBZTEX" & dd2$t == "t2", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVLVLHRL2N" & dd2$t == "t1", "site"] <- "site21"

dd2[dd2$id == "NDAR_INVNTAR3TAF" & dd2$t == "t1", "site"] <- "site17"
dd2[dd2$id == "NDAR_INVNTAR3TAF" & dd2$t == "1yFU", "site"] <- "site17"
dd2[dd2$id == "NDAR_INVR0TYK5V9" & dd2$t == "t1", "site"] <- "site22"
dd2[dd2$id == "NDAR_INVRY96FYZ8" & dd2$t == "t1", "site"] <- "site05"
dd2[dd2$id == "NDAR_INVRY96FYZ8" & dd2$t == "1yFU", "site"] <- "site05"


dd2[dd2$id == "NDAR_INVT1C2GBHB" & dd2$t == "t1", "site"] <- "site19"
dd2[dd2$id == "NDAR_INVT1C2GBHB" & dd2$t == "1yFU", "site"] <- "site19"
dd2[dd2$id == "NDAR_INVT1C2GBHB" & dd2$t == "t2", "site"] <- "site19"
dd2[dd2$id == "NDAR_INVUB6JP787" & dd2$t == "t1", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVUB6JP787" & dd2$t == "1yFU", "site"] <- "site13"


dd2[dd2$id == "NDAR_INVUFF64VGJ" & dd2$t == "t1", "site"] <- "site16"
dd2[dd2$id == "NDAR_INVUFF64VGJ" & dd2$t == "1yFU", "site"] <- "site16"
dd2[dd2$id == "NDAR_INVUKPZU1JW" & dd2$t == "t1", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVUKPZU1JW" & dd2$t == "1yFU", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVVT14CE3D" & dd2$t == "t1", "site"] <- "site22"


dd2[dd2$id == "NDAR_INVWF7C1DEL" & dd2$t == "t1", "site"] <- "site09"
dd2[dd2$id == "NDAR_INVXLFHB010" & dd2$t == "t1", "site"] <- "site16"
dd2[dd2$id == "NDAR_INVY92TEZW6" & dd2$t == "t1", "site"] <- "site13"
dd2[dd2$id == "NDAR_INVY92TEZW6" & dd2$t == "1yFU", "site"] <- "site13"



#####
# Reshape from long format to wide
#####

dd2 <- as_tibble(dd2)

### pivot to wider - i.e. get the wide format ###

df.wide <- dd2 %>% 
  pivot_wider(
    names_from = t, # the variable you want to use for the name of the vars
    id_cols = id, # variable identifying the ids  
    values_from = c(ext, site, pub_male, pub_female,
                    age_cbcl, age_dti, FA, MD, int, dti_incl, family_id,
                    sex, ethn, parent_edu, anxdep, withdep, somatic, social,
                    thought, att, rulebr, agg, tot,
                    cgc_fa, cgh_fa, cst_fa, atr_fa, unc_fa, ilf_fa,
                    ifo_fa, slf_fa, cgc_md, cgh_md, cst_md, 
                    atr_md, unc_md, ilf_md, ifo_md, slf_md,
                    fma_fa, fmi_fa, fma_md, fmi_md, incfind), # vars that we need to move to wide format 
    names_sep = "_" # separator for the name of the vars - it's gonna be e.g. ext_t1
  )


####
# Clean up
####

### keep only cols needed ###
# delete all cols that have only NAs
# this happens because we have a timepoint (1y and 3y FU which has no data for most vars.)

dd2 <- df.wide[ , colSums(is.na(df.wide)) < nrow(df.wide)] 

# delete any cols from the 3yFU and 1yFU (not relevant except for one)

to_match2 <- c("3yFU", "1yFU") 

vec <- grep(paste0(to_match2, collapse = "|"), names(dd2), value = T)

vec # vector containing all vars from the timepoints we don't need 
# BUT we need the parent_edu_1yFU variable so make sure to exclude that from the vector

vec2 <- vec[!vec %in% "parent_edu_1yFU"]


# now exclude all vars which are in vec2

dd2 <- dd2[ , !names(dd2) %in% vec2]



######
# Subject selection  
######

final <- dd2 %>%
  subset(!is.na(int_t1) & !is.na(int_t2) & !is.na(ext_t1) & !is.na(ext_t2)) %>% # inclusion: psych symptoms
  subset(!is.na(FA_t1) & !is.na(FA_t2) & !is.na(MD_t1) & !is.na(MD_t2)) %>%   # inclusion: MRI data
  subset(dti_incl_t1 == 1 & dti_incl_t2 == 1 & !is.na(dti_incl_t2) & !is.na(dti_incl_t1)) %>% # exclusion: recommended inclusion criteria for DTI
  subset((incfind_t1 == 1 | incfind_t1 == 2) & (incfind_t2 == 1 | incfind_t2 == 2)) %>% # no IF 
  filter(site_t1 == site_t2) %>% # posthoc criterion: did not change site across the two time points
  subset(!duplicated(family_id_t1)) #  include just one of the siblings

# 4,605 children

######
# Data checks
######

# also check - are there any invalid values that you need to transform? (999, 888 etc) or are they already set to invalid?

summary(final) # the max for FA at t1 is very high. it's impossible to have such high FA 
# globally. not even such high values are seen for the corpus callosum. Same for CST FA and CGH FA
# and CGC, ATR, UNC, ILF, IFO, SLF. 

# check who has got such high vals 

which.max(final$FA_t1) # 2961

check <- final[2961, c("id", "FA_t1", "FA_t2", "cgc_fa_t1", "cgh_fa_t1", "cgc_fa_t1", "atr_fa_t1", "unc_fa_t1", 
            "ilf_fa_t1", "ifo_fa_t1", "slf_fa_t1", "cgc_fa_t2", "cgh_fa_t2", "cgc_fa_t2", "atr_fa_t2",
            "unc_fa_t2", "ilf_fa_t2", "ifo_fa_t2", "slf_fa_t2")] 

# values at t1 for this kid are extremely high (for cgh we literally have 0.999)
# then the values are in the norm at t2. I think at t1 something went wrong with the recording of the values

# exclude the kid

final <- final[-2961, ]
summary(final)


### change data so it is interpretable ####

# from years to month for ages

final$age_dti_t1 <- final$age_dti_t1/12
final$age_dti_t2 <- final$age_dti_t2/12
final$age_cbcl_t1 <- final$age_cbcl_t1/12
final$age_cbcl_t2 <- final$age_cbcl_t2/12


### transform cbcl data which is very skewed ###
# square root transformation 

names(final)

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

summary(final[cols])


### re-code education ###
# currently there are many levels of the var education 
# to preserve df we will re-code into 3 levels: low, middle, high 
# it now has 22 levels 

final$parent_edu <- final$parent_edu_1yFU # rename var

final <- final %>% mutate(parent_edu = recode_factor(parent_edu,
                                 "1" = "low",
                                 "2" = "low",
                                 "3" = "low",
                                 "4" = "low",
                                 "5" = "low",
                                 "6" = "low", 
                                 "7" = "low", 
                                 "8" = "low", 
                                 "9" = "low", 
                                 "10" = "low", 
                                 "11" = "low", 
                                 "12" = "low", 
                                 "13" = "intermediate", 
                                 "14" = "intermediate", 
                                 "15" = "intermediate",
                                 "16" = "intermediate", 
                                 "17" = "intermediate", 
                                 "18" = "high", 
                                 "19" = "high", 
                                 "20" = "high", 
                                 "21" = "high", 
                                 "777" = "refuse_answer"))  


# clean up from 777/refuse_answer ###

final <- final %>% replace_with_na(replace = list(parent_edu = c("refuse_answer")))

final$parent_edu <- factor(final$parent_edu, ordered = T) # re-code as factor (so it drops the empty level "refuse_answer")

summary(final$parent_edu)


### check - is age dti and age cbcl same? ###

all.equal(final$age_cbcl_t1, final$age_dti_t1) # T 
all.equal(final$age_cbcl_t2, final$age_dti_t2) # T


# keep just one & rename as age

final$age_cbcl_t1 <- NULL
final$age_cbcl_t2 <- NULL

final <- final %>% rename(age_t1 = age_dti_t1,
                      age_t2 = age_dti_t2)


### put puberty (male/female) into one col ### 
# use puberty t2 like in GenR to harmonise across datasets

# loop to see whether any participant has data on both puberty for male and females

for(i in 1:nrow(final)){
  if(!is.na(final$pub_male_t2[i]) & !is.na(final$pub_female_t2[i])){
    stop("both values present!"[i])} 
} 


# create new puberty var 

final$puberty_t2 <- as.numeric(NA)

# fill in the puberty var with puberty info from males and females values 
# or leave it NA if there is neither data

for(i in 1:nrow(final)){ 
  if(!is.na(final$pub_male_t2[i])){
    final$puberty_t2[i] <- final$pub_male_t2[i]
  }else if(!is.na(final$pub_female_t2[i])){
      final$puberty_t2[i] <- final$pub_female_t2[i]
  }else if(is.na(final$pub_male_t2[i]) & is.na(final$pub_female_t2[i])){
        final$puberty_t2[i] <- final$puberty_t2[i]
        }else(stop("there might be an error!!!"[i]))
}


cbind(final$puberty_t2, final$pub_female_t2, final$pub_male_t2) # it worked


# make it a factor with 5 levels categorised from pre to post puberty, as done in the original var.  

final$puberty_t2 <- factor(final$puberty_t2, ordered = T, 
                         levels = c(1,2,3,4,5),
                         labels = c("pre-puberty", "early-puberty", "mid-puberty", "late-puberty", "post-puberty"))


# rename a few vars

final$sex <- final$sex_t1
final$ethn <- final$ethn_t1
final$puberty <- final$puberty_t2


# the var site has 0 cases in some categories - delete by remaking the var a factor

final$site_t1 <- factor(final$site_t1)
final$site_t2 <- factor(final$site_t2)


########
# Create table 1
########

# select relevant vars

finalb <- final[ , c("FA_t1", "MD_t1", "FA_t2", "MD_t2", "int_t1","int_t2", 
                 "ext_t1", "ext_t2", "sex", "ethn",  "parent_edu", "age_t1", 
                 "age_t2", "puberty", "site_t1")]

names(finalb)

table_names <- c("Fractional anisotropy t1", "Mean diffusivity t1", "Fractional anisotropy t2", "Mean diffusivity t2", "Internalising problems t1", 
                         "Internalising problems t2", "Externalising problems t1", "Externalising problems t2", "Sex", "Ethnicity", 
                 "Highest achieved parental education", "Age t1", "Age t2","Perceived pubertal stage", "Site")

names(finalb) <- table_names

table1 <- tbl_summary(finalb, include = all_of(table_names)) %>% 
          modify_header(label = "**Variable**") %>% 
          italicize_levels()


table1    


# save table as word doc

table1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(table1, 
                          path = paste0(res, "table1_ABCD.docx"))





#####
# Clean up dataset and save
#####

### keep just useful columns ###

drop <- c("family_id_t1", "dti_incl_t1", "pub_male_t1", "pub_female_t1", 
          "pub_female_t2", "pub_male_t2", "dti_incl_t2",
          "ethn_t1", "sex_t1", "sex_t2", "puberty_t1", "puberty_t2", 
          "site_t2", "incfind_t2", "incfind_t1",
          "parent_edu_t1", "parent_edu_1yFU")

dd6 <- final[ , !names(final) %in% drop]


### save sumstats ###

temp <- summary(dd6)

write.csv(temp, paste0(res, "summarystatsABCD.csv"))

temp <- dd6 %>% dplyr::select(where(is.numeric)) 

out2 <- lapply(temp, function(x) sd(x, na.rm = T))

write.csv(out2, paste0(res, "sd_summarystats_ABCD.csv"))


### save data ### 

saveRDS(dd6, paste0(indata, "abcd_main_data.Rds"))



#######
# Check how many children have clinical level symptoms
######

quantile(dd6$int_t1, probs = 0.93)

temp <- dd6[dd6$int_t1 > 3.742, ] # 302 children out of 4605 - 6.6% 

quantile(dd6$int_t2, probs = 0.93)

temp <- dd6[dd6$int_t2 > 3.742, ] # 315 children 

quantile(dd6$ext_t1, probs = 0.93)

temp <- dd6[dd6$ext_t1 > 3.742, ] # 284

quantile(dd6$ext_t2, probs = 0.93)

temp <- dd6[dd6$ext_t2 > 3.742, ] # 237 


237/4605 # min 5.1%
315/4605 # max 6.8%


########
# Figures
#########

# check the data distributions 

### histograms for main variables ###

png(paste0(res, "hists_dti_ABCD.png"))
par(mfrow = c(2,2))
hist(final$FA_t1, nclass = 50, main = "FA t1", col="blue")
hist(final$FA_t2, nclass = 50, main = "FA t2", col="blue")
hist(final$MD_t1, nclass = 50, main = "MD t1", col="blue")
hist(final$MD_t2, nclass = 50, main = "MD t2", col="blue")
dev.off()

png(paste0(res, "hists_cbcl_ABCD.png"))
par(mfrow = c(2,2))
hist(final$int_t1, nclass = 50, main = "Internalising problems t1", col="blue")
hist(final$int_t2, nclass = 50, main = "Internalising problems t2", col="blue")
hist(final$ext_t1, nclass = 50, main = "Externalising problems t1", col="blue")
hist(final$ext_t2, nclass = 50, main = "Externalising problems t2", col="blue")
dev.off()



### plots for two variables ###

## between outcomes and predictors ##

# for int 
a <- ggplot(final, aes(int_t1, FA_t2)) +
  geom_smooth()
b <- ggplot(final, aes(int_t1, MD_t2)) +
  geom_smooth()
c <- ggplot(final, aes(FA_t1, int_t2)) +
  geom_smooth()
d <- ggplot(final, aes(MD_t1, int_t2)) + 
  geom_smooth()


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("BrainBeh_internalising_cont_ABCD.png", path = res)  


# for ext 
a <- ggplot(final, aes(ext_t1, FA_t2)) +
  geom_smooth()
b <- ggplot(final, aes(ext_t1, MD_t2)) +
  geom_smooth()
c <- ggplot(final, aes(FA_t1, ext_t2)) +
  geom_smooth()
d <- ggplot(final, aes(MD_t1, ext_t2)) +
  geom_smooth()


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("BrainBeh_externalising_cont_ABCD.png", path = res)  



### plots for outcomes and predictors adjusting for baseline levels ####


# for int 
a <- ggplot(final, aes(int_t1, FA_t2)) +
  geom_smooth()
b <- ggplot(final, aes(int_t1, MD_t2)) +
  geom_smooth()
c <- ggplot(final, aes(FA_t1, int_t2)) +
  geom_smooth()
d <- ggplot(final, aes(MD_t1, int_t2)) + 
  geom_smooth()


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("BrainBeh_internalising_cont_ABCD.png", path = res)  


# for ext 
a <- ggplot(final, aes(ext_t1, FA_t2)) +
  geom_smooth()
b <- ggplot(final, aes(ext_t1, MD_t2)) +
  geom_smooth()
c <- ggplot(final, aes(FA_t1, ext_t2)) +
  geom_smooth()
d <- ggplot(final, aes(MD_t1, ext_t2)) +
  geom_smooth()


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("BrainBeh_externalising_cont_ABCD.png", path = res)  



### violin plots for covariates  ###

# for int 
a <- ggplot(final, aes(x = sex, y = int_t2, fill = sex)) + geom_violin(trim = F)
b <- ggplot(final, aes(x = parent_edu, y = int_t2, fill = parent_edu)) + geom_violin(trim = F)
c <- ggplot(final, aes(x = ethn, y = int_t2, fill = ethn)) + geom_violin(trim = F)
d <- ggplot(final, aes(x = puberty, y = int_t2, fill = puberty)) + geom_violin(trim = F)


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("Int_by_covs_ABCD.png", path = res)  


# for ext
a <- ggplot(final, aes(x = sex, y = ext_t2, fill = sex)) + geom_violin(trim = F)
b <- ggplot(final, aes(x = parent_edu, y = ext_t2, fill = parent_edu)) + geom_violin(trim = F)
c <- ggplot(final, aes(x = ethn, y = ext_t2, fill = ethn)) + geom_violin(trim = F)
d <- ggplot(final, aes(x = puberty, y = ext_t2, fill = puberty)) + geom_violin(trim = F)

all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("Ext_by_covs_ABCD.png", path = res) 


# for FA
a <- ggplot(final, aes(x = sex, y = FA_t2, fill = sex)) + geom_violin(trim = F)
b <- ggplot(final, aes(x = parent_edu, y = FA_t2, fill = parent_edu)) + geom_violin(trim = F)
c <- ggplot(final, aes(x = ethn, y = FA_t2, fill = ethn)) + geom_violin(trim = F)
d <- ggplot(final, aes(x = puberty, y = FA_t2, fill = puberty)) + geom_violin(trim = F)


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all
ggsave("FA_by_covs_ABCD.png", path = res) 


# for MD
a <- ggplot(final, aes(x = sex, y = MD_t2, fill = sex)) + geom_violin(trim = F)
b <- ggplot(final, aes(x = parent_edu, y = MD_t2, fill = parent_edu)) + geom_violin(trim = F)
c <- ggplot(final, aes(x = ethn, y = MD_t2, fill = ethn)) + geom_violin(trim = F)
d <- ggplot(final, aes(x = puberty, y = MD_t2, fill = puberty)) + geom_violin(trim = F)


all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)
all

ggsave("MD_by_covs_ABCD.png", path = res) 



#### syndrome scales ####
# some examples

a <- ggplot(final, aes(social_t1, FA_t2)) + geom_smooth()
b <- ggplot(final, aes(anxdep_t1, FA_t2)) + geom_smooth()
c <- ggplot(final, aes(FA_t1, social_t2)) + geom_smooth()
d <- ggplot(final, aes(FA_t1, anxdep_t2)) + geom_smooth()

all <- ggarrange(a, b, c, d + rremove("x.text"), 
                 labels = c("A", "B", "C", "D"),
                 ncol = 2, nrow = 2)

all

ggsave("syndromes_FA_ABCD.png", path = res) 



#### dti tracts ### 

a <- ggplot(final, aes(ext_t1, slf_fa_t2)) + geom_smooth()
b <- ggplot(final, aes(slf_fa_t1, ext_t2)) + geom_smooth()
c <- ggplot(final, aes(int_t1, cgc_fa_t2)) + geom_smooth()
d <- ggplot(final, aes(cgc_fa_t1, int_t2)) + geom_smooth()


all <- ggarrange(a, b, c, d + rremove("x.text"), 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

all

ggsave("DTItracts_intext_ABCD.png", path = res) 

