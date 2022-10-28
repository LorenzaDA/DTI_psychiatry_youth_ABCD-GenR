##########################
# Source file for CLPMs 
##########################

####
# Model prep for CLPMs 
####

# ordered factors to numeric

dd$puberty_num <- as.numeric(dd$puberty)

dd$parent_edu_num <- as.numeric(dd$parent_edu)


# categorical variables to dummy variables

dd <- data.frame(dd)

rownames(dd) <- dd$id

dd$id <- NULL

dd2 <- fastDummies::dummy_cols(dd) # NB the function dichotomises everything w/o setting ref cateogory so you need to take care of that


# delete reference categories (these are usually the largest categories)
# largest for site = 16, for sex = male, for ethn = white

dd2$site_t1_site16 <- NULL

dd2$sex_M <- NULL

dd2$ethn_white <- NULL

dd <- dd2


########
# General model specifications 
########
# to be used just for exploratory analyses, as first the good fitting model for ABCD
# needs to be found in script 2

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
int_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

var_t2 ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

var_t1 ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
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
ext_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

var_t2 ~ puberty_num + age_t2 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 

var_t1 ~ sex_F + age_t1 + site_t1_site01 + site_t1_site02 + site_t1_site03 + site_t1_site04 + 
site_t1_site05 + site_t1_site06 + site_t1_site07 + site_t1_site08 + site_t1_site09 
+ site_t1_site10 + site_t1_site11 + site_t1_site12 + site_t1_site13 + site_t1_site14
+ site_t1_site15 + site_t1_site17 +  site_t1_site18 +
site_t1_site19 + site_t1_site20 + site_t1_site21 
'



m_fa  <- '
var_t2 + FA_t2_scaled ~ var_t1 + FA_t1_scaled

var_t1 ~~ FA_t1_scaled
var_t2 ~~ FA_t2_scaled

var_t1 ~~ var_t1
FA_t1_scaled ~~ FA_t1_scaled
var_t2 ~~ var_t2
FA_t2_scaled ~~ FA_t2_scaled

var_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

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



m_md <- '
var_t2 + MD_t2_scaled ~ var_t1 + MD_t1_scaled

var_t1 ~~ MD_t1_scaled
var_t2 ~~ MD_t2_scaled

var_t1 ~~ var_t1
MD_t1_scaled ~~ MD_t1_scaled
var_t2 ~~ var_t2
MD_t2_scaled ~~ MD_t2_scaled

var_t2 ~ sex_F + ethn_black + ethn_hispanic + ethn_asian + ethn_other + parent_edu_num

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


