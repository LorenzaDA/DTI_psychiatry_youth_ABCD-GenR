########################################
# 3. Linear mixed effect models - ABCD
########################################
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: running LMEM for FA/MD with int/ext (to mirror CLPMs lagged paths)
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

#####
# Prep environment
#####

# load relevant source file and data

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

dd <- readRDS(paste0(indata, "abcd_main_data.Rds"))


##### 
# Data prep
#####

# select only relevant cols

vars <- c("id", "ext_t1", "ext_t2", "site_t1", "puberty",
           "age_t1", "age_t2", "FA_t1", "FA_t2", "MD_t1", "MD_t2", "int_t1", "int_t2", "parent_edu", 
          "sex", "ethn")

dd2 <- dd[ , names(dd) %in% vars]


# pivot longer - i.e. get long format 
# need to specify time varying cols in cols, .value is necessary, t = new eventname variable, names_sep means the variables that need to go together have the same name before the separator "_"

dd3 <- dd2 %>% pivot_longer(
  cols = c("ext_t1", "ext_t2",
           "age_t1", "age_t2", "FA_t1", "FA_t2", "MD_t1", "MD_t2", "int_t1", "int_t2"), 
  names_to = c(".value", "t"),
  names_sep = "_") 


# make t (time - equivalent to eventname) a factor 
# t1 = baseline wave, t2 = 2y. follow up 

dd3$t <- factor(dd3$t)


# checks to see transformation to long format went fine
# NB ID not displayed to ensure confidentiality 

# dd3[dd3$id == "PUTRELEVANTID", c("ext", "t", "FA")]
# dd2[dd2$id == "PUTRELEVANTID", c("ext_t1", "ext_t2", "FA_t1", "FA_t2")]

# dd3[dd3$id == "PUTRELEVANTID", c("sex", "t")]
# dd2[dd2$id == "PUTRELEVANTID", c("sex")] 

# dd3[dd3$id == "PUTRELEVANTID", c("ethn", "t")]
# dd2[dd2$id == "PUTRELEVANTID", c("ethn")]

# dd3[dd3$id == "PUTRELEVANTID", c("puberty_t2", "t")]
# dd2[dd2$id == "PUTRELEVANTID", c("puberty_t2")]


# create new vars with just baseline values for predictors

dd3$int_baseline <- NA 
dd3$ext_baseline <- NA
dd3$FA_baseline <- NA
dd3$MD_baseline <- NA


# make sure it is all ordered by id and time point 

dd4 <- dd3[with(dd3, order(id, t)), ]


# create the new values for outcomes at baseline so that the all outcome vars are repeated over time
# this was done following https://doi.org/10.1176/appi.ajp.2017.16070813 paper scripts

for (i in 1:nrow(dd4)){ 
  if(dd4$t[i] == "t1"){
    dd4$int_baseline[i] <-  dd4$int[i]
    dd4$ext_baseline[i] <-  dd4$ext[i]
    dd4$FA_baseline[i] <-  dd4$FA[i]
    dd4$MD_baseline[i] <-  dd4$MD[i]
  } else if (dd4$t[i] == "t2"){ 
    dd4$int_baseline[i] <-  dd4$int[i-1]
    dd4$ext_baseline[i] <-  dd4$ext[i-1]
    dd4$FA_baseline[i] <-  dd4$FA[i-1]
    dd4$MD_baseline[i] <-  dd4$MD[i-1]
  } else{ 
    stop("something's wrong - check!"[i])
  }
}

# make id a factor

dd4$id <- factor(dd4$id)


######
# Imputation 
######
# set the imputation settings

predMat <- quickpred(dd4, exclude = c('id')) # prediction matrix

imp0 <- mice(dd4, maxit = 0) # dry run

meth <- imp0$method

# imputation with 30 iterations and 30 datasets

imp_data_dti <- mice(dd4, m=30, maxit = 30, predictorMatrix = predMat, method = meth)

############
# Analyses 
############

## internalising 

fa_int <- with(imp_data_dti, lmer(FA ~ t*int_baseline + age + sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F))  # lagged from int to FA 

int_fa <- with(imp_data_dti, lmer(int ~ t*FA_baseline + age + sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F)) # Fa to int

md_int <- with(imp_data_dti, lmer(MD ~ t*int_baseline + age +  sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F)) # lagged from int to MD 

int_md <- with(imp_data_dti, lmer(int ~ t*MD_baseline + age + sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F)) # lagged from MD to int



## externalising 

fa_ext <- with(imp_data_dti, lmer(FA ~ t*ext_baseline + age + sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F))  # lagged from int to FA 

ext_fa <- with(imp_data_dti, lmer(ext ~ t*FA_baseline + age + sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F))

md_ext <- with(imp_data_dti, lmer(MD ~ t*ext_baseline + age +  sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F)) # lagged from int to MD 

ext_md <- with(imp_data_dti, lmer(ext ~ t*MD_baseline + age + sex + parent_edu + ethn + puberty + site_t1 + (1|id), dd4, REML = F)) # lagged from MD to int



# list of results 

all_int <- list(int_fa = int_fa, fa_int = fa_int, int_md = int_md, md_int = md_int)

all_ext <- list(ext_fa = ext_fa, fa_ext = fa_ext, ext_md = ext_md, md_ext = md_ext)

# pool the results

out_int <- lapply(all_int, function(x) summary(pool(x)))
out_ext <- lapply(all_ext, function(x) summary(pool(x)))


# save

write.csv(out_int, paste0(res, "Internalizing_LMEM_ABCD.csv"))
write.csv(out_ext, paste0(res, "Externalizing_LMEM_ABCD.csv"))




