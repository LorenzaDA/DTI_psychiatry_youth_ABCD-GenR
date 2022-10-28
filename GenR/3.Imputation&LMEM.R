###############
# 3. Imputation & LMEM - GenR
################
# Project: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# Data: Generation R (wave F9 and F13) 
# Aim of script: Run LMEM for internalizing & externalizing problems with global 
# FA & MD  (this is after imputing)
# Author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)

#####
# Environment prep
#####


source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/GenR/0.Source_file_GenR.R")

dd <- readRDS(paste0(indata, "genr_main_data.rds"))


######
# Prep the data
######
# to long data

dd <- dd %>% rename(age_t1 = age_mri_t1,
                    age_t2 = age_mri_t2,
                    agediff_t1 = age_diff_t1, 
                    agediff_t2 = age_diff_t2) %>% 
  select(id, puberty, ethn, sex, mat_edu,
         int_t1, int_t2, 
         ext_t1, ext_t2,
         age_t1, age_t2, agediff_t1, agediff_t2, 
         fa_t1, fa_t2, md_t1, md_t2)


## transform from wide format to long format

dd_long <- dd %>% pivot_longer(
  cols = c("int_t1", "int_t2",
           "ext_t1", "ext_t2",
           "age_t1", "age_t2", "agediff_t1", "agediff_t2", 
           "fa_t1", "fa_t2", "md_t1", "md_t2"), 
  names_to = c(".value", "t"),
  names_sep = "_") 



# make t (time) a factor 

dd_long$t <- factor(dd_long$t)

dd <- dd_long



######
# Other dataframe set ups 
######

# create new vars with just baseline values for outcomes

dd$int_baseline <- NA 
dd$ext_baseline <- NA

dd$fa_baseline <- NA
dd$md_baseline <- NA

# make sure it is all ordered by id and time point 

dd2 <- dd[with(dd, order(id, t)), ]


# create the new values for outcomes at baseline so that the all outcome vars are repeated over time

for (i in 1:nrow(dd2)){ 
  if(dd2$t[i] == "t1"){
    dd2$int_baseline[i] <-  dd2$int[i]
    dd2$ext_baseline[i] <- dd2$ext[i]
    dd2$fa_baseline[i] <-  dd2$fa[i]
    dd2$md_baseline[i] <-  dd2$md[i]
  } else if (dd2$t[i] == "t2"){ 
    dd2$int_baseline[i] <-  dd2$int[i-1]
    dd2$ext_baseline[i] <- dd2$ext[i-1]
    dd2$fa_baseline[i] <-  dd2$fa[i-1]
    dd2$md_baseline[i] <-  dd2$md[i-1]
  } else{ 
    print("something's wrong - check!"[i])
  }
}

# make id a factor 

dd2$id <- factor(dd2$id)

######
# Imputation 
######
# set the imputation settings

predMat <- quickpred(dd2, exclude = c('id')) # prediction matrix

imp0 <- mice(dd2, maxit = 0) # dry run

meth <- imp0$method

# imputation with 30 iterations and 30 datasets

imp_data_dti <- mice(dd2, m=30, maxit = 30, predictorMatrix = predMat, method = meth)


#######
# Run LMEM models 
#######

### Internalising with FA and MD ###

fa_int <- with(imp_data_dti, lmer(fa ~ t*int_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F))  # lagged from int to FA 

int_fa <- with(imp_data_dti, lmer(int ~ t*fa_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from FA to int 

md_int <- with(imp_data_dti, lmer(md ~ t*int_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from int to MD 

int_md <- with(imp_data_dti, lmer(int ~ t*md_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from MD to int



#### Externalising problems with FA and MD ####

fa_ext <- with(imp_data_dti, lmer(fa ~ t*ext_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from ext to FA

ext_fa <- with(imp_data_dti, lmer(ext ~ t*fa_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from FA to ext

md_ext <- with(imp_data_dti, lmer(md ~ t*ext_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from ext to MD

ext_md <- with(imp_data_dti, lmer(ext ~ t*md_baseline + age + agediff + sex + mat_edu + ethn + puberty + (1|id), dd2, REML = F)) # lagged from MD to ext 




# list of results 
all_int <- list(int_fa = int_fa, fa_int = fa_int, int_md = int_md, md_int = md_int)
all_ext <- list(ext_fa = ext_fa, fa_ext = fa_ext, ext_md = ext_md, md_ext = md_ext)


out_int <- lapply(all_int, function(x) summary(pool(x)))
out_ext <- lapply(all_ext, function(x) summary(pool(x)))



# save

write.csv(out_int, paste0(res, "Internalizing_LMEM_GenR.csv"))
write.csv(out_ext, paste0(res, "Externalizing_LMEM_GenR.csv"))
