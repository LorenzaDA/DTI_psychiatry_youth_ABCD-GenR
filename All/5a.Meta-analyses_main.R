#####################################################
# 2. Meta-analysis of lagged paths for ABCD & GenR 
####################################################
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: Generation R (wave F9 and F13), ABCD (release 4.0, data at wave at 9-10, 11-12)
# AIM OF SCRIPT: Run meta-analysis of CLPM lagged paths GenR & ABCD
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl; lorenza.dallaglio1@gmail.com)


#######
# Set environment
######
# NB you need to have extracted estimates and standard errors for each lagged path you are interested in 
# and saved it into a dataframe 

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

indata_genr <- "/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/output/results/GenR/June2022/"

indata_abcd <- "/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/output/results/June2022/"


abcd <- read.csv(paste0(indata_abcd, "results_main_abcd.csv"), header = T) # this is your df with the estimates and SE for abcd

genr <- read.csv(paste0(indata_genr, "results_main_genr.csv"), header = T) # likewise for genr



######
# Meta-analyse
######
# we use the metagen function, which is for generic inverse variance meta-analysis
# inverse variance method used for pooling
# we need to fill in the estimates (TE) & standard errors (seTE)
# info from https://cran.r-project.org/web/packages/meta/meta.pdf

#### put together the abcd and genr data #### 
# so you have a df with cols for GenR and ABCD with estimates and SE 

all <- data.frame(model = 1:8,
                  es.abcd = as.numeric(NA), 
                  es.genr = as.numeric(NA),
                  se.abcd = as.numeric(NA), 
                  se.genr = as.numeric(NA))


# fill in the new df 

all$model <- genr$X 
all$es.abcd <- abcd$est.std 
all$es.genr <- genr$est.std
all$se.abcd <- abcd$se
all$se.genr <- genr$se 
all$p.abcd <- abcd$pvalue
all$p.genr <- genr$pvalue

all


###########
# Meta-analyses
############

# set the n for the projects 
n <- c(4605, 1095) # abcd n first and genr later 

# set labels for the results 
studlab <- c("abcd", "genr")


# create empty vectors and lists you'll need in the loop 

estimates <- c() # estimates to input in metagen function - for a given association you need the two estimates from ABCD & Genr

se <- c() # same as above but for standard errors


results <- list() # list where you'll put the output for each model 

length(results) <- 8 # set the length of the list. 8 bc we have 8 lagged paths here

names(results) <- all$model # set the names of the list, which correspond to the model tested



# create empty measures for your df so you can put results in there

all$es.meta <- as.numeric(NA)

all$se.meta <- as.numeric(NA)

all$p.meta <- as.numeric(NA)


# loop to get results 

for (i in 1:nrow(all)){
  # for each model tested (i.e. rows of the df)
  estimates <- c(all$es.abcd[i], all$es.genr[i])
  # put together the estimates for ABCD and GenR for that given model 
  se <- c(all$se.abcd[i], all$se.genr[i])
  # same for standard errors
  temp <- metagen(estimates, se, data = all, studlab = studlab, n.e = n, comb.fixed = T)
  # run a meta-analysis on those estimates & standard errors for a given model, and the "all" data we inputted, and a given sample size
  results[[i]] <- temp
  # save the results of each meta-analysis into the results list 
  all$p.meta[i] <- temp$pval.fixed
  all$es.meta[i] <-  temp$TE.fixed
  all$se.meta[i] <- temp$seTE.fixed
  # save results also to the df 
}



######
# Save output
######

# prep dataframe for output

names(all)

all2 <- all[, c(1, 8, 9, 10, 2, 4, 6, 3, 5, 7)] # reorder cols 

all2


# save

write.csv(all2, paste0(res, "meta_main_results.csv"), row.names = T)

