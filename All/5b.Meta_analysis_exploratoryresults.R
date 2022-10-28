#######################################
# 5. Meta-analyses exploratory results
#######################################
# Meta-analysis of lagged paths from exploratory results for ABCD & GenR 
# PROJECT: Directionality of brain - behaviour associations (DTI - int/ext)
# Project is preregistered at https://osf.io/tf2d6/ and under license CC 4.0 International
# DATA: Generation R (wave F9 and F13), ABCD (release 4.0, data at wave at 9-10, 11-12)
# AIM OF SCRIPT: Run meta-analyses for the exploratory results of the ABCD and GenR studies 
# author: Lorenza Dall'Aglio (l.dallaglio@erasmusmc.nl, lorenza.dallaglio1@gmail.com)


#######
# Set environment
######

rm(list=ls())

library(meta)

indata_genr <- "/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/output/results/GenR/June2022/"

indata_abcd <- "/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/output/results/June2022/"

######
# load data
######
# load the syndrome scales and tracts results for both cohorts

abcd_syn <- read.csv(paste0(indata_abcd, "results_syndromescales_abcd.csv"), header = T)
abcd_tracts <- read.csv(paste0(indata_abcd, "results_tracts_abcd.csv"), header = T)
abcd_tot <- read.csv2(paste0(indata_abcd, "totalproblems_lagged.csv"), header = T)
  
genr_syn <- read.csv(paste0(indata_genr, "results_syndromescales_genr.csv"), header = T)
genr_tracts <- read.csv(paste0(indata_genr, "results_tracts_genr.csv"), header = T)
genr_tot <- read.csv2(paste0(indata_genr, "totalproblems_lagged.csv"), header = T)


#####
# prep data for meta-analysis
#####
# For the meta-analysis we need to have one column for estimates and one for standard errors 
# with the values for both cohorts - this is per syndrome scale or per tract and per lagged path! 
# and per md/fa 
# Create a table where each relationship tested is shown with estimates and 
# standard errors - make a df for each relation (also lagged)


# put together the ABCD and GenR data 
# so you have a df with cols for GenR and ABCD with estimates and SE 

all_syn <- data.frame(1:32, es.abcd = as.numeric(NA), 
                      es.genr = as.numeric(NA),
                      se.abcd = as.numeric(NA), 
                      se.genr = as.numeric(NA))



all_tracts <- data.frame(1:80, es.abcd = as.numeric(NA), 
                      es.genr = as.numeric(NA),
                      se.abcd = as.numeric(NA), 
                      se.genr = as.numeric(NA))


all_tot <- data.frame(1:4, es.abcd = as.numeric(NA), 
                      es.genr = as.numeric(NA),
                      se.abcd = as.numeric(NA), 
                      se.genr = as.numeric(NA))


# fill in the new df 

all_syn$X1.32 <- abcd_syn$X 
all_syn$es.abcd <- abcd_syn$est.std 
all_syn$es.genr <- genr_syn$est.std
all_syn$se.abcd <- abcd_syn$se
all_syn$se.genr <- genr_syn$se 
all_syn$p.genr <- genr_syn$pvalue
all_syn$p.abcd <- abcd_syn$pvalue

all_tracts$X1.80 <- abcd_tracts$X
all_tracts$es.abcd <- abcd_tracts$est.std 
all_tracts$es.genr <- genr_tracts$est.std
all_tracts$se.abcd <- abcd_tracts$se
all_tracts$se.genr <- genr_tracts$se 
all_tracts$p.genr <- genr_tracts$pvalue
all_tracts$p.abcd <- abcd_tracts$pvalue

all_tot$X1.4 <- abcd_tot$X
all_tot$es.abcd <- abcd_tot$est.std
all_tot$es.genr <- genr_tot$est.std
all_tot$se.abcd <- abcd_tot$se
all_tot$se.genr <- genr_tot$se
all_tot$p.genr <- genr_tot$pvalue
all_tot$p.abcd <- abcd_tot$pvalue


#####
# Meta-analyse
#####

# set the n for the projects 
n <- c(4605, 1095) # abcd n first and genr later

# set labels for the results 
studlab <- c("abcd", "genr")


# create empty vectors and lists you'll need in the loop 

estimates <- c() # estimates to input in metagen function - for a given association you need the two estimates from ABCD & Genr
se <- c() # same as above but for standard errors
results <- list() # list where you'll put the output for each model 
length(results) <- 116 # set the length of the list
names(results) <- c(all_syn$X1.32, all_tracts$X1.80, all_tot$X1.4) # set the names of the list, which correspond to the model tested

all_syn$model <- all_syn$X1.32 
all_syn$X1.32 <- NULL

all_tracts$model <- all_tracts$X1.80
all_tracts$X1.80 <- NULL

all_tot$model <- all_tot$X1.4
all_tot$X1.4 <- NULL

all <- rbind(all_syn, all_tracts, all_tot)

all


# create empty measures so you can put results in there
all$es.meta <- as.numeric(NA)
all$se.meta <- as.numeric(NA)
all$p.meta <- as.numeric(NA)
all$cilower.meta<- as.numeric(NA)
all$ciupper.meta <- as.numeric(NA)


# loop to get results 

for (i in 1:nrow(all)){
  # for each model tested (i.e. rows of the df)
  estimates <- c(all$es.abcd[i], all$es.genr[i])
  # put together the estimates for ABCD and GenR for that given model 
  se <- c(all$se.abcd[i], all$se.genr[i])
  # same for standard errors
  temp <- metagen(estimates, se, data = all, studlab = studlab, n.e = n, comb.fixed = T)
  # run a meta-analysis on those estimates & standard errors for a given model and the all_syn data and a given sample size
  results[[i]] <- temp
  # save the results of each meta-analysis into the results list 
  all$p.meta[i] <- temp$pval.fixed
  all$es.meta[i] <-  temp$TE.fixed
  all$se.meta[i] <- temp$seTE.fixed
  # save results also to a df 
  all$cilower.meta[i] <- temp$lower.fixed
  all$ciupper.meta[i] <- temp$upper.fixed
}

all

names(all)

all <- all[, c("model", "es.meta", "se.meta", "p.meta",
               "es.abcd", "se.abcd", "p.abcd", "es.genr", 
               "se.genr", "p.genr")] # reorder the cols  

all


# check which are stat. sign - NB this is before FDR correction

all[all$p.meta < 0.05, ]
# social to FA, atr FA to int, ext to cst FA, atr FA to ext


# which are stat. sign and with ES consistent across genr and abcd 

f <- function(a,b) {ifelse(a == 0 | b == 0, as.logical("FALSE"),!xor(sign(a)+1,sign(b)+1))}

all$correspondence.es <- f(all$es.abcd, all$es.genr)

all[all$correspondence.es == T, ]

sum(all$correspondence.es == T) # 51 coefficients have the same sign for the effect size


# change models description 

all$model <- gsub("_scaled", "", all$model)
all$model <- gsub("FA", "fa", all$model)
all$model <- gsub("MD", "md", all$model)

all$model


# get fdr information & divide by syndrome expl and tracts expl and tot expl

all_syn <- all[1:32, ] # 32 obs - the ones for syndrome scales

all_tracts <- all[33:112, ] # 80 obs. the ones for the tracts 

all_tot <- all[113:116, ]

# check how many ES correspond for syndrome scale and tracts separately 

sum(all_syn$correspondence.es == T) # 18
sum(all_syn$correspondence.es == F) # 14
sum(all_tracts$correspondence.es == T) # 31
sum(all_tracts$correspondence.es == F) # 49
sum(all_tot$correspondence.es == T) # 2 
sum(all_tot$correspondence.es == F) # 2

all_syn$FDR <- p.adjust(all_syn$p.meta, method = "fdr") 

all_tracts$FDR <- p.adjust(all_tracts$p.meta, method = "fdr") 

all_tot$FDR <- p.adjust(all_tot$p.meta, method = "fdr")


# select and reorder the cols for the results 

names(all_tracts)

col_order <- c("model", "es.meta", "se.meta", "p.meta", "FDR", "es.abcd", "se.abcd", "p.abcd", "es.genr",
               "se.genr", "p.genr")

all_tracts2 <- all_tracts[, col_order]

all_syn <- all_syn[ , col_order]

all_tot <- all_tot[ , col_order]

# save 

write.csv(all_syn, paste0(indata_abcd, "meta_syndromes_results.csv"), row.names = T)

write.csv(all_tracts2, paste0(indata_abcd, "meta_tracts_results.csv"), row.names = T)

write.csv(all_tot, paste0(indata_abcd, "meta_totproblems_results.csv"), row.names = T)


# save rounded

rownames(all_syn) <- all_syn$model
all_syn$model <- NULL
all_syn$correspondence.es <- NULL

rownames(all_tracts2) <- all_tracts2$model
all_tracts2$model <- NULL

rownames(all_tot) <- all_tot$model
all_tot$model <- NULL


all_syn <- round(all_syn, digits = 3)
all_tracts2 <- round(all_tracts2, digits = 3)
all_tot <- round(all_tot, digits = 3)


write.csv(all_syn, paste0(indata_abcd, "meta_syndromes_results_rounded.csv"), row.names = T)
write.csv(all_tracts2, paste0(indata_abcd, "meta_tracts_results_rounded.csv"), row.names = T)
write.csv(all_tot, paste0(indata_abcd, "meta_totproblems_results_rounded.csv"), row.names = T)



#####
# Tables 
#####
# divide between direction: brain --> beh vs. beh --> brain
# divide FA and MD 

## for syndromes

out2 <- read.csv(paste0(indata_abcd, "meta_syndromes_results_rounded.csv"))
out2$model <- out2$X

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
                                                                          gsub("_scaled", "",
                                                                               tab$model)))))))))))))


tab                                  







FA <- grep("fa", tab$model, value = T)
BrtoBeh <- grep("md_t1|fa_t1", tab$model, value = T)

FA_BrtoBeh <- tab[tab$X %in% FA & tab$X %in% BrtoBeh, ]
FA_BehtoBr <- tab[tab$X %in% FA & !tab$X %in% BrtoBeh, ]

MD_BrtoBeh <- tab[!tab$X %in% FA & tab$X %in% BrtoBeh, ]
MD_BehtoBr <- tab[!tab$X %in% FA & !tab$X %in% BrtoBeh, ]


## put it all together in the order you want 

tab <- rbind(FA_BrtoBeh, FA_BehtoBr, MD_BrtoBeh, MD_BehtoBr)

tab


## clean up 

rownames(tab) <- NULL
rownames(tab) <- tab$model2
tab$model <- NULL
tab$model2 <- NULL
tab$X <- NULL

write.csv(tab, paste0(indata_abcd, "exploratory_syndromes_meta.csv"))


## for total problems

out_tot <- read.csv(paste0(indata_abcd, "meta_totproblems_results_rounded.csv"))

out_tot$model <- out_tot$X

out_tot$model2 <- gsub("tot", "Total Problems", 
                       gsub("fa", "FA", 
                            gsub("md", "MD", 
                                 gsub("_t2", " T2", 
                                      gsub("_t1", " T1",  
                                           out_tot$model)))))


out_tot                                  

rownames(out_tot) <- NULL
rownames(out_tot) <- out_tot$model2
out_tot$model <- NULL
out_tot$model2 <- NULL
out_tot$X <- NULL

write.csv(out_tot, paste0(indata_abcd, "exploratory_totproblems_meta.csv"))




## for tracts 

all_tracts2$model2 <- NA

all_tracts2$model2 <- gsub("fmi", "Forceps Minor", 
                           gsub("fma", "Forceps Major", 
                                gsub("slf", "Superior Longitudinal Fasciculus",
                                     gsub("ilf", "Inferior Longitudinal Fasciculus",
                                          gsub("ifo", "Inferior-fronto-occipital Fasciculus",
                                               gsub("unc", "Uncinate",
                                                    gsub("atr", "Anterior Thalamic Radiation", 
                                                         gsub("cst", "Corticospinal Tract",
                                                              gsub("cgh", "Parahippocampal Cingulum", 
                                                                   gsub("cgc", "Cingulate Gyrus", 
                                                                        gsub("t2", "T2", 
                                                                             gsub("t1", "T1",
                                                                                  gsub("fa", "FA", 
                                                                                       gsub("md", "MD",                                                                                                                                                 gsub("_", " ",
                                                                                                                                                                                                                                                             gsub("int", "Internalising", 
                                                                                                                                                                                                                                                                  gsub("ext", "Externalising",
                                                                                                                                                                                                                                                                       rownames(all_tracts2))))))))))))))))))


all_tracts2$model <- rownames(all_tracts2)


FA <- grep("fa", all_tracts2$model, value = T)
BrtoBeh <- grep("md_t1|fa_t1", all_tracts2$model, value = T)

FA_BrtoBeh <- all_tracts2[all_tracts2$model %in% FA & all_tracts2$model %in% BrtoBeh, ]
FA_BehtoBr <- all_tracts2[all_tracts2$model %in% FA & !all_tracts2$model %in% BrtoBeh, ]

MD_BrtoBeh <- all_tracts2[!all_tracts2$model %in% FA & all_tracts2$model %in% BrtoBeh, ]
MD_BehtoBr <- all_tracts2[!all_tracts2$model %in% FA & !all_tracts2$model %in% BrtoBeh, ]


## put it all together in the order you want 

all_tracts2 <- rbind(FA_BrtoBeh, FA_BehtoBr, MD_BrtoBeh, MD_BehtoBr)

all_tracts2


## clean up 

rownames(all_tracts2) <- NULL
rownames(all_tracts2) <- all_tracts2$model2
all_tracts2$model <- NULL
all_tracts2$model2 <- NULL
all_tracts2$X <- NULL

write.csv(all_tracts2, paste0(indata_abcd, "exploratory_tracts_meta.csv"))


all_tracts2[all_tracts2$p.meta < 0.05, ]
all_syn[all_syn$p.meta < 0.05, ]
all_tot[all_tot$p.meta < 0.05, ]
all_tracts[all_tracts$p.meta < 0.05, ]
