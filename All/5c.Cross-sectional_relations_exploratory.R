#############
# Checking cross sectional correlations across cohorts 
#############

rm(list=ls())

#####
# set environment 
####

source("/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/Directionality_DTI_Psychiatricproblems/ABCD/0a.Source_file_general_ABCD_4.0.R")

indata_genr <- "/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/output/results/GenR/June2022/"
indata_abcd <- "/Users/lorenzadallaglio/Documents/PhD thesis/directionality project/output/results/June2022/"


# open data

main_abcd <- read.csv(paste0(indata_abcd, "summarystats_main_abcd.csv"), header = T) 
syn_abcd <- read.csv(paste0(indata_abcd, "summarystats_std_syndromescales_ABCD.csv"), header = T)
tracts_abcd <- read.csv(paste0(indata_abcd, "summarystats_std_tracts_ABCD.csv"), header = T)
tot_abcd <- read.csv2(paste0(indata_abcd, "totalproblems_summarystats.csv"), header = T)

main_genr <- read.csv(paste0(indata_genr, "summarystats_main_genr.csv"), header = T)
syn_genr <- read.csv(paste0(indata_genr, "summarystats_std_syndromes_GenR.csv"), header = T)
tracts_genr <- read.csv(paste0(indata_genr, "summarystats_std_tracts.csv"), header = T)
tot_genr <- read.csv2(paste0(indata_genr, "totalproblems_summarystats.csv"), header = T)

######
# Select cross-sectional associations from the summary stats
#####
# that's always line 5 because of how you structure the tables 

all <- list(main_abcd, syn_abcd, tracts_abcd, main_genr, syn_genr, tracts_genr)
all



# get all cols with "pvalue" in the name and vectorise the output by unlisting 

out <- unlist(lapply(all, function(x) grep("pvalue", names(x), value = T)))


# filter the df for the vars in the out vector and for the row 5 (= where CS relations are)

all2 <- lapply(all, function(x) x[5, names(x) %in% out])

all2


# FDR correct

fdr_p <- lapply(all2, function(x) p.adjust(x, method = "fdr"))



# which ones sign. after FDR correction? 

fdr_2 <- lapply(fdr_p, function(x) x[x < 0.05])
fdr_2


all[2]

# sign before FDR correction 

p_2 <- lapply(all2, function(x) x[x < 0.05])
p_2


