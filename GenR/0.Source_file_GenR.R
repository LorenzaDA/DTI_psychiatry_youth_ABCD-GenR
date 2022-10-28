######################
# 0. Source file
######################

#####
# Set environment
#####

rm(list=ls())

x <- c("foreign","dplyr", "tidyr", "mice", "VIM", "lme4", "miceadds", "stringi",
       "data.table", "ggplot2", "cowplot", "ggpubr", "beanplot", "tidyverse",
       "naniar", "gtsummary", "flextable", "RColorBrewer", "sjPlot", 
       "lavaan", "fastDummies", "semTable", "olsrr", "tableone", "lsr", "lattice",
       "pan", "effects", "car", "rlist", "methods", "corrplot", "magick", 
       "gridExtra", "broom.mixed") # specify packages you want to load


lapply(x, require, character.only = T)  # load packages


# set paths 

indata <- "PUT_PATH_WHERE_YOU_KEEP_THE_DATA"

setwd(indata) 

res <- "PUT_PATH_WHERE_YOU_SAVE_RESULTS"
