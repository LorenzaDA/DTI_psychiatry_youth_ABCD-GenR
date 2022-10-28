############################
# 0a. Source file - general 
############################

#####
# Set environment
#####

rm(list = ls())

x <- c("lavaan", "fastDummies", "dplyr", "lavaanPlot", "data.table", 
       "ggplot2", "cowplot", "ggpubr", "beanplot", "grid",
       "tidyverse", "naniar", "gtsummary", "flextable", "RColorBrewer", 
       "rlist", "methods", "gtsummary", "methods", "lme4", 
       "lattice", "pan", "tidyr", "sjPlot", "effects", "car", "meta",
       "gridExtra", "raincloudplots", "devtools", "corrplot", "psych",
       "grDevices", "ggplotify", "magick", "lattice", "gridExtra", "tidyr", "reshape2")


lapply(x, require, character.only = T)

indata <- 'PUT_PATH_WHERE_YOU_KEEP_THE_DATA'

res <- "PUT_PATH_WHERE_YOU_SAVE_RESULTS"

setwd("PUT_WORKING_DIRECTORY_PATH")
