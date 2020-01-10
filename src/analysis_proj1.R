######
#Initial work on project 1
#####
# setwd("/home/sorenwh/Nextcloud/semester3/StatProj/afl/02445projects/src")
rm(list = ls())

load(file = "data/armdata.RData")
our_experiment_no <- 4

data_collection <- armdata[[our_experiment_no]] 
summary(data_collection)

