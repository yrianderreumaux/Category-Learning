###This is the main file to reproduce the statistical analyses 
###for the manuscript entitled stereotypes disrupt probabilistic category learning

###########Remember to set the current directory as working directory before running this code##########

########Read in packages used for the analyses#########
source('scripts/behavioral_analyses/Packages.R')

#helper functions
source('scripts/behavioral_analyses/helper_functions.R')

#######Script for Exclusion Criterion
source('scripts/behavioral_analyses/ParticipantExclusion.R')

#######Script for Individual Differences
source('scripts/behavioral_analyses/IndividualDifferences.R')

######Analyses for Study 1
source('scripts/behavioral_analyses/Study1.Analyses.R')

#######Analyses for Study 2
source('scripts/behavioral_analyses/Study2.Analyses.R')

#######Analyses for Study 3
source('scripts/behavioral_analyses/Study3.Analyses.R')

#######Analyses for Meta-Analysis 1. Warning, this script includes multiple models and CI estimates that take some time to run. 
source('scripts/behavioral_analyses/Meta-1.R')

#######Script for Power Analyses
source('scripts/powersim.R')




