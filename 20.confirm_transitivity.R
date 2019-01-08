################################################################################
#               Confirm triangle transitivity of hyena data                    #
#                                                                              #
#                                                                              #
#                           By Eli Strauss                                     #
#                                                                              #
#                           November 2018                                      #
################################################################################


rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)
setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
source('0.define_functions.R')
load('3.hyena_data.RData')


tri.female <- c()
for(y in unique(female.contestants$period)){
  mat <- DynaRankR::edgelist_to_matrix(filter(female.interactions, period == y)[,c(1,2)],
                                       identities = filter(female.contestants, period == y)$id)
  tri.female <- c(tri.female, compete::ttri(mat)$ttri)
}
tri.male <- c()
for(y in unique(male.contestants$period)){
  mat <- DynaRankR::edgelist_to_matrix(filter(male.interactions, period == y)[,c(1,2)],
                                       identities = filter(male.contestants, period == y)$id)
  tri.male <- c(tri.female, compete::ttri(mat)$ttri)
}



