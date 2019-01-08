################################################################################
#                        Descriptives and metadata                             #
#                                                                              #
#                                                                              #
#                            By Eli Strauss                                    #
#                                                                              #
#                            November 2018                                     #
################################################################################


rm(list = ls())
options(stringsAsFactors = FALSE)

library(DynaRankR)
library(dplyr)

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
source('0.define_functions.R')
load('3.hyena_data.RData')
load('5.female_ranks.RData')
load('7.male_ranks.RData')


##Male descriptives
mtdt.m <- data.frame(period = rep(NA, length(unique(male.ranks$period))),
                     intx_per_indiv = NA,
                     p_unk = NA)

for(current.period in unique(male.ranks$period)){
  ids <- unique(filter(male.ranks, period == current.period)$id)
  intx.matrix <- filter(male.interactions, winner %in% ids, loser %in% ids, period == current.period) %>%
    dplyr::select(winner, loser) %>%
    edgelist_to_matrix(identities = ids)
  mtdt.m[which(unique(male.ranks$period) == current.period),]$period <- current.period
  mtdt.m[which(unique(male.ranks$period) == current.period),]$p_unk <- 
    length(which((intx.matrix + t(intx.matrix))[upper.tri(intx.matrix)] == 0))/(sum(upper.tri(intx.matrix)))
  mtdt.m[which(unique(male.ranks$period) == current.period),]$intx_per_indiv <- 
    sum(intx.matrix)/length(ids)
}

##Female descriptives
mtdt.f <- data.frame(period = rep(NA, length(unique(female.ranks$period))),
                     intx_per_indiv = NA,
                     p_unk = NA)

for(current.period in unique(female.ranks$period)){
  ids <- unique(filter(female.ranks, period == current.period)$id)
  intx.matrix <- filter(female.interactions, winner %in% ids, loser %in% ids, period == current.period) %>%
    dplyr::select(winner, loser) %>%
    edgelist_to_matrix(identities = ids)
  mtdt.f[which(unique(female.ranks$period) == current.period),]$period <- current.period
  mtdt.f[which(unique(female.ranks$period) == current.period),]$p_unk <- 
    length(which((intx.matrix + t(intx.matrix))[upper.tri(intx.matrix)] == 0))/(sum(upper.tri(intx.matrix)))
  mtdt.f[which(unique(female.ranks$period) == current.period),]$intx_per_indiv <- 
    sum(intx.matrix)/length(ids)
}

############Descriptive metadata############
mean(mtdt.m$intx_per_indiv)
min(mtdt.m$intx_per_indiv)
max(mtdt.m$intx_per_indiv)

mean(mtdt.m$p_unk)
min(mtdt.m$p_unk)
max(mtdt.m$p_unk)

mean(mtdt.f$intx_per_indiv)
min(mtdt.f$intx_per_indiv)
max(mtdt.f$intx_per_indiv)

mean(mtdt.f$p_unk)
min(mtdt.f$p_unk)
max(mtdt.f$p_unk)

##Calculate individuals lost and recruited per period

#Females
fdemography <- data.frame()
for(y in unique(female.ranks$period)){
  if(nrow(filter(female.ranks, period == y-1))){
    
    gain <- length(unique(filter(female.ranks, period == y,
                                 !id %in% filter(female.ranks, period == y-1)$id)$id))
    loss <-length(unique(filter(female.ranks, period == y-1,
                                !id %in% filter(female.ranks, period == y)$id)$id))
    
    fdemography <- rbind(fdemography,
                         data.frame(period = y, gain, loss))
  }
}

summary(fdemography)

#Males
mdemography <- data.frame()
for(y in unique(male.ranks$period)){
  if(nrow(filter(male.ranks, period == y-1))){
    
    gain <- length(unique(filter(male.ranks, period == y,
                                 !id %in% filter(male.ranks, period == y-1)$id)$id))
    loss <-length(unique(filter(male.ranks, period == y-1,
                                !id %in% filter(male.ranks, period == y)$id)$id))
    
    mdemography <- rbind(mdemography,
                         data.frame(period = y, gain, loss))
  }
}

summary(mdemography)


##average group size
female.ranks %>% group_by(period) %>% summarize(nids = length(unique(id))) -> rsum
summary(rsum$nids)

male.ranks %>% group_by(period) %>% summarize(nids = length(unique(id))) -> rsum
summary(rsum$nids)
