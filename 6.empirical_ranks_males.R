################################################################################
#                Get ranks from empirical dataset - males                      #
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

load('3.hyena_data.RData')

set.seed(1989)

male_ranks <- informed_matreorder(contestants = male.contestants,
                         convention = 'tenure',
                         n = 100,
                         shuffles = 10,
                         require.corroboration = TRUE,
                         initial.ranks = male.initial.ranks,
                         interactions = male.interactions)

plot_ranks(male_ranks)

male_ranks$method = 'informed_matreorder'

ds_ranks <- informed_ds(male.contestants, convention = 'none',
                        interactions = male.interactions)

ds_ranks$method <- 'ds'

informed_ds_ranks <- informed_ds(contestants = male.contestants, convention = 'tenure',
                                 initial.ranks = male.initial.ranks, 
                                 interactions = male.interactions)
informed_ds_ranks$method <- 'informed_ds'

elo_ranks <- informed_elo(male.contestants, convention = 'none',
                          K = 100, lambda = 100,
                          initial.ranks = male.initial.ranks, 
                          interactions = male.interactions)
elo_ranks$method <- 'elo'

informed_elo_ranks <- informed_elo(contestants = male.contestants, convention = 'tenure',
                                   K = 100, lambda = 100,
                                   interactions = male.interactions)
informed_elo_ranks$method <- 'informed_elo'

##ISI From EloRating
isi_ranks <- male.contestants
isi_ranks$rank <- NA
isi_ranks$id <- NA
isi_ranks <-dplyr::select(isi_ranks, period, id, rank)

for(current.period in unique(male.contestants$period)){
  ids <- filter(male.contestants, period == current.period)$id
  intx.mat <- filter(male.interactions, period == current.period,
                     winner %in% ids, loser %in% ids) %>%
    dplyr::select(winner, loser) %>% edgelist_to_matrix(identities = ids)
  isi.mat <- EloRating::ISI(intx.mat, 10000)
  isi_ranks[isi_ranks$period == current.period,]$id <- rownames(isi.mat[[1]])
  isi_ranks[isi_ranks$period == current.period,]$rank <- 1:length(ids)
}

isi_ranks$old.order <- NA

isi_ranks <- isi_ranks %>% 
  group_by(period) %>% 
  mutate(stan.rank = -2*(rank-1)/(max(rank)-1) + 1) %>% 
  dplyr::select(period, id, rank, stan.rank, old.order) %>% 
  as.data.frame()

isi_ranks$method <- 'isi'

male.ranks <- rbind(male_ranks, 
                    ds_ranks[,names(male_ranks)],
                    informed_ds_ranks[,names(male_ranks)],
                    elo_ranks[,names(male_ranks)],
                    informed_elo_ranks[,names(male_ranks)],
                    isi_ranks[,names(male_ranks)])

save(male.ranks, file = '7.male_ranks.RData')
