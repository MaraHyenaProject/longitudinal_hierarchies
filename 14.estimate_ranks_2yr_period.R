################################################################################
#                Get ranks from empirical dataset - females                    #
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
library(EloRating)


setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')

load('3.hyena_data.RData')


###Change periods to every two years
female.contestants$period <- 2*floor(female.contestants$period/2)
female.contestants <- unique(female.contestants)
female.interactions$period <- 2*floor(female.interactions$period/2)

female.initial.ranks <- DynaRankR:::add_new_ids_mri(new.ids = filter(female.contestants, period == 1988,
                                             !id %in% female.initial.ranks)$id,
                            contestants = female.contestants, working.ranks = female.initial.ranks,
                            periods = unique(female.contestants$period), 
                            period = 1988)


set.seed(1989)

female_ranks <- informed_matreorder(contestants = female.contestants,
                         convention = 'mri',
                         n = 100,
                         shuffles = 10,
                         require.corroboration = TRUE,
                         initial.ranks = female.initial.ranks,
                         interactions = female.interactions)
plot_ranks(female_ranks)

female_ranks$method = 'informed_matreorder'

ds_ranks <- informed_ds(female.contestants, convention = 'none',
                        initial.ranks = female.initial.ranks, 
                        interactions = female.interactions)

ds_ranks$method <- 'ds'

informed_ds_ranks <- informed_ds(female.contestants, convention = 'mri',
                                 initial.ranks = female.initial.ranks, 
                                 interactions = female.interactions)
informed_ds_ranks$method <- 'informed_ds'

elo_ranks <- informed_elo(female.contestants, convention = 'none',
                          K = 200, lambda = 100,
                          interactions = female.interactions)
elo_ranks$method <- 'elo'

informed_elo_ranks <- informed_elo(contestants = female.contestants, convention = 'mri',
                                   K = 200, lambda = 100,
                                   initial.ranks = female.initial.ranks, 
                                   interactions = female.interactions)
informed_elo_ranks$method <- 'informed_elo'


# ##ISI From EloRating
# isi_ranks <- female.contestants
# isi_ranks$rank <- NA
# isi_ranks$id <- NA
# isi_ranks <-dplyr::select(isi_ranks, period, id, rank)
# 
# for(current.period in unique(female.contestants$period)){
#   ids <- filter(female.contestants, period == current.period)$id
#   intx.mat <- filter(female.interactions, period == current.period,
#                      winner %in% ids, loser %in% ids) %>%
#     dplyr::select(winner, loser) %>% edgelist_to_matrix(identities = ids)
#   isi.mat <- EloRating::ISI(intx.mat, 10000)
#   isi_ranks[isi_ranks$period == current.period,]$id <- rownames(isi.mat[[1]])
#   isi_ranks[isi_ranks$period == current.period,]$rank <- 1:length(ids)
# }
# 
# isi_ranks$old.order <- NA
# 
# isi_ranks <- isi_ranks %>% 
#   group_by(period) %>% 
#   mutate(stan.rank = -2*(rank-1)/(max(rank)-1) + 1) %>% 
#   dplyr::select(period, id, rank, stan.rank, old.order) %>% 
#   as.data.frame()
# 
# isi_ranks$method <- 'isi'

female.ranks.2yr <- rbind(female_ranks, 
                            ds_ranks[,names(female_ranks)],
                            informed_ds_ranks[,names(female_ranks)],
                            elo_ranks[,names(female_ranks)],
                            informed_elo_ranks[,names(female_ranks)])

save(female.ranks.2yr, file = '15.female_ranks_2yr.RData')
