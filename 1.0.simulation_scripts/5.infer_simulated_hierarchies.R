################################################################################
#                 Infer hierarchies with different methods                     #
#                                                                              #
#                                                                              #
#                           By Eli Strauss                                     #
#                                                                              #
#                           November 2018                                      #
################################################################################

rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)
library(aniDom)
library(DynaRankR)
source('../0.define_functions.R')
load('2.params_for_simulation.RData')
load('4.simulated_hierarchies.RData')


percent_unknowns <- c(low = 0.2, medium = 0.5, high = 0.8)
orders <- array(data = list(),
                dim = c(iterations, num_periods, 3, 6),
                dimnames = list(NULL, 
                                NULL,
                                names(percent_unknowns),
                                c('real', 'dynarank', 'informed_elo', 'elo', 'ds', 'isi')))



counter <- 1
dynarank_output <- array(list(), dim = c(iterations, length(percent_unknowns),
                                         7),
                  dimnames <- list(NULL, names(percent_unknowns),
                                   c('real','informed_matreorder','informed_elo','elo', 'informed_ds', 'ds', 'isi')))
###Run dynarank and elo##
for(pu in names(percent_unknowns)){
  for(i in 1:iterations){
    cat(paste0('\n\nPERCENT UNKNOWNS: ', pu, '\n....ITERATION '), i, '\n')
    
    initial.ranks <- LETTERS[initial_orders_for_inference[i][[1]]]
    intx.edges <- interactions.dynarank[i,pu][[1]]
    names(intx.edges) <- c('winner','loser','period')
    dynarank_output[i,pu,'informed_matreorder'] <- list(informed_matreorder(contestants = contestants[[i]],
                               convention = 'phys_attr',
                               n=100, shuffles = 10,
                               require.corroboration = TRUE,
                               initial.ranks = initial.ranks,
                               interactions = intx.edges))
    
    dynarank_output[i,pu,'elo'] <- list(informed_elo(contestants = contestants[[i]],
                                                convention = 'none',
                                                K = 100, lambda = 100,
                                                initial.ranks = initial.ranks,
                                                interactions = intx.edges))
    
    dynarank_output[i,pu,'informed_elo'] <- list(informed_elo(contestants = contestants[[i]],
                                                     convention = 'phys_attr',
                                                     K = 100, lambda = 100,
                                                     initial.ranks = initial.ranks,
                                                     interactions = intx.edges))
    
    dynarank_output[i,pu,'informed_ds'] <- list(informed_ds(contestants = contestants[[i]],
                                                    convention = 'phys_attr',
                                                    initial.ranks = initial.ranks,
                                                    interactions = intx.edges))
    
    
    
    dynarank_output[i,pu,'ds'] <- list(informed_ds(contestants = contestants[[i]],
                                                            convention = 'none',
                                                            initial.ranks = initial.ranks,
                                                            interactions = intx.edges))
    
    ##ISI From EloRating
    isi_ranks <- contestants[[i]]
    isi_ranks$rank <- NA
    isi_ranks$id <- NA
    isi_ranks <-dplyr::select(isi_ranks, period, id, rank)
    
    for(current.period in unique(isi_ranks$period)){
      ids <- filter(contestants[[i]], period == current.period)$id
      intx.mat <- filter(intx.edges, period == current.period,
                         winner %in% ids, loser %in% ids) %>%
        dplyr::select(winner, loser) %>% edgelist_to_matrix(identities = ids)
      isi.mat <- EloRating::ISI(intx.mat, 10000,printmessages = FALSE)
      isi_ranks[isi_ranks$period == current.period,]$id <- rownames(isi.mat[[1]])
      isi_ranks[isi_ranks$period == current.period,]$rank <- 1:length(ids)
    }
    
    isi_ranks$old.order <- NA
    
    isi_ranks <- isi_ranks %>% 
      group_by(period) %>% 
      mutate(stan.rank = -2*(rank-1)/(max(rank)-1) + 1) %>% 
      dplyr::select(period, id, rank, stan.rank, old.order) %>% 
      as.data.frame()
    
    dynarank_output[i,pu,'isi'] <- list(isi_ranks)
    
    
    dynarank_output[i,pu,'real'] <- dynarank_output[i,pu,'elo']
    dynarank_output[i,pu,'real'][[1]]$id <- unlist(true_orders[i,])
    dynarank_output[i,pu,'real'][[1]]$old.order <- NULL
    
  }
}

save(dynarank_output, file = '6.inferred.sim.hierarchies.Rdata')

