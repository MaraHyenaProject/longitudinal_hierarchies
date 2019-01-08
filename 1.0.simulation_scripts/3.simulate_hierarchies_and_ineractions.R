################################################################################
#                 Simulate hierarchies and interactions                        #
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

###Generate initial orders according to a trait value
###Run with 0.9, 0.7, 0.5, 0.2 for different test conditions
initial_orders <- generate_trait_orders(100, rep(trait_veracity, 100))
initial_orders <- initial_orders$order[which(round(initial_orders$rho, digits = 1) == trait_veracity)]
initial_orders_for_inference <- initial_orders[1:iterations]

####Simulate hierarchies and interactions

## list stores true orders as a list of lists of order vectors. 
## true_orders[[iteration]][[period]] pulls out order for a period
## in a given iteration
true_orders <- matrix(data = list(), nrow = iterations, ncol = num_periods)

## list stores trait values as a list of lists of value vectors. 
## trait_values[[iteration]][[period]] pulls out trait value for a period
## in a given iteration
trait_values <- matrix(data = list(), nrow = iterations, ncol = num_periods)

## list stores interactions as list of list of list of dataframes 
## interactions[[iteration]][[period]][[pu]] dataframe of interactions
## for a given iteration, period, and percent unknowns
interactions <- array(list(), dim = c(iterations, num_periods, 3), dimnames = list(NULL, NULL, names(percent_unknowns)))

##contestants for elo and dynarank
contestants <- array(data = data.frame(), dim = c(iterations))
##interactions for elo and dynarank
interactions.dynarank <- array(data = list(), dim = c(iterations, 3),
                               dimnames = list(NULL, names(percent_unknowns)))


for(i in 1:iterations){
  cat(paste0('\nIteration ', i), '\n')
  
  new_order <- sample(LETTERS[1:20])
  #store trait values for current iteration
  trait_value <- data.frame(id = new_order[initial_orders[[i]]],
                            trait = sort(runif(n= num_ids, 0, 100), 
                                         decreasing = T))
  
  new_id_counter <- 1
  
  ##Generate latent hierarchy and observations
  for(period in 1:num_periods){
    cat(paste0('..', period, '..'))
    if(period != 1){
      changers <- sample(1:length(new_order), size = rpois(n = 1, lambda = changers_lambda))
      if(length(changers)){
        destinations <- changers + ceiling(rexp(n = length(changers), rate = change_distance_rate)) * (rbinom(length(changers),1,.5)*2-1)
        
        if(length(destinations[destinations > length(new_order)])) destinations[destinations > length(new_order)] <- length(new_order)
        if(length(destinations[destinations < 1])) destinations[destinations < 1] <- 1
        
        for(c in 1:length(changers)){
          new_order <- append(new_order[-changers[c]], after = destinations[c]-1, values = new_order[changers[c]])
        }
      }
      num_new_ids <- rbinom(n = 1, size = 1, prob = .20)
      if(num_new_ids > 0){
        for(nn in 1:num_new_ids){
          new_id <- letters[new_id_counter]
          new_id_counter <- new_id_counter +1
          destination = sample(1:length(new_order), 1)
          if(destination == length(new_order)){
            trait_window <- c(0, trait_value$trait[trait_value$id == new_order[length(new_order)]])
          }else{
            trait_window <- sort(c(trait_value$trait[trait_value$id == new_order[destination+1]], trait_value$trait[trait_value$id == new_order[destination]]))
          }
          
          new_trait_value <- runif(1, min = trait_window[1], max = trait_window[2])
          new_order <- append(new_order, after = destination, values = new_id)
          trait_value <- rbind(trait_value, data.frame(id = new_id, trait = new_trait_value))
        }
      }
    }
    
    dead <- rbinom(n = 1, size = 1, prob = .20)
    if(dead != 0){
      dead <- sample(1:length(new_order), size = 1)
      new_order <- new_order[-dead]
    }
    
    true_orders[i,period] <-  list(new_order)
    trait_value <-trait_value[match(new_order, trait_value$id),]
    trait_values[i,period] <- list(trait_value$trait)
    
    contestants[[i]] <- rbind(contestants[[i]],
                              data.frame(id = new_order,
                                         period = period,
                                         convention1 = trait_value$trait))
    
    ##Generate intx based on Sanchez-Tojar et al and aniDom
    intx <- generate_interactions(length(new_order), length(new_order)*20, a = 15, b = 0, id.biased = T, rank.biased = T)
    full_edgelist <- data.frame(winner = new_order[intx$interactions$Winner], loser = new_order[intx$interactions$Loser])
    for(pu in names(percent_unknowns)){
      pu_intx <- pu_from_edgelist(full_edgelist, order = new_order, pu = percent_unknowns[pu])
      interactions[i, period, pu] <- list(edgelist_to_matrix(pu_intx$edges, identities = new_order))
      interactions.dynarank[i,pu] <- list(rbind(interactions.dynarank[i,pu][[1]],
                                                cbind(pu_intx$edges, period)))
    }
  }
}

save(list = c('interactions', 'trait_values', 
              'true_orders', 'initial_orders_for_inference',
              'interactions.dynarank', 'contestants'), 
     file = '4.simulated_hierarchies.RData')
