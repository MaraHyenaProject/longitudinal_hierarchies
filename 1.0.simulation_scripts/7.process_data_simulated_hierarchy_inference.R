################################################################################
#              Process data from simulated hierarchy inference                 #
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
load('6.inferred.sim.hierarchies.Rdata')

#####Modeling simulated data#####
model_sim_dat <- list()
method_levels <- names(dynarank_output[1,'low',])
counter <-1

for(i in 1:iterations){
  for(pu in 1:length(percent_unknowns)){
    for(current.period in 1:num_periods){
      
      true_order <- filter(dynarank_output[i,pu,'real'][[1]], period == current.period)$id
      prev_true_order <- filter(dynarank_output[i,pu,'real'][[1]], period == current.period-1)$id
      for(method in method_levels){
        # if(method == 'Real' & period != 1){
        #   real.changes <- sum(!dyadic_similarity_model(orders[[i]][[pu]][[method]][[period]][orders[[i]][[pu]][[method]][[period]] %in% orders[[i]][[pu]][[method]][[period-1]]], orders[[i]][[pu]][[method]][[period-1]]))
        #   next
        # }
        
        inferred_order <- filter(dynarank_output[i,pu,method][[1]], period == current.period)$id
        prev_inferred_order <- filter(dynarank_output[i,pu,method][[1]], period == current.period-1)$id
        
        
        right <- sum(dyadic_similarity_model(inferred_order, true_order))
        wrong <- sum(!dyadic_similarity_model(inferred_order, true_order))
        if(current.period != 1){
          changed <- sum(!dyadic_similarity_model(inferred_order[inferred_order %in% prev_inferred_order], prev_inferred_order[prev_inferred_order %in% inferred_order]))
          static <- sum(dyadic_similarity_model(inferred_order[inferred_order %in% prev_inferred_order], prev_inferred_order[prev_inferred_order %in% inferred_order]))
          real.changes <- sum(!dyadic_similarity_model(true_order[true_order %in% prev_true_order], prev_true_order[prev_true_order %in% true_order]))
          changed.diff <- changed - real.changes
        }else{
          changed <- NA
          static <- NA
          changed.diff <- NA
        }
        
        
        model_sim_dat[[counter]] <- data.frame(iteration = i,
                                               current.period,
                                               percent_unknowns = percent_unknowns[[pu]],
                                               method = method,
                                               right = right,
                                               wrong = wrong,
                                               changed = changed,
                                               static = static,
                                               changed.diff,
                                               identified_changes = ifelse(current.period==1, 0, 1-dyadic_similarity(prev_inferred_order[prev_inferred_order %in% inferred_order], inferred_order[inferred_order %in% prev_inferred_order])),
                                               correlation_with_correct = cor(match(inferred_order, true_order), match(true_order, true_order), method = 'spearman'),
                                               dyadic_similarity = dyadic_similarity(inferred_order, true_order)
                                               )
        counter <- counter+1
      }
    }
  }
}

model_sim_dat <- do.call(rbind, model_sim_dat)

model_changes <- list()
counter <-1
for(i in 1:iterations){
  for(pu in names(percent_unknowns)){
    for(method in method_levels){
      ranks <- get_dynamics(dynarank_output[i,pu,method][[1]], 'rank')
      model_changes[[counter]] <- ranks %>%
        group_by(id) %>%
        filter(period != 20) %>%
        mutate(i, pu, method) %>%
        summarize(iteration = unique(i), percent_unknowns = unique(pu), method = unique(method),
                  delta = sum(abs(delta), na.rm = TRUE),
                  delta.active = sum(abs(delta.active), na.rm = TRUE),
                  delta.passive = sum(abs(delta.passive), na.rm = TRUE))
      
      counter <- counter+1
    }
  }
}

model_changes <- do.call(rbind, model_changes)

save(list = c('model_sim_dat', 'model_changes'), file = '8.simulated_hierarchy_summary.RData')



