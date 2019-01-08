################################################################################
#                       Check different value of k                             #
#                                                                              #
#                                                                              #
#                           By Eli Strauss                                     #
#                                                                              #
#                           November 2018                                      #
################################################################################

rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)
library(DynaRankR)
library(lme4)
setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/1.2.simulation_workflow_0.9_many_changes/')
source('../0.define_functions.R')
load('2.params_for_simulation.RData')
load('4.simulated_hierarchies.RData')
load('6.inferred.sim.hierarchies.Rdata')

###Store the orders produced by each method
# orders <- lapply(orders <- vector(mode = "list", iterations), function(x)
#   x <- lapply(orders <- vector(mode = "list", length(percent_unknowns)), function(x) 
#     x <- list(Real = list(), DynaRank = list(), Elo_Informed = list(), Elo_Uninformed = list(), DS = list(), PERC = list())))
percent_unknowns <- c(low = 0.2, medium = 0.5, high = 0.8)
# orders <- array(data = list(),
#                 dim = c(iterations, num_periods, 3, 6),
#                 dimnames = list(NULL, 
#                                 NULL,
#                                 names(percent_unknowns),
#                                 c('real', 'dynarank', 'informed_elo', 'elo', 'ds', 'isi')))



#dynamic_data <- list()
counter <- 1
# dynarank_output <- array(list(), dim = c(iterations, length(percent_unknowns),
#                                          7),
#                   dimnames <- list(NULL, names(percent_unknowns),
#                                    c('real','informed_matreorder','informed_elo','elo', 'informed_ds', 'ds', 'isi')))
###Run dynarank and elo##
for(pu in names(percent_unknowns)){
  for(i in 1:iterations){
    cat(paste0('\n\nPERCENT UNKNOWNS: ', pu, '\n....ITERATION '), i, '\n')
    
    initial.ranks <- LETTERS[initial_orders_for_inference[i][[1]]]
    intx.edges <- interactions.dynarank[i,pu][[1]]
    names(intx.edges) <- c('winner','loser','period')
    # dynarank_output[i,pu,'informed_matreorder'] <- list(informed_matreorder(contestants = contestants[[i]],
    #                            convention = 'phys_attr',
    #                            n=100, shuffles = 10,
    #                            require.corroboration = TRUE,
    #                            initial.ranks = initial.ranks,
    #                            interactions = intx.edges))
    
    dynarank_output[i,pu,'elo'] <- list(informed_elo(contestants = contestants[[i]],
                                                convention = 'none',
                                                K = 200, lambda = 100,
                                                initial.ranks = initial.ranks,
                                                interactions = intx.edges))
    
    dynarank_output[i,pu,'informed_elo'] <- list(informed_elo(contestants = contestants[[i]],
                                                     convention = 'phys_attr',
                                                     K = 200, lambda = 100,
                                                     initial.ranks = initial.ranks,
                                                     interactions = intx.edges))
    
    # dynarank_output[i,pu,'informed_ds'] <- list(informed_ds(contestants = contestants[[i]],
    #                                                 convention = 'phys_attr',
    #                                                 initial.ranks = initial.ranks,
    #                                                 interactions = intx.edges))
    # 
    # 
    # 
    # dynarank_output[i,pu,'ds'] <- list(informed_ds(contestants = contestants[[i]],
    #                                                         convention = 'none',
    #                                                         initial.ranks = initial.ranks,
    #                                                         interactions = intx.edges))
    # 
    # ##ISI From EloRating
    # isi_ranks <- contestants[[i]]
    # isi_ranks$rank <- NA
    # isi_ranks$id <- NA
    # isi_ranks <-dplyr::select(isi_ranks, period, id, rank)
    # 
    # for(current.period in unique(isi_ranks$period)){
    #   ids <- filter(contestants[[i]], period == current.period)$id
    #   intx.mat <- filter(intx.edges, period == current.period,
    #                      winner %in% ids, loser %in% ids) %>%
    #     dplyr::select(winner, loser) %>% edgelist_to_matrix(identities = ids)
    #   isi.mat <- EloRating::ISI(intx.mat, 10000,printmessages = FALSE)
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
    # dynarank_output[i,pu,'isi'] <- list(isi_ranks)
    
    
    # dynarank_output[i,pu,'real'] <- dynarank_output[i,pu,'elo']
    # dynarank_output[i,pu,'real'][[1]]$id <- unlist(true_orders[i,])
    # dynarank_output[i,pu,'real'][[1]]$old.order <- NULL
    
  }
}

#######7.process_data_simulated_hierarchies
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

####9.mdel_methods_simulations
set.seed(1989)

model_sim_dat$method <- factor(model_sim_dat$method,
                               levels = c('real',
                                          'informed_matreorder','isi',
                                          'informed_elo','elo',
                                          'informed_ds','ds'),
                               labels = c('Real', 
                                          'Informed MatReorder','I&SI',
                                          'Informed Elo','Elo',
                                          'Informed David\'s Score', 'David\'s Score'))
model_changes$method <- factor(model_changes$method,
                               levels = c('real',
                                          'informed_matreorder','isi',
                                          'informed_elo','elo',
                                          'informed_ds','ds'),
                               labels = c('Real', 
                                          'Informed MatReorder','I&SI',
                                          'Informed Elo','Elo',
                                          'Informed David\'s Score', 'David\'s Score'))



model_sim_dat$method <- relevel(model_sim_dat$method, ref = 'Real')
model_sim_dat$period <- as.factor(model_sim_dat$current.period)
model_sim_dat$iteration <- as.factor(model_sim_dat$iteration)

model_changes$iteration <- as.factor(model_changes$iteration)
model_changes$method <- relevel(model_changes$method, ref = 'Real')
model_changes$percent_unknowns <- factor(model_changes$percent_unknowns,
                                         levels = c('low', 'medium', 'high'))


##GLMM modeling hierarchy dynamics
mm.delta <- glmer(data=model_changes,
                  formula = delta ~ method + as.numeric(percent_unknowns) +
                    (1|iteration:id), family = 'poisson',
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5000))) #Changed optimizer to help with convergence issues

mm.delta.passive <- glmer(data=model_changes,
                          formula = delta.passive ~ method + as.numeric(percent_unknowns) +
                            (1|iteration:id), family = 'poisson',
                          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5000))) #Changed optimizer to help with convergence issues

mm.delta.active <- glmer(data=model_changes,
                         formula = delta.active ~ method + as.numeric(percent_unknowns) +
                           (1|iteration:id), family = 'poisson',
                         control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5000))) #Changed optimizer to help with convergence issues

##GLMM modeling proportions of dyadic relationships that are correct for each period
mm.correct <- glmer(data= filter(model_sim_dat, method != 'Real', period != 20), 
                    formula = cbind(right, wrong) ~ method + as.numeric(percent_unknowns) + 
                      (1|iteration:period), family = 'binomial',
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5000)))



####Plotting#####

model_sim_dat$period <- as.numeric(as.character(model_sim_dat$current.period))
model_sim_dat$iteration <- as.numeric(as.character(model_sim_dat$iteration))
model_sim_dat$percent_unknowns <- factor(x = model_sim_dat$percent_unknowns,
                                         levels = c(0.2, 0.5, 0.8),
                                         labels = c('low', 'medium', 'high'))

##Figures 1 and 2 are schematic figures made in powerpoint



cols <- viridis(4)
light.cols = col2rgb(cols, FALSE)
light.cols <- colorspace::mixcolor(color1 = colorspace::RGB(R = t(light.cols/255)), color2 = colorspace::RGB(1,1,1), alpha = 0.5)@coords
combined.cols <- c(cols[1], rgb(light.cols[1,1], light.cols[1,2], light.cols[1,3]),
                   cols[2], rgb(light.cols[2,1], light.cols[2,2], light.cols[2,3]),
                   cols[3], rgb(light.cols[3,1], light.cols[3,2], light.cols[3,3]))

##Figure 3&4 
accuracy <- ggplot(data = filter(model_sim_dat, method != 'Real', period < 20, iteration <= iterations), aes(y = dyadic_similarity, x = percent_unknowns, fill = method)) + 
  geom_violin(aes(col = method))+
  theme_classic() +
  geom_vline(xintercept = 1.5, lty = 3)+
  geom_vline(xintercept = 2.5, lty = 3)+
  ylim(0.4, 1)+
  ylab('Proportion of dyads correct') + 
  xlab('% unknowns')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = combined.cols)+
  theme(legend.position = 'none',
        plot.margin = unit(c(0,0,0,0),'pt'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))+
  ggtitle(label = NULL, subtitle = 'a)')

dw.correct <- dw.data.compiler(mm.correct)

dw.correct$estimate <- factor(dw.correct$estimate, 
                              levels = c('(Intercept)',
                                         'methodInformed MatReorder', 'methodI&SI',
                                         'methodInformed Elo', 'methodElo',
                                         'methodInformed David\'s Score','methodDavid\'s Score',
                                         'as.numeric(percent_unknowns)'),
                              labels = c('Intercept', 
                                         'Informed MatReorder','I&SI',
                                         'Informed Elo','Elo',
                                         'Informed David\'s Score', 'David\'s Score',
                                         '% unknowns'))

correct.stats <- ggplot(filter(dw.correct, estimate != 'Intercept'), aes(x = ci, y = estimate))+
  geom_point(aes(x = u, y = estimate, fill = estimate, col = estimate), shape = 23, size = 2)+
  geom_line(size = 1, aes(col = estimate))+
  theme_classic()+
  xlab('Parameter estimate')+
  ylab('')+
  scale_y_discrete(limits = rev(levels(dw.correct$estimate)[-1:-2]))+
  geom_vline(xintercept = 0, lty = 2, size = 1)+
  scale_fill_manual(values = c('white', cols[2], 'white', cols[3], 'white', 'grey'))+
  scale_color_manual(values = c(combined.cols[-1], 'grey'))+
  theme(legend.position = 'none',
        plot.margin = unit(c(0,5.5,0,0), 'pt'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))+
  ggtitle(label = NULL, subtitle = 'b)')


aoc_active <- ggplot(data = model_changes, aes(y = delta.active, x = percent_unknowns, fill = method)) + 
  geom_violin(aes(col = method), trim = TRUE)+
  theme_classic() +
  ylab('Total ∆a per individual') +
  geom_vline(xintercept = 1.5, lty = 3)+
  geom_vline(xintercept = 2.5, lty = 3)+
  xlab('% unknowns')+
  scale_fill_manual(values = c(cols[4], cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = c(cols[4], combined.cols))+
  theme(legend.position = 'none',
        plot.margin = unit(c(0,0,0,0),'pt'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))+ 
  ggtitle(label = NULL, subtitle = 'c)')

reorder.for.legend <- model_changes
reorder.for.legend$method <- factor(reorder.for.legend$method,
                                    levels = c('Informed MatReorder', 
                                               'I&SI', 'Informed Elo',
                                               'Elo', 'Informed David\'s Score',
                                               'David\'s Score', 'Real'),
                                    labels = c('Informed MatReorder  ', 
                                               'I&SI', 'Informed Elo  ',
                                               'Elo', 'Informed David\'s Score  ',
                                               'David\'s Score', 'Real'))


plot.for.legend <- ggplot(data = reorder.for.legend, aes(y = delta.active, x = percent_unknowns, fill = method)) + 
  geom_violin(aes(col = method), trim = TRUE)+
  theme_classic() +
  ylab('Active dynamics') +
  geom_vline(xintercept = 1.5, lty = 3)+
  geom_vline(xintercept = 2.5, lty = 3)+
  xlab('% unknowns')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white',cols[4]))+
  scale_color_manual(values = c(combined.cols, cols[4]))+
  theme(legend.key.size = unit(0.025, units = 'npc'), legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), 'pt'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))

legend.plot <- cowplot::ggdraw(cowplot::get_legend(plot.for.legend))

dw.changes <- dw.data.compiler(mm.delta.active)

dw.changes$estimate <- factor(dw.changes$estimate, 
                              levels = c('(Intercept)',
                                         'methodInformed MatReorder', 'methodI&SI',
                                         'methodInformed Elo', 'methodElo',
                                         'methodInformed David\'s Score','methodDavid\'s Score',
                                         'as.numeric(percent_unknowns)'),
                              labels = c('Intercept', 
                                         'Informed MatReorder','I&SI',
                                         'Informed Elo','Elo',
                                         'Informed David\'s Score', 'David\'s Score',
                                         '% unknowns'))

change.stats <- ggplot(filter(dw.changes, estimate != 'Intercept'), aes(x = ci, y = estimate))+
  geom_point(aes(x = u, y = estimate, fill = estimate, col = estimate), shape = 23, size = 2)+
  geom_line(size = 1, aes(col = estimate))+
  theme_classic()+
  xlab('Parameter estimate')+
  ylab('')+
  scale_y_discrete(limits = rev(levels(dw.changes$estimate)[-1]))+
  geom_vline(xintercept = 0, lty = 2, size = 1)+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white', 'grey'))+
  scale_color_manual(values = c(combined.cols, 'grey'))+
  theme(legend.position = 'none',
        plot.margin = unit(c(0,5.5,0,0), 'pt'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))+
  ggtitle(label = NULL, subtitle = 'd)')


##Figures 3-4 - example
example <- dynarank_output[1,1,'real'][[1]]

inset <- ggplot(data = example, aes(x = period, y = rank))+
  ylim(25, 0)+
  theme_classic()+
  geom_line(aes(y = rank, x = period, group = id))+
  ylab('Rank') + 
  xlab('Period')+
  ggtitle(label = 'Example of true hierarchy')+
  theme(plot.margin = unit(c(0,5.5,0,0), 'pt'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9))+
  ggtitle('e)')


cairo_pdf(file = paste0('../plots/21.Fig34_', 
                        num_changes, '_',
                        trait_veracity, 'K200.pdf'),
          6, 6)
multiplot(accuracy, correct.stats,
          legend.plot,inset,
          aoc_active, change.stats,
          layout = matrix(c(1,1,1,2,2,2,
                            1,1,1,2,2,2,
                            1,1,1,2,2,2,
                            5,5,5,6,6,6,
                            5,5,5,6,6,6,
                            5,5,5,6,6,6,
                            3,3,3,4,4,4,
                            3,3,3,4,4,4), byrow= TRUE,
                          ncol = 6))
dev.off()

