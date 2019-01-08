################################################################################
#                       Plots of passive dynamics                              #
#                                                                              #
#                                                                              #
#                           By Eli Strauss                                     #
#                                                                              #
#                           November 2018                                      #
################################################################################

rm(list = ls())
options(stringsAsFactors = F)
library(dplyr)
library(ggplot2)
library(viridis)
library(DynaRankR)

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
source('0.define_functions.R')

cols <- viridis(4)
light.cols = col2rgb(cols, FALSE)
light.cols <- colorspace::mixcolor(color1 = colorspace::RGB(R = t(light.cols/255)), color2 = colorspace::RGB(1,1,1), alpha = 0.5)@coords
combined.cols <- c(cols[1], rgb(light.cols[1,1], light.cols[1,2], light.cols[1,3]),
                   cols[2], rgb(light.cols[2,1], light.cols[2,2], light.cols[2,3]),
                   cols[3], rgb(light.cols[3,1], light.cols[3,2], light.cols[3,3]))


directories <- c('1.1.simulation_workflow_0.9_few_changes/',
                 '1.2.simulation_workflow_0.9_many_changes/',
                 '1.5.simulation_workflow_0.2_few_changes/')

for(d in directories){
  
  setwd(d)
  
  load('2.params_for_simulation.RData')
  load('6.inferred.sim.hierarchies.Rdata')
  load('8.simulated_hierarchy_summary.RData')
  set.seed(1989)
  
  
  
  model_changes$method <- factor(model_changes$method,
                                 levels = c('real',
                                            'informed_matreorder','isi',
                                            'informed_elo','elo',
                                            'informed_ds','ds'),
                                 labels = c('Real', 
                                            'Informed MatReorder','I&SI',
                                            'Informed Elo','Elo',
                                            'Informed David\'s Score', 'David\'s Score'))
  
  
  ###Supplemental figure on ∆p
  
  p <- ggplot(data = model_changes, aes(y = delta.passive, x = percent_unknowns, fill = method)) + 
    geom_violin(aes(col = method), trim = TRUE)+
    theme_classic() +
    ylab('Total ∆p per individual') +
    geom_vline(xintercept = 1.5, lty = 3)+
    geom_vline(xintercept = 2.5, lty = 3)+
    xlab('% unknowns')+
    scale_fill_manual(values = c(cols[4], cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
    scale_color_manual(values = c(cols[4], combined.cols))+
    theme(plot.margin = unit(c(0,0,0,0),'pt'),
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9))
  
  assign(paste0('passive', substr(d, 1,3)),p)
  setwd('../')
}

cairo_pdf(file = 'plots/22.passive_plots_sim.pdf',
          8, 3)
multiplot(passive1.1+theme(legend.position = 'none')+ggtitle(label = NULL, subtitle = 'a) Few changes\nr = 0.9'),
          passive1.2+theme(legend.position = 'none', axis.title.y = element_blank())+ggtitle(label = NULL, subtitle = 'b) Many changes\nr = 0.9'),
          passive1.5 + theme(axis.title.y = element_blank()) + ggtitle(label = NULL, subtitle = 'c) Few changes\nr = 0.2'),layout = matrix(data = c(1,2,3,3),
                                                                       nrow = 1))
dev.off()

load('5.female_ranks.RData')

methods <- unique(female.ranks$method)

female.ranks$delta.active <- NA
female.ranks$delta.passive <- NA
model_changes <- data.frame()
for(current.method in methods){
  female.ranks[female.ranks$method == current.method, 'delta.active'] <- female.ranks %>%
    filter(method == current.method) %>%
    get_dynamics(type = 'rank') %>%
    pull(delta.active)
  
  female.ranks[female.ranks$method == current.method, 'delta.passive'] <- female.ranks %>%
    filter(method == current.method) %>%
    get_dynamics(type = 'rank') %>%
    pull(delta.passive)
  
  changes.by.id <- female.ranks %>%
    filter(method == current.method) %>%
    group_by(id) %>%
    summarise(method = unique(method),
              delta.active = sum(abs(delta.active), na.rm = TRUE),
              delta.passive = sum(abs(delta.passive), na.rm = TRUE)) %>%
    as.data.frame()
  
  model_changes <- rbind(model_changes,
                         changes.by.id)
}

model_changes$method <- factor(model_changes$method,
                               levels = c('real',
                                          'informed_matreorder','isi',
                                          'informed_elo','elo',
                                          'informed_ds','ds'),
                               labels = c('Real', 
                                          'Informed MatReorder','I&SI',
                                          'Informed Elo','Elo',
                                          'Informed David\'s Score', 'David\'s Score'))


female.passive <- ggplot(data = model_changes, aes(y = delta.passive, x = method, fill = method, col = method)) +
  geom_boxplot(notch = TRUE) +
  theme_classic() +
  xlab('')+
  ylim(0,110)+
  ggtitle('')+
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())+
  ylab('Total ∆p per individual')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = combined.cols)+
  ggtitle(label = NULL, subtitle = 'a) Philopatric Females')



load('7.male_ranks.RData')

methods <- unique(male.ranks$method)

male.ranks$delta.active <- NA
male.ranks$delta.passive <- NA
model_changes <- data.frame()
for(current.method in methods){
  male.ranks[male.ranks$method == current.method, 'delta.active'] <- male.ranks %>%
    filter(method == current.method) %>%
    get_dynamics(type = 'rank') %>%
    pull(delta.active)
  
  male.ranks[male.ranks$method == current.method, 'delta.passive'] <- male.ranks %>%
    filter(method == current.method) %>%
    get_dynamics(type = 'rank') %>%
    pull(delta.passive)
  
  changes.by.id <- male.ranks %>%
    filter(method == current.method) %>%
    group_by(id) %>%
    summarise(method = unique(method),
              delta.active = sum(abs(delta.active), na.rm = TRUE),
              delta.passive = sum(abs(delta.passive), na.rm = TRUE)) %>%
    as.data.frame()
  
  model_changes <- rbind(model_changes,
                         changes.by.id)
}

model_changes$method <- factor(model_changes$method,
                               levels = c('real',
                                          'informed_matreorder','isi',
                                          'informed_elo','elo',
                                          'informed_ds','ds'),
                               labels = c('Real', 
                                          'Informed MatReorder','I&SI',
                                          'Informed Elo','Elo',
                                          'Informed David\'s Score', 'David\'s Score'))

male.passive <- ggplot(data = model_changes, aes(y = delta.passive, x = method, fill = method, col = method)) +
  geom_boxplot(notch = TRUE) +
  theme_classic() +
  xlab('')+
  ylim(0,110)+
  ggtitle('')+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        axis.title.y = element_blank())+
  ylab('Total ∆p per individual')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = combined.cols)+
  ggtitle(label = NULL, subtitle = 'b) Immigrant males')


cairo_pdf(file = 'plots/22.passive_plots_empirical.pdf',
          6, 4)
multiplot(female.passive, male.passive,
          layout = matrix(c(1,1,2,2,2),nrow = 1))
dev.off()
