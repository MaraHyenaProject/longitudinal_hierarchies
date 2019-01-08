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
library(lme4)
library(ggplot2)
library(viridis)
library(DynaRankR)
source('../0.define_functions.R')
load('2.params_for_simulation.RData')
load('6.inferred.sim.hierarchies.Rdata')
load('8.simulated_hierarchy_summary.RData')
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

summary(glht(mm.delta.active, mcp(method = 'Tukey')))

##GLMM modeling proportions of dyadic relationships that are correct for each period
mm.correct <- glmer(data= filter(model_sim_dat, method != 'Real', period != 20), 
                    formula = cbind(right, wrong) ~ method + as.numeric(percent_unknowns) + 
                       (1|iteration:period), family = 'binomial',
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=5000)))

summary(glht(mm.correct, mcp(method = 'Tukey')))


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


cairo_pdf(file = paste0('../plots/Sim9.Fig34_', 
                                    num_changes, '_',
                                    trait_veracity, '.pdf'),
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



