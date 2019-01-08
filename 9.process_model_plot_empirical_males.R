################################################################################
#                Process, model, plot empirical data - males                   #
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
library(multcomp)
library(ggplot2)
library(grid)
library(viridis)
library(broom)
library(sjPlot)

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
source('0.define_functions.R')
load('7.male_ranks.RData')


##Compare methods - males

##Read in aggression data
aggression <- read.csv(file = '0.rawdata/tblAggression.csv')
male.aggs <- filter(aggression, aggressor %in% male.ranks$id, recip %in% male.ranks$id)
male.aggs$year <- format(as.Date(male.aggs$date), '%Y')

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

method_eval <- data.frame(Method = rep(NA, 24*length(methods)), Change = rep(NA, 24*length(methods)), Proportion_Consistent = rep(NA, 24*length(methods)), PC_Next_Year = rep(NA, 24*length(methods)))
prop_consistent_model <- data.frame(Method = NULL, Consistent = NULL)
prop_consistent_ny_model <- data.frame(Method = NULL, Consistent = NULL)
dyadic_change_model <- data.frame(Method = NULL, Change = NULL, Year = NULL)
current_row <- 1
periods <- unique(male.ranks$period)
periods <- periods[2:(length(periods)-1)]
for(current.period in periods){
  for(current.method in methods){
    franks_method <- filter(male.ranks, method == current.method)
    ids_this_year <- filter(franks_method, period == current.period)$id
    ids_last_year <- filter(franks_method, period == (current.period-1))$id
    
    method_eval[current_row,]$Method <- current.method
    # method_eval[current_row,]$Change <- 
    #   1-dyadic_similarity(intersect(filter(franks_method, period == current.period-1)$id, 
    #                                 filter(franks_method, period == current.period)$id),
    #                       intersect(filter(franks_method, period == current.period)$id, 
    #                                 filter(franks_method, period == current.period-1)$id))
    
    method_eval[current_row,]$Change <- sum(abs(filter(franks_method, period == current.period)$delta.active), na.rm = TRUE)
    
    # dyadic_change_model <- rbind(dyadic_change_model, data.frame(Method = rep(current.method), Year = current.period, Change = 
    #                                                                    !dyadic_similarity_model(intersect(filter(franks_method, period == current.period-1)$id, 
    #                                                                                                       filter(franks_method, period == current.period)$id),
    #                                                                                             intersect(filter(franks_method, period == current.period)$id, 
    #                                                                                                       filter(franks_method, period == current.period-1)$id)) ))
    
    
    
    rank_diffs <- left_join(filter(male.aggs, year == current.period, aggressor %in% ids_this_year, recip %in% ids_this_year), filter(franks_method, period == current.period), by = c('recip' = 'id'))$rank -
      left_join(filter(male.aggs, year == current.period, aggressor %in% ids_this_year, recip %in% ids_this_year), filter(franks_method, period == current.period), by = c('aggressor' = 'id'))$rank
    
    prop_consistent_model <- rbind(prop_consistent_model, data.frame(Method = rep(current.method), Consistent = rank_diffs > 0, Year = as.character(current.period)))
    method_eval[current_row,]$Proportion_Consistent <- sum(rank_diffs > 0)/length(rank_diffs)
    
    rank_diffs <- left_join(filter(male.aggs, year == current.period, aggressor %in% ids_last_year, recip %in% ids_last_year), filter(franks_method, period == (current.period-1)), by = c('recip' = 'id'))$rank -
      left_join(filter(male.aggs, year == current.period, aggressor %in% ids_last_year, recip %in% ids_last_year), filter(franks_method, period == (current.period-1)), by = c('aggressor' = 'id'))$rank
    
    prop_consistent_ny_model <- rbind(prop_consistent_ny_model, data.frame(Method = rep(current.method), Consistent = rank_diffs > 0, Year = as.character(current.period)))
    method_eval[current_row,]$PC_Next_Year <- sum(rank_diffs >0)/length(rank_diffs)
    current_row <- current_row + 1
  }
}





method_eval$Method <- factor(method_eval$Method, 
                             levels = c('informed_matreorder','isi',
                                        'informed_elo','elo',
                                        'informed_ds','ds'),
                             labels = c('Informed MatReorder','I&SI',
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



prop_consistent_model <- prop_consistent_model %>% group_by(Year, Method) %>% summarize(cons = sum(Consistent), incons = sum(!Consistent))

prop_consistent_model$Method = factor(prop_consistent_model$Method,
                                      levels = c('informed_matreorder','isi',
                                                 'informed_elo','elo',
                                                 'informed_ds','ds'),
                                      labels = c('Informed MatReorder','I&SI',
                                                 'Informed Elo','Elo',
                                                 'Informed David\'s Score', 'David\'s Score'))



glm.out <- glm(data = prop_consistent_model, cbind(cons, incons) ~ Method, family = binomial(logit))
summary(glm.out)
pcomp_table(mod = glht(glm.out, mcp(Method = 'Tukey')), title ='prop_consistent_this_year_male')
summary(glht(glm.out, mcp(Method = 'Tukey')))
#Create significance groups for plotting - aggressions from this year
groups.ty <- cld(summary(glht(glm.out, mcp(Method = 'Tukey'))))

##Make letters go from left to right
groups.ty$mcletters$Letters <- toupper(groups.ty$mcletters$Letters) %>%
  gsub(x = ., 'E', 'a') %>%
  gsub(x = ., 'C', 'b') %>%
  gsub(x = ., 'D', 'c') %>%
  gsub(x = ., 'B', 'd') %>%
  gsub(x = ., 'A', 'e')

prop_consistent_ny_model <- prop_consistent_ny_model %>% group_by(Year, Method) %>% summarize(cons = sum(Consistent), incons = sum(!Consistent))
prop_consistent_ny_model$Method = factor(prop_consistent_ny_model$Method,
                                         levels = c('informed_matreorder','isi',
                                                    'informed_elo','elo',
                                                    'informed_ds','ds'),
                                         labels = c('Informed MatReorder','I&SI',
                                                    'Informed Elo','Elo',
                                                    'Informed David\'s Score', 'David\'s Score'))
glm.out <- glm(data = prop_consistent_ny_model, cbind(cons, incons) ~ Method, family = binomial)
summary(glm.out)
pcomp_table(glht(glm.out, mcp(Method = 'Tukey')), title = 'prop_consistent_next_year_male')
#Create significance groups for plotting - aggressions from next year
groups.ny <- cld(summary(glht(glm.out, mcp(Method = 'Tukey'))))

##Make letters go from left to right
groups.ny$mcletters$Letters <- toupper(groups.ny$mcletters$Letters) %>%
  gsub(x = ., 'D', 'a') %>%
  gsub(x = ., 'A', 'b') %>%
  gsub(x = ., 'B','c') %>%
  gsub(x = ., 'C', 'd')


#dyadic_change_model <- dyadic_change_model %>% group_by(Method, Year) %>% summarize(change = sum(Change), no_change = sum(!Change))
#dyadic_change_model$Method <- as.factor(dyadic_change_model$Method)
glm.out <- lme4::glmer(data = model_changes, delta.active ~ method + (1|id), family = "poisson")
summary(glm.out)
pcomp_table(glht(glm.out, mcp(method = 'Tukey')), 'delta_active_male')

glm.out <- lme4::glmer(data = model_changes, delta.passive ~ method + (1|id), family = "poisson")
summary(glm.out)
pcomp_table(glht(glm.out, mcp(method = 'Tukey')), 'delta_passive_male')



cols = viridis(4)
light.cols = col2rgb(cols, FALSE)
light.cols <- colorspace::mixcolor(color1 = colorspace::RGB(R = t(light.cols/255)), color2 = colorspace::RGB(1,1,1), alpha = 0.5)@coords
combined.cols <- c(cols[1], rgb(light.cols[1,1], light.cols[1,2], light.cols[1,3]),
                   cols[2], rgb(light.cols[2,1], light.cols[2,2], light.cols[2,3]),
                   cols[3], rgb(light.cols[3,1], light.cols[3,2], light.cols[3,3]))
##Figure 7
##plot a line for each male for Informed MatReorder ranks 
p1 <- ggplot(data = filter(male.ranks, period >= 1988, period <= 2013, method == 'informed_matreorder'), aes(y = rank, x = period)) + 
  ylim(55,0)+
  theme_classic() + 
  xlab('Period')+
  ylab('Rank')+
  theme(axis.title.x = element_blank())+
  geom_line(aes(y = rank, x = period, group = id), col = combined.cols[1])+
  ggtitle(label = NULL, subtitle = 'a) Informed MatReorder')

p2 <- ggplot(data = filter(male.ranks, period >= 1988, period <= 2013, method == 'isi'), aes(y = rank, x = period)) + 
  ylim(55,0)+
  theme_classic() + 
  xlab('Period')+
  ylab('Rank')+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  geom_line(aes(y = rank, x = period, group = id), col = combined.cols[2])+
  ggtitle(label = NULL, subtitle = 'b) I&SI')

##plot a line for each individual for  Informed Elo ranks
p3 <-ggplot(data = filter(male.ranks, period >= 1988, period <= 2013, method == 'informed_elo'), aes(y = rank, x = period)) + 
  ylim(55,0)+
  theme_classic() + 
  xlab('Period')+
  ylab('Rank')+
  theme(axis.title.x = element_blank())+
  geom_line(aes(y = rank, x = period, group = id), col = combined.cols[3])+
  ggtitle(label = NULL, subtitle = 'c) Informed Elo')

##plot a line for each individual for Elo ranks
p4 <-ggplot(data = filter(male.ranks, period >= 1988, period <= 2013, method == 'elo'), aes(y = rank, x = period)) + 
  ylim(55,0)+
  theme_classic() + 
  xlab('Period')+
  ylab('Rank')+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  geom_line(aes(y = rank, x = period, group = id), col = combined.cols[4])+
  ggtitle(label = NULL, subtitle = 'd) Elo')

##plot a line for each individual for Informed David's Score ranks
p5 <-ggplot(data = filter(male.ranks, period >= 1988, period <= 2013, method == 'informed_ds'), aes(y = rank, x = period)) + 
  ylim(55,0)+
  theme_classic() + 
  xlab('Period')+
  ylab('Rank')+
  geom_line(aes(y = rank, x = period, group = id), col = combined.cols[5])+
  ggtitle(label = NULL, subtitle = 'e) Informed David\'s Score')

##plot a line for each individual for David's Score ranks
p6 <-ggplot(data = filter(male.ranks, period >= 1988, period <= 2013, method == 'ds'), aes(y = rank, x = period)) + 
  ylim(55,0)+
  theme_classic() + 
  xlab('Period')+
  ylab('Rank')+
  geom_line(aes(y = rank, x = period, group = id), col = combined.cols[6])+
  theme(axis.title.y = element_blank())+
  ggtitle(label = NULL, subtitle = 'f) David\'s Score')


pdf(file = 'plots/9.FigS5.pdf', 8, 11)
multiplot(p1,p3,p5,p2,p4,p6, cols = 2)
dev.off()

performance.of.methods <- ggplot(data = model_changes, aes(y = delta.active, x = method, fill = method, col = method)) +
  geom_boxplot(notch = TRUE) +
  theme_classic() +
  xlab('')+
  ylim(0,110)+
  ggtitle('')+
  theme(legend.key.size = unit(0.01, units = 'npc'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())+
  ylab('Total ∆a per individual')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = combined.cols)

cairo_pdf(file = 'plots/9.FigS6.pdf', 5,3)
performance.of.methods
dev.off()

##Figure 7
meds.ty <- method_eval %>% group_by(Method) %>% summarize(mm = median(Proportion_Consistent, na.rm = T))
p_cy <- ggplot(data = method_eval, aes(y = Proportion_Consistent, x = Method, fill = Method, col = Method)) + 
  geom_violin() +
  theme_classic() +
  xlab('')+
  ylim(0.43, 1.02)+
  theme(axis.text.x = element_blank(),
        legend.position = c(.4, .3),
        legend.title = element_blank()) +
  ylab('Proportion of data consistent with order')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = combined.cols)+
  ggtitle(label = NULL, subtitle = 'a) Aggressive interactions from current year')+
  geom_text(data = meds.ty, aes(x = meds.ty$Method, y = meds.ty$mm, label = groups.ty$mcletters$Letters),
            col = c('white','black','white','black','white','black'), size = 5)

meds <- method_eval %>% group_by(Method) %>% summarize(mm = median(PC_Next_Year, na.rm = T))
p_ny <- ggplot(data = method_eval, aes(y = PC_Next_Year, x = Method, fill = Method, col = Method)) + 
  geom_violin() +
  theme_classic() +
  xlab('')+
  ylim(0.43, 1.02)+
  theme(axis.text.x = element_blank(),
        legend.position = 'none') +
  ylab('')+
  scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
  scale_color_manual(values = combined.cols)+
  ggtitle(label = NULL, subtitle = 'b) Aggressive interactions from subsequent year')+
  geom_text(data = meds, aes(x = meds$Method, y = meds$mm, label = groups.ny$mcletters$Letters),
            col = c('white','black','white','black','white','black'), size = 5)

pdf(file = 'plots/9.FigS7.pdf', 9, 5)
multiplot(p_cy, p_ny, cols = 2)
dev.off()
