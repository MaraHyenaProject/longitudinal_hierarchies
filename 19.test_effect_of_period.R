################################################################################
#                      Test effect of period length                            #
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

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
source('0.define_functions.R')
load('18.data_variable_periods.Rdata')


cols = viridis(4)
light.cols = col2rgb(cols, FALSE)
light.cols <- colorspace::mixcolor(color1 = colorspace::RGB(R = t(light.cols/255)), color2 = colorspace::RGB(1,1,1), alpha = 0.5)@coords
combined.cols <- c(cols[1], rgb(light.cols[1,1], light.cols[1,2], light.cols[1,3]),
                   cols[2], rgb(light.cols[2,1], light.cols[2,2], light.cols[2,3]),
                   cols[3], rgb(light.cols[3,1], light.cols[3,2], light.cols[3,3]))

model.changes.p$p <- factor(model.changes.p$p,
                            levels = c('daily', 'yearly', '2 years'))

model.changes.p$method <- factor(model.changes.p$method,
                                 levels = c('informed_matreorder','isi',
                                            'informed_elo','elo',
                                            'informed_ds','ds'),
                                 labels = c('Informed MatReorder','I&SI',
                                            'Informed Elo','Elo',
                                            'Informed David\'s Score', 'David\'s Score'))


effect.of.p <- ggplot(data = filter(model.changes.p, grepl(pattern = 'Informed', method)), aes(y = delta.active, x = p, fill = method, col = method)) +
  geom_boxplot(notch = TRUE) +
  theme_classic() +
  xlab('')+
  ylim(0,110)+
  ggtitle('')+
  ylab('Total ∆a per individual')+
  scale_fill_manual(values = c(cols))+
  theme(legend.title = element_blank())+
  scale_color_manual(values = cols)


cairo_pdf(file = 'plots/19.Fig8.pdf', 5, 3)
effect.of.p
dev.off()


# ggplot(data = model.changes.p, aes(y = delta.active, x = p, fill = method, col = method)) +
#   geom_boxplot(notch = TRUE) +
#   theme_classic() +
#   xlab('')+
#   ggtitle('')+
#   ylab('Total ∆a per individual')+
#   scale_fill_manual(values = c(cols[1], 'white', cols[2], 'white', cols[3], 'white'))+
#   scale_color_manual(values = combined.cols)
