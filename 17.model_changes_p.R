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

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
source('0.define_functions.R')
load('5.female_ranks.RData')
female.ranks.yearly <- female.ranks
load('15.female_ranks_daily.RData')
female.ranks.daily <- female.ranks
load('16.female_ranks_2yr.RData')


cols = viridis(4)
light.cols = col2rgb(cols, FALSE)
light.cols <- colorspace::mixcolor(color1 = colorspace::RGB(R = t(light.cols/255)), color2 = colorspace::RGB(1,1,1), alpha = 0.5)@coords
combined.cols <- c(cols[1], rgb(light.cols[1,1], light.cols[1,2], light.cols[1,3]),
                   cols[2], rgb(light.cols[2,1], light.cols[2,2], light.cols[2,3]),
                   cols[3], rgb(light.cols[3,1], light.cols[3,2], light.cols[3,3]))


model_changes <- data.frame()
female.ranks.yearly$delta.active <- NA
female.ranks.daily$delta.active <- NA
female.ranks.2yr$delta.active <- NA
methods <- unique(female.ranks.yearly$method)
for(current.method in methods){
  female.ranks.yearly[female.ranks.yearly$method == current.method, 'delta.active'] <- female.ranks.yearly %>%
    filter(method == current.method) %>%
    get_dynamics(type = 'rank') %>%
    pull(delta.active)
  
  # if(current.method %in% female.ranks.daily$method){
  #   female.ranks.daily[female.ranks.daily$method == current.method, 'delta.active'] <- female.ranks.daily %>%
  #     filter(method == current.method) %>%
  #     get_dynamics(type = 'rank') %>%
  #     pull(delta.active)
  # }
  
  if(current.method %in% female.ranks.2yr$method){
    female.ranks.2yr[female.ranks.2yr$method == current.method, 'delta.active'] <- female.ranks.2yr %>%
      filter(method == current.method) %>%
      get_dynamics(type = 'rank') %>%
      pull(delta.active)
  }
}
female.ranks.daily$p <- 'daily'
female.ranks.daily$period <- as.numeric(female.ranks.daily$period)
female.ranks.yearly$p <- 'yearly'
female.ranks.2yr$p <- '2 years'

female.ranks <- rbind(female.ranks.daily, female.ranks.yearly, female.ranks.2yr)

model.changes.p <- female.ranks %>%
  group_by(p, method, id) %>%
  summarise(delta.active = sum(abs(delta.active), na.rm = TRUE)) %>%
  as.data.frame()

save(model.changes.p, file = '18.model_changes_p.Rdata')
