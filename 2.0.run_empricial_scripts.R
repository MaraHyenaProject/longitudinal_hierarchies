################################################################################
#                   Run scripts for empirical analysis                         #
#                                                                              #
#                                                                              #
#                            By Eli Strauss                                    #
#                                                                              #
#                            November 2018                                     #
################################################################################

setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/')
set.seed(1989)
source('0.define_functions.R')
source('2.prep_empirical_data.R')
source('4.emprical_ranks_females.R')
source('6.empirical_ranks_males.R')
source('8.process_model_plot_empirical_females.R')
source('9.process_model_plot_empirical_males.R')
source('10.descriptives_and_metadata.R')
source('11.prep_data_daily_periods.R')
source('13.estimate_ranks_daily_period.R')
source('14.estimate_ranks_2yr_period.R')
source('17.process_hierarchies_variable_periods.R')
source('19.test_effect_of_period.R')

