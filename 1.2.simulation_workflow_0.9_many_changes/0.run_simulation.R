################################################################################
#                            Run simulation                                    #
#                                                                              #
#                                                                              #
#                            By Eli Strauss                                    #
#                                                                              #
#                            November 2018                                     #
################################################################################

rm(list = ls())
setwd('~/Documents/Research/Longitudinal_Hierarchies/FinalSubmission/1.2.simulation_workflow_0.9_many_changes/')
set.seed(1989)
####Variable parameters

#trait_veracity = 0.9, 0.7, 0.5, 0.2 to test effect of trait veracity
trait_veracity= 0.9


#text description of hierarchy stability for plot naming later
# many_changes, few_changes
num_changes = 'many_changes'    

# changers_lambda = 1 for few_changes,
# changers_lambda = 5 for many_changes 
changers_lambda = list(few_changes = 1,
                       many_changes = 5)
changers_lambda = changers_lambda[[num_changes]]

#change_distance_rate = 1 for few_changes
#change_distance_rate = 0.5 for many_changes
change_distance_rate = list(few_changes = 1,
                            many_changes = 0.5)

change_distance_rate = change_distance_rate[[num_changes]]

source('../1.0.simulation_scripts/1.params_for_simulation.R')
source('../1.0.simulation_scripts/3.simulate_hierarchies_and_ineractions.R')
source('../1.0.simulation_scripts/5.infer_simulated_hierarchies.R')
source('../1.0.simulation_scripts/7.process_data_simulated_hierarchy_inference.R')
source('../1.0.simulation_scripts/9.model_methods_simulations.R')
