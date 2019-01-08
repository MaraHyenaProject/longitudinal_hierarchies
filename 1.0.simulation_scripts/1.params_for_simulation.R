################################################################################
#                          Params for simulation                               #
#                                                                              #
#                                                                              #
#                            By Eli Strauss                                    #
#                                                                              #
#                            November 2018                                     #
################################################################################

#############Simulate dynamic hierarchy and detect with different methods#######
#####Parameters structuring the data###
##Don't change
num_ids <- 20
ids <- LETTERS[1:num_ids]
num_periods <- 20
num_dyads <- (num_ids*num_ids - num_ids)/2
percent_unknowns <- c(low = 0.2, medium = 0.5, high = 0.8)
iterations <- 10


save.image('2.params_for_simulation.RData')
