#### Code and data for Strauss & Holekamp (2019) Inferring longitudinal hierarchies paper

Strauss ED, Holekamp  KE. Inferring longitudinal hierarchies: framework and methods for studying the dynamics of dominance. *Journal of Animal Ecology.*  
     
#### Overview
Files are numbered according to the order of their appearance in the analysis. File paths in the scripts will need to be changed in order for the code to run properly. Below is a list of files in the order they should be run to reproduce the analysis. 

##### Simulations
* __0.define_functions__ - define functions used throughout analysis
* __1.0.simulation_scripts__ - folder containing simulation scripts. Output is saved in parameter specific folders
    * __1.params_for_simulation__ - set general simulation parameters
    * __3.simulate_hierarchies_and_interactions__ - simulate data 
    * __5.infer_simulated_hierarchies__ - infer hierarchies from simulated data using different methods
    * __7.process_data_simulated_hierarchy_inference__ - process data for analysis
    * __9.model_methods_simulations__ - compare performance of different methods
* __1.1.simulation_workflow_0.9_few_changes__ - folder containing .RData files produced by the scripts in *1.0.simulation_scripts*. Correlation between true ranks and rank-associated trait = 0.9. Hierarchy is simulated with few changes.
* __1.2.simulation_workflow_0.9_many_changes__ - folder containing .RData files produced by the scripts in *1.0.simulation_scripts*. Correlation between true ranks and rank-associated trait = 0.9. Hierarchy is simulated with many changes.
* __1.3.simulation_workflow_0.7_few_changes__ - folder containing .RData files produced by the scripts in *1.0.simulation_scripts*. Correlation between true ranks and rank-associated trait = 0.7. Hierarchy is simulated with few changes.
* __1.4.simulation_workflow_0.5_few_changes__ - folder containing .RData files produced by the scripts in *1.0.simulation_scripts*. Correlation between true ranks and rank-associated trait = 0.5. Hierarchy is simulated with few changes.
* __1.5.simulation_workflow_0.2_few_changes__ - folder containing .RData files produced by the scripts in *1.0.simulation_scripts*. Correlation between true ranks and rank-associated trait = 0.2. Hierarchy is simulated with few changes.
* __21.check_k_few_changes__ - rerun some analyses with K = 200 for Elo-rating methods for supplemental materials. 
* __21.check_k_many_changes__ - rerun some analyses with K = 200 for Elo-rating methods for supplemental materials. 
* __22.passive_plots__ - produce plots of dynamics due to passive processes for supplemental materials. 

##### Analysis of empirical data
* __0.define_functions__ - define functions used throughout analysis
* __0.rawdata__ - raw data before cleaning and prep for analysis. Available upon request from Kay Holekamp. To use tidied data, skip to *3.hyena_data.RData*. 
* __2.0.run_empirical_scripts__ - source all scripts in analysis of empirical data
* __2.prep_empirical_data__ - tidy and prepare raw data for analysis
* __3.hyena_data.RData__ - tidy data file *(start here)*
* __4.empirical_ranks_females__ - infer adult female hierarchies using different methods.
* __5.female_ranks.RData__ - hierarchies inferred by different methods for females
* __6.empirical_ranks_males__ - infer adult male hierarchies using different methods.
* __7.male_ranks.RData__ - hierarchies inferred by different methods for males
* __8.process_model_plot_empirical_females__ - compare performance of methods at inferring longitudinal hierarchies of adult females
* __9.process_model_plot_empirical_males__ - compare performance of methods at inferring longitudianl hierarchies of adult males
* __10.descriptives_and_metadata__ - basic descriptives of empirical data
* __11.prep_data_daily_periods__ - prep data for analysis with daily periods rather than yearly periods
* __12.hyena_data_daily_period.RData__ - data prepped for inferring hierarchies with daily period. 
* __13.estimate_ranks_daily_period__ - infer hierarchies with daily periods 
* __14.estimate_ranks_2yr_period__ - infer hierarchies with 2-year periods
* __15.female_ranks_daily.RData__ - hierarchies inferred with daily periods
* __16.female_ranks_2yr.RData__ - hierarchies inferred with 2-year periods
* __17.process_hierarchies_variable_periods__ - prep data to compare hierarchy dynamics for hierarchies inferred with different period delineation rules
* __18.data_variable_periods.RData__ - data for testing effect of period delineation rule
* __19.test_effect_of_period__ - plot effect of period delineation rule
* __20.confirm_transitivity__ - assess triangle transitivity
* __22.passive_plots__ - plot hierarchy dynamics due to passive processes
