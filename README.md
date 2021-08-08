# test_dep_cor_sims
Monte Carlo estimation of p-values for correlation differences between within-subject conditions

The script test_dep_cor_sims.R is some R code for messing around with a question that came up about testing differences in the correlation between two variables, with within-subject conditions. These simulations turned out to support what comparingcorrelations.org did. The randomization here, essentially, shuffles the scores of one variable, but keeps the values for two conditions together, to the aim of maintaining the dependence between scores.

The test_methods.R file contains code to run and compare some possible alternatives to test the same thing - either the cocor toolbox or mixed-effect models.
