# test_dep_cor_sims
Monte  Carlo estimation of p-values for correlation differences between within-subject conditions

This is some R code for messing around with a question that came up about testing differences in the correlation between two variables, with within-subject conditions. It turned out to support what comparingcorrelations.org did. The randomization here, essentially, shuffles the scores of one variable, but keeps the values for two conditions together, to the aim of maintaining the dependence between scores.

N_Subj <- 60
N_Within <- 2
rel_str = 0.3
subj_var = 1.5;

subj <- rep(1:N_Subj, each=N_Within)
subj_effect <- subj_var * rep(rnorm(N_Subj), each=N_Within)

condition_within_subject <- rep(1:N_Within, length.out=length(subj))

x <- rnorm(length(condition_within_subject))
y <- rnorm(length(condition_within_subject))
y[condition_within_subject==2] = rel_str * x[condition_within_subject==2] + (1 - rel_str) * rnorm(length(condition_within_subject)/2)
y <- y + subj_effect

subj <- as.factor(subj)
condition_within_subject <- as.factor(condition_within_subject)
Data <- data.frame(subj, condition_within_subject, x, y)

test_cor = function(Data) {
  
  ind_cond1 <- Data$condition_within_subject == 1
  ind_cond2 <- Data$condition_within_subject == 2
  r.jk_obs <- cor(Data$x[ind_cond1], Data$y[ind_cond1])
  r.hm_obs <- cor(Data$x[ind_cond2], Data$y[ind_cond2])
  
  drv <- c()
  nIts <- 10000
  for (iIt in 1:nIts) {
    # Shuffle both x-scores over participants
    X <- matrix(Data$x, nrow = 2)
    X <- X[, sample(ncol(X))]
    x <- as.vector(X)
    r.jk_sh <- cor(x[ind_cond1], y[ind_cond1])
    r.hm_sh <- cor(x[ind_cond2], y[ind_cond2])
    dr <- abs(r.jk_sh - r.hm_sh)
    drv <- c(drv, dr)
  }
  p <- sum(drv >= abs(r.jk_obs - r.hm_obs))/length(drv)
  cat('MC p = ', p, '\n')
}

test_cor(Data)
