library(lme4)
require(cocor)

nIts = 5000
p_mixed = c()
p_cocor = c()
for (iIt in 1:nIts) {
  
  if (iIt %% 50 == 0) {
    cat('Iteration ', iIt, '.', timestamp(), '\n')
  }
  
  N_Subj <- 60
  N_Within <- 2
  rel_str = 0
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
  
  fit_noInt <- lmer(y ~ 1 + (1|subj) + condition_within_subject + x, data=Data, REML = FALSE)
  fit_int <- lmer(y ~ 1 + (1|subj) + condition_within_subject * x, data=Data, REML = FALSE)
  #print(fit_noInt)
  anova(fit_noInt)
  #print(fit_int)
  anova(fit_int)
  Out0 <- anova(fit_noInt, fit_int)
  p <- Out0$'Pr(>Chisq)'[2]
  p_mixed <- c(p_mixed, p)
  
  ind1 <- condition_within_subject == 1
  ind2 <- condition_within_subject == 2
  r.jk <- cor(x[ind1], y[ind1])
  r.hm <- cor(x[ind2], y[ind2])
  r.jh <- cor(x[ind1], x[ind2])
  r.jm <- cor(x[ind1], y[ind2])
  r.kh <- cor(y[ind1], x[ind2])
  r.km <- cor(y[ind1], y[ind2])
  Out0 <- cocor.dep.groups.nonoverlap(r.jk=r.jk, r.hm=r.hm, r.jh=r.jh, r.jm=r.jm, r.kh=r.kh, r.km=r.km, n=N_Subj, alternative="two.sided", alpha=0.05, conf.level=0.95, null.value=0)
  p <-Out0@pearson1898$p.value
  p_cocor <- c(p_cocor, p)
}
cat('False pos. mixed: ', mean(p_mixed < .05), '\n')
cat('False pos. cocor: ', mean(p_cocor < .05), '\n')
cat('Correlation p_mixed, p_cocor: ', cor(p_mixed, p_cocor), '\n')
