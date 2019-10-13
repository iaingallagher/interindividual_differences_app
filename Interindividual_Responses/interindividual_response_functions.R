# interindividual response functions

# 1 - typical error
# simple difference between test & retest
# input two vectors represetning test data 1 and test data 2
# te calculated as per Swinton 2018 (sd(diffs)/sqrt(2))
te <- function(t1, t2, ci){
  diffs <- t1-t2
  mean_diff <- mean(diffs)
  sd_diff <- sd(diffs)
  typical_error <- sd(t1-t2)/sqrt(2)
  
  # calc ci
  ci_lim <- qnorm((1-ci)/2, lower.tail=F)  # for normal dist
  te_lower <- mean_diff - (typical_error*ci_lim)
  te_upper <- mean_diff + (typical_error*ci_lim)
  
  te_return_df <- data.frame("Mean Difference"=mean_diff, "SD Differences"=sd_diff, "Typical Error"=typical_error, "True Score CI lower"=te_lower, "True Score CI Upper"=te_upper)
  
  return(te_return_df)
}

# Individual te
# sd of >10 test-retest in same individual
# outout df with mean value and te (column sd)
indiv_te <- function(df, ci){
  indiv_rep_means <- apply(df, 2, mean)
  indiv_te_vals <- apply(df,2,sd)
  
  # calc ci for TE's
  ci_lim <- qnorm((1-ci)/2, lower.tail=F)  # for normal dist
  indiv_te_lower <- indiv_rep_means - (indiv_te_vals*ci_lim)
  indiv_te_upper <- indiv_rep_means + (indiv_te_vals*ci_lim)
  
  output_df <- data.frame(id = colnames(df), indiv_means = indiv_rep_means, indiv_te = indiv_te_vals, ci_lo = indiv_te_lower , ci_hi = indiv_te_upper)
  colnames(output_df) <- c("ID", "Indiv Test Means", "Indiv TE", "Lower CI Limit", "Upper CI Limit")
  # rownames(output_df) <- colnames(df)
  return(output_df)
}

# TE form CoV
# input os observed score & CoV for procedure
cov_te <- function(cv, os, ci){
  cov_te_est <- (cv*os)/100 # Swinton 2018
  # calc ci for TE's
  ci_lim <- qnorm((1-ci)/2, lower.tail=F) # for normal dist
  cv_te_lower <- os - (cov_te_est*ci_lim)
  cv_te_upper <- os + (cov_te_est*ci_lim)
  
  cv_df <- data.frame(os=os, te=cov_te_est, ci_lo = cv_te_lower, ci_hi = cv_te_upper)
  colnames(cv_df) <- c("Obs Score", "Est TE", "Lower CI limit", "Upper CI limit")
  return(cv_df)
}

# 2 - True score CI, variable CI
# true_score_ci <- function(typ_error, qnt){
#   
# 
# }