# interindividual response functions

# 1 - TYPICAL ERROR

# 1.1 INDIVIDUAL TE WITH CI

# Individual TE CI
# uses t-dist, asymptotic to normal if >30 individuals
# sd of >10 test-retest in same individual
# outout df with mean value and te (column sd)
indiv_te_t <- function(df, ci){
  indiv_rep_means <- apply(df, 2, mean)
  indiv_te_vals <- apply(df,2,sd)
  
  deg_free <- nrow(df)
  
  # calc CI for TE's
  ci_lim <- qt((1-ci)/2, df = deg_free, lower.tail=F)  # for t dist
  indiv_te_lower <- indiv_rep_means - (indiv_te_vals*ci_lim)
  indiv_te_upper <- indiv_rep_means + (indiv_te_vals*ci_lim)
  
  ## calc moderated ci for TE's
  
  # se for TE 
  te_se <- indiv_te_vals/sqrt(nrow(df))
  
  # CI's
  ci_lim <- qt((1-ci)/2, df = deg_free, lower.tail=F)  # for t dist
  mod_indiv_te_lower <- indiv_rep_means - (te_se*ci_lim)
  mod_indiv_te_upper <- indiv_rep_means + (te_se*ci_lim)
  
  output_df <- data.frame(id = colnames(df), indiv_means = indiv_rep_means, indiv_te = indiv_te_vals, mod_ci_lo = mod_indiv_te_lower, mod_ci_hi = mod_indiv_te_upper, ci_lo = indiv_te_lower , ci_hi = indiv_te_upper)
  colnames(output_df) <- c("ID", "Indiv Test Means", "Indiv TE", "Moderated Lower CI Limit", "Moderated Upper CI Limit", "Lower CI Limit", "Upper CI Limit")
  # rownames(output_df) <- colnames(df)
  return(output_df)
}


# 1.2 SIMPLE TEST-RETEST DIFFS
# input two vectors representing test data 1 and test data 2
# te calculated as per Swinton 2018 (sd(diffs)/sqrt(2))
te <- function(t1, t2, ci){
  diffs <- t1-t2
  mean_diff <- mean(diffs)
  sd_diff <- sd(diffs)
  typical_error <- sd(t1-t2)/sqrt(2)
  
  # TODO:change to t-dist for lower, upper, need df for t ?nrow
  
  # calc ci
  ci_lim <- qnorm((1-ci)/2, lower.tail=F)  # for normal dist
  te_lower <- mean_diff - (typical_error*ci_lim)
  te_upper <- mean_diff + (typical_error*ci_lim)
  
  te_return_df <- data.frame("Mean Difference"=mean_diff, "SD Differences"=sd_diff, "Typical Error"=typical_error, "True Score CI lower"=te_lower, "True Score CI Upper"=te_upper)
  rownames(te_return_df) <- "Typical Error Results"
  
  return(te_return_df)
}


# TE from CoV
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


# 2 - Change scores
# individual CI for change scores
# input two vectors representing pre & post intervention values, est te  
te <- function(t1, t2, ci){
  diffs <- t1-t2
  mean_diff <- mean(diffs)
  sd_diff <- sd(diffs)
  typical_error <- sd(t1-t2)/sqrt(2)
  
  # TODO:bake in norm or t usinf selector in app
  
  # calc ci
  ci_lim <- qnorm((1-ci)/2, lower.tail=F)  # for normal dist
  te_lower <- mean_diff - (typical_error*ci_lim)
  te_upper <- mean_diff + (typical_error*ci_lim)
  
  te_return_df <- data.frame("Mean Difference"=mean_diff, "SD Differences"=sd_diff, "Typical Error"=typical_error, "True Score CI lower"=te_lower, "True Score CI Upper"=te_upper)
  
  return(te_return_df)
}

# Individual te
# outout df with change score value and ci limits
indiv_cs <- function(pre, post, te, ci){
  indiv_change_scores <- (post-pre) # vector of change scores
  indiv_change_score_sd <- te*sqrt(2) # vector of change score sds
  
  # calc ci for change scores
  ci_lim <- qnorm((1-ci)/2, lower.tail=F)  # for normal dist
  indiv_cs_lower <- indiv_change_score - (change_score_sd*ci_lim)
  indiv_cs_upper <- indiv_change_score + (change_score_sd*ci_lim)
  
  output_df <- data.frame(id = colnames(df), indiv_change = indiv_change_scores, indiv_cs_sd = indiv_change_score_sd, ci_lo = indiv_cs_lower , ci_hi = indiv_cs_upper)
  colnames(output_df) <- c("ID", "Change", "Change SD", "Lower Change CI Limit", "Upper Change CI Limit")
  return(output_df)
}