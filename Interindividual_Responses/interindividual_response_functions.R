# interindividual response functions

# 1 - TYPICAL ERROR ####

# 1.1 INDIVIDUAL TE WITH CI #####

# Individual te CI, t dist 
# using t-test to help moderate CI's given small num of test-retest
# calculates CI with sd from tests and moderated CI with sd / sqrt(n tests) (EXTENSION 1)
# outputs df with mean, te, unmoderated and moderated CI limits
indiv_te_t <- function(df, ci){
  
  n_tests <- nrow(df) # get number of tests from number of dataframe rows
  
  indiv_rep_means <- apply(df, 2, mean) # means
  indiv_te_vals <- apply(df, 2, sd) # typical error
  
  ## calc unmoderated ci for TE's
  ## uses t-dist but unmoderated se
  ci_quantile <- qt((1-ci)/2, df = n_tests-1, lower.tail=F)  # t dist, df = n-1
  indiv_te_lower <- indiv_rep_means - (indiv_te_vals * ci_quantile)
  indiv_te_upper <- indiv_rep_means + (indiv_te_vals * ci_quantile)
  
  # EXTENSION 1, account for >1 test using std err
  
  ## calc moderated ci for TE's
  ## uses t-dist but moderated se for number of tests
  
  # se for CI 
  te_se <- indiv_te_vals/sqrt(n_tests) 
  
  # CI's
  ci_lim <- qnorm((1-ci)/2, lower.tail=F)  # for normal dist
  mod_indiv_te_lower <- indiv_rep_means - (te_se*ci_lim)
  mod_indiv_te_upper <- indiv_rep_means + (te_se*ci_lim)
  
  output_df <- data.frame(id = colnames(df), indiv_means = indiv_rep_means, indiv_te = indiv_te_vals, mod_ci_lo = mod_indiv_te_lower, mod_ci_hi = mod_indiv_te_upper, ci_lo = indiv_te_lower , ci_hi = indiv_te_upper)
  
  colnames(output_df) <- c("ID", "Indiv Test Means", "Indiv TE", "Moderated Lower CI Limit", "Moderated Upper CI Limit", "Lower CI Limit", "Upper CI Limit")
  
  # output_df <- output_df$ID <- factor(output_df$iID, levels=output_df$ID) # fix plot ordering issue?
  # rownames(output_df) <- colnames(df)
  return(output_df)
}


# 1.2 SIMPLE TEST-RETEST DIFFS ####

# input two vectors representing test data 1 and test data 2
# te calculated as per Swinton 2018 (sd(diffs)/sqrt(2))
TE <- function(t1, t2, ci){
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


# 1.3 TE FROM PREV LIT CoV #####

# input CoV, observed score & desired CI
cov_te <- function(cv, os, ci){
  cov_te_est <- (cv*os)/100 # Swinton 2018
  
  # calc ci for TE's
  # here use normal dist because we can't get df for use of t-dist
  ci_lim <- qnorm((1-ci)/2, lower.tail=F) # for normal dist
  cv_te_lower <- os - (cov_te_est*ci_lim)
  cv_te_upper <- os + (cov_te_est*ci_lim)
  
  cv_df <- data.frame(os=os, te=cov_te_est, ci_lo = cv_te_lower, ci_hi = cv_te_upper)
  colnames(cv_df) <- c("Obs Score", "Est TE", "Lower CI limit", "Upper CI limit")
  return(cv_df)
}

# 1.4 TE FROM LIT BASED ON MEAN OF TESTS

# input CoV, df & desired CI
grp_cov_te <- function(cv, df, ci){

  baseline_mean <- mean(df)
  grp_cov_te_est <- cov_te(cv=cv, os=baseline_mean, ci=ci)

  return(grp_cov_te_est)
}




# 2 - CHANGE SCORES ####

# 2.1 - CI FOR CHANGE SCORES
# input two vectors representing pre & post intervention values for number of indivs
# est te & te ci

# Calculate change score CI's accounting for TE
# output df with change score value and ci limits
cs_ci <- function(pre, post, te, ci){
  change_score <- (post-pre) # vector of change scores
  change_sd <- te*sqrt(2) # vector of change score sds
  
  # use t-dist for CIs
  # deg_free <- length(change_score) ## CURRENTLY NOT USED
  # calc ci for change scores
  # ci_lim <- qt((1-ci)/2, df = deg_free, lower.tail=F)  # for t dist
  ci_lim <- qnorm((1-ci)/2, lower.tail=F) # for norm dist
  cs_lower <- change_score - (change_sd*ci_lim)
  cs_upper <- change_score + (change_sd*ci_lim)
  
  output_df <- data.frame(change = change_score, change_sd = change_sd, te = te, ci_lo = cs_lower , ci_hi = cs_upper)
  colnames(output_df) <- c("Change", "Change SD", "TE", "Lower CI Limit", "Upper CI Limit")
  
  return(output_df)
}


# 3 - PROPORTION OF RESPONSE ####

# 3.1 - Variability due to intervention ####
# input two vectors representing change score in intervention group & change score in control group
# estimate intervention response sd

int_sd <- function(df, pre, post, grp_var, ctrl_ind, int_ind){
  # get group data
  # https://stackoverflow.com/questions/17075529/subset-based-on-variable-column-name
  # mad syntax - seems overly complex to pass an unknown column
  ctrl_pre_data <- df[ df[[grp_var]] == ctrl_ind , pre]
  ctrl_post_data <- df[ df[[grp_var]] == ctrl_ind , post]
  ctrl_diff <- ctrl_post_data - ctrl_pre_data
  var_ctrl_cs <- var(ctrl_diff)
  
  int_pre_data <- df[ df[[grp_var]] == int_ind , pre]
  int_post_data <- df[ df[[grp_var]] == int_ind , post]
  int_diff <- int_post_data - int_pre_data
  var_int_cs <- var(int_diff)
  
  # calculate intervention variation
  intervention_sd <- sqrt(var_int_cs - var_ctrl_cs) # calc intervention sd
  # cal mean diff for int group
  int_mean_diff <- mean(int_post_data - int_pre_data)
  # return df
  data_out <- data.frame(`Intervention Mean Diff` = int_mean_diff, `Intervention SD` = intervention_sd)
  
  # return(intervention_sd)
  return(data_out)
}

# 3.2 - Proportion of response
# input df with pre scores column, post scores column; numeric intervention sd & swc
# true change follows normal with u = obs change and sd  = intervention response sd (see 3.1)
# prop response is area of this normal dist beyond swc

prop_resp <- function(mn, sd, eff_sz, direction){
  
  # calculate prop resp for normal dist
  swc <- sd * eff_sz
  
  # calculate prop if above
  if (direction == "Above"){
    prop_responders <- pnorm(swc, mn, sd, lower.tail = FALSE)
  }
  else{
    prop_responders <- pnorm(swc, mn, sd)
  }
  
  # gather outputs
  ret_df <- data.frame(mn, sd, prop_responders)
  return(ret_df)
}
