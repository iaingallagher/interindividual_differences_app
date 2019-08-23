# code for interindividual differences analysis
# from talk given at SIS April 2019


######-----######
# get data and calculate typical error from test-retest
# TODO: CREATE A FUNCTION FOR THIS
# OPERATIONALISE DEFINITIONS OF REPEAT TEST DATA E.G. COLNAMES HAVE TO END IN _1 AND _2 OR SIMILAR
# ADD CODE FOR NORMAL DISTRIBUTION MEAN=0, SD=SD DIFFS/SQRT(2)
# get the data
data_in <- read.table('test_retest.csv', sep=',', header=TRUE)
head(data_in) # you should see the first 6 lines of data, first line is column headers

# create the difference scores
diff_scores <- (data_in$t1 - data_in$t2)
mean(diff_scores) # actually quite far from zero here, would give pause for thought, assumption is it's zero
# get the sd of the difference scores
sd_diff <- sd(diff_scores)
# calculate TE
te <- sd_diff/sqrt(2)
te

######-----######
# true score 95% CI for individuals
# TODO: CREATE FUNCTION TO OPERATIONALISE THIS FOR ANY CHOSEN CI, NORMAL AND T-DISTS
# ADD CODE FOR PLOT (POINT AND LINES EXTENDING)
te_limit = te*1.96 # 95% CI interval
data_in$mean <- rowMeans(data_in[,c(2,3)]) # get means by row for data colums 2 & 3 (t1 & t2)
data_in$lower = data_in$mean-te_limit # subtract interval
data_in$upper = data_in$mean+te_limit # add interval
head(data_in) # check data

######-----######
# TE to CoV; CoV to TE
# TODO: CREATE A FUNCTION FOR THIS
cov <- (te/mean(data_in$mean))*100
cov

######-----######
# meaningful changes
# TODO: CREATE A FUNCTION FOR THIS
# true score ci limit, use sd diffs directly
true_score_ci <- 1.96*sd_diff # margin of error
true_score_lims <- data.frame(row.names=data_in$subj, lower=diff_scores-true_score_ci, mean=diff_scores, upper=diff_scores+true_score_ci) # make a dataframe of the results
true_score_lims

# change above a threshold
# define a function to create a true score plot
true_score_plot <- function(df, yax_lim, null_val){
  yax <- seq(1, yax_lim) # set axis num of people
  plot(df$mean, yax, yaxt='n', ylab='', pch=19, xlab='Mean (cm)', xlim=c(min(df$lower)*1.5,max(df$upper)*1.5), ylim=c(-1, yax_lim), main='True score & 95% CI') # plot but don't label y axis
  abline(h=yax, col='grey90')
  mtext(as.character(rownames(df)), side=2, line=1, at=yax, las=2, cex=0.8) # add horizontal names on y axis
  
  arrows(df$lower, yax, df$upper, yax, angle=90, code=3, length=0.05) # add error bars
  
  abline(v=null_val, col='red') # add vertical line at zero
  
}
true_score_plot(true_score_lims, 20, 0)

######-----######
# swc
# TODO: CREATE A FUNCTION FOR THIS
# ALLOW CHOICE OF USER DEFINED VALUE OR SOME MULTIPLE OF SD (AS STANDARDISED EFFECT SIZE)
swc <- sd(data_in$t1)*0.2 # swc = baseline sd * 0.2
swc
true_score_plot(true_score_lims, 20, swc+true_score_ci)
swc+true_score_ci

######-----######
# variability based on intervention
# TODO: CREATE A FUNCTION FOR THIS, INCLUDE PLOTTING, ALLOW USER TO SELECT COLUMNS OR LIMIT INPUT AND AUTODETECT? PROB FORMER.
# variance of the intervention change, different jump data 
data_in <- read.table('jump_data.csv', header=TRUE, sep=',') # load data
head(data_in) # look at data
# plot
boxplot(data_in$cont, data_in$int) # ALWAYS PLOT YOUR DATA
# what's the difference in variance pre-to-post intervention
var_cont <- var(data_in$cont)
var_int <- var(data_in$int)
# sd of the intervention
sd_int <- sqrt(var_int - var_cont)
sd_int

######-----######
# estimating proportion of response
# TODO: CREATE FUNCTION FOR THIS, ALLOW SWC CHOICE
# the normal distribution for change
mean_int <- mean(data_in$int - data_in$cont)
vals <- seq(0.001, 0.15, 0.001) # x-axis
prob <- dnorm(vals, mean_int, sd_int) # y-axis
plot(vals, prob, type='l', main='Assumed True Score Distribution', xlab='Score (m)', ylab='Density')

# calc swc
swc <- sd(data_in$cont)*0.2
swc
# prop above/below
pnorm(swc, mean_int, sd_int, lower.tail=FALSE) # lower.tail=TRUE

# redraw the plot from above
plot(vals, prob, type='l', main='Assumed True Score Distribution', xlab='Score (m)', ylab='Density')
# define some values for shading
upper.x <- 3 # draw to
lower.x <- swc # draw from
step <- (upper.x - lower.x) / 1000 # step size from -> to
sigma <- sd_int # needed for y values below
mu <- mean_int # needed for y values below
# define coordinates for shading
cord.x <- c(lower.x,seq(lower.x,upper.x,step),upper.x)
cord.y <- c(0,dnorm(seq(lower.x,upper.x,step),mu,sigma),0)
# draw in the shading
polygon(cord.x,cord.y,col=rgb(0, 0, 0, 0.5), border=NA)
segments(x0=swc, y0=0, x1=swc, y1=7.5)
text(swc, 7.5, 'swc', pos=2, offset = 0.2) # label swc

######-----######
# CI by bootstrap
# TODO: CREATE FUNCTION, GET CODE CHECKED (NOT SURE IT DOES WHAT I THINK IT DOES)
set.seed(1234) # just for reproducibility in samples
props <- vector(length=1000)

for(i in 1:1000){
  cont <- sample(data_in$cont, 12, replace=TRUE)
  int <- sample(data_in$int, 12, replace=TRUE)
  diffs <- int-cont
  
  if( sd(int) < sd(cont) ){
    next
  }
  sd_ir <- sqrt(var(int)-var(cont))
  prop <- pnorm(swc, mean(diffs), sd_ir, lower=FALSE)
  props[i] <- prop # collect all the proportions
}

mp <- mean(props[props>0]) # mean of proportions
sd_p <- sd(props[props>0]) # sd of proportions
ci <- c((mp - 1.96*sd_p), (mp + 1.96*sd_p)) # 95% CI based on normal dist
ci