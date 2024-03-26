# BIOSTAT 620 HW #3 Problem 2C

# data is raw data file
# treat_col is the column in data with the treatment options
# prop_scores is the fitted model for propensity scores
# t0 is the name of treatment 0
# t1 is the name of treatment 1
# var is the variable of interest after subsetting based on treatments
quality_plots_smd <- function(data, treat_col, prop_scores, t0, t1, var)
{
  data_sub <- data[,treat_col]
  prop_score_t0 <- prop_scores[data_sub == t0]
  prop_score_t1 <- prop_scores[data_sub == t1]
  hist_prop_score_t0 <- hist(prop_score_t0)
  hist_prop_score_t1 <- hist(prop_score_t1)
  hist_prop_score_t0$counts <- -hist_prop_score_t0$counts
  plot(hist_prop_score_t1, xlim=c(0,1), ylim=c(-50,50),
       main="Histograms of Propensity Scores by Treatment",
       xlab="Propensity Scores", color="blue")
  lines(hist_prop_score_t0, color="red")
  legend("topright", "Treatment 1", "Treatment 0",
         fill=c("blue", "red"), cex=0.5)

  data_t0 <- data[data[treat_col] == t0,]
  data_t1 <- data[data[treat_col] == t1,]
  numer <- abs(mean(data_t1[var]) - mean(data_t0[var]))
  denom <- sqrt((sd(data_t1[var])^2 + sd(data_t0[var])^2)/2)
  smd <- numer/denom
  return(smd)
}
