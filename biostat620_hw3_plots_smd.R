# BIOSTAT 620 HW #3 Problem 2C

# Generate histogram and calculate SMD for quality of matching
quality_plots_smd <- function(data, match_pairs, prop_scores, var_col)
{
  # Split treatment and control groups using match pairs list
  treatment_row <- sapply(match_pairs, function(x) x[1])
  control_row <- sapply(match_pairs, function(x) x[2])

  # Find propensity scores for different groups
  prop_score_control <- prop_scores[unlist(control_row)]
  prop_score_treat <- prop_scores[unlist(treatment_row)]

  # Make paired histogram for the two groups
  hist_prop_score_control <- hist(prop_score_control)
  hist_prop_score_treat <- hist(prop_score_treat)
  hist_prop_score_control$counts <- -hist_prop_score_control$counts
  plot(hist_prop_score_treat, xlim=c(0,1), ylim=c(-100,100),
       main="Histograms of Propensity Scores by Treatment",
       xlab="Propensity Scores", col="blue")
  lines(hist_prop_score_control, col="red")
  legend("topright", c("Treatment", "Control"), fill=c("blue", "red"), cex=0.5)

  # Split variable of interest based on groups
  var_control <- data[, var_col][unlist(control_row)]
  var_treat <- data[, var_col][unlist(treatment_row)]

  # Calculate and return SMD based on variable values
  numer <- abs(mean(var_treat) - mean(var_control))
  denom <- sqrt((sd(var_treat)^2 + sd(var_control)^2)/2)
  smd <- numer/denom
  return(smd)
}
