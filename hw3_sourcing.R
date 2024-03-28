# BIOSTAT 620 - Homework #3
# R file for sourcing functions

propscorematch <- function(data, formula, y) {
  # create model
  model <- glm(formula, data = data, family = binomial(link = "logit"))
  # propensity scores
  prop_score <- model$fitted
  # dataframe
  df <- cbind(data, prop_score)
  # initialize match and controls
  matches <- list()
  controls <- which(y == 0)

  # find matches with greedy rule
  for (i in which(y == 1)) {
    prop_diff <- abs(prop_score[i] - df$prop_score[controls])
    greed <- controls[which.min(prop_diff)]
    matches[[i]] <- c(i, greed)
    controls <- which(y == 0) # allows for repeat
    controls <- controls[-which(controls == greed)]
  }
  return(list(Match.pairs = matches, Propensity = prop_score))
}

# input paramater response has to be a vector
calculate_ate <- function(response, match_pairs) {
  # Split treatment and control groups using match pairs list
  treatment_row <- sapply(match_pairs, function(x) x[1])
  control_row <- sapply(match_pairs, function(x) x[2])
  treatment_group <- response[unlist(treatment_row)]
  control_group <- response[unlist(control_row)]

  # Calculate the average treatment effect (ATE)
  ate <- mean(treatment_group) - mean(control_group)

  t_res <- t.test(treatment_group, control_group, paired=TRUE)
  standard_error <- t_res$stderr
  t_stat <- t_res$statistic[["t"]]
  p_value <- t_res$p.value

  # Return the results
  return(list(ATE = ate, Sd.Error = standard_error, t.stat = t_stat, p.value = p_value))
}

# Generate Histogram and Calculate SMD for quality of matching
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

# Combining into one mega R function
propensity_score_match <- function(data, formula, treatment_col, response_col) {
  match = propscorematch(data, formula, y = data[treatment_col])$Match.pairs
  prop_score = propscorematch(data, formula, y = data[treatment_col])$Propensity
  ate_scores = calculate_ate(data[,response_col], match)
  smd = quality_plots_smd(data, match, prop_score, response_col)
  return(list(Matching.pairs = match, Propensity = prop_score, ATE.scores = ate_scores, SMD = smd))
}
