# input paramater response has to be a vector
calculate_ate <- function(response, match_pairs) {
  # Split treatment and control groups using match pairs list
  treatment_row <- sapply(match_pairs, function(x) x[1])
  control_row <- sapply(match_pairs, function(x) x[2])
  treatment_group <- response[treatment_row]
  control_group <- response[control_row]
  
  # Calculate the average treatment effect (ATE)
  ate <- mean(treatment_group) - mean(control_group)
  
  # Standard error for ATE
  n <- length(match_pairs)
  n_treatment <- length(treatment_group)
  n_control <- length(control_group)
  sd <- sqrt(var(treatment_group) / n_treatment + var(control_group) / n_control)
  standard_error <- sd / sqrt(n)
  
  # Statistic scores
  t_stat <- ate / standard_error
  df <- n - 1
  p_value <- 2 * pt(-abs(t_stat), df)
  
  # Return the results
  return(list(ATE = ate, Sd.Error = standard_error, t.stat = t_stat, p.value = p_value))
}
# Test example:
# result = calculate_ate(M$mrate, match)
# print(result$ATE)
# print(result$p.value)
