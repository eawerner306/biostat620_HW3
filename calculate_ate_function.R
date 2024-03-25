calculate_ate <- function(treatment, reponse) {
  # Split treatment and control groups
  treatment_group <- reponse[treatment == 1]
  control_group <- reponse[treatment == 0]
  
  # Calculate the average treatment effect (ATE)
  ate <- mean(treatment_group) - mean(control_group)
  
  # Standard error for ATE
  n_treatment <- length(treatment_group)
  n_control <- length(control_group)
  sd <- sqrt(var(treatment_group) / n_treatment + var(control_group) / n_control)
  standard_error <- sd / sqrt(min(n_treatment, n_control))
  
  # Statistic scores
  t_stat <- ate / standard_error
  df <- min(n_treatment, n_control) - 1
  p_value <- 2 * pt(-abs(t_stat), df)
  
  # Return the results
  list(ATE = ate, Sd.Error = standard_error, t.stat = t_stat, p.value = p_value)
}

# Test example:
# A <- c(1, 1, 1, 1, 0, 0, 0, 0, 0)
# mrate <- c(11, 14, 24, 20, 26, 20, 3, 7, 8)
# M <- cbind(A, mrate)
# result <- calculate_ate(M[, "A"], M[, "mrate"])
# print(result)