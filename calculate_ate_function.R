# input paramater response has to be a vector
calculate_ate <- function(response, match_pairs) {
  # Split treatment and control groups using match pairs list
  treatment_row <- sapply(match_pairs, function(x) x[1])
  control_row <- sapply(match_pairs, function(x) x[2])
  treatment_group <- response[unlist(treatment_row)]
  control_group <- response[unlist(control_row)]
  
  # Calculate the average treatment effect (ATE)
  ate <- mean(treatment_group) - mean(control_group)
  
  # Standard error for ATE
  n <- length(match_pairs)
  n_treatment <- length(treatment_group)
  n_control <- length(control_group)
  sd <- sqrt(var(treatment_group) / n_treatment + var(control_group) / n_control)
  #standard_error <- sd / sqrt(n)
  
  # Statistic scores
  t_res <- t.test(treatment_group, control_group, paired = TRUE)
  standard_error <- t_res$stderr
  t_stat <- t_res$statistic[["t"]]
  p_value <- t_res$p.value
  
  # Return the results
  return(list(ATE = ate, Sd.Error = standard_error, t.stat = t_stat, p.value = p_value))
}

# Test example:
# match = propscorematch(data = M, formula = A ~ hcover + pcdocs, y = M$A)$Match.pairs
# result_b = calculate_ate(M$mrate, match)
# print(result_b$ATE)
# print(result_b$Sd.Error)
# print(result_b$t.stat)
# print(result_b$p.value)
