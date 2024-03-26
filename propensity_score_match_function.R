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
  return(matches)
}

# Example from 'propensityW24.pdf'
# test <- data.frame(treatment = c(1, 1, 1, 1, 0, 0, 0, 0, 0),
#                  hcover = c(0.6, 0.7, 0.5, 0.7, 0.7, 0.6, 0.2, 0.2, 0.1),
#                  pcdocs = c(0.02, 0.01, 0.02, 0.01, 0.02, 0.01, 0.04, 0.04, 0.05))
# propscorematch(data = test, formula = treatment ~ hcover + pcdocs, y = test$treatment)
