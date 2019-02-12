#
# Author:   Cristian E. Nuno
# Purpose:  Calculate the standard error for each 
#           coefficient of a simple model
# Date:     February 10, 2019
#

# create standard_error function ----
standard_error <- function(model, input, intercept) {
  # returns the standard error for either 
  # the intercept or the input of a simple model
  #
  # Inputs:
  # * model:      a model object
  # * input:      name of variable that your regressing onto Y
  # * intercept:  return the standard error of the intercept?
  #               if FALSE, returns the standard error 
  #               for the predictor; else the standard error
  #               for the intercept
  #
  
  # estimate irreducible error by calculating residual standard error ----
  rse <- 
    sum(model$residuals^2) / model$df.residual
  
  # store the input predictor values from the model -----
  input.values <- model$model[, input]
  
  # calculate the average value from input.values -----
  x.bar <- mean(input.values)
  
  # store each deviation of the input.values from x.bar ----
  deviations <- input.values - x.bar
  
  # store sum of squared deviations ----
  sum.squared.deviations <- sum(deviations^2)
  
  # store the proportion ----
  proportion <- 1 / nrow(model$model)
  
  
  if (intercept) {
    standard.error.squared <- 
      rse * 
      (proportion + (x.bar^2 / sum.squared.deviations))
  } else {
    standard.error.squared <-
      rse / sum.squared.deviations
  }
  # return the square root of standard.error.squared  -----
  return(sqrt(standard.error.squared))
}

# end of script #
