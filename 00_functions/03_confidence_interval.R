#
# Author:   Cristian E. Nuno
# Purpose:  Calculate upper and lower bound confidence intervals 
# Date:     February 10, 2019
#

# calculate confidence intervals ----
confidence_interval <- function(coeff, std.error, level, df) {
  # returns the a confidence interval with upper and lower bounds
  #
  # Inputs:
  # * coeff:      coefficient from model object
  # * std.error:  standard error of the coefficient
  # * level:      confidence level between 0 and 1
  # * df:         degrees of freedom
  
  if (level < 0 | level > 1) {
    stop("level must be a value between 0 and 1 (inclusive)")
  }
  
  # store lower bound
  lower.bound <- (1 - level) / 2
  
  # store upper bound
  upper.bound <- 1 - lower.bound
  
  # calculate quantile value for the student t distribution ----
  t.distribution <- qt(p = c(lower.bound, upper.bound), df = df)
  
  # calculate the product of the t.distribution and the standard error ----
  se.factor <- t.distribution * std.error
  
  # add the lower and upper se.factor values to the coefficient ----
  confidence.interval <- coeff + se.factor
  
  # name the results ----
  names(confidence.interval) <- c("lower", "upper")

  return(confidence.interval)
}

# end of script #
