#
# Author:   Cristian E. Nuno
# Purpose:  Create four single models in a list ----
# Date:     February 5, 2019
#

build_single_models <- function(x, y, df) {
  # Returns a named list of four tibbles
  # with each tibble representing one model object
  # with one explanatory variable
  #
  # Inputs:
  # * x:  character vector representing the explanatory variable
  # * y:  character vector representing the response variable
  # * df: data.frame that contains x & y as columns
  #
  # generalized additive model
  # with a shrinkage version of cubic regression spline
  list("gam" = mgcv::gam(get(y) ~ s(get(x), bs = "cs"), data = df)
       # linear model with 2 degrees of freedom
       , "linear" = lm(get(y) ~ get(x), data = df)
       # smoothing spline with 6 degrees of freedom
       , "ss_df6" = lm(get(y) ~ splines::bs(get(x), df = 6), data = df)
       # smoothing spline with 22 degrees of freedom
       , "ss_df22" = lm(get(y) ~ splines::bs(get(x), df = 22), data = df)) %>%
    # transform each model object into a tibble
    purrr::map(.f = ~ broom::augment(.x) %>%
                 # rename the get..x.. & get..y.. columns
                 # note: gam & linear have get.x. & get.y. whereas
                 #       both splines only have get.y.
                 dplyr::rename_at(dplyr::vars(dplyr::matches("get.."))
                                  , dplyr::funs(stringr::str_replace(.
                                                                     , "get.y."
                                                                     , y) %>%
                                                  stringr::str_replace("get.x."
                                                                       , x)))) %>%
    # add x as column for smoothing spline models 
    purrr::imap(.f = ~ if(.y %in% c("ss_df6", "ss_df22")) {
      .x[, x] <- df[, x]
      
      .x
    } else {
      .x
    })
  
}
