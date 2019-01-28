Statistical Learning
================
Cristian E. Nuno
January 27, 2019

-   [What is Statistical Learning?](#what-is-statistical-learning)
-   [Why estimate f?](#why-estimate-f)
-   [How Do We Estimate f?](#how-do-we-estimate-f)
-   [The Trade-Off Between Prediction Accuracy and Model Interpretability](#the-trade-off-between-prediction-accuracy-and-model-interpretability)
-   [Session Info](#session-info)

``` r
# load necessary packages -----
library(gridExtra)
library(here)
library(scales)
library(tidyverse)

# load necessary objects ----
my.theme <- 
  theme_minimal() + 
  theme(panel.grid = element_blank()
        , plot.caption = element_text(size = 5))

islr.text <-
  "Source: ISLR (http://www-bcf.usc.edu/~gareth/ISL/data.html)"

# set dpi for all chunks ----
knitr::opts_chunk$set(dpi = 300)

# load necessary data ----
df <- 
  read_csv(here("00_raw_data", "advertising.csv")) %>%
  # drop row number column
  select(-X1)

# convert from wide to long ----
df.tidy <-
  df %>%
  gather(key = "type", value = "budget", -sales)
```

What is Statistical Learning?
-----------------------------

Suppose that we are statistical consultants hired by a client to provide advice on how to improve sales of a particular product.

The [`Advertising`](https://github.com/cenuno/islr_notes/blob/master/00_raw_data/advertising.csv) data set consists of the `sales` of that product in 200 different markets, along with advertising budgets for the product in each of those markets for three different media: `TV`, `radio`, and `newspaper`.

While it isn't possible for the client to change the number of sales, they can control the advertising budget in each of the three media. Our goal is to adjust the advertising budget so that we may indirectly increase sales.

In this case, advertising budgets are the *input variables* - *X* - while `sales` is the *output variable* - *Y*.

*X* goes by many names: predictors, independent variables, features, or sometimes just variables. The same for *Y*: response or dependent variable.

The relationship between sales as a measured by each media type's advertising budget is shown belown.

``` r
# visualize all three advertising types and their relationship to sales ----
df.tidy %>%
  ggplot(aes(x = budget, y = sales, color = type)) +
  geom_point(show.legend = FALSE) + 
  geom_smooth(se = FALSE
              , method = "lm"
              , color = "black") +
  scale_x_continuous(name = "Budget", labels = dollar) +
  scale_y_continuous(name = "Sales"
                     , labels = dollar
                     , limits = c(0, 30)) +
  facet_wrap(facets = vars(type)) +
  labs(title = "Increasing the radio advertisement budget\nincreases sales at a higher rate than both TV or newspapers"
       , caption = islr.text) +
  my.theme
```

![](README_files/figure-markdown_github/examine%20advertising-1.png)

Another example of visualizing the relationship between *X* and *Y* uses `income` and `years of education` from the [`Income1`](https://github.com/cenuno/islr_notes/blob/master/00_raw_data/income1.csv) data set.

``` r
income1 <- 
  read_csv(here("00_raw_data", "income1.csv")) %>%
  select(-X1)

base.plot <-
  income1 %>%
  ggplot(aes(x = Education, y = Income)) +
  geom_point(color = "orchid4") +
  scale_y_continuous(name = "Annual income"
                     , labels = function(i) paste0("$", i, "K")) +
  scale_x_continuous(name = "Years of education"
                     , breaks = pretty_breaks()) +
  my.theme +
  theme(plot.title = element_text(size = 8)
        , plot.subtitle = element_text(size = 6))

loess.plot <-
  base.plot + 
  geom_smooth(se = FALSE
              , color = "#898b47"
              , method = "loess") +
  geom_segment(aes(xend = Education
                   , yend = loess(Income ~ Education, data = income1)$fitted)
               , color = "#47898b") +
  labs(title = "The error terms are between the points and the fitted line"
       , subtitle = "Some errors are positive (above the line)\nand others are negative (below the line)"
       , caption = islr.text)

# display both plots side by side ----
grid.arrange(base.plot + 
               labs(title = "More years of education are associated\nwith higher annual incomes")
             , loess.plot
             , nrow = 1
             , ncol = 2)
```

![](README_files/figure-markdown_github/income%20and%20education-1.png)

In the formula *Y* = *f*(*x*)+*ϵ*, *f* represents the *systematic* information that *X* provides about *Y*. *ϵ* is a random *error term* that is both independent from *X* and has a mean of zero.

**In essence, statistical learning refers to a set of approaches for estimating *f***. In this chapter we outline some of the key theoretical concepts that arise in estimating *f*, as well as tools for evaluating the estimates obtained.

Why estimate f?
---------------

There are two main reasons that we may wish to estimate *f*: *prediction* and *inference*.

### Prediction

In many situations, a set of inputs *X* are readily available, but the output *Y* cannot be easily obtained. Since the error term averages to zero, we can predict *Y* in this setting using the following formula:

$$\\hat{Y} = \\hat{f}(X)$$

where $\\hat{f}$ represents our estimate for *f*, and $\\hat{Y}$ represents the resulting prediction for *Y*. In this setting, $\\hat{f}$ is often treated as a black box, in the sense that one is not typically concerned with the exact form of *f*, provided that it yields accurate predictions for *Y*.

The accuracy of $\\hat{Y}$ as a prediction for *Y* depends on two quantitines: *reducible error* and *irreducible error*.

In general, $\\hat{f}$ will not be a perfect estimate for *f*, and this inaccuracy will introduce some error. This error is reducible because we can potentially improve the accuracy $\\hat{f}$ by using the most appropriate statistical learning technique to estimate *f*.

But perfection is not possible. This is because *Y* is also a function of *ϵ* - which by definition cannot be predicted using *X*. The quantity *ϵ* may contain both unmeasured variables and variation that are useful in predicting *Y*.

Since we don’t measure them, *f* cannot use them for its prediction.Therefore, variability associated with *ϵ* also affects the accuracy of our predictions.

This is known as the irreducible error, because no matter how well we estimate *f*, we cannot reduce the error introduced by *ϵ*.

**The focus of this book is on techniques for estimating *f* with the aim of minimizing the reducible error.**

It is important to keep in mind that the irreducible error will always provide an upper bound on the accuracy of our prediction for *Y*. *This bound is almost always unknown in practice.*

### Inference

We are often interested in understanding the way that *Y* is affected as *X*1,...,*X*<sub>*p*</sub> change. In this situation we wish to estimate *f*, but our goal is not necessarily to make predictions for *Y*.

We instead want to understand the relationship between *X* and *Y*, or more specifically, to understand how *Y* changes as a function of *X*1,...,*X*<sub>*p*</sub>. Now $\\hat{f}$ cannot be treated as a black box, because we need to know its exact form.

The following questions now arise:

-   Which predictors are associated with the response?
    -   Identifying the few important predictors among a large set of possible variables can be extremely useful, depending on the application.
-   What is the relationship between the response and each predictor?

-   Can the relationship between *Y* and each predictor be adequately summarized using a linear equation, or is the relationship more complicated?

For instance, consider a company that is interested in conducting a direct-marketing campaign. The goal is to build a model that predicts how an individual will respond to the direct-marketing campaigin based on their demographics.

In this case, the demographic variables serve as predictors, and response to the marketing campaign (either pos- itive or negative) serves as the outcome. The company is not interested in obtaining a deep understanding of the relationships between each individual predictor and the response; instead, the company simply wants an accurate model to predict the response using the predictors.

In contrast, consider the [`Advertising` data visualization](https://github.com/cenuno/islr_notes/blob/master/02_statistical_learning/README_files/figure-markdown_github/examine%20advertising-1.png) from earlier. One may be interested in answering questions such as:

-   Which media contribute to sales?

-   Which media generate the biggest boost in sales?

-   How much increase in sales is associated with a given increase in TV advertising?

This situation falls into the inference paradigm.

Another example involves modeling the brand of a product that a customer might purchase based on variables such as price, store location, discount levels, competition price, and so forth.

In this situation one might really be most interested in **how each of the individual variables affects** the probability of purchase. For instance, what effect will changing the price of a product have on sales? This is an example of modeling for inference.

Finally, some modeling could be conducted both for prediction and inference. For example, in a real estate setting, one may seek to relate values of homes to inputs such as crime rate, zoning, distance from a river, air quality, schools, income level of community, size of houses, and so forth.

In this case one might be interested in how the individual input variables affect the prices—that is, *how much extra will a house be worth if it has a view of the river*? This is an **inference** problem.

Alternatively, one may simply be interested in predicting the value of a home given its characteristics: *is this house under- or over-valued*? This is a **prediction** problem.

Depending on whether our ultimate goal is prediction, inference, or a combination of the two, different methods for estimating *f* may be appropriate.

For example, *linear models* allow for relatively simple and interpretable inference, but may not yield as accurate predictions as some other approaches.

In contrast, some of the highly non-linear approaches that we discuss in the later chapters of this book can potentially provide quite accurate predictions for *Y*, but this comes at the expense of a less interpretable model for which inference is more challenging.

How Do We Estimate f?
---------------------

Throughout this book, we explore many linear and non-linear approaches for estimating *f*. However, these methods generally share certain characteristics.

We will always assume that we have observed a set of *n* different data points. These observations are called the *training data* because we will use these observations to train, or teach, our method how to estimate *f*.

Let *x*<sub>*i**j*</sub> represent the value of the *j*<sup>*t**h*</sup> predictor, or input, for observation *i*, where *i* = 1,2,...,*n* and *j* = 1,2,...,*p*. Correspondingly, let *y*<sub>*i*</sub> represent the response variable for the *i*<sup>*t**h*</sup> observation. Then our training data consist of

{(*x*<sub>1</sub>,*y*<sub>1</sub>),(*x*<sub>2</sub>,*y*<sub>2</sub>),...,(*x*<sub>*n*</sub>,*y*<sub>*n*</sub>)} where *x*<sub>*i*</sub> ={(*x*<sub>*i*1</sub>,*x*<sub>*i*2</sub>,...,*x*<sub>*i**p*</sub>)}**<sup>*T*</sup>.

Our goal is to apply a statistical learning method to the training data in order to estimate the unknown function *f*. In other words, we want to find a function *f* such that *Y* ≈ *f*(*X*) for any observation (*X*, *Y*).

Broadly speaking, most statistical learning methods for this task can be characterized as either parametric or non-parametric.

### Parametric Methods

Parametric methods involve a two-step model-based approach.

1.  Make an assumption about the functional form, or shape, of *f* (i.e. a linear model of *f*(*X*) = *β*<sub>0</sub> + *β*<sub>1</sub>*X*<sub>1</sub> + *β*<sub>2</sub>*X*<sub>2</sub> +... + *β*<sub>*p*</sub>*X*<sub>*p*</sub>).

2.  After a model has been selected, we need a procedure that uses the training data to *fit* or *train* the model. In the case of the linear model in step 1, we need to estimate the parameters *β*<sub>0</sub>,*β*<sub>1</sub>,...,*β*<sub>*p*</sub>. We want to find the values of these parameters such that *Y* ≈ *β*<sub>0</sub> + *β*<sub>1</sub>*X*<sub>1</sub> + *β*<sub>2</sub>*X*<sub>2</sub> +... + *β*<sub>*p*</sub>*X*<sub>*p*</sub>. A common approach for fitting linear models is the *ordinary least squares* approach.

The model-based approach just described is referred to as *parametric*; it reduces the problem of estimating *f* down to one of estimating a set of paramters.

Assuming a parametric form for *f* simplifies the problem of estimating *f* because it is generally much easier to estimate a set of parameters, such as *β*<sub>0</sub>,*β*<sub>1</sub>,...,*β*<sub>*p*</sub> in the linear model than it is to fit an entirely arbitrary function *f*.

The potential disadvantage of a parametric approach is that the model we choose will usually not match the true unknown form of *f*. If the chosen model is too far from the true *f*, then our estimate will be poor.

We can try to address this problem by choosing flexible models that can fit many different possible functional forms for *f*. But in general, fitting a more flexible model requires estimating a greater number of parameters.

These more complex models can lead to a phenomenon known as *overfitting* the data, which essentially means they follow the errors, or noise, too closely. Overfitting is an undesirable situation because the fit obtained will not yield accurate estimates of the response on new observations that were not part of the original training data set.

From the `Income` data set, a parametric approach to understanding `income` described by both `education` and `seniority` as a linear model would look like this:

*i**n**c**o**m**e* ≈ *β*<sub>0</sub> + *β*<sub>1</sub> \* *e**d**u**c**a**t**i**o**n* + *β*<sub>2</sub> \* *s**e**n**i**o**r**i**t**y*

Since we have assumed a linear relationship between the response and the two predictors, the entire fitting problem reduces to estimating *β*<sub>0</sub>,*β*<sub>1</sub>, and *β*<sub>2</sub>, which we do using least squares linear regression.

### Non-parametric methods

Non-parametric methods do not make explicit assumptions about the functional form of *f*. Instead they seek an estimate of *f* that gets as close to the data points as possible without being too rough or wiggly.

Such approaches can have a major advantage over parametric approaches: by avoiding the assumption of a particular functional form for *f*, they have the potential to accurately fit a wider range of possible shapes for *f*.

Any parametric approach brings with it the possibility that the functional form used to estimate *f* is very different from the true *f*, in which case the resulting model will not fit the data well.

In contrast, non-parametric approaches completely avoid this danger, since essentially no assumption about the form of *f* is made. But non-parametric approaches do suffer from a major disadvantage: since they do not reduce the problem of estimating *f* to a small number of parameters, a very large number of observations (far more than is typically needed for a parametric approach) is required in order to obtain an accurate estimate for *f*.

The Trade-Off Between Prediction Accuracy and Model Interpretability
--------------------------------------------------------------------

Session Info
------------

``` r
sessioninfo::session_info()
```

    ## ─ Session info ──────────────────────────────────────────────────────────
    ##  setting  value                       
    ##  version  R version 3.5.2 (2018-12-20)
    ##  os       macOS High Sierra 10.13.6   
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  ctype    en_US.UTF-8                 
    ##  tz       America/Chicago             
    ##  date     2019-01-27                  
    ## 
    ## ─ Packages ──────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.0   2017-04-11 [1] CRAN (R 3.5.0)
    ##  backports     1.1.3   2018-12-14 [1] CRAN (R 3.5.0)
    ##  bindr         0.1.1   2018-03-13 [1] CRAN (R 3.5.0)
    ##  bindrcpp      0.2.2   2018-03-29 [1] CRAN (R 3.5.0)
    ##  broom         0.5.1   2018-12-05 [1] CRAN (R 3.5.0)
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 3.5.0)
    ##  cli           1.0.1   2018-09-25 [1] CRAN (R 3.5.0)
    ##  colorspace    1.3-2   2016-12-14 [1] CRAN (R 3.5.0)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 3.5.1)
    ##  digest        0.6.18  2018-10-10 [1] CRAN (R 3.5.0)
    ##  dplyr       * 0.7.8   2018-11-10 [1] CRAN (R 3.5.0)
    ##  evaluate      0.12    2018-10-09 [1] CRAN (R 3.5.0)
    ##  forcats     * 0.3.0   2018-02-19 [1] CRAN (R 3.5.0)
    ##  generics      0.0.2   2018-11-29 [1] CRAN (R 3.5.0)
    ##  ggplot2     * 3.1.0   2018-10-25 [1] CRAN (R 3.5.0)
    ##  glue          1.3.0   2018-07-17 [1] CRAN (R 3.5.0)
    ##  gridExtra   * 2.3     2017-09-09 [1] CRAN (R 3.5.0)
    ##  gtable        0.2.0   2016-02-26 [1] CRAN (R 3.5.0)
    ##  haven         2.0.0   2018-11-22 [1] CRAN (R 3.5.0)
    ##  here        * 0.1     2017-05-28 [1] CRAN (R 3.5.0)
    ##  hms           0.4.2   2018-03-10 [1] CRAN (R 3.5.0)
    ##  htmltools     0.3.6   2017-04-28 [1] CRAN (R 3.5.0)
    ##  httr          1.4.0   2018-12-11 [1] CRAN (R 3.5.0)
    ##  jsonlite      1.6     2018-12-07 [1] CRAN (R 3.5.0)
    ##  knitr         1.21    2018-12-10 [1] CRAN (R 3.5.2)
    ##  labeling      0.3     2014-08-23 [1] CRAN (R 3.5.0)
    ##  lattice       0.20-38 2018-11-04 [1] CRAN (R 3.5.2)
    ##  lazyeval      0.2.1   2017-10-29 [1] CRAN (R 3.5.0)
    ##  lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.5.0)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.5.0)
    ##  modelr        0.1.2   2018-05-11 [1] CRAN (R 3.5.0)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.0)
    ##  nlme          3.1-137 2018-04-07 [1] CRAN (R 3.5.2)
    ##  pillar        1.3.1   2018-12-15 [1] CRAN (R 3.5.0)
    ##  pkgconfig     2.0.2   2018-08-16 [1] CRAN (R 3.5.0)
    ##  plyr          1.8.4   2016-06-08 [1] CRAN (R 3.5.0)
    ##  purrr       * 0.2.5   2018-05-29 [1] CRAN (R 3.5.0)
    ##  R6            2.3.0   2018-10-04 [1] CRAN (R 3.5.0)
    ##  Rcpp          1.0.0   2018-11-07 [1] CRAN (R 3.5.0)
    ##  readr       * 1.3.1   2018-12-21 [1] CRAN (R 3.5.0)
    ##  readxl        1.2.0   2018-12-19 [1] CRAN (R 3.5.0)
    ##  rlang         0.3.1   2019-01-08 [1] CRAN (R 3.5.2)
    ##  rmarkdown     1.11    2018-12-08 [1] CRAN (R 3.5.0)
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 3.5.0)
    ##  rstudioapi    0.9.0   2019-01-09 [1] CRAN (R 3.5.2)
    ##  rvest         0.3.2   2016-06-17 [1] CRAN (R 3.5.0)
    ##  scales      * 1.0.0   2018-08-09 [1] CRAN (R 3.5.0)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 3.5.0)
    ##  stringi       1.2.4   2018-07-20 [1] CRAN (R 3.5.0)
    ##  stringr     * 1.3.1   2018-05-10 [1] CRAN (R 3.5.0)
    ##  tibble      * 2.0.1   2019-01-12 [1] CRAN (R 3.5.2)
    ##  tidyr       * 0.8.2   2018-10-28 [1] CRAN (R 3.5.0)
    ##  tidyselect    0.2.5   2018-10-11 [1] CRAN (R 3.5.0)
    ##  tidyverse   * 1.2.1   2017-11-14 [1] CRAN (R 3.5.0)
    ##  withr         2.1.2   2018-03-15 [1] CRAN (R 3.5.0)
    ##  xfun          0.4     2018-10-23 [1] CRAN (R 3.5.0)
    ##  xml2          1.2.0   2018-01-24 [1] CRAN (R 3.5.0)
    ##  yaml          2.2.0   2018-07-25 [1] CRAN (R 3.5.0)
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.5/Resources/library
