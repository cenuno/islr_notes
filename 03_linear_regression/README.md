Linear Regression
================
Cristian E. Nuno
February 11, 2019

-   [Simple Linear Regression](#simple-linear-regression)
    -   [Estimating the Coefficients](#estimating-the-coefficients)
    -   [Assessing the Accuracy of the Coefficient Estimates](#assessing-the-accuracy-of-the-coefficient-estimates)
-   [Session Info](#session-info)

``` r
# load necessary packages -----
library(broom)
library(gridExtra)
library(here)
library(ISLR)
library(mgcv)
library(scales)
library(splines)
library(tidyverse)

# load necessary functions ----
source(here("00_functions", "01_build_single_models.R"))

# load necessary objects ----
my.theme <- 
  theme_minimal() + 
  theme(panel.grid = element_blank()
        , plot.caption = element_text(size = 5))

islr.text <-
  "Source: ISLR (http://www-bcf.usc.edu/~gareth/ISL/data.html) | made by @cenuno_"

wage.text <- "Source: Wage and other data for a group of 3000 male workers in the Mid-Atlantic region | made by @cenuno_"

chapter.text <-
  "Source: ISLR, Chapter 3 | made by @cenuno_"

bright.blue   <- "#1e5efb"
bright.yellow <- "#edd830"

# set dpi for all chunks ----
knitr::opts_chunk$set(dpi = 300)
```

This chapter is about *linear regression*, a very simple approach for supervised learning. In particular, **linear regression is a useful tool for predicting a quantitative response**.

Linear regression has been around for a long time and is the topic of innumerable textbooks. Though it may seem somewhat dull compared to some of the more modern statistical learning approaches described in later chapters of this book, linear regression is still a useful and widely used statistical learning method.

Moreover, it serves as a good jumping-off point for newer approaches: as we will see in later chapters, many fancy statistical learning approaches can be seen as generalizations or extensions of linear regression. *Consequently, the importance of having a good understanding of linear regression before studying more complex learning methods cannot be overstated*.

In this chapter, we review some of the key ideas underlying the linear regression model, as well as the least squares approach that is most commonly used to fit this model.

Recall the `Advertising` data from Chapter 2.

![](https://github.com/cenuno/islr_notes/raw/master/02_statistical_learning/README_files/figure-markdown_github/examine%20advertising-1.png)

Suppose that in our role as statistical consultants we are asked to suggest, on the basis of this data, a marketing plan for next year that will result in high product sales. What information would be useful in order to provide such a recommendation?

Here are a few important questions that we might seek to address:

1.  Is there a relationship between advertising budget and sales?
    -   Our first goal should be to determine whether the data provide evidence of an association between advertising expenditure and sales. *If the evidence is weak, then one might argue that no money should be spent on advertising*!
2.  How strong is the relationship between advertising budget and sales?
    -   Assuming that there is a relationship between advertising and sales, we would like to know the strength of this relationship.
    -   **In other words, given a certain advertising budget, can we predict sales with a high level of accuracy?** This would be a strong relationship.
    -   Or is a prediction of sales based on advertising expenditure only slightly better than a random guess? This would be a weak relationship.
3.  Which media contribute to sales?
    -   Do all three media—TV, radio, and newspaper—contribute to sales, or do just one or two of the media contribute?
    -   To answer this question, we must *find a way to separate out the individual effects of each medium when we have spent money on all three media*.
4.  How accurately can we estimate the effect of each medium on sales?
    -   For every dollar spent on advertising in a particular medium, by what amount will sales increase?
    -   How accurately can we predict this amount of increase?
5.  How accurately can we predict future sales?
    -   For any given level of television, radio, or newspaper advertising, what is our prediction for sales, and what is the accuracy of this prediction?
6.  Is the relationship linear?
    -   If there is approximately a straight-line relationship between advertising expenditure in the various media and sales, then linear regression is an appropriate tool.
    -   If not, then it may still be possible to transform the predictor or the response so that linear regression can be used.
7.  Is there synergy among the advertising media?
    -   Perhaps spending $50,000 on television advertising and $50,000 on radio advertising results in more sales than allocating $100,000 to either television or radio individually.
    -   In marketing, this is known as a synergy effect, while in statistics it is called an *interaction effect*.

It turns out linear regression can be used to answer each of these questions. We will first discuss all of these questions in a general context, and then return to them in this specific context in Section 3.4.

Simple Linear Regression
------------------------

Simple linear regression lives up to its name: it is a very straightforward approach for predicting a quantitative response *Y* on the basis of a single predictor variable *X*. It assumes that there is approximately a linear relationship between *X* and *Y*.

Mathematically, we can write this linear relationship as:

*Y* ≈ *β*<sub>0</sub> + *β*<sub>1</sub>*X*,

where ≈ is read as "approximately modeled as" and saying we are regressing *Y* onto *X*.

For example, *X* may represent `TV` advertising and *Y* may represent `sales`. Then we can regress `sales` onto `TV` by fitting the model:

*s**a**l**e**s* ≈ *β*<sub>0</sub> + *β*<sub>1</sub> \* *T**V*.

*β*<sub>0</sub> and *β*<sub>1</sub> are two unknown constants that represent the *intercept* and *slope* terms in the linear model. Together, *β*<sub>0</sub> and *β*<sub>1</sub> are known as the model coefficients or parameters.

Once we have used our training data to produce estimates $\\hat{β}\_0$ and $\\hat{β}\_1$ for the model coefficients, we can predict future sales on the basis of a particular value of TV advertising by computing:

$\\hat{y} = \\hat{β}\_0 + \\hat{β}\_1x$,

where $\\hat{y}$ indicates a prediction of *Y* on the basis of *X* = *x*. Here we use a hat symbol, *ˆ* , to denote the estimated value for an unknown parameter or coefficient, or to denote the predicted value of the response.

### Estimating the Coefficients

In practice, *β*<sub>0</sub> and *β*<sub>1</sub> are unknown. So before we can use *Y* ≈ *β*<sub>0</sub> + *β*<sub>1</sub>*X* to make predictions, we must use data to estimate the coefficients. Let

(*x*<sub>1</sub>, *y*<sub>1</sub>), (*x*<sub>2</sub>, *y*<sub>2</sub>),..., (*x*<sub>*n*</sub>, *y*<sub>*n*</sub>)

represent *n* observation pairs, each of which consists of a measurement of *X* and a measurement of *Y*. In the `Advertising` example, this data set consists of the `TV` advertising budget and product `sales` in *n* = 200 different markets.

Our goal is to obtain coefficient estimates $\\hat{β}\_0$ and $\\hat{β}\_1$ such that the linear model fits the available data well: that is, so that $y\_i ≈ \\hat{\\beta}\_0 + \\hat{β}\_1 \* x\_i$ for *i* = 1, ..., *n*.

In other words, we want to find an intercept $\\hat{β}\_0$ and a slope $\\hat{β}\_1$ such that the resulting line is as close as possible to the *n* = 200 data points. There are a number of ways of *measuring closeness*. **However, by far the most common approach involves minimizing the least squares criterion**, and we take that approach in this chapter.

``` r
# load necessary data ----
df <- 
  read_csv(here("00_raw_data", "advertising.csv")) %>%
  # drop row number column
  select(-X1)

# regress TV onto sales ----
mod <- 
  lm(sales ~ TV, data = df)

# transform lm object into tibble ----
mod.tidy <- augment(mod)

# visualize -----
df %>%
  ggplot(aes(x = TV, y = sales)) +
  geom_point(color = bright.blue) +
  geom_line(data = mod.tidy
            , aes(x = TV, y = .fitted)
            , color = bright.yellow
            , size = 1.25) +
  geom_segment(data = mod.tidy
               , aes(xend = TV, yend = .fitted)
               , color = "gray65") +
  scale_x_continuous(name = "TV budget (in thousands of dollars)", labels = dollar) +
  scale_y_continuous(name = "Sales (in thousands of units)"
                     , limits = c(0, 30)) +
  labs(title = "Linear fit captures the essence of the relationship between sales and TV budget\nalthough it is somewhat deficient on the left-hand side of the plot"
       , subtitle = "Each blue point represents the data that exist between TV and sales;\nThe yellow line is the slope that minimizes the sum of the squared errors;\nEach gray line represents an error from the observed sales value and the predicted value"
       , caption = islr.text) +
  my.theme
```

![](README_files/figure-markdown_github/first%20model-1.png)

Let $\\hat{y}\_i$ = $\\hat{β}\_0$ + $\\hat{β}\_1x\_i$ be the prediction for *Y* based on the *i*<sub>*t**h*</sub> value of *X*. Then *e*<sub>*i*</sub> = *y*<sub>*i*</sub> − $\\hat{y}\_i$ represents the *i*<sub>*t**h*</sub> residual—this is the difference between the *i*<sub>*t**h*</sub> observed response value and the *i*<sub>*t**h*</sub> response value that is predicted by our linear model.

We define the **residual sum of squares (RSS)** as:

*R**S**S* = *e*<sub>1</sub><sup>2</sup> + *e*<sub>2</sub><sup>2</sup> + · · · + *e*<sub>*n*</sub><sup>2</sup>,

or equivalently as:

$RSS = (y\_1 − \\hat{β}\_0 − \\hat{β}\_1x\_1)^2 + (y\_2 − \\hat{β}\_0 − \\hat{β}\_1x\_2)^2 + ... + (y\_n − \\hat{β}\_0 − \\hat{β}\_1x\_n)^2$

The least squares approach chooses \_0 and \_1 to minimize the RSS. Using some calculus, one can show that the minimizers are:

$$\\hat{\\beta}\_1 = \\frac{\\sum^n\_{i = 1}(x\_i - \\bar{x})(y\_i - \\bar{y})}{\\sum^n\_{i = 1}(x\_i - \\bar{x})^2}$$

$$\\hat{\\beta}\_0 = \\bar{y} - \\hat{\\beta}\_1\\bar{x}$$
,

where
$$\\bar{y} \\equiv \\frac{1}{n}\\sum^n\_{i = 1}y\_i$$
 and
$$\\bar{x} \\equiv \\frac{1}{n}\\sum^n\_{i = 1}x\_i$$
 are the sample means. In other words, the equations for $\\hat{\\beta}\_0$ and $\\hat{\\beta}\_1$ define the least squares coefficient estimates for simple linear regression.

The linear model up above displays the simple linear regression fit to the `Advertising` data, where \_0 = 7.03 and \_1 = 0.04754. In other words, according to this approximation, an additional $1,000 spent on TV advertising is associated with with selling an approximately 47.5 additional units of the product.

### Assessing the Accuracy of the Coefficient Estimates

Recall from Chapter 2 that we assume that the true relationship between *X* and *Y* takes the form *Y* = *f*(*X*) + *ϵ* for some unknown function *f*, where *ϵ* is a mean-zero random error term. If *f* is to be approximated by a linear function, then we can write this relationship as:

*Y* = *β*<sub>0</sub> + *β*<sub>1</sub>*X* + *ϵ*
.

Here *β*<sub>0</sub> is the intercept term—that is, the expected value of *Y* when *X* = 0, and *β*<sub>1</sub> is the slope—the average increase in *Y* associated with a one-unit increase in *X*.

The error term is a catch-all for what we miss with this simple model: the true relationship is probably not linear, there may be other variables that cause variation in *Y*, and there may be measurement error. We typically assume that the error term is independent of *X*.

*Y* = *β*<sub>0</sub> + *β*<sub>1</sub>*X* + *ϵ*
 defines the population regression line, which is the best linear approximation to the true relationship between *X* and *Y*. The assumption of linearity is often a useful working model. However, despite what many textbooks might tell us, we seldom believe that the true relationship is linear.

The least squares regression coefficient estimates for \_0 and \_1 characterize the least squares line.

At first glance, the difference between the population regression line and the least squares line may seem subtle and confusing. We only have one data set, and so what does it mean that two different lines describe the relationship between the predictor and the response?

Fundamentally, the **concept of these two lines is a natural extension of the standard statistical approach of using information from a sample to estimate characteristics of a large population**. For example, suppose that we are interested in knowing the population mean *μ* of some random variable *Y*.

Unfortunately, *μ* is unknown, but we do have access to *n* observations from *Y*, which we can write as *y*<sub>1</sub>, ..., *y*<sub>*n*</sub>, and which we can use to estimate *μ*. A reasonable estimate is $\\hat{\\mu}$ = $\\bar{y}$, where $\\bar{y}$ = $\\frac{1}{n}\\sum^n\_{i = 1}y\_i$ is the sample mean.

**The sample mean and the population mean are different, but in general the sample mean will provide a good estimate of the population mean**.

In the same way, the unknown coefficients *β*<sub>0</sub> and *β*<sub>1</sub> in linear regression define the population regression line. We seek to estimate these unknown coefficients using $\\hat{β}\_0$ and $\\hat{β}\_1$. These coefficient estimates define the least squares line.

The analogy between linear regression and estimation of the mean of a random variable is an apt one based on the concept of *bias*. If we use the sample mean $\\hat{\\mu|$ to estimate *μ*, this estimate is unbiased, in the sense that on average, we expect $\\hat{\\mu}$ to equal *μ*.

What exactly does this mean?

It means that on the basis of one particular set of observations *y*<sub>1</sub>, . . . , *y*<sub>*n*</sub>, $\\hat{\\mu}$ might overestimate *μ*, and on the basis of another set of observations, $\\hat{\\mu}$ might underestimate *μ*. But if we could average a huge number of estimates of *μ* obtained from a huge number of sets of observations, then this average would *exactly* equal *μ*.

Hence, an unbiased estimator does not *systematically* over or under-estimate the true parameter. The property of unbiasedness holds for the least squares coefficient estimates as well: if we estimate *β*<sub>0</sub> and *β*<sub>1</sub> on the basis of a particular data set, then our estimates won’t be exactly equal to *β*<sub>0</sub> and *β*<sub>1</sub>. But if we could average the estimates obtained over a huge number of data sets, then the average of these estimates would be spot on!

We continue the analogy with the estimation of the population mean *μ* of a random variable *Y*. A natural question is as follows: how accurate is the sample mean $\\hat{\\mu}$ as an estimate of *μ*?

We have established that the average of $\\hat{\\mu}$’s over many data sets will be very close to *μ*, but that a single estimate $\\hat{\\mu}$ may be a substantial underestimate or overestimate of *μ*. How far off will that single estimate of $\\hat{\\mu}$ be?

In general, we answer this question by computing the *standard error* of $\\hat{\\mu}$, written as $SE(\\hat{\\mu})$. We have the well-known formula:

$$Var(\\hat{\\mu}) = SE(\\hat{\\mu})^2 = \\frac{\\sigma^2}{n}$$
,

where *σ* is the standard deviation of each of the realizations *y*<sub>*i*</sub> of *Y* (this formula holds provided that the *n* observations are uncorrelated). Roughly speaking, the standard error tells us the average amount that this estimate $\\hat{\\mu}$ differs from the actual value of *μ*.

The equation for *S**E* also tells us how this deviation shrinks with *n*—**the more observations we have, the smaller the standard error of $\\hat{\\mu}$**. In a similar vein, we can wonder how close $\\hat{β}\_0$ and $\\hat{β}\_1$ are to the true values *β*<sub>0</sub> and *β*<sub>1</sub>.

To compute the standard errors associated with $\\hat{β}\_0$ and $\\hat{β}\_1$, we use the following formulas:

$$SE(\\hat{\\beta\_0})^2 = \\sigma^2\\bigg\[\\frac{1}{n} + \\frac{\\bar{x}^2}{\\sum^n\_{i = 1}(x\_i - \\bar{x})^2}\\bigg\]$$
, and

$$SE(\\hat{\\beta\_1})^2 = \\frac{\\sigma^2}{\\sum^n\_{i = 1}(x\_i - \\bar{x})^2}$$
,

where *σ*<sup>2</sup> = *V**a**r*(*ϵ*). For these formulas to be strictly valid, we need to assume that the errors *ϵ*<sub>*i*</sub> for each observation are uncorrelated with common variance *σ*<sup>2</sup>. This is clearly not true when regressing `TV` onto `sales`, but the formula still turns out to be a good approximation.

Notice in the formula that $SE(\\hat{β}\_1)$ is smaller when the *x*<sub>*i*</sub> are more spread out; intuitively we have more *leverage* to estimate a slope when this is the case. We also see that $SE(\\hat{\\beta}\_0)$ would be the same as $SE(\\hat{\\mu})$ if $\\bar{x}$ were zero (in which case $\\hat{β}\_0$ would be equal to $\\bar{y}$).

In general, *σ*<sup>2</sup> is not known, but can be estimated from the data. The estimate of *σ* is known as the *residual standard error*, and is given by the formula:

$$RSE = \\sqrt{\\frac{RSS}{n - 2}}$$
.

Strictly speaking, when *σ* is estimated from the data we should write $\\hat{SE}(\\hat{β}\_1)$ to indicate that an estimate has been made, but for simplicity of notation we will drop this extra "hat".

Standard errors can be used to compute *confidence intervals*. **A 95% confidence interval is defined as a range of values such that with 95% probability, the range will contain the true unknown value of the parameter**.

The range is defined in terms of lower and upper limits computed from the sample of data. For linear regression, the 95% confidence interval for *β*<sub>1</sub> approximately takes the form:

$$\\beta\_1 \\pm 2 \* SE(\\hat{\\beta}\_1)$$
.

That is, there is approximately a 95% chance that the interval

$$\\bigg\[\\beta\_1 - 2 \* SE(\\hat{\\beta}\_1), \\beta\_1 + 2 \* SE(\\hat{\\beta}\_1)\\bigg\]$$

will contain the true value of *β*<sub>1</sub> (relies on the assumption that the errors are Gaussian and 2 should be replaced with the 97.5% quantile of a t-distribution with *n* − 2 degrees of freedom).

Similarly, a confidence interval for β0 approximately takes the form:

$$\\beta\_0 \\pm 2 \* SE(\\hat{\\beta}\_0)$$
.

``` r
# source the standard_error() function ----
source(here("00_functions", "02_standard_error.R"))

# source the confidence_interval() function ----
source(here("00_functions", "03_confidence_interval.R"))

# calculate SE for the both coeffiencents ----
se <-
  c(TRUE, FALSE) %>%
  set_names(c("intercept", "input")) %>%
  map(.f = ~ standard_error(model = mod
                            , input = "TV"
                            , intercept = .x))

# calculate lower and upper bounds of the coefficients ----
confidence.interval <-
  se %>%
  map2(.y = coefficients(mod)
       , .f = ~ confidence_interval(coeff = .y
                                    , std.error = .x
                                    , level = 0.95
                                    , df = mod$df.residual))

# store estimates of both intercept and input ----
response.unit <- 1000

intercept.estimates <-
  confidence.interval$intercept %>%
  round(digits = 2) %>%
  "*"(response.unit) %>%
  prettyNum(big.mark = ",")

input.estimates <-
  confidence.interval$input %>%
  round(digits = 3) %>%
  "*"(response.unit)
```

In the case of the advertising data, the 95% confidence interval for *β*<sub>0</sub> is \[6.13, 7.935\] and the 95 % confidence interval for *β*<sub>1</sub> is \[0.042, 0.053\]. The interpretation of the confidence interval requires that we understand the units of the input and response variable.

Therefore, we can conclude that in the absence of any advertising, sales will, on average, fall somewhere between 6,130 and 7,940 units. Furthermore, for each $1,000 increase in television advertising, there will be an average increase in sales of between 42 and 53 units.

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
    ##  date     2019-02-11                  
    ## 
    ## ─ Packages ──────────────────────────────────────────────────────────────
    ##  package     * version date       lib source        
    ##  assertthat    0.2.0   2017-04-11 [1] CRAN (R 3.5.0)
    ##  backports     1.1.3   2018-12-14 [1] CRAN (R 3.5.0)
    ##  bindr         0.1.1   2018-03-13 [1] CRAN (R 3.5.0)
    ##  bindrcpp      0.2.2   2018-03-29 [1] CRAN (R 3.5.0)
    ##  broom       * 0.5.1   2018-12-05 [1] CRAN (R 3.5.0)
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
    ##  ISLR        * 1.2     2017-10-20 [1] CRAN (R 3.5.0)
    ##  jsonlite      1.6     2018-12-07 [1] CRAN (R 3.5.0)
    ##  knitr         1.21    2018-12-10 [1] CRAN (R 3.5.2)
    ##  labeling      0.3     2014-08-23 [1] CRAN (R 3.5.0)
    ##  lattice       0.20-38 2018-11-04 [1] CRAN (R 3.5.2)
    ##  lazyeval      0.2.1   2017-10-29 [1] CRAN (R 3.5.0)
    ##  lubridate     1.7.4   2018-04-11 [1] CRAN (R 3.5.0)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 3.5.0)
    ##  Matrix        1.2-15  2018-11-01 [1] CRAN (R 3.5.2)
    ##  mgcv        * 1.8-26  2018-11-21 [1] CRAN (R 3.5.2)
    ##  modelr        0.1.2   2018-05-11 [1] CRAN (R 3.5.0)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 3.5.0)
    ##  nlme        * 3.1-137 2018-04-07 [1] CRAN (R 3.5.2)
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
