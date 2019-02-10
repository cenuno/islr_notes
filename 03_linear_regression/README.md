Linear Regression
================
Cristian E. Nuno
February 09, 2019

-   [Simple Linear Regression](#simple-linear-regression)
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
    ##  date     2019-02-09                  
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
