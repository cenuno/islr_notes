Introduction
================
Cristian E. Nuno
February 03, 2019

-   [Introduction](#introduction)
    -   [Wage Data](#wage-data)
    -   [Stock Market Data](#stock-market-data)
    -   [Gene Expression Data](#gene-expression-data)
    -   [A Brief History of Statistical Learning](#a-brief-history-of-statistical-learning)
    -   [This Book](#this-book)
    -   [Who Should Read This Book](#who-should-read-this-book)
    -   [Notation and Simple Matrix Algebra](#notation-and-simple-matrix-algebra)
    -   [Organization of This Book](#organization-of-this-book)
    -   [Session Info](#session-info)

``` r
# load necessary packages ----
library(gridExtra)  # misc. functions for 'grid' graphics
library(ISLR)       # data for ISLR examples
library(MASS)       # Venables & Ripley's 'Modern Applied Statistics with s'
library(scales)     # scale functions for visualizations
library(tidyverse)  # data science packages

# load necessary objects ----
wage.text <- "Source: Wage and other data for a group of 3000 male workers in the Mid-Atlantic region | made by @cenuno_"

stock.text <- "Source: Daily percentage returns for the S&P 500 stock index between 2001 and 2005 | made by @cenuno_"

gene.text <- "Source: NCI microarray data that contains cancer type and expression levels on 6,830 genes from 64 cancer cell lines | made by @cenuno_"

my.theme <- 
  theme_minimal() + 
  theme(panel.grid = element_blank()
        , plot.caption = element_text(size = 5))

# set dpi for all chunks ----
knitr::opts_chunk$set(dpi = 300)
```

Introduction
============

-   Statistical learning = understanding data
    -   Supervised: Building a statistical model for predicting, or estimating, an output based on one or more inputs
    -   Unsupervised: There are inputs but no supervising output

Wage Data
---------

In particular, we wish to understand the association between an employee’s age and education, as well as the calendar year, on his wage.

The [`Wage`](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Wage) data set involves predicting a **continuous** or **quantitative** output value, which is referred to as a **regression** problem.

``` r
# hourly wage described by age ----
Wage %>%
  ggplot(aes(x = age, y = wage)) +
  geom_point(color = "gray") +
  geom_smooth(se = FALSE, color = "navy") +
  xlab("Age") +
  ylab("Hourly wage") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Whiles wages typically rise with age, they fall after age 60"
       , caption = wage.text) +
  my.theme
```

![](README_files/figure-markdown_github/wage%20plots-1.png)

``` r
# hourly wage described by year ----
Wage %>%
  ggplot(aes(x = year, y = wage)) +
  geom_point(color = "gray") +
  geom_smooth(se = FALSE, method = "loess", color = "navy") +
  xlab("Year") +
  ylab("Hourly wage") +
  scale_y_continuous(labels = dollar) +
  labs(title = "On average, hourly wage rises slighly over time"
       , caption = wage.text) +
  my.theme
```

![](README_files/figure-markdown_github/wage%20plots-2.png)

``` r
# hourly wage described by education ----
Wage %>%
  ggplot(aes(x = education, y = wage, fill = education)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_brewer(type = "qual", palette = "Accent") +
  xlab("Education level") +
  ylab("Hourly wage") +
  scale_y_continuous(labels = dollar) +
  labs(title = "On average, hourly wage rises with higher levels of educational attainment"
       , caption = wage.text) +
  my.theme
```

![](README_files/figure-markdown_github/wage%20plots-3.png)

Stock Market Data
-----------------

When we wish to predict a non-numerical value - a **categorical** or **qualitative** output, this is known as a **classification** problem.

Let's examine the [`Smarket`](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Smarket) data set, that contains the daily movements in the [Standard & Poor’s 500 (S&P)](https://www.standardandpoors.com/en_US/web/guest/home) stock index over a 5-year period between 2001 and 2005.

The goal is to predict whether the index will *increase* or *decrease* on a given day using the past 5 days’ percentage changes in the index. Here the statistical learning problem does not involve predicting a numerical value.

Instead it involves predicting whether a given day’s stock market performance will fall into the `Up` bucket or the `Down` bucket. This is known as a **classification** problem.

The three plots below look almost identical, suggesting that there is no simple strategy for using yesterday’s movement in the S&P to predict today’s returns.

``` r
# produce a boxplot per lag ----
LagPlot <- function(col) {
  # only show caption with Lag3 plot
  if (col == "Lag3") {
    caption <- stock.text
  } else {
    caption <- ""
  }
  
  Smarket %>%
    ggplot(aes(x = Direction, y = get(col), fill = Direction)) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_manual(values = c("#fd3504", "#04ccfd")) +
    ylab("Percentage change in S&P") +
    xlab("Today's direction") +
    labs(caption = caption) +
    my.theme +
    theme(plot.title = element_text(hjust = 0.5))
    
}

# store plots in a list ----
stock.plots <- 
  paste0("Lag", 1:3) %>%
  set_names() %>%
  map2(.y = c("Yesterday", paste0(c("Two", "Three"), " day's previous"))
       , .f = ~ LagPlot(.x) + labs(title = .y))

# have all 3 plots in one plot space ----
grid.arrange(stock.plots$Lag1, stock.plots$Lag2, stock.plots$Lag3, ncol = 3)
```

![](README_files/figure-markdown_github/stock%20plots-1.png)

Now let's fit a [quadratic discriminant analysis model](http://uc-r.github.io/discriminant_analysis) to the subset of the `Smarket` data corresponding to the 2001–2004 time period, and predicted the probability of a stock market decrease using the 2005 data.

``` r
# split data into training and testing sets ----
train <- Smarket %>% filter(Year %in% 2001:2004 )

test <- Smarket %>% filter(Year == 2005)

# fit a quadratic discriminatory analysis model ----
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = train)

# display model 
qda.fit
```

    ## Call:
    ## qda(Direction ~ Lag1 + Lag2, data = train)
    ## 
    ## Prior probabilities of groups:
    ##     Down       Up 
    ## 0.491984 0.508016 
    ## 
    ## Group means:
    ##             Lag1        Lag2
    ## Down  0.04279022  0.03389409
    ## Up   -0.03954635 -0.03132544

``` r
# predict() is used to predict the % of market direction given value of the predictors
qda.predict <- predict(qda.fit, test)

# cross-reference the predicted Direction values with the actual values ----
#
# note: of the 50 predicted values of Down, only 30 were correctly identified
#
# note: of the 202 predicted values of Up, only 121 were correctly identified
#
table(qda.predict$class, test$Direction)
```

    ##       
    ##        Down  Up
    ##   Down   30  20
    ##   Up     81 121

``` r
# True positive rate ----
# i.e. of all the predicted values, how many were actually correct
#
# sum(30, 121) / nrow(test)
mean(qda.predict$class == test$Direction)
```

    ## [1] 0.5992063

``` r
# visualize predicted probabilities by Direction ----
qda.predict$posterior %>%
  # transfrom matrix to tibble
  as_tibble() %>%
  # transfrom tibble from wide to long
  gather(key = "direction", value = "pred_prop") %>%
  ggplot(aes(x = direction, y = pred_prop, fill = direction)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("#fd3504", "#04ccfd")) +
  scale_y_continuous(labels = percent) +
  xlab("Today's direction") +
  ylab("Posterior probabilities") +
  labs(title = "Posterior probability that the corresponding observations\nwill or will not classify the correct stock market direction"
       , caption = stock.text) +
  my.theme
```

![](README_files/figure-markdown_github/stock%20qda-1.png)

Gene Expression Data
--------------------

The previous two applications illustrate data sets with both input and output variables.

There are situations in which we *only observe input variables, with no corresponding output*.

-   Ex: market setting uses demographic data to understand which types of current customers are similar to one another by **grouping individuals according to their observed characteristics**. This is known as a *clustering* problem.

Let's examine the [`NCI60`](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/NCI60) data set and how to visualize it using [principal component analysis](https://uc-r.github.io/pca).

``` r
# apply PCA ----
# note: standardize values by centering and scaling
NCI60$pca.data <- 
  prcomp(NCI60$data
         , center = TRUE
         , scale. = TRUE)

# create color schema for 4 types of clusters ---
cluster.color.schema <-
  map2_chr(.x = NCI60$pca.data$x[, "PC1"]
           , .y = NCI60$pca.data$x[, "PC2"]
           , .f = ~
             if (.x < -40 & .y > -20) {
               "red"
             } else if (.x < 0 & .y < -20) {
               "#0077cc"
             } else if (.x > -40 & .y < 40 & .x < 50) {
               "royalblue4"
             } else if (.x > 25 & .y < 5) {
               "green3"
             })

# visualize cluster plot ----
NCI60$pca.data$x %>%
  as_tibble() %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = cluster.color.schema) +
  my.theme +
  labs(title = "PCA makes it possible to visualize the 64 cancer cell lines and their\n6,830 measurements in a 2-dimensional space"
       , caption = gene.text)
```

![](README_files/figure-markdown_github/gene%20expression-1.png)

``` r
# store color schema as seen in fig 1.4 on page 5 ----
# note: overwriting the cancer.type.color.schema object
cancer.type.color.schema <-
  map_chr(.x = NCI60$labs %>% set_names()
          , .f = ~
            if (.x == "CNS"){
              "orange"
            } else if (.x == "RENAL"){
              "purple"
            } else if (.x == "BREAST"){
              "red"
            } else if (.x == "NSCLC"){
              "royalblue4"
            } else if (.x == "UNKNOWN"){
              "red"
            } else if (.x == "OVARIAN"){
              "royalblue4"
            } else if (.x == "MELANOMA"){
              "#0077cc"
            } else if (.x == "PROSTATE"){
              "purple"
            } else if (.x == "LEUKEMIA"){
              "green3"
            } else if (.x == "K562B-repro"){
              "green3"
            } else if (.x == "K562A-repro"){
              "palegreen"
            } else if (.x == "COLON"){
              "yellow"
            } else if (.x == "MCF7A-repro"){
              "darkseagreen3"
            } else if (.x == "MCF7D-repro"){
              "darkseagreen2"
            })

cancer.type.shape <-
  map_dbl(.x = NCI60$labs %>% set_names()
         , .f = ~
           if (.x == "CNS"){
             17
           } else if (.x == "RENAL"){
             19
           }else if (.x == "BREAST"){
             19
           } else if (.x == "NSCLC"){
             17
           } else if (.x == "UNKNOWN"){
             17
           } else if (.x == "OVARIAN"){
             18
           } else if (.x == "MELANOMA"){
             19
           } else if (.x == "PROSTATE"){
             15
           } else if (.x == "LEUKEMIA"){
             17
           } else if (.x == "K562B-repro"){
             19
           } else if (.x == "K562A-repro"){
             15
           } else if (.x == "COLON"){
             18
           } else if (.x == "MCF7A-repro"){
             18
           } else if (.x == "MCF7D-repro"){
             15
           })

# visualize data space by cancer type ----
NCI60$pca.data$x %>%
  as_tibble() %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = cancer.type.color.schema
             , shape = cancer.type.shape) +
  my.theme +
  labs(title = "Now each color and shape represents one of the 14 different types of cancer"
       , caption = gene.text)
```

![](README_files/figure-markdown_github/gene%20expression-2.png)

There is clear evidence that cell lines with the same cancer type tend to be located near each other in this two-dimensional representation. In addition, even though the cancer infor- mation was not used to produce the left-hand panel, the clustering obtained does bear some resemblance to some of the actual cancer types observed in the right-hand panel.

A Brief History of Statistical Learning
---------------------------------------

At the beginning of the nineteenth century, [Legendre and Gauss published papers on the method of least squares](http://econ.ucsb.edu/~doug/240a/The%20Discovery%20of%20Statistical%20Regression.htm), which implemented the earliest form of what is now known as linear regression. Linear regression is used for predicting [quantitative values](https://en.wikipedia.org/wiki/Quantitative), such as an individual’s salary.

In order to predict [qualitative values](https://en.wikipedia.org/wiki/Qualitative), such as whether a patient survives or dies, or whether the stock market increases or decreases, [Fisher](https://en.wikipedia.org/wiki/Ronald_Fisher) proposed [linear discriminant analysis](https://en.wikipedia.org/wiki/Linear_discriminant_analysis) in 1936.

In the 1940s, various authors put forth an alternative approach, [logistic regression](https://en.wikipedia.org/wiki/Logistic_regression). In the early 1970s, [Nelder](https://en.wikipedia.org/wiki/John_Nelder) and [Wedderburn](https://en.wikipedia.org/wiki/Robert_Wedderburn_(statistician)) coined the term [generalized linear models](https://en.wikipedia.org/wiki/Generalized_linear_model) for an entire class of statistical learning methods that include both linear and logistic regression as special cases.

By the 1980s, [computing technology had finally improved sufficiently](https://gizmodo.com/the-trillion-fold-increase-in-computing-power-visualiz-1706676799) that non-linear methods were no longer computationally prohibitive.

In mid 1980s [Breiman](https://en.wikipedia.org/wiki/Leo_Breiman), [Friedman](https://en.wikipedia.org/wiki/Jerome_H._Friedman), [Olshen](http://statweb.stanford.edu/~olshen/) and [Stone](https://statistics.berkeley.edu/people/chuck-stone) introduced [classification and regression trees](https://rafalab.github.io/pages/649/section-11.pdf), and were among the first to demonstrate the power of a detailed practical implementation of a method, including [cross-validation](https://en.wikipedia.org/wiki/Cross-validation_(statistics)) for [model selection](https://en.wikipedia.org/wiki/Model_selection).

[Hastie](https://en.wikipedia.org/wiki/Trevor_Hastie) and [Tibshirani](https://en.wikipedia.org/wiki/Robert_Tibshirani) coined the term [generalized additive models](https://en.wikipedia.org/wiki/Generalized_additive_model) in 1986 for a class of non-linear extensions to generalized linear models, and also provided a practical software implementation.

This Book
---------

[The Elements of Statistical Learning (ESL)](https://web.stanford.edu/~hastie/Papers/ESLII.pdf) by Hastie, Tibshirani, and Friedman was first published in 2001. Since that time, it has become an important reference on the fundamentals of statistical machine learning.

Over time, there has been growing recognition across a number of fields, from business to health care to genetics to the social sciences and beyond, that statistical learning is a powerful tool with important practical applications. As a result, **the field has moved from one of primarily academic interest to a mainstream discipline**, with an enormous potential audience.

The purpose of An Introduction to Statistical Learning (ISL) is to facilitate the transition of statistical learning from an academic to a mainstream field.

ISL is built on four premises:

1.  Many statistical learning methods are relevant and useful in a wide range of academic and non-academic disciplines, beyond just the statistical sciences.
    -   Focus on most widely used methods
2.  Statistical learning should not be viewed as a series of black boxes.
    -   Attempted to carefully describe the model, intuition, assumptions, and trade-offs behind each of the methods that we consider.
3.  While it is important to know what job is performed by each cog, it is not necessary to have the skills to construct the machine inside the box!
    -   Assumed that the reader is comfortable with basic mathematical concepts, but does not possess a graduate degree in the mathematical sciences.
4.  Presume that the reader is interested in applying statistical learning methods to real-world problems.
    -   [`R`](https://www.r-project.org/about.html) is the language of choice for academic statisticians, and new approaches often become available in `R` years before they are implemented in commercial packages.

Who Should Read This Book
-------------------------

This book is **intended for anyone who is interested in using modern statistical methods for modeling and prediction from data**.

This group includes scientists, engineers, data analysts, or quants, but also less technical individuals with degrees in non-quantitative fields such as the social sciences or business. We expect that the reader will have had at least one elementary course in statistics.

Notation and Simple Matrix Algebra
----------------------------------

We will use *n* to represent the number of distinct data points, or observa- tions, in our sample. We will let *p* denote the number of variables that are available for use in making predictions.

For instance, the `Wage` data set contains 11 variables for 3000 people. Therefore, we have *n* = 3000 observations and *p* = 11 variables.

Note that any `data set`, `column name`, or `file` will be distinguished by a gray-colored background.

Organization of This Book
-------------------------

-   Chapter 2 introduces the basic terminology and concepts behind statistical learning.
    -   This chapter also presents the K-nearest neighbor classifier, a very simple method that works surprisingly well on many problems.
-   Chapter 3 reviews linear regression, the fundamental starting point for all regression methods.

-   Chapter 4 discusses two of the most important classical classification methods, logistic regression and linear discriminant analysis.

-   Chapter 5 introduces cross-validation and the bootstrap, which can be used to estimate the accuracy of a number of different methods in order to choose the best one.

-   Chapter 6 considers a host of linear methods, both classical and more modern, which offer potential improvements over standard linear regression.
    -   These include stepwise selection, ridge regression, principal components regression, partial least squares, and the lasso.
-   Chapter 7 introduces a number of non-linear methods that work well for problems with a single input variable.
    -   Show how these methods can be used to fit non-linear additive models for which there is more than one input.
-   Chapter 8 investigates tree-based methods, including bagging, boosting, and random forests.

-   Chapter 9 introduces support vector machines, a set of approaches for performing both linear and non-linear classification.

-   Chapter 10 considers a setting in which we have input variables but no output variable.
    -   In particular, we present principal components analysis, K-means clustering, and hierarchical clustering

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
    ##  date     2019-02-03                  
    ## 
    ## ─ Packages ──────────────────────────────────────────────────────────────
    ##  package      * version  date       lib source        
    ##  assertthat     0.2.0    2017-04-11 [1] CRAN (R 3.5.0)
    ##  backports      1.1.3    2018-12-14 [1] CRAN (R 3.5.0)
    ##  bindr          0.1.1    2018-03-13 [1] CRAN (R 3.5.0)
    ##  bindrcpp     * 0.2.2    2018-03-29 [1] CRAN (R 3.5.0)
    ##  broom          0.5.1    2018-12-05 [1] CRAN (R 3.5.0)
    ##  cellranger     1.1.0    2016-07-27 [1] CRAN (R 3.5.0)
    ##  cli            1.0.1    2018-09-25 [1] CRAN (R 3.5.0)
    ##  colorspace     1.3-2    2016-12-14 [1] CRAN (R 3.5.0)
    ##  crayon         1.3.4    2017-09-16 [1] CRAN (R 3.5.1)
    ##  digest         0.6.18   2018-10-10 [1] CRAN (R 3.5.0)
    ##  dplyr        * 0.7.8    2018-11-10 [1] CRAN (R 3.5.0)
    ##  evaluate       0.12     2018-10-09 [1] CRAN (R 3.5.0)
    ##  forcats      * 0.3.0    2018-02-19 [1] CRAN (R 3.5.0)
    ##  generics       0.0.2    2018-11-29 [1] CRAN (R 3.5.0)
    ##  ggplot2      * 3.1.0    2018-10-25 [1] CRAN (R 3.5.0)
    ##  glue           1.3.0    2018-07-17 [1] CRAN (R 3.5.0)
    ##  gridExtra    * 2.3      2017-09-09 [1] CRAN (R 3.5.0)
    ##  gtable         0.2.0    2016-02-26 [1] CRAN (R 3.5.0)
    ##  haven          2.0.0    2018-11-22 [1] CRAN (R 3.5.0)
    ##  hms            0.4.2    2018-03-10 [1] CRAN (R 3.5.0)
    ##  htmltools      0.3.6    2017-04-28 [1] CRAN (R 3.5.0)
    ##  httr           1.4.0    2018-12-11 [1] CRAN (R 3.5.0)
    ##  ISLR         * 1.2      2017-10-20 [1] CRAN (R 3.5.0)
    ##  jsonlite       1.6      2018-12-07 [1] CRAN (R 3.5.0)
    ##  knitr          1.21     2018-12-10 [1] CRAN (R 3.5.2)
    ##  labeling       0.3      2014-08-23 [1] CRAN (R 3.5.0)
    ##  lattice        0.20-38  2018-11-04 [1] CRAN (R 3.5.2)
    ##  lazyeval       0.2.1    2017-10-29 [1] CRAN (R 3.5.0)
    ##  lubridate      1.7.4    2018-04-11 [1] CRAN (R 3.5.0)
    ##  magrittr       1.5      2014-11-22 [1] CRAN (R 3.5.0)
    ##  MASS         * 7.3-51.1 2018-11-01 [1] CRAN (R 3.5.2)
    ##  Matrix         1.2-15   2018-11-01 [1] CRAN (R 3.5.2)
    ##  mgcv           1.8-26   2018-11-21 [1] CRAN (R 3.5.2)
    ##  modelr         0.1.2    2018-05-11 [1] CRAN (R 3.5.0)
    ##  munsell        0.5.0    2018-06-12 [1] CRAN (R 3.5.0)
    ##  nlme           3.1-137  2018-04-07 [1] CRAN (R 3.5.2)
    ##  pillar         1.3.1    2018-12-15 [1] CRAN (R 3.5.0)
    ##  pkgconfig      2.0.2    2018-08-16 [1] CRAN (R 3.5.0)
    ##  plyr           1.8.4    2016-06-08 [1] CRAN (R 3.5.0)
    ##  purrr        * 0.2.5    2018-05-29 [1] CRAN (R 3.5.0)
    ##  R6             2.3.0    2018-10-04 [1] CRAN (R 3.5.0)
    ##  RColorBrewer   1.1-2    2014-12-07 [1] CRAN (R 3.5.0)
    ##  Rcpp           1.0.0    2018-11-07 [1] CRAN (R 3.5.0)
    ##  readr        * 1.3.1    2018-12-21 [1] CRAN (R 3.5.0)
    ##  readxl         1.2.0    2018-12-19 [1] CRAN (R 3.5.0)
    ##  rlang          0.3.1    2019-01-08 [1] CRAN (R 3.5.2)
    ##  rmarkdown      1.11     2018-12-08 [1] CRAN (R 3.5.0)
    ##  rstudioapi     0.9.0    2019-01-09 [1] CRAN (R 3.5.2)
    ##  rvest          0.3.2    2016-06-17 [1] CRAN (R 3.5.0)
    ##  scales       * 1.0.0    2018-08-09 [1] CRAN (R 3.5.0)
    ##  sessioninfo    1.1.1    2018-11-05 [1] CRAN (R 3.5.0)
    ##  stringi        1.2.4    2018-07-20 [1] CRAN (R 3.5.0)
    ##  stringr      * 1.3.1    2018-05-10 [1] CRAN (R 3.5.0)
    ##  tibble       * 2.0.1    2019-01-12 [1] CRAN (R 3.5.2)
    ##  tidyr        * 0.8.2    2018-10-28 [1] CRAN (R 3.5.0)
    ##  tidyselect     0.2.5    2018-10-11 [1] CRAN (R 3.5.0)
    ##  tidyverse    * 1.2.1    2017-11-14 [1] CRAN (R 3.5.0)
    ##  withr          2.1.2    2018-03-15 [1] CRAN (R 3.5.0)
    ##  xfun           0.4      2018-10-23 [1] CRAN (R 3.5.0)
    ##  xml2           1.2.0    2018-01-24 [1] CRAN (R 3.5.0)
    ##  yaml           2.2.0    2018-07-25 [1] CRAN (R 3.5.0)
    ## 
    ## [1] /Library/Frameworks/R.framework/Versions/3.5/Resources/library
