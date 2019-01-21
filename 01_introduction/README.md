Introduction
================
Cristian E. Nuno
January 21, 2019

-   [Introduction](#introduction)
    -   [Wage Data](#wage-data)
    -   [Stock Market Data](#stock-market-data)
    -   [Gene Expression Data](#gene-expression-data)

``` r
# load necessary packages ----
library(gridExtra)  # misc. functions for 'grid' graphics
library(ISLR)       # data for ISLR examples
library(MASS)       # Venables & Ripley's 'Modern Applied Statistics with s'
library(scales)     # scale functions for visualizations
library(tidyverse)  # data science packages

# load necessary objects ----
wage.text <- "Source: Wage and other data for a group of 3000 male workers in the Mid-Atlantic region"

stock.text <- "Source: Daily percentage returns for the S&P 500 stock index between 2001 and 2005"

gene.text <- "Source: NCI microarray data that contains cancer type and expression levels on 6,830 genes from 64 cancer cell lines"

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
