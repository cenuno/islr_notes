Introduction
================
Cristian E. Nuno
January 20, 2019

-   [Introduction](#introduction)
    -   [Wage Data](#wage-data)
    -   [Stock Market Data](#stock-market-data)

``` r
# load necessary packages ----
library(gridExtra)  # misc. functions for 'grid' graphics
library(ISLR)       # data for ISLR examples
library(scales)     # scale functions for visualizations
library(tidyverse)  # data science packages

# load necessary objects ----
wage.text <- "Source: Wage and other data for a group of 3000 male workers in the Mid-Atlantic region"

stock.text <- "Source: Daily percentage returns for the S&P 500 stock index between 2001 and 2005"

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

When we wish to predict a non-numerical value - a **categorical** or **qualitative** output, this is known as a **classification** problem. Let's examine the [`Smarket`](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Smarket) data set.

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
