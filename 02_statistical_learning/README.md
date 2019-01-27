Statistical Learning
================
Cristian E. Nuno
January 27, 2019

-   [What is Statistical Learning?](#what-is-statistical-learning)
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
