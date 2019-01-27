Statistical Learning
================
Cristian E. Nuno
January 27, 2019

-   [What is Statistical Learning?](#what-is-statistical-learning)

``` r
# load necessary packages -----
library(scales)
library(here)
library(tidyverse)

# load necessary objects ----
my.theme <- 
  theme_minimal() + 
  theme(panel.grid = element_blank()
        , plot.caption = element_text(size = 5))

advertising.text <-
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

In this case, advertising budgets are the *input variables* - *X* - while `sales` is the *output variable* - *Y*. The relationship between sales as a measured by each media type's advertising budget is shown belown.

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
       , caption = advertising.text) +
  my.theme
```

![](README_files/figure-markdown_github/examine%20advertising-1.png)
