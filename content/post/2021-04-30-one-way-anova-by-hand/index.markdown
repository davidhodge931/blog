---
title: One-way ANOVA by hand
author: 'David Hodge'
date: '2021-04-30'
slug: one-way-anova-by-hand
categories: ["R"]
tags: ["R"]
---

One-way ANOVA is a test used to assess whether there is a statistically significant difference between the mean of different levels of a factor.

So in this case, there is a response numeric variable and an explanatory categorical variable with more than 1 level.

ANOVA considers the probability of observing the sample ratio of explained variance to unexplained variance (i.e. the F statistic), if the null hypothesis is true that all population means are equal.

You can perform a one-way ANOVA very easily in R using the `aov` function etc. But for the purposes of understanding what is actually gone on in this test,  let's calculate it 'by hand'.

First, let's load some libraries and sample data.


```r
library(tidyverse)
library(tidymodels)
library(simplevis)

cat_var <- rep(c(8, 16, 32, 64), each = 8)

num_var <- c(1.4, 2.0, 3.2, 1.4, 2.3, 4.0, 5.0, 4.7,
           3.2, 6.8, 5.0, 2.5, 6.1, 4.8, 4.6, 4.2,
           6.2, 3.1, 3.2, 4.0, 4.5, 6.4, 4.4, 4.1,
           5.8, 6.6, 6.5, 5.9, 5.9, 3.0, 5.9, 5.6)

data <- tibble(cat_var, num_var) %>% 
  mutate(cat_var = as.factor(cat_var))

head(data, n = 1)
```

```
## # A tibble: 1 x 2
##   cat_var num_var
##   <fct>     <dbl>
## 1 8           1.4
```

Next, let's visualise the data.


```r
ggplot_boxplot(data, cat_var, num_var,
       title = NULL,
       x_title = "Catagorical explanatory variable", 
       y_title = "Numeric response variable")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-2-1.png" width="672" />

I'm going to use a fixed effects means model for simplicity (Model I).

Yij = μi + Eij 

The hypotheses are as follow.

H0: _all_ population means are equal

H1: _not all_ population means are equal

1. Calculate SSE, df and MSE for if H1 is TRUE (i.e. complete model)


```r
complete <- data %>%  
  group_by(cat_var) %>% 
  mutate(fit = mean(num_var)) %>% #in complete model, fit is the group mean 
  mutate(error = fit - num_var) %>% 
  mutate(sq_error = error ^ 2) %>%
  ungroup()

head(complete)
```

```
## # A tibble: 6 x 5
##   cat_var num_var   fit  error sq_error
##   <fct>     <dbl> <dbl>  <dbl>    <dbl>
## 1 8           1.4     3  1.6     2.56  
## 2 8           2       3  1       1     
## 3 8           3.2     3 -0.200   0.0400
## 4 8           1.4     3  1.6     2.56  
## 5 8           2.3     3  0.7     0.49  
## 6 8           4       3 -1       1
```

```r
(complete <- complete %>% 
  summarise(n = n(), sse = sum(sq_error)) %>% 
  mutate(p = length(unique(data$cat_var))) %>% 
  mutate(df = n - p) %>% 
  select(sse, df, n, p) %>% 
  mutate(mse = sse / df) # mse = sigma squared 
)
```

```
## # A tibble: 1 x 5
##     sse    df     n     p   mse
##   <dbl> <int> <int> <int> <dbl>
## 1  47.8    28    32     4  1.71
```

2. Calculate SSE, df and MSE for if H0 is TRUE (i.e. reduced model)


```r
reduced <- data %>%  
  mutate(fit = mean(num_var)) %>% #in reduced model, fit is the overall mean 
  mutate(error = fit - num_var) %>% 
  mutate(sq_error = error ^ 2) %>%
  ungroup() 

head(reduced)
```

```
## # A tibble: 6 x 5
##   cat_var num_var   fit error sq_error
##   <fct>     <dbl> <dbl> <dbl>    <dbl>
## 1 8           1.4  4.45 3.05     9.28 
## 2 8           2    4.45 2.45     5.99 
## 3 8           3.2  4.45 1.25     1.55 
## 4 8           1.4  4.45 3.05     9.28 
## 5 8           2.3  4.45 2.15     4.61 
## 6 8           4    4.45 0.447    0.200
```

```r
(reduced <- reduced %>% 
  summarise(n = n(), sse = sum(sq_error)) %>% # sse = sum square error
  mutate(p = 1) %>% 
  mutate(df = n - p) %>% 
  select(sse, df, n, p)
)
```

```
## # A tibble: 1 x 4
##     sse    df     n     p
##   <dbl> <dbl> <int> <dbl>
## 1  76.4    31    32     1
```

3. Calculate the _extra_ SSE, df and MSE explained by the complete model. 


```r
(extra_sse <- reduced$sse - complete$sse)
```

```
## [1] 28.67094
```

```r
(extra_df <- reduced$df - complete$df)
```

```
## [1] 3
```

```r
(extra_mse <- extra_sse / extra_df)
```

```
## [1] 9.556979
```

4. Calculate the ratio of _extra_ variance (i.e. MSE) the complete model has explained to the variance (i.e. MSE) in the complete model itself. 


```r
(f <- extra_mse / complete$mse)
```

```
## [1] 5.601893
```

```r
pf(f, extra_df, complete$df, lower.tail = F)
```

```
## [1] 0.003871857
```

Let's check whether we got it right.


```r
anova <- aov(num_var ~ cat_var, data)
summary(anova)
```

```
##             Df Sum Sq Mean Sq F value  Pr(>F)   
## cat_var      3  28.67   9.557   5.602 0.00387 **
## Residuals   28  47.77   1.706                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Nice one!
