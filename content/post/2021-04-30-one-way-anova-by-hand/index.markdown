---
title: One-way ANOVA by hand
author: 'David Hodge'
date: '2021-04-30'
slug: one-way-anova-by-hand
categories: ["R"]
tags: ["R"]
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>

<script src="{{< blogdown/postref >}}index_files/plotly-binding/plotly.js"></script>

<script src="{{< blogdown/postref >}}index_files/typedarray/typedarray.min.js"></script>

<script src="{{< blogdown/postref >}}index_files/jquery/jquery.min.js"></script>

<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>

<link href="{{< blogdown/postref >}}index_files/plotly-htmlwidgets-css/plotly-htmlwidgets.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index_files/plotly-main/plotly-latest.min.js"></script>

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>

<script src="{{< blogdown/postref >}}index_files/plotly-binding/plotly.js"></script>

<script src="{{< blogdown/postref >}}index_files/typedarray/typedarray.min.js"></script>

<script src="{{< blogdown/postref >}}index_files/jquery/jquery.min.js"></script>

<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>

<link href="{{< blogdown/postref >}}index_files/plotly-htmlwidgets-css/plotly-htmlwidgets.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index_files/plotly-main/plotly-latest.min.js"></script>

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>

<script src="{{< blogdown/postref >}}index_files/plotly-binding/plotly.js"></script>

<script src="{{< blogdown/postref >}}index_files/typedarray/typedarray.min.js"></script>

<script src="{{< blogdown/postref >}}index_files/jquery/jquery.min.js"></script>

<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>

<link href="{{< blogdown/postref >}}index_files/plotly-htmlwidgets-css/plotly-htmlwidgets.css" rel="stylesheet" />

<script src="{{< blogdown/postref >}}index_files/plotly-main/plotly-latest.min.js"></script>

One-way ANOVA is a test used to assess whether there is a statistically significant difference between the mean of groups.

There is 1 response numeric variable and 1 explanatory categorical variable with more than 1 level.

ANOVA considers the probability of observing the sample ratio of explained variance to unexplained variance (i.e. the F statistic)… if the null hypothesis is true that all population means are equal.

You can perform a one-way ANOVA very easily in R using the `aov` function etc. But what fun would that be?\!

On a serious note, it is really helpful for understanding to see how the algorithm of the test works to calculate it ‘by hand’.

First, let’s load some libraries, and create some sample data.

``` r
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

head(data, n = 3)
```

    ## # A tibble: 3 x 2
    ##   cat_var num_var
    ##   <fct>     <dbl>
    ## 1 8           1.4
    ## 2 8           2  
    ## 3 8           3.2

Next, let’s visualise the data.

``` r
plot <- ggplot_boxplot(data, cat_var, num_var,
       y_zero = T,
       title = NULL,
       x_title = "Catagorical explanatory variable", 
       y_title = "Numeric response variable")

plotly::ggplotly(plot) %>% 
  plotly_camera()
```

<div id="htmlwidget-1" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"data":[{"x":[1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4],"y":[1.4,2,3.2,1.4,2.3,4,5,4.7,3.2,6.8,5,2.5,6.1,4.8,4.6,4.2,6.2,3.1,3.2,4,4.5,6.4,4.4,4.1,5.8,6.6,6.5,5.9,5.9,3,5.9,5.6],"hoverinfo":"y","type":"box","fillcolor":"rgba(49,104,142,1)","marker":{"opacity":null,"outliercolor":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"},"size":5.66929133858268},"line":{"color":"rgba(50,50,50,1)","width":1.88976377952756},"showlegend":false,"xaxis":"x","yaxis":"y","frame":null,"hoverlabel":{"align":"left"}}],"layout":{"margin":{"t":25.2984640929846,"r":26.5670402656704,"b":39.5184723951847,"l":30.2200083022001},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,4.6],"tickmode":"array","ticktext":["8","16","32","64"],"tickvals":[1,2,3,4],"categoryorder":"array","categoryarray":["8","16","32","64"],"nticks":null,"ticks":"outside","tickcolor":"rgba(50,50,50,1)","ticklen":3.65296803652968,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(50,50,50,1)","linewidth":0.398505603985056,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Catagorical explanatory variable","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0,7],"tickmode":"array","ticktext":["0","1","2","3","4","5","6","7"],"tickvals":[0,1,2,3,4,5,6,7],"categoryorder":"array","categoryarray":["0","1","2","3","4","5","6","7"],"nticks":null,"ticks":"outside","tickcolor":"rgba(50,50,50,1)","ticklen":3.65296803652968,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(50,50,50,1)","linewidth":0.398505603985056,"showgrid":true,"gridcolor":"rgba(211,211,211,1)","gridwidth":0.265670402656704,"zeroline":false,"anchor":"x","title":{"text":"Numeric response variable","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"modeBarButtonsToRemove":["zoom2d","pan2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","sendDataToCloud","toggleHover","resetViews","toggleSpikelines","resetViewMapbox","toggleSpikelines","resetViewMapbox","lasso2d","select2d"],"displaylogo":false},"source":"A","attrs":{"5a505c16d19":{"x":{},"y":{},"type":"box"}},"cur_data":"5a505c16d19","visdat":{"5a505c16d19":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

I’m going to use a fixed effects means model for simplicity (Model I).

Yij = μi + Eij

The hypotheses are as follow.

H0: *all* population means are equal

H1: *not all* population means are equal

We will use a significance level of p = 0.05.

Note the mean square error (MSE) is used to quantify variance, and this is obtained by dividing the sum of squared error (SSE) by the degrees of freedom (df).

1.  Calculate SSE, df and MSE for if H1 is TRUE (i.e. complete model)

<!-- end list -->

``` r
complete <- data %>%  
  group_by(cat_var) %>% 
  mutate(fit = mean(num_var)) %>% #in complete model, fit is the group mean 
  mutate(error = fit - num_var) %>% 
  mutate(sq_error = error ^ 2) %>%
  ungroup()

head(complete, 3)
```

    ## # A tibble: 3 x 5
    ##   cat_var num_var   fit  error sq_error
    ##   <fct>     <dbl> <dbl>  <dbl>    <dbl>
    ## 1 8           1.4     3  1.6     2.56  
    ## 2 8           2       3  1       1     
    ## 3 8           3.2     3 -0.200   0.0400

``` r
(complete <- complete %>% 
  summarise(n = n(), sse = sum(sq_error)) %>% 
  mutate(p = length(unique(data$cat_var))) %>% 
  mutate(df = n - p) %>% 
  select(sse, df, n, p) %>% 
  mutate(mse = sse / df) # mse = sigma squared 
)
```

    ## # A tibble: 1 x 5
    ##     sse    df     n     p   mse
    ##   <dbl> <int> <int> <int> <dbl>
    ## 1  47.8    28    32     4  1.71

2.  Calculate SSE, df and MSE for if H0 is TRUE (i.e. reduced model)

<!-- end list -->

``` r
reduced <- data %>%  
  mutate(fit = mean(num_var)) %>% #in reduced model, fit is the overall mean 
  mutate(error = fit - num_var) %>% 
  mutate(sq_error = error ^ 2) %>%
  ungroup() 

head(reduced, 3)
```

    ## # A tibble: 3 x 5
    ##   cat_var num_var   fit error sq_error
    ##   <fct>     <dbl> <dbl> <dbl>    <dbl>
    ## 1 8           1.4  4.45  3.05     9.28
    ## 2 8           2    4.45  2.45     5.99
    ## 3 8           3.2  4.45  1.25     1.55

``` r
(reduced <- reduced %>% 
  summarise(n = n(), sse = sum(sq_error)) %>% # sse = sum square error
  mutate(p = 1) %>% 
  mutate(df = n - p) %>% 
  select(sse, df, n, p)
)
```

    ## # A tibble: 1 x 4
    ##     sse    df     n     p
    ##   <dbl> <dbl> <int> <dbl>
    ## 1  76.4    31    32     1

3.  Calculate the SSE, df and MSE explained by the complete model.

<!-- end list -->

``` r
(explained_sse <- reduced$sse - complete$sse)
```

    ## [1] 28.67094

``` r
(explained_df <- reduced$df - complete$df)
```

    ## [1] 3

``` r
(explained_mse <- explained_sse / explained_df)
```

    ## [1] 9.556979

4.  Calculate the ratio of variance (i.e. MSE) the complete model has explained to the variance (i.e. MSE) that is left unexplained.

<!-- end list -->

``` r
(f <- explained_mse / complete$mse)
```

    ## [1] 5.601893

``` r
pf(f, explained_df, complete$df, lower.tail = F)
```

    ## [1] 0.003871857

There was a p \< 0.05, so we will reject the null hyothesis, and accept that there is a statistically significant difference between the means of the groups.

Let’s check whether we got it right… We did\!

``` r
anova <- aov(num_var ~ cat_var, data)
summary(anova)
```

    ##             Df Sum Sq Mean Sq F value  Pr(>F)   
    ## cat_var      3  28.67   9.557   5.602 0.00387 **
    ## Residuals   28  47.77   1.706                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Of course, we should also check the assumptions:

  - independence of observations
  - constant variation across groups
  - normal distribution of residual error.

We can do the last 2 visually, and the 1st by understanding the study design.

``` r
data <- augment(anova)

plot <- ggplot_boxplot(data, cat_var, .resid,
  y_balance = TRUE,
  title = "Residual error across groups", 
  x_title = "Group", 
  y_title = "Residual error")

plotly::ggplotly(plot) %>% 
  plotly_camera()
```

<div id="htmlwidget-2" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"data":[{"x":[1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4],"y":[-1.6,-1,0.200000000000001,-1.6,-0.7,1,2,1.7,-1.45,2.15,0.350000000000002,-2.15,1.45,0.150000000000002,-0.049999999999998,-0.449999999999998,1.7125,-1.3875,-1.2875,-0.4875,0.0125000000000002,1.9125,-0.0874999999999995,-0.3875,0.149999999999999,0.949999999999999,0.85,0.25,0.25,-2.65,0.25,-0.0500000000000007],"hoverinfo":"y","type":"box","fillcolor":"rgba(49,104,142,1)","marker":{"opacity":null,"outliercolor":"rgba(0,0,0,1)","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"},"size":5.66929133858268},"line":{"color":"rgba(50,50,50,1)","width":1.88976377952756},"showlegend":false,"xaxis":"x","yaxis":"y","frame":null,"hoverlabel":{"align":"left"}},{"x":[0.4,4.6],"y":[0,0],"text":"yintercept: 0","type":"scatter","mode":"lines","line":{"width":1.13385826771654,"color":"rgba(50,50,50,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null,"hoverlabel":{"align":"left"}}],"layout":{"margin":{"t":39.9103362391034,"r":26.5670402656704,"b":39.5184723951847,"l":36.8617683686177},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"title":{"text":"<b> Residual error across groups <\/b>","font":{"color":"rgba(0,0,0,1)","family":"Helvetica","size":14.6118721461187},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,4.6],"tickmode":"array","ticktext":["8","16","32","64"],"tickvals":[1,2,3,4],"categoryorder":"array","categoryarray":["8","16","32","64"],"nticks":null,"ticks":"outside","tickcolor":"rgba(50,50,50,1)","ticklen":3.65296803652968,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(50,50,50,1)","linewidth":0.398505603985056,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Group","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-3,3],"tickmode":"array","ticktext":["-3","-2","-1","0","1","2","3"],"tickvals":[-3,-2,-1,0,1,2,3],"categoryorder":"array","categoryarray":["-3","-2","-1","0","1","2","3"],"nticks":null,"ticks":"outside","tickcolor":"rgba(50,50,50,1)","ticklen":3.65296803652968,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(50,50,50,1)","linewidth":0.398505603985056,"showgrid":true,"gridcolor":"rgba(211,211,211,1)","gridwidth":0.265670402656704,"zeroline":false,"anchor":"x","title":{"text":"Residual error","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"modeBarButtonsToRemove":["zoom2d","pan2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","sendDataToCloud","toggleHover","resetViews","toggleSpikelines","resetViewMapbox","toggleSpikelines","resetViewMapbox","lasso2d","select2d"],"displaylogo":false},"source":"A","attrs":{"5a5079732d06":{"x":{},"y":{},"type":"box"},"5a5066ffc82":{"yintercept":{}}},"cur_data":"5a5079732d06","visdat":{"5a5079732d06":["function (y) ","x"],"5a5066ffc82":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>

``` r
plot <- ggplot(data) +
  geom_qq(aes(sample = .resid)) +
  geom_qq_line(aes(sample = .resid)) +
  labs(title = "Normal Q-Q plot", 
       x = "Theoretical normal distribution quantile", 
       y = "Residual error quantile") +
  theme_point() 

plotly::ggplotly(plot) %>% 
  plotly_camera()
```

<div id="htmlwidget-3" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"data":[{"x":[-2.15387469406146,-1.67593972277344,-1.41779713799627,-1.22985875921659,-1.07751556704028,-0.946781756301046,-0.830510878205399,-0.724514383492365,-0.626099012346421,-0.53340970624128,-0.445096524985516,-0.360129891789569,-0.277690439821577,-0.197099084294312,-0.117769874579095,-0.0391760855030976,0.0391760855030976,0.117769874579095,0.197099084294312,0.277690439821577,0.360129891789569,0.445096524985516,0.53340970624128,0.626099012346421,0.724514383492365,0.830510878205399,0.946781756301046,1.07751556704028,1.22985875921659,1.41779713799627,1.67593972277344,2.15387469406146],"y":[-2.65,-2.15,-1.6,-1.6,-1.45,-1.3875,-1.2875,-1,-0.7,-0.4875,-0.449999999999998,-0.3875,-0.0874999999999995,-0.0500000000000007,-0.049999999999998,0.0125000000000002,0.149999999999999,0.150000000000002,0.200000000000001,0.25,0.25,0.25,0.350000000000002,0.85,0.949999999999999,1,1.45,1.7,1.7125,1.9125,2,2.15],"text":["theoretical: -2.15387469<br />sample: -2.6500<br />.resid: -2.6500","theoretical: -1.67593972<br />sample: -2.1500<br />.resid: -2.1500","theoretical: -1.41779714<br />sample: -1.6000<br />.resid: -1.6000","theoretical: -1.22985876<br />sample: -1.6000<br />.resid: -1.6000","theoretical: -1.07751557<br />sample: -1.4500<br />.resid: -1.4500","theoretical: -0.94678176<br />sample: -1.3875<br />.resid: -1.3875","theoretical: -0.83051088<br />sample: -1.2875<br />.resid: -1.2875","theoretical: -0.72451438<br />sample: -1.0000<br />.resid: -1.0000","theoretical: -0.62609901<br />sample: -0.7000<br />.resid: -0.7000","theoretical: -0.53340971<br />sample: -0.4875<br />.resid: -0.4875","theoretical: -0.44509652<br />sample: -0.4500<br />.resid: -0.4500","theoretical: -0.36012989<br />sample: -0.3875<br />.resid: -0.3875","theoretical: -0.27769044<br />sample: -0.0875<br />.resid: -0.0875","theoretical: -0.19709908<br />sample: -0.0500<br />.resid: -0.0500","theoretical: -0.11776987<br />sample: -0.0500<br />.resid: -0.0500","theoretical: -0.03917609<br />sample:  0.0125<br />.resid:  0.0125","theoretical:  0.03917609<br />sample:  0.1500<br />.resid:  0.1500","theoretical:  0.11776987<br />sample:  0.1500<br />.resid:  0.1500","theoretical:  0.19709908<br />sample:  0.2000<br />.resid:  0.2000","theoretical:  0.27769044<br />sample:  0.2500<br />.resid:  0.2500","theoretical:  0.36012989<br />sample:  0.2500<br />.resid:  0.2500","theoretical:  0.44509652<br />sample:  0.2500<br />.resid:  0.2500","theoretical:  0.53340971<br />sample:  0.3500<br />.resid:  0.3500","theoretical:  0.62609901<br />sample:  0.8500<br />.resid:  0.8500","theoretical:  0.72451438<br />sample:  0.9500<br />.resid:  0.9500","theoretical:  0.83051088<br />sample:  1.0000<br />.resid:  1.0000","theoretical:  0.94678176<br />sample:  1.4500<br />.resid:  1.4500","theoretical:  1.07751557<br />sample:  1.7000<br />.resid:  1.7000","theoretical:  1.22985876<br />sample:  1.7125<br />.resid:  1.7125","theoretical:  1.41779714<br />sample:  1.9125<br />.resid:  1.9125","theoretical:  1.67593972<br />sample:  2.0000<br />.resid:  2.0000","theoretical:  2.15387469<br />sample:  2.1500<br />.resid:  2.1500"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null,"hoverlabel":{"align":"left"}},{"x":[-2.15387469406146,2.15387469406146],"y":[-2.58450500483383,2.68450500483383],"text":["x: -2.153875<br />y: -2.584505","x:  2.153875<br />y:  2.684505"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null,"hoverlabel":{"align":"left"}}],"layout":{"margin":{"t":39.9103362391034,"r":26.5670402656704,"b":39.5184723951847,"l":36.8617683686177},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"title":{"text":"<b> Normal Q-Q plot <\/b>","font":{"color":"rgba(0,0,0,1)","family":"Helvetica","size":14.6118721461187},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.3692621634676,2.3692621634676],"tickmode":"array","ticktext":["-2","-1","0","1","2"],"tickvals":[-2,-1,0,1,2],"categoryorder":"array","categoryarray":["-2","-1","0","1","2"],"nticks":null,"ticks":"outside","tickcolor":"rgba(50,50,50,1)","ticklen":3.65296803652968,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(50,50,50,1)","linewidth":0.398505603985056,"showgrid":true,"gridcolor":"rgba(211,211,211,1)","gridwidth":0.265670402656704,"zeroline":false,"anchor":"y","title":{"text":"Theoretical normal distribution quantile","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-2.91672525024169,2.95123025507553],"tickmode":"array","ticktext":["-2","-1","0","1","2"],"tickvals":[-2,-1,0,1,2],"categoryorder":"array","categoryarray":["-2","-1","0","1","2"],"nticks":null,"ticks":"outside","tickcolor":"rgba(50,50,50,1)","ticklen":3.65296803652968,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(50,50,50,1)","linewidth":0.398505603985056,"showgrid":true,"gridcolor":"rgba(211,211,211,1)","gridwidth":0.265670402656704,"zeroline":false,"anchor":"x","title":{"text":"Residual error quantile","font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(50,50,50,1)","family":"Helvetica","size":13.2835201328352}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false,"modeBarButtonsToRemove":["zoom2d","pan2d","zoomIn2d","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","sendDataToCloud","toggleHover","resetViews","toggleSpikelines","resetViewMapbox","toggleSpikelines","resetViewMapbox","lasso2d","select2d"],"displaylogo":false},"source":"A","attrs":{"5a5029bd1230":{"sample":{},"type":"scatter"},"5a501d087f07":{"sample":{}}},"cur_data":"5a5029bd1230","visdat":{"5a5029bd1230":["function (y) ","x"],"5a501d087f07":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
