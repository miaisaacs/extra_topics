Statistical Learning
================
Mia Isaacs
2024-11-21

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(glmnet)
```

    ## Loading required package: Matrix
    ## 
    ## Attaching package: 'Matrix'
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack
    ## 
    ## Loaded glmnet 4.1-8

``` r
set.seed(10)
```

## try some lassoing!!

load and clean birthweight datset

``` r
bwt_df = 
  read_csv("data/birthweight.csv") |> 
  janitor::clean_names() |>
  mutate(
    babysex = 
        case_match(babysex,
            1 ~ "male",
            2 ~ "female"
        ),
    babysex = fct_infreq(babysex),
    frace = 
        case_match(frace,
            1 ~ "white",
            2 ~ "black", 
            3 ~ "asian", 
            4 ~ "puerto rican", 
            8 ~ "other"),
    frace = fct_infreq(frace),
    mrace = 
        case_match(mrace,
            1 ~ "white",
            2 ~ "black", 
            3 ~ "asian", 
            4 ~ "puerto rican",
            8 ~ "other"),
    mrace = fct_infreq(mrace),
    malform = as.logical(malform)) |> 
  sample_n(200)
```

    ## Rows: 4342 Columns: 20
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (20): babysex, bhead, blength, bwt, delwt, fincome, frace, gaweeks, malf...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

construct inputs for ‘glmnet’

``` r
x = model.matrix(bwt ~ ., data = bwt_df)[, -1]
y = bwt_df |> pull(bwt)
```

fit lasso for several lambdas

``` r
lambda = 10^seq(-2, 2.75, by = 0.1)

lasso_fit =
  glmnet(x = x, y = y, lambda = lambda)

lasso_cv =
  cv.glmnet(x, y, lambda = lambda)

lambda_opt = lasso_cv[["lambda.min"]]
```

usual lasso plot

``` r
lasso_fit |> 
  broom::tidy() |> 
  select(term, lambda, estimate) |> 
  complete(term, lambda, fill = list(estimate = 0) ) |> 
  filter(term != "(Intercept)") |> 
  ggplot(aes(x = lambda, y = estimate, group = term, color = term)) + 
  geom_path() + 
  geom_vline(xintercept = lambda_opt, color = "blue", size = 1.2) +
  theme(legend.position = "none")
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](stat_learning_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
lasso_cv |> 
  broom::tidy() |> 
  ggplot(aes(x = log(lambda, 10), y = estimate)) + 
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
lasso_fit = 
  glmnet(x, y, lambda = lambda_opt)

lasso_fit |> broom::tidy()
```

    ## # A tibble: 13 × 5
    ##    term               step    estimate lambda dev.ratio
    ##    <chr>             <dbl>       <dbl>  <dbl>     <dbl>
    ##  1 (Intercept)           1 -5036.          10     0.740
    ##  2 bhead                 1   129.          10     0.740
    ##  3 blength               1    63.1         10     0.740
    ##  4 delwt                 1     2.04        10     0.740
    ##  5 fincome               1     1.79        10     0.740
    ##  6 fraceblack            1  -114.          10     0.740
    ##  7 gaweeks               1     9.34        10     0.740
    ##  8 malformTRUE           1   -83.6         10     0.740
    ##  9 menarche              1    -4.02        10     0.740
    ## 10 mraceblack            1    -0.00514     10     0.740
    ## 11 mracepuerto rican     1   -52.9         10     0.740
    ## 12 smoken                1    -5.61        10     0.740
    ## 13 wtgain                1     2.68        10     0.740

## clustering pokemon

``` r
poke_df = 
  read_csv("data/pokemon.csv") |> 
  janitor::clean_names() |> 
  select(hp, speed)
```

    ## Rows: 800 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): Name, Type 1, Type 2
    ## dbl (9): #, Total, HP, Attack, Defense, Sp. Atk, Sp. Def, Speed, Generation
    ## lgl (1): Legendary
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
poke_df |> 
  ggplot(aes(x = hp, y = speed)) + 
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
kmeans_fit =
  kmeans(x = poke_df, centers = 3)
```

``` r
poke_df =
  broom::augment(kmeans_fit, poke_df)

poke_df |> 
  ggplot(aes(x = hp, y = speed, color = .cluster)) +
  geom_point()
```

![](stat_learning_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
clusts =
  tibble(k = 2:4) |>
  mutate(
    km_fit =    map(k, \(n_clust) kmeans(poke_df, centers = n_clust)),
    augmented = map(km_fit, \(fit) broom::augment(x = fit, poke_df))
  )

clusts |> 
  select(-km_fit) |> 
  unnest(augmented) |> 
  ggplot(aes(hp, speed, color = .cluster)) +
  geom_point(aes(color = .cluster)) +
  facet_grid(~k)
```

![](stat_learning_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
