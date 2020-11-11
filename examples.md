examples
================
Yu
November 11, 2020

Problem 1
---------

Read in the data.

``` r
homicide_df = 
  read_csv('data/homicide-data.csv') %>% 
  mutate(
    city_state = str_c(city, state, sep = '_'),
    resolved = case_when(
      disposition == 'Closed without arrest' ~ 'unsolved',
      disposition == 'Open/No arrest' ~ 'unsolved',
      disposition == 'Closed by arrest' ~ 'solved'
    )
  ) %>% 
  select(city_state, resolved) %>% 
  filter(city_state != 'Tulsa_AL')
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_double(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

Let's look at this a bit

``` r
aggregate_df = 
  homicide_df %>% 
    group_by(city_state) %>% 
    summarize(
      hom_total = n(),
      hom_unsolved = sum(resolved == 'unsolved')
    )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

Can I do prop test for a single city?

``` r
prop.test( 
  aggregate_df %>% filter(city_state == 'Baltimore_MD') %>% pull(hom_unsolved),
  aggregate_df %>% filter(city_state == 'Baltimore_MD') %>% pull(hom_total)) %>% 
  broom::tidy()
```

    ## # A tibble: 1 x 8
    ##   estimate statistic  p.value parameter conf.low conf.high method    alternative
    ##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
    ## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample~ two.sided

Try to iterate...

``` r
result_df = 
  aggregate_df %>% 
  mutate(
    prop_test = map2(.x = hom_unsolved, .y = hom_total, ~prop.test(x = .x, n = .y)),
    tidy_test = map(.x = prop_test, ~broom::tidy(.x))
  ) %>% 
  select(-prop_test) %>% 
  unnest(tidy_test) %>% 
  select(city_state, estimate, conf.low, conf.high)
```

``` r
result_df %>% 
  mutate(city_state = fct_reorder(city_state, estimate)) %>% 
  ggplot(aes(x = city_state, y = estimate))+
  geom_point()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="examples_files/figure-markdown_github/unnamed-chunk-5-1.png" width="90%" />
