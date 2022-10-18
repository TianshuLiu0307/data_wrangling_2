strings_and_factors
================
Tianshu Liu
2022-10-18

## String vectors

``` r
string_vec = c("my", "name", "is", "jeff")

str_detect(string_vec, "jeff")
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
str_replace(string_vec, "jeff", "Jeff")
```

    ## [1] "my"   "name" "is"   "Jeff"

``` r
string_vec = c(
  "i think we all rule for participating",
  "i think i have been caught",
  "i think this will be quite fun actually",
  "it will be fun, i think"
  )

str_detect(string_vec, "i think")
```

    ## [1] TRUE TRUE TRUE TRUE

``` r
str_detect(string_vec, "^i think")  # start with "i think"
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
str_detect(string_vec, "i think$")  # end with "i think"
```

    ## [1] FALSE FALSE FALSE  TRUE

``` r
string_vec = c(
  "Y'all remember Pres. HW Bush?",
  "I saw a green bush",
  "BBQ and Bushwalking at Molonglo Gorge",
  "BUSH -- LIVE IN CONCERT!!"
  )

str_detect(string_vec, "[Bb]ush")
```

    ## [1]  TRUE  TRUE  TRUE FALSE

``` r
string_vec = c(
  '7th inning stretch',
  '1st half soon to begin. Texas won the toss.',
  'she is 5 feet 4 inches tall',
  '3AM - cant sleep :('
  )

str_detect(string_vec, "[0-9][a-zA-Z]") #
```

    ## [1]  TRUE  TRUE FALSE  TRUE

``` r
string_vec = c(
  'Its 7:11 in the evening',
  'want to go to 7-11?',
  'my flight is AA711',
  'NetBios: scanning ip 203.167.114.66'
  )

str_detect(string_vec, "7.11")
```

    ## [1]  TRUE  TRUE FALSE  TRUE

``` r
string_vec = c(
  'The CI is [2, 5]',
  ':-]',
  ':-[',
  'I found the answer on pages [6-7]'
  )

str_detect(string_vec, "\\[")
```

    ## [1]  TRUE FALSE  TRUE  TRUE

``` r
str_detect(string_vec, "\\[[0-9]")
```

    ## [1]  TRUE FALSE FALSE  TRUE

## Why factors are weird

``` r
factor_vec = factor(c("male", "male", "female", "female"))
factor_vec
```

    ## [1] male   male   female female
    ## Levels: female male

``` r
as.numeric(factor_vec)
```

    ## [1] 2 2 1 1

``` r
factor_vec = fct_relevel(factor_vec, c("male", "female"))

factor_vec
```

    ## [1] male   male   female female
    ## Levels: male female

``` r
as.numeric(factor_vec)
```

    ## [1] 1 1 2 2

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

table_marj = 
  read_html(nsduh_url) %>% 
  html_table() %>% 
  first() %>% 
  slice(-1)
```

tidy up

``` r
marj_df = 
table_marj %>% 
  select(-contains("P value")) %>%  # only remain columns without "P value"
  pivot_longer(
    -State,
    names_to = "age_year",
    values_to = "percent"
  ) %>% 
  separate(age_year, into = c("age", "year"), sep = "\\(") %>% 
  mutate(
    percent  =str_replace(percent, "[a-c]$",""),    # end up with "a-c"
    percent = as.numeric(percent),
    year = str_replace(year, "\\)", "")
  ) %>% 
  filter(
    !State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West", "District of Columbia")
  )
```

``` r
marj_df %>% 
  ggplot(aes(x = State, y = percent, color = year)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

``` r
# Use fct_reorder to arrange x-axis
marj_df %>% 
  filter(age == "12-17") %>% 
  mutate(
    State = fct_reorder(State, percent)
  ) %>% 
  ggplot(aes(x = State, y = percent, color = year)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-10-2.png" width="90%" />

## Restaurant

``` r
data("rest_inspec")
```

``` r
rest_inspec  = 
  rest_inspec %>% 
  filter(grade %in% c("A", "B", "C"), boro != "Missing") %>% 
  mutate(
    boro = str_to_title(boro)
  )
  
rest_inspec %>% 
  group_by(boro, grade) %>% 
  summarise(
    n_obs  = n()
  ) %>% 
  pivot_wider(
    names_from = grade,
    values_from = n_obs
  )
```

    ## `summarise()` has grouped output by 'boro'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 4
    ## # Groups:   boro [5]
    ##   boro              A     B     C
    ##   <chr>         <int> <int> <int>
    ## 1 Bronx         13688  2801   701
    ## 2 Brooklyn      37449  6651  1684
    ## 3 Manhattan     61608 10532  2689
    ## 4 Queens        35952  6492  1593
    ## 5 Staten Island  5215   933   207

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  group_by(boro) %>% 
  summarise(n_pizza = n())
```

    ## # A tibble: 5 × 2
    ##   boro          n_pizza
    ##   <chr>           <int>
    ## 1 Bronx            1531
    ## 2 Brooklyn         2305
    ## 3 Manhattan        2479
    ## 4 Queens           1954
    ## 5 Staten Island     471

``` r
rest_inspec %>% 
  filter(str_detect(dba, "[Pp][Ii][Zz][Zz][Aa]")) %>% 
  mutate(
    boro = fct_infreq(boro),  # desc order by boro frequency
    boro = fct_recode(boro, "The city" = "Manhattan")   # rename "Manhattan"
  ) %>% 
  ggplot(aes(x = boro)) +
  geom_bar()
```

<img src="strings_and_factors_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" />

## Airbnb
