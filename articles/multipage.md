# Site-wide usage

``` r

library(usedthese)
```

Having added
[`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)
to several of your Quarto website pages, you may want to make an overall
site analysis of your package and function usage.
[`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md)
scrapes and consolidates the tables into a `tibble` ready for analysis:

``` r


used_there("https://www.quantumjitter.com/project/")
#> # A tibble: 1,895 × 4
#>    Package Function     n url                                          
#>    <chr>   <chr>    <int> <chr>                                        
#>  1 base    append       1 https://www.quantumjitter.com/project/saowtu/
#>  2 base    c           10 https://www.quantumjitter.com/project/saowtu/
#>  3 base    cumsum       2 https://www.quantumjitter.com/project/saowtu/
#>  4 base    function     1 https://www.quantumjitter.com/project/saowtu/
#>  5 base    if           1 https://www.quantumjitter.com/project/saowtu/
#>  6 base    is.na        1 https://www.quantumjitter.com/project/saowtu/
#>  7 base    library     10 https://www.quantumjitter.com/project/saowtu/
#>  8 base    mean         2 https://www.quantumjitter.com/project/saowtu/
#>  9 base    readRDS      1 https://www.quantumjitter.com/project/saowtu/
#> 10 base    return       1 https://www.quantumjitter.com/project/saowtu/
#> # ℹ 1,885 more rows
```

[Favourite Things](https://www.quantumjitter.com/project/box/) shows an
example analysis which takes the tibble output from
[`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md),
augments these data with a category, and plots the most-used packages,
the most-used functions and a word cloud.
