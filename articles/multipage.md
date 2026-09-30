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
#> # A tibble: 1,899 × 4
#>    Package    Function                n url                                     
#>    <chr>      <chr>               <int> <chr>                                   
#>  1 base       c                       1 https://www.quantumjitter.com/project/g…
#>  2 base       factor                  1 https://www.quantumjitter.com/project/g…
#>  3 base       library                 7 https://www.quantumjitter.com/project/g…
#>  4 base       mean                    2 https://www.quantumjitter.com/project/g…
#>  5 base       seq                     1 https://www.quantumjitter.com/project/g…
#>  6 base       seq_len                 1 https://www.quantumjitter.com/project/g…
#>  7 base       set.seed                1 https://www.quantumjitter.com/project/g…
#>  8 base       sprintf                 1 https://www.quantumjitter.com/project/g…
#>  9 base       sqrt                    1 https://www.quantumjitter.com/project/g…
#> 10 conflicted conflict_prefer_all     1 https://www.quantumjitter.com/project/g…
#> # ℹ 1,889 more rows
```

[Favourite Things](https://www.quantumjitter.com/project/box/) shows an
example analysis which takes the tibble output from
[`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md),
augments these data with a category, and plots the most-used packages,
the most-used functions and a word cloud.
