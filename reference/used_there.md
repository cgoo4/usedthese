# Scrape the summaries for site-wide analysis

**\[experimental\]**

Harvests and consolidates function usage tables from pages of a Quarto
website by searching for tables with the CSS class "usedthese"

## Usage

``` r
used_there(url, num_links = 30)
```

## Arguments

- url:

  The url to the website listing page of posts containing usage tables
  created with
  [`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)

- num_links:

  The number of links returned from the listing page may be restricted
  using this argument.

## Value

A tibble summarising package & function usage

## Examples

``` r
# Uses a Quarto listing url to scrape & consolidate usage
used_there("https://www.quantumjitter.com/project/", 1)
#> # A tibble: 43 × 4
#>    Package    Function                n url                                     
#>    <chr>      <chr>               <int> <chr>                                   
#>  1 base       as.Date                 1 https://www.quantumjitter.com/project/j…
#>  2 base       as.numeric              1 https://www.quantumjitter.com/project/j…
#>  3 base       c                       2 https://www.quantumjitter.com/project/j…
#>  4 base       is.na                   1 https://www.quantumjitter.com/project/j…
#>  5 base       library                 9 https://www.quantumjitter.com/project/j…
#>  6 base       max                     1 https://www.quantumjitter.com/project/j…
#>  7 base       sum                     4 https://www.quantumjitter.com/project/j…
#>  8 clock      date_format             1 https://www.quantumjitter.com/project/j…
#>  9 conflicted conflict_prefer_all     1 https://www.quantumjitter.com/project/j…
#> 10 conflicted conflict_scout          1 https://www.quantumjitter.com/project/j…
#> # ℹ 33 more rows
```
