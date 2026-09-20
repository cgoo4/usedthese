# Getting Started

To add a summary table of package & function usage to the foot of a
Quarto document, add
[`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)
to the end of the code. A separate code chunk with an appropriate
heading is suggested but not essential.

The package author recommends using the [conflicted
package](https://conflicted.r-lib.org) to resolve conflicts. In the
example below, ‘dplyr’ has been preferred over stats for
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and over
the xts package for
[`last()`](https://dplyr.tidyverse.org/reference/nth.html).

An alternative approach to using conflicted is to use the `exclude` or
`include.only` argument in
[`library()`](https://rdrr.io/r/base/library.html). This is also shown
below with the xts version of `first` excluded and hence the dplyr
version preferred.

## Some code

``` r

options(tidyverse.quiet = TRUE)
options(xts.warn_dplyr_breaks_lag = FALSE)
library(conflicted)
library(dplyr)
library(tibble)
conflicts_prefer(dplyr::filter, dplyr::last)
#> [conflicted] Will prefer dplyr::filter over any other package.
#> [conflicted] Will prefer dplyr::last over any other package.
library(usedthese)
library(xts, exclude = "first")
#> Loading required package: zoo
#> 
#> Attaching package: 'zoo'
#> 
#> The following objects are masked from 'package:base':
#> 
#>     as.Date, as.Date.numeric

conflict_scout()
#> 3 conflicts
#> • `filter()`: dplyr
#> • `lag()`: dplyr and stats
#> • `last()`: dplyr
```

## More code

``` r

tribble(~group, ~a1, ~a2, ~b1,
        "x", 1, 2, 3,
        "x", 4, 5, 6,
        "y", 7, 8, 9) |> 
  select(-starts_with("b")) |> 
  filter(group == "x") |> 
  mutate(first_a1 = first(a1),
         last_a2 = last(a2))
#> # A tibble: 2 × 5
#>   group    a1    a2 first_a1 last_a2
#>   <chr> <dbl> <dbl>    <dbl>   <dbl>
#> 1 x         1     2        1       5
#> 2 x         4     5        1       5
```

## Summary of usage

In the example below,
[`tribble()`](https://tibble.tidyverse.org/reference/tribble.html) is
counted once against the (originating) tibble package even though it is
also loaded by dplyr. And had we not used the conflicted package,
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) for
example would have shown against the package name “dplyr, stats”.

The rendered table is assigned the CSS class `.usedthese` to help other
`used_*` functions find and aggregate multiple tables across one or more
websites.

``` r


used_here()
```

| Package    | Function                                                     |
|:-----------|:-------------------------------------------------------------|
| base       | library\[5\], options\[2\]                                   |
| conflicted | conflict_scout\[1\], conflicts_prefer\[1\]                   |
| dplyr      | filter\[1\], first\[1\], last\[1\], mutate\[1\], select\[1\] |
| tibble     | tribble\[1\]                                                 |
| tidyselect | starts_with\[1\]                                             |
| usedthese  | used_here\[1\]                                               |
