# usedthese

## Overview

usedthese is a pair of functions to summarise package & function usage
in Quarto documents consistent with knitr’s syntax highlighting:

1.  [`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)
    adds a nicely-rendered summary table of usage to a single document;

2.  [`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md)
    harvests and consolidates the tables created above ready for
    site-wide analysis.

## Example Use Case

Each “little project” on the Quarto website
[quantumjitter.com](https://www.quantumjitter.com/project/) focuses on a
data science technique or machine learning model to analyse real-world
data. At the foot of each of these is a quantified view of the R
packages and functions used in the associated code. This is achieved by
including
[`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)
in each Quarto document.

[`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md)
scrapes all the tables created by
[`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)
to enable a [Favourite
Things](https://www.quantumjitter.com/project/box/) article on how
packages and functions are used across the website.

I personally find this approach makes it easier for me to check for
consistency of usage, acknowledge the packages I most need to keep
abreast of (or go deeper on), and spot opportunities for updates to the
latest and greatest.

When the Tidyverse blog announced changes to
[dplyr](https://tidyverse.org/blog/2022/11/dplyr-1-1-0-is-coming-soon/)
and [purrr](https://tidyverse.org/blog/2022/12/purrr-1-0-0/) a quick
review of my [Favourite
Things](https://www.quantumjitter.com/project/box/) identified a number
of opportunities to try out the exciting new features. For example,
dplyr introduced temporary grouping with the `.by` argument for mutate
and amigos. `group_by()` and `ungroup()` had been used many times and
most of these occurrences could be replaced with the new more concise
approach.

## Installation

``` r

install.packages("usedthese")
```

## Development version

To get a bug fix, or to use a feature from the development version, you
can install usedthese from GitHub.

``` r

# install.packages("pak")
pak::pak("cgoo4/usedthese")
```
