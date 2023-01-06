
<!-- README.md is generated from README.Rmd. Please edit that file -->

# usedthese

<!-- badges: start -->
<!-- badges: end -->

The overall goal of usedthese is to:

1.  With `used_here()`, make it super easy to add a nicely-rendered
    summary table of R package & function usage, consistent with knitr’s
    syntax highlighting, to the foot of a rendered Quarto or R Markdown
    document. \[Development version available.\]

2.  With `used_there()`, harvest multiple such tables to create an
    overall summary of usage for your website. \[Statement of future
    intent.\]

3.  With `used_elsewhere()`, websites deploying the usedthese package,
    and where the owners have explicitly chosen to add their website url
    to a participation list, may be analysed for a broader view of
    package & function usage. \[Statement of future intent.\]

4.  With `used_where()`, for a supplied specific function example use
    cases may be found from websites participating per 3. \[Statement of
    future intent.\]

## Case Study

Each “little project” on
[quantumjitter.com](https://www.quantumjitter.com/project/) focuses on a
data science technique or machine learning model to analyse real-world
data. At the foot of each of these is a concise quantified view of the R
packages and functions used in the associated code. This is achieved by
including `used_here()` in each Quarto document.

`used_there()` harvests all the tables created by `used_here()` to
enable a [Favourite Things](https://www.quantumjitter.com/project/box/)
view of how R packages and functions are used right across
quantumjitter.com.

When an updated version of dplyr introduced the `.by` argument for
mutate and friends, it was helpful to see how many times `group_by()`
and `ungroup()` had been used across all projects. And after updating
the projects where it made sense to do so, it was interesting to see the
overall impact of the changes on usage.

## Installation

You can install the development version of usedthese from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cgoo4/usedthese")
```
