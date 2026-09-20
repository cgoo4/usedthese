# Summarise function usage in a single document

**\[experimental\]**

Consistent with knitr syntax highlighting, `used_here()` adds a summary
table of R package & function usage to a knitted Quarto or R Markdown
document

## Usage

``` r
used_here(fil = current_input())
```

## Arguments

- fil:

  If the usage summary is required in the document you are currently
  knitting, then no argument need be specified.

  If you want to create a summary by running just the code chunk, then
  it is necessary to specify the quoted name of the saved file. You
  should first load and attach the packages used in a fresh R session.

## Value

A printed kable table with the css class "usedthese"

## Details

If the rendered summary includes rows where the package name is multiple
packages separated by a comma, this will be due to an unresolved
conflict. The recommended approach is to use the 'conflicted' package.

## Examples

``` r
# Simple example which mimics a two-line script and creates
# an html table with a CSS class "usedthese"
usedthese::used_here("mean(c(1, 2, 3))\nsum(c(1, 2, 3))")
#> Warning: The `file` argument of `vroom()` must use `I()` for literal data as of vroom
#> 1.5.0.
#>   
#>   # Bad:
#>   vroom("X,Y\n1.5,2.3\n")
#>   
#>   # Good:
#>   vroom(I("X,Y\n1.5,2.3\n"))
#> ℹ The deprecated feature was likely used in the readr package.
#>   Please report the issue at <https://github.com/tidyverse/readr/issues>.
#> <table class="usedthese table table-striped" style="margin-left: auto; margin-right: auto;">
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Package </th>
#>    <th style="text-align:left;"> Function </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;"> base </td>
#>    <td style="text-align:left;"> c[2], mean[1], sum[1] </td>
#>   </tr>
#> </tbody>
#> </table>
```
