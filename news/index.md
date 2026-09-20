# Changelog

## usedthese 0.5.1

- Removed unused {httr} dependency.

## usedthese 0.5.0

CRAN release: 2024-06-26

- Refactored code.
- Improved error messages using `cli_abort`.
- Centralised roxygen `@importFrom` tags.
- Used mocking to test without the need for an internet connection.
- Added test for non-scalar `num_links`.
- Updated citation.

## usedthese 0.4.0

CRAN release: 2024-05-27

- Spring clean.
- Default branch master to main.

## usedthese 0.3.3

CRAN release: 2023-06-15

- Fixed occasional
  [`used_here()`](https://cgoo4.github.io/usedthese/reference/used_here.md)
  warning.
- Documentation updates.

## usedthese 0.3.2

CRAN release: 2023-03-24

- [`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md)
  fails gracefully if Internet resource unavailable.

## usedthese 0.3.1

CRAN release: 2023-02-24

- Patch update to fix test error.
- Default
  [`used_there()`](https://cgoo4.github.io/usedthese/reference/used_there.md)
  `num_links` to 30.

## usedthese 0.3.0

CRAN release: 2023-02-15

- Respects `include.only` and `exclude` arguments specified in
  [`library()`](https://rdrr.io/r/base/library.html).
- Small performance improvement with dplyr 1.1 and tidyr 1.3.
- Remove suggests for meta-packages tidyverse and fpp3.

## usedthese 0.2.0

CRAN release: 2023-01-21

- Support use of the conflicted package.
- Include functions using the double-colon operator.
- Resolve cases of a function counted against two packages.

## usedthese 0.1.0

CRAN release: 2023-01-16

- First submission to CRAN.
