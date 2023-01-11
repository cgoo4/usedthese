## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Resubmission
This is a resubmission. In this version I have:

* Improved function performance
* Fixed spelling: 'knitr' now in quotes (as it's a package name)
* Fixed DESCRIPTION & CITATION url
* Changed Depends to R (>= 4.1): Pipe added in this release

## Additional Comments

* Maintainer name & email are correct
* `devtools::check_Rhub` reports 'no command tidy found' on platform Fedora Linux, but per this link this [should not be a problem for CRAN submission](https://stackoverflow.com/questions/74857062/rhub-cran-check-keeps-giving-html-note-on-fedora-test-no-command-tidy-found)
