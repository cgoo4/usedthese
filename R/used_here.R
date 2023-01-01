#' Extract & summarise function usage
#'
#' @param fil A file name (.R, .Rmd or .qmd)
#'
#' @return A tibble
#'
#' @export
#'
#' @examples
#' # Mimics the input of a two-line R script
#' used_here("mean(c(1, 2, 3))\nsum(c(1, 2, 3))")
#' #   A tibble: 1 × 2
#' #   Package Function
#' #   <chr>   <chr>
#' # 1 base    c[2];  mean[1];  sum[1]
#'
used_here <- \(fil = knitr::current_input()){
  box <- .packages() |> # Attached packages & functions
    rlang::set_names() |>
    purrr::map(\(x) base::ls(stringr::str_c("package:", x))) |>
    tibble::enframe("pckg", "func") |>
    tidyr::unnest(cols = func)

  origin <- tibble::tribble( # Keep these where there are conflicts
    ~Package, ~func,
    "dplyr", "filter",
    "tibble", "as_tibble",
    "tibble", "tibble",
    "tibble", "tribble",
    "quanteda", "t"
  )

  # Extract code

  options(knitr.duplicate.label = "allow")

  if (stringr::str_ends(fil, "Rmd|qmd|rmarkdown")) {
    purrr::walk(fil, knitr::purl, quiet = TRUE, documentation = 0)

    fil <- stringr::str_replace(fil, "Rmd|qmd|rmarkdown", "R")
  }

  code_only <- fil |>
    readr::read_lines() |>
    highr::hi_latex() |>
    stringr::str_extract_all("(?<=kwd\\{)[a-zA-Z0-9_.]*(?=\\})") |>
    unlist() |>
    sort()

  base::unlink(fil)

  # Which functions are used in the code?

  purrr::map2(box$func, box$pckg, \(i, j) {
    tibble::tibble(
      Package = j,
      func = i,
      total = code_only |> stringr::str_count(stringr::str_c("^\\Q", i, "\\E$")) |> base::sum()
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(total > 0) |>
    dplyr::mutate(
      conflict = dplyr::if_else(func %in% base::conflicts(), 1, 0),
      Function = stringr::str_c(func, "[", total, "]"),
      Package = stringr::str_remove(Package, "package:")
    ) |>
    dplyr::arrange(dplyr::desc(conflict), func) |>
    dplyr::filter(conflict == 0 | stringr::str_c(Package, func) %in%
      stringr::str_c(origin$Package, origin$func)) |>
    dplyr::summarise(
      Function = stringr::str_c(Function, collapse = ";  "),
      .by = Package
    ) |>
    dplyr::arrange(Package)
}
