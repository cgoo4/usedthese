sift <- \(){
  box <- .packages() |> # Attached packages & functions
    rlang::set_names() |>
    purrr::map(\(x) ls(str_c("package:", x))) |>
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

  # options(knitr.duplicate.label = "allow")

  # walk("index.qmd", knitr::purl, quiet = TRUE, documentation = 0)

  code_only <- "R/sift.R" |>
    brio::read_lines() |>
    highr::hi_latex() |>
    stringr::str_extract_all("(?<=kwd\\{)[a-zA-Z0-9_.]*(?=\\})") |>
    unlist() |>
    sort()

  # unlink("index.R")

  # Which functions are used in the code?

  purrr::map2(box$func, box$pckg, \(i, j) {
    tibble::tibble(
      Package = j,
      func = i,
      total = code_only |> str_count(str_c("^\\Q", i, "\\E$")) |> sum()
    )
  }) |>
    purrr::list_rbind() |>
    filter(total > 0) |>
    dplyr::mutate(
      conflict = if_else(func %in% conflicts(), 1, 0),
      Function = str_c(func, "[", total, "]"),
      Package = str_remove(Package, "package:")
    ) |>
    dplyr::arrange(desc(conflict), func) |>
    dplyr::filter(conflict == 0 | str_c(Package, func) %in%
      stringr::str_c(origin$Package, origin$func)) |>
    dplyr::summarise(
      Function = str_c(Function, collapse = ";  "),
      .by = Package
    ) |>
    dplyr::arrange(Package)
}
