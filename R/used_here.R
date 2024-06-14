#' Summarise function usage in a single document
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Consistent with knitr syntax highlighting, [used_here()] adds a
#' summary table of R package & function usage to a knitted Quarto or R Markdown document
#'
#' @details If the rendered summary includes rows where the package name is multiple packages
#' separated by a comma, this will be due to an unresolved conflict. The recommended approach
#' is to use the 'conflicted' package.
#'
#' @param fil If the usage summary is required in the document you are currently knitting,
#' then no argument need be specified.
#'
#' If you want to create a summary by running just the code chunk, then it is necessary to
#' specify the quoted name of the saved file. You should first load and attach the packages
#' used in a fresh R session.
#'
#' @return A printed kable table with the css class "usedthese"
#'
#' @export
#'
#' @examples
#' # Simple example which mimics a two-line script and creates
#' # an html table with a CSS class "usedthese"
#' usedthese::used_here("mean(c(1, 2, 3))\nsum(c(1, 2, 3))")
#'
used_here <- \(fil = knitr::current_input()) {
  if (is.null(fil)) {
    rlang::abort(
      "If you are knitting the current document, i.e. you clicked the Render button, then leave fil unspecified. If you are running the code chunks, then ensure you library the packages first in a fresh R session and specify the saved filename quoted.",
      fil = fil
    )
  }

  old <- options(knitr.duplicate.label = "allow")
  withr::defer(options(old))

  if (stringr::str_ends(fil, "Rmd|qmd|rmarkdown")) {
    purrr::walk(fil, knitr::purl, quiet = TRUE, documentation = 0)

    fil <- stringr::str_replace(fil, "Rmd|qmd|rmarkdown", "R")
  }

  pckg_loaded <- .packages() |>
    rlang::set_names()

  funs_loaded <- pckg_loaded |>
    purrr::map(\(x) base::ls(stringr::str_c("package:", x))) |>
    tibble::enframe("pckg_loaded", "func") |>
    tidyr::unnest(func)

  get_mode <- \(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  funs_origin <- pckg_loaded |>
    purrr::map(getNamespaceImports) |>
    purrr::list_flatten() |>
    tibble::enframe() |>
    dplyr::filter(value != "TRUE") |>
    tidyr::unnest(value) |>
    tidyr::separate_wider_delim(name, "_", names = c("pckg_loaded", "pckg_origin")) |>
    dplyr::rename(func = value) |>
    dplyr::mutate(pckg_origin = get_mode(pckg_origin), .by = func) |>
    dplyr::distinct()

  funs_scouted <- conflicted::conflict_scout() |>
    unlist() |>
    dplyr::bind_rows()

  if (nrow(funs_scouted) > 0) {
    funs_scouted <- funs_scouted |>
      tidyr::pivot_longer(tidyselect::everything(), names_to = "func") |>
      dplyr::mutate(func = stringr::str_remove(func, "\\d$")) |>
      dplyr::summarise(pckg_preferred = stringr::str_flatten_comma(value, na.rm = TRUE), .by = func)
  } else {
    funs_scouted <- tibble::tibble(pckg_preferred = "zzz", func = "zzz")
  }

  funs_augmented <- funs_loaded |>
    dplyr::left_join(funs_origin, dplyr::join_by(pckg_loaded, func)) |>
    dplyr::left_join(funs_scouted, dplyr::join_by(func)) |>
    dplyr::group_by(func) |>
    tidyr::fill(pckg_origin, .direction = "updown") |>
    dplyr::mutate(
      pckg_loaded = dplyr::coalesce(pckg_origin, pckg_loaded),
      pckg_loaded = dplyr::coalesce(pckg_preferred, pckg_loaded)
    ) |>
    dplyr::select(pckgx = pckg_loaded, func) |>
    dplyr::arrange(func, pckgx) |>
    dplyr::distinct(func, .keep_all = TRUE)

  funs_coded <- fil |>
    readr::read_lines() |>
    highr::hi_latex(fallback = TRUE) |>
    stringr::str_extract_all("([a-zA-Z_]+::)?\\\\hlkwd\\{([^\\{\\}]*(?=\\}))") |>
    purrr::list_c() |>
    tibble::as_tibble() |>
    tidyr::separate_wider_regex(value, c(pckg = ".*?", "\\\\hlkwd\\{", func = ".*")) |>
    dplyr::mutate(pckg = stringr::str_remove(pckg, "::") |> dplyr::na_if(""))

  funs_used <-
    funs_coded |>
    dplyr::left_join(funs_augmented, dplyr::join_by(func)) |>
    dplyr::mutate(pckg = dplyr::coalesce(pckg, pckgx)) |>
    dplyr::count(pckg, func) |>
    dplyr::mutate(func = stringr::str_c(func, "[", n, "]")) |>
    dplyr::summarise(func = stringr::str_c(func, collapse = ", "), .by = pckg) |>
    tidyr::drop_na()

  funs_used |>
    knitr::kable(
      format = "html",
      table.attr = "class = 'usedthese'",
      col.names = c("Package", "Function")
    ) |>
    kableExtra::kable_styling("striped")
}
