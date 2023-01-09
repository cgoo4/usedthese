#' Summarise function usage in a single document
#'
#' Consistent with knitr syntax highlighting, [used_here()] adds a
#' summary table of R package & function usage to a knitted Quarto or R Markdown document
#'
#' @details If the rendered summary includes rows where the package name is prefixed with "DUPE", this will
#' be due to a conflict [used_here()] was unable to resolve, e.g. [dplyr::first()] versus
#' [xts::first()]. In such cases, the recommendation is to add "exclude = first" to the library
#' call of the unwanted version.
#'
#' @param fil If the usage summary is required in the document you are currently knitting the current document, it is not necessary to specify
#' then no argument need be specified.
#'
#' If you want to create the summary by running the code chunk directly, then it is necessary to
#' specify the quoted name of the saved file. This would also be the case if the usage summary
#' is required in, say, a blog post that's summarising the code in some other file.
#'
#' @return A printed kable table with the css class "usedthese"
#'
#' @export
#'
#' @examples
#' # Simple example -- see Get Started for an extended example
#' mean(c(1, 2, 3))
#' sum(c(1, 2, 3))
#'
#' # used_here()
#' # Package Function
#' # base    c[2];  mean[1];  sum[1]
#'
used_here <- \(fil = knitr::current_input()) {

  if (is.null(fil)) {
    rlang::abort("The fil argument must either be a quoted Quarto/R Markdown filename or, if left unspecified, must be the document you are currently knitting, e.g. you clicked the Render button.", "used_here_error", fil = fil)
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
    tidyr::unnest(cols = func)

  funs_origin <- pckg_loaded |>
    purrr::map(getNamespaceImports) |>
    purrr::list_flatten() |>
    tibble::enframe() |>
    dplyr::filter(value != "TRUE") |>
    tidyr::unnest(value) |>
    tidyr::separate(name, into = c("pckg_loaded", "pckg_origin")) |>
    dplyr::rename(func = value) |>
    dplyr::distinct()

  pckg_base <-
    c("stats",
      "graphics",
      "grDevices",
      "utils",
      "datasets",
      "methods",
      "base")

  funs_augmented <- funs_loaded |>
    dplyr::left_join(
      funs_origin,
      dplyr::join_by(pckg_loaded == pckg_loaded, func == func)
    ) |>
    dplyr::mutate(base = dplyr::if_else(pckg_loaded %in% pckg_base, 1L, 0L)) |>
    dplyr::group_by(func) |>
    dplyr::filter(base == base::min(base)) |>
    tidyr::fill(pckg_origin, .direction = "updown") |>
    dplyr::mutate(n = dplyr::n(),
                  # pckg_loaded = dplyr::if_else(n > 1, pckg_origin, pckg_loaded)
                  pckg_loaded =
                    dplyr::if_else(
                      n > 1 & is.na(pckg_origin),
                      stringr::str_c("[DUPE]", pckg_loaded),
                      dplyr::if_else(n > 1 &
                                !is.na(pckg_origin), pckg_origin, pckg_loaded)
                    )) |>
    dplyr::select(pckg = pckg_loaded, func) |>
    dplyr::distinct()

  funs_coded <- fil |>
    readr::read_lines() |>
    highr::hi_latex(fallback = TRUE) |>
    stringr::str_extract_all("(?<=kwd\\{)[^\\{\\}]*(?=\\})") |>
    purrr::list_c()

  funs_used <-
    purrr::map2(funs_augmented$func, funs_augmented$pckg, \(i, j) {
      tibble::tibble(
        pckg = j,
        func = i,
        total = funs_coded |> stringr::str_count(stringr::str_c("^\\Q", i, "\\E$")) |> base::sum()
      )
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(total > 0) |>
    dplyr::mutate(func = stringr::str_c(func, "[", total, "]")) |>
    dplyr::summarise(func = stringr::str_c(func, collapse = ";  "), .by = pckg) |>
    dplyr::arrange(pckg)

  funs_used |>
    knitr::kable(
      format = "html",
      table.attr = "class = 'usedthese'",
      col.names = c("Package", "Function")
    ) |>
    kableExtra::kable_styling("striped")
}
