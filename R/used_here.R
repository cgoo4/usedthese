#' Summarise function usage in a single document
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   Consistent with knitr syntax highlighting, [used_here()] adds a summary
#'   table of R package & function usage to a knitted Quarto or R Markdown
#'   document
#'
#' @details If the rendered summary includes rows where the package name is
#'   multiple packages separated by a comma, this will be due to an unresolved
#'   conflict. The recommended approach is to use the 'conflicted' package.
#'
#' @param fil If the usage summary is required in the document you are currently
#'   knitting, then no argument need be specified.
#'
#'   If you want to create a summary by running just the code chunk, then it is
#'   necessary to specify the quoted name of the saved file. You should first
#'   load and attach the packages used in a fresh R session.
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
used_here <- \(fil = current_input()) {
  if (is.null(fil)) {
    cli_abort(c(
      "`fil` must be either `current_input()` or a saved filename",
      "i" = "When knitting a qmd/Rmd, `fil` defaults to `current_input()`.",
      "i" = "When running a code chunk, quote a saved filename.",
      "x" = "You specified fil = {fil}."
    ))
  }

  old <- options(knitr.duplicate.label = "allow")
  defer(options(old))

  if (str_ends(fil, "Rmd|qmd|rmarkdown")) {
    walk(fil, purl, quiet = TRUE, documentation = 0)
    fil <- str_replace(fil, "Rmd|qmd|rmarkdown", "R")
  }

  pkg_loaded <- .packages() |> set_names()
  funs_origin <- get_loaded_pkg_imports(pkg_loaded[pkg_loaded != "usedthese"])
  funs_scouted <- conflict_scout() |> unlist() |> bind_rows()

  if (nrow(funs_scouted) > 0) {
    funs_scouted <- summarise_funs_scouted(funs_scouted)
  } else {
    funs_scouted <- tibble(pkg_preferred = "zzz", func = "zzz")
  }

  funs_augmented <- pkg_loaded |>
    get_funs_loaded() |>
    augment_funs_loaded(funs_origin, funs_scouted)

  fil |>
    extract_highlighted_funs() |>
    summarise_funs_used(funs_augmented) |>
    print_with_class()
}



#' Get loaded functions
#'
#' @rdname used_here
#' @usage NULL
get_funs_loaded <- \(x) {
  map(x, \(x) ls(str_c("package:", x))) |>
    enframe("pkg_loaded", "func") |>
    unnest(func)
}

#' Get mode
#'
#' @rdname used_here
#' @usage NULL
get_mode <- \(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Get the Imports of loaded packages
#'
#' @rdname used_here
#' @usage NULL
get_loaded_pkg_imports <- \(x){
  map(x, getNamespaceImports) |>
    list_flatten() |>
    enframe() |>
    filter(value != "TRUE") |>
    unnest(value) |>
    separate_wider_delim(name, "_", names = c("pkg_loaded", "pkg_origin")) |>
    rename(func = value) |>
    mutate(pkg_origin = get_mode(pkg_origin), .by = func) |>
    distinct()
}

#' Summarise functions scouted
#'
#' @rdname used_here
#' @usage NULL
summarise_funs_scouted <- \(x){
  pivot_longer(x, everything(), names_to = "func") |>
    mutate(func = str_remove(func, "\\d$")) |>
    summarise(
      pkg_preferred = str_flatten_comma(value, na.rm = TRUE),
      .by = func
      )
}

#' Augment functions loaded
#'
#' @rdname used_here
#' @usage NULL
augment_funs_loaded <- \(x, y, z){
  left_join(x, y, join_by(pkg_loaded, func)) |>
    left_join(z, join_by(func)) |>
    group_by(func) |>
    fill(pkg_origin, .direction = "updown") |>
    mutate(
      pkg_loaded = coalesce(pkg_origin, pkg_loaded),
      pkg_loaded = coalesce(pkg_preferred, pkg_loaded)
    ) |>
    select(pkgx = pkg_loaded, func) |>
    arrange(func, pkgx) |>
    distinct(func, .keep_all = TRUE)
}

#' Extract code-highlighted functions
#'
#' @rdname used_here
#' @usage NULL
extract_highlighted_funs <- \(x){
  read_lines(x) |>
    hi_latex(fallback = TRUE) |>
    str_extract_all("([a-zA-Z_]+::)?\\\\hlkwd\\{([^\\{\\}]*(?=\\}))") |>
    list_c() |>
    as_tibble() |>
    separate_wider_regex(value, c(pkg = ".*?", "\\\\hlkwd\\{", func = ".*")) |>
    mutate(pkg = str_remove(pkg, "::") |> na_if(""))
}

#' Summarise functions used
#'
#' @rdname used_here
#' @usage NULL
summarise_funs_used <- \(x, y){
  left_join(x, y, join_by(func)) |>
    mutate(pkg = coalesce(pkg, pkgx)) |>
    count(pkg, func) |>
    mutate(func = str_c(func, "[", n, "]")) |>
    summarise(func = str_c(func, collapse = ", "), .by = pkg) |>
    drop_na()
}

#' Print summary table with class
#'
#' @rdname used_here
#' @usage NULL
print_with_class <- \(x){
  kable(
    x,
    format = "html",
    table.attr = "class = 'usedthese'", # essential for used_here()
    col.names = c("Package", "Function")
  ) |>
    kable_styling("striped")
}
