#' Scrape the summaries for site-wide analysis
#'
#' @description `r lifecycle::badge('experimental')`
#'
#'   Harvests and consolidates function usage tables from pages of a Quarto
#'   website by searching for tables with the CSS class "usedthese"
#'
#' @param url The url to the website listing page of posts containing usage
#'   tables created with [used_here()]
#'
#' @param num_links The number of links returned from the listing page may be
#'   restricted using this argument.
#'
#' @return A tibble summarising package & function usage
#'
#' @export
#'
#' @examples
#' # Uses a Quarto listing url to scrape & consolidate usage
#' \donttest{used_there("https://www.quantumjitter.com/project/", 1)}
#'
used_there <- \(url, num_links = 30) {
  if(!(is.vector(num_links) && length(num_links) == 1)) {
  cli_abort(c(
    "{.var num_links} must be a scalar",
    "x" = "You've supplied a {.cls {class(num_links)}}
    object of length {length(num_links)}."
  ))
  } else

  html <- try_fetch(url |> read_html(),
    error = \(cnd) cli_abort(c(
      "{url} is currently unavailable.",
      "i" = "Verify the URL or try again later."
    ), parent = cnd)
  )

  get_links(html, url, num_links) |> extract_usage()
}



#' Get links from list page
#'
#' @rdname used_there
#' @usage NULL
get_links <- \(html, url, num_links){
html |>
  html_elements(
    ".quarto-grid-link, .quarto-default-link, .quarto-table-link"
  ) |>
  html_attr("href") |>
  str_replace("^",
              str_c(
                parse_url(url)$scheme, "://",
                parse_url(url)$hostname
              )
  ) |>
  tail(num_links)
}

#' Extract usage from multiple URLs
#'
#' @rdname used_there
#' @usage NULL
extract_usage <- \(urls){
  map(urls, \(x) {
    read_html(x) |>
      html_element(".usedthese") |>
      html_table() |>
      mutate(url = x)
  }) |>
    list_flatten() |>
    list_rbind() |>
    separate_longer_delim(Function, delim = ",") |>
    extract(Function, c("Function", "n"),
            "([^ ]+)\\[(.+)\\]",
            convert = TRUE
    )
}
