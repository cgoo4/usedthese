#' Scrape the summaries for site-wide analysis
#'
#' Harvests and consolidates function usage tables from pages of a Quarto website
#' by searching for tables with the CSS class "usedthese"
#'
#' @param url The url to the website listing page of posts containing usage tables created with
#' [used_here()]
#'
#' @param num_links The number of links returned from the listing page may be restricted using
#' this argument. Defaults to 20.
#'
#' @return A tibble summarising package & function usage
#'
#' @export
#'
#' @examples
#' # Uses a Quarto listing url to scrape & consolidate usage
#' \donttest{used_there("https://www.quantumjitter.com/project/", 1)}
#'
used_there <- \(url, num_links = 20) {
  urls <- url |>
    rvest::read_html() |>
    rvest::html_elements(".quarto-grid-link ,
                         .quarto-default-link ,
                         .quarto-table-link") |>
    rvest::html_attr("href") |>
    stringr::str_replace("^",
                         stringr::str_c(
                           httr::parse_url(url)$scheme, "://",
                           httr::parse_url(url)$hostname
                         )
    ) |>
    utils::head(num_links)

  purrr::map(urls, \(x) {
    x |>
      rvest::read_html() |>
      rvest::html_element(".usedthese") |>
      rvest::html_table() |>
      dplyr::mutate(url = x)
  }) |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    tidyr::separate_rows(Function, sep = ",") |>
    tidyr::extract(Function, c("Function", "n"),
                   "([^ ]+)\\[(.+)\\]",
                   convert = TRUE)
}
