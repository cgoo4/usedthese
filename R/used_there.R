#' Scrape the summaries for site-wide analysis
#'
#' Harvests and aggregates the function usage tables from multiple pages in a website
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
#' # Uses a Quarto listing url to aggregate usage across website pages
#' used_there("https://www.quantumjitter.com/project/", 1)
#'
used_there <- \(url, num_links = 20){
  urls <- url |>
    rvest::read_html() |>
    rvest::html_elements(".quarto-grid-link ,
                         .quarto-default-link ,
                         .quarto-table-link") |>
    rvest::html_attr("href")

  if (num_links < length(urls)) {
    urls <- urls[1:num_links]
  }

  purrr::map(urls, \(x) {
    stringr::str_c(
      httr::parse_url(url)$scheme, "://",
      httr::parse_url(url)$hostname, x
    ) |>
      rvest::read_html() |>
      rvest::html_element(".usedthese") |>
      rvest::html_table() |>
      dplyr::mutate(url = x)
  }) |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    tidyr::separate_rows(Function, sep = ";") |>
    tidyr::extract(Function, c("Function", "n"),
                   "([^ ]+)\\[(.+)\\]",
                   convert = TRUE
    )
}
