#' Aggregate multi-table usage
#'
#' Harvests and aggregates the function usage tables from multiple pages in a website
#'
#' @param url The url to the website listing page of posts containing usage tables created with
#' [used_here()]
#'
#' @return A tibble summarising package & function usage
#'
#' @export
#'
#' @examples
#' # Uses a Quarto listing url to aggregate usage across website pages
#' used_there("https://www.quantumjitter.com/project/")
#'
used_there <- \(url) {
  urls <- url |>
    rvest::read_html() |>
    rvest::html_elements(".quarto-grid-link ,
                         .quarto-default-link ,
                         .quarto-table-link") |>
    rvest::html_attr("href") |>
    tibble::as_tibble() |>
    dplyr::transmute(stringr::str_c("https://",
                                    httr::parse_url(url)$hostname,
                                    value)) |>
    dplyr::pull()

  table_df <- purrr::map(urls, \(x) {
    x |>
      rvest::read_html() |>
      rvest::html_elements(".usedthese") |>
      rvest::html_table()
  }) |>
    purrr::list_flatten() |>
    purrr::list_rbind() |>
    janitor::clean_names(replace = c("io" = "")) |>
    dplyr::select(package, functn) |>
    tidyr::drop_na()

  n_url <- urls |> dplyr::n_distinct()

  table_df |>
    tidyr::separate_rows(functn, sep = ";") |>
    tidyr::separate(functn, c("functn", "count"), "\\Q[\\E") |>
    dplyr::mutate(
      count = stringr::str_remove(count, "]") |> as.integer(),
      functn = stringr::str_squish(functn),
      n_url = n_url
    ) |>
    dplyr::count(package, functn, wt = count)
}
