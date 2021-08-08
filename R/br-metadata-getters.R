#' @import stringr
#' @import dplyr
#' @import purrr
#' @importFrom rvest read_html
#' @importFrom rvest html_element
#' @importFrom rvest html_elements
#' @importFrom rvest html_attrs
#' @importFrom rvest html_table
#' @importFrom magrittr %>%

#' @title Read current prices table
#'
#' @description gets you a html table that you can use to extract current prices
#' (by simply applying html_table) or companies names required to run a crawler
#' (names in url are different both from ticker and the company name here)
#'
#' @return
#' @export br_read_crawler_metadata
br_read_crawler_metadata <- function() {
  prices_table <- br_companies_list_url() %>%
    read_html() %>%
    html_element("table.qTableFull")
  prices <- prices_table %>%
    html_table() %>%
    filter(
      !str_detect(Profil, "if\\(gdfp_async\\)\\{|Profil")
    ) %>%
    mutate(
      ticker = str_extract(Profil, "[A-Z0-9]+"),
      name = str_extract(Profil, "\\(.+") %>%
        str_remove_all("\\(|\\)"),
      name_for_crawler = br_extract_names_for_crawler(prices_table)
    )

}
br_extract_names_for_crawler <- function(prices_table) {
  prices_table %>%
    html_elements('tr') %>%
    html_elements("td") %>%
    html_elements("a") %>%
    html_attrs() %>%
    map_chr("href") %>%
    str_remove("/notowania/")
}
