#' @import stringr
#' @import dplyr
#' @import purrr
#' @importFrom tibble as_tibble
#' @importFrom glue glue
parse_colnames <- function(df) {
  nms <- colnames(df)[2:length(df)]
  okres <- str_extract(nms, "[a-z]{3} [0-9]{2}")
  type <- str_extract(nms, "O?[0-9]+[A-Z]?")
  glue("{type}_{okres}") %>%
    str_replace_all(" ", "_")
}
parse_table <- function(df) {
  df %>%
    set_names(
      c("var", parse_colnames(df))
    ) %>%
    select(1:(length(.)-1)) %>%
    mutate(
      across(-1, partial(str_remove_all, pattern = "r.+")),
      across(-1, partial(str_remove_all, pattern = " "))
    ) %>%
    t() %>%
    as_tibble(rownames = "report_type") %>%
    set_names(.[1, ]) %>%
    slice(-1) %>%
    mutate(
      across(2, lubridate::as_date),
      across(3:length(.), as.numeric)
    )
}
bri_get_pnl <- function(name_for_crawler) {
  message("Company: ", name_for_crawler)
  create_url(
    br_endpoint_pnl, name_for_crawler
  ) %>%
    read_html() %>%
    html_elements("table") %>%
    magrittr::extract2(2) %>%
    html_table() %>%
    parse_table()
}
