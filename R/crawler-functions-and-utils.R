#' @importFrom assertthat assert_that
#' @importFrom assertthat is.string
#' @import stringr
#' @import dplyr
#' @import purrr
#' @importFrom tibble tibble
gen_random_interval <- function(sleep, sleep_mean_delay) {
  i <- 0
  if (sleep) {
    i <- round(rnorm(1, sleep_mean_delay, .3*sleep_mean_delay))
    if (i <= 0) i <- sample(1:3, 1)
    message("Wait: ", i, " seconds")
  }
  i
}
sleep_and_return <- function(x, ...) {
  Sys.sleep(gen_random_interval(...))
  x
}
#' Create iteration table
#'
#' @description you can probably download the data without it just iterating on
#' the vector of companies names but in case there's some anti-crawler filter
#' applied it seemed wise to create a random order iterator
#'
#' @param companies_list you can either get it from br_read_crawler_metadata()
#' or crawler_metadata tibble saved in a package. See example:
#'
#' @return tibble
#' @examples
#' data(crawler_metadata)
#' iteration_table <- construct_iteration_table(crawler_metadata$name_for_crawler)
#' @export construct_iteration_table
construct_iteration_table <- function(companies_list) {
  tibble(
    name_for_crawler = companies_list,
    id = sample(seq_along(companies_list), length(companies_list))
  ) %>%
    arrange(id)
}
#' @title Run crawler
#'
#' @param names_for_crawler vector of names to iterate on. See examples.
#' @param .f a function to use for iteration. You can use all functions with a
#' prefix "bri_". As of now only bri_get_financial_report is available
#' @param dir a directory name where the crawler results should be stored.
#' Default to "crawler_results"
#' @param sleep TRUE/FALSE whether between crawler requests should be a pause.
#' Default to TRUE as I don't know if there is some anti-crawler filter here
#' at play
#' @param sleep_mean_delay an average time (in seconds) that the crawler should
#' wait to get another company's data. Pause times are randomly distributed.
#'
#' @return nothing xd
#' @export br_run_crawler
#'
#' @examples
#' data(crawler_metadata)
#' vector_of_names <- crawler_metadata$name_for_crawler
#' br_run_crawler(
#'   vector_of_names,
#'   bri_get_pnl,
#'   dir = "gpw_pnls"
#' )
#' # ... here's where you wait
#' all_reports_data <- br_read_crawler_data("gpw_pnls")
br_run_crawler <- function(names_for_crawler,
                           .f,
                           dir = "crawler_results",
                           sleep = TRUE,
                           sleep_mean_delay = 10,
                           ...) {
  assert_that(
    is.character(names_for_crawler),
    is.function(.f),
    is.string(dir)
  )
  if (!dir.exists(dir)) dir.create(dir)
  safeget <- safely(.f, otherwise = tibble())
  walk(
    names_for_crawler,
    ~safeget(.x, ...) %>%
      sleep_and_return(sleep, sleep_mean_delay) %>%
      chuck("result") %>%
      mutate(name_for_crawler = .x) %>%
      saveRDS(glue("{dir}/{.x}.rds"))
  )
}
#' @export br_inspect_crawler_errors
br_inspect_crawler_errors <- function(filename) {
  assert_that(
    is.string(dir),
    dir.exists(dir)
  )
  list.files(dir, full.names = TRUE) %>%
    map(
      ~readRDS(.x) %>%
        chuck("error") %>%
        pluck(name = str_remove(.x, "\\.rds$"))
    )
}
#' @title Read crawler results
#'
#' @param dir a directory name where crawler results are stored
#'
#' @return a tibble with all results
#' @export br_read_crawler_results
br_read_crawler_results <- function(dir) {
  assert_that(
    is.string(dir),
    dir.exists(dir)
  )
  list.files(dir, full.names = TRUE) %>%
    map_dfr(
      ~readRDS(.x) %>%
        chuck("result") %>%
        mutate(name_for_crawler = str_remove(.x, "\\.rds$"))
    ) %>%
    left_join(crawler_metadata, by = "name_for_crawler")
}
