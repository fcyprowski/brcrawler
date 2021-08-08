br_endpoint_pnl <- "raporty-finansowe-rachunek-zyskow-i-strat"

create_url <- function(endpoint, company_name) {
  httr::modify_url(
    url = Sys.getenv("br_host"),
    path = glue::glue("{endpoint}/{company_name}")
  )
}

br_companies_list_url <- function() {
  httr::modify_url(Sys.getenv("br_host"), path = "gielda/akcje_gpw")
}
