is_lau <- function(ids) {
  grepl("[A-Z]{2}_[0-9]+", ids)
}


lau_parse <- function(ids) {
  lau <- utils::strcapture(
    "([A-Z]{2})_[0-9]+",
    ids,
    proto = list(country = "")
  )

  list(
    country = unique(lau$country),
    gisco_ids = ids
  )
}


lau_link <- function(.data, id_col, country, gisco_ids, ...) {
  reference <- giscoR::gisco_get_lau(
    country = country,
    gisco_id = args$gisco_id,
    ...
  )

  .data <- left_merge(
    .data,
    reference,
    by.x = ids,
    by.y = "GISCO_ID"
  )
}
