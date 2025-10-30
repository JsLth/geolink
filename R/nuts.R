is_nuts <- function(ids) {
  grepl("^[A-Z]{2}[A-Z0-9]{1,3}$", ids)
}


nuts_parse <- function(ids) {
  nuts <- utils::strcapture(
    "^([A-Z]{2})([A-Z0-9])?([A-Z0-9])?([A-Z0-9])?$",
    ids,
    proto = list(country = "", level1 = "", level2 = "", level3 = "")
  )

  nuts$level1 <- nzchar(nuts$level1)
  nuts$level2 <- nzchar(nuts$level2)
  nuts$level3 <- nzchar(nuts$level3)
  level <- nuts$level1 + nuts$level2 + nuts$level3

  list(
    country = unique(nuts$country),
    level = unique(level),
    nuts_ids = ids
  )

  data.frame(
    country = nuts$country,
    level = ,
    nuts_id = ids
  )
}


nuts_link <- function(.data, id_col, country, level, nuts_ids, ...) {
  for (lvl in level) {
    reference <- giscoR::gisco_get_nuts(
      country = country,
      nuts_level = lvl,
      nuts_id = nuts_ids,
      ...
    )

    merged <- left_merge(
      .data[level == lvl, , drop = FALSE],
      reference,
      by.x = id_col,
      by.y = "NUTS_ID"
    )
    .data[level == lvl, names(merged)] <- merged
  }

  as_sf_tibble(.data, crs = sf::st_crs(reference))
}
