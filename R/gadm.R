is_gadm <- function(ids) {
  grepl("^([A-Z]{3})(\\.[0-9]+){1,4}_[0-9]{1}?$", ids)
}


gadm_parse <- function(ids) {
  gadm <- utils::strcapture(
    "^([A-Z]{3})(\\.[0-9]+)?(\\.[0-9]+)?(\\.[0-9]+)?(\\.[0-9]+)?_[0-9]{1}?$",
    ids,
    proto = list(country = "", level1 = "", level2 = "", level3 = "", level4 = "")
  )

  is_lvl <- startsWith(names(gadm), "level")
  gadm[is_lvl] <- lapply(gadm[is_lvl], nzchar)

  list(
    country = unique(gadm$country),
    level = unique(do.call(psum, gadm[is_lvl]))
  )
}


gadm_link <- function(.data, id_col, country, level, ...) {
  for (lvl in level) {
    gid_col <- paste0("GID_", lvl)
    reference <- sf::st_as_sf(geodata::gadm(
      country,
      level = lvl,
      path = tempdir(),
      quiet = FALSE,
      ...
    ))[c(gid_col, "geometry")]

    merged <- left_merge(
      .data[level == lvl, , drop = FALSE],
      reference,
      by.x = id_col,
      by.y = gid_col
    )
    .data[level == lvl, names(merged)] <- merged
  }

  as_sf_tibble(.data, crs = sf::st_crs(reference))
}
