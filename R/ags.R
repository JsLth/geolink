is_ags <- function(ids) {
  grepl("^[0-1]([0-9]{1})?([0-9]{1})?([0-9]{2})?([0-9]{3})?$", ids)
}


ags_parse <- function(ids) {
  ids_pad <- paste0(ids, strrep("0", 8 - nchar(ids)))
  ags_raw <- utils::strcapture(
    "([0-9]{2})([0-9]{1})([0-9]{2})([0-9]{3})",
    ids_pad,
    list(lan = 0, rbz = 0, krs = 0, gem = 0)
  )

  ags <- ags_raw
  is_city_state <- ags$lan %in% c(2, 11)
  for (i in seq_along(ags)) {
    ags[[i]] <- ags[[i]] > 0

    if (any(is_city_state)) {
      ags[[i]][is_city_state] <- TRUE
    }
  }

  # Include catch-all level "sta" that is selected when all other levels
  # are FALSE, i.e. 00000000 (Germany)
  ags <- cbind(sta = TRUE, ags)

  level <- unlist(.mapply(ags, MoreArgs = NULL, FUN = function(...) {
    names(which.max(rev(c(...))))
  }))

  # Assign the most common level to Berlin and Hamburg
  level[ags_raw$lan %in% c(2, 11)] <- names(which.max(table(level)))

  list(country = "DEU", level = unique(level), ags = ids)
}


ags_link <- function(.data, id_col, country, level, ags, ...) {
  for (lvl in level) {
    reference <- ffm::bkg_admin(
      level = lvl,
      ags == ags,
      ...
    )

    if (!nrow(reference)) {
      cli::cli_abort("No AGS input IDs match any AGS at level {level}.")
    }

    merged <- left_merge(
      .data[level == lvl, , drop = FALSE],
      reference,
      by.x = id_col,
      by.y = "ags"
    )
    .data[level == lvl, names(merged)] <- merged
  }

  as_sf_tibble(.data, crs = sf::st_crs(reference))
}
