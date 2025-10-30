is_inspire <- function(ids) {
  is_short_inspire(ids) | is_long_inspire(ids)
}


is_short_inspire <- function(ids) {
  grepl("[0-9]+k?mN[0-9]+E[0-9]+", ids)
}


is_long_inspire <- function(ids) {
  grepl("CRS[0-9]+RES[0-9]+mN[0-9]+E[0-9]+", ids)
}


inspire_parse <- function(ids) {
  if (all(startsWith(ids, "CRS"))) {
    parsed <- utils::strcapture(
      "^CRS([0-9]+)RES([0-9]+)mN([0-9]+)E([0-9]+)$",
      x = ids,
      proto = list(
        crs = integer(), res = numeric(),
        y = integer(), x = integer()
      )
    )
  }
  else {
    parsed <- utils::strcapture(
      "^([0-9]+k?m)N([0-9]+)E([0-9]+)$",
      x = ids,
      proto = list(
        res = character(), y = integer(), x = integer()
      )
    )
    parsed$res <- res_to_m(parsed$res)
  }

  parsed$x <- parsed$x * parsed$res + parsed$res/2
  parsed$y <- parsed$y * parsed$res + parsed$res/2

  crs <- unique(parsed$crs) %||% 3035
  if (length(crs) > 1) {
    cli::cli_abort(c(
      "Found more than one CRS in the provided INSPIRE IDs.",
      "i" = "Make sure all INSPIRE IDs come from the same source."
    ))
  }

  res <- unique(parsed$res)
  if (length(crs) > 1) {
    cli::cli_abort(c(
      "Found more than one grid resolution in the provided INSPIRE IDs.",
      "i" = "Make sure all INSPIRE IDs come from the same source."
    ))
  }

  list(crs = crs, res = res, x = parsed$x, y = parsed$y, ids = ids)
}


inspire_link <- function(.data, id_col, crs, res, x, y, ids) {
  grid <- .mapply(create_square, list(x, y), MoreArgs = list(size = res))
  grid <- sf::st_as_sfc(grid, crs = crs)
  reference <- sf::st_sf(ids = ids, geometry = grid)

  left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = "ids"
  )
}


res_to_m <- function(res) {
  is_km <- grepl("(?<=[0-9])km", res, perl = TRUE)
  numbers <- as.numeric(regex_match(res, "^[0-9]+", i = 1))
  numbers <- ifelse(is_km, numbers * 1000, numbers)
  numbers
}


create_square <- function(x, y, size) {
  half <- size / 2
  coords <- rbind(
    c(x - half, y - half),
    c(x + half, y - half),
    c(x + half, y + half),
    c(x - half, y + half),
    c(x - half, y - half)  # MUST close the polygon
  )
  sf::st_polygon(list(coords))
}
