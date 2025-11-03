#' INSPIRE geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link, parse and validate grid geometries from
#' Eurostat's Infrastructure for Spatial Information in the
#' European Community (INSPIRE,
#' \url{https://ec.europa.eu/eurostat/web/gisco/geodata/grids}).
#'
#' @param ids A character vector of IDs.
#' @inheritParams enrich
#' @param x,y X and Y coordinates used to create the grid.
#' @param res Cell size of the grid in meters.
#' @param crs EPSG code of the CRS that \code{x} and \code{y} use.
#' @param ... Not used
#'
#' @returns
#' \code{is_inspire}: A logical vector of length \code{length(ids)}.
#'
#' \code{inspire_parse}: A list with fields \code{country} and \code{level}.
#'
#' \code{inspire_link}: An sf dataframe containing \code{.data} and an added
#' geometry column.
#'
#' @export
#' @name inspire
#'
#' @examplesIf
#' \donttest{# Link grid cells
#' enrich(c("CRS3035RES100mN1E2", "CRS3035RES100mN2E2"))}
inspire_link <- function(.data, id_col, x, y, res, crs = 3035, ...) {
  grid <- .mapply(create_square, list(x, y), MoreArgs = list(size = res))
  grid <- sf::st_as_sfc(grid, crs = crs)
  reference <- sf::st_sf(ids = .data[[id_col]], geometry = grid)

  left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = "ids"
  )
}


#' @rdname inspire
#' @export
is_inspire <- function(ids) {
  is_short_inspire(ids) | is_long_inspire(ids)
}


#' @rdname inspire
#' @export
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


is_short_inspire <- function(ids) {
  grepl("[0-9]+k?mN[0-9]+E[0-9]+", ids)
}


is_long_inspire <- function(ids) {
  grepl("CRS[0-9]+RES[0-9]+mN[0-9]+E[0-9]+", ids)
}
