#' GADM geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link, parse and validate geometries from the Database
#' of Global Administrative Areas (GADM, \url{https://gadm.org/}).
#'
#' @param ids A character vector of IDs.
#' @inheritParams naturalearth
#' @param country Character vector of unique ISO-3 country codes contained
#'   by \code{ids}. Only geometries within these countries are returned.
#' @param level Vector of geographic levels. Only geometries that adhere to
#'   these levels are returned. Can be an integer between 0 (country level)
#'   and 4 (municipality level).
#' @param ... Further arguments passed to \code{\link[geodata:gadm]{geodata::gadm}}.
#'
#' @returns
#' \code{is_gadm}: A logical vector of length \code{length(ids)}.
#'
#' \code{gadm_parse}: A list with fields \code{country} and \code{level}.
#'
#' \code{gadm_link}: An sf dataframe containing \code{.data} and an added
#' geometry column.
#'
#' @export
#' @name gadm
#'
#' @examples \donttest{enrich("MCO", linker = "gadm")}
gadm_link <- function(.data, id_col, country, level = 0, resolution = 1, ...) {
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


#' @rdname gadm
#' @export
gadm_parse <- function(ids) {
  gadm <- utils::strcapture(
    "^([A-Z]{3})(\\.[0-9]+)?(\\.[0-9]+)?(\\.[0-9]+)?(\\.[0-9]+)?(?:_[0-9]{1})?$",
    ids,
    proto = list(
      country = "",
      level1 = "",
      level2 = "",
      level3 = "",
      level4 = ""
    )
  )

  is_lvl <- startsWith(names(gadm), "level")
  gadm[is_lvl] <- lapply(gadm[is_lvl], nzchar)

  list(
    country = unique(gadm$country),
    level = unique(do.call(psum, gadm[is_lvl]))
  )
}


#' @rdname gadm
#' @export
is_gadm <- function(ids) {
  grepl("^([A-Z]{3})(\\.[0-9]+){1,4}_[0-9]{1}?$", ids)
}
