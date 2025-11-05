#' geoBoundaries geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link geometries from geoBoundaries
#' (\url{https://www.geoboundaries.org/}). Provides no functions for validating
#' and parsing as geoBoundaries does not provide its own admin IDs.
#'
#' Requires the \href{https://github.com/wmgeolab/rgeoboundaries}{rgeoboundaries}
#' package to be installed.
#'
#' @param .data A dataframe with at least one column named according to the
#'   \code{id_col} argument.
#' @param id_col Column in \code{.data} holding the spatial identifiers
#'   in question.
#' @param country Character vector of unique ISO-3 country codes contained
#'   by \code{ids}. Only geometries within these countries are returned.
#' @param ... Further arguments passed to
#'   \code{\link[rgeoboundaries]{geoboundaries}}.
#'
#' @returns
#' An sf dataframe containing \code{.data} and an added geometry column.
#'
#' @export
#' @name geoboundaries
#'
#' @examplesIf getFromNamespace("run_examples", ns = "geolink")()
#' # Link Monaco
#' enrich("MCO", linker = "geoboundaries")
gb_link <- function(.data, id_col, country, ...) {
  check_installed("wmgeolab/rgeoboundaries", "to use data from geoBoundaries")
  geoboundaries <- getExportedValue("rgeoboundaries", "geoboundaries")

  reference <- list()
  for (cnt in country) {
    reference[[cnt]] <- tryCatch(
      geoboundaries(
        cnt,
        adm_lvl = "adm0"
      )[c("shapeISO", "geometry")],
      error = function(e) {
        sf::st_sf(
          shapeISO = cnt,
          geometry = sf::st_sfc(make_empty_geometry("MULTIPOLYGON")),
          crs = 4326
        )
      }
    )
  }
  reference <- do.call(rbind, reference)

  left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = "shapeISO"
  )
}
