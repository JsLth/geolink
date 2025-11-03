#' Natural Earth geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link  geometries from Natural Earth
#' (\url{https://www.naturalearthdata.com/}). Provides no functions for validating
#' and parsing as Natural Earth does not provide its own admin IDs.
#'
#' Depending on the resolution selected, requires the \code{rnaturalearthdata}
#' or \code{rnaturalearthhires} packages to be installed.
#'
#' @inheritParams enrich
#' @param resolution Resolution of the polygon geometries. Can be 10
#'   (1:10m), 50 (1:50m) or 110 (1:110m). A resolution of 1:10 requires
#'   \href{https://docs.ropensci.org/rnaturalearthhires/}{\code{rnaturalearthhires}}
#'   package to be installed. Otherwise, requires the
#'   \href{https://docs.ropensci.org/rnaturalearthdata/}{\code{rnaturalearthdata}}
#'   package. Note that at lower resolutions, some tiny countries disappear.
#' @param ... Not used.
#'
#' @returns
#' An sf dataframe containing \code{.data} and an added geometry column.
#'
#' @export
#' @name naturalearth
#'
#' @examples \donttest{# Monaco cannot be matched at resolution = 110
#' enrich("MCO", linker = "naturalearth")
#'
#' # But on higher resolutions, it can
#' enrich("MCO", linker = "naturalearth", resolution = 50)}
naturalearth_link <- function(.data, id_col, resolution = 110) {
  if (identical(resolution, "10")) {
    check_installed("rnaturalearthhires", "to use high-resolution data from Natural Earth")
  } else {
    check_installed("rnaturalearthdata", "to use data from Natural Earth")
  }

  reference <- switch(
    as.character(resolution),
    "110" = rbind(
      rnaturalearthdata::countries110["adm0_a3"],
      rnaturalearthdata::tiny_countries110["adm0_a3"]
    ),
    "50" = rbind(
      rnaturalearthdata::countries50["adm0_a3"],
      rnaturalearthdata::tiny_countries50["adm0_a3"]
    ),
    "10" = rnaturalearthhires::countries10["ADM0_A3"],
    cli::cli_abort("Argument `resolution` can be 110, 50, or 10, not {resolution}.")
  )

  left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = names(reference)[1]
  )
}
