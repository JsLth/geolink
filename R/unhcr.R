#' UNHCR geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link, parse and validate geometries from the Office
#' of the United Nations High Commissioner for Refugees (UNHCR,
#' \url{https://data.unhcr.org/en/geoservices/}).
#'
#' @param ids A character vector of IDs.
#' @inheritParams enrich
#' @param country Character vector of unique ISO-3 country codes contained
#'   by \code{ids}. Only geometries within these countries are returned.
#' @param level Vector of geographic levels. Only geometries that adhere to
#'   these levels are returned. Can be an integer between 0 (country level)
#'   and 2 (regional level).
#' @param resolution Resolution of the polygon geometries. Can be 1
#'   (1:1,000,000), 15 (1:15,000,000) or 25 (1:25,000,000). Ignored if
#'   \code{level != 0}.
#' @param ... Further arguments passed to the query endpoint of the
#'   UNHCR API. See the \href{https://gis.unhcr.org/arcgis/sdk/rest/index.html#/Query_Feature_Service_Layer/02ss0000002r000000/}{documentation}
#'   for details.
#'
#' @returns
#' \code{is_unhcr}: A logical vector of length \code{length(ids)}.
#'
#' \code{unhcr_parse}: A list with fields \code{country} and \code{level}.
#'
#' \code{unhcr_link}: An sf dataframe containing \code{.data} and an added
#' geometry column.
#'
#' @export
#' @name unhcr
#'
#' @examples \donttest{enrich("MCO", linker = "unhcr")}
unhcr_link <- function(.data,
                       id_col,
                       country = NULL,
                       level = 0,
                       resolution = c("1", "15", "25"),
                       ...) {
  check_installed("httr2", "to retrieve UNHCR geometries")

  for (lvl in level) {
    refcode <- if (lvl == 0) "iso3" else "pcode"
    reference <- unhcr_query(
      country = country,
      ids = .data[[id_col]],
      level = lvl,
      resolution = resolution,
      ...
    )[c(refcode, "geometry")]

    merged <- left_merge(
      .data[level == lvl, , drop = FALSE],
      reference,
      by.x = id_col,
      by.y = refcode
    )
    .data[level == lvl, names(merged)] <- merged
  }

  as_sf_tibble(.data, crs = sf::st_crs(reference))
}


#' @rdname unhcr
#' @export
is_unhcr <- function(ids) {
  grepl("^(?:([0-9]{2})([A-Z]{3})([0-9]{3})([0-9]{3})?|([A-Z]{3}))$", ids)
}


#' @rdname unhcr
#' @export
unhcr_parse <- function(ids) {
  unhcr <- utils::strcapture(
    "^([0-9]{2})?([A-Z]{3})?([0-9]{3})?([0-9]{3})?$",
    ids,
    proto = list(prefix = "", country = "", adm1 = "", adm2 = "")
  )

  level <- ifelse(
    nzchar(unhcr$adm2) && !is.na(unhcr$adm2),
    2,
    ifelse(
      nzchar(unhcr$adm1) && !is.na(unhcr$adm1),
      1,
      ifelse(nzchar(unhcr$country) && !is.na(unhcr$country), 0, NA)
    )
  )

  list(
    country = unique(unhcr$country),
    level = unique(level),
    ids = ids
  )
}


unhcr_api <- "https://gis.unhcr.org/arcgis/rest/services/core_v2"


unhcr_query <- function(country = NULL, ids = NULL, level = 0, resolution = c("1", "15", "25")) {
  resolution <- match.arg(resolution)
  out_fields <- c(
    "iso3",
    if (level > 0) "pcode",
    if (level > 1) "adm1_pcode"
  )

  query <- ""
  if (!is.null(country)) {
    query <- sprintf(
      "iso3 IN (%s)",
      paste(sQuote(country, q = FALSE), collapse = ",")
    )
  }

  if (!is.null(ids) && level > 0) {
    query <- paste(
      query, if (nzchar(query)) "AND",
      sprintf(
        "pcode IN (%s)",
        paste(sQuote(ids, q = FALSE), collapse = ",")
      )
    )
  }

  endpoint <- switch(
    as.character(level),
    "0" = sprintf("wrl_polbnd_int_%sm_a_unhcr", resolution),
    "1" = "wrl_polbnd_adm1_a_unhcr",
    "2" = "wrl_polbnd_adm2_a_unhcr"
  )

  req <- httr2::request(unhcr_api) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_url_path_append("FeatureServer/0/query") |>
    httr2::req_url_query(
      where = trimws(query),
      returnGeometry = TRUE,
      f = "geojson",
      outFields = paste(out_fields, collapse = ",")
    )

  resp <- httr2::req_perform(req) |>
    httr2::resp_body_string()

  sf::read_sf(resp)
}
