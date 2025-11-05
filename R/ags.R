#' AGS geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link, parse and validate geometries from the
#' German official municipality key (AGS,
#' \url{https://en.wikipedia.org/wiki/Community_Identification_Number}).
#'
#' Requires the \href{https://jslth.github.io/ffm/}{\code{ffm}}
#' package to be installed.
#'
#' @param ids A character vector of IDs.
#' @inheritParams naturalearth
#' @param level Vector of geographic levels. Can be \code{"sta"}, \code{"lan"},
#' \code{"rbz"}, \code{"krs"}, \code{"vwg"} or \code{"gem"}. See
#' \code{\link[ffm]{bkg_admin}} for details.
#' @param country Character vector of unique ISO-3 country codes contained
#'   by \code{ids}. Always \code{"DEU"}.
#' @param ... Further arguments passed \code{\link[ffm]{bkg_admin}}.
#'   Particularly attribute filters or spatial filters.
#'
#' @returns
#' \code{is_ags}: A logical vector of length \code{length(ids)}.
#'
#' \code{ags_parse}: A list with fields \code{country} and \code{level}.
#'
#' \code{ags_link}: An sf dataframe containing \code{.data} and an added
#' geometry column.
#'
#' @export
#' @name ags
#'
#' @examplesIf requireNamespace("ffm")
#' \donttest{# Link Hamburg
#' enrich("02", linker = "ags")}
ags_link <- function(.data, id_col, level, country = "DEU", ...) {
  ids_old <- .data[[id_col]]
  ids_pad <- paste0(ids_old, strrep("0", 8 - nchar(ids_old)))
  .data[[id_col]] <- ids_pad

  for (lvl in level) {
    reference <- ffm::bkg_admin(
      level = lvl,
      ags == ids_pad,
      gf == 4,
      ...
    )[c("ags", "geometry")]

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

  .data[[id_col]] <- ids_old
  as_sf_tibble(.data, crs = sf::st_crs(reference))
}


#' @rdname ags
#' @export
is_ags <- function(ids) {
  grepl("^[0-1]([0-9]{1})?([0-9]{1})?([0-9]{2})?([0-9]{3})?$", ids)
}


#' @rdname ags
#' @export
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

  list(country = "DEU", level = unique(level))
}
