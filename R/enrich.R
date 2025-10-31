#' Enrich data with geometries
#' @description Given a dataframe with spatial identifiers NUTS codes or ISO-3
#' country codes links them with their spatial representation. If you have a
#' dataframe with an identifier and you don't know which type of identifier it
#' is, this function can also guess it for you.
#'
#' @param .data A dataframe with at least one column named according to the
#'   \code{id_col} argument.
#' @param id_col Column in \code{.data} holding the spatial identifiers
#'   in question.
#' @param linker The type of spatial identifier present in
#'   \code{.data[[id_col]]}. Can be either of:
#'   \itemize{
#'    \item{\code{NULL}, in which case the geolinker is guessed based on the
#'      ID format.}
#'    \item{The name of a geolinker registered by
#'      \code{\link{register_geolinker}}. By default, this includes at least
#'      \code{"gadm"}, \code{"nuts"}, \code{"inspire"}, \code{"lau"},
#'      \code{"ags"}, \code{"fips"}, and \code{"postcode"}.}
#'    \item{A function taking the arguments \code{.data} and \code{id_col}.
#'      The output should be an \code{sf} dataframe with linked data.
#'      This is equivalent of registering a link function using
#'      \code{\link{register_geolinker}} and then passing its name to this
#'      argument.}
#'   }
#' @param country For linkers where a country is required (e.g., \code{"gadm"}
#'   or \code{"nuts"}), specifies a character vector of countries. If
#'   \code{NULL}, the countries are parsed from the ID strings. Note that
#'   some geolinkers might not have a parse function but require country
#'   specifications. In this case, this argument may be necessary.
#' @param level For linkers where a geographic level is required (e.g.,
#'   \code{"gadm"} or \code{"nuts"}), specifies a character vector of levels.
#'   If \code{NULL}, the levels are parsed from the ID strings. Note that
#'   some geolinkers might not have a parse function but require country
#'   specifications. In this case, this argument may be necessary.
#' @param iso3_scheme If \code{.data[[id_col]]} contains any country code that
#'   can be parsed by \code{\link[countrycode]{countrycode}}, specifies the
#'   country code scheme that the IDs follow, e.g. \code{"eurostat"} or
#'   \code{"un"}. See \code{\link[countrycode]{codelist}} for possible code
#'   schemes. \code{enrich} will convert them to ISO-3 and merge them using the
#'   geometry source specified in \code{iso3_default}. If \code{NULL} (default)
#'   and \code{iso3_auto} is \code{TRUE}, tries to guess the code scheme.
#'   Otherwise, leaves the IDs as-is.
#' @param iso3_auto If \code{TRUE} (default), tries to automatically convert
#'   identifiers to ISO-3 using \code{\link[countrycode]{guess_field}}. This
#'   is necessary if \code{id_col} holds any country codes specified in
#'   \code{\link[countrycode]{codelist}} (e.g., GAUL, FAO, M49, FIPS, Eurostat).
#' @param iso3_default In case \code{id_col} holds country codes that are
#'   converted to ISO-3, specifies which geometry source to use. Defaults to
#'   \code{"naturalearth"}, but can also take \code{"gadm"}. Future extensions
#'   might include \code{"geoboundaries"} and \code{"gaul"}.
#' @param crs Output coordinate reference system that merged data are
#'   transformed to. Defaults to EPSG:3035 (ETRS89 / LAEA Europe).
#' @param verbose If \code{TRUE}, prints informative messages about the
#'   guessing process.
#' @param ... Further arguments passed to the linking function.
#'
#' @returns An sf dataframe (or tibble if possible) with the original data
#'   of \code{.data} and a merged geometry column.
#'
#' @export
enrich <- function(.data,
                   id_col = "id",
                   linker = NULL,
                   country = NULL,
                   level = NULL,
                   iso3_scheme = NULL,
                   iso3_auto = TRUE,
                   iso3_default = "gadm",
                   crs = 3035,
                   verbose = TRUE,
                   ...) {
  if (is.character(.data)) {
    .data <- data.frame(.data)
    names(.data) <- id_col
  } else if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a dataframe.")
  }

  if (!id_col %in% names(.data)) {
    cli::cli_abort("Column {.field {id_col}} could not be found in `.data`.")
  }

  if (!is.null(linker)) {
    if (length(linker) != 1) {
      cli::cli_abort("Argument `linker` must be of length 1.")
    }

    supported_linkers <- all_geolinkers()
    if (!linker %in% supported_linkers) {
      cli::cli_abort(c(
        "{linker} is not registered as a linking type (yet).",
        "i" = "Supported geolinkers include: {supported_linkers}",
        "*" = "Alternatively, register a new linker using `register_geolinker()`."
      ))
    }
  }

  if ((is.null(linker) || identical(linker, "gadm")) && iso3_auto) {
    ids <- convert_to_iso3(.data[[id_col]])
  }

  if (is.null(linker)) {
    linker <- guess_linker(
      ids,
      iso3_auto = iso3_auto,
      iso3_default = iso3_default
    )
  }

  parse_fun <- get_linker(linker, "parse")
  args <- parse_fun(ids)

  if (is.null(country) && "country" %in% names(args)) {
    country <- unique(args$country)
    info(c("*" = "Detected the following {cli::qty(level)}countr{?s/ies}: {country}"))
  } else {
    args$country <- country
  }

  if (is.null(level) && "level" %in% names(args)) {
    level <- unique(args$level)
    info(c("*" = "Detected the following {cli::qty(level)}level{?s}: {level}"))
  } else {
    args$level <- level
  }

  link_fun <- get_linker(linker, "link")
  args <- c(list(.data = .data, id_col = id_col), args)
  args <- args[intersect(names(args), names(formals(link_fun)))]
  args <- c(args, ...)
  .data <- do.call(link_fun, args)

  out <- as_sf_tibble(.data)
  sf::st_transform(out, crs)
}


#' Takes a character vector and, if at least 90% of ids can be identified as
#' valid country codes, converts them to ISO-3 country codes
#' @param ids Character vector
#' @noRd
convert_to_iso3 <- function(ids) {
  cc_guess <- countrycode::guess_field(ids, min_similarity = 90)
  if (nrow(cc_guess) && !identical(cc_guess$code[1], "iso3c")) {
    type <- cc_guess$code[1]
    prob <- cc_guess$percent_of_unique_matched[1]
    info("Detected {type} codes with {prob}% certainty.")
    ids <- countrycode::countrycode(ids, origin = type, destination = "iso3c")
  }

  ids
}


guess_linker <- function(ids, iso3_auto = FALSE, iso3_default = "gadm") {
  linkers <- all_geolinkers(only_guessable = TRUE)
  linkers <- c(iso3_default, setdiff(linkers, iso3_default))

  guess <- NULL
  for (i in seq_along(linkers)) {
    tried_linker <- linkers[i]
    check_fun <- get_linker(tried_linker, "check")

    if (i == 1 && iso3_auto && all(check_fun(ids))) {
      guess <- "gadm"
      break
    }

    if (all(check_fun(ids))) {
      guess <- tried_linker
      break
    }
  }

  if (is.null(guess)) {
    cli::cli_abort(c(
      "Could not automatically detect admin type.",
      "i" = "Please use the `type` argument to manually specify the type of admin identifiers."
    ))
  }

  info(c("*" = "No ID type specified, using {.val {guess}} IDs."))
  guess
}
