enrich <- function(.data,
                   id_col = "id",
                   linker = NULL,
                   country = NULL,
                   level = NULL,
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
  linkers <- all_geolinkers()
  linkers <- linkers[match(linkers, priorities())]
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


supported_sources <- c(
  "gadm", "naturalearth", "inspire", "nuts", "ags", "postcode"
)
