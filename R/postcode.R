postcode_link <- function(.data, id_col, ...) {
  if (!"username" %in% ...names()) {
    cli::cli_abort(c(
      "If `linker = \"postcode\"`, the `username` argument must be provided.",
      "i" = paste(
        "You must be registered for the GeoNames",
        "({.url https://www.geonames.org/export/web-services.html})",
        "web service to query postcodes."
      )
    ))
  }

  reference <- lapply(.data[[id_col]], function(id) {
    args <- c(list(postalcode = id, maxRows = 1), ...)
    res <- suppressWarnings(do.call(geonames::GNpostalCodeSearch, args))
    if (is.null(res$lng) || is.null(res$lat)) {
      sf::st_point()
    } else {
      sf::st_point(as.numeric(c(res$lng, res$lat)))
    }
  })
  reference <- sf::st_sf(
    postcode = .data[[id_col]],
    geometry = sf::st_as_sfc(reference),
    crs = 4326
  )

  left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = "postcode"
  )
}
