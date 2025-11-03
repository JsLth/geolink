left_merge <- function(x, y, by.x, by.y, ...) {
  idx <- match(y[[by.y]], x[[by.x]])
  matches <- !is.na(idx)
  idx <- idx[matches]
  n <- nrow(x)

  for (col in setdiff(names(y), by.y)) {
    join_col <- y[[col]][matches]
    if (inherits(join_col, "sfc")) {
      crs <- sf::st_crs(join_col)
      geom_col <- col
      type <- as.character(unique(sf::st_geometry_type(y[[col]])))[1]
      new_col <- replicate(n, make_empty_geometry(type), simplify = FALSE)
      new_col <- sf::st_as_sfc(new_col)
    } else {
      new_col <- rep(NA, n)
    }

    new_col[idx] <- join_col
    x[[col]] <- new_col
  }

  is_spatial <- any(vapply(x, inherits, "sfc", FUN.VALUE = logical(1)))
  if (is_spatial) {
    as_sf_tibble(x, crs = crs, sf_column_name = geom_col)
  } else {
    as_data_frame(x)
  }
}


psum <- function(..., na.rm=FALSE) {
  dat <- do.call(cbind, list(...))
  res <- rowSums(dat, na.rm = na.rm)
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res
}


make_empty_geometry <- function(type) {
  switch(
    type,
    POINT = sf::st_point(),
    MULTIPOINT = sf::st_multipoint(),
    LINESTRING = sf::st_linestring(),
    MULTILINESTRING = sf::st_multilinestring(),
    POLYGON = sf::st_polygon(),
    MULTIPOLYGON = sf::st_multipolygon(),
    GEOMETRYCOLLECTION = sf::st_geometrycollection()
  )
}


regex_match <- function(text, pattern, i = NULL, ...) {
  match <- regmatches(text, regexec(pattern, text, ...))
  if (!is.null(i)) {
    match <- vapply(match, FUN.VALUE = character(1), function(x) {
      if (length(x) >= i) {
        x[[i]]
      }
      else {
        NA_character_
      }
    })
  }
  match
}


#' Converts an object to an sf tibble
#' @returns An sf tibble
#' @noRd
as_sf_tibble <- function(x, ...) {
  sf::st_as_sf(as_data_frame(x), ...)
}


#' Converts an object to a tibble or dataframe
#' @returns Dataframe or tibble
#' @noRd
as_data_frame <- function(x) {
  if (is_installed("tibble")) {
    tibble::as_tibble(x)
  } else {
    as.data.frame(x)
  }
}


is_installed <- function(x) {
  suppressWarnings(suppressMessages(requireNamespace(x, quietly = TRUE)))
}


info <- function(..., .envir = parent.frame()) {
  cli::cli_inform(..., .envir = .envir)
}


overwrite_env <- function(env1, env2) {
  rm(list = ls(envir = env1), envir = env1)
  list2env(env2, envir = env1)
  invisible(env1)
}
