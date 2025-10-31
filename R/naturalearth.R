naturalearth_link <- function(.data, id_col, resolution = "110") {
  if (identical(resolution, "10")) {
    check_installed("rnaturalearthhires", "to use high-resolution data from Natural Earth")
  } else {
    check_installed("rnaturalearthdata", "to use data from Natural Earth")
  }

  reference <- switch(
    resolution,
    "110" = rnaturalearthdata::countries110["adm0_a3"],
    "50" = rnaturalearthdata::countries50["adm0_a3"],
    "10" = rnaturalearthhires::countries10["ADM0_A3"]
  )

  left_merge(
    .data[[id_col]],
    reference,
    by.x = id_col,
    by.y = names(reference)[1]
  )
}
