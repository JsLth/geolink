fips_link <- function(.data, id_col) {
  geom <- fipio::fips_geometry(.data[[id_col]])
  reference <- sf::st_sf(fips = .data[[id_col]], geometry = geom)
  left_merge(.data, reference, by.x = id_col, by.y = "fips")
}
