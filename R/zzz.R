.onLoad <- function(libname, pkgname) {
  register_geolinker("gadm", gadm_link, is_gadm, gadm_parse, priority = 0)
  register_geolinker("nuts", nuts_link, is_nuts, nuts_parse, priority = "last")
  register_geolinker("inspire", inspire_link, is_inspire, inspire_parse, priority = "last")
  register_geolinker("lau", lau_link, is_lau, lau_parse, priority = "last")
  register_geolinker("ags", ags_link, is_ags, ags_parse, priority = "last")
  register_geolinker("postcode", postcode_link, priority = "last")
}
