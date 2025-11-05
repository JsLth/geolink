.onLoad <- function(libname, pkgname) {
  register_geolinker("gadm", gadm_link, is_gadm, gadm_parse, priority = 0)
  register_geolinker("unhcr", unhcr_link, is_unhcr, unhcr_parse, priority = "last")
  register_geolinker("nuts", nuts_link, is_nuts, nuts_parse, priority = "last")
  register_geolinker("inspire", inspire_link, is_inspire, inspire_parse, priority = "last")
  register_geolinker("lau", lau_link, is_lau, lau_parse, priority = "last")
  register_geolinker("ags", ags_link, is_ags, ags_parse, priority = "last")
  register_geolinker("naturalearth", naturalearth_link)
  register_geolinker("geoboundaries", gb_link)
  register_geolinker("postcode", postcode_link)
  register_geolinker("fips", fips_link)
}
