geolinkers <- new.env(parent = emptyenv())
assign("src", "geolink", envir = geolinkers)


#' Register a geolinker
#' @description
#' Registers a geolinker for use in \code{\link{enrich}}. A geolinker
#' needs a name and at least one function that performs the linking.
#'
#' \code{unregister_geolinker} removes a previously registered geolinker.
#'
#' \code{all_geolinkers} lists all currently registered geolinkers in their
#' respective priority order.
#'
#' @param name Name of the geolinker. This is the string that you pass
#'   to the \code{linker} argument in \code{\link{enrich}}.
#' @param link A function that takes at least two arguments: \code{.data}
#'   and \code{col_id}, which correspond to the same arguments in
#'   \code{\link{enrich}}. The purpose of this function is to link
#'   \code{.data} with their respective geometries. Can also take an arbitrary
#'   number of additional arguments. These arguments should correspond to the
#'   output of \code{parse}.
#' @param check An optional function that takes exactly one argument, the
#'   IDs to be checked. The output should be a logical vector with the same
#'   length as the input. The purpose of this function is to check whether
#'   the IDs adhere to the format of the geolinker IDs. If \code{NULL}, the
#'   geolinker cannot be guessed by \code{enrich}.
#' @param parse An optional function that takes exactly one argument, the
#'   IDs to be parsed. The output should be a list with the same names as
#'   the additional input arguments of the \code{link} function. The purpose
#'   of this function is to extract information from the IDs to pass to the
#'   linking function.
#' @param priority If \code{check} is not \code{NULL}, specifies the guessing
#'   priority of the geolinker. For example, if \code{priority = 0},
#'   \code{\link{enrich}} will always try this geolinker first, if
#'   \code{priority = 1}, then second, etc. Can also be \code{"last"} (default)
#'   to put the geolinker at the end of the priority order.
#' @param only_guessable Whether to return only those geolinker names that
#'   have a check function. In other words, if \code{TRUE}, only those
#'   geolinkers are returned which can be guessed by \code{\link{enrich}} if
#'   no linker is specified. If \code{FALSE}, returns all geolinkers.
#'
#' @returns A list of the input functions, invisibly.
#' @export
#'
#' @examples
#' # The following functions define a geolinker that guesses, parses and links
#' # IDs of the following form:
#' # x: {x_coord}, y: {y_coord}
#'
#' check <- function(ids) {
#'   grepl("x: -?[0-9]{0,3}, y: -?[0-9]{0,2}", ids)
#' }
#'
#' parse <- function(ids) {
#'   strcapture(
#'     "x: (-?[0-9]{0,3}), y: (-?[0-9]{0,2})",
#'     ids,
#'     proto = list(x = double(), y = double())
#'   )
#' }
#'
#' link <- function(.data, id_col, x, y, link_crs) {
#'   geom <- Map(\(x, y) sf::st_point(c(x, y)), x, y)
#'   geom <- sf::st_as_sfc(geom, crs = link_crs)
#'   .data$geometry <- geom
#'   sf::st_as_sf(.data)
#' }
#'
#' register_geolinker(
#'   "coords_fmt",
#'   link = link,
#'   check = check,
#'   parse = parse,
#'   priority = 0
#' )
#'
#' .data <- data.frame(id = c("x: 8, y: 47", "x: 9, y: 46", "x: 10, y: 45"))
#' enrich(.data, link_crs = 4326)
register_geolinker <- function(name,
                               link,
                               check = NULL,
                               parse = NULL,
                               priority = "last") {
  check_string(name)
  check_function(link)
  check_function(check, null = TRUE)
  check_function(parse, null = TRUE)

  check_args(link, arg_names = ".data")
  check_args(check, arg_n = 1, null = TRUE)
  check_args(parse, arg_n = 1, null = TRUE)

  geolinker <- list(link = link, check = check, parse = parse)
  assign(name, geolinker, envir = geolinkers)

  if (!is.null(priority) && !is.null(check)) {
    order <- get0("order", envir = geolinkers)

    if (identical(priority, "last")) {
      priority <- length(order)
    }

    order <- append(order, name, after = priority)
    assign("order", order, envir = geolinkers)
  }

  invisible(geolinker)
}


#' @rdname register_geolinker
#' @export
unregister_geolinker <- function(name) {
  rm(name, envir = geolinkers)
}


#' @rdname register_geolinker
#' @export
all_geolinkers <- function(only_guessable = FALSE) {
  linkers <- setdiff(ls(envir = geolinkers), "order")
  priorities <- priorities()

  if (isTRUE(only_guessable)) {
    linkers <- intersect(linkers, priorities)
  } else {
    priorities <- c(priorities, setdiff(linkers, priorities))
  }

  linkers[match(priorities, linkers)]
}


priorities <- function() {
  get0("order", envir = geolinkers)
}


get_linker <- function(name, fun = NULL) {
  geolinker <- get(name, envir = geolinkers)

  if (!is.null(fun)) {
    geolinker[[fun]] %||% function(...) list()
  } else {
    geolinker
  }
}
