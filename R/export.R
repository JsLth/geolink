#' Export and import geolinkers
#' @description
#' \code{export_geolinkers} exports a database of geolinkers registered
#' using \code{register_geolinkers} to a file.
#'
#' \code{import_geolinkers} imports an exported database of geolinkers to
#' the current R session.
#'
#' \strong{Note:} To prevent unexpected behavior, \code{import_geolinkers}
#' \emph{always} overwrites all existing geolinkers in a session. In
#' interactive sessions, the function will warn about this behavior.
#'
#' @param path A directory path where to store the geolinkers.
#' @param name The name of the exported file. If \code{NULL}, generates
#'   a random name.
#' @param file A file path to import. Must point to a file previously created
#'   by \code{export_geolinkers}.
#' @param ... Further arguments passed to \code{\link{saveRDS}}.
#'
#' @returns \code{export_geolinkers} returns the output file path invisibly.
#' \code{import_geolinkers} returns \code{NULL} invisibly.
#'
#' @export
#'
#' @examples
#' # in session A:
#' geodir <- tempdir()
#' export_geolinkers(geodir, name = "example")
#'
#' # in session B:
#' import_geolinkers(file.path(geodir, "example"))
export_geolinkers <- function(path, name = NULL, ...) {
  check_string(name)

  if (is.null(name)) {
    out_path <- tempfile("geolink", tmpdir = path)
  } else {
    out_path <- file.path(path, name)
  }

  saveRDS(geolinkers, file = out_path, ...)
}


#' @rdname export_geolinkers
#' @export
import_geolinkers <- function(file) {
  new <- readRDS(file)

  if (interactive()) {
    info(paste(
      "You are about to import a database of geolinkers.",
      "Do you wish to overwrite all geolinkers?"
    ))
    if (menu(c("Yes", "No")) != 1) {
      return(invisible())
    }
  }

  overwrite_env(geolinkers, name)
  NULL
}
