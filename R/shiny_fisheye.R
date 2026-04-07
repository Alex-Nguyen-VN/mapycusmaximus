#' Launch Interactive Fisheye Lens Explorer
#'
#' @param debug Controls whether the Debug tab is shown.
#'   Accepted values are `"off"` (default), `"on"`, `FALSE`, and `TRUE`.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return The value returned by [shiny::runApp()], called primarily
#'   for its side effect of launching the application.
#' @export
shiny_fisheye <- function(debug = "off", ...) {
  required_pkgs <- c("shiny", "tidyr", "dplyr", "purrr", "ggthemes", "sf")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "The Shiny app requires additional packages. Install them with:\n",
      "  install.packages(c(",
      paste(sprintf('"%s"', missing_pkgs), collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny_app", package = "mapycusmaximus")

  if (app_dir == "" || !dir.exists(app_dir)) {
    stop(
      "Could not find Shiny app directory. ",
      "Try re-installing the package.",
      call. = FALSE
    )
  }

  debug_mode <- isTRUE(debug) || identical(tolower(as.character(debug)), "on")

  old_opt <- getOption("mapycusmaximus.shiny_debug")
  on.exit(options(mapycusmaximus.shiny_debug = old_opt), add = TRUE)

  options(mapycusmaximus.shiny_debug = debug_mode)

  message("Launching Fisheye Lens Explorer...")

  shiny::runApp(app_dir, ...)
}