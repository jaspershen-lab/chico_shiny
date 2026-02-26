# Internal helper: resolve files from installed package, or from repo `inst/`
# when running locally in development without installing the package.
chico_system_file <- function(..., must_exist = TRUE) {
  path <- system.file(..., package = "chicoshiny")
  if (nzchar(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }

  rel_path <- do.call(file.path, as.list(c(...)))
  dev_path <- file.path(getwd(), "inst", rel_path)
  if (file.exists(dev_path)) {
    return(normalizePath(dev_path, winslash = "/", mustWork = FALSE))
  }

  if (!must_exist) {
    return(dev_path)
  }

  stop(
    "Could not locate resource under package or local inst/: ",
    rel_path,
    call. = FALSE
  )
}

register_chico_www <- function() {
  resource_paths <- shiny::resourcePaths()
  target <- chico_system_file("www")
  current <- unname(resource_paths["www"])
  if (is.na(current) || !nzchar(current) || normalizePath(current, winslash = "/", mustWork = FALSE) != target) {
    shiny::addResourcePath("www", target)
  }
  invisible(TRUE)
}

register_chico_markdown_assets <- function() {
  resource_paths <- shiny::resourcePaths()
  target <- chico_system_file("markdown")
  current <- unname(resource_paths["markdown_assets"])
  if (is.na(current) || !nzchar(current) || normalizePath(current, winslash = "/", mustWork = FALSE) != target) {
    shiny::addResourcePath("markdown_assets", target)
  }
  invisible(TRUE)
}
