import_packages <- function(pkgs, repos = "https://cloud.r-project.org") {

  if (!is.character(pkgs)) {
    stop("Invalid pkgs inputs.")
  }

  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs)) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, repos = repos)
  }

  invisible(
    lapply(pkgs, function(pkg) {
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
    })
  )
}


source_dir <- function(dirpath,
                       recursive = FALSE,
                       echo = FALSE,
                       verbose = TRUE) {

  stopifnot(dir.exists(dirpath))

  files <- list.files(
    path = dirpath,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = recursive
  )

  if (length(files) == 0) {
    warning("No .R files found in: ", dirpath)
    return(invisible(NULL))
  }

  for (f in files) {
    if (verbose) logmsg(paste0("Sourcing: ", f))
    suppressPackageStartupMessages(
        source(f, echo = echo)
    )
  }

  invisible(files)
}
