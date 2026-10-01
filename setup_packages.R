setup_pkgs <- 'options(repos = c(CRAN = "https://cloud.r-project.org"))

required <- c(
  "shiny", "bslib", "DT", "plotly", "data.table",
  "dplyr", "tidyr", "openxlsx", "xml2", "shinyjs",
  "shinyFiles", "jsonlite", "htmltools", "readr"
)

cat("\\n  R version:", R.version.string, "\\n")

rv <- getRversion()
if (rv < "4.2.0") {
  cat("\\n  WARNING: R", as.character(rv), "detected.\\n")
  cat("  IVDrDataManager is tested on R 4.2 or newer.\\n\\n")
}

lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(lib) && !dir.exists(lib)) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  cat("  Created personal library:", lib, "\\n")
}

installed <- rownames(installed.packages())
missing   <- setdiff(required, installed)

if (length(missing) == 0) {
  cat("\\n  All", length(required), "packages already installed.\\n")
} else {
  cat("\\n  Installing", length(missing), "package(s):\\n")
  cat("   ", paste(missing, collapse = ", "), "\\n\\n")

  for (p in missing) {
    cat("  ->", p, "... ")
    ok <- tryCatch({
      install.packages(p, quiet = TRUE, dependencies = TRUE)
      requireNamespace(p, quietly = TRUE)
    }, error = function(e) FALSE)
    cat(if (isTRUE(ok)) "ok\\n" else "FAILED\\n")
  }
}

cat("\\n  Verifying...\\n")
installed <- rownames(installed.packages())
still_missing <- setdiff(required, installed)

if (length(still_missing) > 0) {
  cat("\\n  ERROR - could not install:\\n")
  cat("   ", paste(still_missing, collapse = ", "), "\\n\\n")
  cat("  Common causes:\\n")
  cat("    - no internet connection or a proxy is blocking CRAN\\n")
  cat("    - antivirus blocking downloads\\n")
  cat("    - no write permission to the library folder\\n\\n")
  cat("  Try running install.bat as Administrator.\\n\\n")
  quit(status = 1)
}

cat("  All packages ready.\\n\\n")
quit(status = 0)
'

writeLines(setup_pkgs, "setup_packages.R")
cat("Wrote setup_packages.R\n")
