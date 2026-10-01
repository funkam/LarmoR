options(repos = c(CRAN = "https://cloud.r-project.org"))

# CRAN packages required by LarmoR
required <- c(
  "shiny", "bslib", "DT", "plotly", "data.table",
  "dplyr", "openxlsx", "xml2", "shinyjs", "jsonlite",
  "htmltools", "future", "future.apply", "readxl",
  "fs", "htmlwidgets", "rstudioapi"
)

# Bioconductor packages
bioc_required <- c("PepsNMR")

cat("\n  R version:", R.version.string, "\n")

rv <- getRversion()
if (rv < "4.2.0") {
  cat("\n  WARNING: R", as.character(rv), "detected.\n")
  cat("  LarmoR is tested on R 4.2 or newer.\n\n")
}

lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(lib) && !dir.exists(lib)) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  cat("  Created personal library:", lib, "\n")
}

installed <- rownames(installed.packages())
missing   <- setdiff(required, installed)

if (length(missing) == 0) {
  cat("\n  All", length(required), "CRAN packages already installed.\n")
} else {
  cat("\n  Installing", length(missing), "CRAN package(s):\n")
  cat("   ", paste(missing, collapse = ", "), "\n\n")

  for (p in missing) {
    cat("  ->", p, "... ")
    ok <- tryCatch({
      install.packages(p, quiet = TRUE, dependencies = TRUE)
      requireNamespace(p, quietly = TRUE)
    }, error = function(e) FALSE)
    cat(if (isTRUE(ok)) "ok\n" else "FAILED\n")
  }
}

# ---- Bioconductor ----
bioc_missing <- setdiff(bioc_required, rownames(installed.packages()))

if (length(bioc_missing) > 0) {
  cat("\n  Installing Bioconductor package(s):\n")
  cat("   ", paste(bioc_missing, collapse = ", "), "\n\n")

  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    cat("  -> BiocManager ... ")
    tryCatch({
      install.packages("BiocManager", quiet = TRUE)
      cat("ok\n")
    }, error = function(e) cat("FAILED\n"))
  }

  if (requireNamespace("BiocManager", quietly = TRUE)) {
    for (p in bioc_missing) {
      cat("  ->", p, "(Bioconductor) ... ")
      ok <- tryCatch({
        BiocManager::install(p, ask = FALSE, update = FALSE, quiet = TRUE)
        requireNamespace(p, quietly = TRUE)
      }, error = function(e) FALSE)
      cat(if (isTRUE(ok)) "ok\n" else "FAILED\n")
    }
  }
}

cat("\n  Verifying...\n")
installed <- rownames(installed.packages())
still_missing <- setdiff(c(required, bioc_required), installed)

if (length(still_missing) > 0) {
  cat("\n  ERROR - could not install:\n")
  cat("   ", paste(still_missing, collapse = ", "), "\n\n")
  cat("  Common causes:\n")
  cat("    - no internet connection or a proxy is blocking CRAN\n")
  cat("    - antivirus blocking downloads\n")
  cat("    - no write permission to the library folder\n")
  cat("    - no binary available for this R version (needs Rtools)\n\n")
  cat("  Try running install.bat as Administrator.\n\n")
  quit(status = 1)
}

cat("  All packages ready.\n\n")
quit(status = 0)
