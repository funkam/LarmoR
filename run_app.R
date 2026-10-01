run_app <- 'app_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) normalizePath(dirname(f)) else getwd()
}, error = function(e) getwd())

setwd(app_dir)

log_file <- file.path(app_dir, "error_log.txt")

log_msg <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...)
  cat(msg, "\\n", sep = "")
  try(cat(msg, "\\n", file = log_file, append = TRUE, sep = ""), silent = TRUE)
}

# Fresh log each session
try(unlink(log_file), silent = TRUE)
log_msg("LarmoR starting")
log_msg("Directory: ", app_dir)
log_msg(R.version.string)

# --- Dependency check --------------------------------------------------
# Must match setup_packages.R
required <- c("shiny", "bslib", "DT", "plotly", "data.table",
              "dplyr", "openxlsx", "xml2", "shinyjs", "jsonlite",
              "htmltools", "future", "future.apply", "readxl",
              "fs", "htmlwidgets", "rstudioapi", "PepsNMR")

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing)) {
  log_msg("ERROR - missing packages: ", paste(missing, collapse = ", "))
  cat("\\n")
  cat("  Missing R packages:\\n")
  cat("   ", paste(missing, collapse = ", "), "\\n\\n")
  cat("  Please run install.bat to fix this.\\n\\n")
  quit(status = 1)
}

suppressPackageStartupMessages(library(shiny))

# --- Pick a free port --------------------------------------------------
find_port <- function(candidates = c(4321, 4322, 4323, 5555, 6060, 7070)) {
  for (p in candidates) {
    con <- suppressWarnings(try(
      socketConnection("127.0.0.1", port = p, server = FALSE,
                       blocking = FALSE, open = "r+", timeout = 1),
      silent = TRUE
    ))
    if (inherits(con, "try-error")) return(p)   # nothing listening -> free
    close(con)
  }
  sample(8000:8999, 1)
}

port <- find_port()
url  <- paste0("http://127.0.0.1:", port)

log_msg("Port: ", port)

cat("\\n")
cat("  ---------------------------------------------------------------\\n")
cat("   Ready at: ", url, "\\n")
cat("  ---------------------------------------------------------------\\n")
cat("\\n")

# Open browser shortly after the server comes up
later_open <- function() {
  Sys.sleep(2)
  try(utils::browseURL(url), silent = TRUE)
}
if (requireNamespace("later", quietly = TRUE)) {
  later::later(later_open, delay = 2)
} else {
  try(utils::browseURL(url), silent = TRUE)
}

options(
  shiny.launch.browser = FALSE,
  shiny.port           = port,
  shiny.host           = "127.0.0.1",
  shiny.maxRequestSize = 500 * 1024^2,
  warn                 = 1
)

result <- tryCatch({
  shiny::runApp(appDir = app_dir, port = port, host = "127.0.0.1",
                launch.browser = FALSE, quiet = FALSE)
  NULL
}, error = function(e) e)

if (!is.null(result)) {
  log_msg("FATAL: ", conditionMessage(result))
  cat("\\n")
  cat("  ---------------------------------------------------------------\\n")
  cat("   Error: ", conditionMessage(result), "\\n")
  cat("  ---------------------------------------------------------------\\n")
  cat("\\n")
  quit(status = 1)
}

log_msg("Shut down normally")
quit(status = 0)
'

writeLines(run_app, "run_app.R")
cat("Wrote run_app.R\n")
