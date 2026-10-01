# app.R - IVDr_Data Manager

# Suppress startup messages for speed
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(xml2)
  library(dplyr)
  library(data.table)
  library(DT)
  library(openxlsx)
  library(plotly)
  library(future.apply)
  library(rstudioapi)
  library(PepsNMR)
  library(jsonlite)

# ---- CONFIGURATION ----
# Settings live in config.json, written by the setup wizard and editable
# from the Settings tab. lab_name below is only the pre-wizard fallback.
lab_name <- "NMR Lab"

})

# Null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b


# APP DIRECTORY  always where app.R lives


# Save the working directory BEFORE Shiny changes it
APP_DIR <- normalizePath(getwd(), winslash = "/")

# Override: if we can find app.R in the call stack, use that
for (i in seq_len(sys.nframe())) {
  f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
  if (!is.null(f) && nchar(f) > 0 && grepl("app\\.R$", f, ignore.case = TRUE)) {
    APP_DIR <- normalizePath(dirname(f), winslash = "/")
    break
  }
}

message("APP_DIR resolved to: ", APP_DIR)



options(shiny.maxRequestSize = 200 * 1024^2)
#plan(multisession, workers = max(1, parallel::detectCores() - 1))



# SETUP WIZARD UI -------

setup_ui <- function() {
  fluidPage(
    useShinyjs(),
    theme = bs_theme(version = 5, bootswatch = "flatly"),
    tags$head(
      tags$style(HTML("
        .setup-container {
          max-width: 850px;
          margin: 30px auto;
          padding: 15px;
        }
        .setup-step { display: none; }
        .setup-step.active { display: block; }
        .field-grid {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 6px;
        }
        .field-chip {
          display: flex;
          align-items: center;
          gap: 8px;
          padding: 6px 10px;
          background: #f8f9fa;
          border-radius: 6px;
          border: 1px solid #ecf0f1;
          font-size: 0.9em;
        }
        .field-chip:hover { background: #ecf0f1; }
        .field-chip .form-check { margin: 0; padding: 0; min-height: auto; }
        .field-chip .form-check-input { margin-top: 0; }
        .field-chip .chip-label { font-weight: 600; flex-shrink: 0; }
        .field-chip .chip-desc { color: #7f8c8d; font-size: 0.82em; }
        .required-badge {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          padding: 5px 10px;
          background: #d5f5e3;
          border-radius: 6px;
          font-size: 0.85em;
          color: #1a7a4c;
          font-weight: 600;
        }
        .step-indicator {
          display: flex;
          justify-content: center;
          gap: 8px;
          margin-bottom: 20px;
        }
        .step-dot {
          width: 12px; height: 12px;
          border-radius: 50%;
          background: #bdc3c7;
        }
        .step-dot.active { background: #3498db; }
        .step-dot.done { background: #18bc9c; }
        .module-card {
          display: flex;
          align-items: center;
          gap: 12px;
          padding: 10px 14px;
          background: #f8f9fa;
          border-radius: 8px;
          border: 1px solid #ecf0f1;
          margin-bottom: 6px;
        }
        .module-card:hover { background: #ecf0f1; }
        .module-card .form-check { margin: 0; padding: 0; min-height: auto; }
        .module-card .mod-icon { font-size: 1.2em; width: 28px; text-align: center; color: #3498db; }
        .module-card .mod-info { flex: 1; }
        .module-card .mod-name { font-weight: 600; }
        .module-card .mod-desc { font-size: 0.82em; color: #7f8c8d; }
      "))
    ),
    
    tags$div(class = "setup-container",
             
             tags$div(
               style = "text-align: center; margin-bottom: 24px;",
               tags$img(src = "logo.png", height = "90px",
                        style = "margin-bottom: 12px;"),
               tags$h2("LarmoR", style = "margin: 0; font-weight: 600; color: #2c3e50;"),
               tags$p(class = "text-muted",
                      style = "margin-top: 6px; font-size: 0.95em;",
                      "NMR Sample Management & IVDr Data Extraction"),
               tags$p(class = "text-muted",
                      style = "margin-top: 10px; font-size: 0.85em;",
                      "Configure the dashboard for your lab.")
             ),
             
             tags$div(class = "step-indicator",
                      tags$div(id = "dot1", class = "step-dot active"),
                      tags$div(id = "dot2", class = "step-dot"),
                      tags$div(id = "dot3", class = "step-dot"),
                      tags$div(id = "dot4", class = "step-dot"),
                      tags$div(id = "dot5", class = "step-dot")
             ),
             
             # ---- Step 1: General ----
             tags$div(id = "setup_step_1", class = "setup-step active",
                      card(
                        card_header(tags$strong(icon("gear"), " Step 1: General Settings")),
                        card_body(
                          padding = "15px",
                          layout_column_wrap(
                            width = 1/2,
                            textInput("setup_lab_name", "Lab / Facility Name:",
                                      placeholder = "e.g. NMR Core Facility", width = "100%"),
                            textInput("setup_data_path", "NMR Data Directory:",
                                      value = "", width = "100%")
                          ),
                          tags$div(
                            style = "text-align: right; margin-top: 15px;",
                            actionButton("setup_next_1", "Next",
                                         class = "btn-primary", icon = icon("arrow-right"))
                          )
                        )
                      )
             ),
             
             # ---- Step 2: Archive columns ----
             tags$div(id = "setup_step_2", class = "setup-step",
                      card(
                        card_header(
                          tags$strong(icon("table-columns"), " Step 2: Archive Fields"),
                          tags$br(),
                          tags$small(class = "text-muted", "Select which fields to track per sample.")
                        ),
                        card_body(
                          padding = "15px",
                          
                          # Upload existing archive option
                          tags$div(
                            style = "background: #eaf2f8; border-radius: 8px; padding: 12px; margin-bottom: 15px;",
                            tags$div(
                              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                              icon("upload", style = "color: #3498db;"),
                              tags$strong("Import existing archive (optional)"),
                              tags$span(class = "text-muted", style = "font-size: 0.82em;",
                                        "Upload a CSV/Excel to auto-detect columns")
                            ),
                            layout_column_wrap(
                              width = 1/2,
                              fileInput("setup_upload_archive", NULL,
                                        accept = c(".csv", ".xlsx", ".xls", ".tsv"),
                                        width = "100%"),
                              uiOutput("setup_upload_status")
                            ),
                            uiOutput("setup_column_mapping")
                          ),
                          
                          tags$div(
                            style = "margin-bottom: 8px;",
                            tags$h6(style = "margin-bottom: 6px;", "Required fields:"),
                            tags$div(
                              style = "display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;",
                              tags$span(class = "required-badge", icon("check"), "Date"),
                              tags$span(class = "required-badge", icon("check"), "Name"),
                              tags$span(class = "required-badge", icon("check"), "Project")
                            )
                          ),
                          
                          tags$h6(style = "margin-bottom: 6px;", "Optional fields:"),
                          tags$div(class = "field-grid",
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_typ", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Type"),
                                       tags$div(class = "chip-desc", "Plasma, Serum, Urine..."))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_groesse", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Size"),
                                       tags$div(class = "chip-desc", "Sample volume"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_box", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Box"),
                                       tags$div(class = "chip-desc", "Box assignment"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_box_code", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Box Code"),
                                       tags$div(class = "chip-desc", "Barcode / QR"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_sex", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Sex"),
                                       tags$div(class = "chip-desc", "m / f / d"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_age", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Age"),
                                       tags$div(class = "chip-desc", "Subject age"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_diagnosis", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Diagnosis"),
                                       tags$div(class = "chip-desc", "Clinical indication"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_timepoint", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Timepoint"),
                                       tags$div(class = "chip-desc", "T0, T1, Follow-up"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_storage", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Storage"),
                                       tags$div(class = "chip-desc", "Location / temp"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_operator", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Operator"),
                                       tags$div(class = "chip-desc", "Who processed"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_col_notes", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Notes"),
                                       tags$div(class = "chip-desc", "Free text"))
                            )
                          ),
                          
                          tags$hr(style = "margin: 12px 0;"),
                          tags$h6(style = "margin-bottom: 6px;", "Custom fields:"),
                          tags$div(
                            style = "display: flex; gap: 8px; align-items: end;",
                            textInput("setup_custom_col_name", NULL, placeholder = "Field name", width = "35%"),
                            textInput("setup_custom_col_desc", NULL, placeholder = "Description", width = "45%"),
                            actionButton("setup_add_custom_col", "Add",
                                         class = "btn-sm btn-outline-primary", icon = icon("plus"),
                                         style = "margin-bottom: 15px;")
                          ),
                          uiOutput("setup_custom_cols_list"),
                          
                          tags$div(
                            style = "display: flex; justify-content: space-between; margin-top: 15px;",
                            actionButton("setup_back_2", "Back",
                                         class = "btn-outline-secondary", icon = icon("arrow-left")),
                            actionButton("setup_next_2", "Next",
                                         class = "btn-primary", icon = icon("arrow-right"))
                          )
                        )
                      )
             ),
             
             # ---- Step 3: Project fields ----
             tags$div(id = "setup_step_3", class = "setup-step",
                      card(
                        card_header(
                          tags$strong(icon("flask"), " Step 3: Project Fields"),
                          tags$br(),
                          tags$small(class = "text-muted", "Select which fields to track per project.")
                        ),
                        card_body(
                          padding = "15px",
                          
                          tags$div(
                            style = "margin-bottom: 8px;",
                            tags$h6(style = "margin-bottom: 6px;", "Required fields:"),
                            tags$div(
                              style = "display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 10px;",
                              tags$span(class = "required-badge", icon("check"), "Title"),
                              tags$span(class = "required-badge", icon("check"), "Abbreviation")
                            )
                          ),
                          
                          tags$h6(style = "margin-bottom: 6px;", "Optional fields:"),
                          tags$div(class = "field-grid",
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_pi", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "PI"),
                                       tags$div(class = "chip-desc", "Principal Investigator"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_email", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Email"),
                                       tags$div(class = "chip-desc", "Contact email"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_contact", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Contact"),
                                       tags$div(class = "chip-desc", "Contact person"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_clinic", NULL, value = TRUE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Clinic / Institute"),
                                       tags$div(class = "chip-desc", "Associated institution"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_group", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Working Group"),
                                       tags$div(class = "chip-desc", "Research group / AG"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_phone", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Phone"),
                                       tags$div(class = "chip-desc", "Phone number"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_description", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Description"),
                                       tags$div(class = "chip-desc", "Project objective"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_start", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Start Date"),
                                       tags$div(class = "chip-desc", "Project start"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_end", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "End Date"),
                                       tags$div(class = "chip-desc", "Planned end"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_status", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Status"),
                                       tags$div(class = "chip-desc", "Active / Completed"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_ethik", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Ethics Approval"),
                                       tags$div(class = "chip-desc", "Ethics committee #"))
                            ),
                            tags$div(class = "field-chip",
                              checkboxInput("setup_proj_notes", NULL, value = FALSE, width = "auto"),
                              tags$div(tags$span(class = "chip-label", "Notes"),
                                       tags$div(class = "chip-desc", "Free text"))
                            )
                          ),
                          
                          tags$hr(style = "margin: 12px 0;"),
                          tags$h6(style = "margin-bottom: 6px;", "Custom fields:"),
                          tags$div(
                            style = "display: flex; gap: 8px; align-items: end;",
                            textInput("setup_custom_proj_name", NULL, placeholder = "Field name", width = "35%"),
                            textInput("setup_custom_proj_desc", NULL, placeholder = "Description", width = "45%"),
                            actionButton("setup_add_custom_proj", "Add",
                                         class = "btn-sm btn-outline-primary", icon = icon("plus"),
                                         style = "margin-bottom: 15px;")
                          ),
                          uiOutput("setup_custom_projs_list"),
                          
                          tags$div(
                            style = "display: flex; justify-content: space-between; margin-top: 15px;",
                            actionButton("setup_back_3", "Back",
                                         class = "btn-outline-secondary", icon = icon("arrow-left")),
                            actionButton("setup_next_3", "Next",
                                         class = "btn-primary", icon = icon("arrow-right"))
                          )
                        )
                      )
             ),
             
             # ---- Step 4: Modules ----
             tags$div(id = "setup_step_4", class = "setup-step",
                      card(
                        card_header(
                          tags$strong(icon("puzzle-piece"), " Step 4: Enable Modules"),
                          tags$br(),
                          tags$small(class = "text-muted", "Core modules are always active. Choose any optional extras.")
                        ),
                        card_body(
                          padding = "15px",

                          tags$div(
                            style = "font-size: 0.75em; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
                            "Core \u2014 always enabled"
                          ),

                          tags$div(class = "module-card", style = "opacity: 0.85;",
                            tags$div(style = "width: 38px; text-align: center;",
                                     icon("check", style = "color: #18bc9c;")),
                            tags$div(class = "mod-icon", icon("flask")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Sample Submission"),
                              tags$div(class = "mod-desc", "Build ICON-NMR submission tables"))
                          ),
                          tags$div(class = "module-card", style = "opacity: 0.85;",
                            tags$div(style = "width: 38px; text-align: center;",
                                     icon("check", style = "color: #18bc9c;")),
                            tags$div(class = "mod-icon", icon("file-code")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Data Extraction (XML)"),
                              tags$div(class = "mod-desc", "Parse Bruker IVDr XML result files"))
                          ),
                          tags$div(class = "module-card", style = "opacity: 0.85;",
                            tags$div(style = "width: 38px; text-align: center;",
                                     icon("check", style = "color: #18bc9c;")),
                            tags$div(class = "mod-icon", icon("archive")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Sample Archive"),
                              tags$div(class = "mod-desc", "Central sample management, search, and filtering"))
                          ),
                          tags$div(class = "module-card", style = "opacity: 0.85;",
                            tags$div(style = "width: 38px; text-align: center;",
                                     icon("check", style = "color: #18bc9c;")),
                            tags$div(class = "mod-icon", icon("folder-tree")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Projects"),
                              tags$div(class = "mod-desc", "Project registry and metadata"))
                          ),
                          tags$div(class = "module-card", style = "opacity: 0.85;",
                            tags$div(style = "width: 38px; text-align: center;",
                                     icon("check", style = "color: #18bc9c;")),
                            tags$div(class = "mod-icon", icon("clock-rotate-left")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Measurement Archive"),
                              tags$div(class = "mod-desc", "Scan and archive NMR measurement folders"))
                          ),

                          tags$hr(style = "margin: 16px 0 10px 0;"),

                          tags$div(
                            style = "font-size: 0.75em; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
                            "Optional"
                          ),

                          tags$div(class = "module-card",
                            checkboxInput("setup_mod_viewer", NULL, value = TRUE, width = "auto"),
                            tags$div(class = "mod-icon", icon("wave-square")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Spectra Viewer"),
                              tags$div(class = "mod-desc", "Interactive 1D spectrum display with metabolite regions"))
                          ),
                          tags$div(class = "module-card",
                            checkboxInput("setup_mod_qk", NULL, value = FALSE, width = "auto"),
                            tags$div(class = "mod-icon", icon("chart-line")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "QC Monitor"),
                              tags$div(class = "mod-desc", "Levey-Jennings charts for quality control"))
                          ),
                          tags$div(class = "module-card",
                            checkboxInput("setup_mod_boxes", NULL, value = FALSE, width = "auto"),
                            tags$div(class = "mod-icon", icon("box")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "Biobank"),
                              tags$div(class = "mod-desc", "Sample-Box registration, tracking, and status management"))
                          ),
                          tags$div(class = "module-card",
                            checkboxInput("setup_mod_nmr", NULL, value = FALSE, width = "auto"),
                            tags$div(class = "mod-icon", icon("magnet")),
                            tags$div(class = "mod-info",
                              tags$div(class = "mod-name", "NMR Copy"),
                              tags$div(class = "mod-desc", "Copy spectra from backup + optional bucketing"))
                          ),

                          tags$div(
                            style = "display: flex; justify-content: space-between; margin-top: 15px;",
                            actionButton("setup_back_4", "Back",
                                         class = "btn-outline-secondary", icon = icon("arrow-left")),
                            actionButton("setup_next_4", "Next",
                                         class = "btn-primary", icon = icon("arrow-right"))
                          )
                        )
                      )
             ),
             
             # ---- Step 5: Summary ----
             tags$div(id = "setup_step_5", class = "setup-step",
                      card(
                        card_header(tags$strong(icon("clipboard-check"), " Step 5: Summary")),
                        card_body(
                          padding = "15px",
                          uiOutput("setup_summary"),
                          tags$hr(),
                          tags$div(
                            style = "display: flex; justify-content: space-between; margin-top: 15px;",
                            actionButton("setup_back_5", "Back",
                                         class = "btn-outline-secondary", icon = icon("arrow-left")),
                            actionButton("setup_finish", "Complete Setup & Start",
                                         class = "btn-success btn-lg", icon = icon("rocket"),
                                         style = "padding: 10px 25px;")
                          )
                        )
                      )
             )
    )
  )
}


# CONFIG FILE------


CONFIG_PATH <- file.path(APP_DIR, "config.json")

config_exists <- function() {
  file.exists(CONFIG_PATH)
}

read_config <- function() {
  if (!config_exists()) return(NULL)
  cfg <- jsonlite::fromJSON(CONFIG_PATH, simplifyVector = TRUE)
  # Normalise column_types: the wizard writes plain strings, the Settings tab
  # writes list(type=, choices=). Coerce everything to the list form.
  if (!is.null(cfg$archive$column_types)) {
    cfg$archive$column_types <- lapply(cfg$archive$column_types, function(ct) {
      if (is.list(ct)) ct else list(type = as.character(ct), choices = NULL)
    })
  }
  cfg
}

save_config <- function(config) {
  jsonlite::write_json(config, CONFIG_PATH, pretty = TRUE, auto_unbox = TRUE)
}




# UI ----------------------------------------------------------------------
main_app_ui <- function() {
  config <- read_config()
  
  # Module configuration.
  # Core modules (submission, extraction, archive, projects, measurements)
  # are always enabled and not user-toggleable. Only optional modules are
  # read from config, so pre-1.0 config files migrate automatically.
  mod <- if (is.null(config) || is.null(config$modules)) list() else config$modules

  # Core - always on
  mod$submission <- TRUE
  mod$archive    <- TRUE
  mod$xml        <- TRUE
  mod$projects   <- TRUE
  mod$meas       <- TRUE

  # Optional - viewer defaults on, the rest off if not present
  if (is.null(mod$viewer)) mod$viewer <- TRUE
  for (m in c("qk", "boxes", "nmr")) {
    if (is.null(mod[[m]])) mod[[m]] <- FALSE
  }
  
  lab_name <- if (!is.null(config)) config$lab_name else "NMR Tools"
  
  # ---- Build panel list dynamically ----
  panels <- list()
  
  # TAB 0: DASHBOARD (always shown)-----
  panels[["start"]] <- nav_panel(
    title = "Start",
    icon = icon("house"),
    tags$div(
      style = "padding: 15px;",
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px;",
        tags$h4(style = "color: #2c3e50; font-weight: 700; margin: 0;", "Dashboard"),
        tags$div(
          actionButton("demo_mode_btn", "Demo Mode",
                       class = "btn-sm btn-outline-secondary",
                       icon = icon("eye-slash")),
          actionButton("reset_setup", "Reset Setup",
                       class = "btn-outline-danger btn-sm", icon = icon("rotate-left"),
                       style = "margin-left: 5px;")
        )
      ),

      # ---- KPIs ----
      layout_column_wrap(
        width = 1/4,
        style = "margin-bottom: 15px;",
        value_box(title = "Total Samples", value = textOutput("kpi_total_samples"),
                  showcase = icon("vial"), theme = "primary"),
        value_box(title = "Projects", value = textOutput("kpi_total_projects"),
                  showcase = icon("flask"), theme = "success"),
        value_box(title = "This Year", value = textOutput("kpi_this_year"),
                  showcase = icon("calendar"), theme = "info"),
        value_box(title = "This Month", value = textOutput("kpi_this_month"),
                  showcase = icon("calendar-day"), theme = "warning")
      ),

      # ---- Charts Row 1 ----
      layout_column_wrap(
        width = 1/2,
        style = "margin-bottom: 15px;",
        card(
          card_header(class = "bg-light", tags$strong("Samples per Year")),
          card_body(padding = "10px", plotlyOutput("chart_per_year", height = "280px"))
        ),
        card(
          card_header(
            class = "bg-light",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between;",
              tags$strong("Top 10 Projects"),
              selectInput("dashboard_year_projects", label = NULL,
                          choices = NULL, selected = NULL, width = "120px")
            )
          ),
          card_body(padding = "10px", plotlyOutput("chart_per_project", height = "280px"))
        )
      ),

      # ---- Charts Row 2 ----
      layout_column_wrap(
        width = 1/2,
        style = "margin-bottom: 15px;",
        card(
          card_header(class = "bg-light", tags$strong("Samples per Type")),
          card_body(padding = "10px", plotlyOutput("chart_per_type", height = "280px"))
        ),
        card(
          card_header(
            class = "bg-light",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between;",
              tags$strong("Monthly"),
              selectInput("dashboard_year_monthly", label = NULL,
                          choices = NULL, selected = NULL, width = "120px")
            )
          ),
          card_body(padding = "10px", plotlyOutput("chart_per_month", height = "280px"))
        )
      ),

      # ---- Submissions Table ----
      card(
        style = "margin-bottom: 15px;",
        card_header(class = "bg-light", tags$strong("Submissions This Year")),
        card_body(
          style = "max-height: 400px; overflow-y: auto; padding: 10px;",
          DTOutput("recent_submissions")
        )
      ),

      # ---- Backups ----
      card(
        card_header(
          class = "bg-light",
          tags$div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            tags$strong("Backups"),
            actionButton("manual_backup_btn", "Manual Backup",
                         class = "btn-sm btn-outline-secondary", icon = icon("floppy-disk"))
          )
        ),
        card_body(padding = "10px", verbatimTextOutput("backup_info"))
      ),
      tags$div(
        style = "margin-top: 15px; font-size: 0.8em; color: #95a5a6; text-align: center;",
        tags$p("Version 1.0"),
        tags$p(paste0("Date: ", format(Sys.Date(), "%Y-%m-%d")))
      )
    )
  )
  
  # TAB 1: SAMPLE LIST (always shown)------
  panels[["submission"]] <- nav_panel(
    title = "Sample Submission",
    icon = icon("list-check"),
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        card(
          card_header(class = "bg-primary text-white", tags$strong("1. Enter Samples")),
          card_body(
            padding = "10px",
            radioButtons("input_method", label = NULL,
                         choices = c("Manual entry" = "manual",
                                     "Upload file" = "upload"),
                         selected = "manual"),
            conditionalPanel(
              condition = "input.input_method == 'manual'",
              textAreaInput("manual_names", "Sample names (one per line):",
                            rows = 8, placeholder = "Sample_001\nSample_002\nSample_003")
            ),
            conditionalPanel(
              condition = "input.input_method == 'upload'",
              fileInput("sample_file", "Upload file:",
                        accept = c(".csv", ".txt", ".xlsx")),
              tags$small(class = "text-muted",
                         "CSV/TXT with column 'Name', or Excel file.")
            )
          )
        ),
        card(
          card_header(class = "bg-primary text-white", tags$strong("1b. Project & Box")),
          card_body(
            padding = "10px",
            style = "overflow: visible;",
            selectizeInput("project_select", "Project:",
                           choices = NULL,
                           options = list(
                             placeholder = "Select project...",
                             dropdownParent = "body"
                           )),
            selectizeInput("box_select", "Box:",
                           choices = NULL,
                           options = list(
                             placeholder = "Select project first...",
                             dropdownParent = "body"
                           )),
            tags$small(class = "text-muted",
                       "Only open boxes are shown. Status is updated automatically.")
          )
        ),
        card(
          card_header(class = "bg-primary text-white", tags$strong("1c. Additional Fields")),
          card_body(
            padding = "10px",
            uiOutput("sample_input_fields")
          )
        ),
        card(
          card_header(class = "bg-primary text-white", tags$strong("2. Parameters")),
          card_body(
            padding = "10px",
            selectInput("sub_solvent", "Sample type:",
                        choices = NULL),
            radioButtons("tube_size", "Tube size:",
                         choices = c("5mm" = "5mm", "3mm" = "3mm"),
                         selected = "5mm", inline = TRUE),
            numericInput("rack_number", "Rack number:", value = 1, min = 1, max = 5),
            numericInput("start_slot", "Start position:", value = 1, min = 1, max = 96)
          )
        ),
        card(
          card_header(class = "bg-primary text-white", tags$strong("3. Experiments")),
          card_body(
            padding = "10px",
            uiOutput("experiment_selection_ui"),
            tags$small(class = "text-muted", "Default experiments auto-suggested.")
          )
        ),
        card(
          card_header(class = "bg-primary text-white", tags$strong("4. File Path")),
          card_body(
            padding = "10px",
            textInput("data_path", "Data base path:", value = "D:/IVDrData/data")
          )
        ),
        tags$div(
          style = "padding: 10px 0;",
          actionButton("generate_btn", "Generate Sample Submission",
                       class = "btn-success btn-lg", icon = icon("table"),
                       style = "width: 100%; margin-bottom: 10px; font-weight: bold;"),
          tags$div(id = "download_excel_div", style = "display:none;",
                   downloadButton("download_excel_btn", "Download Excel",
                                  class = "btn-info", style = "width: 100%; font-weight: bold;"))
        )
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header(class = "bg-light",
                      tags$div(style = "display: flex; align-items: center;",
                               icon("terminal", style = "margin-right: 8px;"),
                               tags$strong("Status"))),
          card_body(padding = "15px", verbatimTextOutput("submit_status_text"))
        ),
        card(
          card_header(class = "bg-light",
                      tags$div(style = "display: flex; align-items: center;",
                               icon("table", style = "margin-right: 8px;"),
                               tags$strong("Sample Submission Preview"))),
          card_body(padding = "10px", DTOutput("preview_submission"))
        )
      )
    )
  )
  
  # TAB 2: DATA EXTRACTION------
    panels[["xml"]] <- nav_panel(
      title = "Data Extraction",
      icon = icon("file-code"),
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          card(
            card_header(class = "bg-primary text-white", tags$strong("1. Sample Type")),
            card_body(
              padding = "10px",
              radioButtons("sample_type", label = NULL,
                           choices = c("Blood (Plasma)" = "blood", "Urine" = "urine"),
                           selected = "blood"),
              conditionalPanel(
                condition = "input.sample_type == 'urine'",
                radioButtons("urine_panel_type", "IVDR Urine Panel:",
                             choices = c("Extended (E)" = "extended",
                                         "Newborn Extended (NE)" = "newborn"),
                             selected = "extended")
              )
            )
          ),
          card(
            card_body(
              padding = "10px",
              selectizeInput("xml_project_select", "Project:",
                             choices = NULL, width = "100%",
                             options = list(
                               placeholder = "Search project...",
                               maxOptions = 50,
                               dropdownParent = "body"
                             )),
              textInput("xml_batch_note", "Note:", placeholder = "optional")
            )
          ),
          card(
            card_header(class = "bg-primary text-white", tags$strong("2. Data Folder")),
            card_body(
              padding = "10px",
              textInput("manual_folder_path", "Folder path:",
                        placeholder = "e.g. D:\\Data\\",
                        width = "100%"),
              tags$div(
                style = "display: flex; gap: 5px; margin-bottom: 8px;",
                actionButton("select_folder_btn", "Browse...",
                             icon = icon("folder-open"),
                             class = "btn-outline-primary btn-sm"),
                actionButton("use_path_btn", "Use Path",
                             icon = icon("check"),
                             class = "btn-primary btn-sm")
              ),
              tags$div(
                style = "background: #f8f9fa; border-radius: 6px; padding: 8px; font-size: 0.8em; word-wrap: break-word;",
                textOutput("folder_path_display", inline = TRUE)
              ),
              tags$hr(style = "margin: 8px 0;"),
              actionButton("scan_btn", "Scan for XML Files",
                           class = "btn-info btn-sm", icon = icon("magnifying-glass"),
                           style = "width: 100%; font-weight: 600; margin-bottom: 8px;"),
              uiOutput("scan_results_ui")
            ),
          ),
          card(
            card_header(class = "bg-primary text-white", tags$strong("3. Panels")),
            card_body(
              padding = "10px",
              uiOutput("panel_selection_ui"),
              tags$hr(style = "margin: 8px 0;"),
              radioButtons("lipid_naming", "Lipid parameter names:",
                           choices = c("LARMOR descriptive" = "larmor",
                                       "Bruker raw codes" = "bruker"),
                           selected = if (exists("LIPID_NAMING")) LIPID_NAMING else "larmor"),
              tags$small(class = "text-muted",
                         "Descriptive: TG_mg_dl · Raw: TPTG")
            )
          ),
          card(
            card_header(class = "bg-primary text-white", tags$strong("4. Process & Export")),
            card_body(
              padding = "10px",
              radioButtons("output_type", "Export format:",
                           choices = c("Excel (all sheets)" = "combined",
                                       "Individual CSVs" = "individual"),
                           selected = "combined"),
              textInput("filename", "Filename:", value = "results"),
              tags$hr(style = "margin: 8px 0;"),
              actionButton("process_btn", "Process",
                           class = "btn-success btn-lg", icon = icon("play"),
                           style = "width: 100%; margin-bottom: 10px; font-weight: bold;"),
              tags$div(id = "download_div", style = "display: none;",
                       tags$hr(style = "margin: 8px 0;"),
                       conditionalPanel(
                         condition = "input.output_type == 'combined'",
                         downloadButton("download_xlsx_btn", "Download Excel",
                                        class = "btn-success btn-sm", style = "width: 100%;")
                       ),
                       conditionalPanel(
                         condition = "input.output_type == 'individual'",
                         tags$div(
                           style = "display: flex; flex-direction: column; gap: 5px;",
                           downloadButton("dl_summary", "Summary",
                                          class = "btn-outline-info btn-sm", style = "width: 100%;"),
                           downloadButton("dl_qc", "QC Report",
                                          class = "btn-outline-info btn-sm", style = "width: 100%;"),
                           tags$hr(style = "margin: 4px 0;"),
                           downloadButton("dl_metabolites_conc", "Metabolites (Conc)",
                                          class = "btn-outline-success btn-sm", style = "width: 100%;"),
                           downloadButton("dl_metabolites_raw", "Metabolites (RawConc)",
                                          class = "btn-outline-success btn-sm", style = "width: 100%;"),
                           downloadButton("dl_metabolites_err", "Metabolites (ErrConc)",
                                          class = "btn-outline-success btn-sm", style = "width: 100%;"),
                           downloadButton("dl_metabolites_sig", "Metabolites (SigCorr)",
                                          class = "btn-outline-success btn-sm", style = "width: 100%;"),
                           downloadButton("dl_metabolites_lod", "Metabolites (LOD)",
                                          class = "btn-outline-success btn-sm", style = "width: 100%;"),
                           tags$hr(style = "margin: 4px 0;"),
                           downloadButton("dl_lipids", "Lipids",
                                          class = "btn-outline-warning btn-sm", style = "width: 100%;")
                         )
                       )
              )
            )
          )
        ),
        layout_columns(
          col_widths = 12,
          # Extraction Status Panel
          card(
            card_header(class = "bg-light",
                        tags$div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          tags$div(
                            icon("clipboard-list", style = "margin-right: 8px;"),
                            tags$strong("Extraction Status")
                          ),
                          uiOutput("xml_status_badge")
                        )
            ),
            card_body(
              padding = "12px",
              uiOutput("xml_status_panel")
            )
          )
        ),
        layout_columns(
          col_widths = 12,
          card(
            card_header(class = "bg-light",
                        tags$div(style = "display: flex; align-items: center;",
                                 icon("table", style = "margin-right: 8px;"),
                                 tags$strong("Data Preview"))),
            card_body(
              padding = "10px",
              navset_card_tab(
                nav_panel("Metabolites", DTOutput("preview_metas")),
                nav_panel("Lipids", DTOutput("preview_lipids")),
                nav_panel("QC",
                          navset_card_tab(
                            nav_panel("Overview",
                                      layout_column_wrap(
                                        width = 1/4,
                                        style = "margin-bottom: 15px;",
                                        value_box(title = "Samples Checked", value = textOutput("qc_kpi_total"),
                                                  showcase = icon("microscope"), theme = "primary"),
                                        value_box(title = "Passed (\u226590%)", value = textOutput("qc_kpi_pass"),
                                                  showcase = icon("circle-check"), theme = "success"),
                                        value_box(title = "Failed", value = textOutput("qc_kpi_fail"),
                                                  showcase = icon("circle-xmark"), theme = "danger"),
                                        value_box(title = "Overall Pass Rate", value = textOutput("qc_kpi_overall_rate"),
                                                  showcase = icon("percent"), theme = "info")
                                      ),
                                      card(
                                        card_header(
                                          class = "bg-light",
                                          tags$div(
                                            style = "display: flex; align-items: center; justify-content: space-between;",
                                            tags$strong("QC Overview per Sample"),
                                            tags$small(class = "text-muted",
                                                       "Threshold: \u2265 90% checks passed = PASS")
                                          )
                                        ),
                                        card_body(padding = "10px", DTOutput("qc_overall_table"))
                                      )
                            ),
                            nav_panel("By Category",
                                      layout_column_wrap(
                                        width = 1/3, style = "margin-bottom: 15px;",
                                        value_box(title = "Matrix Integrity", value = textOutput("qc_kpi_matrix"),
                                                  showcase = icon("droplet"), theme = "info"),
                                        value_box(title = "Sample Preparation", value = textOutput("qc_kpi_prep"),
                                                  showcase = icon("flask-vial"), theme = "warning"),
                                        value_box(title = "NMR Spectral Quality", value = textOutput("qc_kpi_spectral"),
                                                  showcase = icon("wave-square"), theme = "secondary")
                                      ),
                                      card(
                                        card_header(class = "bg-light", tags$strong("QC by Category")),
                                        card_body(padding = "10px", DTOutput("qc_summary_table"))
                                      )
                            ),
                            nav_panel("Details",
                                      card(
                                        card_header(
                                          class = "bg-light",
                                          tags$div(
                                            style = "display: flex; align-items: center; justify-content: space-between;",
                                            tags$strong("All QC Parameters"),
                                            selectInput("qc_detail_filter", label = NULL,
                                                        choices = c("All" = "all", "Only FAIL" = "fail"),
                                                        selected = "all", width = "150px")
                                          )
                                        ),
                                        card_body(padding = "10px", DTOutput("qc_detail_table"))
                                      )
                            ),
                            nav_panel("Visualization",
                                      layout_column_wrap(
                                        width = 1/2,
                                        card(
                                          card_header(class = "bg-light", tags$strong("Pass/Fail per Sample")),
                                          card_body(padding = "10px",
                                                    plotlyOutput("qc_chart_samples", height = "350px"))
                                        ),
                                        card(
                                          card_header(class = "bg-light", tags$strong("Errors per Parameter")),
                                          card_body(padding = "10px",
                                                    plotlyOutput("qc_chart_params", height = "350px"))
                                        )
                                      )
                            )
                          )
                )
              )
            )
          )
        )
      )
    )
  
 
  
  # TAB 4: ARCHIVE------
    panels[["archiv"]] <- nav_panel(
      title = "Archive",
      icon = icon("clock-rotate-left"),
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          title = tags$div(icon("filter", style = "margin-right: 5px;"), "Archive Filters"),
          
          # Project filter
          selectizeInput("archive_project_filter", "Project:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All projects..."),
                         width = "100%"),
          
          # Date range filter
          dateRangeInput("archive_date_range", "Date Range:",
                         start = NULL, end = NULL,
                         separator = " to ", width = "100%"),
          
          # Sample type filter
          selectizeInput("archive_type_filter", "Sample Type:",
                         choices = c("All" = "all"),
                         selected = "all", width = "100%"),
          
          # Box filter
          selectizeInput("archive_box_filter", "Box:",
                         choices = NULL, multiple = TRUE,
                         options = list(placeholder = "All boxes..."),
                         width = "100%"),
          
          tags$hr(),
          
          # Dynamic custom column filters
          tags$h6(icon("sliders", style = "margin-right: 5px;"), "Custom Column Filters"),
          uiOutput("archive_custom_filters_ui"),
          
          tags$hr(),
          
          # Free text search
          textInput("archive_search", "Free Text Search:",
                    placeholder = "Search across all columns...",
                    width = "100%"),
          
          tags$div(
            style = "display: flex; gap: 10px; margin-top: 10px;",
            actionButton("archive_apply_filters", "Apply",
                         class = "btn-primary btn-sm", icon = icon("filter")),
            actionButton("archive_clear_filters", "Clear",
                         class = "btn-secondary btn-sm", icon = icon("xmark"))
          ),
          
          tags$hr(),
          uiOutput("archive_filter_summary"),
          
          tags$hr(),
          # Detail project selector
          selectizeInput("archive_plot_project", "Project Detail View:",
                         choices = NULL,
                         options = list(placeholder = "Select project...")),
          
          downloadButton("download_archive_btn", "Download Archive",
                         class = "btn-outline-primary", style = "width: 100%;")
        ),
        
        # Main content
        tags$div(
          style = "padding: 10px;",
          
          # KPIs
          layout_column_wrap(
            width = 1/4,
            value_box(title = "Total Samples", value = textOutput("archive_kpi_total"),
                      showcase = icon("vial"), theme = "primary"),
            value_box(title = "Projects", value = textOutput("archive_kpi_projects"),
                      showcase = icon("flask"), theme = "success"),
            value_box(title = "Filtered", value = textOutput("archive_kpi_filtered"),
                      showcase = icon("filter"), theme = "warning"),
            value_box(title = "Boxes", value = textOutput("archive_kpi_boxes"),
                      showcase = icon("box"), theme = "info")
          ),
          
          # Tabs
          navset_card_tab(
            title = tags$div(icon("archive"), " Archive"),
            
            # Table View
            nav_panel(
              title = "Table",
              icon = icon("table"),
              tags$div(
                style = "display: flex; justify-content: flex-end; gap: 10px; margin-bottom: 10px;",
                actionButton("archive_send_to_nmr", "Send to NMR Copy",
                             class = "btn-sm btn-info", icon = icon("share-from-square")),
                actionButton("archive_edit_selected", "Edit",
                             class = "btn-sm btn-warning", icon = icon("pen")),
                actionButton("archive_delete_selected", "Delete",
                             class = "btn-sm btn-danger", icon = icon("trash"))
              ),
              DTOutput("archive_table")
            ),
            
            # Charts
            nav_panel(
              title = "Charts",
              icon = icon("chart-pie"),
              layout_column_wrap(
                width = 1/2,
                style = "margin-bottom: 15px;",
                card(
                  card_header(class = "bg-light", tags$strong("Timeline")),
                  card_body(padding = "10px",
                            plotlyOutput("archive_timeline", height = "300px"))
                ),
                card(
                  card_header(class = "bg-light", tags$strong("Samples per Project")),
                  card_body(padding = "10px",
                            plotlyOutput("archive_project_bars", height = "300px"))
                )
              ),
              layout_column_wrap(
                width = 1/2,
                style = "margin-bottom: 15px;",
                card(
                  card_header(class = "bg-light",
                              tags$strong(textOutput("archive_detail_title", inline = TRUE))),
                  card_body(padding = "10px",
                            plotlyOutput("archive_project_timeline", height = "300px"))
                ),
                card(
                  card_header(class = "bg-light", tags$strong("Project Boxes")),
                  card_body(padding = "10px",
                            plotlyOutput("archive_project_boxes", height = "300px"))
                )
              ),
              card(
                style = "margin-top: 15px;",
                card_header(class = "bg-light", tags$strong("Project Timelines (Gantt)")),
                card_body(padding = "10px",
                          plotlyOutput("archive_gantt", height = "400px"))
              )
            ),
            
            # Summary / Distribution
            nav_panel(
              title = "Summary",
              icon = icon("chart-bar"),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header(class = "bg-light", tags$strong("Custom Column Distribution")),
                  card_body(
                    padding = "10px",
                    selectInput("archive_summary_col", "Select Column:",
                                choices = NULL, width = "100%"),
                    plotlyOutput("archive_plot_custom", height = "300px")
                  )
                ),
                card(
                  card_header(class = "bg-light", tags$strong("Cross-tabulation")),
                  card_body(
                    padding = "10px",
                    layout_column_wrap(
                      width = 1/2,
                      selectInput("archive_crosstab_row", "Rows:", choices = NULL, width = "100%"),
                      selectInput("archive_crosstab_col", "Columns:", choices = NULL, width = "100%")
                    ),
                    DTOutput("archive_crosstab_table")
                  )
                )
              )
            )
          )
        )
      )
    )
  
  
  # TAB 5: PROJECTS (always shown)------
  panels[["projekte"]] <- nav_panel(
    title = "Projects",
    icon = icon("flask"),
    navset_card_tab(
      nav_panel(
        title = "Project List",
        icon = icon("table"),
        layout_sidebar(
          sidebar = sidebar(
            width = 280,
            card(
              card_header(class = "bg-primary text-white", tags$strong("Filter")),
              card_body(
                padding = "10px",
                selectizeInput("proj_filter_type", "Sample Type:",
                               choices = c("All" = "all"),
                               selected = "all"),
                textInput("proj_filter_search", "Search:",
                          placeholder = "Title or abbreviation...")
              )
            ),
            tags$div(
              style = "padding: 10px 0;",
              actionButton("save_projects_btn", "Save Project List",
                           class = "btn-primary", icon = icon("floppy-disk"),
                           style = "width: 100%; margin-bottom: 10px;"),
              downloadButton("download_projects_btn", "Download Excel",
                             class = "btn-outline-primary", style = "width: 100%;")
            )
          ),
          tags$div(
            style = "padding: 10px;",
            layout_column_wrap(
              width = 1/3,
              value_box(title = "Total Projects", value = textOutput("proj_kpi_total"),
                        showcase = icon("flask"), theme = "primary"),
              value_box(title = "Active Projects", value = textOutput("proj_kpi_active"),
                        showcase = icon("circle-play"), theme = "success"),
              value_box(title = "Sample Types", value = textOutput("proj_kpi_types"),
                        showcase = icon("vials"), theme = "info")
            ),
            card(
              style = "margin-top: 15px;",
              card_header(
                class = "bg-light",
                tags$div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  tags$strong("Project List"),
                  tags$div(
                    style = "display: flex; gap: 8px;",
                    actionButton("edit_project_btn", "Edit",
                                 class = "btn-sm btn-outline-warning", icon = icon("pen")),
                    actionButton("delete_project_btn", "Delete Selected",
                                 class = "btn-sm btn-outline-danger", icon = icon("trash"))
                  )
                )
              ),
              card_body(
                padding = "10px",
                style = "min-height: 600px;",
                DTOutput("projects_table")
              ),
              card(
                card_header(class = "bg-light",
                            tags$div(
                              style = "display: flex; align-items: center; justify-content: space-between;",
                              tags$strong("Sample-Boxes for Selected Project "),
                              textOutput("project_box_progress_text", inline = TRUE)
                            )
                ),
                card_body(
                  padding = "10px",
                  tags$div(
                    style = "margin-bottom: 10px;",
                    uiOutput("project_progress_bar")
                  ),
                  DTOutput("project_boxes_table")
                )
              )
            )
          )
        )
      ),
      nav_panel(
        title = "New Project",
        icon = icon("plus"),
        tags$div(
          style = "max-width: 800px; margin: 0 auto; padding: 30px;",
          tags$h4(style = "color: #2c3e50; margin-bottom: 20px;",
                  icon("flask", style = "margin-right: 10px;"),
                  "Create New Project"),
          tags$p(class = "text-muted", style = "margin-bottom: 25px;",
                 "Fill out the form to add a new project to the list."),
          card(
            card_body(
              padding = "25px",
              layout_column_wrap(
                width = 1/2,
                textInput("proj_title", "Title *:",
                          placeholder = "e.g. Cancer ", width = "100%"),
                textInput("proj_abbrev", "Abbreviation *:",
                          placeholder = "e.g. CAN", width = "100%")
              ),
              tags$hr(style = "margin: 20px 0;"),
              tags$h6(style = "color: #7f8c8d; margin-bottom: 15px;",
                      icon("user", style = "margin-right: 5px;"), "Contact Details"),
              layout_column_wrap(
                width = 1/2,
                textInput("proj_name", "Contact Person:",
                          placeholder = "First and last name", width = "100%"),
                textInput("proj_email", "Email:",
                          placeholder = "name@hospital.com", width = "100%")
              ),
              layout_column_wrap(
                width = 1/2,
                textInput("proj_group", "Group / AG:",
                          placeholder = "e.g. Clinic 1", width = "100%"),
                textInput("proj_contact", "Clinic/Institute:",
                          placeholder = "e.g. Urology", width = "100%")
              ),
              tags$hr(style = "margin: 20px 0;"),
              tags$h6(style = "color: #7f8c8d; margin-bottom: 15px;",
                      icon("vial", style = "margin-right: 5px;"), "Sample Information"),
              layout_column_wrap(
                width = 1/3,
                selectInput("proj_sample_type", "Sample Type:",
                            choices = c("Plasma", "Serum", "Urine","Other"),
                            selected = "Plasma", width = "100%"),
                textInput("proj_n_samples", "Number of Samples:",
                          placeholder = "e.g. 300", width = "100%"),
                textInput("proj_boxes", "Boxes:",
                          placeholder = "e.g. 4", width = "100%")
              ),
              tags$hr(style = "margin: 20px 0;"),
              tags$div(
                style = "display: flex; justify-content: flex-end; gap: 10px;",
                actionButton("clear_project_form_btn", "Clear Form",
                             class = "btn-outline-secondary", icon = icon("eraser")),
                actionButton("add_project_btn", "Add Project",
                             class = "btn-success btn-lg", icon = icon("plus"),
                             style = "font-weight: bold;")
              )
            )
          )
        )
      )
    )
  )
  
  
  # TAB 3: BOX REGISTER------
  if (isTRUE(mod$boxes)) {
    panels[["boxes"]] <- nav_panel(
      title = "Biobank",
      icon = icon("boxes-stacked"),
      navset_card_tab(
        nav_panel(
          title = "Overview",
          icon = icon("table"),
          layout_sidebar(
            sidebar = sidebar(
              width = 280,
              card(
                card_header(class = "bg-primary text-white", tags$strong("Filter")),
                card_body(
                  padding = "10px",
                  selectizeInput("box_filter_project", "Project:",
                                 choices = NULL, multiple = TRUE,
                                 options = list(placeholder = "All projects...")),
                  selectInput("box_filter_status", "Status:",
                              choices = c("All" = "all",
                                          "Received" = "Received",
                                          "Measured" = "Measured"),
                              selected = "all"),
                  dateRangeInput("box_filter_dates", "Date range:",
                                 start = NULL, end = NULL,
                                 separator = " to ")
                )
              ),
              tags$div(
                style = "padding: 10px 0;",
                actionButton("save_box_registry_btn", "Save Box-Registry",
                             class = "btn-primary", icon = icon("floppy-disk"),
                             style = "width: 100%; margin-bottom: 10px;"),
                downloadButton("download_box_registry_btn", "Download CSV",
                               class = "btn-outline-primary", style = "width: 100%;")
              )
            ),
            tags$div(
              style = "padding: 10px;",
              layout_column_wrap(
                width = 1/4,
                value_box(title = "Total Sample-Boxes", value = textOutput("box_kpi_total"),
                          showcase = icon("box"), theme = "primary"),
                value_box(title = "Received", value = textOutput("box_kpi_received"),
                          showcase = icon("inbox"), theme = "info"),
                value_box(title = "In Preparation", value = textOutput("box_kpi_prep"),
                          showcase = icon("flask-vial"), theme = "warning"),
                value_box(title = "Measured", value = textOutput("box_kpi_measured"),
                          showcase = icon("check-circle"), theme = "success")
              ),
              card(
                style = "margin-top: 15px;",
                card_header(class = "bg-light", tags$strong("Sample-Boxes per Project")),
                card_body(padding = "10px",
                          plotlyOutput("box_chart_per_project", height = "250px"))
              ),
              card(
                style = "margin-top: 15px;",
                card_header(
                  class = "bg-light",
                  tags$div(
                    style = "display: flex; align-items: center; justify-content: space-between;",
                    tags$strong("Biobank"),
                    tags$div(
                      actionButton("box_edit_btn", "Edit",
                                   class = "btn-sm btn-outline-warning", icon = icon("pen")),
                      actionButton("box_delete_btn", "Delete",
                                   class = "btn-sm btn-outline-danger", icon = icon("trash"),
                                   style = "margin-left: 5px;")
                    )
                  )
                ),
                card_body(
                  padding = "10px",
                  style = "min-height: 400px;",
                  DTOutput("box_registry_table")
                )
              )
            )
          )
        ),
        nav_panel(
          title = "New Sample-Box",
          icon = icon("plus"),
          tags$div(
            style = "max-width: 800px; margin: 0 auto; padding: 30px;",
            tags$h4(style = "color: #2c3e50; margin-bottom: 20px;",
                    icon("box", style = "margin-right: 10px;"),
                    "Register New Box"),
            tags$p(class = "text-muted", style = "margin-bottom: 25px;",
                   "Register an incoming sample box and assign a code."),
            card(
              card_body(
                padding = "25px",
                tags$div(
                  style = "background: #ecf0f1; border-radius: 8px; padding: 15px; margin-bottom: 20px; text-align: center;",
                  tags$small(class = "text-muted", "Auto-generated box code:"),
                  tags$h3(style = "color: #2c3e50; margin: 5px 0; font-family: monospace;",
                          textOutput("next_box_code", inline = TRUE))
                ),
                layout_column_wrap(
                  width = 1/2,
                  selectizeInput("box_project", "Project *:",
                                 choices = NULL,
                                 options = list(placeholder = "Select project..."),
                                 width = "100%"),
                  textInput("box_name_input", "Box Name *:",
                            placeholder = "e.g. AML Batch 3", width = "100%")
                ),
                tags$hr(style = "margin: 20px 0;"),
                tags$h6(style = "color: #7f8c8d; margin-bottom: 15px;",
                        icon("info-circle", style = "margin-right: 5px;"), "Details"),
                layout_column_wrap(
                  width = 1/3,
                  numericInput("box_n_samples", "Number of Samples:",
                               value = NULL, min = 1, width = "100%"),
                  dateInput("box_date_received", "Date Received:",
                            value = Sys.Date(), width = "100%"),
                  selectInput("box_status", "Status:",
                              choices = c("Received" = "Received",
                                          "Measured" = "Measured"),
                              selected = "Received", width = "100%")
                ),
                textAreaInput("box_notes", "Notes:",
                              placeholder = "Optional notes...",
                              rows = 3, width = "100%"),
                tags$hr(style = "margin: 20px 0;"),
                tags$div(
                  style = "display: flex; justify-content: flex-end; gap: 10px;",
                  actionButton("clear_box_form_btn", "Clear Form",
                               class = "btn-outline-secondary", icon = icon("eraser")),
                  actionButton("register_box_btn", "Register Box",
                               class = "btn-success btn-lg", icon = icon("plus"),
                               style = "font-weight: bold;")
                )
              )
            )
          )
        )
      )
    )
  }
  
  # TAB 6: QC MONITOR------
  if (isTRUE(mod$qk)) {
    panels[["qk"]] <- nav_panel(
      "QC Monitor",
      icon = icon("chart-line"),
      layout_sidebar(
        sidebar = sidebar(
          width = 280,
          title = "QC Settings",
          card(
            card_header(class = "bg-light", tags$strong("QC Data")),
            card_body(
              padding = "10px",
              tags$small(class = "text-muted", "QC file is updated automatically"),
              tags$hr(style = "margin: 8px 0;"),
              actionButton("qk_refresh_btn", "Refresh",
                           icon = icon("rotate"), class = "btn-outline-primary btn-sm",
                           style = "width: 100%;"),
              tags$hr(style = "margin: 8px 0;"),
              selectInput("qk_parameter", "Parameter:",
                          choices = NULL, width = "100%"),
              checkboxInput("qk_show_limits", "Show limits", value = TRUE),
              checkboxInput("qk_show_outliers", "Highlight outliers", value = TRUE),
              tags$hr(style = "margin: 8px 0;"),
              tags$strong("QC Matrix:"),
              selectInput("qk_matrix", NULL,
                          choices = c("Plasma" = "plasma", "Urine" = "urine"),
                          selected = "plasma", width = "100%"),
              tags$hr(style = "margin: 8px 0;"),
              tags$strong("QC Sample Pattern:"),
              tags$div(
                style = "margin-top: 5px;",
                textInput("qk_sample_pattern", NULL,
                          value = "^[Qq][KkCc][12]?",
                          placeholder = "e.g. ^QK or ^QC_",
                          width = "100%"),
                tags$small(class = "text-muted",
                           "Regex to identify QC samples (e.g. QK1, QC2, QC_Plasma)")
              ),
              tags$hr(style = "margin: 8px 0;"),
              tags$strong("Status:"),
              tags$div(
                style = "margin-top: 5px;",
                textOutput("qk_status_text")
              )
            )
          )
,
          card(
            card_header(class = "bg-light", 
                        tags$div(
                          style = "display: flex; align-items: center; justify-content: space-between;",
                          tags$strong(icon("box"), " Lot Management"),
                          uiOutput("qk_lot_badge")
                        )
            ),
            card_body(
              padding = "10px",
              uiOutput("qk_active_lot_info"),
              tags$hr(style = "margin: 8px 0;"),
              actionButton("qk_new_lot", "New Lot",
                           icon = icon("plus"), class = "btn-outline-success btn-sm",
                           style = "width: 100%;"),
              tags$div(style = "margin-top: 5px;",
                actionButton("qk_archive_lot", "Archive Current Lot",
                             icon = icon("box-archive"), class = "btn-outline-warning btn-sm",
                             style = "width: 100%;")
              ),
              tags$div(style = "margin-top: 5px;",
                actionButton("qk_browse_lots", "Browse Archived Lots",
                             icon = icon("folder-open"), class = "btn-outline-info btn-sm",
                             style = "width: 100%;")
              )
            )
          )
        ),
        navset_card_tab(
          nav_panel("Trend",
                    card(
                      card_header(class = "bg-light",
                                  tags$div(
                                    style = "display: flex; align-items: center; justify-content: space-between;",
                                    tags$strong("QC Trend"),
                                    tags$div(
                                      style = "display: flex; gap: 8px;",
                                      value_box(title = NULL, value = textOutput("qk_kpi_total_inline"),
                                                theme = "primary", height = "50px", width = "100px"),
                                      value_box(title = NULL, value = textOutput("qk_kpi_ool_inline"),
                                                theme = "danger", height = "50px", width = "100px")
                                    )
                                  )
                      ),
                      card_body(plotlyOutput("qk_trend_plot", height = "450px"))
                    )
          ),
          nav_panel("Overview",
                    layout_column_wrap(
                      width = 1/4,
                      style = "margin-bottom: 15px;",
                      value_box(title = "Total Measurements", value = textOutput("qk_kpi_total"),
                                showcase = icon("flask"), theme = "primary"),
                      value_box(title = "Within Limits", value = textOutput("qk_kpi_pass"),
                                showcase = icon("circle-check"), theme = "success"),
                      value_box(title = "Out of Limits", value = textOutput("qk_kpi_fail"),
                                showcase = icon("circle-xmark"), theme = "danger"),
                      value_box(title = "Last Date", value = textOutput("qk_kpi_last_date"),
                                showcase = icon("calendar"), theme = "info")
                    ),
                    card(
                      card_header(class = "bg-light", tags$strong("QC Data Table")),
                      card_body(padding = "10px", DTOutput("qk_data_table"))
                    )
          ),
          nav_panel("Outliers",
                    card(
                      card_header(class = "bg-light",
                                  tags$strong("Measurements Outside Limits")),
                      card_body(padding = "10px", DTOutput("qk_outlier_table"))
                    )
          )
,
          nav_panel("Settings",
                    icon = icon("gear"),
                    card(
                      card_header(class = "bg-light",
                                  tags$div(
                                    style = "display: flex; align-items: center; justify-content: space-between;",
                                    tags$strong(icon("sliders"), " QC Parameter Settings"),
                                    tags$div(
                                      style = "display: flex; gap: 6px;",
                                      actionButton("qk_save_limits", "Save Limits",
                                                   class = "btn-sm btn-primary", icon = icon("floppy-disk")),
                                      downloadButton("qk_download_limits", "Export",
                                                     class = "btn-sm btn-outline-secondary")
                                    )
                                  )
                      ),
                      card_body(
                        padding = "15px",
                        
                        # Import existing limits
                        tags$div(
                          style = "background: #eaf2f8; border-radius: 8px; padding: 12px; margin-bottom: 15px;",
                          tags$div(
                            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                            icon("upload", style = "color: #3498db;"),
                            tags$strong("Import limits file (optional)"),
                            tags$span(class = "text-muted", style = "font-size: 0.82em;",
                                      "CSV with columns: Parameter, Lower, Higher")
                          ),
                          layout_column_wrap(
                            width = 1/2,
                            fileInput("qk_upload_limits", NULL,
                                      accept = c(".csv", ".xlsx", ".xls"),
                                      width = "100%"),
                            tags$div(
                              style = "padding-top: 25px;",
                              uiOutput("qk_upload_limits_status")
                            )
                          )
                        ),
                        
                        # Add new parameter
                        tags$div(
                          style = "background: #f8f9fa; border-radius: 8px; padding: 12px; margin-bottom: 15px;",
                          tags$h6(icon("plus-circle"), " Add Parameter"),
                          tags$div(
                            style = "display: flex; gap: 8px; align-items: end;",
                            tags$div(style = "width: 30%;", uiOutput("qk_param_name_input")),

                            numericInput("qk_new_param_lower", "Lower limit:",
                                         value = NA, width = "20%"),
                            numericInput("qk_new_param_upper", "Upper limit:",
                                         value = NA, width = "20%"),
                            actionButton("qk_add_param", "Add",
                                         class = "btn-sm btn-outline-success", icon = icon("plus"),
                                         style = "margin-bottom: 15px;")
                          )
                        ),
                        
                        # Current parameters table
                        tags$h6(icon("list"), " Monitored Parameters"),
                        DTOutput("qk_limits_table"),
                        
                        tags$div(
                          style = "margin-top: 10px; display: flex; gap: 8px;",
                          actionButton("qk_delete_param", "Delete Selected",
                                       class = "btn-sm btn-outline-danger", icon = icon("trash")),
                          actionButton("qk_edit_param", "Edit Selected",
                                       class = "btn-sm btn-outline-warning", icon = icon("pen"))
                        )
                      )
                    )
          )
        )
      )
    )
  }
  
  # TAB 7: MEASUREMENT ARCHIVE-------
    panels[["meas"]] <- nav_panel(
      "Measurement Archive",
      icon = icon("database"),
      tags$div(
        style = "padding: 15px;",
        layout_column_wrap(
          width = 1/4,
          style = "margin-bottom: 15px;",
          value_box(title = "Total Measurements", value = textOutput("archive_total_meas"),
                    showcase = icon("flask"), theme = "primary"),
          value_box(title = "Projects", value = textOutput("archive_total_projects"),
                    showcase = icon("folder"), theme = "info"),
          value_box(title = "Total Samples", value = textOutput("archive_total_samples"),
                    showcase = icon("vial"), theme = "success"),
          value_box(title = "Last Measurement", value = textOutput("archive_last_date"),
                    showcase = icon("calendar"), theme = "warning")
        ),
        layout_column_wrap(
          width = 1/2,
          style = "margin-bottom: 15px;",
          card(
            card_header(class = "bg-light", tags$strong("Measurements per Project")),
            card_body(plotlyOutput("archive_project_plot", height = "250px"))
          ),
          card(
            card_header(class = "bg-light", tags$strong("Measurements over Time")),
            card_body(plotlyOutput("archive_timeline_plot", height = "250px"))
          )
        ),
        card(
          card_header(class = "bg-light",
                      tags$div(
                        style = "display: flex; align-items: center; justify-content: space-between;",
                        tags$strong("Measurement Archive Overview"),
                        downloadButton("dl_meas_archive", "Download Archive",
                                       class = "btn-outline-primary btn-sm")
                      )
          ),
          card_body(padding = "10px", DTOutput("meas_archive_table"))
        )
      )
    )
  
  # TAB 8: COPY NMR DATA-----
  if (isTRUE(mod$nmr)) {
    panels[["nmr"]] <- nav_panel(
      "NMR Copy",
      icon = icon("magnet"),
      layout_sidebar(
        sidebar = sidebar(
          width = 320,
          open = TRUE,
          card(
            card_header(class = "bg-light", tags$strong("1. Source & Destination")),
            card_body(
              padding = "10px",
              textInput("nmr_source_path", "Source directory (Backup):",
                        value = "D:/Data/", width = "100%"),
              textInput("nmr_dest_path", "Destination directory:",
                        placeholder = "D:/Projects/...", width = "100%"),
              textInput("nmr_mid_level", "Subfolder level:",
                        value = "nmr", width = "100%")
            )
          ),
          card(
            card_header(class = "bg-light", tags$strong("2. Sample Selection")),
            card_body(
              padding = "10px",
              radioButtons("nmr_selection_mode", "Mode:",
                           choices = c("Sample list (Excel)" = "samplelist",
                                       "Select date folders" = "datefolders",
                                       "Project (from Archive)" = "project"),
                           selected = "samplelist"),
              conditionalPanel(
                condition = "input.nmr_selection_mode == 'samplelist'",
                fileInput("nmr_excel_file", "Excel file:",
                          accept = c(".xlsx", ".xls"), width = "100%"),
                textInput("nmr_name_column", "Column name:", value = "Name", width = "100%")
              ),
              conditionalPanel(
                condition = "input.nmr_selection_mode == 'datefolders'",
                actionButton("nmr_scan_dates", "Scan date folders",
                             class = "btn-sm btn-outline-primary", icon = icon("magnifying-glass"),
                             style = "width: 100%; margin-bottom: 10px;"),
                tags$small(class = "text-muted", "Selection in main window")
              ),
              conditionalPanel(
                condition = "input.nmr_selection_mode == 'project'",
                actionButton("nmr_scan_projects", "Scan projects (Archive)",
                             class = "btn-sm btn-outline-primary", icon = icon("magnifying-glass"),
                             style = "width: 100%; margin-bottom: 10px;"),
                tags$small(class = "text-muted", "Selection in main window"),
                tags$hr(style = "margin: 8px 0;"),
                checkboxInput("nmr_project_all_types", "All sample types", value = TRUE),
                conditionalPanel(
                  condition = "input.nmr_project_all_types == false",
                  selectizeInput("nmr_project_type_filter", "Sample type:",
                                 choices = NULL, width = "100%",
                                 multiple = TRUE,
                                 options = list(placeholder = "Select type..."))
                )
              )
            )
          ),
          card(
            card_header(class = "bg-light", tags$strong("3. Experiments")),
            card_body(
              padding = "10px",
              actionButton("nmr_scan_experiments", "Scan experiments",
                           class = "btn-sm btn-outline-primary", icon = icon("magnifying-glass"),
                           style = "width: 100%; margin-bottom: 10px;"),
              tags$small(class = "text-muted", "Scans pulse programs in source directory"),
              tags$hr(style = "margin: 8px 0;"),
              uiOutput("nmr_experiment_selector")
            )
          ),
          card(
            card_header(class = "bg-light", tags$strong("4. Options")),
            card_body(
              padding = "10px",
              checkboxInput("nmr_keep_date_structure", "Keep date folder structure",
                            value = TRUE),
              tags$small(class = "text-muted",
                         "On: dest/date/sample/exp | Off: dest/sample/exp"),
              tags$hr(style = "margin: 8px 0;"),
              checkboxInput("nmr_do_bucketing", "Run PepsNMR Bucketing",
                            value = FALSE),
              conditionalPanel(
                condition = "input.nmr_do_bucketing == true",
                numericInput("nmr_ws_from", "Window Selection from (ppm):", value = 10, width = "100%"),
                numericInput("nmr_ws_to", "Window Selection to (ppm):", value = 0, width = "100%"),
                selectInput("nmr_spectra_type", "Spectra type:",
                            choices = c("serum", "urine"), width = "100%"),
                textInput("nmr_output_csv", "Output CSV path:",
                          placeholder = "signal_intensities.csv", width = "100%")
              )
            )
          ),
          tags$hr(style = "margin: 8px 0;"),
          actionButton("nmr_start_btn", "Start",
                       class = "btn-success", icon = icon("play"),
                       style = "width: 100%;"),
          tags$div(style = "height: 10px;"),
          actionButton("nmr_stop_btn", "Cancel",
                       class = "btn-outline-danger btn-sm", icon = icon("stop"),
                       style = "width: 100%;")
        ),
        tags$div(
          style = "padding: 10px;",
          
          # Archive transfer indicator
          uiOutput("nmr_archive_filter_badge"),
          
          layout_column_wrap(
            width = 1/3,
            style = "margin-bottom: 15px;",
            value_box(title = "Samples Found", value = textOutput("nmr_n_found"),
                      
                      showcase = icon("check"), theme = "success"),
            value_box(title = "Not Found", value = textOutput("nmr_n_missing"),
                      showcase = icon("xmark"), theme = "danger"),
            value_box(title = "Copied", value = textOutput("nmr_n_copied"),
                      showcase = icon("copy"), theme = "primary")
          ),
          conditionalPanel(
            condition = "input.nmr_selection_mode == 'datefolders'",
            card(
              style = "margin-bottom: 15px;",
              card_header(
                class = "bg-light",
                tags$div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  tags$strong("Select Date Folders"),
                  tags$div(
                    actionButton("nmr_select_all_dates", "All",
                                 class = "btn-outline-secondary btn-sm",
                                 style = "margin-right: 5px;"),
                    actionButton("nmr_deselect_all_dates", "None",
                                 class = "btn-outline-secondary btn-sm")
                  )
                )
              ),
              card_body(
                style = "max-height: 500px; overflow-y: auto; padding: 10px;",
                uiOutput("nmr_date_selector")
              ),
              card_footer(
                class = "bg-light",
                textOutput("nmr_date_selection_info")
              )
            )
          ),
          conditionalPanel(
            condition = "input.nmr_selection_mode == 'project'",
            card(
              style = "margin-bottom: 15px;",
              card_header(
                class = "bg-light",
                tags$div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  tags$strong("Projects from Archive"),
                  tags$div(
                    actionButton("nmr_select_all_projects", "All",
                                 class = "btn-outline-secondary btn-sm",
                                 style = "margin-right: 5px;"),
                    actionButton("nmr_deselect_all_projects", "None",
                                 class = "btn-outline-secondary btn-sm")
                  )
                )
              ),
              card_body(
                style = "max-height: 400px; overflow-y: auto; padding: 10px;",
                uiOutput("nmr_project_selector")
              ),
              card_footer(
                class = "bg-light",
                tags$div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  textOutput("nmr_project_selection_info"),
                  textOutput("nmr_project_sample_total")
                )
              )
            )
          ),
          card(
            card_header(class = "bg-light", tags$strong("Progress")),
            card_body(
              padding = "10px",
              tags$div(id = "nmr_progress_container",
                       tags$div(class = "progress", style = "height: 25px; margin-bottom: 10px;",
                                tags$div(id = "nmr_progress_bar",
                                         class = "progress-bar progress-bar-striped progress-bar-animated",
                                         role = "progressbar",
                                         style = "width: 0%;",
                                         "0%")
                       )
              ),
              tags$div(
                style = "max-height: 300px; overflow-y: auto;",
                verbatimTextOutput("nmr_log_output", placeholder = TRUE)
              )
            )
          ),
          conditionalPanel(
            condition = "input.nmr_do_bucketing == true",
            card(
              style = "margin-top: 15px;",
              card_header(class = "bg-light", tags$strong("PepsNMR Result")),
              card_body(
                padding = "10px",
                DTOutput("nmr_bucketing_result"),
                tags$div(style = "margin-top: 10px;",
                         downloadButton("nmr_dl_bucketing", "Download CSV",
                                        class = "btn-outline-success btn-sm")
                )
              )
            )
          )
        )
      )
    )
  }
  
  # TAB 9: NMR SPECTRUM VIEWER-----
  if (isTRUE(mod$viewer)) {
    panels[["nmr_viewer"]] <- nav_panel(
      "Spectra Viewer",
      icon = icon("wave-square"),
      tags$div(
        style = "padding: 15px;",
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            title = "Spectrum Browser",
            
            # Folder selection
            tags$strong("Sample Folder:"),
            tags$div(
              style = "display: flex; gap: 4px; margin-top: 5px;",
              textInput("nmr_view_path", NULL, 
                        placeholder = "Path to sample folder...",
                        width = "100%"),
              actionButton("nmr_view_browse", NULL, 
                           icon = icon("folder-open"),
                           class = "btn-outline-primary btn-sm",
                           style = "margin-top: 0; height: 38px;")
            ),
            
            tags$hr(style = "margin: 8px 0;"),
            
            # Quick access from extracted data
            tags$strong("Quick Access:"),
            tags$div(
              style = "margin-top: 5px;",
              selectizeInput("nmr_view_sample", "From extracted data:",
                             choices = NULL,
                             options = list(placeholder = "Select sample..."),
                             width = "100%")
            ),
            
            tags$hr(style = "margin: 8px 0;"),
            
            # Experiment list
            tags$strong("Experiments:"),
            tags$div(
              style = "margin-top: 5px;",
              DTOutput("nmr_view_exp_table", height = "250px")
            ),
            
            tags$hr(style = "margin: 8px 0;"),
            
            # Display options
            tags$strong("Display Options:"),
            tags$div(
              style = "margin-top: 5px;",
              layout_column_wrap(
                width = 1/2,
                numericInput("nmr_view_ppm_min", "PPM min:", value = -1, step = 0.5, width = "100%"),
                numericInput("nmr_view_ppm_max", "PPM max:", value = 12, step = 0.5, width = "100%")
              ),
              checkboxInput("nmr_view_normalize", "Normalize intensity", value = FALSE),
              checkboxInput("nmr_view_show_regions", "Show IVDr metabolite regions", value = FALSE),
              conditionalPanel(
                condition = "input.nmr_view_show_regions",
                checkboxGroupInput("nmr_view_categories", "Categories:",
                  choices = c("BCAA", "Amino Acid", "Ketone", "Energy", 
                              "Sugar", "Short Chain FA", "Protein/Lipid", "Other"),
                  selected = c("BCAA", "Amino Acid", "Ketone", "Energy", "Sugar", "Short Chain FA"),
                  inline = TRUE
                )
              ),
              tags$hr(style = "margin: 8px 0;"),
              actionButton("nmr_view_overlay", "Overlay Selected",
                           class = "btn-outline-info btn-sm", icon = icon("layer-group"),
                           style = "width: 100%;"),
              tags$div(style = "margin-top: 5px;",
                actionButton("nmr_view_clear", "Clear All",
                             class = "btn-outline-secondary btn-sm", icon = icon("eraser"),
                             style = "width: 100%;")
              )
            )
          ),
          
          # Main content
          layout_columns(
            col_widths = 12,
            
            # Spectrum plot
            card(
              card_header(
                class = "bg-light",
                tags$div(
                  style = "display: flex; align-items: center; justify-content: space-between;",
                  tags$div(
                    icon("wave-square", style = "margin-right: 8px;"),
                    tags$strong("NMR Spectrum")
                  ),
                  tags$div(
                    style = "display: flex; gap: 6px;",
                    uiOutput("nmr_view_info_badge"),
                    downloadButton("nmr_view_download_plot", "Save Plot",
                                   class = "btn-sm btn-outline-secondary"),
                    downloadButton("nmr_view_download_data", "Export CSV",
                                   class = "btn-sm btn-outline-secondary")
                  )
                )
              ),
              card_body(
                padding = "5px",
                plotlyOutput("nmr_view_spectrum", height = "500px")
              )
            ),
            
            # Spectrum info
            card(
              card_header(class = "bg-light", tags$strong(icon("info-circle"), " Acquisition Info")),
              card_body(
                padding = "10px",
                uiOutput("nmr_view_acq_info")
              )
            )
          )
        )
      )
    )
  }

  # TAB 9: SETTINGS (always shown)------
  panels[["settings"]] <- nav_panel(
    title = "Settings",
    icon = icon("gear"),
    tags$div(
      style = "padding: 15px;",
      tags$h4(style = "color: #2c3e50; font-weight: 700; margin-bottom: 20px;",
              icon("gear", style = "margin-right: 10px;"), "Settings"),
      
      navset_card_tab(
        # ---- Archive Column Manager ----
        nav_panel(
          title = "Archive Columns",
          icon = icon("columns"),
          tags$div(
            style = "max-width: 900px; margin: 0 auto; padding: 20px;",
            tags$p(class = "text-muted",
                   "Add or remove additional columns from the archive. ",
                   "These columns can be filled via file upload when creating sample lists."),
            
            layout_column_wrap(
              width = 1/2,
              # Current columns
              card(
                card_header(tags$strong(icon("list"), " Current Archive Columns")),
                card_body(
                  padding = "15px",
                  uiOutput("current_archive_cols_ui")
                )
              ),
              # Add new column
              card(
                card_header(tags$strong(icon("plus-circle"), " Add New Column")),
                card_body(
                  padding = "15px",
                  textInput("new_archive_col_name", "Column Name:",
                            placeholder = "e.g. Age, Sex, Diagnosis...", width = "100%"),
                  selectInput("new_archive_col_type", "Column Type:",
                              choices = c("text", "select", "numeric"),
                              width = "100%"),
                  conditionalPanel(
                    condition = "input.new_archive_col_type == 'select'",
                    textInput("new_archive_col_choices", "Choices (comma-separated):",
                              placeholder = "e.g. m, w, d", width = "100%")
                  ),
                  tags$div(
                    style = "display: flex; gap: 10px; margin-top: 15px;",
                    actionButton("add_archive_col_btn", "Add Column",
                                 class = "btn-primary", icon = icon("plus")),
                    actionButton("remove_archive_col_btn", "Remove Selected",
                                 class = "btn-danger", icon = icon("trash"))
                  )
                )
              )
            ),
            
            tags$div(
              style = "background: #fef9e7; border-radius: 6px; padding: 12px; margin-top: 15px; font-size: 0.85em;",
              icon("triangle-exclamation", style = "color: #f39c12;"),
              " Adding a column will update the config and archive CSV. ",
              "Removing a column removes it from the config but ",
              tags$strong("does not delete existing data"), " from the CSV."
            )
          )
        ),
        
        # ---- Project Column Manager ----
        nav_panel(
          title = "Project Columns",
          icon = icon("folder-open"),
          tags$div(
            style = "max-width: 900px; margin: 0 auto; padding: 20px;",
            tags$p(class = "text-muted",
                   "Add or remove columns from the project list. ",
                   "Title and Abbreviation are required and cannot be removed."),

            layout_column_wrap(
              width = 1/2,
              card(
                card_header(tags$strong(icon("list"), " Current Project Columns")),
                card_body(
                  padding = "15px",
                  uiOutput("current_project_cols_ui")
                )
              ),
              card(
                card_header(tags$strong(icon("plus-circle"), " Add New Column")),
                card_body(
                  padding = "15px",
                  textInput("new_project_col_name", "Column Name:",
                            placeholder = "e.g. Funding_Source", width = "100%"),
                  tags$small(class = "text-muted",
                             "Spaces and special characters become underscores."),
                  tags$div(
                    style = "display: flex; gap: 8px; margin-top: 15px;",
                    actionButton("add_project_col_btn", "Add Column",
                                 class = "btn-primary", icon = icon("plus")),
                    actionButton("remove_project_col_btn", "Remove Selected",
                                 class = "btn-danger", icon = icon("trash"))
                  )
                )
              )
            ),

            tags$div(
              style = "background: #fef9e7; border-radius: 6px; padding: 12px; margin-top: 15px; font-size: 0.85em;",
              icon("triangle-exclamation", style = "color: #f39c12;"),
              " Adding a column updates the config and projects CSV. ",
              "Removing a column removes it from the config but ",
              tags$strong("does not delete existing data"), " from the CSV."
            )
          )
        ),
        
        # ---- Module Manager ----
        nav_panel(
          title = "Modules",
          icon = icon("puzzle-piece"),
          tags$div(
            style = "max-width: 600px; margin: 0 auto; padding: 20px;",
            tags$p(class = "text-muted",
                   "Enable or disable modules. Changes take effect after restart."),
            uiOutput("module_toggles_ui"),
            tags$div(
              style = "margin-top: 20px;",
              actionButton("save_module_settings", "Save & Restart",
                           class = "btn-warning", icon = icon("rotate-right"))
            )
          )
        ),
        
        # ---- General Settings ----
        nav_panel(
          title = "General",
          icon = icon("sliders"),
          tags$div(
            style = "max-width: 600px; margin: 0 auto; padding: 20px;",
            textInput("settings_lab_name", "Lab Name:",
                      value = if (!is.null(config)) config$lab_name else "",
                      width = "100%"),
            textInput("settings_data_path", "Data Path:",
                      value = if (!is.null(config)) config$data_path else "",
                      width = "100%"),
            tags$div(
              style = "margin-top: 20px;",
              actionButton("save_general_settings", "Save",
                           class = "btn-primary", icon = icon("floppy-disk"))
            )
          )
        )
      )
    )
  )
  
  
  
  # ---- BUILD PAGE WITH do.call ----
  navbar_args <- list(
    id = "main_nav",
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .selectize-dropdown {
          z-index: 10000 !important;
          max-height: 300px !important;
          overflow-y: auto !important;
        }
        .sidebar .card-body { overflow: visible !important; }
        .sidebar .card.shadow-sm:hover {
          transform: translateX(5px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.15) !important;
          transition: all 0.2s ease;
        }
        #projects_table table { font-size: 11px !important; }
        #projects_table table th { font-size: 11px !important; padding: 4px 6px !important; }
        #projects_table table td { font-size: 11px !important; padding: 3px 6px !important; }
        #projects_table .dataTables_filter,
        #projects_table .dataTables_info,
        #projects_table .dataTables_length,
        #projects_table .dataTables_paginate { font-size: 11px !important; }

      "))
    ),
    title = lab_name,
    theme = bs_theme(
      version = 5, bootswatch = "flatly",
      primary = "#2c3e50", secondary = "#18bc9c",
      success = "#18bc9c", info = "#3498db",
      font_scale = 0.9, `enable-rounded` = TRUE
    )
  )
  
  # Add all panels to navbar args
  navbar_args <- c(navbar_args, unname(panels))

  # LarmoR branding, pinned to the right of the navbar
  navbar_args <- c(navbar_args, list(
    nav_spacer(),
    nav_item(
      tags$div(
        style = "display:flex; align-items:center; gap:8px; padding:0 12px; height:100%;",
        tags$img(src = "logo.png", height = "26px"),
        tags$span(style = "font-weight:600; font-size:1.05em; color:white;", "LarmoR")
      )
    )
  ))
  
  # Build the page
  do.call(page_navbar, navbar_args)
}


ui <- function(request) {
  if (config_exists()) {
    main_app_ui()
  } else {
    setup_ui()
  }
}
# END OF UI


# SERVER -------

server <- function(input, output, session) {
  # DEBUG: Show where files are being read from
  message("========================================")
  message("getwd(): ", getwd())
  message("APP_DIR: ", APP_DIR)
  message("CONFIG_PATH: ", CONFIG_PATH)
  message("config.json exists: ", file.exists(CONFIG_PATH))
  message("archive.csv exists: ", file.exists(file.path(APP_DIR, "archive.csv")))
  message("archive.csv at getwd: ", file.exists(file.path(getwd(), "archive.csv")))
  message("========================================")


  
# Reset button ------------------------------------------------------------

  observeEvent(input$reset_setup, {
    showModal(modalDialog(
      title = "Reset Setup?",
      tags$p("Config will be deleted. The setup wizard will appear on next start."),
      tags$p(class = "text-danger", tags$strong("Data files will be kept!")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset_setup", "Reset", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset_setup, {
    file.remove(file.path(APP_DIR, "config.json"))
    removeModal()
    showNotification("Config deleted. Please restart the app.", type = "message")
  })
  
  
  # SETUP WIZARD SERVER--------
  
  
  if (!config_exists()) {
    
    # Custom columns storage
    rv_setup <- reactiveValues(
      custom_archive_cols = data.frame(name = character(), desc = character(), stringsAsFactors = FALSE),
      custom_project_cols = data.frame(name = character(), desc = character(), stringsAsFactors = FALSE)
    )
    
    # ---- Step navigation ----
    setup_go_to_step <- function(step) {
      for (i in 1:5) {
        shinyjs::toggleClass(paste0("setup_step_", i), "active", condition = (i == step))
        shinyjs::toggleClass(paste0("dot", i), "active", condition = (i == step))
        shinyjs::toggleClass(paste0("dot", i), "done", condition = (i < step))
      }
    }
    

  # ---- SETUP: Archive Upload & Column Mapping ----
  rv_setup_upload <- reactiveValues(
    data = NULL,
    headers = NULL
  )
  
  observeEvent(input$setup_upload_archive, {
    req(input$setup_upload_archive)
    file <- input$setup_upload_archive
    ext <- tolower(tools::file_ext(file$name))
    
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "tsv") {
        read.delim(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(file$datapath, col_types = "text"))
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (is.null(df) || ncol(df) == 0) {
      rv_setup_upload$data <- NULL
      rv_setup_upload$headers <- NULL
      showNotification("Could not read file. Please check format.", type = "error")
      return()
    }
    
    rv_setup_upload$data <- df
    rv_setup_upload$headers <- names(df)
    showNotification(paste0("File loaded: ", ncol(df), " columns, ", nrow(df), " rows (preview)"), 
                     type = "message", duration = 4)
  })
  
  output$setup_upload_status <- renderUI({
    headers <- rv_setup_upload$headers
    if (is.null(headers)) {
      return(tags$div(
        style = "padding: 8px; color: #7f8c8d; font-size: 0.85em;",
        icon("info-circle"), " Upload a CSV or Excel file to auto-detect columns"
      ))
    }
    
    tags$div(
      style = "background: #d5f5e3; border-radius: 6px; padding: 8px 12px; font-size: 0.85em;",
      icon("check-circle", style = "color: #18bc9c;"),
      tags$strong(paste0(length(headers), " columns detected: ")),
      tags$span(style = "color: #555;", paste(headers, collapse = ", "))
    )
  })
  
  output$setup_column_mapping <- renderUI({
    headers <- rv_setup_upload$headers
    if (is.null(headers)) return(NULL)
    
    none_option <- c("-- skip --" = "")
    col_choices <- c(none_option, setNames(headers, headers))
    
    # Auto-match common column names
    auto_match <- function(target, candidates) {
      patterns <- switch(target,
        "Date" = c("date", "datum", "received", "eingang"),
        "Name" = c("name", "sample", "probe", "id", "sample_id"),
        "Project" = c("project", "projekt", "study"),
        "Type" = c("type", "typ", "sample.type", "matrix"),
        "Size" = c("size", "volume", "groesse"),
        "Box" = c("box", "box_name"),
        "Box_Code" = c("box_code", "barcode"),
        "Sex" = c("sex", "gender", "geschlecht"),
        "Age" = c("age", "alter"),
        "Diagnosis" = c("diagnosis", "diagnose"),
        "Timepoint" = c("timepoint", "zeitpunkt", "visit"),
        "Storage" = c("storage", "lagerort", "location"),
        "Operator" = c("operator", "user", "technician"),
        "Notes" = c("notes", "note", "comment", "remarks", "bemerkung"),
        c()
      )
      for (p in patterns) {
        match_idx <- grep(paste0("^", p, "$"), tolower(candidates))
        if (length(match_idx) > 0) return(candidates[match_idx[1]])
        partial_idx <- grep(p, tolower(candidates))
        if (length(partial_idx) > 0) return(candidates[partial_idx[1]])
      }
      return("")
    }
    
    required_fields <- c("Date", "Name", "Project")
    optional_fields <- c("Type", "Size", "Box", "Box_Code", "Sex", "Age", 
                         "Diagnosis", "Timepoint", "Storage", "Operator", "Notes")
    all_fields <- c(required_fields, optional_fields)
    
    # Find unmapped columns
    mapped_cols <- sapply(all_fields, function(f) auto_match(f, headers))
    unmapped <- setdiff(headers, mapped_cols[mapped_cols != ""])
    
    tags$div(
      style = "background: #fff; border: 1px solid #dee2e6; border-radius: 8px; padding: 12px; margin-top: 10px;",
      tags$h6(icon("arrows-left-right"), " Map your columns to archive fields:"),
      
      tags$div(
        style = "display: grid; grid-template-columns: 130px 1fr; gap: 4px 10px; align-items: center; font-size: 0.85em;",
        
        tagList(lapply(all_fields, function(field) {
          auto <- auto_match(field, headers)
          is_req <- field %in% required_fields
          tagList(
            tags$div(
              style = paste0("font-weight: ", if (is_req) "700" else "500", 
                             "; padding: 2px 0;",
                             if (is_req) " color: #1a7a4c;" else ""),
              if (is_req) icon("asterisk", style = "font-size: 0.6em; margin-right: 2px;"),
              field
            ),
            selectInput(
              paste0("setup_map_", tolower(field)),
              NULL,
              choices = col_choices,
              selected = auto,
              width = "100%"
            )
          )
        }))
      ),
      
      # Show unmapped columns
      if (length(unmapped) > 0) {
        tags$div(
          style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid #eee;",
          tags$div(
            style = "font-size: 0.82em; color: #7f8c8d; margin-bottom: 6px;",
            icon("info-circle"),
            paste0(" ", length(unmapped), " unmapped columns will be added as custom fields: "),
            tags$strong(paste(unmapped, collapse = ", "))
          ),
          checkboxInput("setup_import_unmapped", "Add unmapped columns as custom fields", value = TRUE)
        )
      }
    )
  })
    observeEvent(input$setup_next_1, setup_go_to_step(2))
    observeEvent(input$setup_back_2, setup_go_to_step(1))
    observeEvent(input$setup_next_2, setup_go_to_step(3))
    observeEvent(input$setup_back_3, setup_go_to_step(2))
    observeEvent(input$setup_next_3, setup_go_to_step(4))
    observeEvent(input$setup_back_4, setup_go_to_step(3))
    observeEvent(input$setup_next_4, setup_go_to_step(5))
    observeEvent(input$setup_back_5, setup_go_to_step(4))
    
    # ---- Add custom archive columns ----
    observeEvent(input$setup_add_custom_col, {
      name <- trimws(input$setup_custom_col_name)
      desc <- trimws(input$setup_custom_col_desc)
      if (nchar(name) == 0) {
        showNotification("Please enter a field name!", type = "warning")
        return()
      }
      # Sanitize name
      name_clean <- gsub("[^A-Za-z0-9_]", "_", name)
      rv_setup$custom_archive_cols <- rbind(rv_setup$custom_archive_cols,
                                            data.frame(name = name_clean, desc = desc, stringsAsFactors = FALSE))
      updateTextInput(session, "setup_custom_col_name", value = "")
      updateTextInput(session, "setup_custom_col_desc", value = "")
    })
    
    output$setup_custom_cols_list <- renderUI({
      df <- rv_setup$custom_archive_cols
      if (nrow(df) == 0) return(NULL)
      
      tags$div(
        lapply(seq_len(nrow(df)), function(i) {
          tags$div(class = "field-row",
                   tags$span(class = "field-name", icon("plus-circle", style = "color: #3498db;"), paste0(" ", df$name[i])),
                   tags$span(class = "field-desc", df$desc[i]),
                   actionButton(paste0("setup_remove_col_", i), icon("trash"),
                                class = "btn-sm btn-outline-danger",
                                style = "padding: 2px 8px;")
          )
        })
      )
    })
    
    # Remove custom archive columns
    observe({
      df <- rv_setup$custom_archive_cols
      if (nrow(df) == 0) return()
      lapply(seq_len(nrow(df)), function(i) {
        observeEvent(input[[paste0("setup_remove_col_", i)]], {
          rv_setup$custom_archive_cols <- rv_setup$custom_archive_cols[-i, , drop = FALSE]
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
    
    # ---- Add custom project columns ----
    observeEvent(input$setup_add_custom_proj, {
      name <- trimws(input$setup_custom_proj_name)
      desc <- trimws(input$setup_custom_proj_desc)
      if (nchar(name) == 0) {
        showNotification("Please enter a field name!", type = "warning")
        return()
      }
      name_clean <- gsub("[^A-Za-z0-9_]", "_", name)
      rv_setup$custom_project_cols <- rbind(rv_setup$custom_project_cols,
                                            data.frame(name = name_clean, desc = desc, stringsAsFactors = FALSE))
      updateTextInput(session, "setup_custom_proj_name", value = "")
      updateTextInput(session, "setup_custom_proj_desc", value = "")
    })
    
    output$setup_custom_projs_list <- renderUI({
      df <- rv_setup$custom_project_cols
      if (nrow(df) == 0) return(NULL)
      
      tags$div(
        lapply(seq_len(nrow(df)), function(i) {
          tags$div(class = "field-row",
                   tags$span(class = "field-name", icon("plus-circle", style = "color: #3498db;"), paste0(" ", df$name[i])),
                   tags$span(class = "field-desc", df$desc[i]),
                   actionButton(paste0("setup_remove_proj_", i), icon("trash"),
                                class = "btn-sm btn-outline-danger",
                                style = "padding: 2px 8px;")
          )
        })
      )
    })
    
    observe({
      df <- rv_setup$custom_project_cols
      if (nrow(df) == 0) return()
      lapply(seq_len(nrow(df)), function(i) {
        observeEvent(input[[paste0("setup_remove_proj_", i)]], {
          rv_setup$custom_project_cols <- rv_setup$custom_project_cols[-i, , drop = FALSE]
        }, ignoreInit = TRUE, once = TRUE)
      })
    })
  
    # ---- Summary ----
    output$setup_summary <- renderUI({
      archive_cols <- c("Date", "Name", "Project")
      if (isTRUE(input$setup_col_typ)) archive_cols <- c(archive_cols, "Type")
      if (isTRUE(input$setup_col_groesse)) archive_cols <- c(archive_cols, "Size")
      if (isTRUE(input$setup_col_box)) archive_cols <- c(archive_cols, "Box")
      if (isTRUE(input$setup_col_box_code)) archive_cols <- c(archive_cols, "Box_Code")
      if (isTRUE(input$setup_col_sex)) archive_cols <- c(archive_cols, "Sex")
      if (isTRUE(input$setup_col_age)) archive_cols <- c(archive_cols, "Age")
      if (isTRUE(input$setup_col_diagnosis)) archive_cols <- c(archive_cols, "Diagnosis")
      if (isTRUE(input$setup_col_timepoint)) archive_cols <- c(archive_cols, "Timepoint")
      if (isTRUE(input$setup_col_storage)) archive_cols <- c(archive_cols, "Storage")
      if (isTRUE(input$setup_col_operator)) archive_cols <- c(archive_cols, "Operator")
      if (isTRUE(input$setup_col_notes)) archive_cols <- c(archive_cols, "Notes")
      if (nrow(rv_setup$custom_archive_cols) > 0) {
        archive_cols <- c(archive_cols, rv_setup$custom_archive_cols$name)
      }
      
      project_cols <- c("Title", "Abbreviation")
      if (isTRUE(input$setup_proj_pi)) project_cols <- c(project_cols, "PI")
      if (isTRUE(input$setup_proj_email)) project_cols <- c(project_cols, "Email")
      if (isTRUE(input$setup_proj_contact)) project_cols <- c(project_cols, "Contact")
      if (isTRUE(input$setup_proj_clinic)) project_cols <- c(project_cols, "Clinic_Institute")
      if (isTRUE(input$setup_proj_group)) project_cols <- c(project_cols, "AG")
      if (isTRUE(input$setup_proj_phone)) project_cols <- c(project_cols, "Phone")
      if (isTRUE(input$setup_proj_description)) project_cols <- c(project_cols, "Description")
      if (isTRUE(input$setup_proj_start)) project_cols <- c(project_cols, "Start_Date")
      if (isTRUE(input$setup_proj_end)) project_cols <- c(project_cols, "End_Date")
      if (isTRUE(input$setup_proj_status)) project_cols <- c(project_cols, "Status")
      if (isTRUE(input$setup_proj_ethik)) project_cols <- c(project_cols, "Ethics_Approval")
      if (isTRUE(input$setup_proj_notes)) project_cols <- c(project_cols, "Notes_Project")
      if (nrow(rv_setup$custom_project_cols) > 0) {
        project_cols <- c(project_cols, rv_setup$custom_project_cols$name)
      }
      
      modules <- character(0)
      modules <- c("Sample Submission", "Data Extraction", "Sample Archive",
                   "Projects", "Measurement Archive")
      if (isTRUE(input$setup_mod_viewer)) modules <- c(modules, "Spectra Viewer")
      if (isTRUE(input$setup_mod_qk))     modules <- c(modules, "QC Monitor")
      if (isTRUE(input$setup_mod_boxes))  modules <- c(modules, "Biobank")
      if (isTRUE(input$setup_mod_nmr))    modules <- c(modules, "NMR Copy")
      
      # Check for uploaded archive
      has_upload <- !is.null(rv_setup_upload$data)
      upload_rows <- if (has_upload) nrow(rv_setup_upload$data) else 0
      upload_cols <- if (has_upload) ncol(rv_setup_upload$data) else 0
      
      tagList(
        tags$h6(icon("building"), " Lab: ", tags$strong(input$setup_lab_name)),
        if (nchar(trimws(input$setup_data_path)) > 0) {
          tags$div(style = "font-size: 0.85em; color: #7f8c8d; margin-bottom: 5px;",
                   icon("folder"), " Data path: ", tags$code(input$setup_data_path))
        },
        
        # Import info
        if (has_upload) {
          tags$div(
            style = "background: #d1ecf1; border-radius: 6px; padding: 10px; margin: 10px 0; font-size: 0.9em;",
            icon("file-import", style = "color: #17a2b8;"),
            tags$strong(" Archive Import: "),
            paste0(upload_rows, " rows from uploaded file will be imported with ",
                   sum(sapply(c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code",
                                "Sex", "Age", "Diagnosis", "Timepoint", "Storage", "Operator", "Notes"),
                              function(f) {
                                val <- input[[paste0("setup_map_", tolower(f))]]
                                !is.null(val) && nchar(val) > 0
                              })),
                   " mapped columns.")
          )
        },
        
        tags$hr(),
        
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(class = "bg-light", tags$strong(icon("archive"), 
                        paste0(" Archive Fields (", length(archive_cols), ")"))),
            card_body(
              padding = "10px",
              style = "max-height: 200px; overflow-y: auto;",
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 4px;",
                lapply(archive_cols, function(col) {
                  is_required <- col %in% c("Date", "Name", "Project")
                  tags$span(
                    class = paste0("badge ", if (is_required) "bg-success" else "bg-primary"),
                    style = "font-size: 0.82em; padding: 4px 8px; font-weight: 500;",
                    if (is_required) icon("lock", style = "font-size: 0.7em; margin-right: 3px;"),
                    col
                  )
                })
              )
            )
          ),
          card(
            card_header(class = "bg-light", tags$strong(icon("folder-open"), 
                        paste0(" Project Fields (", length(project_cols), ")"))),
            card_body(
              padding = "10px",
              style = "max-height: 200px; overflow-y: auto;",
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 4px;",
                lapply(project_cols, function(col) {
                  is_required <- col %in% c("Title", "Abbreviation")
                  tags$span(
                    class = paste0("badge ", if (is_required) "bg-success" else "bg-primary"),
                    style = "font-size: 0.82em; padding: 4px 8px; font-weight: 500;",
                    if (is_required) icon("lock", style = "font-size: 0.7em; margin-right: 3px;"),
                    col
                  )
                })
              )
            )
          )
        ),
        
        tags$div(style = "margin-top: 10px;",
          card(
            card_header(class = "bg-light", tags$strong(icon("puzzle-piece"), 
                        paste0(" Active Modules (", length(modules), ")"))),
            card_body(
              padding = "10px",
              tags$div(
                style = "display: flex; flex-wrap: wrap; gap: 6px;",
                lapply(modules, function(mod) {
                  mod_icon <- switch(mod,
                    "Sample Archive" = "archive",
                    "Biobank" = "box",
                    "Data Extraction" = "file-code",
                    "QC Monitor" = "chart-line",
                    "NMR Copy" = "magnet",
                    "Measurement Archive" = "clock-rotate-left",
                    "cube"
                  )
                  tags$span(
                    class = "badge bg-info",
                    style = "font-size: 0.85em; padding: 5px 10px;",
                    icon(mod_icon, style = "margin-right: 4px;"), mod
                  )
                })
              )
            )
          )
        )
      )
    })
    
    # ---- FINISH: Create files and config ----
    observeEvent(input$setup_finish, {
      
      # ---- Build archive columns ----
      archive_cols <- c("Date", "Name", "Project")
      archive_col_types <- list(
        Date = "date", Name = "text", Project = "select"
      )
      
      optional_archive <- list(
        list(id = "setup_col_typ", name = "Type", type = "select"),
        list(id = "setup_col_groesse", name = "Size", type = "text"),
        list(id = "setup_col_box", name = "Box", type = "text"),
        list(id = "setup_col_box_code", name = "Box_Code", type = "text"),
        list(id = "setup_col_sex", name = "Sex", type = "select"),
        list(id = "setup_col_age", name = "Age", type = "numeric"),
        list(id = "setup_col_diagnosis", name = "Diagnosis", type = "text"),
        list(id = "setup_col_timepoint", name = "Timepoint", type = "text"),
        list(id = "setup_col_storage", name = "Storage", type = "text"),
        list(id = "setup_col_operator", name = "Operator", type = "text"),
        list(id = "setup_col_notes", name = "Notes", type = "text")
      )
      
      for (opt in optional_archive) {
        if (isTRUE(input[[opt$id]])) {
          archive_cols <- c(archive_cols, opt$name)
          archive_col_types[[opt$name]] <- opt$type
        }
      }
      
      # Add custom archive columns
      if (nrow(rv_setup$custom_archive_cols) > 0) {
        for (i in seq_len(nrow(rv_setup$custom_archive_cols))) {
          col_name <- rv_setup$custom_archive_cols$name[i]
          archive_cols <- c(archive_cols, col_name)
          archive_col_types[[col_name]] <- "text"
        }
      }
      
      # ---- Build project columns ----
      project_cols <- c("Title", "Abbreviation")
      project_col_types <- list(
        Title = "text", Abbreviation = "text"
      )
      
      optional_project <- list(
        list(id = "setup_proj_pi", name = "PI", type = "text"),
        list(id = "setup_proj_email", name = "Email", type = "text"),
        list(id = "setup_proj_contact", name = "Contact", type = "text"),
        list(id = "setup_proj_clinic", name = "Clinic_Institute", type = "text"),
        list(id = "setup_proj_group", name = "AG", type = "text"),
        list(id = "setup_proj_phone", name = "Phone", type = "text"),
        list(id = "setup_proj_description", name = "Description", type = "text"),
        list(id = "setup_proj_start", name = "Start_Date", type = "date"),
        list(id = "setup_proj_end", name = "End_Date", type = "date"),
        list(id = "setup_proj_status", name = "Status", type = "select"),
        list(id = "setup_proj_ethik", name = "Ethics_Approval", type = "text"),
        list(id = "setup_proj_notes", name = "Notes_Project", type = "text")
      )
      
      for (opt in optional_project) {
        if (isTRUE(input[[opt$id]])) {
          project_cols <- c(project_cols, opt$name)
          project_col_types[[opt$name]] <- opt$type
        }
      }
      
      # Add custom project columns
      if (nrow(rv_setup$custom_project_cols) > 0) {
        for (i in seq_len(nrow(rv_setup$custom_project_cols))) {
          col_name <- rv_setup$custom_project_cols$name[i]
          project_cols <- c(project_cols, col_name)
          project_col_types[[col_name]] <- "text"
        }
      }
      
      # ---- Build modules list ----
      modules <- list(
          # Core modules - always enabled
          submission = TRUE,
          archive    = TRUE,
          xml        = TRUE,
          projects   = TRUE,
          meas       = TRUE,
          # Optional modules
          viewer = isTRUE(input$setup_mod_viewer),
          qk     = isTRUE(input$setup_mod_qk),
          boxes  = isTRUE(input$setup_mod_boxes),
          nmr    = isTRUE(input$setup_mod_nmr)
      )
      
      # ---- Create config ----
      config <- list(
        lab_name = input$setup_lab_name,
        data_path = input$setup_data_path,
        created = as.character(Sys.time()),
        version = "2.0",
        archive = list(
          columns = archive_cols,
          column_types = archive_col_types
        ),
        projects = list(
          columns = project_cols,
          column_types = project_col_types
        ),
        modules = modules
      )
      
      
      # ---- Create/Import archive.csv ----
      archive_path <- file.path(APP_DIR, "archive.csv")
      uploaded_data <- rv_setup_upload$data
      uploaded_headers <- rv_setup_upload$headers
      
      if (!is.null(uploaded_data) && !is.null(uploaded_headers)) {
        # ---- Import uploaded archive with column mapping ----
        mapping <- list()
        all_fields <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code",
                        "Sex", "Age", "Diagnosis", "Timepoint", "Storage", "Operator", "Notes")
        
        for (field in all_fields) {
          map_id <- paste0("setup_map_", tolower(field))
          mapped_col <- input[[map_id]]
          if (!is.null(mapped_col) && nchar(mapped_col) > 0) {
            mapping[[field]] <- mapped_col
          }
        }
        
        # Build the imported data frame with standardized column names
        import_df <- data.frame(matrix(ncol = 0, nrow = nrow(uploaded_data)), stringsAsFactors = FALSE)
        
        # Map columns
        mapped_source_cols <- c()
        for (target in names(mapping)) {
          source_col <- mapping[[target]]
          if (source_col %in% names(uploaded_data)) {
            import_df[[target]] <- as.character(uploaded_data[[source_col]])
            mapped_source_cols <- c(mapped_source_cols, source_col)
            
            # Auto-enable the corresponding checkbox column in archive_cols
            if (!target %in% archive_cols) {
              archive_cols <- c(archive_cols, target)
              archive_col_types[[target]] <- "text"
            }
          }
        }
        
        # Add unmapped columns as custom fields if requested
        if (isTRUE(input$setup_import_unmapped)) {
          unmapped <- setdiff(uploaded_headers, mapped_source_cols)
          for (col in unmapped) {
            safe_name <- gsub("[^a-zA-Z0-9_]", "_", col)
            if (!safe_name %in% names(import_df) && !safe_name %in% archive_cols) {
              import_df[[safe_name]] <- as.character(uploaded_data[[col]])
              archive_cols <- c(archive_cols, safe_name)
              archive_col_types[[safe_name]] <- "text"
            }
          }
        }
        
        # Ensure all configured archive columns exist in import
        for (col in archive_cols) {
          if (!col %in% names(import_df)) {
            import_df[[col]] <- NA_character_
          }
        }
        
        # Reorder to match archive_cols
        import_df <- import_df[, archive_cols, drop = FALSE]
        
        # Write imported archive
        write.csv(import_df, archive_path, row.names = FALSE)
        message("Imported archive with ", nrow(import_df), " rows and ", ncol(import_df), " columns")
        
        # Update the config with any new columns from mapping
        config$archive$columns <- archive_cols
        config$archive$column_types <- archive_col_types
        
      } else if (!file.exists(archive_path)) {
        # No upload - create empty archive
        archive_df <- data.frame(matrix(ncol = length(archive_cols), nrow = 0))
        names(archive_df) <- archive_cols
        write.csv(archive_df, archive_path, row.names = FALSE)
        message("Created empty archive.csv at: ", archive_path)
      } else {
        # Archive exists - add missing columns
        existing_df <- read.csv(archive_path, nrows = 1, check.names = FALSE)
        missing_in_file <- setdiff(archive_cols, names(existing_df))
        if (length(missing_in_file) > 0) {
          full_df <- read.csv(archive_path, check.names = FALSE)
          for (col in missing_in_file) full_df[[col]] <- NA_character_
          write.csv(full_df, archive_path, row.names = FALSE)
          message("Added columns to existing archive.csv: ", paste(missing_in_file, collapse = ", "))
        }
      }
      
      # ---- Create empty projects.csv ----
      proj_path <- file.path(APP_DIR, "projects.csv")
      if (!file.exists(proj_path)) {
        projects_df <- data.frame(matrix(ncol = length(project_cols), nrow = 0))
        names(projects_df) <- project_cols
        write.csv(projects_df, proj_path, row.names = FALSE)
        message("Created projects.csv at: ", proj_path)
      } else {
        existing_df <- read.csv(proj_path, nrows = 1, check.names = FALSE)
        missing_in_file <- setdiff(project_cols, names(existing_df))
        if (length(missing_in_file) > 0) {
          full_df <- read.csv(proj_path, check.names = FALSE)
          for (col in missing_in_file) full_df[[col]] <- NA_character_
          write.csv(full_df, proj_path, row.names = FALSE)
          message("Added columns to existing projects.csv: ", paste(missing_in_file, collapse = ", "))
        }
      }
      
      
      
      
      # ---- Create empty box_registry.csv ----
      box_path <- file.path(APP_DIR, "box_registry.csv")
      if (!file.exists(box_path)) {
        box_df <- data.frame(
          Box_Code = character(), Project = character(),
          Box_Name = character(), Sample_Count = integer(),
          Received = character(), Status = character(),
          Measured_Date = character(), Notes = character(),
          stringsAsFactors = FALSE
        )
        write.csv(box_df, box_path, row.names = FALSE)
        message("Created box_registry.csv at: ", box_path)
      }
      
      # ---- Save config ----
      save_config(config)
      message("Config saved to: ", CONFIG_PATH)
      
      # ---- Restart modal ----
      showModal(modalDialog(
        title = tags$div(icon("check-circle", style = "color: #18bc9c; font-size: 2em;"),
                         " Setup Complete!"),
        tags$div(
          style = "text-align: center; padding: 20px;",
          tags$p("Configuration has been saved."),
          tags$p("The app will now restart..."),
          tags$hr(),
          tags$p(class = "text-muted",
                 icon("info-circle"),
                 " You can change these settings later under 'Settings'.")
        ),
        footer = actionButton("setup_restart", "Restart App",
                              class = "btn-success", icon = icon("rotate-right")),
        easyClose = FALSE
      ))
    })
    
    observeEvent(input$setup_restart, {
      session$reload()
    })
    
    # Stop here don't run the main app logic
    return()
  }
  
  
  # LOAD CONFIG
  config <- read_config()
  
  archive_columns <- if (!is.null(config)) config$archive$columns else c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")

  # ---- Schema reconciliation ----
  # config.json is the source of truth for which columns exist. If a CSV on
  # disk is missing columns (e.g. added via Settings) or has extras, align it
  # here rather than letting rbind() fail later with a length mismatch.
  reconcile_cols <- function(df, cfg_cols) {
    if (is.null(df)) return(df)
    cfg_cols <- as.character(unlist(cfg_cols))
    for (m in setdiff(cfg_cols, names(df))) {
      df[[m]] <- if (nrow(df) == 0) character(0) else NA_character_
    }
    extras <- setdiff(names(df), cfg_cols)
    df[, c(cfg_cols, extras), drop = FALSE]
  }
  project_columns <- if (!is.null(config)) config$projects$columns else c("Title", "Abbreviation")
  active_modules <- if (!is.null(config)) config$modules else list(archive = TRUE, boxes = TRUE, xml = TRUE, qk = TRUE, nmr = TRUE, meas = TRUE)
  
  # FILE PATHS
  projects_csv_path <- file.path(APP_DIR, "projects.csv")
  projects_xlsx_path <- file.path(APP_DIR, "Projects.xlsx")
  
  # Use config data_path for NMR source and data path
  observe({
    config <- read_config()
    if (!is.null(config) && !is.null(config$data_path)) {
      updateTextInput(session, "nmr_source_path", value = config$data_path)
      updateTextInput(session, "data_path", value = config$data_path)
    }
  }, priority = 50)
  
  
  output$sample_input_fields <- renderUI({
    config <- read_config()
    
    if (is.null(config) || is.null(config$archive)) {
      return(NULL)
    }
    
    cols <- config$archive$columns
    
    # These are auto-filled — never shown as manual inputs
    auto_cols <- c("Date", "Name", "Project", "Box", "Box_Code", "Type", "Size")
    extra_cols <- setdiff(cols, auto_cols)
    
    if (length(extra_cols) == 0) {
      return(NULL)
    }
    
    # Show info about additional columns — only via file upload
    tagList(
      tags$div(
        style = "background: #eaf2f8; border-radius: 6px; padding: 10px; margin-bottom: 10px;",
        tags$strong(icon("info-circle"), " Additional Archive Fields"),
        tags$p(
          style = "font-size: 0.85em; margin-top: 5px; margin-bottom: 5px;",
          "The following fields are configured but can ",
          tags$strong("only be filled via file upload"), ":"
        ),
        tags$ul(
          style = "font-size: 0.8em; margin-bottom: 5px; padding-left: 20px;",
          lapply(extra_cols, function(col) {
            tags$li(tags$code(col))
          })
        ),
        tags$div(
          style = "background: #fef9e7; border-radius: 4px; padding: 8px; margin-top: 8px; font-size: 0.8em;",
          icon("triangle-exclamation", style = "color: #f39c12;"),
          " Your uploaded file must have columns with ",
          tags$strong("exactly matching names"), 
          " (case-sensitive). Unmatched columns will be left empty."
        )
      )
    )
  })
  
  
  
  
    
 # demo mode
  rv_demo <- reactiveValues(active = FALSE)
  
  observeEvent(input$demo_mode_btn, {
    rv_demo$active <- !rv_demo$active
    
    if (rv_demo$active) {
      updateActionButton(session, "demo_mode_btn", label = "Demo-Modus aktiv",
                         icon = icon("eye-slash"))
      showNotification("Demo mode activated - project names anonymized.",
                       type = "message", duration = 3)
    } else {
      updateActionButton(session, "demo_mode_btn", label = "Demo-Modus",
                         icon = icon("eye"))
      showNotification("Demo-Modus deaktiviert.", type = "message", duration = 3)
    }
  })


  
  # ---- Demo mode: anonymized project choices ----
  nmr_demo_project_choices <- reactive({
    req(rv_projects$data)
    projects <- rv_projects$data$Abbreviation
    projects <- projects[!is.na(projects) & nchar(projects) > 0]
    
    if (rv_demo$active) {
      n <- length(projects)
      letters_ext <- c(LETTERS, paste0(LETTERS, "2"))[seq_len(n)]
      anon <- paste0("PRJ_", letters_ext)
      # Return mapping: original -> anonymized
      setNames(anon, projects)
    } else {
      setNames(projects, projects)
    }
  })
  # ---- Update ALL project dropdowns on demo mode toggle ----
  observeEvent(rv_demo$active, {
    req(rv_projects$data)
    
    projects <- rv_projects$data$Abbreviation
    projects <- projects[!is.na(projects) & nchar(projects) > 0]
    
    if (rv_demo$active) {
      n <- length(projects)
      letters_ext <- c(LETTERS, paste0(LETTERS, "2"))[seq_len(n)]
      display_names <- paste0("PRJ_", letters_ext)
      choices <- setNames(projects, display_names)
    } else {
      choices <- setNames(projects, projects)
    }
    
    # Sample Submission project dropdown (actual ID: project_select)
    current <- input$project_select
    updateSelectizeInput(session, "project_select", choices = choices, selected = current)
    
    # Datenextraktion project dropdown (actual ID: xml_project_select)
    current <- input$xml_project_select
    choices_with_empty <- c("Please select..." = "", choices)
    updateSelectizeInput(session, "xml_project_select", choices = choices_with_empty, selected = current)
    
    # Archiv project filter (multiple = TRUE)
    current <- input$archive_project_filter
    updateSelectizeInput(session, "archive_project_filter", choices = choices, selected = current)
    
    # Archiv detail project
    current <- input$archive_plot_project
    updateSelectizeInput(session, "archive_plot_project", choices = choices, selected = current)
    
    # Box-Register: new box project dropdown
    current <- input$box_project
    updateSelectizeInput(session, "box_project", choices = choices, selected = current)
    
    # Box-Register: filter project dropdown (multiple = TRUE)
    current <- input$box_filter_project
    updateSelectizeInput(session, "box_filter_project", choices = choices, selected = current)
    
  })
  
  
  
  # Anonymize project names for demo mode
  anonymize_projects <- function(projects) {
    if (!rv_demo$active) return(projects)
    
    unique_projects <- unique(projects[!is.na(projects) & nchar(projects) > 0])
    mapping <- setNames(paste0("Project ", LETTERS[seq_along(unique_projects)]), unique_projects)
    
    result <- projects
    for (i in seq_along(unique_projects)) {
      result[result == unique_projects[i]] <- mapping[unique_projects[i]]
    }
    result
  }
  
   # HELPER: Parse dates from various formats
   parse_date_safe <- function(x) {
    if (is.null(x) || length(x) == 0) return(as.character(Sys.Date()))
    x <- as.character(x)

    # Try formats in order of likelihood
    formats <- c(
      "%d-%b-%Y %H:%M:%S",   # "25-Jun-2026 19:32:59" (XML format)
      "%d-%b-%Y",             # "25-Jun-2026"
      "%Y-%m-%d",             # "2026-07-15"
      "%d.%m.%Y",             # "15.07.2026"
      "%d/%m/%Y",             # "15/07/2026"
      "%Y-%m-%d %H:%M:%S"    # "2026-07-15 08:30:00"
    )

    for (fmt in formats) {
      result <- suppressWarnings(as.Date(x, format = fmt))
      if (!is.na(result)) return(format(result, "%Y-%m-%d"))
    }

    # Fallback
    return(as.character(Sys.Date()))
  }

  # Vectorized version
  parse_dates_safe <- function(x) {
    sapply(x, parse_date_safe, USE.NAMES = FALSE)
  }


# ---- Bruker NMR Spectrum Reader ----
read_bruker_1r <- function(pdata_dir) {
  file_1r <- file.path(pdata_dir, "1r")
  file_procs <- file.path(pdata_dir, "procs")
  if (!file.exists(file_1r)) stop("1r file not found: ", file_1r)
  if (!file.exists(file_procs)) stop("procs file not found: ", file_procs)
  procs_lines <- readLines(file_procs, warn = FALSE)
  get_param <- function(name) {
    pattern <- paste0("##", rawToChar(as.raw(36)), name, "=")
    idx <- which(startsWith(procs_lines, pattern))
    if (length(idx) == 0) return(NA)
    val <- sub(pattern, "", procs_lines[idx[1]], fixed = TRUE)
    suppressWarnings(as.numeric(trimws(val)))
  }
  SI <- get_param("SI")
  SF <- get_param("SF")
  OFFSET <- get_param("OFFSET")
  SW_p <- get_param("SW_p")
  NC_proc <- get_param("NC_proc")
  BYTORDP <- get_param("BYTORDP")
  if (is.na(SI) || is.na(SF) || is.na(OFFSET) || is.na(SW_p)) {
    stop("Could not read required parameters from procs file")
  }
  endian <- if (!is.na(BYTORDP) && BYTORDP == 1) "big" else "little"
  raw_data <- readBin(file_1r, what = "integer", n = SI, size = 4, endian = endian)
  if (!is.na(NC_proc)) {
    intensity <- raw_data * (2^NC_proc)
  } else {
    intensity <- as.numeric(raw_data)
  }
  sw_ppm <- SW_p / SF
  ppm <- seq(OFFSET, OFFSET - sw_ppm, length.out = SI)
  data.frame(ppm = ppm, intensity = intensity, stringsAsFactors = FALSE)
}

  # ---- EXPERIMENT TYPE DEFINITIONS ----
  # Loaded from experiment_types.csv so labs can define their own parameter
  # sets without editing code. One row per sample type x tube size.
  # The order of entries in the "experiments" column defines submission order.
  load_experiment_types <- function() {
    path <- file.path(APP_DIR, "experiment_types.csv")
    if (!file.exists(path)) path <- file.path(APP_DIR, "experiment_types.example.csv")

    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE),
                   error = function(e) NULL)

    required <- c("id", "label", "group", "tube_size", "solvent",
                  "archive_type", "experiments", "active")

    if (is.null(df) || !all(required %in% names(df))) {
      showNotification(
        paste0("experiment_types.csv is outdated or malformed - using built-in defaults. ",
               "Delete it and restart to regenerate from experiment_types.example.csv."),
        type = "warning", duration = NULL)
      warning("experiment_types.csv missing or malformed - using built-in defaults")
      return(data.frame(
        id = c("Plasma", "Plasma", "Urine", "Urine"),
        label = c("Plasma", "Plasma", "Urine", "Urine"),
        group = "IVDr Standard",
        tube_size = c("5mm", "3mm", "5mm", "3mm"),
        solvent = c("Plasma", "Plasma_3mm", "Urine", "Urine_3mm"),
        archive_type = c("Plasma", "Plasma", "Urine", "Urine"),
        experiments = c(
          "N PROF_PLASMA_NOESY;N PROF_PLASMA_JRES;N PROF_PLASMA_CPMG;N PROF_PLASMA_DIFF;N PROF_PLASMA_DAS",
          "N PROF_PLASMA_NOESY_3mm;N PROF_PLASMA_JRES_3mm;N PROF_PLASMA_CPMG_3mm;N PROF_PLASMA_DIFF_3mm;N PROF_PLASMA_DAS",
          "N PROF_URINE_NOESY;N PROF_URINE_JRES;N PROF_URINE_DAS",
          "N PROF_URINE_NOESY_3mm;N PROF_URINE_JRES_3mm;N PROF_URINE_DAS"
        ),
        active = TRUE,
        stringsAsFactors = FALSE
      ))
    }

    df$active <- as.logical(df$active)
    df[!is.na(df$active) & df$active, , drop = FALSE]
  }

  EXP_TYPES <- load_experiment_types()

  # Populate the sample type dropdown, grouped by the "group" column
  observe({
    uniq <- EXP_TYPES[!duplicated(EXP_TYPES$id), c("id", "label", "group")]
    grouped <- split(setNames(uniq$id, uniq$label), uniq$group)
    updateSelectInput(session, "sub_solvent",
                      choices = grouped,
                      selected = if ("Plasma" %in% uniq$id) "Plasma" else uniq$id[1])
  })

  # Look up one row by sample type and tube size
  get_exp_def <- function(type_id, size) {
    hit <- EXP_TYPES[EXP_TYPES$id == type_id & EXP_TYPES$tube_size == size, , drop = FALSE]
    if (nrow(hit) == 0) {
      hit <- EXP_TYPES[EXP_TYPES$id == type_id, , drop = FALSE]
    }
    if (nrow(hit) == 0) return(NULL)
    hit[1, , drop = FALSE]
  }

  # Split the semicolon list, preserving order
  get_exp_list <- function(def) {
    if (is.null(def)) return(character(0))
    out <- trimws(strsplit(as.character(def$experiments), ";", fixed = TRUE)[[1]])
    out[nzchar(out)]
  }

find_bruker_experiments <- function(sample_dir) {
  if (!dir.exists(sample_dir)) return(data.frame())
  all_1r <- list.files(sample_dir, pattern = "^1r$", recursive = TRUE, full.names = TRUE)
  if (length(all_1r) == 0) return(data.frame())
  results <- lapply(all_1r, function(f) {
    pdata_dir <- dirname(f)
    exp_dir <- dirname(dirname(pdata_dir))
    title_file <- file.path(pdata_dir, "title")
    title <- if (file.exists(title_file)) {
      trimws(paste(readLines(title_file, warn = FALSE), collapse = " "))
    } else ""
    acqus_file <- file.path(exp_dir, "acqus")
    pulse_prog <- ""
    if (file.exists(acqus_file)) {
      acqus_lines <- readLines(acqus_file, warn = FALSE)
      pp_idx <- which(startsWith(acqus_lines, paste0("##", rawToChar(as.raw(36)), "PULPROG=")))
      if (length(pp_idx) > 0) {
        pp_val <- sub(paste0("##", rawToChar(as.raw(36)), "PULPROG= *<"), "", acqus_lines[pp_idx[1]])
        pulse_prog <- sub(">.*", "", pp_val)
      }
    }
    exp_num <- basename(exp_dir)
    proc_num <- basename(pdata_dir)
    data.frame(Experiment = exp_num, ProcNo = proc_num,
               PulseProgram = pulse_prog,
               Title = if (nchar(title) > 0) title else "",
               Path = pdata_dir, stringsAsFactors = FALSE)
  })
  do.call(rbind, results)
}

# ---- Bruker IVDr Plasma Metabolite Regions ----
# Chemical shift regions for B.I.QUANT-PS metabolites (ppm ranges)
get_ivdr_metabolite_regions <- function() {

  data.frame(
    Metabolite = c(
      "Leucine", "Isoleucine", "Valine (g)", "Valine (g2)",
      "3-Hydroxybutyrate", "Lactate", "Alanine",
      "Acetate", "Acetone", "Acetoacetate",
      "Glutamine", "Glutamate", "Pyruvate",
      "Citrate (a)", "Citrate (b)", "Dimethylsulfone",
      "Creatinine", "Creatine",
      "Histidine", "Glucose (alpha-H1)", "Glucose (beta-H1)",
      "Tyrosine (a)", "Tyrosine (b)",
      "Phenylalanine",
      "Formate", "Mannose",
      "Glycine", "Threonine",
      "Ethanol (CH3)", "2-Hydroxybutyrate",
      "Methanol", "Dimethylamine",
      "TMAO", "Betaine",
      "myo-Inositol", "Glycerol"
    ),
    ppm_center = c(
      0.962, 0.940, 1.000, 1.050,
      1.208, 1.336, 1.490,
      1.928, 2.240, 2.290,
      2.460, 2.360, 2.380,
      2.540, 2.680, 3.160,
      3.055, 3.040,
      7.080, 5.240, 4.650,
      6.910, 7.200,
      7.380,
      8.460, 5.200,
      3.570, 1.340,
      1.185, 0.910,
      3.370, 2.730,
      3.280, 3.275,
      3.580, 3.640
    ),
    Multiplicity = c(
      "t", "d", "d", "d",
      "d", "d", "d",
      "s", "s", "s",
      "m", "m", "s",
      "d", "d", "s",
      "s", "s",
      "s", "d", "d",
      "d", "d",
      "m",
      "s", "d",
      "s", "d",
      "t", "t",
      "s", "s",
      "s", "s",
      "m", "m"
    ),
    J_Hz = c(
      6.7, 7.0, 7.1, 7.1,
      6.3, 6.9, 7.2,
      0, 0, 0,
      7.5, 7.5, 0,
      15.1, 15.1, 0,
      0, 0,
      0, 3.8, 8.0,
      8.5, 8.5,
      7.5,
      0, 1.7,
      0, 6.6,
      7.0, 7.5,
      0, 0,
      0, 0,
      9.0, 6.0
    ),
    Category = c(
      "BCAA", "BCAA", "BCAA", "BCAA",
      "Ketone", "Energy", "Amino Acid",
      "Short Chain FA", "Ketone", "Ketone",
      "Amino Acid", "Amino Acid", "Energy",
      "Energy", "Energy", "Other",
      "Other", "Other",
      "Amino Acid", "Sugar", "Sugar",
      "Amino Acid", "Amino Acid",
      "Amino Acid",
      "Other", "Sugar",
      "Amino Acid", "Amino Acid",
      "Other", "Other",
      "Other", "Other",
      "Other", "Other",
      "Sugar", "Other"
    ),
    stringsAsFactors = FALSE
  ) -> df
  
  # Calculate ppm ranges from center, multiplicity, and J-coupling at 600 MHz
  freq <- 600  # MHz
  linewidth_pad <- 0.005  # ppm padding for linewidth in plasma (broader than pure compounds)
  
  df$ppm_min <- sapply(seq_len(nrow(df)), function(i) {
    center <- df$ppm_center[i]
    J <- df$J_Hz[i]
    mult <- df$Multiplicity[i]
    J_ppm <- J / freq
    spread <- switch(mult,
      "s" = 0.006,
      "d" = J_ppm / 2 + 0.004,
      "t" = J_ppm + 0.004,
      "q" = 1.5 * J_ppm + 0.004,
      "dd" = J_ppm + 0.006,
      "m" = 0.025,
      "br" = 0.045,
      0.012
    )
    center - spread - linewidth_pad
  })
  
  df$ppm_max <- sapply(seq_len(nrow(df)), function(i) {
    center <- df$ppm_center[i]
    J <- df$J_Hz[i]
    mult <- df$Multiplicity[i]
    J_ppm <- J / freq
    spread <- switch(mult,
      "s" = 0.006,
      "d" = J_ppm / 2 + 0.004,
      "t" = J_ppm + 0.004,
      "q" = 1.5 * J_ppm + 0.004,
      "dd" = J_ppm + 0.006,
      "m" = 0.025,
      "br" = 0.045,
      0.012
    )
    center + spread + linewidth_pad
  })
  
  df
}

# Color palette for metabolite categories
get_metabolite_colors <- function() {
  c(
    "BCAA" = "rgba(231, 76, 60, 0.15)",
    "Amino Acid" = "rgba(52, 152, 219, 0.15)",
    "Ketone" = "rgba(155, 89, 182, 0.15)",
    "Energy" = "rgba(46, 204, 113, 0.15)",
    "Sugar" = "rgba(241, 196, 15, 0.15)",
    "Short Chain FA" = "rgba(230, 126, 34, 0.15)",
    "Protein/Lipid" = "rgba(149, 165, 166, 0.15)",
    "Other" = "rgba(127, 140, 141, 0.10)"
  )
}

get_metabolite_border_colors <- function() {
  c(
    "BCAA" = "rgba(231, 76, 60, 0.6)",
    "Amino Acid" = "rgba(52, 152, 219, 0.6)",
    "Ketone" = "rgba(155, 89, 182, 0.6)",
    "Energy" = "rgba(46, 204, 113, 0.6)",
    "Sugar" = "rgba(241, 196, 15, 0.6)",
    "Short Chain FA" = "rgba(230, 126, 34, 0.6)",
    "Protein/Lipid" = "rgba(149, 165, 166, 0.6)",
    "Other" = "rgba(127, 140, 141, 0.4)"
  )
}



  # REACTIVE VALUES------------------

  rv <- reactiveValues(
    folder_path = NULL,
    files_metas = NULL,
    files_lipids = NULL,
    files_qc = NULL,
    processed = FALSE,
    xml_m = NULL,
    xml_l = NULL,
    xml_qc = NULL,
    status = "Ready. Please select a folder."
  )

  rv_sub <- reactiveValues(
    submission_table = NULL,
    sub_status = "Ready. Please enter samples and select parameters.",
    uploaded_extra_cols = NULL
  )
  
  rv_nmr_transfer <- reactiveValues(
    samples = NULL,
    project = NULL,
    source = NULL,
    triggered = FALSE
  )
  

  rv_qc <- reactiveValues(
    detail = NULL,
    summary = NULL,
    overall = NULL
  )

    # BACKUP SYSTEM--------------
  backup_dir <- file.path(APP_DIR, "backups")
  if (!dir.exists(backup_dir)) dir.create(backup_dir)

  create_backup <- function(type = "auto") {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    if (!is.null(rv_archive$data) && nrow(rv_archive$data) > 0) {
      archive_backup_file <- file.path(backup_dir,
                                       paste0("archive_", timestamp, "_", type, ".csv"))
      write.csv(rv_archive$data, archive_backup_file, row.names = FALSE)
    }

    if (!is.null(rv_projects$data) && nrow(rv_projects$data) > 0) {
      projects_backup_file <- file.path(backup_dir,
                                        paste0("projects_", timestamp, "_", type, ".xlsx"))
      wb <- createWorkbook()
      addWorksheet(wb, "Projects")
      writeData(wb, "Projects", rv_projects$data)
      saveWorkbook(wb, projects_backup_file, overwrite = TRUE)
    }

    if (!is.null(rv_boxes$data) && nrow(rv_boxes$data) > 0) {
      boxes_backup_file <- file.path(backup_dir,
                                     paste0("boxes_", timestamp, "_", type, ".csv"))
      write.csv(rv_boxes$data, boxes_backup_file, row.names = FALSE)
    }

    # Clean up old backups (keep last 30 sets = 90 files)
    all_backups <- list.files(backup_dir, full.names = TRUE)
    if (length(all_backups) > 90) {
      file_info <- file.info(all_backups)
      file_info <- file_info[order(file_info$mtime), ]
      to_delete <- rownames(file_info)[1:(length(all_backups) - 90)]
      file.remove(to_delete)
    }

    return(timestamp)
  }


  # LOAD PERSISTENT DATA----------------


  # Projects - check CSV first (from setup), then Excel (legacy)
  rv_projects <- reactiveValues(
    data = tryCatch({
      if (file.exists(projects_csv_path)) {
        df <- as.data.frame(data.table::fread(projects_csv_path))
        message("Loaded projects from CSV: ", nrow(df), " rows from ", projects_csv_path)
        df
      } else if (file.exists(projects_xlsx_path)) {
        df <- openxlsx::read.xlsx(projects_xlsx_path)
        message("Loaded projects from Excel: ", nrow(df), " rows from ", projects_xlsx_path)
        expected_cols <- c("Title", "Abbreviation", "Name", "Email",
                           "Group / AG", "Contact Clinic/Institute",
                           "Type of Sample", "Number of Samples", "Boxes", "Measured")
        for (col in expected_cols) {
          if (!col %in% names(df)) df[[col]] <- NA
        }
        df
      } else {
        message("No projects file found, creating empty from config")
        cols <- project_columns
        df <- data.frame(matrix(ncol = length(cols), nrow = 0))
        names(df) <- cols
        df
      }
    }, error = function(e) {
      message("Error loading projects: ", e$message)
      data.frame(Title = character(), Abbreviation = character(), stringsAsFactors = FALSE)
    }),
    selected_row = NULL
  )
  
  # Archive
  archive_path <- file.path(APP_DIR, "archive.csv")
  rv_archive <- reactiveValues(
    data = tryCatch({
      if (file.exists(archive_path)) {
        df <- as.data.frame(fread(archive_path))
        if ("Date" %in% names(df)) df$Date <- parse_dates_safe(df$Date)
        # Align with config.json - columns added via Settings may not be in the CSV yet
        df <- reconcile_cols(df, archive_columns)
        if (!"Box" %in% names(df)) df$Box <- NA_character_
        if (!"Box_Code" %in% names(df)) df$Box_Code <- NA_character_
        message("Loaded archive: ", nrow(df), " rows from ", archive_path)
        df
      } else {
        message("No archive.csv found, creating empty from config")
        cols <- archive_columns
        df <- data.frame(matrix(ncol = length(cols), nrow = 0))
        names(df) <- cols
        df
      }
    }, error = function(e) {
      message("Error loading archive: ", e$message)
      data.frame(Date = character(), Name = character(),
                 Project = character(), stringsAsFactors = FALSE)
    })
  )
  
  # Box Registry
  box_registry_path <- file.path(APP_DIR, "box_registry.csv")
  rv_boxes <- reactiveValues(
    data = tryCatch({
      df <- as.data.frame(fread(box_registry_path))
      if (!"Measured_Date" %in% names(df)) df$Measured_Date <- NA_character_
      df
    }, error = function(e) {
      data.frame(
        Box_Code = character(), Project = character(),
        Box_Name = character(), Sample_Count = integer(),
        Received = character(), Status = character(),
        Measured_Date = character(), Notes = character(),
        stringsAsFactors = FALSE
      )
    }),
    selected_row = NULL
  )

  # Progress bar for selected project
  output$project_progress_bar <- renderUI({
    req(rv_projects$selected_row)
    df <- rv_projects$data
    abbr <- df$Abbreviation[rv_projects$selected_row]

    if (is.null(rv_boxes$data) || nrow(rv_boxes$data) == 0) {
      return(tags$div(class = "text-muted", "No boxes registered."))
    }

    project_boxes <- rv_boxes$data[rv_boxes$data$Project == abbr, ]
    if (nrow(project_boxes) == 0) {
      return(tags$div(class = "text-muted", "No boxes for this project."))
    }

    total <- nrow(project_boxes)
    measured <- sum(project_boxes$Status %in% c("Measured", "Completed"), na.rm = TRUE)
    pct <- round(measured / total * 100, 0)

    color <- if (pct >= 100) "bg-success" else if (pct >= 50) "bg-warning" else "bg-info"

    tags$div(
      tags$div(
        style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
        tags$small(tags$strong(paste0(measured, " / ", total, " Sample-boxes measured"))),
        tags$small(tags$strong(paste0(pct, "%")))
      ),
      tags$div(
        class = "progress", style = "height: 22px; border-radius: 6px;",
        tags$div(
          class = paste("progress-bar", color),
          role = "progressbar",
          style = paste0("width: ", pct, "%;"),
          `aria-valuenow` = pct,
          `aria-valuemin` = "0",
          `aria-valuemax` = "100",
          paste0(pct, "%")
        )
      )
    )
  })

  # Text showing progress
  output$project_box_progress_text <- renderText({
    req(rv_projects$selected_row)
    df <- rv_projects$data
    abbr <- df$Abbreviation[rv_projects$selected_row]

    if (is.null(rv_boxes$data) || nrow(rv_boxes$data) == 0) return("")

    project_boxes <- rv_boxes$data[rv_boxes$data$Project == abbr, ]
    if (nrow(project_boxes) == 0) return("")

    total <- nrow(project_boxes)
    measured <- sum(project_boxes$Status %in% c("Measured", "Completed"), na.rm = TRUE)
    paste0(measured, "/", total)
  })

  # Table of boxes for selected project
  output$project_boxes_table <- renderDT({
    req(rv_projects$selected_row)
    df <- rv_projects$data
    abbr <- df$Abbreviation[rv_projects$selected_row]

    if (is.null(rv_boxes$data) || nrow(rv_boxes$data) == 0) {
      return(datatable(data.frame(Info = "No boxes registered"), rownames = FALSE))
    }

    project_boxes <- rv_boxes$data[rv_boxes$data$Project == abbr, ]
    if (nrow(project_boxes) == 0) {
      return(datatable(data.frame(Info = "No boxes for this project"), rownames = FALSE))
    }

    # Select relevant columns
    display_cols <- intersect(
      c("Box_Code", "Box_Name", "Sample_Count", "Received", "Status", "Measured_Date", "Notes"),
      names(project_boxes)
    )
    project_boxes <- project_boxes[, display_cols, drop = FALSE]

    datatable(project_boxes, selection = "none", class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 10, dom = "t"),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("Received", "In Preparation", "Measured", "Completed"),
                    c("#d6eaf8", "#fdebd0", "#d5f5e3", "#eaecee"),
                    default = "#ffffff"
                  ))
  })

  # Track selected project row
  observeEvent(input$projects_table_rows_selected, {
    rv_projects$selected_row <- input$projects_table_rows_selected
  })

    # XML PARSER FUNCTIONS (OPTIMIZED)------------------
    blood_meta_parser <- function(file_paths) {
    results <- future_lapply(file_paths, function(fp) {
      tryCatch({
        xml_data <- read_xml(fp)
        sample_node <- xml_find_first(xml_data, ".//SAMPLE")
        sample_name <- sub("_e.*", "", xml_attr(sample_node, "name"))

        # Extract sample name from path if xml attribute is empty
        if (is.na(sample_name) || nchar(sample_name) == 0) {
          parts <- unlist(strsplit(fp, "[/\\\\]"))
          nmr_idx <- which(parts == "nmr")
          if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
        }

        features <- xml_find_all(xml_data, ".//FEATURE")
        if (length(features) == 0) return(NULL)

        # Vectorized extraction - much faster than looping
        data.table(
          ID = sample_name,
          Parameter = xml_attr(features, "name"),
          Value = as.numeric(xml_attr(features, "value")),
          Unit = xml_attr(features, "unit")
        )
      }, error = function(e) NULL)
    })

    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) return(NULL)
    rbindlist(results, use.names = TRUE, fill = TRUE)
  }

  blood_lipid_parser <- function(file_paths) {
    results <- future_lapply(file_paths, function(fp) {
      tryCatch({
        xml_data <- read_xml(fp)
        sample_node <- xml_find_first(xml_data, ".//SAMPLE")
        sample_name <- sub("_e.*", "", xml_attr(sample_node, "name"))

        if (is.na(sample_name) || nchar(sample_name) == 0) {
          parts <- unlist(strsplit(fp, "[/\\\\]"))
          nmr_idx <- which(parts == "nmr")
          if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
        }

        features <- xml_find_all(xml_data, ".//FEATURE")
        if (length(features) == 0) return(NULL)

        data.table(
          ID = sample_name,
          Parameter = xml_attr(features, "name"),
          Value = as.numeric(xml_attr(features, "value")),
          Unit = xml_attr(features, "unit")
        )
      }, error = function(e) NULL)
    })

    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) return(NULL)
    rbindlist(results, use.names = TRUE, fill = TRUE)
  }

  blood_qc_parser <- function(file_paths) {
    results <- future_lapply(file_paths, function(fp) {
      tryCatch({
        xml_data <- read_xml(fp)
        sample_node <- xml_find_first(xml_data, ".//SAMPLE")
        sample_name <- sub("_e.*", "", xml_attr(sample_node, "name"))

        if (is.na(sample_name) || nchar(sample_name) == 0) {
          parts <- unlist(strsplit(fp, "[/\\\\]"))
          nmr_idx <- which(parts == "nmr")
          if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
        }

        features <- xml_find_all(xml_data, ".//FEATURE")
        if (length(features) == 0) return(NULL)

        data.table(
          ID = sample_name,
          Parameter = xml_attr(features, "name"),
          Value = as.numeric(xml_attr(features, "value")),
          Unit = xml_attr(features, "unit")
        )
      }, error = function(e) NULL)
    })

    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) return(NULL)
    rbindlist(results, use.names = TRUE, fill = TRUE)
  }


  # QC CHECKER FUNCTION (OPTIMIZED)--------------------

  blood_qc_checker <- function(file_paths) {
    results <- future_lapply(file_paths, function(fp) {
      tryCatch({
        qc_xml <- read_xml(fp)

        sample_node <- xml_find_first(qc_xml, ".//SAMPLE")
        sample_name <- sub("_e.*", "", xml_attr(sample_node, "name"))

        if (is.na(sample_name) || nchar(sample_name) == 0) {
          parts <- unlist(strsplit(fp, "[/\\\\]"))
          nmr_idx <- which(parts == "nmr")
          if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
        }

        groups <- xml_find_all(qc_xml, ".//GROUP")
        if (length(groups) == 0) return(NULL)

        qc_rows <- vector("list", 100)
        row_count <- 0

        for (group in groups) {
          group_name <- xml_attr(group, "name")

          category <- if (grepl("matrix|integrity", group_name, ignore.case = TRUE)) {
            "Matrix Integrity"
          } else if (grepl("preparation|sample prep", group_name, ignore.case = TRUE)) {
            "Sample Preparation"
          } else if (grepl("spectral|spectrum|NMR", group_name, ignore.case = TRUE)) {
            "NMR Spectral Quality"
          } else {
            group_name
          }

          params <- xml_find_all(group, ".//PARAMETER")
          if (length(params) == 0) next

          for (param in params) {
            param_name <- xml_attr(param, "name")
            value_node <- xml_child(param, 1)
            if (is.na(value_node)) next

            param_value <- xml_attr(value_node, "value")
            param_unit <- xml_attr(value_node, "unit")
            param_status <- xml_attr(value_node, "status")
            lower_limit <- xml_attr(value_node, "lower")
            upper_limit <- xml_attr(value_node, "upper")
            limit <- xml_attr(value_node, "limit")

            # Determine pass/fail
            if (!is.na(param_status) && nchar(param_status) > 0) {
              passed <- grepl("pass|ok|true", param_status, ignore.case = TRUE)
            } else if (!is.na(param_value)) {
              val <- suppressWarnings(as.numeric(param_value))
              if (!is.na(val)) {
                lower <- suppressWarnings(as.numeric(lower_limit))
                upper <- suppressWarnings(as.numeric(upper_limit))
                lim <- suppressWarnings(as.numeric(limit))
                if (!is.na(lower) && !is.na(upper)) {
                  passed <- val >= lower & val <= upper
                } else if (!is.na(lim)) {
                  passed <- val <= lim
                } else {
                  passed <- NA
                }
              } else {
                passed <- NA
              }
            } else {
              passed <- NA
            }

            row_count <- row_count + 1
            qc_rows[[row_count]] <- list(
              ID = sample_name,
              Category = category,
              Parameter = param_name,
              Value = ifelse(is.na(param_value), "", param_value),
              Unit = ifelse(is.na(param_unit), "", param_unit),
              Lower_Limit = ifelse(is.na(lower_limit), "", lower_limit),
              Upper_Limit = ifelse(is.na(upper_limit), "", upper_limit),
              Limit = ifelse(is.na(limit), "", limit),
              Status = ifelse(is.na(param_status), "", param_status),
              Passed = ifelse(is.na(passed), "N/A", ifelse(passed, "PASS", "FAIL"))
            )
          }
        }

        if (row_count > 0) {
          rbindlist(qc_rows[1:row_count])
        } else {
          NULL
        }
      }, error = function(e) NULL)
    })

    results <- results[!sapply(results, is.null)]
    if (length(results) == 0) return(NULL)
    as.data.frame(rbindlist(results, use.names = TRUE, fill = TRUE))
  }

  # XML PARSER: FOLDER SELECTION & FILE SCANNING---------------
  # FOLDER SELECTION----------------
  # Try system dialog (may not work in all environments)
  observeEvent(input$select_folder_btn, {

    folder <- NA

    # Method 1: rstudioapi (works in RStudio when choose.dir is broken)
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      folder <- tryCatch({
        rstudioapi::selectDirectory(
          caption = "Select Folder",
          label = "Ausw\u00E4hlen",
          path = getwd()
        )
      }, error = function(e) NA)
    }

    # Method 2: choose.dir fallback
    if (is.na(folder)) {
      folder <- tryCatch({
        utils::choose.dir(default = getwd(), caption = "Select Folder")
      }, error = function(e) NA)
    }

    # Method 3: tcltk fallback
    if (is.na(folder)) {
      folder <- tryCatch({
        tcltk::tk_choose.dir(default = getwd(), caption = "Select Folder")
      }, error = function(e) NA)
    }

    if (!is.na(folder) && nchar(folder) > 0) {
      updateTextInput(session, "manual_folder_path", value = folder)
      scan_folder(folder)
    } else {
      showNotification(
        "Folder dialog not available. Please enter path manually und click 'Use Path'.",
        type = "warning", duration = 5
      )
    }
  })

  # Manual path entry
  observeEvent(input$use_path_btn, {
    folder <- trimws(input$manual_folder_path)
    if (nchar(folder) == 0) {
      showNotification("Please enter a path.", type = "warning", duration = 3)
      return()
    }
    if (!dir.exists(folder)) {
      showNotification("Folder does not exist!", type = "error", duration = 4)
      return()
    }
    rv$folder_path <- folder
    rv$processed <- FALSE
    rv$files_metas <- NULL
    rv$files_lipids <- NULL
    rv$files_qc <- NULL
    rv$sample_names <- NULL
    shinyjs::hide("download_div")
    showNotification(paste0("Path set: ", folder), type = "message", duration = 3)
  })

  # Also trigger on Enter key in the path input
  observeEvent(input$manual_folder_path, {
    # Do nothing here - wait for button click
  }, ignoreInit = TRUE)


  # Scan button handler
  observeEvent(input$scan_btn, {
    folder <- rv$folder_path
    if (is.null(folder) || !dir.exists(folder)) {
      showNotification("Please set a valid folder path first.", type = "warning", duration = 3)
      return()
    }
    scan_folder(folder)
  })

  # Shared scan function
  scan_folder <- function(folder) {
    rv$folder_path <- folder
    rv$processed <- FALSE
    shinyjs::hide("download_div")

    # Show searching notification
    id <- showNotification(
      tags$div(
        style = "display: flex; align-items: center; gap: 12px;",
        tags$div(
          class = "spinner-border spinner-border-sm text-primary",
          role = "status"
        ),
        tags$span("Looking for XML files...")
      ),
      duration = NULL,
      closeButton = FALSE,
      type = "message"
    )

    # Scan for XML files
    all_xml <- list.files(folder, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)

    if (input$sample_type == "blood") {
      rv$files_metas <- all_xml[grepl("plasma_quant_report", all_xml, ignore.case = TRUE)]
      rv$files_lipids <- all_xml[grepl("plasma_lipo_report", all_xml, ignore.case = TRUE)]
      rv$files_qc <- all_xml[grepl("plasma_qc_report", all_xml, ignore.case = TRUE)]
    } else {
      rv$files_metas <- all_xml[grepl("urine_quant_report", all_xml, ignore.case = TRUE)]
      rv$files_lipids <- NULL
      rv$files_qc <- all_xml[grepl("urine_qc_report", all_xml, ignore.case = TRUE)]
    }

    # Remove notification
    removeNotification(id)

    # Extract sample names
    sample_names <- unique(sapply(all_xml, function(fp) {
      parts <- unlist(strsplit(fp, "[/\\\\]"))
      nmr_idx <- which(parts == "nmr")
      if (length(nmr_idx) > 0 && nmr_idx[1] < length(parts)) {
        parts[nmr_idx[1] + 1]
      } else {
        NA
      }
    }))
    sample_names <- sample_names[!is.na(sample_names)]
    rv$sample_names <- sample_names

    # Result notification
    n_total <- length(rv$files_metas) + length(rv$files_lipids) + length(rv$files_qc)
    if (n_total > 0) {
      showNotification(
        tags$div(
          icon("circle-check", style = "color: #18bc9c; margin-right: 8px;"),
          tags$strong(paste0(n_total, " XML files found!")),
          tags$br(),
          tags$small(paste0(length(sample_names), " samples detected"))
        ),
        type = "message", duration = 4
      )
    } else {
      showNotification(
        tags$div(
          icon("triangle-exclamation", style = "color: #e74c3c; margin-right: 8px;"),
          tags$strong("No XML files found!"),
          tags$br(),
          tags$small("Check folder and sample type.")
        ),
        type = "error", duration = 6
      )
    }

    rv$status <- paste0(
      "Folder: ", folder, "\n",
      "XML files found: ", length(all_xml), "\n",
      "  Metabolites (quant): ", length(rv$files_metas), "\n",
      "  Lipids (lipo): ", length(rv$files_lipids), "\n",
      "  QC: ", length(rv$files_qc), "\n",
      "Samples detected: ", length(sample_names), "\n",
      ifelse(length(sample_names) > 0,
             paste0("  ", paste(head(sample_names, 10), collapse = ", "),
                    ifelse(length(sample_names) > 10, ", ...", "")),
             "")
    )
  }

  output$folder_path_display <- renderText({
    if (is.null(rv$folder_path)) "No folder selected" else rv$folder_path
  })

  output$scan_status <- renderText({
    if (is.null(rv$folder_path)) return("")
    paste0("M:", length(rv$files_metas), " L:", length(rv$files_lipids), " Q:", length(rv$files_qc))
  })

  # Scan results panel in sidebar
  output$scan_results_ui <- renderUI({
    n_m <- length(rv$files_metas)
    n_l <- length(rv$files_lipids)
    n_q <- length(rv$files_qc)
    n_total <- n_m + n_l + n_q
    n_samples <- length(rv$sample_names)
    
    if (is.null(rv$folder_path)) {
      return(tags$div(
        style = "text-align: center; color: #95a5a6; padding: 8px; font-size: 0.85em;",
        icon("info-circle"), " Set a folder path and click Scan"
      ))
    }
    
    if (n_total == 0 && !is.null(rv$folder_path)) {
      return(tags$div(
        style = "text-align: center; color: #e74c3c; padding: 8px; font-size: 0.85em;",
        icon("triangle-exclamation"), " No XML files found. Click Scan to search."
      ))
    }
    
    tags$div(
      style = "background: #f0faf5; border: 1px solid #d4edda; border-radius: 6px; padding: 10px; font-size: 0.85em;",
      tags$div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 6px;",
        icon("circle-check", style = "color: #18bc9c;"),
        tags$strong(paste0(n_total, " XML files found")),
        tags$span(class = "badge bg-primary", style = "font-size: 0.75em;",
                  paste0(n_samples, " samples"))
      ),
      tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 8px;",
        if (n_m > 0) tags$span(
          style = "display: flex; align-items: center; gap: 4px;",
          icon("flask", style = "color: #2ecc71; font-size: 0.8em;"),
          paste0("Metabolites: ", n_m)
        ),
        if (n_l > 0) tags$span(
          style = "display: flex; align-items: center; gap: 4px;",
          icon("droplet", style = "color: #f39c12; font-size: 0.8em;"),
          paste0("Lipids: ", n_l)
        ),
        if (n_q > 0) tags$span(
          style = "display: flex; align-items: center; gap: 4px;",
          icon("clipboard-check", style = "color: #3498db; font-size: 0.8em;"),
          paste0("QC: ", n_q)
        )
      )
    )
  })


  # ---- XML Extraction Status Panel ----
  output$xml_status_badge <- renderUI({
    n_m <- length(rv$files_metas)
    n_l <- length(rv$files_lipids)
    n_q <- length(rv$files_qc)
    n_total <- n_m + n_l + n_q
    
    if (n_total == 0) {
      tags$span(class = "badge bg-secondary", style = "font-size: 0.85em;", "No files scanned")
    } else if (rv$processed) {
      tags$span(class = "badge bg-success", style = "font-size: 0.85em;",
                icon("circle-check", style = "margin-right: 4px;"), "Extraction complete")
    } else {
      tags$span(class = "badge bg-info", style = "font-size: 0.85em;",
                icon("clock", style = "margin-right: 4px;"), paste0(n_total, " files ready"))
    }
  })
  
  output$xml_status_panel <- renderUI({
    n_m <- length(rv$files_metas)
    n_l <- length(rv$files_lipids)
    n_q <- length(rv$files_qc)
    n_total <- n_m + n_l + n_q
    samples <- rv$sample_names
    n_samples <- length(samples)
    
    if (n_total == 0 && !rv$processed) {
      return(tags$div(
        style = "text-align: center; padding: 30px; color: #95a5a6;",
        icon("folder-open", style = "font-size: 2.5em; margin-bottom: 10px;"),
        tags$br(),
        tags$strong("No folder scanned yet"),
        tags$br(),
        tags$small("Select a folder and click Scan to find XML files.")
      ))
    }
    
    # File type breakdown
    file_cards <- layout_column_wrap(
      width = 1/4,
      style = "margin-bottom: 12px;",
      value_box(
        title = "Total XML Files",
        value = n_total,
        showcase = icon("file-code"),
        theme = "primary",
        height = "100px"
      ),
      value_box(
        title = "Metabolite Reports",
        value = n_m,
        showcase = icon("flask"),
        theme = if (n_m > 0) "success" else "secondary",
        height = "100px"
      ),
      value_box(
        title = "Lipoprotein Reports",
        value = n_l,
        showcase = icon("droplet"),
        theme = if (n_l > 0) "info" else "secondary",
        height = "100px"
      ),
      value_box(
        title = "QC Reports",
        value = n_q,
        showcase = icon("clipboard-check"),
        theme = if (n_q > 0) "warning" else "secondary",
        height = "100px"
      )
    )
    
    # Sample list with missing report detection
    sample_section <- NULL
    if (n_samples > 0) {
      # Detect which reports each sample has
      get_sample_from_path <- function(fp) {
        parts <- unlist(strsplit(fp, "[/\\]"))
        nmr_idx <- which(parts == "nmr")
        if (length(nmr_idx) > 0 && nmr_idx[1] < length(parts)) parts[nmr_idx[1] + 1]
        else NA
      }
      
      meta_samples <- unique(sapply(rv$files_metas, get_sample_from_path))
      lipo_samples <- unique(sapply(rv$files_lipids, get_sample_from_path))
      qc_samples <- unique(sapply(rv$files_qc, get_sample_from_path))
      
      is_blood <- input$sample_type == "blood"
      
      sample_rows <- lapply(samples, function(s) {
        has_meta <- s %in% meta_samples
        has_lipo <- if (is_blood) s %in% lipo_samples else TRUE
        has_qc <- s %in% qc_samples
        
        missing <- character(0)
        if (!has_meta) missing <- c(missing, "Metabolites")
        if (is_blood && !has_lipo) missing <- c(missing, "Lipoproteins")
        if (!has_qc) missing <- c(missing, "QC")
        
        status_icon <- if (length(missing) == 0) {
          icon("circle-check", style = "color: #18bc9c;")
        } else {
          icon("triangle-exclamation", style = "color: #e67e22;")
        }
        
        tags$tr(
          tags$td(style = "padding: 4px 8px; font-size: 0.85em; font-weight: 500;", s),
          tags$td(style = "padding: 4px 8px; text-align: center;",
                  if (has_meta) icon("check", style = "color: #18bc9c;") 
                  else icon("xmark", style = "color: #e74c3c;")),
          if (is_blood) tags$td(style = "padding: 4px 8px; text-align: center;",
                  if (has_lipo) icon("check", style = "color: #18bc9c;") 
                  else icon("xmark", style = "color: #e74c3c;")),
          tags$td(style = "padding: 4px 8px; text-align: center;",
                  if (has_qc) icon("check", style = "color: #18bc9c;") 
                  else icon("xmark", style = "color: #e74c3c;")),
          tags$td(style = "padding: 4px 8px;",
                  if (length(missing) > 0) {
                    tags$span(class = "badge bg-warning", style = "font-size: 0.75em;",
                              paste(missing, collapse = ", "))
                  } else {
                    tags$span(class = "badge bg-success", style = "font-size: 0.75em;", "Complete")
                  })
        )
      })
      
      # Count complete vs incomplete
      complete_count <- sum(sapply(samples, function(s) {
        has_meta <- s %in% meta_samples
        has_lipo <- if (is_blood) s %in% lipo_samples else TRUE
        has_qc <- s %in% qc_samples
        has_meta && has_lipo && has_qc
      }))
      incomplete_count <- n_samples - complete_count
      
      header_cols <- list(
        tags$th(style = "padding: 4px 8px; font-size: 0.8em;", "Sample"),
        tags$th(style = "padding: 4px 8px; text-align: center; font-size: 0.8em;", "Metab."),
        if (is_blood) tags$th(style = "padding: 4px 8px; text-align: center; font-size: 0.8em;", "Lipo."),
        tags$th(style = "padding: 4px 8px; text-align: center; font-size: 0.8em;", "QC"),
        tags$th(style = "padding: 4px 8px; font-size: 0.8em;", "Status")
      )
      
      sample_section <- tags$div(
        style = "margin-top: 8px;",
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px;",
          tags$h6(style = "margin: 0;", icon("users"), 
                  paste0(" ", n_samples, " Samples Detected")),
          tags$div(
            style = "display: flex; gap: 6px;",
            tags$span(class = "badge bg-success", paste0(complete_count, " complete")),
            if (incomplete_count > 0) tags$span(class = "badge bg-warning", paste0(incomplete_count, " incomplete"))
          )
        ),
        tags$div(
          style = "max-height: 300px; overflow-y: auto; border: 1px solid #dee2e6; border-radius: 6px;",
          tags$table(
            class = "table table-sm table-hover",
            style = "margin: 0; font-size: 0.85em;",
            tags$thead(
              style = "position: sticky; top: 0; background: #f8f9fa;",
              tags$tr(header_cols)
            ),
            tags$tbody(sample_rows)
          )
        )
      )
    }
    
    tagList(file_cards, sample_section)
  })

  output$panel_selection_ui <- renderUI({
    has_m <- !is.null(rv$files_metas) && length(rv$files_metas) > 0
    has_l <- !is.null(rv$files_lipids) && length(rv$files_lipids) > 0
    has_q <- !is.null(rv$files_qc) && length(rv$files_qc) > 0

    choices <- c()
    if (has_m) choices <- c(choices, "Metabolites" = "metas")
    if (has_l) choices <- c(choices, "Lipids" = "lipids")
    if (has_q) choices <- c(choices, "QC" = "qc")

    if (length(choices) == 0) {
      return(tags$p(class = "text-muted", "Please select a folder first."))
    }

    checkboxGroupInput("panels_selected", label = NULL,
                       choices = choices, selected = choices)
  })

  # QC HELPER FUNCTIONS------------------
  qc_summary <- function(qc_detail) {
    if (is.null(qc_detail) || nrow(qc_detail) == 0) return(NULL)
    qc_detail %>%
      filter(Passed != "N/A") %>%
      group_by(ID, Category) %>%
      summarise(
        Total = n(),
        Passed_n = sum(Passed == "PASS"),
        Failed_n = sum(Passed == "FAIL"),
        Pass_Rate = round(Passed_n / Total * 100, 1),
        Status = ifelse(Failed_n == 0, "PASS", "FAIL"),
        Failed_Params = paste(Parameter[Passed == "FAIL"], collapse = "; "),
        .groups = "drop"
      )
  }

  qc_overall <- function(qc_detail) {
    if (is.null(qc_detail) || nrow(qc_detail) == 0) return(NULL)
    qc_detail %>%
      filter(Passed != "N/A") %>%
      group_by(ID) %>%
      summarise(
        Total_Checks = n(),
        Passed_n = sum(Passed == "PASS"),
        Failed_n = sum(Passed == "FAIL"),
        Pass_Rate = round(Passed_n / Total_Checks * 100, 1),
        Overall_Status = ifelse(Pass_Rate >= 90, "PASS", "FAIL"),
        Matrix_Integrity = ifelse(
          sum(Category == "Matrix Integrity" & Passed == "FAIL") == 0, "PASS", "FAIL"),
        Sample_Preparation = ifelse(
          sum(Category == "Sample Preparation" & Passed == "FAIL") == 0, "PASS", "FAIL"),
        NMR_Spectral_Quality = ifelse(
          sum(Category == "NMR Spectral Quality" & Passed == "FAIL") == 0, "PASS", "FAIL"),
        Failed_Parameters = paste(Parameter[Passed == "FAIL"], collapse = "; "),
        .groups = "drop"
      )
  }

  # XML PARSER: PROCESSING----------------
  observeEvent(input$process_btn, {
    req(rv$folder_path)

    # Initialize parallel workers on first use
    if (!isTRUE(rv$parallel_ready)) {
      id <- showNotification("Parallel processing initialised...",
                             duration = NULL, closeButton = FALSE, type = "message")
      plan(multisession, workers = max(1, parallel::detectCores() - 1))
      rv$parallel_ready <- TRUE
      removeNotification(id)
    }

    rv$xml_m <- NULL
    rv$xml_l <- NULL
    rv$xml_qc <- NULL
    rv_qc$detail <- NULL
    rv_qc$summary <- NULL
    rv_qc$overall <- NULL
    parallel_ready = FALSE
    rv$processed <- FALSE


    shinyjs::hide("download_div")

    panels <- input$panels_selected
    if (is.null(panels) || length(panels) == 0) {
      rv$status <- "ERROR: Please select at least one panel."
      return()
    }

    total_steps <- 0
    if ("metas" %in% panels && length(rv$files_metas) > 0) total_steps <- total_steps + length(rv$files_metas)
    if ("lipids" %in% panels && length(rv$files_lipids) > 0) total_steps <- total_steps + length(rv$files_lipids)
    if ("qc" %in% panels && length(rv$files_qc) > 0) total_steps <- total_steps + length(rv$files_qc) * 2

    withProgress(message = "Processing...", value = 0, {

      status_msg <- ""
      # METABOLITES (plasma_quant_report) ------------------
      # Structure: PARAMETER -> VALUE[@conc, @concUnit, @lod, @loq]
      #                       -> RELDATA[@rawConc, @rawConcUnit, @errConc, @sigCorr]
      #                       -> REFERENCE[@vmax, @vmin]

      if ("metas" %in% panels && length(rv$files_metas) > 0) {
        incProgress(0, detail = paste0("Metabolites: 0/", length(rv$files_metas)))

        results_m <- lapply(seq_along(rv$files_metas), function(i) {
          incProgress(1/total_steps,
                      detail = paste0("Metabolites: ", i, "/", length(rv$files_metas)))
          tryCatch({
            fp <- rv$files_metas[i]
            xml_data <- read_xml(fp)

            sample_node <- xml_find_first(xml_data, ".//SAMPLE")
            sample_name <- xml_attr(sample_node, "name")
            if (is.na(sample_name) || nchar(sample_name) == 0) {
              parts <- unlist(strsplit(fp, "[/\\\\]"))
              nmr_idx <- which(parts == "nmr")
              if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
            }

            params <- xml_find_all(xml_data, ".//QUANTIFICATION/PARAMETER")
            if (length(params) == 0) return(NULL)

            param_names <- xml_attr(params, "name")

            # VALUE node: conc, lod, loq
            value_nodes <- xml_find_first(params, ".//VALUE")
            conc_values <- as.numeric(xml_attr(value_nodes, "conc"))
            conc_units <- xml_attr(value_nodes, "concUnit")
            lod_values <- as.numeric(xml_attr(value_nodes, "lod"))

            # RELDATA node: rawConc, errConc, sigCorr
            reldata_nodes <- xml_find_first(params, ".//RELDATA")
            raw_conc <- as.numeric(xml_attr(reldata_nodes, "rawConc"))
            sig_corr <- as.numeric(xml_attr(reldata_nodes, "sigCorr"))

            n <- length(param_names)

            # Extract measurement date from SAMPLE node
            sample_date_raw <- xml_attr(sample_node, "date")
            meas_date <- parse_date_safe(sample_date_raw)

            data.table(
              ID = sample_name,
              MeasDate = meas_date,
              Parameter = param_names,
              Value = conc_values,
              RawConc = raw_conc,
              ErrConc = as.numeric(xml_attr(reldata_nodes, "errConc")),
              SigCorr = sig_corr,
              LOD = lod_values,
              Unit = conc_units,
              Order = seq_len(n)
            )
          }, error = function(e) NULL)
        })

        results_m <- results_m[!sapply(results_m, is.null)]
        if (length(results_m) > 0) {
          rv$xml_m <- rbindlist(results_m, use.names = TRUE, fill = TRUE)

          # Deduplicate
          dupes <- rv$xml_m[, .N, by = .(ID, Parameter)][N > 1]
          if (nrow(dupes) > 0) {
            rv$xml_m <- rv$xml_m[, .SD[1], by = .(ID, Parameter)]
          }

          status_msg <- paste0(status_msg,
                               "\u2713 Metabolites: ", length(results_m), " samples, ",
                               length(unique(rv$xml_m$Parameter)), " Parameter\n")
        } else {
          status_msg <- paste0(status_msg, "\u2717 Metabolites: No data extracted\n")
        }
      }

      # LIPIDS (plasma_lipo_report)---------------
      # Structure: QUANTIFICATION/PARAMETER[@name, @type] -> VALUE[@value, @unit]
      # Keep ALL parameters (don't filter by type)

      if ("lipids" %in% panels && length(rv$files_lipids) > 0) {
        incProgress(0, detail = paste0("Lipids: 0/", length(rv$files_lipids)))

        # Lipid name mapping (Bruker short names -> your standard names)
        # Lipid parameter naming
        # Mapping lives in lipid_naming.csv so it can be edited without
        # touching the code. Two schemes are available:
        #   "bruker" - keep the raw 4-character IVDr codes (TPTG, LDCH, ...)
        #   "larmor" - descriptive names with units (TG_mg_dl, LDL_CHOL_mg_dl, ...)
        lipid_scheme <- if (!is.null(input$lipid_naming)) input$lipid_naming else "larmor"

        lipid_map_df <- tryCatch(
          read.csv(file.path(APP_DIR, "lipid_naming.csv"), stringsAsFactors = FALSE),
          error = function(e) NULL
        )

        if (is.null(lipid_map_df) || !all(c("bruker_code", "larmor_short") %in% names(lipid_map_df))) {
          warning("lipid_naming.csv missing or malformed - falling back to raw Bruker codes")
          lipid_name_map <- character(0)
        } else if (identical(lipid_scheme, "bruker")) {
          # Identity mapping: code maps to itself
          lipid_name_map <- setNames(lipid_map_df$bruker_code, lipid_map_df$bruker_code)
        } else {
          lipid_name_map <- setNames(lipid_map_df$larmor_short, lipid_map_df$bruker_code)
        }

        results_l <- lapply(seq_along(rv$files_lipids), function(i) {
          incProgress(1/total_steps,
                      detail = paste0("Lipids: ", i, "/", length(rv$files_lipids)))
          tryCatch({
            fp <- rv$files_lipids[i]
            xml_data <- read_xml(fp)

            sample_node <- xml_find_first(xml_data, ".//SAMPLE")
            sample_name <- xml_attr(sample_node, "name")
            if (is.na(sample_name) || nchar(sample_name) == 0) {
              parts <- unlist(strsplit(fp, "[/\\\\]"))
              nmr_idx <- which(parts == "nmr")
              if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
            }

            params <- xml_find_all(xml_data, ".//QUANTIFICATION/PARAMETER")
            if (length(params) == 0) return(NULL)

            param_names <- xml_attr(params, "name")

            # Get VALUE child - try "value" first, then "conc"
            value_nodes <- xml_find_first(params, ".//VALUE")
            lipo_values <- as.numeric(xml_attr(value_nodes, "value"))
            lipo_units <- xml_attr(value_nodes, "unit")

            # If all values are NA, try "conc" attribute instead
            if (all(is.na(lipo_values))) {
              lipo_values <- as.numeric(xml_attr(value_nodes, "conc"))
              lipo_units <- xml_attr(value_nodes, "concUnit")
            }

            # Map short names to full names
          # Map codes to output names.
          # Unmapped codes pass through unchanged rather than being dropped,
          # so new IVDr parameters are never silently lost.
          full_names <- ifelse(
            param_names %in% names(lipid_name_map),
            lipid_name_map[param_names],
            param_names
          )

            valid <- !is.na(full_names) & !is.na(lipo_values)
            if (sum(valid) == 0) return(NULL)

            data.table(
              ID = sample_name,
              Parameter = full_names[valid],
              Value = lipo_values[valid],
              Unit = lipo_units[valid],
              Order = seq_along(full_names)[valid]
            )
          }, error = function(e) NULL)
        })

        results_l <- results_l[!sapply(results_l, is.null)]
        if (length(results_l) > 0) {
          rv$xml_l <- rbindlist(results_l, use.names = TRUE, fill = TRUE)

          # Deduplicate - keep first occurrence
          dupes <- rv$xml_l[, .N, by = .(ID, Parameter)][N > 1]
          if (nrow(dupes) > 0) {
            rv$xml_l <- rv$xml_l[, .SD[1], by = .(ID, Parameter)]
          }

          status_msg <- paste0(status_msg,
                               "\u2713 Lipids: ", length(results_l), " samples, ",
                               length(unique(rv$xml_l$Parameter)), " Parameter\n")
        } else {
          status_msg <- paste0(status_msg, "\u2717 Lipids: No data extracted\n")
        }
      }


# QC (plasma_qc_report) ------------------
      # Structure:
      #   SAMPLE/INFO[@name, @value] - top level pass/fail
      #   QUANTIFICATION/PARAMETER[@name, @type, @comment] -> VALUE[@value, @unit]
      #   REFERENCE[@vmax, @vmin] for limits

      if ("qc" %in% panels && length(rv$files_qc) > 0) {

        # ---- QC Data Parse (simple values) ----
        incProgress(0, detail = paste0("QC Data: 0/", length(rv$files_qc)))

        results_q <- lapply(seq_along(rv$files_qc), function(i) {
          incProgress(1/total_steps,
                      detail = paste0("QC Data: ", i, "/", length(rv$files_qc)))
          tryCatch({
            fp <- rv$files_qc[i]
            xml_data <- read_xml(fp)

            sample_node <- xml_find_first(xml_data, ".//SAMPLE")
            sample_name <- xml_attr(sample_node, "name")
            if (is.na(sample_name) || nchar(sample_name) == 0) {
              parts <- unlist(strsplit(fp, "[/\\\\]"))
              nmr_idx <- which(parts == "nmr")
              if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
            }

            params <- xml_find_all(xml_data, ".//QUANTIFICATION/PARAMETER")
            if (length(params) == 0) return(NULL)

            param_names <- xml_attr(params, "name")
            value_nodes <- xml_find_first(params, ".//VALUE")
            qc_values <- as.numeric(xml_attr(value_nodes, "value"))
            qc_units <- xml_attr(value_nodes, "unit")

            valid <- !is.na(param_names) & !is.na(qc_values)
            if (sum(valid) == 0) return(NULL)

            data.table(
              ID = sample_name,
              Parameter = param_names[valid],
              Value = qc_values[valid],
              Unit = qc_units[valid]
            )
          }, error = function(e) NULL)
        })

        results_q <- results_q[!sapply(results_q, is.null)]
        if (length(results_q) > 0) {
          rv$xml_qc <- rbindlist(results_q, use.names = TRUE, fill = TRUE)

          # Deduplicate
          dupes <- rv$xml_qc[, .N, by = .(ID, Parameter)][N > 1]
          if (nrow(dupes) > 0) {
            rv$xml_qc <- rv$xml_qc[, .SD[1], by = .(ID, Parameter)]
          }

          status_msg <- paste0(status_msg,
                               "\u2713 QC Data: ", length(results_q), " samples\n")
        } else {
          status_msg <- paste0(status_msg, "\u2717 QC Data: No data extracted\n")
        }

        # ---- QC CHECK (pass/fail evaluation) ----
        incProgress(0, detail = paste0("QC-Check: 0/", length(rv$files_qc)))

        qc_check_results <- lapply(seq_along(rv$files_qc), function(i) {
          incProgress(1/total_steps,
                      detail = paste0("QC-Check: ", i, "/", length(rv$files_qc)))
          tryCatch({
            fp <- rv$files_qc[i]
            qc_xml <- read_xml(fp)

            sample_node <- xml_find_first(qc_xml, ".//SAMPLE")
            sample_name <- xml_attr(sample_node, "name")
            if (is.na(sample_name) || nchar(sample_name) == 0) {
              parts <- unlist(strsplit(fp, "[/\\\\]"))
              nmr_idx <- which(parts == "nmr")
              if (length(nmr_idx) > 0) sample_name <- parts[nmr_idx + 1]
            }

            qc_rows <- list()
            row_count <- 0

            # --- Part A: SAMPLE/INFO nodes (top-level tests) ---
            info_nodes <- xml_find_all(qc_xml, ".//SAMPLE/INFO")
            for (info in info_nodes) {
              info_name <- xml_attr(info, "name")
              info_value <- xml_attr(info, "value")

              # Categorize
              category <- if (grepl("NMR Experiment|Spectral", info_name, ignore.case = TRUE)) {
                "NMR Spectral Quality"
              } else if (grepl("Preparation", info_name, ignore.case = TRUE)) {
                "Sample Preparation"
              } else if (grepl("Matrix|Identity|Integrity|Contamination", info_name, ignore.case = TRUE)) {
                "Matrix Integrity"
              } else {
                "NMR Spectral Quality"
              }

              passed <- if (grepl("^passed$", info_value, ignore.case = TRUE)) {
                "PASS"
              } else if (grepl("not passed", info_value, ignore.case = TRUE)) {
                "FAIL"
              } else {
                "N/A"
              }

              row_count <- row_count + 1
              qc_rows[[row_count]] <- list(
                ID = sample_name,
                Category = category,
                Parameter = info_name,
                Value = info_value,
                Unit = "",
                Lower_Limit = "",
                Upper_Limit = "",
                Limit = "",
                Status = info_value,
                Passed = passed
              )
            }

            # --- Part B: QUANTIFICATION/PARAMETER nodes (detailed checks) ---
            params <- xml_find_all(qc_xml, ".//QUANTIFICATION/PARAMETER")

            for (param in params) {
              param_name <- xml_attr(param, "name")
              param_type <- xml_attr(param, "type")
              param_comment <- xml_attr(param, "comment")

              # Categorize by "type" attribute
              category <- if (grepl("Spectral Quality", param_type, ignore.case = TRUE)) {
                "NMR Spectral Quality"
              } else if (grepl("Preparation", param_type, ignore.case = TRUE)) {
                "Sample Preparation"
              } else if (grepl("Matrix Integrity", param_type, ignore.case = TRUE)) {
                "Matrix Integrity"
              } else if (grepl("Matrix Identity", param_type, ignore.case = TRUE)) {
                "Matrix Integrity"
              } else if (grepl("Contamination", param_type, ignore.case = TRUE)) {
                "Matrix Integrity"
              } else {
                param_type
              }

              # Get VALUE node
              value_node <- xml_find_first(param, ".//VALUE")
              param_value <- xml_attr(value_node, "value")
              param_unit <- xml_attr(value_node, "unit")

              # Get REFERENCE node (first one) for limits
              ref_node <- xml_find_first(param, ".//REFERENCE")
              vmax <- if (!is.na(ref_node)) trimws(xml_attr(ref_node, "vmax")) else NA_character_
              vmin <- if (!is.na(ref_node)) trimws(xml_attr(ref_node, "vmin")) else NA_character_

              # Clean up "-/-" and "-" values
              if (!is.na(vmax) && vmax %in% c("-/-", "-", "")) vmax <- NA_character_
              if (!is.na(vmin) && vmin %in% c("-/-", "-", "")) vmin <- NA_character_

              # Determine pass/fail from "comment" attribute
              passed <- if (!is.na(param_comment)) {
                if (grepl("^passed$", param_comment, ignore.case = TRUE)) {
                  "PASS"
                } else if (grepl("not passed", param_comment, ignore.case = TRUE)) {
                  "FAIL"
                } else if (grepl("Unknown", param_comment, ignore.case = TRUE)) {
                  "N/A"
                } else {
                  "N/A"
                }
              } else {
                "N/A"
              }

              row_count <- row_count + 1
              qc_rows[[row_count]] <- list(
                ID = sample_name,
                Category = category,
                Parameter = paste0(param_name, " (", param_type, ")"),
                Value = ifelse(is.na(param_value), "", param_value),
                Unit = ifelse(is.na(param_unit), "", param_unit),
                Lower_Limit = ifelse(is.na(vmin), "", vmin),
                Upper_Limit = ifelse(is.na(vmax), "", vmax),
                Limit = "",
                Status = ifelse(is.na(param_comment), "", param_comment),
                Passed = passed
              )
            }

            if (row_count > 0) rbindlist(qc_rows) else NULL
          }, error = function(e) NULL)
        })

        qc_check_results <- qc_check_results[!sapply(qc_check_results, is.null)]
        if (length(qc_check_results) > 0) {
          qc_detail <- as.data.frame(rbindlist(qc_check_results, use.names = TRUE, fill = TRUE))
          rv_qc$detail <- qc_detail
          rv_qc$summary <- qc_summary(qc_detail)
          rv_qc$overall <- qc_overall(qc_detail)

          if (!is.null(rv_qc$overall)) {
            n_pass <- sum(rv_qc$overall$Overall_Status == "PASS")
            n_fail <- sum(rv_qc$overall$Overall_Status == "FAIL")
            status_msg <- paste0(status_msg,
                                 "\u2713 QC-Check: ", n_pass, " bestanden, ", n_fail, " nicht bestanden\n")
          }
        } else {
          status_msg <- paste0(status_msg, "\u2717 QC Check: No results\n")
        }
      }

      incProgress(0, detail = "Done!")

    }) # end withProgress

    rv$processed <- TRUE
    rv$status <- paste0("Processing complete!\n\n", status_msg)
    shinyjs::show("download_div")
  })




  # XML PARSER: PREVIEW TABLES-------------------
  output$preview_metas <- renderDT({
    req(rv$xml_m)
    first_id <- rv$xml_m$ID[1]
    param_order <- rv$xml_m[ID == first_id][order(Order)]$Parameter

    wide <- dcast(rv$xml_m, ID ~ Parameter, value.var = "Value", fun.aggregate = mean, na.rm = TRUE)
    cols_ordered <- intersect(param_order, names(wide))
    wide <- wide[, c("ID", cols_ordered), with = FALSE]

    datatable(wide, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })


  output$preview_lipids <- renderDT({
    req(rv$xml_l)

    if ("Order" %in% names(rv$xml_l)) {
      first_id <- rv$xml_l$ID[1]
      param_order <- rv$xml_l[ID == first_id][order(Order)]$Parameter
      wide <- dcast(rv$xml_l, ID ~ Parameter, value.var = "Value", fun.aggregate = mean, na.rm = TRUE)
      cols_ordered <- intersect(param_order, names(wide))
      wide <- wide[, c("ID", cols_ordered), with = FALSE]
    } else {
      wide <- dcast(rv$xml_l, ID ~ Parameter, value.var = "Value", fun.aggregate = mean, na.rm = TRUE)
    }

    datatable(wide, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })

  # XML PARSER: DOWNLOAD HANDLERS
  # Helper: dcast with preserved XML order
  dcast_ordered <- function(dt, value_col = "Value") {
    if ("Order" %in% names(dt)) {
      first_id <- dt$ID[1]
      param_order <- dt[ID == first_id][order(Order)]$Parameter
      wide <- dcast(dt, ID ~ Parameter, value.var = value_col, fun.aggregate = mean, na.rm = TRUE)
      cols_ordered <- intersect(param_order, names(wide))
      wide[, c("ID", cols_ordered), with = FALSE]
    } else {
      dcast(dt, ID ~ Parameter, value.var = value_col, fun.aggregate = mean, na.rm = TRUE)
    }
  }

  # ---- EXCEL (combined) ----
  output$download_xlsx_btn <- downloadHandler(
    filename = function() { paste0(input$filename, ".xlsx") },
    content = function(file) {
      wb <- createWorkbook()

      # SHEET 1: SUMMARY
      addWorksheet(wb, "Summary")
      n_samples_m <- if (!is.null(rv$xml_m)) length(unique(rv$xml_m$ID)) else 0
      n_samples_l <- if (!is.null(rv$xml_l)) length(unique(rv$xml_l$ID)) else 0
      n_params_m <- if (!is.null(rv$xml_m)) length(unique(rv$xml_m$Parameter)) else 0
      n_params_l <- if (!is.null(rv$xml_l)) length(unique(rv$xml_l$Parameter)) else 0

      all_ids <- unique(c(
        if (!is.null(rv$xml_m)) rv$xml_m$ID else character(0),
        if (!is.null(rv$xml_l)) rv$xml_l$ID else character(0)
      ))

      summary_info <- data.frame(
        Field = c("Export Date", "Folder", "Sample Type",
                  "Number of Samples", "Metabolite Parameters", "Lipid Parameters",
                  "", "QC Threshold", "QC Passed", "QC Failed"),
        Value = c(
          format(Sys.time(), "%Y-%m-%d %H:%M"),
          ifelse(!is.null(rv$folder_path), rv$folder_path, ""),
          ifelse(input$sample_type == "blood", "Blood (Plasma/Serum)", "Urine"),
          as.character(length(all_ids)),
          as.character(n_params_m),
          as.character(n_params_l),
          "",
          ">= 90% Pass Rate",
          if (!is.null(rv_qc$overall)) as.character(sum(rv_qc$overall$Overall_Status == "PASS")) else "0",
          if (!is.null(rv_qc$overall)) as.character(sum(rv_qc$overall$Overall_Status == "FAIL")) else "0"
        ),
        stringsAsFactors = FALSE
      )

      writeData(wb, "Summary", summary_info, startRow = 1)
      setColWidths(wb, "Summary", cols = 1:2, widths = c(25, 50))

      if (length(all_ids) > 0) {
        sample_list <- data.frame(Nr = seq_along(all_ids), Sample_ID = all_ids, stringsAsFactors = FALSE)

        if (!is.null(rv_qc$overall) && nrow(rv_qc$overall) > 0) {
          qc_merge <- rv_qc$overall[, c("ID", "Overall_Status", "Pass_Rate", "Failed_Parameters")]
          names(qc_merge)[1] <- "Sample_ID"
          sample_list <- merge(sample_list, qc_merge, by = "Sample_ID", all.x = TRUE)
          sample_list <- sample_list[order(sample_list$Nr), ]
          sample_list$Note <- ifelse(
            !is.na(sample_list$Overall_Status) & sample_list$Overall_Status == "FAIL",
            "check spectra", "")
        } else {
          sample_list$Overall_Status <- NA_character_
          sample_list$Pass_Rate <- NA_real_
          sample_list$Failed_Parameters <- NA_character_
          sample_list$Note <- ""
        }

        sample_list <- sample_list[, c("Nr", "Sample_ID", "Overall_Status", "Pass_Rate", "Note", "Failed_Parameters")]
        names(sample_list) <- c("Nr", "Sample", "QC Status", "Pass Rate (%)", "Note", "Failed Parameters")

        writeData(wb, "Summary", sample_list, startRow = nrow(summary_info) + 3)

        sample_header_row <- nrow(summary_info) + 3
        if (!is.null(rv_qc$overall)) {
          fail_rows <- which(sample_list$`QC Status` == "FAIL") + sample_header_row
          if (length(fail_rows) > 0) {
            failStyle <- createStyle(fgFill = "#fadbd8", fontColour = "#c0392b")
            for (fr in fail_rows) addStyle(wb, "Summary", failStyle, rows = fr, cols = 1:ncol(export_df), stack = TRUE)
          }
          pass_rows <- which(sample_list$`QC Status` == "PASS") + sample_header_row
          if (length(pass_rows) > 0) {
            passStyle <- createStyle(fgFill = "#d5f5e3")
            for (pr in pass_rows) addStyle(wb, "Summary", passStyle, rows = pr, cols = 1:ncol(export_df), stack = TRUE)
          }
        }
        setColWidths(wb, "Summary", cols = 1:ncol(export_df), widths = c(5, 20, 12, 12, 15, 50))
      }

      # SHEET 2: METABOLITES
      if (!is.null(rv$xml_m) && nrow(rv$xml_m) > 0) {
        wide_conc <- dcast_ordered(rv$xml_m, "Value")
        addWorksheet(wb, "Metabolites")
        writeData(wb, "Metabolites", wide_conc)
        setColWidths(wb, "Metabolites", cols = 1:ncol(wide_conc), widths = "auto")
      }

      # SHEET 3: METABOLITES RAW
      if (!is.null(rv$xml_m) && nrow(rv$xml_m) > 0) {
        wide_raw <- dcast_ordered(rv$xml_m, "RawConc")
        addWorksheet(wb, "Metabolites_RawConc")
        writeData(wb, "Metabolites_RawConc", wide_raw)
        setColWidths(wb, "Metabolites_RawConc", cols = 1:ncol(wide_raw), widths = "auto")
      }

      # SHEET 4: METABOLITES ERROR
      if (!is.null(rv$xml_m) && nrow(rv$xml_m) > 0) {
        wide_err <- dcast_ordered(rv$xml_m, "ErrConc")
        addWorksheet(wb, "Metabolites_ErrConc")
        writeData(wb, "Metabolites_ErrConc", wide_err)
        setColWidths(wb, "Metabolites_ErrConc", cols = 1:ncol(wide_err), widths = "auto")
      }

      # SHEET 5: METABOLITES SIGCORR
      if (!is.null(rv$xml_m) && nrow(rv$xml_m) > 0) {
        wide_sig <- dcast_ordered(rv$xml_m, "SigCorr")
        addWorksheet(wb, "Metabolites_SigCorr")
        writeData(wb, "Metabolites_SigCorr", wide_sig)
        setColWidths(wb, "Metabolites_SigCorr", cols = 1:ncol(wide_sig), widths = "auto")
      }

      # SHEET 6: METABOLITES LOD
      if (!is.null(rv$xml_m) && nrow(rv$xml_m) > 0) {
        wide_lod <- dcast_ordered(rv$xml_m, "LOD")
        addWorksheet(wb, "Metabolites_LOD")
        writeData(wb, "Metabolites_LOD", wide_lod)
        setColWidths(wb, "Metabolites_LOD", cols = 1:ncol(wide_lod), widths = "auto")
      }

      # SHEET 7: LIPIDS
      if (!is.null(rv$xml_l) && nrow(rv$xml_l) > 0) {
        wide_l <- dcast_ordered(rv$xml_l, "Value")
        addWorksheet(wb, "Lipids")
        writeData(wb, "Lipids", wide_l)
        setColWidths(wb, "Lipids", cols = 1:ncol(wide_l), widths = "auto")
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # ---- INDIVIDUAL CSVs ----
  output$dl_metabolites_conc <- downloadHandler(
    filename = function() { paste0(input$filename, "_metabolites_conc.csv") },
    content = function(file) {
      req(rv$xml_m)
      fwrite(dcast_ordered(rv$xml_m, "Value"), file)
    }
  )

  output$dl_metabolites_raw <- downloadHandler(
    filename = function() { paste0(input$filename, "_metabolites_rawconc.csv") },
    content = function(file) {
      req(rv$xml_m)
      fwrite(dcast_ordered(rv$xml_m, "RawConc"), file)
    }
  )

  output$dl_metabolites_err <- downloadHandler(
    filename = function() { paste0(input$filename, "_metabolites_errconc.csv") },
    content = function(file) {
      req(rv$xml_m)
      fwrite(dcast_ordered(rv$xml_m, "ErrConc"), file)
    }
  )

  output$dl_metabolites_sig <- downloadHandler(
    filename = function() { paste0(input$filename, "_metabolites_sigcorr.csv") },
    content = function(file) {
      req(rv$xml_m)
      fwrite(dcast_ordered(rv$xml_m, "SigCorr"), file)
    }
  )

  output$dl_metabolites_lod <- downloadHandler(
    filename = function() { paste0(input$filename, "_metabolites_lod.csv") },
    content = function(file) {
      req(rv$xml_m)
      fwrite(dcast_ordered(rv$xml_m, "LOD"), file)
    }
  )

  output$dl_lipids <- downloadHandler(
    filename = function() { paste0(input$filename, "_lipids.csv") },
    content = function(file) {
      req(rv$xml_l)
      fwrite(dcast_ordered(rv$xml_l, "Value"), file)
    }
  )

  # ---- SUMMARY CSV ----
  output$dl_summary <- downloadHandler(
    filename = function() { paste0(input$filename, "_summary.csv") },
    content = function(file) {
      all_ids <- unique(c(
        if (!is.null(rv$xml_m)) rv$xml_m$ID else character(0),
        if (!is.null(rv$xml_l)) rv$xml_l$ID else character(0)
      ))

      sample_list <- data.frame(
        Nr = seq_along(all_ids),
        Sample = all_ids,
        stringsAsFactors = FALSE
      )

      if (!is.null(rv_qc$overall) && nrow(rv_qc$overall) > 0) {
        qc_merge <- rv_qc$overall[, c("ID", "Overall_Status", "Pass_Rate", "Failed_Parameters")]
        names(qc_merge)[1] <- "Sample"
        sample_list <- merge(sample_list, qc_merge, by = "Sample", all.x = TRUE)
        sample_list <- sample_list[order(sample_list$Nr), ]
        sample_list$Note <- ifelse(
          !is.na(sample_list$Overall_Status) & sample_list$Overall_Status == "FAIL",
          "check spectra", "")
      } else {
        sample_list$Overall_Status <- NA_character_
        sample_list$Pass_Rate <- NA_real_
        sample_list$Failed_Parameters <- NA_character_
        sample_list$Note <- ""
      }

      sample_list <- sample_list[, c("Nr", "Sample", "Overall_Status", "Pass_Rate", "Note", "Failed_Parameters")]
      names(sample_list) <- c("Nr", "Sample", "QC_Status", "Pass_Rate_Pct", "Note", "Failed_Parameters")

      fwrite(sample_list, file)
    }
  )

  # ---- QC REPORT CSV ----
  output$dl_qc <- downloadHandler(
    filename = function() { paste0(input$filename, "_qc_report.csv") },
    content = function(file) {
      req(rv_qc$overall)
      fwrite(rv_qc$overall, file)
    }
  )

  # SAMPLE SUBMISSION: EXPERIMENT SELECTION-----------------------
  output$experiment_selection_ui <- renderUI({
    def <- get_exp_def(input$sub_solvent, input$tube_size)
    default_exp <- get_exp_list(def)

    if (length(default_exp) == 0) {
      return(tags$div(class = "text-muted small",
        "No experiments defined for this sample type. Check experiment_types.csv."))
    }

    checkboxGroupInput("experiments", "Experiments:",
                       choices = default_exp, selected = default_exp)
  })

  # SAMPLE SUBMISSION: PROJECT & BOX DROPDOWN-----------------
  observe({
    choices <- sort(rv_projects$data$Abbreviation[!is.na(rv_projects$data$Abbreviation)])
    updateSelectizeInput(session, "project_select", choices = choices, server = TRUE)
  })

  observeEvent(input$project_select, {
    proj <- input$project_select
    if (is.null(proj) || nchar(proj) == 0) {
      updateSelectizeInput(session, "box_select", choices = character(0))
      return()
    }

    boxes_for_project <- rv_boxes$data %>%
      filter(Project == proj) %>%
      filter(Status %in% c("Received", "In Preparation", "Received", "In Preparation", "Open", "New"))

    if (nrow(boxes_for_project) == 0) {
      updateSelectizeInput(session, "box_select",
                           choices = c("No open boxes" = ""),
                           selected = "")
    } else {
      choices <- setNames(
        boxes_for_project$Box_Code,
        paste0(boxes_for_project$Box_Code, " \u2014 ", boxes_for_project$Box_Name,
               " (", boxes_for_project$Sample_Count, " samples)")
      )
      updateSelectizeInput(session, "box_select", choices = choices)
    }
  })

  # SAMPLE SUBMISSION: GENERATE SUBMISSION-------------------
  observeEvent(input$generate_btn, {

    rv_sub$sub_status <- "creating samplelist..."
    rv_sub$submission_table <- NULL
    shinyjs::hide("download_excel_div")

    tryCatch({
      # Get sample names
      if (input$input_method == "manual") {
        names_raw <- input$manual_names
        if (is.null(names_raw) || nchar(trimws(names_raw)) == 0) {
          rv_sub$sub_status <- "Error: Please enter sample IDs."
          return()
        }
        sample_names <- trimws(unlist(strsplit(names_raw, "\n")))
        sample_names <- sample_names[nchar(sample_names) > 0]
      } else {
        req(input$sample_file)
        ext <- tools::file_ext(input$sample_file$name)
        if (ext %in% c("csv", "txt")) {
          uploaded <- read.csv(input$sample_file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        } else if (ext == "xlsx") {
          uploaded <- openxlsx::read.xlsx(input$sample_file$datapath)
        } else {
          rv_sub$sub_status <- "Error: Unsupported file format."
          return()
        }
        
        # Get sample names
        if ("Name" %in% names(uploaded)) {
          sample_names <- uploaded$Name
        } else if ("name" %in% names(uploaded)) {
          sample_names <- uploaded$name
        } else {
          sample_names <- uploaded[[1]]
        }
        sample_names <- trimws(as.character(sample_names))
        sample_names <- sample_names[nchar(sample_names) > 0]
        
        # ---- Detect extra archive columns from uploaded file ----
        config <- read_config()
        rv_sub$uploaded_extra_cols <- NULL  # Reset
        
        if (!is.null(config)) {
          auto_cols <- c("Date", "Project", "Box", "Box_Code", "Type", "Size")
          extra_cols <- setdiff(config$archive$columns, c(auto_cols, "Name"))
          
          if (length(extra_cols) > 0) {
            matched <- character(0)
            unmatched <- character(0)
            uploaded_extra <- list()
            
            for (col in extra_cols) {
              if (col %in% names(uploaded)) {
                # Exact match
                uploaded_extra[[col]] <- as.character(uploaded[[col]])
                matched <- c(matched, col)
              } else {
                # Check case-insensitive match
                ci_match <- names(uploaded)[tolower(names(uploaded)) == tolower(col)]
                if (length(ci_match) > 0) {
                  uploaded_extra[[col]] <- as.character(uploaded[[ci_match[1]]])
                  matched <- c(matched, paste0(col, " (matched from '", ci_match[1], "')"))
                } else {
                  unmatched <- c(unmatched, col)
                }
              }
            }
            
            rv_sub$uploaded_extra_cols <- uploaded_extra
            
            # ---- Check for file columns that DON'T match any config column ----
            # Columns in the uploaded file that are not auto-cols and not matched to config
            name_col <- if ("Name" %in% names(uploaded)) "Name" else if ("name" %in% names(uploaded)) "name" else names(uploaded)[1]
            all_known_cols <- c(auto_cols, "Name", "name", extra_cols)
            # Also check case-insensitive
            file_cols_unmatched <- names(uploaded)[!names(uploaded) %in% all_known_cols & 
                                                     !tolower(names(uploaded)) %in% tolower(all_known_cols)]
            
            # Show match results in status
            match_info <- ""
            if (length(matched) > 0) {
              match_info <- paste0("\n  Matched columns: ", paste(matched, collapse = ", "))
            }
            if (length(unmatched) > 0) {
              match_info <- paste0(match_info, 
                                   "\n  WARNING - Config columns not in file (will be empty): ", 
                                   paste(unmatched, collapse = ", "))
              showNotification(
                paste0("Column mismatch! These archive columns were not found in your file: ",
                       paste(unmatched, collapse = ", "),
                       ". They will be left empty."),
                type = "warning", duration = 8
              )
            }
            if (length(file_cols_unmatched) > 0) {
              match_info <- paste0(match_info,
                                   "\n  INFO - File columns not in config (ignored): ",
                                   paste(file_cols_unmatched, collapse = ", "))
              showNotification(
                paste0("Your file contains columns not configured in the archive: ",
                       paste(file_cols_unmatched, collapse = ", "),
                       ". These columns were ignored. To include them, add them in Setup > Archive Columns."),
                type = "warning", duration = 10
              )
            }
            
            rv_sub$sub_status <- paste0("File loaded: ", length(sample_names), " samples",
                                        match_info)
          }
          
        }
      }
      
      

      if (length(sample_names) == 0) {
        rv_sub$sub_status <- "Error: No valid sample id found."
        return()
      }

      # Get parameters (with safety guards)
      solvent <- if (is.null(input$sub_solvent) || length(input$sub_solvent) == 0) "Plasma" else input$sub_solvent
      size <- if (is.null(input$tube_size) || length(input$tube_size) == 0) "5mm" else input$tube_size
      rack <- tryCatch(as.integer(input$rack_number), error = function(e) 1L, warning = function(w) 1L)
      if (is.null(rack) || length(rack) == 0 || is.na(rack)) rack <- 1L
      experiments <- input$experiments
      path <- if (is.null(input$data_path) || length(input$data_path) == 0) "" else input$data_path

      if (is.null(experiments) || length(experiments) == 0) {
        rv_sub$sub_status <- "Error: Select at least one experiment."
        return()
      }

      # Get project and box info
      project_name <- input$project_select
      if (is.null(project_name) || length(project_name) == 0 || nchar(trimws(project_name)) == 0) {
        project_name <- "Unknown"
      }
      
      selected_box_code <- input$box_select
      if (is.null(selected_box_code) || length(selected_box_code) == 0 || 
          !nzchar(trimws(as.character(selected_box_code)))) {
        box_name <- paste0("Box_", format(Sys.Date(), "%Y%m%d"))
        selected_box_code <- NA_character_
      } else {
        selected_box_code <- as.character(selected_box_code)
        box_row <- which(rv_boxes$data$Box_Code == selected_box_code)
        if (length(box_row) > 0) {
          box_name <- rv_boxes$data[box_row, "Box_Name"]
        } else {
          box_name <- selected_box_code
        }
      }
      

      # Determine SOLVENT label from experiment_types.csv
      exp_def <- get_exp_def(solvent, size)
      SOLVENT <- if (!is.null(exp_def)) as.character(exp_def$solvent) else solvent
      if (solvent == "Media" && size == "5mm") SOLVENT <- "Plasma"
      if (solvent == "Media" && size == "3mm") SOLVENT <- "Plasma_3mm"

      # Build file path
      Date <- Sys.Date()
      YEAR <- format(Date, "%Y")
      DISK <- file.path(path, YEAR, "data", as.character(Date), "nmr")

      # Build HOLDER positions
      # Rack 1-5, positions 1-96 each. Holder = rack digit + 2-digit position.
      # e.g. rack 1 pos 6 -> "106", rack 5 pos 33 -> "533"
      # Overflow wraps to the next rack; after rack 5 it returns to rack 1.
      n_samples <- length(sample_names)

      start_slot <- tryCatch(as.integer(input$start_slot),
                             error = function(e) 1L, warning = function(w) 1L)
      if (is.null(start_slot) || length(start_slot) == 0 || is.na(start_slot)) start_slot <- 1L
      start_slot <- max(1L, min(96L, start_slot))

      RACK_SIZE  <- 96L
      N_RACKS    <- 5L

      # Zero-based running offset from the very first slot used
      offset <- (start_slot - 1L) + seq_len(n_samples) - 1L

      # Position within a rack (1-96)
      POSI <- (offset %% RACK_SIZE) + 1L

      # How many racks we have advanced, wrapping 1..5
      rack_advance <- offset %/% RACK_SIZE
      RACK_NUM <- ((rack - 1L + rack_advance) %% N_RACKS) + 1L

      HOLDER <- paste0(RACK_NUM, sprintf("%02d", POSI))

      # Build base table
      base_df <- data.frame(
        DISK = DISK,
        NAME = sample_names,
        SOLVENT = SOLVENT,
        HOLDER = HOLDER,
        TITLE = sample_names,
        stringsAsFactors = FALSE
      )

      # Merge with experiments
      # Experiment order comes from experiment_types.csv.
      # Keep only what the user selected, preserving the defined order.
      exp_order <- get_exp_list(get_exp_def(solvent, size))
      experiments_ordered <- exp_order[exp_order %in% experiments]

      if (length(experiments_ordered) == 0) {
        rv_sub$sub_status <- "Error: No valid experiments for this sample type."
        return()
      }

      # Build submission: one row per sample per experiment, in correct order
      submission <- do.call(rbind, lapply(seq_len(n_samples), function(i) {
        data.frame(
          DISK = base_df$DISK[i],
          NAME = base_df$NAME[i],
          SOLVENT = base_df$SOLVENT[i],
          EXPERIMENT = experiments_ordered,
          HOLDER = base_df$HOLDER[i],
          TITLE = base_df$TITLE[i],
          stringsAsFactors = FALSE
        )
      }))

      rownames(submission) <- NULL


      rv_sub$submission_table <- submission
      rv_sub$sub_status <- tryCatch(paste0(
        "Sample list created!\n",
        "  Samples: ", n_samples, "\n",
        "  Experiments per sample: ", length(experiments), "\n",
        "  All submissions: ", nrow(submission), "\n",
        "  Project: ", if (is.null(project_name) || length(project_name) == 0) "Unknown" else project_name, "\n",
        "  Box: ", if (is.null(box_name) || length(box_name) == 0) "None" else box_name,
        if (!is.null(selected_box_code) && length(selected_box_code) > 0 && !is.na(selected_box_code)) paste0(" (", selected_box_code, ")") else ""
      ), error = function(e) "Sample list created!")
      shinyjs::show("download_excel_div")

      # ---- ARCHIVE ----
      config <- read_config()
      archive_cols <- if (!is.null(config)) config$archive$columns else c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
      
      archive_typ <- if (is.null(input$sub_solvent) || length(input$sub_solvent) == 0) "Unknown" else input$sub_solvent
      if (is.null(archive_typ) || length(archive_typ) == 0) archive_typ <- "Unknown"
      if (archive_typ == "Urine_Neo") archive_typ <- "Urine"
      
      current_box_code <- selected_box_code
      current_box_name <- box_name
      
      # Get uploaded extra columns (only from file upload)
      uploaded_extra <- rv_sub$uploaded_extra_cols
      
      new_archive_rows <- do.call(rbind, lapply(seq_along(sample_names), function(i) {
        sname <- sample_names[i]
        row <- list()
        for (col in archive_cols) {
          row[[col]] <- switch(col,
                               "Date" = format(Sys.Date(), "%Y-%m-%d"),
                               "Name" = sname,
                               "Project" = project_name,
                               "Type" = archive_typ,
                               "Size" = if (is.null(input$tube_size) || length(input$tube_size) == 0) "?" else input$tube_size,
                               "Box" = current_box_name,
                               "Box_Code" = if (!is.null(current_box_code) && length(current_box_code) > 0 && !is.na(current_box_code)) current_box_code else NA_character_,
                               {
                                 # Extra columns — only from uploaded file
                                 if (!is.null(uploaded_extra) && col %in% names(uploaded_extra) && 
                                     i <= length(uploaded_extra[[col]])) {
                                   val <- uploaded_extra[[col]][i]
                                   if (!is.na(val) && nchar(trimws(as.character(val))) > 0) as.character(val) else NA_character_
                                 } else {
                                   NA_character_
                                 }
                               }
          )
        }
        as.data.frame(row, stringsAsFactors = FALSE)
      }))
      
      # Ensure archive has all columns from config
      for (col in archive_cols) {
        if (!col %in% names(rv_archive$data)) rv_archive$data[[col]] <- NA_character_
        if (!col %in% names(new_archive_rows)) new_archive_rows[[col]] <- NA_character_
      }
      
      rv_archive$data <- rbind(
        rv_archive$data[, archive_cols, drop = FALSE],
        new_archive_rows[, archive_cols, drop = FALSE]
      )
      
      tryCatch({
        write.csv(rv_archive$data, archive_path, row.names = FALSE)
      }, error = function(e) {
        showNotification(paste0("Archive error: ", e$message), type = "error")
      })
      
      # ---- SAFE STATUS MESSAGE ----
      n <- length(sample_names)
      proj <- if (is.null(project_name) || length(project_name) == 0) "Unknown" else as.character(project_name)
      bx <- if (is.null(box_name) || length(box_name) == 0) "None" else as.character(box_name)
      typ <- if (is.null(input$sub_solvent) || length(input$sub_solvent) == 0) "?" else input$sub_solvent
      tube <- if (is.null(input$tube_size) || length(input$tube_size) == 0) "?" else input$tube_size
      rack <- if (is.null(input$rack_number) || length(input$rack_number) == 0) "?" else as.character(input$rack_number)
      slot <- if (is.null(input$start_slot) || length(input$start_slot) == 0) "?" else as.character(input$start_slot)
      arch_new <- if (is.null(new_archive_rows)) 0 else nrow(new_archive_rows)
      arch_total <- if (is.null(rv_archive$data)) 0 else nrow(rv_archive$data)
      
      rv_sub$sub_status <- paste0(
        "Sample list created successfully!",
        "\n\nSamples: ", n,
        "\nProject: ", proj,
        "\nBox: ", bx,
        "\nType: ", typ,
        "\nTube: ", tube,
        "\nRack: ", rack, " | Start: ", slot,
        "\n\nArchive: ", arch_new, " entries added (Total: ", arch_total, ")"
      )
      
      # Update box status to "Measured"
      if (!is.na(selected_box_code) && length(selected_box_code) > 0 && nchar(selected_box_code) > 0) {
        box_row_idx <- which(rv_boxes$data$Box_Code == selected_box_code)
        if (length(box_row_idx) > 0) {
          rv_boxes$data[box_row_idx, "Status"] <- "Measured"
          
          tryCatch({
            write.csv(rv_boxes$data, box_registry_path, row.names = FALSE)
          }, error = function(e) {
            showNotification(paste0("Box register error: ", e$message), type = "error")
          })
          
          rv_sub$sub_status <- paste0(rv_sub$sub_status,
                                      "\n\nBox '", selected_box_code, "' -> Status: Measured")
        }
      }
      
      # Increment Measured in Projects list
      tryCatch({
        if (proj != "Unknown" && !is.null(rv_projects$data) && nrow(rv_projects$data) > 0 &&
            "Abbreviation" %in% names(rv_projects$data)) {
          proj_idx <- which(rv_projects$data$Abbreviation == proj)
          if (length(proj_idx) > 0) {
            if ("Measured" %in% names(rv_projects$data)) {
              current_val <- rv_projects$data[proj_idx, "Measured"]
              current_num <- suppressWarnings(as.numeric(current_val))
              if (is.na(current_num)) current_num <- 0
              rv_projects$data[proj_idx, "Measured"] <- as.character(current_num + 1)
              
              tryCatch({
                write.csv(rv_projects$data, projects_csv_path, row.names = FALSE)
              }, error = function(e) {
                showNotification(paste0("Project save error: ", e$message), type = "error")
              })
              
              rv_sub$sub_status <- paste0(rv_sub$sub_status,
                                          "\nProject '", proj, "': Measured = ", current_num + 1)
            } else {
              rv_sub$sub_status <- paste0(rv_sub$sub_status,
                                          "\nProject '", proj, "': No Measured column configured.")
            }
          }
        } else {
          rv_sub$sub_status <- paste0(rv_sub$sub_status,
                                      "\n\nNo project selected - Measured not updated.")
        }
      }, error = function(e) {
        # Silently handle project update errors
        rv_sub$sub_status <- paste0(rv_sub$sub_status,
                                    "\nProject update skipped: ", e$message)
      })
      
      # Extra column info
      tryCatch({
        config <- read_config()
        if (!is.null(config)) {
          auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
          extra_cols <- setdiff(config$archive$columns, auto_cols)
          if (length(extra_cols) > 0 && arch_new > 0) {
            filled <- sapply(extra_cols, function(col) {
              if (col %in% names(new_archive_rows)) {
                vals <- new_archive_rows[[col]]
                sum(!is.na(vals) & nchar(trimws(as.character(vals))) > 0)
              } else {
                0
              }
            })
            filled_info <- paste0(extra_cols, ": ", filled, "/", arch_new)
            rv_sub$sub_status <- paste0(rv_sub$sub_status,
                                        "\nAdditional fields: ", paste(filled_info, collapse = ", "))
          }
        }
      }, error = function(e) {
        # Silently ignore extra column info errors
      })
      
    }, error = function(e) {
      rv_sub$sub_status <- paste0("Error: ", e$message)
      showNotification(paste0("Error: ", e$message), type = "error")
    })
  })
  

  observeEvent(input$input_method, {
    if (input$input_method == "manual") {
      rv_sub$uploaded_extra_cols <- NULL
    }
  })  
  
  # SAMPLE SUBMISSION: STATUS & PREVIEW & DOWNLOAD---------------
  output$submit_status_text <- renderText({
    status <- rv_sub$sub_status
    if (is.null(status) || length(status) == 0) {
      "Ready. Please enter samples and select parameters."
    } else {
      as.character(status)
    }
  })
  
    output$preview_submission <- renderDT({
    req(rv_sub$submission_table)
    datatable(rv_sub$submission_table,
              options = list(scrollX = TRUE, pageLength = 20,
                             language = list(
                               url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                             )),
              rownames = FALSE)
  })

  output$download_excel_btn <- downloadHandler(
    filename = function() {
      paste0("SampleSubmission_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Sample Submission")
      # ICON-NMR's default import mapping expects "SAMPLE NAME".
      # The internal column stays NAME; only the exported header differs.
      export_df <- rv_sub$submission_table
      cfg <- read_config()
      hdr <- cfg$icon_headers
      if (is.null(hdr)) hdr <- list(NAME = "SAMPLE NAME")
      for (internal in names(hdr)) {
        if (internal %in% names(export_df)) {
          names(export_df)[names(export_df) == internal] <- hdr[[internal]]
        }
      }
      writeData(wb, "Sample Submission", export_df)
      setColWidths(wb, "Sample Submission", cols = 1:ncol(export_df), widths = "auto")
      headerStyle <- createStyle(
        textDecoration = "bold", fgFill = "#2c3e50",
        fontColour = "#ffffff", halign = "center"
      )
      addStyle(wb, "Sample Submission", headerStyle, rows = 1, cols = 1:ncol(export_df))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # BIOBANK: LOGIC--------------------
  # Generate next box code
  output$next_box_code <- renderText({
    year <- format(Sys.Date(), "%Y")
    existing <- rv_boxes$data$Box_Code
    year_codes <- existing[grepl(paste0("BOX-", year), existing)]
    if (length(year_codes) == 0) {
      next_num <- 1
    } else {
      nums <- as.numeric(gsub(paste0("BOX-", year, "-"), "", year_codes))
      next_num <- max(nums, na.rm = TRUE) + 1
    }
    paste0("BOX-", year, "-", sprintf("%04d", next_num))
  })

  # Update project dropdown in box register
  observe({
    choices <- sort(rv_projects$data$Abbreviation[!is.na(rv_projects$data$Abbreviation)])
    updateSelectizeInput(session, "box_project", choices = choices, server = TRUE)
  })

  # Update filter choices
  observe({
    df <- rv_boxes$data
    if (nrow(df) > 0) {
      projects <- sort(unique(df$Project))
      updateSelectizeInput(session, "box_filter_project", choices = projects, server = TRUE)
      dates <- as.Date(df$Received)
      dates <- dates[!is.na(dates)]
      if (length(dates) > 0) {
        updateDateRangeInput(session, "box_filter_dates",
                             start = min(dates), end = max(dates))
      }
    }
  })

  # Register new box
  observeEvent(input$register_box_btn, {
    if (is.null(input$box_project) || nchar(input$box_project) == 0) {
      showNotification("Please select a project.", type = "error", duration = 4)
      return()
    }
    if (is.null(input$box_name_input) || nchar(trimws(input$box_name_input)) == 0) {
      showNotification("PLease enter a box id.", type = "error", duration = 4)
      return()
    }

    year <- format(Sys.Date(), "%Y")
    existing <- rv_boxes$data$Box_Code
    year_codes <- existing[grepl(paste0("BOX-", year), existing)]
    if (length(year_codes) == 0) {
      next_num <- 1
    } else {
      nums <- as.numeric(gsub(paste0("BOX-", year, "-"), "", year_codes))
      next_num <- max(nums, na.rm = TRUE) + 1
    }
    box_code <- paste0("BOX-", year, "-", sprintf("%04d", next_num))

    new_box <- data.frame(
      Box_Code = box_code,
      Project = input$box_project,
      Box_Name = trimws(input$box_name_input),
      Sample_Count = ifelse(is.null(input$box_n_samples), NA, input$box_n_samples),
      Received = as.character(input$box_date_received),
      Status = input$box_status,
      Measured_Date = NA_character_,   # <-- ADD THIS
      Notes = trimws(input$box_notes),
      stringsAsFactors = FALSE
    )

    rv_boxes$data <- rbind(rv_boxes$data, new_box)

    tryCatch({
      write.csv(rv_boxes$data, box_registry_path, row.names = FALSE)
    }, error = function(e) {
      showNotification(paste0("Save error: ", e$message), type = "error")
    })

    tryCatch({ create_backup(type = "box_registration") }, error = function(e) {})

    updateTextInput(session, "box_name_input", value = "")
    updateNumericInput(session, "box_n_samples", value = NA)
    updateTextAreaInput(session, "box_notes", value = "")
    updateDateInput(session, "box_date_received", value = Sys.Date())
    updateSelectInput(session, "box_status", selected = "Received")

    showNotification(
      paste0("Box '", box_code, "' registered with project'", input$box_project, "'!"),
      type = "message", duration = 4)
  })

  # Clear box form
  observeEvent(input$clear_box_form_btn, {
    updateTextInput(session, "box_name_input", value = "")
    updateNumericInput(session, "box_n_samples", value = NA)
    updateTextAreaInput(session, "box_notes", value = "")
    updateDateInput(session, "box_date_received", value = Sys.Date())
    updateSelectInput(session, "box_status", selected = "Received")
  })

  # Filtered box data
  boxes_filtered <- reactive({
    df <- rv_boxes$data
    if (nrow(df) == 0) return(df)

    if (!is.null(input$box_filter_project) && length(input$box_filter_project) > 0) {
      df <- df %>% filter(Project %in% input$box_filter_project)
    }
    if (!is.null(input$box_filter_status) && input$box_filter_status != "all") {
      df <- df %>% filter(Status == input$box_filter_status)
    }
    if (!is.null(input$box_filter_dates)) {
      df$Received_date <- as.Date(df$Received)
      df <- df %>% filter(Received_date >= input$box_filter_dates[1] &
                            Received_date <= input$box_filter_dates[2])
      df$Received_date <- NULL
    }
    df
  })

  # Box KPIs
  output$box_kpi_total <- renderText({ nrow(rv_boxes$data) })
  output$box_kpi_received <- renderText({
    sum(rv_boxes$data$Status == "Received", na.rm = TRUE)
  })
  output$box_kpi_prep <- renderText({
    sum(rv_boxes$data$Status == "In Preparation", na.rm = TRUE)
  })
  output$box_kpi_measured <- renderText({
    sum(rv_boxes$data$Status == "Measured", na.rm = TRUE) +
      sum(rv_boxes$data$Status == "Completed", na.rm = TRUE)
  })

  # Box chart
  output$box_chart_per_project <- renderPlotly({
    df <- boxes_filtered()
    if (nrow(df) == 0) return(plotly_empty())

    box_counts <- df %>% count(Project, Status) %>% arrange(desc(n))
    colors <- c("Received" = "#3498db", "In Preparation" = "#f39c12",
                "Measured" = "#18bc9c", "Completed" = "#95a5a6")

    plot_ly(box_counts, y = ~reorder(Project, n), x = ~n, color = ~Status,
            type = "bar", orientation = "h", colors = colors,
            hovertemplate = "%{y}: %{x} Boxen<extra>%{fullData.name}</extra>") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Number of Boxes"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.2),
             margin = list(l = 150, t = 10))
  })

  # Box registry table
  output$box_registry_table <- renderDT({
    df <- boxes_filtered()
    datatable(df, selection = "single", class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 20,
                             order = list(list(4, "desc")),
                             language = list(
                               url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                             )),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(
                    c("Received", "Measured"),
                    c("#d6eaf8", "#d5f5e3")
                  ))
  })

  # Edit box (modal)
  observeEvent(input$box_edit_btn, {
    row_idx <- input$box_registry_table_rows_selected
    if (length(row_idx) == 0) {
      showNotification("Please select a row first.", type = "warning")
      return()
    }

    filtered_df <- boxes_filtered()
    box_code <- filtered_df[row_idx, "Box_Code"]
    full_row_idx <- which(rv_boxes$data$Box_Code == box_code)
    rv_boxes$selected_row <- full_row_idx
    row <- rv_boxes$data[full_row_idx, ]

    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        icon("pen-to-square", style = "color: #2c3e50;"),
        tags$span("Edit Box"),
        tags$code(row$Box_Code)
      ),
      size = "l", easyClose = TRUE,

      tags$div(
        style = "padding: 10px;",
        layout_column_wrap(
          width = 1/2,
          textInput("edit_box_project", "Project:", value = row$Project, width = "100%"),
          textInput("edit_box_name", "Box Name:", value = row$Box_Name, width = "100%")
        ),
        layout_column_wrap(
          width = 1/3,
          numericInput("edit_box_samples", "Sample Count:",
                       value = row$Sample_Count, min = 1, width = "100%"),
          dateInput("edit_box_date", "Date Received:",
                    value = as.Date(row$Received), language = "de", width = "100%"),
          selectInput("edit_box_status", "Status:",
                      choices = c("Received", 
                                  "Measured"),
                      selected = row$Status, width = "100%")
        ),
        textAreaInput("edit_box_notes", "Notes:",
                      value = row$Notes, rows = 3, width = "100%")
      ),

      footer = tags$div(
        style = "display: flex; justify-content: flex-end; gap: 10px;",
        actionButton("cancel_box_edit_btn", "Abbrechen",
                     class = "btn-secondary", icon = icon("xmark")),
        actionButton("update_box_btn", "Save",
                     class = "btn-warning", icon = icon("check"))
      )
    ))
  })

  # Update box
  observeEvent(input$update_box_btn, {
    row_idx <- rv_boxes$selected_row
    if (is.null(row_idx)) return()

    rv_boxes$data[row_idx, "Project"] <- input$edit_box_project
    rv_boxes$data[row_idx, "Box_Name"] <- input$edit_box_name
    rv_boxes$data[row_idx, "Sample_Count"] <- input$edit_box_samples
    rv_boxes$data[row_idx, "Received"] <- as.character(input$edit_box_date)
    rv_boxes$data[row_idx, "Status"] <- input$edit_box_status
    rv_boxes$data[row_idx, "Notes"] <- input$edit_box_notes

    tryCatch({
      write.csv(rv_boxes$data, box_registry_path, row.names = FALSE)
    }, error = function(e) {
      showNotification(paste0("Save Error: ", e$message), type = "error")
    })

    rv_boxes$selected_row <- NULL
    removeModal()
    showNotification("Box updated!", type = "message", duration = 3)
  })

  # Cancel box edit
  observeEvent(input$cancel_box_edit_btn, {
    rv_boxes$selected_row <- NULL
    removeModal()
  })

  # Delete box
  observeEvent(input$box_delete_btn, {
    row_idx <- input$box_registry_table_rows_selected
    if (length(row_idx) == 0) {
      showNotification("Please select a row first.", type = "warning")
      return()
    }

    filtered_df <- boxes_filtered()
    box_code <- filtered_df[row_idx, "Box_Code"]
    full_row_idx <- which(rv_boxes$data$Box_Code == box_code)

    showModal(modalDialog(
      title = tags$span(icon("triangle-exclamation"), " Confirm deletion!"),
      tags$p(paste0("Delete Box '", box_code, "'?")),
      footer = tags$div(
        actionButton("confirm_box_delete_btn", "Yes, delete", class = "btn-danger"),
        actionButton("cancel_box_delete_btn", "Cancel", class = "btn-secondary")
      ),
      easyClose = TRUE
    ))
    rv_boxes$selected_row <- full_row_idx
  })

  observeEvent(input$confirm_box_delete_btn, {
    row_idx <- rv_boxes$selected_row
    if (!is.null(row_idx)) {
      rv_boxes$data <- rv_boxes$data[-row_idx, ]
      write.csv(rv_boxes$data, box_registry_path, row.names = FALSE)
      showNotification("Box deleted.", type = "warning", duration = 3)
    }
    rv_boxes$selected_row <- NULL
    removeModal()
  })

  observeEvent(input$cancel_box_delete_btn, {
    rv_boxes$selected_row <- NULL
    removeModal()
  })

  # Save box registry
  observeEvent(input$save_box_registry_btn, {
    tryCatch({
      write.csv(rv_boxes$data, box_registry_path, row.names = FALSE)
      create_backup(type = "box_save")
      showNotification("Box-Registry saved and backup created!",
                       type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste0("Error: ", e$message), type = "error")
    })
  })

  # Download box registry
  output$download_box_registry_btn <- downloadHandler(
    filename = function() {
      paste0("box_register_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(boxes_filtered(), file, row.names = FALSE)
    }
  )

  # DASHBOARD OUTPUTS-------------------
  # KPIs
  output$kpi_total_samples <- renderText({ nrow(rv_archive$data) })

  output$kpi_total_projects <- renderText({
    length(unique(rv_archive$data$Project[rv_archive$data$Project != "Unknown"]))
  })

  output$kpi_this_year <- renderText({
    df <- rv_archive$data
    if (nrow(df) == 0) return("0")
    df$Date <- as.Date(df$Date)
    sum(format(df$Date, "%Y") == format(Sys.Date(), "%Y"), na.rm = TRUE)
  })

  output$kpi_this_month <- renderText({
    df <- rv_archive$data
    if (nrow(df) == 0) return("0")
    df$Date <- as.Date(df$Date)
    sum(format(df$Date, "%Y-%m") == format(Sys.Date(), "%Y-%m"), na.rm = TRUE)
  })

  # Update year choices
  observe({
    df <- rv_archive$data
    if (nrow(df) > 0) {
      years <- sort(unique(format(as.Date(df$Date), "%Y")), decreasing = TRUE)
      choices <- c("All" = "all", setNames(years, years))
      updateSelectInput(session, "dashboard_year_projects",
                        choices = choices, selected = format(Sys.Date(), "%Y"))
      updateSelectInput(session, "dashboard_year_monthly",
                        choices = choices, selected = format(Sys.Date(), "%Y"))
    }
  })

  # Chart: Samples per Year (stacked by type)
  output$chart_per_year <- renderPlotly({
    df <- rv_archive$data
    if (nrow(df) == 0) return(plotly_empty())

    df$Date <- as.Date(df$Date)
    df$Year <- format(df$Date, "%Y")
    if (!"Type" %in% names(df)) df$Type <- "Unknown"

    year_type_counts <- df %>%
      group_by(Year, Type) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(Year)

    colors <- c("Plasma" = "#3498db", "Urine" = "#e67e22",
                "Media" = "#9b59b6", "Unknown" = "#95a5a6",
                "Serum" = "#18bc9c", "Cells" = "#e74c3c")

    plot_ly(year_type_counts, x = ~Year, y = ~Count, color = ~Type,
            type = "bar", colors = colors,
            hovertemplate = "%{x}<br>%{fullData.name}: %{y} samples<extra></extra>") %>%
      layout(barmode = "stack",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Samples"),
             legend = list(orientation = "h", y = -0.15),
             margin = list(t = 10))
  })

  # Chart: Top 10 Projects (filtered by year)
  output$chart_per_project <- renderPlotly({
    df <- rv_archive$data
    if (nrow(df) == 0) return(plotly_empty())

    df$Date <- as.Date(df$Date)
    df$Year <- format(df$Date, "%Y")

    yr <- input$dashboard_year_projects
    if (!is.null(yr) && yr != "all") {
      df <- df %>% filter(Year == yr)
    }
    if (nrow(df) == 0) return(plotly_empty())
   
     # Anonymize in demo mode
    df$Project <- anonymize_projects(df$Project)

    proj_counts <- df %>%
      filter(Project != "Unknown") %>%
      count(Project) %>%
      arrange(desc(n)) %>%
      head(10)

    plot_ly(proj_counts, y = ~reorder(Project, n), x = ~n, type = "bar",
            orientation = "h", marker = list(color = "#3498db"),
            hovertemplate = "%{y}: %{x} samples<extra></extra>") %>%
      layout(xaxis = list(title = "Number of Samples"),
             yaxis = list(title = ""),
             margin = list(l = 150, t = 10))
  })

  # Chart: Samples per Type
  output$chart_per_type <- renderPlotly({
    df <- rv_archive$data
    if (nrow(df) == 0) return(plotly_empty())
    if (!"Type" %in% names(df)) df$Type <- "Unknown"

    type_counts <- df %>% count(Type)
    colors <- c("#18bc9c", "#3498db", "#e67e22", "#9b59b6", "#e74c3c", "#f39c12")
    

    plot_ly(type_counts, labels = ~Type, values = ~n, type = "pie",
            marker = list(colors = colors),
            textinfo = "label+percent", textposition = "inside") %>%
      layout(margin = list(t = 10), showlegend = TRUE)
  })

  # Chart: Samples per Month (filtered by year)
  output$chart_per_month <- renderPlotly({
    df <- rv_archive$data
    if (nrow(df) == 0) return(plotly_empty())

    df$Date <- as.Date(df$Date)
    df$Year <- format(df$Date, "%Y")

    yr <- input$dashboard_year_monthly
    if (is.null(yr) || yr == "all") yr <- format(Sys.Date(), "%Y")

    df_year <- df %>%
      filter(Year == yr) %>%
      mutate(Month = format(Date, "%m")) %>%
      count(Month) %>%
      arrange(Month)

    all_months <- data.frame(Month = sprintf("%02d", 1:12), stringsAsFactors = FALSE)
    df_year <- merge(all_months, df_year, by = "Month", all.x = TRUE)
    df_year$n[is.na(df_year$n)] <- 0

    month_labels <- c("Jan", "Feb", "M\u00E4r", "Apr", "Mai", "Jun",
                      "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    df_year$Month_label <- month_labels[as.numeric(df_year$Month)]
    df_year$Month_label <- factor(df_year$Month_label, levels = month_labels)

    plot_ly(df_year, x = ~Month_label, y = ~n, type = "bar",
            marker = list(color = "#e67e22"),
            hovertemplate = "%{x}: %{y} samples<extra></extra>") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Number of Samples"),
             margin = list(t = 10))
  })

  # Recent Submissions
  output$recent_submissions <- renderDT({
    df <- rv_archive$data
    if (nrow(df) == 0) return(datatable(data.frame()))

    df$Date <- as.Date(parse_dates_safe(df$Date))
    if (!"Box" %in% names(df)) df$Box <- NA_character_

    df <- df[!is.na(df$Date) & format(df$Date, "%Y") == format(Sys.Date(), "%Y"), ]
    if (nrow(df) == 0) return(datatable(data.frame(Info = "No submissions this year."), rownames = FALSE))

    # Anonymize if demo mode
    df$Project <- anonymize_projects(df$Project)

    recent <- df %>%
      group_by(Date, Project, Type, Size, Box) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Date))

    datatable(recent,
              colnames = c("Date", "Project", "Type", "Gr\u00f6\u00dfe", "Box", "Count"),
              options = list(
                scrollY = "350px", scrollCollapse = TRUE,
                paging = FALSE, searching = TRUE, info = TRUE, dom = "fti",
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json")
              ),
              rownames = FALSE)
  })
  
  

  # Backup info
  observeEvent(input$manual_backup_btn, {
    tryCatch({
      ts <- create_backup(type = "manual")
      showNotification(paste0("Backup created: ", ts), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste0("Backup error: ", e$message), type = "error")
    })
  })

  output$backup_info <- renderText({
    input$manual_backup_btn
    input$generate_btn
    input$save_projects_btn

    if (!dir.exists(backup_dir)) return("No backup folder found.")
    all_backups <- list.files(backup_dir, full.names = TRUE)
    if (length(all_backups) == 0) return("No backups yet.")

    file_info <- file.info(all_backups)
    latest <- max(file_info$mtime)
    archive_backups <- list.files(backup_dir, pattern = "^archive_")
    project_backups <- list.files(backup_dir, pattern = "^projects_")
    box_backups <- list.files(backup_dir, pattern = "^boxes_")

    paste0(
      "Backup folder: ", backup_dir, "\n",
      "Archive backups: ", length(archive_backups), "\n",
      "Project backups: ", length(project_backups), "\n",
      "Biobank backups: ", length(box_backups), "\n",
      "Last backup: ", format(latest, "%d.%m.%Y %H:%M:%S"), "\n",
      "Storage: ", round(sum(file_info$size) / 1024 / 1024, 2), " MB"
    )
  })

  
  # ARCHIV TAB OUTPUTS-----------------
  
  # Update filter choices
  observe({
    df <- rv_archive$data
    if (is.null(df) || nrow(df) == 0) return()
    
    if ("Project" %in% names(df)) {
      projects <- sort(unique(na.omit(df$Project)))
      projects <- projects[projects != "Unknown" & nchar(projects) > 0]
      updateSelectizeInput(session, "archive_project_filter", choices = projects, server = TRUE)
      updateSelectizeInput(session, "archive_plot_project", choices = projects, server = TRUE)
    }
    
    if ("Type" %in% names(df)) {
      types <- sort(unique(na.omit(df$Type)))
      updateSelectizeInput(session, "archive_type_filter",
                           choices = c("All" = "all", setNames(types, types)),
                           selected = "all")
    }
    
    if ("Box" %in% names(df)) {
      boxes <- sort(unique(na.omit(df$Box)))
      updateSelectizeInput(session, "archive_box_filter", choices = boxes)
    }
    
    if ("Date" %in% names(df)) {
      dates <- as.Date(df$Date)
      dates <- dates[!is.na(dates)]
      if (length(dates) > 0) {
        updateDateRangeInput(session, "archive_date_range",
                             start = min(dates), end = max(dates))
      }
    }
    
    # Update summary/crosstab column selectors
    config <- read_config()
    if (!is.null(config)) {
      all_cols <- intersect(config$archive$columns, names(df))
      updateSelectInput(session, "archive_summary_col", choices = all_cols)
      updateSelectInput(session, "archive_crosstab_row", choices = all_cols)
      updateSelectInput(session, "archive_crosstab_col", choices = all_cols)
    }
  })
  
  # ---- ARCHIVE: DYNAMIC CUSTOM COLUMN FILTERS ----
  output$archive_custom_filters_ui <- renderUI({
    config <- read_config()
    df <- rv_archive$data
    if (is.null(config) || is.null(df) || nrow(df) == 0) {
      return(tags$p(class = "text-muted", style = "font-size: 0.85em;",
                    "No custom columns configured."))
    }
    
    auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
    custom_cols <- setdiff(config$archive$columns, auto_cols)
    custom_cols <- intersect(custom_cols, names(df))
    
    if (length(custom_cols) == 0) {
      return(tags$p(class = "text-muted", style = "font-size: 0.85em;",
                    "No custom columns in data."))
    }
    
    filter_inputs <- lapply(custom_cols, function(col) {
      col_id <- paste0("archive_filter_custom_", gsub("[^a-zA-Z0-9]", "_", col))
      values <- unique(na.omit(as.character(df[[col]])))
      values <- values[nchar(trimws(values)) > 0]
      values <- sort(values)
      
      col_type <- "text"
      if (!is.null(config$archive$column_types) && !is.null(config$archive$column_types[[col]])) {
        ct <- config$archive$column_types[[col]]
        col_type <- if (is.list(ct)) ct$type else ct
      }
      
      if (col_type == "numeric" || all(suppressWarnings(!is.na(as.numeric(values))))) {
        num_vals <- suppressWarnings(as.numeric(values))
        num_vals <- num_vals[!is.na(num_vals)]
        if (length(num_vals) >= 2) {
          sliderInput(col_id, paste0(col, ":"),
                      min = floor(min(num_vals)), max = ceiling(max(num_vals)),
                      value = c(floor(min(num_vals)), ceiling(max(num_vals))),
                      width = "100%")
        } else {
          selectizeInput(col_id, paste0(col, ":"),
                         choices = values, multiple = TRUE,
                         options = list(placeholder = "All..."), width = "100%")
        }
      } else if (length(values) <= 30) {
        selectizeInput(col_id, paste0(col, ":"),
                       choices = values, multiple = TRUE,
                       options = list(placeholder = "All..."), width = "100%")
      } else {
        textInput(col_id, paste0(col, ":"),
                  placeholder = paste0("Search ", col, "..."), width = "100%")
      }
    })
    
    tagList(filter_inputs)
  })
  
  # ---- ARCHIVE: FILTERED DATA ----
  archive_filtered <- reactive({
    input$archive_apply_filters
    
    df <- rv_archive$data
    if (is.null(df) || nrow(df) == 0) return(df)
    
    df$Date <- as.Date(df$Date)
    if (!"Box" %in% names(df)) df$Box <- NA_character_
    if (!"Box_Code" %in% names(df)) df$Box_Code <- NA_character_
    
    # Project filter
    proj_filter <- input$archive_project_filter
    if (!is.null(proj_filter) && length(proj_filter) > 0 && "Project" %in% names(df)) {
      df <- df[df$Project %in% proj_filter, , drop = FALSE]
    }
    
    # Date filter
    if ("Date" %in% names(df) && !is.null(input$archive_date_range)) {
      date_range <- input$archive_date_range
      if (length(date_range) == 2) {
        valid <- !is.na(df$Date) & df$Date >= date_range[1] & df$Date <= date_range[2]
        df <- df[valid, , drop = FALSE]
      }
    }
    
    # Type filter
    if (!is.null(input$archive_type_filter) && input$archive_type_filter != "all" && "Type" %in% names(df)) {
      df <- df[df$Type == input$archive_type_filter, , drop = FALSE]
    }
    
    # Box filter
    box_filter <- input$archive_box_filter
    if (!is.null(box_filter) && length(box_filter) > 0 && "Box" %in% names(df)) {
      df <- df[df$Box %in% box_filter, , drop = FALSE]
    }
    
    # Custom column filters
    config <- read_config()
    if (!is.null(config)) {
      auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
      custom_cols <- setdiff(config$archive$columns, auto_cols)
      custom_cols <- intersect(custom_cols, names(df))
      
      for (col in custom_cols) {
        col_id <- paste0("archive_filter_custom_", gsub("[^a-zA-Z0-9]", "_", col))
        filter_val <- input[[col_id]]
        
        if (!is.null(filter_val) && length(filter_val) > 0) {
          if (is.numeric(filter_val) && length(filter_val) == 2) {
            num_vals <- suppressWarnings(as.numeric(df[[col]]))
            df <- df[!is.na(num_vals) & num_vals >= filter_val[1] & num_vals <= filter_val[2], , drop = FALSE]
          } else if (is.character(filter_val) && length(filter_val) == 1 &&
                     !filter_val %in% unique(na.omit(df[[col]]))) {
            if (nchar(trimws(filter_val)) > 0) {
              df <- df[grepl(filter_val, df[[col]], ignore.case = TRUE), , drop = FALSE]
            }
          } else {
            df <- df[df[[col]] %in% filter_val, , drop = FALSE]
          }
        }
      }
    }
    
    # Free text search
    search <- input$archive_search
    if (!is.null(search) && nchar(trimws(search)) > 0) {
      search <- tolower(trimws(search))
      matches <- apply(df, 1, function(row) {
        any(grepl(search, tolower(as.character(row)), fixed = TRUE))
      })
      df <- df[matches, , drop = FALSE]
    }
    
    df
  })
  
  # ---- ARCHIVE: CLEAR FILTERS ----
  observeEvent(input$archive_clear_filters, {
    updateSelectizeInput(session, "archive_project_filter", selected = character(0))
    updateSelectizeInput(session, "archive_type_filter", selected = "all")
    updateSelectizeInput(session, "archive_box_filter", selected = character(0))
    updateTextInput(session, "archive_search", value = "")
    
    df <- rv_archive$data
    if (!is.null(df) && nrow(df) > 0 && "Date" %in% names(df)) {
      dates <- as.Date(df$Date)
      dates <- dates[!is.na(dates)]
      if (length(dates) > 0) {
        updateDateRangeInput(session, "archive_date_range",
                             start = min(dates), end = max(dates))
      }
    }
    
    config <- read_config()
    if (!is.null(config)) {
      auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
      custom_cols <- setdiff(config$archive$columns, auto_cols)
      for (col in custom_cols) {
        col_id <- paste0("archive_filter_custom_", gsub("[^a-zA-Z0-9]", "_", col))
        tryCatch({
          updateSelectizeInput(session, col_id, selected = character(0))
        }, error = function(e) {
          tryCatch(updateTextInput(session, col_id, value = ""), error = function(e2) NULL)
        })
      }
    }
    
    showNotification("Filters cleared.", type = "message", duration = 2)
  })
  
  # ---- ARCHIVE: KPIs ----
  output$archive_kpi_total <- renderText({
    df <- rv_archive$data
    if (is.null(df)) return("0")
    as.character(nrow(df))
  })
  
  output$archive_kpi_projects <- renderText({
    df <- rv_archive$data
    if (is.null(df) || nrow(df) == 0 || !"Project" %in% names(df)) return("0")
    as.character(length(unique(na.omit(df$Project))))
  })
  
  output$archive_kpi_filtered <- renderText({
    df <- archive_filtered()
    if (is.null(df)) return("0")
    total <- if (is.null(rv_archive$data)) 0 else nrow(rv_archive$data)
    paste0(nrow(df), " / ", total)
  })
  
  output$archive_kpi_boxes <- renderText({
    df <- rv_archive$data
    if (is.null(df) || nrow(df) == 0 || !"Box" %in% names(df)) return("0")
    as.character(length(unique(na.omit(df$Box))))
  })
  
  # ---- ARCHIVE: FILTER SUMMARY ----
  output$archive_filter_summary <- renderUI({
    df <- archive_filtered()
    total <- if (is.null(rv_archive$data)) 0 else nrow(rv_archive$data)
    filtered <- if (is.null(df)) 0 else nrow(df)
    
    active_filters <- character(0)
    if (!is.null(input$archive_project_filter) && length(input$archive_project_filter) > 0)
      active_filters <- c(active_filters, paste0("Project: ", paste(input$archive_project_filter, collapse = ", ")))
    if (!is.null(input$archive_type_filter) && input$archive_type_filter != "all")
      active_filters <- c(active_filters, paste0("Type: ", input$archive_type_filter))
    if (!is.null(input$archive_box_filter) && length(input$archive_box_filter) > 0)
      active_filters <- c(active_filters, paste0("Box: ", paste(input$archive_box_filter, collapse = ", ")))
    if (!is.null(input$archive_search) && nchar(trimws(input$archive_search)) > 0)
      active_filters <- c(active_filters, paste0("Search: '", input$archive_search, "'"))
    
    config <- read_config()
    if (!is.null(config)) {
      auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
      custom_cols <- setdiff(config$archive$columns, auto_cols)
      for (col in custom_cols) {
        col_id <- paste0("archive_filter_custom_", gsub("[^a-zA-Z0-9]", "_", col))
        val <- input[[col_id]]
        if (!is.null(val) && length(val) > 0) {
          if (is.numeric(val) && length(val) == 2) {
            active_filters <- c(active_filters, paste0(col, ": ", val[1], " - ", val[2]))
          } else if (is.character(val) && nchar(trimws(paste(val, collapse = ""))) > 0) {
            active_filters <- c(active_filters, paste0(col, ": ", paste(val, collapse = ", ")))
          }
        }
      }
    }
    
    tags$div(
      style = "background: #eaf2f8; border-radius: 6px; padding: 10px; font-size: 0.85em;",
      tags$strong(icon("info-circle"), " Filter Summary"),
      tags$p(style = "margin: 5px 0;",
             paste0("Showing ", filtered, " of ", total, " samples")),
      if (length(active_filters) > 0) {
        tags$ul(
          style = "padding-left: 20px; margin: 5px 0;",
          lapply(active_filters, function(f) tags$li(f))
        )
      } else {
        tags$p(style = "color: #95a5a6; margin: 5px 0;", "No filters active")
      }
    )
  })
  
  # ---- ARCHIVE: TABLE ----
  output$archive_table <- renderDT({
    df <- archive_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Info = "No archive entries found. Try adjusting your filters."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    
    if (rv_demo$active) {
      if ("Name" %in% names(df)) df$Name <- paste0("Sample_", sprintf("%04d", seq_len(nrow(df))))
      df$Project <- anonymize_projects(df$Project)
      if ("Box" %in% names(df)) df$Box <- paste0("Box_", as.numeric(as.factor(df$Box)))
      if ("Box_Code" %in% names(df)) df$Box_Code <- paste0("BC_", as.numeric(as.factor(df$Box_Code)))
    }
    
    datatable(df,
              selection = "multiple",
              class = "compact stripe hover",
              rownames = FALSE,
              filter = "top",
              options = list(
                scrollX = TRUE,
                pageLength = 25,
                dom = "lfrtip",
                order = list(list(0, "desc"))
              ))
  })
  
  # ---- ARCHIVE: EDIT SELECTED ----
  observeEvent(input$archive_edit_selected, {
    rows <- input$archive_table_rows_selected
    if (is.null(rows) || length(rows) == 0) {
      showNotification("Please select rows to edit.", type = "warning", duration = 3)
      return()
    }
    
    df <- archive_filtered()
    if (is.null(df) || nrow(df) == 0) return()
    
    selected <- df[rows, , drop = FALSE]
    config <- read_config()
    auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
    custom_cols <- if (!is.null(config)) {
      intersect(setdiff(config$archive$columns, auto_cols), names(df))
    } else {
      character(0)
    }
    
    if (length(custom_cols) == 0) {
      showNotification("No custom columns to edit.", type = "info", duration = 3)
      return()
    }
    
    edit_inputs <- lapply(custom_cols, function(col) {
      current_val <- if (length(rows) == 1) {
        as.character(selected[[col]])
      } else {
        vals <- unique(na.omit(as.character(selected[[col]])))
        if (length(vals) == 1) vals else ""
      }
      if (is.na(current_val)) current_val <- ""
      
      col_type <- "text"
      col_choices <- NULL
      if (!is.null(config$archive$column_types) && !is.null(config$archive$column_types[[col]])) {
        ct <- config$archive$column_types[[col]]
        col_type <- if (is.list(ct)) ct$type else ct
        col_choices <- if (is.list(ct)) ct$choices else NULL
      }
      
      if (col_type == "select" && !is.null(col_choices)) {
        selectInput(paste0("archive_edit_", gsub("[^a-zA-Z0-9]", "_", col)),
                    col, choices = c("", col_choices),
                    selected = current_val, width = "100%")
      } else {
        textInput(paste0("archive_edit_", gsub("[^a-zA-Z0-9]", "_", col)),
                  col, value = current_val, width = "100%")
      }
    })
    
    showModal(modalDialog(
      title = tags$div(icon("pen", style = "color: #f39c12;"),
                       paste0(" Edit ", length(rows), " sample(s)")),
      size = "m", easyClose = TRUE,
      tags$div(style = "padding: 10px;", edit_inputs),
      if (length(rows) > 1) {
        tags$div(
          style = "background: #eaf2f8; padding: 8px; border-radius: 6px; font-size: 0.85em; margin-top: 10px;",
          icon("info-circle"),
          " Values will be applied to all selected rows. Leave blank to keep existing values."
        )
      },
      footer = tagList(
        modalButton("Cancel"),
        actionButton("archive_save_edit", "Save", class = "btn-warning", icon = icon("check"))
      )
    ))
  })
  
  observeEvent(input$archive_save_edit, {
    rows <- input$archive_table_rows_selected
    if (is.null(rows) || length(rows) == 0) return()
    
    df <- archive_filtered()
    if (is.null(df) || nrow(df) == 0) return()
    
    config <- read_config()
    auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
    custom_cols <- if (!is.null(config)) {
      intersect(setdiff(config$archive$columns, auto_cols), names(df))
    } else {
      character(0)
    }
    
    selected_names <- as.character(df[rows, "Name"])
    selected_dates <- as.character(df[rows, "Date"])
    
    for (col in custom_cols) {
      input_id <- paste0("archive_edit_", gsub("[^a-zA-Z0-9]", "_", col))
      new_val <- input[[input_id]]
      if (!is.null(new_val) && nchar(trimws(new_val)) > 0) {
        for (i in seq_along(rows)) {
          full_match <- which(
            as.character(rv_archive$data$Name) == selected_names[i] &
              as.character(rv_archive$data$Date) == selected_dates[i]
          )
          if (length(full_match) > 0) {
            rv_archive$data[full_match[1], col] <- new_val
          }
        }
      }
    }
    
    tryCatch({
      write.csv(rv_archive$data, archive_path, row.names = FALSE)
      showNotification(paste0("Updated ", length(rows), " row(s)."), type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste0("Save error: ", e$message), type = "error")
    })
    
    removeModal()
  })
  
  # ---- ARCHIVE: DELETE SELECTED ----
  observeEvent(input$archive_delete_selected, {
    rows <- input$archive_table_rows_selected
    if (is.null(rows) || length(rows) == 0) {
      showNotification("Please select rows to delete.", type = "warning", duration = 3)
      return()
    }
    
    showModal(modalDialog(
      title = tags$div(icon("triangle-exclamation", style = "color: #e74c3c;"), " Confirm Delete"),
      tags$p(paste0("Delete ", length(rows), " selected sample(s) from the archive?")),
      tags$p(class = "text-muted", "This cannot be undone."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("archive_confirm_delete", "Delete", class = "btn-danger", icon = icon("trash"))
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$archive_confirm_delete, {
    rows <- input$archive_table_rows_selected
    if (is.null(rows) || length(rows) == 0) return()
    
    df <- archive_filtered()
    if (is.null(df) || nrow(df) == 0) return()
    
    selected_names <- as.character(df[rows, "Name"])
    selected_dates <- as.character(df[rows, "Date"])
    
    rows_to_remove <- integer(0)
    for (i in seq_along(rows)) {
      full_match <- which(
        as.character(rv_archive$data$Name) == selected_names[i] &
          as.character(rv_archive$data$Date) == selected_dates[i]
      )
      rows_to_remove <- c(rows_to_remove, full_match)
    }
    
    rows_to_remove <- unique(rows_to_remove)
    if (length(rows_to_remove) > 0) {
      rv_archive$data <- rv_archive$data[-rows_to_remove, , drop = FALSE]
      tryCatch({
        write.csv(rv_archive$data, archive_path, row.names = FALSE)
        showNotification(paste0("Deleted ", length(rows_to_remove), " row(s)."), type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste0("Save error: ", e$message), type = "error")
      })
    }
    
    removeModal()
  })
  
  # ---- ARCHIVE: SEND TO NMR COPY ----
  observeEvent(input$archive_send_to_nmr, {
    rows <- input$archive_table_rows_selected
    df <- archive_filtered()
    
    # If no rows selected, use entire filtered set
    if (is.null(rows) || length(rows) == 0) {
      if (is.null(df) || nrow(df) == 0) {
        showNotification("No samples to send. Adjust your filters.", type = "warning", duration = 4)
        return()
      }
      selected_df <- df
      selection_type <- "filtered"
    } else {
      selected_df <- df[rows, , drop = FALSE]
      selection_type <- "selected"
    }
    
    # Get unique sample names
    sample_names <- unique(as.character(selected_df$Name))
    sample_names <- sample_names[!is.na(sample_names) & nchar(trimws(sample_names)) > 0]
    
    if (length(sample_names) == 0) {
      showNotification("No valid sample names found.", type = "error", duration = 4)
      return()
    }
    
    # Get project info
    projects <- if ("Project" %in% names(selected_df)) {
      unique(na.omit(selected_df$Project))
    } else {
      character(0)
    }
    
    # Get types
    types <- if ("Type" %in% names(selected_df)) {
      unique(na.omit(selected_df$Type))
    } else {
      character(0)
    }
    
    # Get date range
    date_range <- if ("Date" %in% names(selected_df)) {
      dates <- as.Date(selected_df$Date)
      dates <- dates[!is.na(dates)]
      if (length(dates) > 0) paste0(min(dates), " to ", max(dates)) else "Unknown"
    } else {
      "Unknown"
    }
    
    # Show confirmation modal
    showModal(modalDialog(
      title = tags$div(
        icon("share-from-square", style = "color: #17a2b8;"),
        " Send Samples to NMR Copy Module"
      ),
      size = "m",
      
      tags$div(
        style = "padding: 10px;",
        
        # Summary card
        tags$div(
          style = "background: #eaf2f8; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
          tags$h6(icon("info-circle"), " Transfer Summary"),
          tags$table(
            style = "width: 100%; font-size: 0.9em;",
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Source:"),
              tags$td(style = "padding: 4px 8px;", 
                      if (selection_type == "selected") 
                        paste0(length(rows), " selected rows") 
                      else 
                        "All filtered results")
            ),
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Samples:"),
              tags$td(style = "padding: 4px 8px;", paste0(length(sample_names), " unique samples"))
            ),
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Project(s):"),
              tags$td(style = "padding: 4px 8px;", 
                      if (length(projects) > 0) paste(projects, collapse = ", ") else "None")
            ),
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Type(s):"),
              tags$td(style = "padding: 4px 8px;",
                      if (length(types) > 0) paste(types, collapse = ", ") else "Unknown")
            ),
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Date Range:"),
              tags$td(style = "padding: 4px 8px;", date_range)
            )
          )
        ),
        
        # Sample preview
        tags$div(
          style = "max-height: 200px; overflow-y: auto; background: #f8f9fa; border-radius: 6px; padding: 10px; font-family: monospace; font-size: 0.85em;",
          tags$strong("Sample Names:"),
          tags$div(
            style = "margin-top: 5px;",
            paste(head(sample_names, 50), collapse = "\n"),
            if (length(sample_names) > 50) paste0("\n... and ", length(sample_names) - 50, " more")
          )
        ),
        
        # Options
        tags$div(
          style = "margin-top: 15px;",
          radioButtons("nmr_transfer_mode", "Transfer Mode:",
                       choices = c(
                         "Send sample names only" = "names_only",
                         "Send with project filter" = "with_project"
                       ),
                       selected = "with_project", inline = TRUE)
        ),
        
        if (length(projects) > 1) {
          tags$div(
            style = "margin-top: 10px;",
            selectInput("nmr_transfer_project", "Select Project for NMR:",
                        choices = projects, selected = projects[1], width = "100%")
          )
        }
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_send_to_nmr", "Send to NMR Copy",
                     class = "btn-info", icon = icon("share-from-square"))
      ),
      easyClose = TRUE
    ))
  })
  
  # Confirm and transfer
  observeEvent(input$confirm_send_to_nmr, {
    rows <- input$archive_table_rows_selected
    df <- archive_filtered()
    
    if (is.null(rows) || length(rows) == 0) {
      selected_df <- df
    } else {
      selected_df <- df[rows, , drop = FALSE]
    }
    
    sample_names <- unique(as.character(selected_df$Name))
    sample_names <- sample_names[!is.na(sample_names) & nchar(trimws(sample_names)) > 0]
    
    # Get project
    project <- NULL
    if (input$nmr_transfer_mode == "with_project") {
      if (!is.null(input$nmr_transfer_project)) {
        project <- input$nmr_transfer_project
      } else if ("Project" %in% names(selected_df)) {
        projects <- unique(na.omit(selected_df$Project))
        if (length(projects) == 1) project <- projects
      }
    }
    
    # Store in transfer reactive
    rv_nmr_transfer$samples <- sample_names
    rv_nmr_transfer$project <- project
    rv_nmr_transfer$source <- "archive"
    rv_nmr_transfer$triggered <- !rv_nmr_transfer$triggered  # Toggle to trigger observer
    
    removeModal()
    
    # Switch to NMR Copy tab
    nav_select("main_nav", selected = "NMR Copy", session = session)
    
    showNotification(
      paste0(length(sample_names), " samples sent to NMR Copy module",
             if (!is.null(project)) paste0(" (Project: ", project, ")") else ""),
      type = "message", duration = 5
    )
  })
  
  
  
  
  # ---- ARCHIVE: DOWNLOAD ----
  output$download_archive_btn <- downloadHandler(
    filename = function() {
      paste0("Archive_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      df <- archive_filtered()
      if (is.null(df) || nrow(df) == 0) df <- data.frame(Info = "No data")
      wb <- createWorkbook()
      addWorksheet(wb, "Archive")
      writeData(wb, "Archive", df)
      headerStyle <- createStyle(textDecoration = "bold", fgFill = "#2c3e50",
                                 fontColour = "#ffffff", halign = "center")
      addStyle(wb, "Archive", headerStyle, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
      setColWidths(wb, "Archive", cols = 1:ncol(df), widths = "auto")
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- ARCHIVE: CHARTS ----
  output$archive_detail_title <- renderText({
    proj <- input$archive_plot_project
    if (is.null(proj) || length(proj) == 0 || nchar(trimws(proj)) == 0) {
      "Project Detail (select a project)"
    } else {
      display_name <- anonymize_projects(proj)
      paste0("Detail: ", display_name)
    }
  })
  
  output$archive_timeline <- renderPlotly({
    df <- archive_filtered()
    if (is.null(df) || nrow(df) == 0 || !"Date" %in% names(df)) {
      return(plotly_empty() %>% layout(title = "No data"))
    }
    
    df$Date <- as.Date(df$Date)
    df <- df[!is.na(df$Date), ]
    if (nrow(df) == 0) return(plotly_empty())
    
    df$Project <- anonymize_projects(df$Project)
    
    df$Month <- format(df$Date, "%Y-%m")
    monthly <- as.data.frame(table(Month = df$Month), stringsAsFactors = FALSE)
    names(monthly) <- c("Month", "Count")
    monthly <- monthly[order(monthly$Month), ]
    monthly$Cumulative <- cumsum(monthly$Count)
    
    plot_ly(monthly) %>%
      add_bars(x = ~Month, y = ~Count, name = "Monthly",
               marker = list(color = "#3498db", opacity = 0.7)) %>%
      add_lines(x = ~Month, y = ~Cumulative, name = "Cumulative",
                yaxis = "y2", line = list(color = "#e74c3c", width = 2)) %>%
      layout(
        yaxis = list(title = "Samples / Month"),
        yaxis2 = list(title = "Cumulative", overlaying = "y", side = "right"),
        xaxis = list(title = ""),
        legend = list(orientation = "h", y = -0.2),
        margin = list(r = 60)
      )
  })
  
  output$archive_project_bars <- renderPlotly({
    df <- archive_filtered()
    if (is.null(df) || nrow(df) == 0 || !"Project" %in% names(df)) {
      return(plotly_empty() %>% layout(title = "No data"))
    }
    
    df$Project <- anonymize_projects(df$Project)
    
    proj_counts <- as.data.frame(table(Project = df$Project), stringsAsFactors = FALSE)
    names(proj_counts) <- c("Project", "Count")
    proj_counts <- proj_counts[order(-proj_counts$Count), ]
    proj_counts <- head(proj_counts, 15)
    
    plot_ly(proj_counts, y = ~reorder(Project, Count), x = ~Count, type = "bar",
            marker = list(color = "#18bc9c"), orientation = "h") %>%
      layout(xaxis = list(title = "Samples"), yaxis = list(title = ""),
             margin = list(l = 150))
  })
  
  output$archive_project_timeline <- renderPlotly({
    proj <- input$archive_plot_project
    df <- archive_filtered()
    
    if (is.null(proj) || length(proj) == 0 || nchar(trimws(proj)) == 0 ||
        is.null(df) || nrow(df) == 0 || !"Project" %in% names(df)) {
      return(plotly_empty() %>% layout(
        annotations = list(text = "Select a project in the sidebar",
                           showarrow = FALSE, font = list(size = 14, color = "#95a5a6"))
      ))
    }
    
    proj_df <- df[df$Project == proj, , drop = FALSE]
    if (nrow(proj_df) == 0) return(plotly_empty())
    
    proj_df$Date <- as.Date(proj_df$Date)
    proj_df <- proj_df[!is.na(proj_df$Date), ]
    
    proj_df$Month <- format(proj_df$Date, "%Y-%m")
    monthly <- as.data.frame(table(Month = proj_df$Month), stringsAsFactors = FALSE)
    names(monthly) <- c("Month", "Count")
    monthly <- monthly[order(monthly$Month), ]
    
    display_name <- anonymize_projects(proj)
    
    plot_ly(monthly, x = ~Month, y = ~Count, type = "bar",
            marker = list(color = "#3498db")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = "Samples"),
             title = list(text = display_name, font = list(size = 13)))
  })
  
  # Project boxes pie
  output$archive_project_boxes <- renderPlotly({
    proj <- input$archive_plot_project
    df <- archive_filtered()
    
    if (is.null(proj) || length(proj) == 0 || is.null(df) || nrow(df) == 0 ||
        !"Project" %in% names(df) || !"Box" %in% names(df)) {
      return(plotly_empty())
    }
    
    proj_df <- df[df$Project == proj, , drop = FALSE]
    if (nrow(proj_df) == 0) return(plotly_empty())
    
    box_counts <- as.data.frame(table(Box = proj_df$Box), stringsAsFactors = FALSE)
    names(box_counts) <- c("Box", "Count")
    box_counts <- box_counts[order(-box_counts$Count), ]
    
    if (rv_demo$active) {
      box_counts$Box <- paste0("Box_", seq_len(nrow(box_counts)))
    }
    
    plot_ly(box_counts, labels = ~Box, values = ~Count, type = "pie",
            textinfo = "label+value",
            marker = list(colors = c("#3498db", "#18bc9c", "#e67e22", "#9b59b6", "#e74c3c"))) %>%
      layout(showlegend = FALSE)
  })
  
  # Gantt chart
  output$archive_gantt <- renderPlotly({
    df <- archive_filtered()
    if (is.null(df) || nrow(df) == 0 || !"Project" %in% names(df) || !"Date" %in% names(df)) {
      return(plotly_empty() %>% layout(title = "No data"))
    }
    
    df$Date <- as.Date(df$Date)
    df <- df[!is.na(df$Date), ]
    if (nrow(df) == 0) return(plotly_empty())
    
    df$Project <- anonymize_projects(df$Project)
    
    gantt_data <- do.call(rbind, lapply(split(df, df$Project), function(proj_df) {
      data.frame(
        Project = proj_df$Project[1],
        Start = min(proj_df$Date),
        End = max(proj_df$Date),
        Samples = nrow(proj_df),
        stringsAsFactors = FALSE
      )
    }))
    
    gantt_data <- gantt_data[order(gantt_data$Start), ]
    colors <- c("#3498db", "#18bc9c", "#e67e22", "#9b59b6", "#e74c3c", "#f39c12",
                "#1abc9c", "#2ecc71", "#e74c3c", "#34495e")
    
    p <- plot_ly()
    for (i in seq_len(nrow(gantt_data))) {
      p <- p %>% add_segments(
        x = gantt_data$Start[i], xend = gantt_data$End[i],
        y = gantt_data$Project[i], yend = gantt_data$Project[i],
        line = list(color = colors[(i - 1) %% length(colors) + 1], width = 15),
        showlegend = FALSE,
        text = paste0(gantt_data$Project[i], ": ", gantt_data$Samples[i], " samples\n",
                      gantt_data$Start[i], " to ", gantt_data$End[i]),
        hoverinfo = "text"
      )
    }
    
    p %>% layout(
      xaxis = list(title = ""),
      yaxis = list(title = "", categoryorder = "array",
                   categoryarray = rev(gantt_data$Project)),
      margin = list(l = 150)
    )
  })
  
  # ---- ARCHIVE: CUSTOM COLUMN DISTRIBUTION ----
  output$archive_plot_custom <- renderPlotly({
    col <- input$archive_summary_col
    df <- archive_filtered()
    
    if (is.null(col) || length(col) == 0 || nchar(trimws(col)) == 0 ||
        is.null(df) || nrow(df) == 0 || !col %in% names(df)) {
      return(plotly_empty() %>% layout(
        annotations = list(text = "Select a column above",
                           showarrow = FALSE, font = list(size = 14, color = "#95a5a6"))
      ))
    }
    
    values <- na.omit(as.character(df[[col]]))
    values <- values[nchar(trimws(values)) > 0]
    
    if (length(values) == 0) {
      return(plotly_empty() %>% layout(title = "No data for this column"))
    }
    
    num_vals <- suppressWarnings(as.numeric(values))
    if (sum(!is.na(num_vals)) > length(values) * 0.8) {
      plot_ly(x = num_vals[!is.na(num_vals)], type = "histogram",
              marker = list(color = "#3498db", line = list(color = "#2c3e50", width = 1))) %>%
        layout(xaxis = list(title = col), yaxis = list(title = "Count"))
    } else {
      val_counts <- as.data.frame(table(Value = values), stringsAsFactors = FALSE)
      names(val_counts) <- c("Value", "Count")
      val_counts <- val_counts[order(-val_counts$Count), ]
      if (nrow(val_counts) > 20) val_counts <- val_counts[1:20, ]
      
      plot_ly(val_counts, x = ~reorder(Value, -Count), y = ~Count, type = "bar",
              marker = list(color = "#18bc9c")) %>%
        layout(xaxis = list(title = col, tickangle = -45), yaxis = list(title = "Count"))
    }
  })
  
  # ---- ARCHIVE: CROSS-TABULATION ----
  output$archive_crosstab_table <- renderDT({
    row_col <- input$archive_crosstab_row
    col_col <- input$archive_crosstab_col
    df <- archive_filtered()
    
    if (is.null(row_col) || is.null(col_col) || is.null(df) || nrow(df) == 0 ||
        length(row_col) == 0 || length(col_col) == 0 ||
        !row_col %in% names(df) || !col_col %in% names(df)) {
      return(datatable(data.frame(Info = "Select row and column variables."),
                       rownames = FALSE, options = list(dom = "t")))
    }
    
    ct <- as.data.frame.matrix(table(df[[row_col]], df[[col_col]]))
    ct <- cbind(data.frame(Row = rownames(ct), stringsAsFactors = FALSE), ct)
    names(ct)[1] <- row_col
    
    datatable(ct, rownames = FALSE, class = "compact stripe",
              options = list(scrollX = TRUE, dom = "t", pageLength = 50))
  })
  
  observeEvent(input$box_measured_btn, {
    req(rv_boxes$selected_row)
    row_idx <- rv_boxes$selected_row

    # Update box status
    rv_boxes$data$Status[row_idx] <- "Measured"
    rv_boxes$data$Measured_Date[row_idx] <- format(Sys.Date(), "%d.%m.%Y")

    # Save box registry
    fwrite(rv_boxes$data, box_registry_path)

    # The projects table will auto-update because it reads from rv_boxes$data reactively
    showNotification(
      paste0("Box '", rv_boxes$data$Box_Name[row_idx], "' als gemessen markiert."),
      type = "message", duration = 3
    )
  })

  # PROJEKTE TAB OUTPUTS--------------------
  # Filter by type
  observe({
    df <- rv_projects$data
    if (nrow(df) > 0 && "Type of Sample" %in% names(df)) {
      types <- sort(unique(df$`Type of Sample`[!is.na(df$`Type of Sample`)]))
      updateSelectizeInput(session, "proj_filter_type",
                           choices = c("All" = "all", setNames(types, types)),
                           selected = "all")
    }
  })

  # Filtered projects
  projects_filtered <- reactive({
    df <- rv_projects$data
    if (is.null(df) || nrow(df) == 0) return(df)
    
    # Filter by type
    if (!is.null(input$proj_filter_type) && input$proj_filter_type != "all") {
      type_col <- intersect(c("Type of Sample", "Type.of.Sample"), names(df))
      if (length(type_col) > 0) {
        df <- df[grepl(input$proj_filter_type, df[[type_col[1]]], ignore.case = TRUE), , drop = FALSE]
      }
    }
    
    # Filter by search
    if (!is.null(input$proj_filter_search) && nchar(trimws(input$proj_filter_search)) > 0) {
      search <- tolower(trimws(input$proj_filter_search))
      matches <- apply(df, 1, function(row) {
        any(grepl(search, tolower(as.character(row)), fixed = TRUE))
      })
      df <- df[matches, , drop = FALSE]
    }
    
    df
  })
  

  # Project KPIs
  output$proj_kpi_total <- renderText({
    if (is.null(rv_projects$data)) return("0")
    as.character(nrow(rv_projects$data))
  })
  
  output$proj_kpi_active <- renderText({
    df <- rv_projects$data
    if (is.null(df) || nrow(df) == 0) return("0")
    
    measured_col <- intersect(c("Measured", "measured"), names(df))
    boxes_col <- intersect(c("Boxes", "boxes"), names(df))
    
    if (length(measured_col) > 0 && length(boxes_col) > 0) {
      m <- suppressWarnings(as.numeric(df[[measured_col[1]]]))
      b <- suppressWarnings(as.numeric(df[[boxes_col[1]]]))
      active <- sum(!is.na(m) & !is.na(b) & m < b, na.rm = TRUE)
      as.character(active)
    } else {
      as.character(nrow(df))
    }
  })
  
  output$proj_kpi_types <- renderText({
    df <- rv_projects$data
    if (is.null(df) || nrow(df) == 0) return("0")
    
    type_col <- intersect(c("Type of Sample", "Type.of.Sample"), names(df))
    if (length(type_col) > 0) {
      types <- unique(unlist(strsplit(as.character(df[[type_col[1]]]), ";")))
      types <- types[!is.na(types) & nchar(trimws(types)) > 0]
      as.character(length(types))
    } else {
      "0"
    }
  })
  
  output$projects_table <- renderDT({
    df <- projects_filtered()
    
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(
        data.frame(Info = "No projects yet. Add one in the 'New Project' tab."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    # Anonymize in demo mode
    if (rv_demo$active) {
      if ("Title" %in% names(df)) df$Title <- paste0("Project_", seq_len(nrow(df)))
      if ("Abbreviation" %in% names(df)) df$Abbreviation <- paste0("PRJ_", sprintf("%03d", seq_len(nrow(df))))
      if ("Name" %in% names(df)) df$Name <- paste0("Person_", seq_len(nrow(df)))
      if ("PI" %in% names(df)) df$PI <- paste0("Person_", seq_len(nrow(df)))
      if ("Email" %in% names(df)) df$Email <- paste0("user", seq_len(nrow(df)), "@example.com")
    }
    
    datatable(df,
              selection = "single",
              class = "compact stripe hover",
              rownames = FALSE,
              options = list(
                scrollX = TRUE,
                pageLength = 25,
                dom = "lfrtip"
              ))
  })
  
  

  # Edit project (button click)
  observeEvent(input$edit_project_btn, {
    row_idx <- input$projects_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) {
      showNotification("Please select a project to edit.", type = "warning", duration = 3)
      return()
    }
    
    df <- projects_filtered()
    if (is.null(df) || nrow(df) == 0 || row_idx > nrow(df)) return()
    
    rv_projects$selected_row <- row_idx
    row <- df[row_idx, ]
    
    # Helper to safely get column value
    get_col <- function(row, ...) {
      cols <- c(...)
      for (col in cols) {
        if (col %in% names(row) && !is.na(row[[col]])) return(as.character(row[[col]]))
      }
      return("")
    }
    
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        icon("pen-to-square", style = "color: #2c3e50;"),
        tags$span("Edit Project")
      ),
      size = "l", easyClose = TRUE,
      
      tags$div(
        style = "padding: 10px;",
        layout_column_wrap(
          width = 1/2,
          textInput("edit_title", "Title:", 
                    value = get_col(row, "Title"), width = "100%"),
          textInput("edit_abbrev", "Abbreviation:", 
                    value = get_col(row, "Abbreviation"), width = "100%")
        ),
        tags$hr(style = "margin: 15px 0;"),
        tags$h6(style = "color: #7f8c8d; margin-bottom: 10px;",
                icon("user", style = "margin-right: 5px;"), "Contact Details"),
        layout_column_wrap(
          width = 1/2,
          textInput("edit_name", "Contact Person:", 
                    value = get_col(row, "Name", "PI", "Contact"), width = "100%"),
          textInput("edit_email", "Email:", 
                    value = get_col(row, "Email"), width = "100%")
        ),
        layout_column_wrap(
          width = 1/2,
          textInput("edit_group", "Group / AG:", 
                    value = get_col(row, "Group / AG", "AG"), width = "100%"),
          textInput("edit_contact", "Clinic/Institute:", 
                    value = get_col(row, "Contact Clinic/Institute", "Clinic_Institute"), width = "100%")
        ),
        tags$hr(style = "margin: 15px 0;"),
        tags$h6(style = "color: #7f8c8d; margin-bottom: 10px;",
                icon("vial", style = "margin-right: 5px;"), "Sample Information"),
        layout_column_wrap(
          width = 1/4,
          selectInput("edit_sample_type", "Sample Type:",
                      choices = c("Plasma", "Serum", "Urine", "Media",
                                  "Plasma;Urine", "Plasma;Serum",
                                  "Plasma;Media;Extracts", "Media;Extracts",
                                  "Cells", "Tissue", "Other"),
                      selected = get_col(row, "Type of Sample"), width = "100%"),
          textInput("edit_n_samples", "Number of Samples:",
                    value = get_col(row, "Number of Samples"), width = "100%"),
          textInput("edit_boxes", "Boxes:", 
                    value = get_col(row, "Boxes"), width = "100%"),
          textInput("edit_measured", "Measured:", 
                    value = get_col(row, "Measured"), width = "100%")
        )
      ),
      
      footer = tags$div(
        style = "display: flex; justify-content: flex-end; gap: 10px;",
        actionButton("cancel_edit_btn", "Cancel",
                     class = "btn-secondary", icon = icon("xmark")),
        actionButton("update_project_btn", "Save Changes",
                     class = "btn-warning", icon = icon("check"))
      )
    ))
  })
  
  # Update project
  observeEvent(input$update_project_btn, {
    row_idx <- rv_projects$selected_row
    if (is.null(row_idx)) return()
    
    df <- projects_filtered()
    if (is.null(df) || nrow(df) == 0 || row_idx > nrow(df)) {
      removeModal()
      return()
    }
    
    filtered_row <- df[row_idx, ]
    full_idx <- which(rv_projects$data$Abbreviation == filtered_row$Abbreviation)
    if (length(full_idx) == 0) full_idx <- row_idx
    if (full_idx > nrow(rv_projects$data)) {
      removeModal()
      return()
    }
    
    # Helper to set column if it exists
    set_col <- function(col, value) {
      if (col %in% names(rv_projects$data)) {
        rv_projects$data[full_idx, col] <<- value
      }
    }
    
    set_col("Title", input$edit_title)
    set_col("Abbreviation", input$edit_abbrev)
    set_col("Name", input$edit_name)
    set_col("PI", input$edit_name)
    set_col("Email", input$edit_email)
    set_col("Group / AG", input$edit_group)
    set_col("AG", input$edit_group)
    set_col("Contact Clinic/Institute", input$edit_contact)
    set_col("Clinic_Institute", input$edit_contact)
    set_col("Contact", input$edit_contact)
    set_col("Type of Sample", input$edit_sample_type)
    set_col("Number of Samples", input$edit_n_samples)
    set_col("Boxes", input$edit_boxes)
    set_col("Measured", input$edit_measured)
    
    rv_projects$selected_row <- NULL
    removeModal()
    showNotification("Project updated!", type = "message", duration = 3)
  })
  
  # Cancel edit
  observeEvent(input$cancel_edit_btn, {
    rv_projects$selected_row <- NULL
    removeModal()
  })
  
  # Delete project
  # Delete project (with confirmation)
  # Delete project (with confirmation)
  observeEvent(input$delete_project_btn, {
    row_idx <- input$projects_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) {
      showNotification("Please select a project to delete.", type = "warning", duration = 3)
      return()
    }
    
    df <- projects_filtered()
    if (is.null(df) || nrow(df) == 0 || row_idx > nrow(df)) {
      showNotification("No valid project selected.", type = "warning", duration = 3)
      return()
    }
    
    proj_name <- df$Abbreviation[row_idx]
    proj_title <- if ("Title" %in% names(df)) df$Title[row_idx] else proj_name
    
    config <- read_config()
    biobank_enabled <- isTRUE(config$modules$boxes)
    
    n_archive <- 0
    n_boxes <- 0
    if ("Project" %in% names(rv_archive$data)) {
      n_archive <- sum(rv_archive$data$Project == proj_name, na.rm = TRUE)
    }
    if (biobank_enabled && !is.null(rv_boxes$data) && nrow(rv_boxes$data) > 0 && "Project" %in% names(rv_boxes$data)) {
      n_boxes <- sum(rv_boxes$data$Project == proj_name, na.rm = TRUE)
    }
    
    showModal(modalDialog(
      title = tags$div(
        icon("triangle-exclamation", style = "color: #e74c3c;"),
        " Confirm Deletion"
      ),
      size = "m", easyClose = TRUE,
      
      tags$div(
        style = "padding: 10px;",
        tags$div(
          style = "background: #fdf0ed; border: 1px solid #f5c6cb; border-radius: 8px; padding: 15px; margin-bottom: 15px;",
          tags$h5(style = "color: #e74c3c; margin-bottom: 10px;",
                  icon("exclamation-circle"), " Are you sure?"),
          tags$p("You are about to delete the following project:"),
          tags$table(
            style = "width: 100%; font-size: 0.95em; margin-top: 10px;",
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600; width: 120px;", "Title:"),
              tags$td(style = "padding: 4px 8px;", proj_title)
            ),
            tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Abbreviation:"),
              tags$td(style = "padding: 4px 8px;", tags$code(proj_name))
            ),
            if (n_archive > 0) tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Archive entries:"),
              tags$td(style = "padding: 4px 8px; color: #e67e22;", 
                      paste0(n_archive, " samples linked"))
            ),
            if (n_boxes > 0) tags$tr(
              tags$td(style = "padding: 4px 8px; font-weight: 600;", "Biobank boxes:"),
              tags$td(style = "padding: 4px 8px; color: #e67e22;",
                      paste0(n_boxes, " boxes registered"))
            )
          )
        ),
        
        if (n_archive > 0 || n_boxes > 0) {
          tags$div(
            style = "background: #fff3cd; border-radius: 6px; padding: 10px; margin-bottom: 10px; font-size: 0.85em;",
            icon("triangle-exclamation", style = "color: #856404;"),
            " Note: This only removes the project from the list. Archive entries and biobank boxes will NOT be deleted."
          )
        },
        
        tags$p(style = "color: #6c757d; font-size: 0.85em; margin-top: 10px;",
               icon("info-circle"), " This action cannot be undone.")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_project", "Delete Project",
                     class = "btn-danger", icon = icon("trash"))
      )
    ))
  })
  
  # Confirm project deletion
  observeEvent(input$confirm_delete_project, {
    row_idx <- input$projects_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) {
      removeModal()
      return()
    }
    
    df <- projects_filtered()
    if (is.null(df) || nrow(df) == 0 || row_idx > nrow(df)) {
      removeModal()
      return()
    }
    
    filtered_row <- df[row_idx, ]
    if (!"Abbreviation" %in% names(filtered_row)) {
      showNotification("Cannot identify project.", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    full_idx <- which(rv_projects$data$Abbreviation == filtered_row$Abbreviation)
    if (length(full_idx) == 0) {
      showNotification("Project not found.", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    deleted_name <- rv_projects$data[full_idx, "Abbreviation"]
    rv_projects$data <- rv_projects$data[-full_idx, , drop = FALSE]
    rv_projects$selected_row <- NULL
    
    tryCatch({
      write.csv(rv_projects$data, projects_csv_path, row.names = FALSE)
    }, error = function(e) NULL)
    
    removeModal()
    showNotification(paste0("Project '", deleted_name, "' deleted."),
                     type = "warning", duration = 4)
  })
  
  # Confirm project deletion
  observeEvent(input$confirm_delete_project, {
    row_idx <- input$projects_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) {
      removeModal()
      return()
    }
    
    df <- projects_filtered()
    if (is.null(df) || nrow(df) == 0 || row_idx > nrow(df)) {
      removeModal()
      return()
    }
    
    filtered_row <- df[row_idx, ]
    
    if (!"Abbreviation" %in% names(filtered_row)) {
      showNotification("Cannot identify project.", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    full_idx <- which(rv_projects$data$Abbreviation == filtered_row$Abbreviation)
    if (length(full_idx) == 0) {
      showNotification("Project not found in data.", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    deleted_name <- rv_projects$data[full_idx, "Abbreviation"]
    rv_projects$data <- rv_projects$data[-full_idx, , drop = FALSE]
    rv_projects$selected_row <- NULL
    
    # Save immediately
    tryCatch({
      write.csv(rv_projects$data, projects_csv_path, row.names = FALSE)
    }, error = function(e) NULL)
    
    removeModal()
    showNotification(paste0("Project '", deleted_name, "' deleted."),
                     type = "warning", duration = 4)
  })
  
  # Add new project
  observeEvent(input$add_project_btn, {
    if (nchar(trimws(input$proj_title)) == 0 || nchar(trimws(input$proj_abbrev)) == 0) {
      showNotification("Please enter at least Title and Abbreviation.", type = "error", duration = 4)
      return()
    }
    
    # Check for duplicate abbreviation
    if (nrow(rv_projects$data) > 0 && 
        "Abbreviation" %in% names(rv_projects$data) &&
        input$proj_abbrev %in% rv_projects$data$Abbreviation) {
      showNotification("This abbreviation already exists!", type = "error", duration = 4)
      return()
    }
    
    config <- read_config()
    proj_cols <- if (!is.null(config)) config$projects$columns else c("Title", "Abbreviation")
    biobank_enabled <- isTRUE(config$modules$boxes)
    
    # Build new row
    input_map <- list(
      "Title" = trimws(input$proj_title),
      "Abbreviation" = trimws(input$proj_abbrev),
      "PI" = input$proj_name,
      "Email" = input$proj_email,
      "AG" = input$proj_group,
      "Clinic_Institute" = input$proj_contact,
      "Contact" = input$proj_contact,
      "Type of Sample" = input$proj_sample_type,
      "Number of Samples" = input$proj_n_samples,
      "Boxes" = input$proj_boxes,
      "Measured" = "0",
      "Name" = input$proj_name,
      "Group / AG" = input$proj_group,
      "Contact Clinic/Institute" = input$proj_contact
    )
    
    new_row <- as.data.frame(
      lapply(proj_cols, function(col) {
        val <- input_map[[col]]
        if (!is.null(val) && nchar(trimws(as.character(val))) > 0) {
          as.character(val)
        } else {
          NA_character_
        }
      }),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(new_row) <- proj_cols
    
    if (nrow(rv_projects$data) == 0) {
      rv_projects$data <- new_row
    } else {
      for (col in names(new_row)) {
        if (!col %in% names(rv_projects$data)) rv_projects$data[[col]] <- NA_character_
      }
      for (col in names(rv_projects$data)) {
        if (!col %in% names(new_row)) new_row[[col]] <- NA_character_
      }
      new_row <- new_row[, names(rv_projects$data), drop = FALSE]
      rv_projects$data <- rbind(rv_projects$data, new_row)
    }
    
    # Save project
    tryCatch({
      write.csv(rv_projects$data, projects_csv_path, row.names = FALSE)
    }, error = function(e) {
      showNotification(paste0("Save error: ", e$message), type = "error", duration = 5)
    })
    
    abbr <- trimws(input$proj_abbrev)
    n_boxes_created <- 0
    
    # ---- Show box registration modal if biobank is enabled ----
    if (biobank_enabled) {
      showModal(modalDialog(
        title = tags$div(
          icon("check-circle", style = "color: #18bc9c;"),
          paste0(" Project '", abbr, "' Created!")
        ),
        size = "m", easyClose = TRUE,
        
        tags$div(
          style = "padding: 10px;",
          tags$div(
            style = "background: #d4edda; border-radius: 6px; padding: 12px; margin-bottom: 15px;",
            icon("check", style = "color: #18bc9c;"),
            paste0(" Project '", abbr, "' has been saved successfully.")
          ),
          
          tags$h6(icon("box"), " Register Boxes (optional)"),
          tags$p(class = "text-muted", style = "font-size: 0.85em;",
                 "You can register boxes for this project now, or do it later in the Biobank tab."),
          
          checkboxInput("new_proj_register_boxes", "Register boxes now", value = FALSE),
          
          conditionalPanel(
            condition = "input.new_proj_register_boxes == true",
            tags$div(
              style = "background: #f8f9fa; border-radius: 6px; padding: 15px; margin-top: 10px;",
              
              layout_column_wrap(
                width = 1/2,
                numericInput("new_proj_n_boxes", "Number of boxes:", 
                             value = 1, min = 1, max = 50, width = "100%"),
                numericInput("new_proj_samples_per_box", "Samples per box:",
                             value = 10, min = 1, max = 200, width = "100%")
              ),
              
              textInput("new_proj_box_prefix", "Box name prefix:",
                        placeholder = "e.g. Plasma, Urine, Serum...", width = "100%"),
              
              selectInput("new_proj_box_status", "Initial status:",
                          choices = c("Received", "In Preparation"),
                          selected = "Received", width = "100%"),
              
              tags$div(
                style = "background: #d1ecf1; border-radius: 6px; padding: 10px; margin-top: 10px; font-size: 0.85em;",
                icon("info-circle", style = "color: #17a2b8;"),
                " Box codes will be auto-generated as: ",
                tags$code(paste0(abbr, "_Prefix_001")), ", ",
                tags$code(paste0(abbr, "_Prefix_002")), ", etc."
              )
            )
          )
        ),
        
        footer = tagList(
          actionButton("new_proj_skip_boxes", "Done (no boxes)", class = "btn-secondary"),
          actionButton("new_proj_save_boxes", "Register Boxes", class = "btn-success", icon = icon("box"))
        )
      ))
    } else {
      showNotification(paste0("Project added: ", abbr), type = "message", duration = 4)
    }
    
    # Clear form
    updateTextInput(session, "proj_title", value = "")
    updateTextInput(session, "proj_abbrev", value = "")
    updateTextInput(session, "proj_name", value = "")
    updateTextInput(session, "proj_email", value = "")
    updateTextInput(session, "proj_group", value = "")
    updateTextInput(session, "proj_contact", value = "")
    updateTextInput(session, "proj_n_samples", value = "")
    updateTextInput(session, "proj_boxes", value = "")
  })
  
  
  

  # Clear project form

  # ---- Skip boxes (just close modal) ----
  observeEvent(input$new_proj_skip_boxes, {
    removeModal()
    showNotification("Project created (no boxes registered).", type = "message", duration = 4)
  })
  
  # ---- Register boxes for new project ----
  observeEvent(input$new_proj_save_boxes, {
    # Get the most recently added project abbreviation
    abbr <- rv_projects$data$Abbreviation[nrow(rv_projects$data)]
    
    if (is.null(abbr) || is.na(abbr) || nchar(trimws(abbr)) == 0) {
      showNotification("Could not determine project. Please register boxes manually.", type = "error")
      removeModal()
      return()
    }
    
    if (!isTRUE(input$new_proj_register_boxes)) {
      removeModal()
      showNotification("Project created (no boxes registered).", type = "message", duration = 4)
      return()
    }
    
    n_boxes <- input$new_proj_n_boxes
    prefix <- trimws(input$new_proj_box_prefix)
    samples_per_box <- input$new_proj_samples_per_box
    status <- input$new_proj_box_status
    
    if (is.null(n_boxes) || n_boxes < 1) n_boxes <- 1
    if (nchar(prefix) == 0) prefix <- "Box"
    if (is.null(samples_per_box) || samples_per_box < 1) samples_per_box <- 10
    
    # Get existing box codes to avoid duplicates
    existing_codes <- if (!is.null(rv_boxes$data) && nrow(rv_boxes$data) > 0) {
      rv_boxes$data$Box_Code
    } else {
      character(0)
    }
    
    # Build new boxes data frame
    new_boxes <- data.frame(
      Box_Code = character(n_boxes),
      Project = rep(abbr, n_boxes),
      Box_Name = character(n_boxes),
      Sample_Count = rep(as.integer(samples_per_box), n_boxes),
      Received = rep(as.character(Sys.Date()), n_boxes),
      Status = rep(status, n_boxes),
      Measured_Date = rep(NA_character_, n_boxes),
      Notes = rep(NA_character_, n_boxes),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_len(n_boxes)) {
      box_code <- paste0(abbr, "_", prefix, "_", sprintf("%03d", i))
      
      # Ensure unique code
      counter <- i
      while (box_code %in% c(existing_codes, new_boxes$Box_Code[seq_len(i - 1)])) {
        counter <- counter + 1
        box_code <- paste0(abbr, "_", prefix, "_", sprintf("%03d", counter))
      }
      
      new_boxes$Box_Code[i] <- box_code
      new_boxes$Box_Name[i] <- paste0(prefix, " ", sprintf("%03d", i))
    }
    
    # Ensure columns match existing box data
    for (col in names(rv_boxes$data)) {
      if (!col %in% names(new_boxes)) new_boxes[[col]] <- NA_character_
    }
    for (col in names(new_boxes)) {
      if (!col %in% names(rv_boxes$data)) rv_boxes$data[[col]] <- NA_character_
    }
    
    rv_boxes$data <- rbind(rv_boxes$data, new_boxes[, names(rv_boxes$data), drop = FALSE])
    
    # Save box registry
    tryCatch({
      write.csv(rv_boxes$data, box_registry_path, row.names = FALSE)
      
      removeModal()
      showNotification(
        paste0("Project '", abbr, "' created with ", n_boxes, " box(es): ",
               paste(new_boxes$Box_Code, collapse = ", ")),
        type = "message", duration = 6
      )
    }, error = function(e) {
      showNotification(paste0("Error saving boxes: ", e$message), type = "error")
      removeModal()
    })
  })
  observeEvent(input$clear_project_form_btn, {
    updateTextInput(session, "proj_title", value = "")
    updateTextInput(session, "proj_abbrev", value = "")
    updateTextInput(session, "proj_name", value = "")
    updateTextInput(session, "proj_email", value = "")
    updateTextInput(session, "proj_group", value = "")
    updateTextInput(session, "proj_contact", value = "")
    updateTextInput(session, "proj_n_samples", value = "")
    updateTextInput(session, "proj_boxes", value = "")
    updateSelectInput(session, "proj_sample_type", selected = "Plasma")
  })

  # Save projects to disk
  observeEvent(input$save_projects_btn, {
    tryCatch({
      # Save as CSV (primary format)
      write.csv(rv_projects$data, projects_csv_path, row.names = FALSE)
      
      # Also save as Excel for compatibility
      wb <- createWorkbook()
      addWorksheet(wb, "Projects")
      writeData(wb, "Projects", rv_projects$data)
      setColWidths(wb, "Projects", cols = 1:ncol(rv_projects$data), widths = "auto")
      saveWorkbook(wb, projects_xlsx_path, overwrite = TRUE)
      
      create_backup(type = "manual_save")
      showNotification("Project list saved + backup created!",
                       type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste0("Save error: ", e$message),
                       type = "error", duration = 5)
    })
  })
  

  # Download projects
  output$download_projects_btn <- downloadHandler(
    filename = function() {
      paste0("Projects_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Projects")
      writeData(wb, "Projects", rv_projects$data)
      setColWidths(wb, "Projects", cols = 1:ncol(rv_projects$data), widths = "auto")
      headerStyle <- createStyle(textDecoration = "bold", fgFill = "#2c3e50",
                                 fontColour = "#ffffff", halign = "center")
      addStyle(wb, "Projects", headerStyle, rows = 1, cols = 1:ncol(rv_projects$data))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$qc_kpi_overall_rate <- renderText({
    if (is.null(rv_qc$overall)) return("-")
    avg_rate <- round(mean(rv_qc$overall$Pass_Rate, na.rm = TRUE), 1)
    paste0(avg_rate, "%")
  })

  # ---- SETTINGS: ARCHIVE COLUMN MANAGER ----
  
  # Bumped whenever archive columns change, to force the panel to re-render
  rv_archive_cols_version <- reactiveVal(0)

  output$current_archive_cols_ui <- renderUI({
    rv_archive_cols_version()
    input$add_archive_col_btn
    input$confirm_remove_archive_cols
    
    config <- read_config()
    if (is.null(config) || is.null(config$archive$columns)) {
      return(tags$p(class = "text-muted", "No config found."))
    }
    
    cols <- config$archive$columns
    auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
    
    col_items <- lapply(cols, function(col) {
      is_auto <- col %in% auto_cols
      col_type <- "text"
      if (!is.null(config$archive$column_types) && !is.null(config$archive$column_types[[col]])) {
        ct <- config$archive$column_types[[col]]
        col_type <- if (is.list(ct)) ct$type else ct
      }
      
      tags$div(
        style = paste0(
          "display: flex; align-items: center; justify-content: space-between; ",
          "padding: 6px 10px; margin-bottom: 4px; border-radius: 4px; ",
          "background: ", if (is_auto) "#eaf2f8" else "#fef9e7", ";"
        ),
        tags$div(
          style = "display: flex; align-items: center; gap: 8px;",
          if (!is_auto) {
            checkboxInput(
              paste0("archive_col_select_", gsub("[^a-zA-Z0-9]", "_", col)),
              label = NULL, value = FALSE, width = "20px"
            )
          },
          tags$span(style = if (is_auto) "font-weight: 600;" else "", col)
        ),
        tags$div(
          style = "display: flex; gap: 5px;",
          tags$span(
            class = "badge",
            style = paste0("background: ", if (is_auto) "#3498db" else "#f39c12",
                           "; color: white; font-size: 0.7em;"),
            if (is_auto) "auto" else col_type
          )
        )
      )
    })
    
    tagList(
      tags$div(
        style = "max-height: 400px; overflow-y: auto; border: 1px solid #eee; border-radius: 6px; padding: 8px;",
        col_items
      ),
      tags$p(
        style = "font-size: 0.8em; color: #95a5a6; margin-top: 8px;",
        icon("info-circle"),
        paste0(length(cols), " columns (",
               sum(cols %in% auto_cols), " auto, ",
               sum(!cols %in% auto_cols), " custom)")
      )
    )
  })
  
  # Add new archive column
  observeEvent(input$add_archive_col_btn, {
    col_name <- trimws(input$new_archive_col_name)
    if (nchar(col_name) == 0) {
      showNotification("Please enter a column name.", type = "error", duration = 3)
      return()
    }
    
    # Sanitize
    col_name_clean <- gsub("[^A-Za-z0-9_]", "_", col_name)
    
    config <- read_config()
    if (is.null(config)) {
      showNotification("No config found.", type = "error", duration = 3)
      return()
    }
    
    if (col_name_clean %in% config$archive$columns) {
      showNotification(paste0("Column '", col_name_clean, "' already exists!"), type = "error", duration = 3)
      return()
    }
    
    config$archive$columns <- c(config$archive$columns, col_name_clean)
    
    if (is.null(config$archive$column_types)) config$archive$column_types <- list()
    config$archive$column_types[[col_name_clean]] <- list(
      type = input$new_archive_col_type,
      choices = if (input$new_archive_col_type == "select") {
        trimws(unlist(strsplit(input$new_archive_col_choices, ",")))
      } else {
        NULL
      }
    )
    
    tryCatch({
      save_config(config)
      
      # Add column to existing archive CSV
      if (!is.null(rv_archive$data)) {
        if (!col_name_clean %in% names(rv_archive$data)) {
          rv_archive$data[[col_name_clean]] <- NA_character_
          write.csv(rv_archive$data, archive_path, row.names = FALSE)
        }
      }
      
      rv_archive_cols_version(rv_archive_cols_version() + 1)
      showNotification(paste0("Column '", col_name_clean, "' added!"), type = "message", duration = 4)
      updateTextInput(session, "new_archive_col_name", value = "")
      updateTextInput(session, "new_archive_col_choices", value = "")
    }, error = function(e) {
      showNotification(paste0("Error: ", e$message), type = "error", duration = 5)
    })
  })
  
  # Remove selected archive columns
  observeEvent(input$remove_archive_col_btn, {
    config <- read_config()
    if (is.null(config)) return()
    
    auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
    custom_cols <- setdiff(config$archive$columns, auto_cols)
    
    cols_to_remove <- character(0)
    for (col in custom_cols) {
      input_id <- paste0("archive_col_select_", gsub("[^a-zA-Z0-9]", "_", col))
      val <- input[[input_id]]
      if (!is.null(val) && val == TRUE) {
        cols_to_remove <- c(cols_to_remove, col)
      }
    }
    
    if (length(cols_to_remove) == 0) {
      showNotification("No custom columns selected.", type = "warning", duration = 3)
      return()
    }
    
    showModal(modalDialog(
      title = tags$div(icon("triangle-exclamation", style = "color: #e74c3c;"), " Confirm Removal"),
      tags$p("Remove these columns from the archive config?"),
      tags$ul(lapply(cols_to_remove, function(col) tags$li(tags$code(col)))),
      tags$div(
        style = "background: #fef9e7; padding: 10px; border-radius: 6px; font-size: 0.85em;",
        icon("info-circle", style = "color: #f39c12;"),
        " Existing data in the CSV will not be deleted."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_remove_archive_cols", "Remove", class = "btn-danger", icon = icon("trash"))
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_remove_archive_cols, {
    config <- read_config()
    if (is.null(config)) return()
    
    auto_cols <- c("Date", "Name", "Project", "Type", "Size", "Box", "Box_Code")
    custom_cols <- setdiff(config$archive$columns, auto_cols)
    
    cols_to_remove <- character(0)
    for (col in custom_cols) {
      input_id <- paste0("archive_col_select_", gsub("[^a-zA-Z0-9]", "_", col))
      val <- input[[input_id]]
      if (!is.null(val) && val == TRUE) {
        cols_to_remove <- c(cols_to_remove, col)
      }
    }
    
    config$archive$columns <- setdiff(config$archive$columns, cols_to_remove)
    if (!is.null(config$archive$column_types)) {
      for (col in cols_to_remove) config$archive$column_types[[col]] <- NULL
    }
    
    tryCatch({
      save_config(config)
      removeModal()
      rv_archive_cols_version(rv_archive_cols_version() + 1)
      showNotification(paste0("Removed: ", paste(cols_to_remove, collapse = ", ")), type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste0("Error: ", e$message), type = "error", duration = 5)
    })
  })

  # ---- Project column: add ----
  observeEvent(input$add_project_col_btn, {
    col_name <- trimws(input$new_project_col_name)
    if (nchar(col_name) == 0) {
      showNotification("Please enter a column name.", type = "error", duration = 3)
      return()
    }
    col_clean <- gsub("[^A-Za-z0-9_]", "_", col_name)

    config <- read_config()
    if (is.null(config)) {
      showNotification("No config found.", type = "error", duration = 3)
      return()
    }

    existing <- as.character(unlist(config$projects$columns))
    if (col_clean %in% existing) {
      showNotification(paste0("Column '", col_clean, "' already exists!"),
                       type = "error", duration = 3)
      return()
    }

    config$projects$columns <- as.list(c(existing, col_clean))

    tryCatch({
      save_config(config)

      if (!is.null(rv_projects$data) && !col_clean %in% names(rv_projects$data)) {
        rv_projects$data[[col_clean]] <-
          if (nrow(rv_projects$data) == 0) character(0) else NA_character_
        write.csv(rv_projects$data, projects_csv_path, row.names = FALSE)
      }

      rv_project_cols_version(rv_project_cols_version() + 1)
      showNotification(paste0("Column '", col_clean, "' added!"),
                       type = "message", duration = 4)
      updateTextInput(session, "new_project_col_name", value = "")
    }, error = function(e) {
      showNotification(paste0("Error: ", e$message), type = "error", duration = 5)
    })
  })

  # ---- Project column: confirm dialog ----
  observeEvent(input$remove_project_col_btn, {
    config <- read_config()
    if (is.null(config)) return()

    required <- c("Title", "Abbreviation")
    custom <- setdiff(as.character(unlist(config$projects$columns)), required)

    selected <- character(0)
    for (col in custom) {
      id <- paste0("project_col_select_", gsub("[^a-zA-Z0-9]", "_", col))
      if (isTRUE(input[[id]])) selected <- c(selected, col)
    }

    if (length(selected) == 0) {
      showNotification("No removable columns selected.", type = "warning", duration = 3)
      return()
    }

    showModal(modalDialog(
      title = tags$span(icon("triangle-exclamation", style = "color:#e74c3c;"),
                        " Remove Project Columns"),
      tags$p("Remove the following from the project configuration?"),
      tags$ul(lapply(selected, tags$li)),
      tags$p(class = "text-muted",
             "Existing data stays in projects.csv and is not deleted."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_remove_project_cols", "Remove", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  # ---- Project column: remove ----
  observeEvent(input$confirm_remove_project_cols, {
    config <- read_config()
    if (is.null(config)) return()

    required <- c("Title", "Abbreviation")
    custom <- setdiff(as.character(unlist(config$projects$columns)), required)

    to_remove <- character(0)
    for (col in custom) {
      id <- paste0("project_col_select_", gsub("[^a-zA-Z0-9]", "_", col))
      if (isTRUE(input[[id]])) to_remove <- c(to_remove, col)
    }
    to_remove <- setdiff(to_remove, required)

    if (length(to_remove) == 0) {
      removeModal()
      return()
    }

    config$projects$columns <-
      as.list(setdiff(as.character(unlist(config$projects$columns)), to_remove))

    tryCatch({
      save_config(config)
      removeModal()
      rv_project_cols_version(rv_project_cols_version() + 1)
      showNotification(paste0("Removed: ", paste(to_remove, collapse = ", ")),
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste0("Error: ", e$message), type = "error", duration = 5)
    })
  })
  
  # ---- SETTINGS: PROJECT COLUMNS ----
  rv_project_cols_version <- reactiveVal(0)

  output$current_project_cols_ui <- renderUI({
    rv_project_cols_version()
    config <- read_config()
    if (is.null(config)) return(tags$p("No config."))

    cols <- as.character(unlist(config$projects$columns))
    required <- c("Title", "Abbreviation")

    tags$div(
      style = "max-height: 400px; overflow-y: auto;",
      lapply(cols, function(col) {
        is_req <- col %in% required
        tags$div(
          style = paste0("display: flex; align-items: center; gap: 8px; padding: 6px 10px; margin: 3px 0; border-radius: 4px; background: ",
                         if (is_req) "#eaf2f8" else "#f8f9fa", ";"),
          if (is_req) {
            icon("lock", style = "color: #3498db; width: 16px;")
          } else {
            tags$div(
              style = "width: 16px;",
              checkboxInput(
                paste0("project_col_select_", gsub("[^a-zA-Z0-9]", "_", col)),
                NULL, value = FALSE, width = "auto"
              )
            )
          },
          tags$span(style = if (is_req) "font-weight: 600;" else "", col)
        )
      })
    )
  })
  
  # ---- SETTINGS: MODULE TOGGLES ----
  output$module_toggles_ui <- renderUI({
    config <- read_config()
    if (is.null(config)) return(NULL)
    mod <- config$modules

    core <- list(
      list(name = "Sample Submission",   icon = "flask"),
      list(name = "Data Extraction",     icon = "file-code"),
      list(name = "Sample Archive",      icon = "archive"),
      list(name = "Projects",            icon = "folder-tree"),
      list(name = "Measurement Archive", icon = "clock-rotate-left")
    )

    optional <- list(
      list(id = "mod_viewer", name = "Spectra Viewer", key = "viewer", icon = "wave-square"),
      list(id = "mod_qk",     name = "QC Monitor",     key = "qk",     icon = "chart-line"),
      list(id = "mod_boxes",  name = "Biobank",        key = "boxes",  icon = "box"),
      list(id = "mod_nmr",    name = "NMR Copy",       key = "nmr",    icon = "magnet")
    )

    tagList(
      tags$div(
        style = "font-size: 0.75em; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
        "Core \u2014 always enabled"
      ),
      lapply(core, function(m) {
        tags$div(
          style = "display: flex; align-items: center; gap: 10px; padding: 6px 10px; margin-bottom: 4px; border-radius: 6px; background: #f8f9fa; opacity: 0.85;",
          icon("check", style = "color: #18bc9c; width: 16px;"),
          icon(m$icon, style = "color: #3498db; width: 20px;"),
          tags$span(m$name)
        )
      }),

      tags$hr(style = "margin: 14px 0 10px 0;"),

      tags$div(
        style = "font-size: 0.75em; font-weight: 600; color: #6c757d; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 8px;",
        "Optional"
      ),
      lapply(optional, function(m) {
        tags$div(
          style = "display: flex; align-items: center; gap: 10px; padding: 4px 10px; margin-bottom: 2px;",
          checkboxInput(m$id, NULL, value = isTRUE(mod[[m$key]]), width = "auto"),
          icon(m$icon, style = "color: #3498db; width: 20px;"),
          tags$span(m$name)
        )
      })
    )
  })
  
  observeEvent(input$save_module_settings, {
    config <- read_config()
    if (is.null(config)) return()

    # Core modules stay TRUE; only optional ones are user-controlled.
    config$modules <- list(
      submission = TRUE,
      archive    = TRUE,
      xml        = TRUE,
      projects   = TRUE,
      meas       = TRUE,
      viewer = isTRUE(input$mod_viewer),
      qk     = isTRUE(input$mod_qk),
      boxes  = isTRUE(input$mod_boxes),
      nmr    = isTRUE(input$mod_nmr)
    )

    save_config(config)
    showNotification("Modules saved. Restarting...", type = "message")
    session$reload()
  })
  
  # ---- SETTINGS: GENERAL ----
  observeEvent(input$save_general_settings, {
    config <- read_config()
    if (is.null(config)) return()
    
    config$lab_name <- input$settings_lab_name
    config$data_path <- input$settings_data_path
    save_config(config)
    showNotification("Settings saved!", type = "message", duration = 3)
  })
  
  
  
  # QC CHECK OUTPUTS--------------------
  # QK MONITOR - OUTLIER COMPUTATION (must be defined before outputs that use it)
  qk_outliers <- reactive({
    if (is.null(rv_qk$data) || is.null(rv_qk$limits)) return(data.frame())
    if (nrow(rv_qk$data) == 0 || nrow(rv_qk$limits) == 0) return(data.frame())

    df <- rv_qk$data
    limits <- rv_qk$limits

    outlier_list <- list()

    for (i in seq_len(nrow(limits))) {
      param <- limits$Parameter[i]
      lower <- as.numeric(limits$Lower[i])
      upper <- as.numeric(limits$Higher[i])

      if (!param %in% names(df)) next

      values <- as.numeric(df[[param]])
      ool <- which(!is.na(values) & (values < lower | values > upper))

      if (length(ool) > 0) {
        for (idx in ool) {
          outlier_list[[length(outlier_list) + 1]] <- data.frame(
            Name = as.character(df$Name[idx]),
            Date = as.character(df$Date[idx]),
            Parameter = param,
            Value = values[idx],
            Lower = lower,
            Upper = upper,
            Deviation = ifelse(values[idx] < lower,
                               paste0(round((lower - values[idx]) / lower * 100, 1), "% below"),
                               paste0(round((values[idx] - upper) / upper * 100, 1), "% above")),
            stringsAsFactors = FALSE
          )
        }
      }
    }

    if (length(outlier_list) > 0) do.call(rbind, outlier_list) else data.frame()
  })

  # QC KPIs
  output$qc_kpi_total <- renderText({
    if (is.null(rv_qc$overall)) return("0")
    nrow(rv_qc$overall)
  })
  output$qc_kpi_pass <- renderText({
    if (is.null(rv_qc$overall)) return("0")
    sum(rv_qc$overall$Overall_Status == "PASS")
  })
  output$qc_kpi_fail <- renderText({
    if (is.null(rv_qc$overall)) return("0")
    sum(rv_qc$overall$Overall_Status == "FAIL")
  })
  output$qc_kpi_matrix <- renderText({
    if (is.null(rv_qc$overall)) return("-")
    n_pass <- sum(rv_qc$overall$Matrix_Integrity == "PASS")
    n_total <- nrow(rv_qc$overall)
    paste0(n_pass, "/", n_total, " OK")
  })
  output$qc_kpi_prep <- renderText({
    if (is.null(rv_qc$overall)) return("-")
    n_pass <- sum(rv_qc$overall$Sample_Preparation == "PASS")
    n_total <- nrow(rv_qc$overall)
    paste0(n_pass, "/", n_total, " OK")
  })
  output$qc_kpi_spectral <- renderText({
    if (is.null(rv_qc$overall)) return("-")
    n_pass <- sum(rv_qc$overall$NMR_Spectral_Quality == "PASS")
    n_total <- nrow(rv_qc$overall)
    paste0(n_pass, "/", n_total, " OK")
  })

  # QC Overall table
  output$qc_overall_table <- renderDT({
    req(rv_qc$overall)
    datatable(rv_qc$overall, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 25,
                             order = list(list(3, "asc")),
                             language = list(
                               url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                             )),
              rownames = FALSE) %>%
      formatStyle("Overall_Status",
                  backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d5f5e3", "#fadbd8")),
                  fontWeight = "bold") %>%
      formatStyle("Matrix_Integrity",
                  backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d5f5e3", "#fadbd8"))) %>%
      formatStyle("Sample_Preparation",
                  backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d5f5e3", "#fadbd8"))) %>%
      formatStyle("NMR_Spectral_Quality",
                  backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d5f5e3", "#fadbd8"))) %>%
      formatStyle("Pass_Rate",
                  background = styleColorBar(c(0, 100), "#18bc9c"),
                  backgroundSize = "98% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "left",
                  fontWeight = "bold",
                  color = styleInterval(90, c("#e74c3c", "#2c3e50")))
  })

  # QC Summary by category
  output$qc_summary_table <- renderDT({
    req(rv_qc$summary)
    datatable(rv_qc$summary, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 25,
                             language = list(
                               url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                             )),
              rownames = FALSE) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d5f5e3", "#fadbd8")),
                  fontWeight = "bold") %>%
      formatStyle("Pass_Rate",
                  background = styleColorBar(c(0, 100), "#18bc9c"),
                  backgroundSize = "98% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "left")
  })

  # QC Detail table
  output$qc_detail_table <- renderDT({
    req(rv_qc$detail)
    df <- rv_qc$detail
    if (!is.null(input$qc_detail_filter) && input$qc_detail_filter == "fail") {
      df <- df %>% filter(Passed == "FAIL")
    }
    datatable(df, class = "compact stripe hover", filter = "top",
              options = list(scrollX = TRUE, pageLength = 30,
                             language = list(
                               url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                             )),
              rownames = FALSE) %>%
      formatStyle("Passed",
                  backgroundColor = styleEqual(c("PASS", "FAIL", "N/A"),
                                               c("#d5f5e3", "#fadbd8", "#f8f9fa")),
                  fontWeight = "bold")
  })

  # QC Chart: Pass/Fail per sample
  output$qc_chart_samples <- renderPlotly({
    req(rv_qc$detail)
    df <- rv_qc$detail %>%
      filter(Passed != "N/A") %>%
      group_by(ID, Passed) %>%
      summarise(n = n(), .groups = "drop")

    colors <- c("PASS" = "#18bc9c", "FAIL" = "#e74c3c")

    plot_ly(df, y = ~ID, x = ~n, color = ~Passed, type = "bar",
            orientation = "h", colors = colors,
            hovertemplate = "%{y}: %{x} Parameter<extra>%{fullData.name}</extra>") %>%
      layout(barmode = "stack",
             xaxis = list(title = "Number of Parameters"),
             yaxis = list(title = "", categoryorder = "total ascending"),
             legend = list(orientation = "h", y = -0.15),
             margin = list(l = 120, t = 10))
  })

  # QC Chart: Most failed parameters
  output$qc_chart_params <- renderPlotly({
    req(rv_qc$detail)
    df <- rv_qc$detail %>%
      filter(Passed == "FAIL") %>%
      count(Parameter, Category) %>%
      arrange(desc(n)) %>%
      head(15)

    if (nrow(df) == 0) {
      return(plotly_empty() %>%
               layout(title = list(text = "No errors found! \u2713",
                                   font = list(color = "#18bc9c"))))
    }

    colors <- c("Matrix Integrity" = "#3498db",
                "Sample Preparation" = "#f39c12",
                "NMR Spectral Quality" = "#9b59b6")

    plot_ly(df, y = ~reorder(Parameter, n), x = ~n, color = ~Category,
            type = "bar", orientation = "h", colors = colors,
            hovertemplate = "%{y}: %{x} samples failed<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "Number of Failed Samples"),
             yaxis = list(title = ""),
             legend = list(orientation = "h", y = -0.15),
             margin = list(l = 200, t = 10))
  })


  # QK MONITOR MODULE


  # ---- QC Lot Management ----
  qk_lots_dir <- function() {
    d <- file.path(APP_DIR, "QK_lots")
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    d
  }
  
  qk_lots_registry_path <- function() {
    file.path(APP_DIR, "qk_lots.json")
  }
  
  load_qk_lots <- function() {
    path <- qk_lots_registry_path()
    if (file.exists(path)) {
      tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
               error = function(e) list(active = NULL, lots = list()))
    } else {
      list(active = NULL, lots = list())
    }
  }
  
  save_qk_lots <- function(registry) {
    jsonlite::write_json(registry, qk_lots_registry_path(), pretty = TRUE, auto_unbox = TRUE)
  }
  
  # Load lots registry on startup
  observe({
    reg <- load_qk_lots()
    rv_qk$lots <- reg$lots
    rv_qk$active_lot <- reg$active
  }, priority = 110)

  # QK file paths
  qk_data_path <- reactive({
    matrix <- if (!is.null(input$qk_matrix)) input$qk_matrix else "plasma"
    matrix_label <- switch(matrix, "plasma" = "Plasma", "urine" = "Urine", tools::toTitleCase(matrix))
    file.path(APP_DIR, paste0(matrix_label, "_NMR_QK.csv"))
  })
  qk_limits_path <- reactive({
    matrix <- if (!is.null(input$qk_matrix)) input$qk_matrix else "plasma"
    matrix_label <- switch(matrix, "plasma" = "Plasma", "urine" = "Urine", tools::toTitleCase(matrix))
    file.path(APP_DIR, paste0(matrix_label, "_QK_limits.csv"))
  })
  
  # Reactive values for QK
  rv_qk <- reactiveValues(
    data = NULL,
    limits = NULL,
    outliers = NULL,
    lots = NULL,
    active_lot = NULL
  )

  # Load QK data on startup
  observe({
    # Use reactive path if available, otherwise default to Plasma
    dp <- tryCatch(qk_data_path(), error = function(e) file.path(APP_DIR, "Plasma_NMR_QK.csv"))
    lp <- tryCatch(qk_limits_path(), error = function(e) file.path(APP_DIR, "Plasma_QK_limits.csv"))
    
    rv_qk$data <- tryCatch({
      if (!file.exists(dp)) return(NULL)
      df <- fread(dp, stringsAsFactors = FALSE)
      df <- as.data.frame(df)
      if ("Date" %in% names(df)) {
        df$Date <- parse_dates_safe(df$Date)
      }
      df
    }, error = function(e) NULL)

    rv_qk$limits <- tryCatch({
      if (!file.exists(lp)) return(NULL)
      df <- fread(lp, stringsAsFactors = FALSE)
      df$Parameter <- trimws(df$Parameter)
      as.data.frame(df)
    }, error = function(e) NULL)
  }, priority = 100)



  # Update parameter choices when data or limits change
  observe({
    # Combine parameters from limits file AND data columns
    limit_params <- if (!is.null(rv_qk$limits) && nrow(rv_qk$limits) > 0) rv_qk$limits$Parameter else character(0)
    
    # Get numeric columns from data (exclude Date, Name, etc.)
    data_params <- character(0)
    if (!is.null(rv_qk$data) && nrow(rv_qk$data) > 0) {
      skip_cols <- c("Date", "Name", "DateParsed", "Status", "Value")
      candidate_cols <- setdiff(names(rv_qk$data), skip_cols)
      for (col in candidate_cols) {
        vals <- suppressWarnings(as.numeric(rv_qk$data[[col]]))
        if (!all(is.na(vals))) data_params <- c(data_params, col)
      }
    }
    
    # Merge: limits params first, then any extra data columns
    all_params <- unique(c(limit_params, data_params))
    
    if (length(all_params) > 0) {
      # Mark which ones have limits configured
      labels <- sapply(all_params, function(p) {
        if (p %in% limit_params) p
        else paste0(p, " (no limits)")
      })
      updateSelectInput(session, "qk_parameter", 
                        choices = setNames(all_params, labels), 
                        selected = all_params[1])
    } else {
      updateSelectInput(session, "qk_parameter", choices = character(0))
    }
  })

  # Refresh button - reload data and check for new QK samples

  # Reload QC data when matrix changes
  observeEvent(input$qk_matrix, {
    # Load data for selected matrix
    rv_qk$data <- tryCatch({
      dp <- qk_data_path()
      if (!file.exists(dp)) return(NULL)
      df <- fread(dp, stringsAsFactors = FALSE)
      df <- as.data.frame(df)
      if ("Date" %in% names(df)) df$Date <- parse_dates_safe(df$Date)
      df
    }, error = function(e) NULL)
    
    # Load limits for selected matrix
    rv_qk$limits <- tryCatch({
      lp <- qk_limits_path()
      if (!file.exists(lp)) return(NULL)
      df <- fread(lp, stringsAsFactors = FALSE)
      df$Parameter <- trimws(df$Parameter)
      as.data.frame(df)
    }, error = function(e) NULL)
    
    # Update parameter dropdown
    if (!is.null(rv_qk$limits) && nrow(rv_qk$limits) > 0) {
      params <- rv_qk$limits$Parameter
      updateSelectInput(session, "qk_parameter", choices = params, selected = params[1])
    } else {
      updateSelectInput(session, "qk_parameter", choices = character(0))
    }
    
    matrix_label <- switch(input$qk_matrix, 
                           "plasma" = "Plasma", "urine" = "Urine", 
                           tools::toTitleCase(input$qk_matrix))
    showNotification(paste0("Switched to ", matrix_label, " QC"), type = "message", duration = 2)
  }, ignoreInit = TRUE)
  observeEvent(input$qk_refresh_btn, {
    # Reload from file
    rv_qk$data <- tryCatch({
      df <- fread(qk_data_path(), stringsAsFactors = FALSE)
      as.data.frame(df)
    }, error = function(e) NULL)

    showNotification("QC data updated.", type = "message", duration = 3)
  })

  # ---- QC SETTINGS HANDLERS ----
  
  # Render limits table
  output$qk_limits_table <- renderDT({
    req(rv_qk$limits)
    datatable(rv_qk$limits,
              selection = "single",
              rownames = FALSE,
              editable = list(target = "cell", disable = list(columns = 0)),
              options = list(
                pageLength = 20,
                dom = "tp",
                columnDefs = list(
                  list(className = "dt-center", targets = "_all")
                )
              )) %>%
      formatStyle("Parameter", fontWeight = "bold") %>%
      formatStyle("Lower", color = "#e67e22") %>%
      formatStyle("Higher", color = "#e74c3c")
  })
  
  # Edit limits inline
  observeEvent(input$qk_limits_table_cell_edit, {
    info <- input$qk_limits_table_cell_edit
    row <- info$row
    col <- info$col + 1  # DT is 0-indexed
    value <- info$value
    
    if (col %in% c(2, 3)) {  # Lower or Higher columns
      value <- suppressWarnings(as.numeric(value))
    }
    
    rv_qk$limits[row, col] <- value
    showNotification("Limit updated. Click Save to persist.", type = "message", duration = 3)
  })
  
  # Add new parameter

  # Render parameter name input with auto-suggest from data columns
  output$qk_param_name_input <- renderUI({
    suggestions <- character(0)
    if (!is.null(rv_qk$data) && nrow(rv_qk$data) > 0) {
      skip_cols <- c("Date", "Name", "DateParsed", "Status", "Value")
      existing_params <- if (!is.null(rv_qk$limits)) rv_qk$limits$Parameter else character(0)
      candidate_cols <- setdiff(names(rv_qk$data), c(skip_cols, existing_params))
      for (col in candidate_cols) {
        vals <- suppressWarnings(as.numeric(rv_qk$data[[col]]))
        if (!all(is.na(vals))) suggestions <- c(suggestions, col)
      }
    }
    
    if (length(suggestions) > 0) {
      selectizeInput("qk_new_param_name", "Parameter name:",
                     choices = c("", suggestions),
                     selected = "",
                     options = list(create = TRUE, placeholder = "Select or type new..."),
                     width = "100%")
    } else {
      textInput("qk_new_param_name", "Parameter name:",
                placeholder = "e.g. Glucose", width = "100%")
    }
  })
  observeEvent(input$qk_add_param, {
    name <- trimws(input$qk_new_param_name)
    if (nchar(name) == 0) {
      showNotification("Please enter a parameter name.", type = "warning", duration = 3)
      return()
    }
    
    # Check for duplicate
    if (!is.null(rv_qk$limits) && name %in% rv_qk$limits$Parameter) {
      showNotification(paste0("Parameter '", name, "' already exists."), type = "warning", duration = 3)
      return()
    }
    
    new_row <- data.frame(
      Parameter = name,
      Lower = input$qk_new_param_lower,
      Higher = input$qk_new_param_upper,
      stringsAsFactors = FALSE
    )
    
    if (is.null(rv_qk$limits) || nrow(rv_qk$limits) == 0) {
      rv_qk$limits <- new_row
    } else {
      rv_qk$limits <- rbind(rv_qk$limits, new_row)
    }
    
    # Update parameter dropdown
    params <- rv_qk$limits$Parameter
    updateSelectInput(session, "qk_parameter", choices = params, selected = params[length(params)])
    
    # Clear inputs
    updateTextInput(session, "qk_new_param_name", value = "")
    updateNumericInput(session, "qk_new_param_lower", value = NA)
    updateNumericInput(session, "qk_new_param_upper", value = NA)
    
    showNotification(paste0("Parameter '", name, "' added. Click Save to persist."), 
                     type = "message", duration = 3)
  })
  
  # Delete selected parameter
  observeEvent(input$qk_delete_param, {
    row_idx <- input$qk_limits_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) {
      showNotification("Please select a parameter to delete.", type = "warning", duration = 3)
      return()
    }
    
    param_name <- rv_qk$limits$Parameter[row_idx]
    
    showModal(modalDialog(
      title = tags$div(icon("triangle-exclamation", style = "color: #e74c3c;"), " Confirm Deletion"),
      tags$p(paste0("Remove parameter '", param_name, "' from QC monitoring?")),
      tags$p(class = "text-muted", style = "font-size: 0.85em;",
             "This will not delete existing QC data for this parameter."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("qk_confirm_delete_param", "Delete", class = "btn-danger", icon = icon("trash"))
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$qk_confirm_delete_param, {
    row_idx <- input$qk_limits_table_rows_selected
    if (!is.null(row_idx) && length(row_idx) > 0) {
      deleted <- rv_qk$limits$Parameter[row_idx]
      rv_qk$limits <- rv_qk$limits[-row_idx, , drop = FALSE]
      
      params <- rv_qk$limits$Parameter
      updateSelectInput(session, "qk_parameter", choices = params, 
                        selected = if (length(params) > 0) params[1] else NULL)
      
      showNotification(paste0("Parameter '", deleted, "' removed."), type = "warning", duration = 3)
    }
    removeModal()
  })
  
  # Edit selected parameter
  observeEvent(input$qk_edit_param, {
    row_idx <- input$qk_limits_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) {
      showNotification("Please select a parameter to edit.", type = "warning", duration = 3)
      return()
    }
    
    row <- rv_qk$limits[row_idx, ]
    
    showModal(modalDialog(
      title = tags$div(icon("pen"), " Edit Parameter"),
      size = "m", easyClose = TRUE,
      
      layout_column_wrap(
        width = 1/3,
        textInput("qk_edit_param_name", "Parameter:", value = row$Parameter, width = "100%"),
        numericInput("qk_edit_param_lower", "Lower limit:", value = row$Lower, width = "100%"),
        numericInput("qk_edit_param_upper", "Upper limit:", value = row$Higher, width = "100%")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("qk_save_edit_param", "Save", class = "btn-primary", icon = icon("check"))
      )
    ))
  })
  
  observeEvent(input$qk_save_edit_param, {
    row_idx <- input$qk_limits_table_rows_selected
    if (!is.null(row_idx) && length(row_idx) > 0) {
      rv_qk$limits$Parameter[row_idx] <- trimws(input$qk_edit_param_name)
      rv_qk$limits$Lower[row_idx] <- input$qk_edit_param_lower
      rv_qk$limits$Higher[row_idx] <- input$qk_edit_param_upper
      
      params <- rv_qk$limits$Parameter
      updateSelectInput(session, "qk_parameter", choices = params, selected = params[row_idx])
      
      showNotification("Parameter updated. Click Save to persist.", type = "message", duration = 3)
    }
    removeModal()
  })
  
  # Save limits to file
  observeEvent(input$qk_save_limits, {
    req(rv_qk$limits)
    tryCatch({
      write.csv(rv_qk$limits, qk_limits_path(), row.names = FALSE)
      showNotification(paste0("QC limits saved (", nrow(rv_qk$limits), " parameters)."), 
                       type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste0("Error saving limits: ", e$message), type = "error")
    })
  })
  
  # Download limits
  output$qk_download_limits <- downloadHandler(
    filename = function() {
      paste0("QC_limits_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv_qk$limits)
      write.csv(rv_qk$limits, file, row.names = FALSE)
    }
  )
  
  # Upload limits file
  observeEvent(input$qk_upload_limits, {
    req(input$qk_upload_limits)
    file <- input$qk_upload_limits
    ext <- tolower(tools::file_ext(file$name))
    
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(readxl::read_excel(file$datapath, col_types = "text"))
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Could not read limits file.", type = "error")
      return()
    }
    
    # Try to find Parameter, Lower, Higher columns
    names_lower <- tolower(names(df))
    param_col <- which(names_lower %in% c("parameter", "name", "analyte", "metabolite"))[1]
    lower_col <- which(names_lower %in% c("lower", "lower_limit", "min", "low"))[1]
    upper_col <- which(names_lower %in% c("higher", "upper", "upper_limit", "max", "high"))[1]
    
    if (is.na(param_col)) {
      showNotification("Could not find Parameter column. Expected: Parameter, Name, Analyte, or Metabolite", 
                       type = "error", duration = 5)
      return()
    }
    
    result <- data.frame(
      Parameter = trimws(as.character(df[[param_col]])),
      Lower = if (!is.na(lower_col)) suppressWarnings(as.numeric(df[[lower_col]])) else NA_real_,
      Higher = if (!is.na(upper_col)) suppressWarnings(as.numeric(df[[upper_col]])) else NA_real_,
      stringsAsFactors = FALSE
    )
    
    # Remove empty rows
    result <- result[nchar(result$Parameter) > 0 & !is.na(result$Parameter), ]
    
    if (nrow(result) == 0) {
      showNotification("No valid parameters found in file.", type = "warning")
      return()
    }
    
    rv_qk$limits <- result
    
    params <- result$Parameter
    updateSelectInput(session, "qk_parameter", choices = params, selected = params[1])
    
    showNotification(paste0("Imported ", nrow(result), " parameters. Click Save to persist."), 
                     type = "message", duration = 4)
  })
  
  output$qk_upload_limits_status <- renderUI({
    if (is.null(rv_qk$limits) || nrow(rv_qk$limits) == 0) {
      return(tags$div(
        style = "color: #e67e22; font-size: 0.85em;",
        icon("triangle-exclamation"),
        " No QC limits configured yet. Add parameters or upload a limits file."
      ))
    }
    tags$div(
      style = "color: #18bc9c; font-size: 0.85em;",
      icon("check-circle"),
      paste0(" ", nrow(rv_qk$limits), " parameters configured")
    )
  })

  # Detect QK samples after XML processing - ask user before adding
  observeEvent(rv$xml_m, {
    req(rv$xml_m)
    
    # Get QC sample pattern (configurable)
    qk_pattern <- if (!is.null(input$qk_sample_pattern) && nchar(trimws(input$qk_sample_pattern)) > 0) {
      input$qk_sample_pattern
    } else {
      "^[Qq][KkCc][12]?"
    }
    
    all_ids <- unique(rv$xml_m$ID)
    qk_ids <- all_ids[grepl(qk_pattern, all_ids)]
    
    if (length(qk_ids) == 0) return()
    
    existing_names <- if (!is.null(rv_qk$data)) as.character(rv_qk$data$Name) else character(0)
    new_qk_ids <- qk_ids[!qk_ids %in% existing_names]
    
    if (length(new_qk_ids) == 0) return()
    
    # Store detected QK IDs for later use
    rv_qk$pending_ids <- new_qk_ids
    
    # Ask user
    showModal(modalDialog(
      title = tags$div(
        icon("flask-vial", style = "color: #3498db;"),
        " QC Samples Detected"
      ),
      size = "m", easyClose = TRUE,
      
      tags$div(
        style = "padding: 10px;",
        tags$div(
          style = "background: #d1ecf1; border-radius: 8px; padding: 12px; margin-bottom: 15px;",
          icon("info-circle", style = "color: #17a2b8;"),
          tags$strong(paste0(" ", length(new_qk_ids), " new QC sample(s) detected:")),
          tags$div(
            style = "margin-top: 8px; display: flex; flex-wrap: wrap; gap: 4px;",
            lapply(new_qk_ids, function(id) {
              tags$span(class = "badge bg-info", style = "font-size: 0.85em;", id)
            })
          )
        ),
        tags$p("Would you like to add these samples to the QC Monitor?"),
        tags$p(class = "text-muted", style = "font-size: 0.85em;",
               icon("info-circle"),
               " QC values will be extracted from the processed XML data and added to the monitoring database.")
      ),
      
      footer = tagList(
        actionButton("qk_skip_add", "Skip", class = "btn-secondary", icon = icon("forward")),
        actionButton("qk_confirm_add", "Add to QC Monitor", class = "btn-primary", icon = icon("plus"))
      )
    ))
  })
  
  # Skip QC add
  observeEvent(input$qk_skip_add, {
    rv_qk$pending_ids <- NULL
    removeModal()
    showNotification("QC samples skipped.", type = "message", duration = 3)
  })
  
  # Confirm QC add
  observeEvent(input$qk_confirm_add, {
    removeModal()
    
    new_qk_ids <- rv_qk$pending_ids
    if (is.null(new_qk_ids) || length(new_qk_ids) == 0) return()
    
    # Get monitored params from limits (if any)
    qk_params <- if (!is.null(rv_qk$limits) && nrow(rv_qk$limits) > 0) rv_qk$limits$Parameter else character(0)
    
    new_rows <- lapply(new_qk_ids, function(qk_id) {
      # Get measurement date
      qk_meta <- rv$xml_m[rv$xml_m$ID == qk_id, ]
      meas_date <- if ("MeasDate" %in% names(qk_meta) && nrow(qk_meta) > 0) {
        as.character(qk_meta$MeasDate[1])
      } else {
        format(Sys.Date(), "%Y-%m-%d")
      }
      
      row <- data.frame(Name = as.character(qk_id), Date = as.character(meas_date),
                        stringsAsFactors = FALSE)
      
      # Collect ALL parameters from metabolite XML data for this QC sample
      all_params_to_extract <- character(0)
      
      if (!is.null(rv$xml_m)) {
        meta_data <- rv$xml_m[rv$xml_m$ID == qk_id, ]
        if (nrow(meta_data) > 0) {
          all_params_to_extract <- unique(c(all_params_to_extract, as.character(meta_data$Parameter)))
          for (j in seq_len(nrow(meta_data))) {
            param_name <- as.character(meta_data$Parameter[j])
            param_val <- suppressWarnings(as.numeric(meta_data$Value[j]))
            if (nchar(param_name) > 0 && !is.na(param_val)) {
              row[[param_name]] <- param_val
            }
          }
        }
      }
      
      # Also collect from lipoprotein XML data
      if (!is.null(rv$xml_l)) {
        lipo_data <- rv$xml_l[rv$xml_l$ID == qk_id, ]
        if (nrow(lipo_data) > 0) {
          for (j in seq_len(nrow(lipo_data))) {
            param_name <- as.character(lipo_data$Parameter[j])
            param_val <- suppressWarnings(as.numeric(lipo_data$Value[j]))
            if (nchar(param_name) > 0 && !is.na(param_val) && is.null(row[[param_name]])) {
              row[[param_name]] <- param_val
            }
          }
        }
      }
      
      row
    })
    
    new_df <- tryCatch({
      # Ensure all rows have same columns
      all_cols <- unique(unlist(lapply(new_rows, names)))
      for (k in seq_along(new_rows)) {
        for (col in all_cols) {
          if (!col %in% names(new_rows[[k]])) new_rows[[k]][[col]] <- NA_real_
        }
      }
      do.call(rbind, new_rows)
    }, error = function(e) {
      showNotification(paste0("Error building QC data: ", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(new_df) || nrow(new_df) == 0) return()
    
    if (!is.null(rv_qk$data) && nrow(rv_qk$data) > 0) {
      existing <- rv_qk$data
      existing$Name <- as.character(existing$Name)
      existing$Date <- as.character(existing$Date)
      all_cols <- union(names(existing), names(new_df))
      for (col in all_cols) {
        if (!col %in% names(existing)) existing[[col]] <- NA_real_
        if (!col %in% names(new_df)) new_df[[col]] <- NA_real_
      }
      new_df <- new_df[, names(existing), drop = FALSE]
      rv_qk$data <- rbind(existing, new_df)
    } else {
      rv_qk$data <- new_df
    }
    
    # Report what was found
    skip_cols <- c("Name", "Date")
    data_cols <- setdiff(names(new_df), skip_cols)
    found_params <- character(0)
    for (.p in data_cols) {
      vals <- suppressWarnings(as.numeric(new_df[[.p]]))
      if (!all(is.na(vals))) found_params <- c(found_params, .p)
    }
    
    tryCatch({
      fwrite(rv_qk$data, qk_data_path())
      msg <- paste0(nrow(new_df), " QC sample(s) added: ", paste(new_qk_ids, collapse = ", "),
                    " | ", length(found_params), " parameters extracted")
      if (length(qk_params) > 0) {
        missing_in_limits <- qk_params[!qk_params %in% found_params]
        if (length(missing_in_limits) > 0) {
          msg <- paste0(msg, " | Not found: ", paste(missing_in_limits, collapse = ", "))
        }
      }
      showNotification(msg, type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste0("Error saving QC data: ", e$message), type = "error")
    })
    
    rv_qk$pending_ids <- NULL
  })



  # ---- QK KPIs ----
  output$qk_kpi_total <- renderText({
    if (is.null(rv_qk$data)) return("0")
    as.character(nrow(rv_qk$data))
  })

  output$qk_kpi_total_inline <- renderText({
    if (is.null(rv_qk$data)) return("0")
    paste0(nrow(rv_qk$data), " Mess.")
  })

  output$qk_kpi_pass <- renderText({
    outliers <- qk_outliers()
    if (is.null(rv_qk$data)) return("0")
    total <- nrow(rv_qk$data)
    failed_samples <- length(unique(outliers$Name))
    as.character(total - failed_samples)
  })

  output$qk_kpi_fail <- renderText({
    outliers <- qk_outliers()
    if (nrow(outliers) == 0) return("0")
    as.character(length(unique(outliers$Name)))
  })

  output$qk_kpi_ool_inline <- renderText({
    outliers <- qk_outliers()
    if (nrow(outliers) == 0) return("0 OOL")
    paste0(nrow(outliers), " OOL")
  })

  output$qk_kpi_last_date <- renderText({
    if (is.null(rv_qk$data) || nrow(rv_qk$data) == 0) return("-")
    dates <- as.Date(rv_qk$data$Date, format = "%Y-%m-%d")
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) return("-")
    format(max(dates), "%d.%m.%Y")
  })

  output$qk_status_text <- renderText({
    if (is.null(rv_qk$data)) return("No QC data loaded.")
    outliers <- qk_outliers()
    paste0(nrow(rv_qk$data), " measurements loaded\n",
           nrow(outliers), " measurements ouf ot bounds")
  })

  # ---- QK TREND PLOT ----

  # ---- QC Lot Management Handlers ----
  
  # Active lot badge
  output$qk_lot_badge <- renderUI({
    lot <- rv_qk$active_lot
    if (is.null(lot) || lot == "") {
      tags$span(class = "badge bg-secondary", style = "font-size: 0.75em;", "No lot")
    } else {
      tags$span(class = "badge bg-success", style = "font-size: 0.75em;", lot)
    }
  })
  
  # Active lot info display
  output$qk_active_lot_info <- renderUI({
    lot_name <- rv_qk$active_lot
    lots <- rv_qk$lots
    
    if (is.null(lot_name) || lot_name == "") {
      return(tags$div(
        style = "text-align: center; color: #95a5a6; padding: 8px; font-size: 0.85em;",
        icon("info-circle"),
        tags$br(),
        "No active lot. Create one to start tracking."
      ))
    }
    
    lot_info <- lots[[lot_name]]
    if (is.null(lot_info)) {
      return(tags$div(style = "font-size: 0.85em;",
        tags$strong(lot_name)))
    }
    
    n_measurements <- if (!is.null(rv_qk$data)) nrow(rv_qk$data) else 0
    
    tags$div(
      style = "font-size: 0.85em;",
      tags$div(style = "display: flex; justify-content: space-between;",
        tags$span(style = "font-weight: 600;", lot_name),
        tags$span(class = "badge bg-primary", paste0(n_measurements, " meas."))
      ),
      if (!is.null(lot_info$start_date)) tags$div(
        style = "color: #7f8c8d; font-size: 0.85em; margin-top: 2px;",
        paste0("Started: ", lot_info$start_date)
      ),
      if (!is.null(lot_info$material) && nchar(lot_info$material) > 0) tags$div(
        style = "color: #7f8c8d; font-size: 0.85em;",
        paste0("Material: ", lot_info$material)
      ),
      if (!is.null(lot_info$notes) && nchar(lot_info$notes) > 0) tags$div(
        style = "color: #7f8c8d; font-size: 0.85em; font-style: italic;",
        lot_info$notes
      )
    )
  })
  
  # Create new lot dialog
  observeEvent(input$qk_new_lot, {
    showModal(modalDialog(
      title = tags$div(icon("plus"), " Create New QC Lot"),
      size = "m",
      textInput("qk_lot_name", "Lot Name:",
                value = paste0("Lot_", format(Sys.Date(), "%Y-%m")),
                placeholder = "e.g. Lot_2026-09_PoolPlasma",
                width = "100%"),
      dateInput("qk_lot_start_date", "Start Date:",
                value = Sys.Date(), width = "100%"),
      textInput("qk_lot_material", "Material / Reference:",
                placeholder = "e.g. Bruker QC Plasma Lot 12345",
                width = "100%"),
      textAreaInput("qk_lot_notes", "Notes:",
                    placeholder = "Optional notes about this lot...",
                    rows = 2, width = "100%"),
      checkboxInput("qk_lot_copy_limits", "Copy current limits to new lot", value = TRUE),
      tags$div(
        class = "alert alert-info", style = "font-size: 0.85em; margin-top: 10px;",
        icon("info-circle"),
        " Creating a new lot will start fresh QC data collection. ",
        "The current data will remain until you archive it."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("qk_lot_create_confirm", "Create Lot",
                     class = "btn-success", icon = icon("plus"))
      )
    ))
  })
  
  # Confirm create lot
  observeEvent(input$qk_lot_create_confirm, {
    removeModal()
    
    lot_name <- trimws(input$qk_lot_name)
    if (nchar(lot_name) == 0) {
      showNotification("Lot name cannot be empty.", type = "error")
      return()
    }
    
    # Sanitize lot name for folder
    lot_folder_name <- gsub("[^A-Za-z0-9_-]", "_", lot_name)
    lot_dir <- file.path(qk_lots_dir(), lot_folder_name)
    
    if (dir.exists(lot_dir)) {
      showNotification("A lot with this name already exists.", type = "error")
      return()
    }
    
    dir.create(lot_dir, recursive = TRUE)
    
    # Save lot metadata
    lot_meta <- list(
      name = lot_name,
      folder = lot_folder_name,
      start_date = as.character(input$qk_lot_start_date),
      material = input$qk_lot_material,
      notes = input$qk_lot_notes,
      created = as.character(Sys.time()),
      status = "active"
    )
    
    jsonlite::write_json(lot_meta, file.path(lot_dir, "meta.json"),
                         pretty = TRUE, auto_unbox = TRUE)
    
    # Copy current limits if requested
    if (isTRUE(input$qk_lot_copy_limits) && !is.null(rv_qk$limits) && nrow(rv_qk$limits) > 0) {
      write.csv(rv_qk$limits, file.path(lot_dir, "limits.csv"), row.names = FALSE)
    }
    
    # Create empty data file for the new lot
    empty_df <- data.frame(Name = character(0), Date = character(0), stringsAsFactors = FALSE)
    fwrite(empty_df, file.path(lot_dir, "data.csv"))
    
    # Update registry
    reg <- load_qk_lots()
    reg$lots[[lot_name]] <- lot_meta
    reg$active <- lot_name
    save_qk_lots(reg)
    
    # Update reactive values
    rv_qk$lots <- reg$lots
    rv_qk$active_lot <- lot_name
    
    # Clear current data for fresh start
    rv_qk$data <- empty_df
    fwrite(empty_df, qk_data_path())
    
    showNotification(paste0("Lot created: ", lot_name), type = "message", duration = 5)
  })
  
  # Archive current lot dialog
  observeEvent(input$qk_archive_lot, {
    lot_name <- rv_qk$active_lot
    n_data <- if (!is.null(rv_qk$data)) nrow(rv_qk$data) else 0
    
    if (is.null(lot_name) || lot_name == "") {
      showNotification("No active lot to archive.", type = "warning")
      return()
    }
    
    showModal(modalDialog(
      title = tags$div(icon("box-archive"), " Archive Lot"),
      size = "m",
      tags$div(
        class = "alert alert-warning",
        tags$strong("You are about to archive:"),
        tags$br(),
        tags$span(style = "font-size: 1.1em; font-weight: 600;", lot_name),
        tags$br(),
        tags$span(paste0(n_data, " measurements will be archived."))
      ),
      tags$p("This will:"),
      tags$ul(
        tags$li("Save all current QC data and limits to the lot archive"),
        tags$li("Mark the lot as archived"),
        tags$li("Clear the current QC data for a fresh start")
      ),
      textAreaInput("qk_archive_notes", "Archive Notes (optional):",
                    placeholder = "Reason for archiving, observations...",
                    rows = 2, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("qk_archive_confirm", "Archive & Close Lot",
                     class = "btn-warning", icon = icon("box-archive"))
      )
    ))
  })
  
  # Confirm archive
  observeEvent(input$qk_archive_confirm, {
    removeModal()
    
    lot_name <- rv_qk$active_lot
    if (is.null(lot_name) || lot_name == "") return()
    
    reg <- load_qk_lots()
    lot_info <- reg$lots[[lot_name]]
    if (is.null(lot_info)) return()
    
    lot_folder_name <- lot_info$folder
    lot_dir <- file.path(qk_lots_dir(), lot_folder_name)
    if (!dir.exists(lot_dir)) dir.create(lot_dir, recursive = TRUE)
    
    # Save current data to lot archive
    if (!is.null(rv_qk$data) && nrow(rv_qk$data) > 0) {
      fwrite(rv_qk$data, file.path(lot_dir, "data.csv"))
    }
    if (!is.null(rv_qk$limits) && nrow(rv_qk$limits) > 0) {
      write.csv(rv_qk$limits, file.path(lot_dir, "limits.csv"), row.names = FALSE)
    }
    
    # Update lot metadata
    lot_info$status <- "archived"
    lot_info$end_date <- as.character(Sys.Date())
    lot_info$archive_notes <- input$qk_archive_notes
    lot_info$n_measurements <- if (!is.null(rv_qk$data)) nrow(rv_qk$data) else 0
    
    jsonlite::write_json(lot_info, file.path(lot_dir, "meta.json"),
                         pretty = TRUE, auto_unbox = TRUE)
    
    # Update registry
    reg$lots[[lot_name]] <- lot_info
    reg$active <- NULL
    save_qk_lots(reg)
    
    # Clear current data
    empty_df <- data.frame(Name = character(0), Date = character(0), stringsAsFactors = FALSE)
    rv_qk$data <- empty_df
    fwrite(empty_df, qk_data_path())
    
    rv_qk$lots <- reg$lots
    rv_qk$active_lot <- NULL
    
    showNotification(paste0("Lot archived: ", lot_name, " (", lot_info$n_measurements, " measurements)"),
                     type = "message", duration = 6)
  })
  
  # Browse archived lots dialog
  observeEvent(input$qk_browse_lots, {
    reg <- load_qk_lots()
    lots <- reg$lots
    
    if (length(lots) == 0) {
      showNotification("No lots found. Create your first lot.", type = "info")
      return()
    }
    
    # Build lot summary table
    lot_rows <- lapply(names(lots), function(nm) {
      lot <- lots[[nm]]
      status <- if (!is.null(lot$status)) lot$status else "unknown"
      n_meas <- if (!is.null(lot$n_measurements)) lot$n_measurements else "?"
      start <- if (!is.null(lot$start_date)) lot$start_date else "-"
      end_d <- if (!is.null(lot$end_date)) lot$end_date else "-"
      material <- if (!is.null(lot$material) && nchar(lot$material) > 0) lot$material else "-"
      
      status_badge <- if (status == "active") {
        tags$span(class = "badge bg-success", "Active")
      } else if (status == "archived") {
        tags$span(class = "badge bg-secondary", "Archived")
      } else {
        tags$span(class = "badge bg-warning", status)
      }
      
      tags$tr(
        tags$td(style = "padding: 6px 8px; font-weight: 500;", nm),
        tags$td(style = "padding: 6px 8px; text-align: center;", status_badge),
        tags$td(style = "padding: 6px 8px; text-align: center;", as.character(n_meas)),
        tags$td(style = "padding: 6px 8px;", start),
        tags$td(style = "padding: 6px 8px;", end_d),
        tags$td(style = "padding: 6px 8px;", material)
      )
    })
    
    showModal(modalDialog(
      title = tags$div(icon("folder-open"), " QC Lot Archive"),
      size = "l",
      tags$div(
        style = "max-height: 400px; overflow-y: auto;",
        tags$table(
          class = "table table-sm table-hover",
          style = "margin: 0;",
          tags$thead(
            style = "position: sticky; top: 0; background: #f8f9fa;",
            tags$tr(
              tags$th(style = "padding: 6px 8px;", "Lot Name"),
              tags$th(style = "padding: 6px 8px; text-align: center;", "Status"),
              tags$th(style = "padding: 6px 8px; text-align: center;", "Measurements"),
              tags$th(style = "padding: 6px 8px;", "Start"),
              tags$th(style = "padding: 6px 8px;", "End"),
              tags$th(style = "padding: 6px 8px;", "Material")
            )
          ),
          tags$tbody(lot_rows)
        )
      ),
      tags$hr(),
      tags$div(
        style = "display: flex; gap: 8px; align-items: center;",
        selectInput("qk_lot_select", "Load archived lot:",
                    choices = c("Select lot..." = "", names(lots)),
                    width = "300px"),
        actionButton("qk_lot_load", "Load Data",
                     class = "btn-outline-primary btn-sm", icon = icon("download")),
        actionButton("qk_lot_restore", "Restore as Active",
                     class = "btn-outline-success btn-sm", icon = icon("rotate-left")),
        actionButton("qk_lot_delete", "Delete Lot",
                     class = "btn-outline-danger btn-sm", icon = icon("trash"))
      ),
      footer = modalButton("Close")
    ))
  })
  
  # Load archived lot data (read-only view)
  observeEvent(input$qk_lot_load, {
    req(input$qk_lot_select, nchar(input$qk_lot_select) > 0)
    
    lot_name <- input$qk_lot_select
    reg <- load_qk_lots()
    lot_info <- reg$lots[[lot_name]]
    if (is.null(lot_info)) return()
    
    lot_dir <- file.path(qk_lots_dir(), lot_info$folder)
    data_file <- file.path(lot_dir, "data.csv")
    limits_file <- file.path(lot_dir, "limits.csv")
    
    if (file.exists(data_file)) {
      lot_data <- tryCatch({
        df <- fread(data_file, stringsAsFactors = FALSE)
        as.data.frame(df)
      }, error = function(e) NULL)
      
      if (!is.null(lot_data)) {
        rv_qk$data <- lot_data
        if (file.exists(limits_file)) {
          rv_qk$limits <- tryCatch({
            df <- fread(limits_file, stringsAsFactors = FALSE)
            as.data.frame(df)
          }, error = function(e) rv_qk$limits)
        }
        showNotification(paste0("Loaded lot: ", lot_name, " (", nrow(lot_data), " measurements)"),
                         type = "message", duration = 4)
      }
    } else {
      showNotification("No data file found for this lot.", type = "warning")
    }
  })
  
  # Restore archived lot as active
  observeEvent(input$qk_lot_restore, {
    req(input$qk_lot_select, nchar(input$qk_lot_select) > 0)
    removeModal()
    
    lot_name <- input$qk_lot_select
    reg <- load_qk_lots()
    lot_info <- reg$lots[[lot_name]]
    if (is.null(lot_info)) return()
    
    # Load the lot data into current
    lot_dir <- file.path(qk_lots_dir(), lot_info$folder)
    data_file <- file.path(lot_dir, "data.csv")
    limits_file <- file.path(lot_dir, "limits.csv")
    
    if (file.exists(data_file)) {
      lot_data <- tryCatch(as.data.frame(fread(data_file)), error = function(e) NULL)
      if (!is.null(lot_data)) {
        rv_qk$data <- lot_data
        fwrite(lot_data, qk_data_path())
      }
    }
    if (file.exists(limits_file)) {
      lot_limits <- tryCatch(as.data.frame(fread(limits_file)), error = function(e) NULL)
      if (!is.null(lot_limits)) {
        rv_qk$limits <- lot_limits
        write.csv(lot_limits, qk_limits_path(), row.names = FALSE)
      }
    }
    
    # Mark as active
    lot_info$status <- "active"
    reg$lots[[lot_name]] <- lot_info
    reg$active <- lot_name
    save_qk_lots(reg)
    
    rv_qk$lots <- reg$lots
    rv_qk$active_lot <- lot_name
    
    showNotification(paste0("Restored lot: ", lot_name), type = "message", duration = 4)
  })
  
  # Delete lot
  observeEvent(input$qk_lot_delete, {
    req(input$qk_lot_select, nchar(input$qk_lot_select) > 0)
    
    lot_name <- input$qk_lot_select
    
    if (identical(lot_name, rv_qk$active_lot)) {
      showNotification("Cannot delete the active lot. Archive it first.", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = tags$div(icon("trash"), " Delete Lot"),
      tags$div(
        class = "alert alert-danger",
        tags$strong("Permanently delete lot: "), lot_name,
        tags$br(),
        "This action cannot be undone. All data will be lost."
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("qk_lot_delete_confirm", "Delete Permanently",
                     class = "btn-danger", icon = icon("trash"))
      )
    ))
  })
  
  observeEvent(input$qk_lot_delete_confirm, {
    removeModal()
    lot_name <- input$qk_lot_select
    reg <- load_qk_lots()
    lot_info <- reg$lots[[lot_name]]
    if (is.null(lot_info)) return()
    
    # Delete folder
    lot_dir <- file.path(qk_lots_dir(), lot_info$folder)
    if (dir.exists(lot_dir)) unlink(lot_dir, recursive = TRUE)
    
    # Remove from registry
    reg$lots[[lot_name]] <- NULL
    if (identical(reg$active, lot_name)) reg$active <- NULL
    save_qk_lots(reg)
    
    rv_qk$lots <- reg$lots
    rv_qk$active_lot <- reg$active
    
    showNotification(paste0("Deleted lot: ", lot_name), type = "message", duration = 4)
  })
  
  # ---- Auto-save to active lot ----
  # When QC data changes, also save to the active lot folder
  observe({
    req(rv_qk$active_lot)
    req(rv_qk$data)
    
    lot_name <- rv_qk$active_lot
    reg <- load_qk_lots()
    lot_info <- reg$lots[[lot_name]]
    if (is.null(lot_info)) return()
    
    lot_dir <- file.path(qk_lots_dir(), lot_info$folder)
    if (!dir.exists(lot_dir)) return()
    
    # Save data to lot folder
    tryCatch({
      fwrite(rv_qk$data, file.path(lot_dir, "data.csv"))
      # Update measurement count
      lot_info$n_measurements <- nrow(rv_qk$data)
      reg$lots[[lot_name]] <- lot_info
      save_qk_lots(reg)
    }, error = function(e) NULL)
  })

  output$qk_trend_plot <- renderPlotly({
    req(rv_qk$data, input$qk_parameter)

    param <- input$qk_parameter
    if (!param %in% names(rv_qk$data)) return(empty_plot("Parameter not found"))

    df <- rv_qk$data

    # Parse dates - try multiple formats
    df$DateParsed <- as.Date(df$Date, format = "%Y-%m-%d")
    if (all(is.na(df$DateParsed))) {
      df$DateParsed <- as.Date(df$Date, format = "%d.%m.%Y")
    }
    # Fill remaining NAs with other formats
    na_dates <- is.na(df$DateParsed)
    if (any(na_dates)) {
      df$DateParsed[na_dates] <- as.Date(df$Date[na_dates], format = "%d.%m.%Y")
    }
    na_dates <- is.na(df$DateParsed)
    if (any(na_dates)) {
      df$DateParsed[na_dates] <- as.Date(df$Date[na_dates], format = "%d-%b-%Y")
    }

    df$Value <- as.numeric(df[[param]])
    df <- df[!is.na(df$Value) & !is.na(df$DateParsed), ]

    # Sort by date
    df <- df[order(df$DateParsed), ]

    if (nrow(df) == 0) return(empty_plot("No data"))

    # Get limits
    limits <- rv_qk$limits
    limit_row <- limits[limits$Parameter == param, ]
    lower <- if (nrow(limit_row) > 0) as.numeric(limit_row$Lower) else NA
    upper <- if (nrow(limit_row) > 0) as.numeric(limit_row$Higher) else NA

    df$Status <- ifelse(
      !is.na(lower) & !is.na(upper),
      ifelse(df$Value >= lower & df$Value <= upper, "OK", "OOL"),
      "OK"
    )

    colors <- c("OK" = "#18bc9c", "OOL" = "#e74c3c")

    p <- plot_ly(df, x = ~DateParsed, y = ~Value, type = "scatter", mode = "markers+lines",
                 color = ~Status, colors = colors,
                 text = ~paste0(Name, "\n", format(DateParsed, "%d.%m.%Y"), "\n", param, ": ", round(Value, 3)),
                 hoverinfo = "text",
                 marker = list(size = 8)) %>%
      layout(
        title = list(text = param, x = 0),
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(title = param),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.15)
      )

    if (input$qk_show_limits && !is.na(lower) && !is.na(upper)) {
      p <- p %>%
        add_segments(x = min(df$DateParsed), xend = max(df$DateParsed),
                     y = upper, yend = upper,
                     line = list(color = "#e74c3c", dash = "dash", width = 1.5),
                     name = paste0("Upper (", upper, ")"),
                     inherit = FALSE, showlegend = TRUE) %>%
        add_segments(x = min(df$DateParsed), xend = max(df$DateParsed),
                     y = lower, yend = lower,
                     line = list(color = "#e74c3c", dash = "dash", width = 1.5),
                     name = paste0("Lower (", lower, ")"),
                     inherit = FALSE, showlegend = TRUE)
    }

    mean_val <- mean(df$Value, na.rm = TRUE)
    p <- p %>%
      add_segments(x = min(df$DateParsed), xend = max(df$DateParsed),
                   y = mean_val, yend = mean_val,
                   line = list(color = "#3498db", dash = "dot", width = 1),
                   name = paste0("Mean (", round(mean_val, 2), ")"),
                   inherit = FALSE, showlegend = TRUE)

    p
  })

  # ---- QK DATA TABLE ----
  output$qk_data_table <- renderDT({
    req(rv_qk$data, rv_qk$limits)

    df <- rv_qk$data
    limits <- rv_qk$limits

    # Sort by date descending (most recent first)
    df$DateSort <- as.Date(df$Date, format = "%Y-%m-%d")
    df <- df[order(df$DateSort, decreasing = TRUE), ]
    df$DateSort <- NULL

    # Select display columns
    param_cols <- limits$Parameter[limits$Parameter %in% names(df)]
    display_df <- df[, c("Name", "Date", param_cols), drop = FALSE]

    dt <- datatable(display_df, class = "compact stripe hover",
                    options = list(scrollX = TRUE, pageLength = 20,
                                   order = list(list(1, "desc")),
                                   language = list(
                                     url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                                   )),
                    rownames = FALSE)

    # Highlight out-of-limit values
    for (i in seq_len(nrow(limits))) {
      param <- limits$Parameter[i]
      if (!param %in% names(display_df)) next
      lower <- as.numeric(limits$Lower[i])
      upper <- as.numeric(limits$Higher[i])

      dt <- dt %>%
        formatStyle(param,
                    backgroundColor = styleInterval(
                      c(lower, upper),
                      c("#fadbd8", "#d5f5e3", "#fadbd8")
                    ))
    }

    dt
  })




  empty_plot <- function(message = "No data available") {
    plot_ly() %>%
      add_annotations(
        text = message,
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 14, color = "#95a5a6")
      ) %>%
      layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE)
      )
  }

  # ---- QK OUTLIER TABLE ----
  output$qk_outlier_table <- renderDT({
    outliers <- qk_outliers()
    if (nrow(outliers) == 0) {
      return(datatable(
        data.frame(Info = "No outliers found - all values within limits!"),
        rownames = FALSE
      ))
    }

    datatable(outliers, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 25,
                             order = list(list(1, "desc")),
                             language = list(
                               url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json"
                             )),
              rownames = FALSE) %>%
      formatStyle("Deviation",
                  color = styleEqual(
                    grep("above", outliers$Deviation, value = TRUE),
                    rep("#e74c3c", sum(grepl("above", outliers$Deviation)))
                  )) %>%
      formatStyle("Value",
                  fontWeight = "bold",
                  color = "#c0392b")
  })

# measurement archive -----------------------------------------------------

  # ---- Populate NMR project dropdown from ARCHIVE (not projects table) ----
  observe({
    req(rv_archive$data)
    df <- rv_archive$data
    
    if (nrow(df) == 0) return()
    
    # Get unique projects from archive with sample counts
    project_counts <- df %>%
      filter(!is.na(Project) & nchar(Project) > 0) %>%
      count(Project, name = "n") %>%
      arrange(desc(n))
    
    # Create choices with sample count in label
    choices <- setNames(
      project_counts$Project,
      paste0(project_counts$Project, " (", project_counts$n, " samples)")
    )
    
    updateSelectizeInput(session, "nmr_project_select",
                         choices = choices, server = TRUE,
                         options = list(placeholder = "Search project..."))
  })
  


  # File path
  meas_archive_path <- file.path(APP_DIR, "Measurement_Archive.xlsx")
  
  # Reactive values
  rv_meas_archive <- reactiveValues(
    data = tryCatch({
      if (file.exists(meas_archive_path)) {
        openxlsx::read.xlsx(meas_archive_path, sheet = "Overview")
      } else {
        data.frame(
          Date = character(), Project = character(),
          Sample_Type = character(), N_Samples = integer(),
          N_Metabolites = integer(), N_Lipids = integer(),
          QC_Pass = integer(), QC_Fail = integer(),
          Note = character(), stringsAsFactors = FALSE
        )
      }
    }, error = function(e) {
      data.frame(
        Date = character(), Project = character(),
        Sample_Type = character(), N_Samples = integer(),
        N_Metabolites = integer(), N_Lipids = integer(),
        QC_Pass = integer(), QC_Fail = integer(),
        Note = character(), stringsAsFactors = FALSE
      )
    })
  )

  # Update project selector for XML parser
  observeEvent(rv_projects$data, {
    choices <- sort(rv_projects$data$Abbreviation[!is.na(rv_projects$data$Abbreviation) &
                                                    nchar(rv_projects$data$Abbreviation) > 0])
    updateSelectInput(session, "xml_project_select",
                      choices = c("Please select..." = "", choices))
  }, ignoreNULL = TRUE)

  # ---- ARCHIVE FUNCTION ----
  archive_measurement_data <- function(project_name, overwrite = FALSE, skip_ids = character(0)) {

    meas_date <- if (!is.null(rv$xml_m) && "MeasDate" %in% names(rv$xml_m)) {
      rv$xml_m$MeasDate[1]
    } else {
      format(Sys.Date(), "%Y-%m-%d")
    }

    # Filter out skipped IDs
    xml_m_filtered <- if (length(skip_ids) > 0 && !is.null(rv$xml_m)) {
      rv$xml_m[!rv$xml_m$ID %in% skip_ids]
    } else {
      rv$xml_m
    }

    xml_l_filtered <- if (length(skip_ids) > 0 && !is.null(rv$xml_l)) {
      rv$xml_l[!rv$xml_l$ID %in% skip_ids]
    } else {
      rv$xml_l
    }

    n_samples <- length(unique(c(
      if (!is.null(xml_m_filtered) && nrow(xml_m_filtered) > 0) xml_m_filtered$ID else character(0),
      if (!is.null(xml_l_filtered) && nrow(xml_l_filtered) > 0) xml_l_filtered$ID else character(0)
    )))

    if (n_samples == 0) {
      showNotification("No new samples to archive.", type = "warning", duration = 3)
      return()
    }

    n_metabolites <- if (!is.null(xml_m_filtered) && nrow(xml_m_filtered) > 0) length(unique(xml_m_filtered$ID)) else 0
    n_lipids <- if (!is.null(xml_l_filtered) && nrow(xml_l_filtered) > 0) length(unique(xml_l_filtered$ID)) else 0

    # QC for non-skipped samples
    qc_pass <- 0
    qc_fail <- 0
    if (!is.null(rv_qc$overall) && nrow(rv_qc$overall) > 0) {
      qc_filtered <- rv_qc$overall[!rv_qc$overall$ID %in% skip_ids, ]
      qc_pass <- sum(qc_filtered$Overall_Status == "PASS")
      qc_fail <- sum(qc_filtered$Overall_Status == "FAIL")
    }

    # New overview row
    new_row <- data.frame(
      Date = as.character(meas_date),
      Project = project_name,
      Sample_Type = ifelse(input$sample_type == "blood", "Blood", "Urine"),
      N_Samples = n_samples,
      N_Metabolites = n_metabolites,
      N_Lipids = n_lipids,
      QC_Pass = qc_pass,
      QC_Fail = qc_fail,
      Note = ifelse(is.null(input$xml_batch_note) || nchar(input$xml_batch_note) == 0,
                    "", input$xml_batch_note),
      stringsAsFactors = FALSE
    )

    rv_meas_archive$data <- rbind(rv_meas_archive$data, new_row)

    tryCatch({
      wb <- createWorkbook()

      # Sheet 1: Overview
      addWorksheet(wb, "Overview")
      writeData(wb, "Overview", rv_meas_archive$data)
      setColWidths(wb, "Overview", cols = 1:ncol(rv_meas_archive$data), widths = "auto")

      # Sheet 2: Metabolites
      if (!is.null(xml_m_filtered) && nrow(xml_m_filtered) > 0) {
        wide_m <- dcast_ordered(xml_m_filtered, "Value")
        wide_m$Project <- project_name
        wide_m$Measurement_Date <- meas_date
        wide_m$Extraction_Date <- format(Sys.Date(), "%Y-%m-%d")

        meta_cols <- c("Project", "Measurement_Date", "Extraction_Date", "ID")
        other_cols <- setdiff(names(wide_m), meta_cols)
        wide_m <- wide_m[, c(meta_cols, other_cols), with = FALSE]

        existing_m <- tryCatch({
          if (file.exists(meas_archive_path)) {
            as.data.table(openxlsx::read.xlsx(meas_archive_path, sheet = "Metabolites"))
          } else {
            NULL
          }
        }, error = function(e) NULL)

        if (overwrite && !is.null(existing_m) && nrow(existing_m) > 0) {
          existing_m$Measurement_Date <- as.character(existing_m$Measurement_Date)
          ids_to_remove <- unique(xml_m_filtered$ID)
          existing_m <- existing_m[!(existing_m$ID %in% ids_to_remove &
                                       existing_m$Measurement_Date == meas_date), ]
        }

        if (!is.null(existing_m) && nrow(existing_m) > 0) {
          all_cols <- union(names(existing_m), names(wide_m))
          for (col in all_cols) {
            if (!col %in% names(existing_m)) existing_m[[col]] <- NA
            if (!col %in% names(wide_m)) wide_m[[col]] <- NA
          }
          wide_m <- wide_m[, names(existing_m), with = FALSE]
          combined_m <- rbind(existing_m, wide_m)
        } else {
          combined_m <- wide_m
        }

        addWorksheet(wb, "Metabolites")
        writeData(wb, "Metabolites", combined_m)
      }

      # Sheet 3: Lipids
      if (!is.null(xml_l_filtered) && nrow(xml_l_filtered) > 0) {
        wide_l <- dcast_ordered(xml_l_filtered, "Value")
        wide_l$Project <- project_name
        wide_l$Measurement_Date <- meas_date
        wide_l$Extraction_Date <- format(Sys.Date(), "%Y-%m-%d")

        meta_cols <- c("Project", "Measurement_Date", "Extraction_Date", "ID")
        other_cols <- setdiff(names(wide_l), meta_cols)
        wide_l <- wide_l[, c(meta_cols, other_cols), with = FALSE]

        existing_l <- tryCatch({
          if (file.exists(meas_archive_path)) {
            as.data.table(openxlsx::read.xlsx(meas_archive_path, sheet = "Lipids"))
          } else {
            NULL
          }
        }, error = function(e) NULL)

        if (overwrite && !is.null(existing_l) && nrow(existing_l) > 0) {
          existing_l$Measurement_Date <- as.character(existing_l$Measurement_Date)
          ids_to_remove <- unique(xml_l_filtered$ID)
          existing_l <- existing_l[!(existing_l$ID %in% ids_to_remove &
                                       existing_l$Measurement_Date == meas_date), ]
        }

        if (!is.null(existing_l) && nrow(existing_l) > 0) {
          all_cols <- union(names(existing_l), names(wide_l))
          for (col in all_cols) {
            if (!col %in% names(existing_l)) existing_l[[col]] <- NA
            if (!col %in% names(wide_l)) wide_l[[col]] <- NA
          }
          wide_l <- wide_l[, names(existing_l), with = FALSE]
          combined_l <- rbind(existing_l, wide_l)
        } else {
          combined_l <- wide_l
        }

        addWorksheet(wb, "Lipids")
        writeData(wb, "Lipids", combined_l)
      }

      # Sheet 4: QC
      if (!is.null(rv_qc$overall) && nrow(rv_qc$overall) > 0) {
        qc_data <- rv_qc$overall[!rv_qc$overall$ID %in% skip_ids, ]
        qc_data$Project <- project_name
        qc_data$Measurement_Date <- meas_date
        qc_data$Extraction_Date <- format(Sys.Date(), "%Y-%m-%d")

        existing_qc <- tryCatch({
          if (file.exists(meas_archive_path)) {
            as.data.frame(openxlsx::read.xlsx(meas_archive_path, sheet = "QC"))
          } else {
            NULL
          }
        }, error = function(e) NULL)

        if (overwrite && !is.null(existing_qc) && nrow(existing_qc) > 0) {
          existing_qc$Measurement_Date <- as.character(existing_qc$Measurement_Date)
          ids_to_remove <- unique(qc_data$ID)
          existing_qc <- existing_qc[!(existing_qc$ID %in% ids_to_remove &
                                         existing_qc$Measurement_Date == meas_date), ]
        }

        if (!is.null(existing_qc) && nrow(existing_qc) > 0) {
          all_cols <- union(names(existing_qc), names(qc_data))
          for (col in all_cols) {
            if (!col %in% names(existing_qc)) existing_qc[[col]] <- NA
            if (!col %in% names(qc_data)) qc_data[[col]] <- NA
          }
          qc_data <- qc_data[, names(existing_qc)]
          combined_qc <- rbind(existing_qc, qc_data)
        } else {
          combined_qc <- qc_data
        }

        addWorksheet(wb, "QC")
        writeData(wb, "QC", combined_qc)
      }

      saveWorkbook(wb, meas_archive_path, overwrite = TRUE)

      # Create backup
      tryCatch({
        create_backup(type = "archive")
      }, error = function(e) NULL)

      skipped_msg <- if (length(skip_ids) > 0) {
        paste0(" | ", length(skip_ids), " Duplikate \u00fcbersprungen")
      } else {
        ""
      }

      showNotification(
        paste0("Measurement archive updated! Project: ", project_name,
               " | ", n_samples, " samples archived", skipped_msg),
        type = "message", duration = 5
      )

    }, error = function(e) {
      showNotification(paste0("Archiv-Fehler: ", e$message), type = "error", duration = 5)
    })
  }

  # ---- TRIGGER AFTER PROCESSING ----
  observeEvent(rv$processed, {
    if (!isTRUE(rv$processed)) return()

    project_name <- input$xml_project_select
    if (is.null(project_name) || nchar(project_name) == 0) {
      project_name <- "Unassigned"
    }

    # Duplicate check
    if (!is.null(rv$xml_m) && nrow(rv$xml_m) > 0) {
      current_ids <- unique(rv$xml_m$ID)
      current_date <- if ("MeasDate" %in% names(rv$xml_m)) {
        rv$xml_m$MeasDate[1]
      } else {
        format(Sys.Date(), "%Y-%m-%d")
      }

      existing_m <- tryCatch({
        if (file.exists(meas_archive_path)) {
          as.data.table(openxlsx::read.xlsx(meas_archive_path, sheet = "Metabolites"))
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(existing_m) && nrow(existing_m) > 0) {
        if ("Measurement_Date" %in% names(existing_m) && "ID" %in% names(existing_m)) {
          existing_m$Measurement_Date <- as.character(existing_m$Measurement_Date)
          duplicates <- current_ids[current_ids %in%
                                      existing_m$ID[existing_m$Measurement_Date == current_date]]

          if (length(duplicates) > 0) {
            showModal(modalDialog(
              title = tags$div(icon("triangle-exclamation", style = "color: #e74c3c;"),
                               " Duplicates present!"),
              tags$div(
                tags$p(tags$strong(paste0(length(duplicates), " of ", length(current_ids),
                                          " samples are already in the measurement archive:"))),
                tags$div(
                  style = "max-height: 150px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 6px;",
                  tags$code(paste(head(duplicates, 20), collapse = ", "),
                            if (length(duplicates) > 20) paste0("... (+", length(duplicates) - 20, " additional)"))
                ),
                tags$hr(),
                tags$p("Was m\u00f6chten Sie tun?")
              ),
              footer = tagList(
                actionButton("archive_skip_dupes", "Only archive new samples",
                             class = "btn-warning", icon = icon("forward")),
                actionButton("archive_overwrite", "Overwrite",
                             class = "btn-danger", icon = icon("rotate")),
                modalButton("Cancel")
              ),
              size = "m"
            ))
            return()
          }
        }
      }
    }

    # No duplicates - proceed
    archive_measurement_data(project_name, overwrite = FALSE, skip_ids = character(0))
  })

  # ---- HANDLE DUPLICATE DECISIONS ----
  observeEvent(input$archive_skip_dupes, {
    removeModal()
    project_name <- input$xml_project_select
    if (is.null(project_name) || nchar(project_name) == 0) project_name <- "Unassigned"

    current_ids <- unique(rv$xml_m$ID)
    current_date <- if ("MeasDate" %in% names(rv$xml_m)) {
      rv$xml_m$MeasDate[1]
    } else {
      format(Sys.Date(), "%Y-%m-%d")
    }

    existing_m <- tryCatch({
      as.data.table(openxlsx::read.xlsx(meas_archive_path, sheet = "Metabolites"))
    }, error = function(e) NULL)

    duplicates <- character(0)
    if (!is.null(existing_m) && "Measurement_Date" %in% names(existing_m) && "ID" %in% names(existing_m)) {
      existing_m$Measurement_Date <- as.character(existing_m$Measurement_Date)
      duplicates <- current_ids[current_ids %in%
                                  existing_m$ID[existing_m$Measurement_Date == current_date]]
    }

    archive_measurement_data(project_name, overwrite = FALSE, skip_ids = duplicates)
  })

  observeEvent(input$archive_overwrite, {
    removeModal()
    project_name <- input$xml_project_select
    if (is.null(project_name) || nchar(project_name) == 0) project_name <- "Unassigned"

    archive_measurement_data(project_name, overwrite = TRUE, skip_ids = character(0))
  })

  # ---- MEASUREMENT ARCHIVE VIEWER OUTPUTS ----
  output$archive_total_meas <- renderText({
    as.character(nrow(rv_meas_archive$data))
  })

  output$archive_total_projects <- renderText({
    if (nrow(rv_meas_archive$data) == 0) return("0")
    as.character(length(unique(rv_meas_archive$data$Project[rv_meas_archive$data$Project != "Unassigned"])))
  })

  output$archive_total_samples <- renderText({
    if (nrow(rv_meas_archive$data) == 0) return("0")
    as.character(sum(rv_meas_archive$data$N_Samples, na.rm = TRUE))
  })

  output$archive_last_date <- renderText({
    if (nrow(rv_meas_archive$data) == 0) return("-")
    dates <- parse_dates_safe(rv_meas_archive$data$Date)
    dates <- as.Date(dates)
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) return("-")
    format(max(dates), "%d.%m.%Y")
  })

  output$meas_archive_table <- renderDT({
    if (is.null(rv_meas_archive$data) || nrow(rv_meas_archive$data) == 0) {
      return(datatable(data.frame(Info = "No measurements archived yet."), rownames = FALSE))
    }
    
    df <- rv_meas_archive$data
    
    # Anonymize in demo mode
    if (rv_demo$active) {
      if ("Project" %in% names(df)) df$Project <- anonymize_projects(df$Project)
      if ("Project" %in% names(df)) df$Project <- anonymize_projects(df$Project)
      if ("Note" %in% names(df)) df$Note <- ""
      if ("Notiz" %in% names(df)) df$Notiz <- ""
      if ("File" %in% names(df)) df$File <- paste0("messung_", sprintf("%03d", seq_len(nrow(df))), ".xlsx")
      if ("Datei" %in% names(df)) df$Datei <- paste0("messung_", sprintf("%03d", seq_len(nrow(df))), ".xlsx")
      if ("Operator" %in% names(df)) df$Operator <- paste0("User_", sample(LETTERS[1:5], nrow(df), replace = TRUE))
    }
    
    datatable(df, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 20,
                             order = list(list(0, "desc"))
                             ),
              rownames = FALSE) %>%
      formatStyle("QC_Fail",
                  color = styleInterval(0, c("#2c3e50", "#e74c3c")),
                  fontWeight = styleInterval(0, c("normal", "bold")))
  })
  

  output$archive_project_plot <- renderPlotly({
    if (is.null(rv_meas_archive$data) || nrow(rv_meas_archive$data) == 0) {
      return(empty_plot("No measurements yet"))
    }
    
    df <- rv_meas_archive$data
    df <- df[df$Project != "Unassigned", ]
    if (nrow(df) == 0) return(empty_plot("No projects assigned"))
    
    # Anonymize in demo mode
    df$Project <- anonymize_projects(df$Project)
    
    project_summary <- aggregate(N_Samples ~ Project, data = df, FUN = sum)
    project_summary <- project_summary[order(project_summary$N_Samples, decreasing = TRUE), ]
    
    plot_ly(project_summary,
            x = ~reorder(Project, N_Samples),
            y = ~N_Samples,
            type = "bar",
            marker = list(color = "#3498db")) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Samples"),
        margin = list(b = 80)
      )
  })
  
  output$archive_timeline_plot <- renderPlotly({
    if (is.null(rv_meas_archive$data) || nrow(rv_meas_archive$data) == 0) {
      return(empty_plot("No measurements yet"))
    }
    
    df <- rv_meas_archive$data
    df$DateParsed <- as.Date(parse_dates_safe(df$Date))
    df <- df[!is.na(df$DateParsed), ]
    if (nrow(df) == 0) return(empty_plot("No valid data"))
    
    # Anonymize in demo mode
    df$Project <- anonymize_projects(df$Project)
    
    df <- df[order(df$DateParsed), ]
    df$Cumulative_Samples <- cumsum(df$N_Samples)
    
    plot_ly(df, x = ~DateParsed, y = ~Cumulative_Samples,
            type = "scatter", mode = "lines+markers",
            marker = list(size = 6, color = "#18bc9c"),
            line = list(color = "#18bc9c", width = 2),
            text = ~paste0(Project, "\n", format(DateParsed, "%d.%m.%Y"),
                           "\n", N_Samples, " samples"),
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title = "Date", type = "date"),
        yaxis = list(title = "Kumulative samples")
      )
  })
  


  # ---- MEASUREMENT ARCHIVE: DOWNLOAD ----
  output$dl_meas_archive <- downloadHandler(
    filename = function() {
      paste0("Measurement_Archive_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      file.copy(meas_archive_path, file)
    }
  )

# Copy --------------------------------------------------------------------

  
  # ---- NMR COPY: RECEIVE FROM ARCHIVE ----
  observeEvent(rv_nmr_transfer$triggered, {
    samples <- rv_nmr_transfer$samples
    project <- rv_nmr_transfer$project
    source <- rv_nmr_transfer$source
    
    if (is.null(samples) || length(samples) == 0) return()
    if (is.null(source) || source != "archive") return()
    
    # Set the project filter if provided
    if (!is.null(project) && length(project) > 0) {
      tryCatch({
        updateSelectizeInput(session, "nmr_project_filter", selected = project)
      }, error = function(e) NULL)
    }
    
    # Store sample names for filtering
    rv_nmr_transfer$source <- NULL  # Reset so it doesn't re-trigger
    
    # Log the transfer
    nmr_log(paste0("Received ", length(samples), " samples from Archive",
                   if (!is.null(project)) paste0(" (Project: ", project, ")") else ""))
    
    # If there's a sample filter input in the NMR module, populate it
    tryCatch({
      # Check if there's a text input for sample filtering
      if (!is.null(input$nmr_sample_filter)) {
        updateTextAreaInput(session, "nmr_sample_filter",
                            value = paste(samples, collapse = "\n"))
      }
    }, error = function(e) NULL)
    
    # Filter the NMR data table to only show these samples
    rv_nmr_transfer$active_filter <- samples
    
    showNotification(
      tags$div(
        icon("check-circle", style = "color: #18bc9c;"),
        paste0(" ", length(samples), " samples loaded from archive."),
        tags$br(),
        tags$small("Use these samples to select NMR data for copying.")
      ),
      type = "message", duration = 6
    )
  }, ignoreInit = TRUE)
  
  # ---- NMR: Archive filter badge ----
  output$nmr_archive_filter_badge <- renderUI({
    samples <- rv_nmr_transfer$active_filter
    if (is.null(samples) || length(samples) == 0) return(NULL)
    
    tags$div(
      style = "background: #d1ecf1; border: 1px solid #bee5eb; border-radius: 6px; padding: 10px; margin-bottom: 10px;",
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between;",
        tags$div(
          icon("filter", style = "color: #17a2b8; margin-right: 8px;"),
          tags$strong(paste0("Archive Filter Active: ", length(samples), " samples")),
          tags$span(style = "color: #6c757d; margin-left: 10px; font-size: 0.85em;",
                    paste0("(", paste(head(samples, 3), collapse = ", "),
                           if (length(samples) > 3) paste0(", +", length(samples) - 3, " more") else "",
                           ")"))
        ),
        actionButton("nmr_clear_archive_filter", "Clear Filter",
                     class = "btn-sm btn-outline-info", icon = icon("xmark"))
      )
    )
  })
  
  observeEvent(input$nmr_clear_archive_filter, {
    rv_nmr_transfer$active_filter <- NULL
    rv_nmr_transfer$samples <- NULL
    rv_nmr_transfer$project <- NULL
    showNotification("Archive filter cleared.", type = "message", duration = 3)
  })
  
  
  # ---- Scan projects from archive ----
  rv_nmr_archive_projects <- reactiveValues(choices = NULL, project_data = NULL)
  observeEvent(input$nmr_scan_projects, {
    req(rv_archive$data)
    df <- rv_archive$data
    
    if (nrow(df) == 0) {
      showNotification("Archiv ist leer!", type = "warning")
      return()
    }
    
    project_summary <- df %>%
      filter(!is.na(Project) & nchar(Project) > 0) %>%
      group_by(Project) %>%
      summarise(
        n_samples = n_distinct(Name),
        types = paste(unique(Type[!is.na(Type)]), collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(Project)
    
    # Anonymize labels in demo mode
    if (rv_demo$active) {
      n <- nrow(project_summary)
      letters_ext <- c(LETTERS, paste0(LETTERS, "2"))[seq_len(n)]
      display_names <- paste0("PRJ_", letters_ext)
      
      choices <- setNames(
        project_summary$Project,
        paste0(display_names, " \u2014 ", project_summary$n_samples, " samples")
      )
    } else {
      choices <- setNames(
        project_summary$Project,
        paste0(project_summary$Project, " \u2014 ", project_summary$n_samples,
               " samples (", project_summary$types, ")")
      )
    }
    
    rv_nmr_archive_projects$choices <- choices
    rv_nmr_archive_projects$project_data <- project_summary
    
    showNotification(paste0(length(choices), " projects found in archive."),
                     type = "message", duration = 3)
  })
  
  
  
  # ---- Render project selector ----
  output$nmr_project_selector <- renderUI({
    if (is.null(rv_nmr_archive_projects$choices) || length(rv_nmr_archive_projects$choices) == 0) {
      return(tags$div(
        style = "text-align: center; padding: 30px; color: #95a5a6;",
        icon("folder-open", style = "font-size: 2em; margin-bottom: 10px;"),
        tags$br(),
        "Click 'Scan Projects' first"
      ))
    }
    
    tags$div(
      style = "column-count: auto; column-width: 350px; column-gap: 15px;",
      checkboxGroupInput("nmr_selected_projects", label = NULL,
                         choices = rv_nmr_archive_projects$choices,
                         selected = NULL,
                         width = "100%")
    )
  })
  
  # ---- Select all / none projects ----
  observeEvent(input$nmr_select_all_projects, {
    if (!is.null(rv_nmr_archive_projects$choices)) {
      updateCheckboxGroupInput(session, "nmr_selected_projects",
                               selected = rv_nmr_archive_projects$choices)
    }
  })
  
  observeEvent(input$nmr_deselect_all_projects, {
    updateCheckboxGroupInput(session, "nmr_selected_projects",
                             selected = character(0))
  })
  
  # ---- Selection info ----
  output$nmr_project_selection_info <- renderText({
    selected <- input$nmr_selected_projects
    total <- length(rv_nmr_archive_projects$choices)
    n_selected <- if (is.null(selected)) 0 else length(selected)
    paste0(n_selected, " von ", total, " projects selected")
  })
  
  output$nmr_project_sample_total <- renderText({
    selected <- input$nmr_selected_projects
    if (is.null(selected) || length(selected) == 0) return("0 samples")
    
    req(rv_archive$data)
    df <- rv_archive$data[rv_archive$data$Project %in% selected, ]
    
    # Apply type filter
    if (!isTRUE(input$nmr_project_all_types) && !is.null(input$nmr_project_type_filter)) {
      df <- df[df$Type %in% input$nmr_project_type_filter, ]
    }
    
    n <- length(unique(df$Name[!is.na(df$Name) & nchar(df$Name) > 0]))
    paste0(n, " samples total")
  })
  
  # ---- Update type filter based on selected projects ----
  observeEvent(input$nmr_selected_projects, {
    req(rv_archive$data)
    selected <- input$nmr_selected_projects
    if (is.null(selected) || length(selected) == 0) return()
    
    df <- rv_archive$data[rv_archive$data$Project %in% selected, ]
    types <- unique(df$Type[!is.na(df$Type) & nchar(df$Type) > 0])
    updateSelectizeInput(session, "nmr_project_type_filter",
                         choices = sort(types))
  }, ignoreInit = TRUE)
  
  
  
  
  
    # ---- Populate project dropdown for NMR ----
  observeEvent(rv_projects$data, {
    choices <- sort(rv_projects$data$Abbreviation[!is.na(rv_projects$data$Abbreviation) & 
                                                    nchar(rv_projects$data$Abbreviation) > 0])
    updateSelectizeInput(session, "nmr_project_select",
                         choices = choices, server = TRUE,
                         options = list(placeholder = "Search project..."))
  }, ignoreNULL = TRUE)
  
  # ---- Get samples for selected project ----
  rv_nmr_project_samples <- reactiveValues(data = NULL)
  
  # ---- Get samples for selected project from archive ----
  observeEvent(list(input$nmr_project_select, input$nmr_project_all_types, input$nmr_project_type_filter), {
    req(input$nmr_project_select)
    req(rv_archive$data)
    
    df <- rv_archive$data
    project <- input$nmr_project_select
    
    if (is.null(project) || nchar(project) == 0) {
      rv_nmr_project_samples$data <- NULL
      return()
    }
    
    # Filter by project (exact match from archive)
    df <- df[df$Project == project, ]
    
    # Filter by type if needed
    if (!isTRUE(input$nmr_project_all_types) && !is.null(input$nmr_project_type_filter)) {
      df <- df[df$Type %in% input$nmr_project_type_filter, ]
    }
    
    rv_nmr_project_samples$data <- df
    
    # Update type filter choices based on this project's types
    types <- unique(df$Type[!is.na(df$Type) & nchar(df$Type) > 0])
    updateSelectizeInput(session, "nmr_project_type_filter",
                         choices = sort(types))
  }, ignoreInit = TRUE)
  
  
  # ---- Project info display ----
  output$nmr_project_info <- renderUI({
    df <- rv_nmr_project_samples$data
    if (is.null(df) || nrow(df) == 0) {
      return(tags$small(class = "text-muted", "No samples found"))
    }
    
    n_samples <- length(unique(df$Name))
    types <- paste(unique(df$Type), collapse = ", ")
    
    tags$div(
      style = "font-size: 0.85em;",
      tags$div(icon("vial", style = "color: #18bc9c; margin-right: 5px;"),
               tags$strong(n_samples), " samples"),
      tags$div(icon("tags", style = "color: #3498db; margin-right: 5px;"),
               types)
    )
  })
  
  # ---- Project sample count ----
  output$nmr_project_sample_count <- renderText({
    df <- rv_nmr_project_samples$data
    if (is.null(df) || nrow(df) == 0) return("0 samples")
    paste0(length(unique(df$Name)), " samples")
  })
  
  # ---- Project samples table preview ----
  output$nmr_project_samples_table <- renderDT({
    df <- rv_nmr_project_samples$data
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Info = "Select a project..."), rownames = FALSE))
    }
    
    # Show relevant columns
    display_cols <- intersect(c("Name", "Type", "Size", "Date", "Box"), names(df))
    df_display <- df[, display_cols, drop = FALSE]
    df_display <- df_display[!duplicated(df_display$Name), ]
    df_display <- df_display[order(df_display$Name), ]
    
    datatable(df_display, class = "compact stripe hover",
              options = list(
                scrollX = TRUE, pageLength = 50,
                paging = FALSE, searching = TRUE,
                dom = "fti",
                language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/de-DE.json")
              ),
              rownames = FALSE)
  })
  
  # ---- Scan for available experiments ----
  rv_nmr_experiments <- reactiveValues(available = character(0))
  
  observeEvent(input$nmr_scan_experiments, {
    source_base <- input$nmr_source_path
    mid_level <- input$nmr_mid_level
    
    if (!dir.exists(source_base)) {
      showNotification("Source directory not found!", type = "error")
      return()
    }
    
    experiments_found <- character(0)
    
    withProgress(message = "Scanning experiments...", value = 0, {
      year_dirs <- list.dirs(source_base, recursive = FALSE, full.names = TRUE)
      n_years <- length(year_dirs)
      
      for (y_idx in seq_along(year_dirs)) {
        year_dir <- year_dirs[y_idx]
        data_dir <- file.path(year_dir, "data")
        if (!dir.exists(data_dir)) next
        
        date_dirs <- list.dirs(data_dir, recursive = FALSE, full.names = TRUE)
        
        # Sample a few date folders (not all, for speed)
        sample_dates <- head(date_dirs, 5)
        
        for (date_dir in sample_dates) {
          mid_dir <- file.path(date_dir, mid_level)
          if (!dir.exists(mid_dir)) next
          
          sample_dirs <- head(list.dirs(mid_dir, recursive = FALSE, full.names = TRUE), 3)
          
          for (sample_dir in sample_dirs) {
            sub_dirs <- list.dirs(sample_dir, recursive = FALSE, full.names = TRUE)
            numeric_dirs <- sub_dirs[grepl("^[0-9]+$", basename(sub_dirs))]
            
            for (num_dir in numeric_dirs) {
              pp_file <- file.path(num_dir, "pulseprogram")
              if (!file.exists(pp_file)) next
              tryCatch({
                first_line <- trimws(readLines(pp_file, n = 1, warn = FALSE))
                if (nchar(first_line) > 0) {
                  experiments_found <- c(experiments_found, first_line)
                }
              }, error = function(e) NULL)
            }
          }
        }
        
        incProgress(1 / n_years)
      }
    })
    
    rv_nmr_experiments$available <- sort(unique(experiments_found))
    
    if (length(rv_nmr_experiments$available) == 0) {
      showNotification("No experiments found!", type = "warning")
    } else {
      showNotification(paste0(length(rv_nmr_experiments$available), " experiments found."),
                       type = "message")
    }
  })
  
  # ---- Render experiment selector ----
  output$nmr_experiment_selector <- renderUI({
    if (length(rv_nmr_experiments$available) == 0) {
      return(tags$div(
        style = "color: #95a5a6; font-size: 0.85em;",
        "Scan first to find experiments"
      ))
    }
    
    # Common experiments on top
    common <- c(";noesygppr1d", ";cpmgpr1d", ";zg30", ";ledbpgppr2s1d",
                ";jresgpprqf", ";cpmgpr", ";noesypr1d", ";zgpr")
    available <- rv_nmr_experiments$available
    
    # Sort: common ones first, then rest alphabetically
    is_common <- available %in% common
    sorted_experiments <- c(available[is_common], available[!is_common])
    
    checkboxGroupInput("nmr_selected_experiments", label = NULL,
                       choices = sorted_experiments,
                       selected = intersect(common, available),
                       width = "100%")
  })
  
  
  # NMR DATEN: Copy & Bucketing-----
  
  rv_nmr <- reactiveValues(
    log = character(0),
    n_found = 0,
    n_missing = 0,
    n_copied = 0,
    progress = 0,
    running = FALSE,
    bucketing_result = NULL,
    date_folders = character(0)
  )
  
  # ---- Helper: normalize name for fuzzy matching ----
  nmr_normalize_name <- function(x) {
    x <- toupper(trimws(x))
    x <- gsub("([A-Z])[-_ .]+([0-9])", "\\1\\2", x)
    x <- gsub("([0-9])[-_ .]+([A-Z])", "\\1\\2", x)
    x
  }
  
  # ---- Helper: get experiment from pulseprogram ----
  nmr_get_experiment <- function(numbered_dir) {
    pp_file <- file.path(numbered_dir, "pulseprogram")
    if (!file.exists(pp_file)) return(NA_character_)
    tryCatch({
      first_line <- trimws(readLines(pp_file, n = 1, warn = FALSE))
      first_line
    }, error = function(e) NA_character_)
  }
  
  # ---- Helper: add log message ----

  # ---- NMR SPECTRUM VIEWER HANDLERS ----
  
  rv_nmr_view <- reactiveValues(
    experiments = NULL,
    spectrum = NULL,
    overlay_spectra = list(),
    current_path = NULL,
    acq_info = NULL
  )
  
  # Browse folder
  observeEvent(input$nmr_view_browse, {
    folder <- tryCatch({
      if (.Platform$OS.type == "windows") {
        utils::choose.dir(default = getwd(), caption = "Select NMR sample folder")
      } else {
        system("zenity --file-selection --directory 2>/dev/null", intern = TRUE)
      }
    }, error = function(e) NULL)
    
    if (!is.null(folder) && nchar(folder) > 0 && folder != "NA") {
      updateTextInput(session, "nmr_view_path", value = folder)
    }
  })
  
  # Populate quick access from extracted data
  observe({
    sample_ids <- character(0)
    if (!is.null(rv$xml_m)) {
      sample_ids <- unique(as.character(rv$xml_m$ID))
    }
    updateSelectizeInput(session, "nmr_view_sample", choices = c("Select sample..." = "", sample_ids),
                         server = TRUE)
  })
  
  # When quick access sample is selected, try to find its folder
  observeEvent(input$nmr_view_sample, {
    req(input$nmr_view_sample, nchar(input$nmr_view_sample) > 0)
    
    # Try to find the sample folder in the scanned folder
    base_folder <- rv$folder_path
    if (is.null(base_folder) || !dir.exists(base_folder)) return()
    
    sample_id <- input$nmr_view_sample
    
    # Search for the sample folder
    all_dirs <- list.dirs(base_folder, recursive = TRUE, full.names = TRUE)
    match_dirs <- all_dirs[basename(all_dirs) == sample_id]
    
    if (length(match_dirs) > 0) {
      # Look for the one containing nmr data
      for (d in match_dirs) {
        if (length(list.files(d, pattern = "^1r$", recursive = TRUE)) > 0) {
          updateTextInput(session, "nmr_view_path", value = d)
          break
        }
        # Also check parent/child for nmr subfolder
        nmr_sub <- file.path(d, "nmr")
        if (dir.exists(nmr_sub)) {
          updateTextInput(session, "nmr_view_path", value = nmr_sub)
          break
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # Scan for experiments when path changes
  observeEvent(input$nmr_view_path, {
    req(input$nmr_view_path, nchar(input$nmr_view_path) > 0)
    path <- input$nmr_view_path
    
    if (!dir.exists(path)) {
      rv_nmr_view$experiments <- NULL
      return()
    }
    
    exps <- tryCatch(find_bruker_experiments(path), error = function(e) {
      showNotification(paste0("Error scanning: ", e$message), type = "error", duration = 4)
      NULL
    })
    
    rv_nmr_view$experiments <- exps
    rv_nmr_view$current_path <- path
  }, ignoreInit = TRUE)
  
  # Render experiment table
  output$nmr_view_exp_table <- renderDT({
    req(rv_nmr_view$experiments, nrow(rv_nmr_view$experiments) > 0)

    display_df <- rv_nmr_view$experiments[, c("Experiment", "ProcNo", "PulseProgram"), drop = FALSE]

    # basename() returns character - coerce so DT sorts numerically
    display_df$Experiment <- suppressWarnings(as.integer(display_df$Experiment))
    display_df$ProcNo     <- suppressWarnings(as.integer(display_df$ProcNo))

    # Shorten common Bruker prefixes so the column fits
    display_df$PulseProgram <- sub("^N PROF_", "", display_df$PulseProgram)

    datatable(display_df,
              colnames = c("Exp", "Proc", "Pulse Program"),
              selection = "single",
              rownames = FALSE,
              options = list(
                pageLength = 25,
                dom = "t",
                scrollY = "220px",
                scrollX = FALSE,
                autoWidth = FALSE,
                columnDefs = list(
                  list(width = "15%", targets = 0, className = "dt-center"),
                  list(width = "15%", targets = 1, className = "dt-center"),
                  list(width = "70%", targets = 2)
                )
              )) %>%
      formatStyle("PulseProgram", fontWeight = "bold", fontSize = "0.8em")
  })
  
  # Load spectrum when experiment is selected
  observeEvent(input$nmr_view_exp_table_rows_selected, {
    req(rv_nmr_view$experiments)
    row_idx <- input$nmr_view_exp_table_rows_selected
    if (is.null(row_idx) || length(row_idx) == 0) return()
    
    pdata_path <- rv_nmr_view$experiments$Path[row_idx]
    
    spec <- tryCatch({
      read_bruker_1r(pdata_path)
    }, error = function(e) {
      showNotification(paste0("Error reading spectrum: ", e$message), type = "error", duration = 5)
      NULL
    })
    
    if (!is.null(spec)) {
      rv_nmr_view$spectrum <- spec
      
      # Read acquisition info
      exp_dir <- dirname(dirname(pdata_path))
      acqus_file <- file.path(exp_dir, "acqus")
      info <- list(
        experiment = rv_nmr_view$experiments$Experiment[row_idx],
        pulse_program = rv_nmr_view$experiments$PulseProgram[row_idx],
        title = rv_nmr_view$experiments$Title[row_idx],
        path = pdata_path,
        points = nrow(spec)
      )
      
      if (file.exists(acqus_file)) {
        acq_lines <- readLines(acqus_file, warn = FALSE)
        get_acq <- function(name) {
          pattern <- paste0("##", rawToChar(as.raw(36)), name, "=")
          idx <- which(startsWith(acq_lines, pattern))
          if (length(idx) == 0) return(NA)
          trimws(sub(pattern, "", acq_lines[idx[1]], fixed = TRUE))
        }
        info$SFO1 <- get_acq("SFO1")
        info$NS <- get_acq("NS")
        info$DS <- get_acq("DS")
        info$TD <- get_acq("TD")
        info$SW <- get_acq("SW")
        info$TE <- get_acq("TE")
        info$D1 <- get_acq("D1")
        info$DATE <- get_acq("DATE")
      }
      
      rv_nmr_view$acq_info <- info
    }
  })
  
  # Overlay button
  observeEvent(input$nmr_view_overlay, {
    req(rv_nmr_view$spectrum)
    row_idx <- input$nmr_view_exp_table_rows_selected
    if (is.null(row_idx)) return()
    
    label <- paste0("Exp ", rv_nmr_view$experiments$Experiment[row_idx],
                    " (", rv_nmr_view$experiments$PulseProgram[row_idx], ")")
    
    rv_nmr_view$overlay_spectra[[length(rv_nmr_view$overlay_spectra) + 1]] <- list(
      data = rv_nmr_view$spectrum,
      label = label
    )
    showNotification(paste0("Added overlay: ", label), type = "message", duration = 2)
  })
  
  # Clear overlays
  observeEvent(input$nmr_view_clear, {
    rv_nmr_view$overlay_spectra <- list()
    rv_nmr_view$spectrum <- NULL
    rv_nmr_view$acq_info <- NULL
    showNotification("Cleared all spectra.", type = "message", duration = 2)
  })
  
  # Info badge
  output$nmr_view_info_badge <- renderUI({
    if (is.null(rv_nmr_view$spectrum)) {
      return(tags$span(class = "badge bg-secondary", "No spectrum loaded"))
    }
    info <- rv_nmr_view$acq_info
    tags$div(
      style = "display: flex; gap: 4px;",
      tags$span(class = "badge bg-primary", paste0("Exp ", info$experiment)),
      tags$span(class = "badge bg-info", info$pulse_program),
      tags$span(class = "badge bg-secondary", paste0(info$points, " pts")),
      if (length(rv_nmr_view$overlay_spectra) > 0) {
        tags$span(class = "badge bg-warning", 
                  paste0(length(rv_nmr_view$overlay_spectra), " overlay(s)"))
      }
    )
  })
  
  # Render spectrum plot
  output$nmr_view_spectrum <- renderPlotly({
    if (is.null(rv_nmr_view$spectrum)) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              list(text = "Select a sample and experiment to view spectrum",
                   xref = "paper", yref = "paper", x = 0.5, y = 0.5,
                   showarrow = FALSE, font = list(size = 16, color = "#95a5a6"))
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    df <- rv_nmr_view$spectrum
    
    # Apply PPM range
    ppm_min <- input$nmr_view_ppm_min
    ppm_max <- input$nmr_view_ppm_max
    if (!is.null(ppm_min) && !is.null(ppm_max) && !is.na(ppm_min) && !is.na(ppm_max)) {
      df <- df[df$ppm >= ppm_min & df$ppm <= ppm_max, ]
    }
    
    if (nrow(df) == 0) return(plot_ly() %>% layout(title = "No data in PPM range"))
    
    # Normalize if requested
    if (isTRUE(input$nmr_view_normalize)) {
      max_int <- max(abs(df$intensity), na.rm = TRUE)
      if (max_int > 0) df$intensity <- df$intensity / max_int
    }
    
    # Build plot
    info <- rv_nmr_view$acq_info
    label <- paste0("Exp ", info$experiment, " - ", info$pulse_program)
    
    p <- plot_ly() %>%
      add_lines(data = df, x = ~ppm, y = ~intensity,
                name = label,
                line = list(color = "#2c3e50", width = 0.8),
                hoverinfo = "text",
                text = ~paste0("ppm: ", round(ppm, 4), "
Intensity: ", round(intensity, 0)))
    
    # Add overlays
    overlay_colors <- c("#e74c3c", "#3498db", "#2ecc71", "#e67e22", "#9b59b6", 
                        "#1abc9c", "#f39c12", "#e91e63")
    for (k in seq_along(rv_nmr_view$overlay_spectra)) {
      ov <- rv_nmr_view$overlay_spectra[[k]]
      ov_df <- ov$data
      if (!is.null(ppm_min) && !is.null(ppm_max) && !is.na(ppm_min) && !is.na(ppm_max)) {
        ov_df <- ov_df[ov_df$ppm >= ppm_min & ov_df$ppm <= ppm_max, ]
      }
      if (isTRUE(input$nmr_view_normalize)) {
        max_int <- max(abs(ov_df$intensity), na.rm = TRUE)
        if (max_int > 0) ov_df$intensity <- ov_df$intensity / max_int
      }
      col_idx <- ((k - 1) %% length(overlay_colors)) + 1
      p <- p %>% add_lines(data = ov_df, x = ~ppm, y = ~intensity,
                            name = ov$label,
                            line = list(color = overlay_colors[col_idx], width = 0.7))
    }
    
    # Layout

    # Add IVDr metabolite region annotations
    if (isTRUE(input$nmr_view_show_regions)) {
      regions <- get_ivdr_metabolite_regions()
      fill_colors <- get_metabolite_colors()
      border_colors <- get_metabolite_border_colors()
      
      # Filter by selected categories
      sel_cats <- input$nmr_view_categories
      if (!is.null(sel_cats) && length(sel_cats) > 0) {
        regions <- regions[regions$Category %in% sel_cats, ]
      }
      
      # Filter to visible PPM range
      if (!is.null(ppm_min) && !is.null(ppm_max) && !is.na(ppm_min) && !is.na(ppm_max)) {
        regions <- regions[regions$ppm_max >= ppm_min & regions$ppm_min <= ppm_max, ]
      }
      
      if (nrow(regions) > 0) {
        # Build shapes list for shaded regions
        shapes <- lapply(seq_len(nrow(regions)), function(r) {
          list(
            type = "rect",
            x0 = regions$ppm_min[r],
            x1 = regions$ppm_max[r],
            y0 = 0,
            y1 = 1,
            yref = "paper",
            fillcolor = fill_colors[regions$Category[r]],
            line = list(color = border_colors[regions$Category[r]], width = 0.5),
            layer = "below"
          )
        })
        
        # Build annotations - labels inside the shaded regions
        annotations <- lapply(seq_len(nrow(regions)), function(r) {
          mid_ppm <- (regions$ppm_min[r] + regions$ppm_max[r]) / 2
          list(
            x = mid_ppm,
            y = 0.95,
            yref = "paper",
            text = paste0("<b>", regions$Metabolite[r], "</b>"),
            showarrow = FALSE,
            font = list(size = 9, color = "#333333"),
            textangle = -90,
            xanchor = "center",
            yanchor = "top",
            bgcolor = "rgba(255,255,255,0.7)",
            borderpad = 1
          )
        })
        
        p <- p %>% layout(shapes = shapes, annotations = annotations)
      }
    }
    
    x_range <- c(ppm_max, ppm_min)
    
    p %>% layout(
      xaxis = list(title = "Chemical Shift (ppm)", dtick = 0.5,
                   showgrid = TRUE, gridcolor = "#ecf0f1", range = x_range),
      yaxis = list(title = "Intensity", showgrid = FALSE,
                   zeroline = TRUE, zerolinecolor = "#bdc3c7"),
      showlegend = length(rv_nmr_view$overlay_spectra) > 0,
      legend = list(orientation = "h", y = -0.15),
      margin = list(t = 120),
      hovermode = "x unified"
    ) %>%
      config(displayModeBar = TRUE, 
             modeBarButtonsToAdd = list("drawrect", "eraseshape"),
             modeBarButtonsToRemove = list("lasso2d", "select2d"))
  })
  
  # Acquisition info panel
  output$nmr_view_acq_info <- renderUI({
    if (is.null(rv_nmr_view$acq_info)) {
      return(tags$div(
        style = "text-align: center; color: #95a5a6; padding: 15px;",
        "No spectrum loaded"
      ))
    }
    
    info <- rv_nmr_view$acq_info
    
    make_info_item <- function(label, value) {
      if (is.null(value) || length(value) == 0) return(NULL)
      value <- paste(as.character(value), collapse = " ")
      if (is.na(value) || value == "" || value == "NA") return(NULL)
      tags$div(
        style = "display: inline-flex; gap: 5px; margin: 3px 6px; font-size: 0.85em;",
        tags$span(style = "color: #7f8c8d; font-weight: 500;", paste0(label, ":")),
        tags$span(style = "font-weight: 600;", value)
      )
    }
    
    # Convert DATE timestamp if numeric
    date_str <- info$DATE
    if (!is.null(date_str) && !is.na(date_str)) {
      date_num <- suppressWarnings(as.numeric(date_str))
      if (!is.na(date_num)) {
        date_str <- format(as.POSIXct(date_num, origin = "1970-01-01"), "%Y-%m-%d %H:%M")
      }
    }
    
    tags$div(
      style = "display: flex; flex-wrap: wrap; align-items: center;",
      make_info_item("Experiment", info$experiment),
      make_info_item("Pulse Program", info$pulse_program),
      make_info_item("Title", info$title),
      make_info_item("Frequency", paste0(info$SFO1, " MHz")),
      make_info_item("Scans (NS)", info$NS),
      make_info_item("Dummy Scans", info$DS),
      make_info_item("Data Points", info$TD),
      make_info_item("Spectral Width", paste0(info$SW, " ppm")),
      make_info_item("Temperature", if (!is.null(info$TE) && !is.na(info$TE)) paste0(info$TE, " K") else NULL),
      make_info_item("Relaxation Delay", if (!is.null(info$D1) && !is.na(info$D1)) paste0(info$D1, " s") else NULL),
      make_info_item("Date", date_str),
      make_info_item("Points (processed)", info$points),
      make_info_item("Path", tags$code(style = "font-size: 0.8em;", info$path))
    )
  })
  
  # Download spectrum as CSV
  output$nmr_view_download_data <- downloadHandler(
    filename = function() {
      info <- rv_nmr_view$acq_info
      paste0("NMR_spectrum_exp", info$experiment, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv_nmr_view$spectrum)
      write.csv(rv_nmr_view$spectrum, file, row.names = FALSE)
    }
  )
  
  # Download plot as HTML
  output$nmr_view_download_plot <- downloadHandler(
    filename = function() {
      info <- rv_nmr_view$acq_info
      paste0("NMR_spectrum_exp", info$experiment, "_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      req(rv_nmr_view$spectrum)
      p <- plotly::last_plot()
      htmlwidgets::saveWidget(p, file, selfcontained = TRUE)
    }
  )

  nmr_log <- function(msg) {
    rv_nmr$log <- c(rv_nmr$log, paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }
  
  # ---- Scan date folders (no cap, sorted newest first) ----
  observeEvent(input$nmr_scan_dates, {
    source_base <- input$nmr_source_path
    if (!dir.exists(source_base)) {
      showNotification("Source directory not found!", type = "error")
      return()
    }
    
    mid_level <- input$nmr_mid_level
    date_folders <- character(0)
    folder_labels <- character(0)
    
    withProgress(message = "Scanning folders...", value = 0, {
      year_dirs <- sort(list.dirs(source_base, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
      n_years <- length(year_dirs)
      
      for (y_idx in seq_along(year_dirs)) {
        year_dir <- year_dirs[y_idx]
        data_dir <- file.path(year_dir, "data")
        if (!dir.exists(data_dir)) next
        
        date_dirs <- sort(list.dirs(data_dir, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
        
        for (date_dir in date_dirs) {
          mid_dir <- file.path(date_dir, mid_level)
          if (dir.exists(mid_dir)) {
            n_samples <- length(list.dirs(mid_dir, recursive = FALSE))
            if (n_samples > 0) {
              label <- paste0(basename(date_dir), " (", basename(year_dir), ") \u2014 ",
                              n_samples, " samples")
              date_folders <- c(date_folders, date_dir)
              folder_labels <- c(folder_labels, label)
            }
          }
        }
        
        incProgress(1 / n_years)
      }
    })
    
    # Create named vector (label = value)
    rv_nmr$date_folders <- setNames(date_folders, folder_labels)
    
    if (length(date_folders) == 0) {
      showNotification("No date folders found!", type = "warning")
    } else {
      showNotification(paste0(length(date_folders), " date folders found."), type = "message")
    }
  })
  
  
  # ---- Render date folder selector (grouped by year, multi-column) ----
  output$nmr_date_selector <- renderUI({
    if (length(rv_nmr$date_folders) == 0) {
      return(tags$div(
        style = "text-align: center; padding: 30px; color: #95a5a6;",
        icon("folder-open", style = "font-size: 2em; margin-bottom: 10px;"),
        tags$br(),
        "Click 'Scan date folders' first"
      ))
    }
    
    # Group folders by year
    folder_paths <- rv_nmr$date_folders
    folder_labels <- names(rv_nmr$date_folders)
    
    # Extract year from label: "20250115 (2025)  12 Proben"
    years <- gsub(".*\\(([0-9]{4})\\).*", "\\1", folder_labels)
    unique_years <- sort(unique(years), decreasing = TRUE)
    
    # Build UI with year sections
    year_sections <- lapply(unique_years, function(yr) {
      idx <- which(years == yr)
      yr_choices <- setNames(folder_paths[idx], folder_labels[idx])
      
      tags$div(
        style = "margin-bottom: 15px;",
        tags$div(
          style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
          tags$h6(style = "margin: 0; font-weight: 700; color: #2c3e50;",
                  icon("calendar", style = "margin-right: 5px;"), yr),
          
          tags$span(class = "badge bg-secondary", paste0(length(idx), " folders")),
          actionButton(paste0("nmr_select_year_", yr), "All",
                       class = "btn-outline-primary btn-sm",
                       style = "padding: 1px 8px; font-size: 0.75em;")
        ),
        tags$div(
          style = "column-count: auto; column-width: 280px; column-gap: 15px; padding-left: 10px;",
          checkboxGroupInput(paste0("nmr_dates_", yr), label = NULL,
                             choices = yr_choices,
                             selected = NULL,
                             width = "100%")
        ),
        tags$hr(style = "margin: 5px 0;")
      )
    })
    
    # CSS for compact checkboxes
    tagList(
      tags$style(HTML("
        [id^='nmr_dates_'] .checkbox {
          break-inside: avoid;
          margin: 1px 0;
          font-size: 0.82em;
        }
        [id^='nmr_dates_'] .checkbox label {
          padding-left: 20px;
        }
      ")),
      year_sections
    )
  })
  
  
  # ---- Select ALL dates across all years ----
  observeEvent(input$nmr_select_all_dates, {
    folder_paths <- rv_nmr$date_folders
    folder_labels <- names(rv_nmr$date_folders)
    years <- gsub(".*\\(([0-9]{4})\\).*", "\\1", folder_labels)
    unique_years <- sort(unique(years), decreasing = TRUE)
    
    for (yr in unique_years) {
      idx <- which(years == yr)
      updateCheckboxGroupInput(session, paste0("nmr_dates_", yr),
                               selected = folder_paths[idx])
    }
  })
  
  # ---- Deselect ALL ----
  observeEvent(input$nmr_deselect_all_dates, {
    folder_labels <- names(rv_nmr$date_folders)
    years <- gsub(".*\\(([0-9]{4})\\).*", "\\1", folder_labels)
    unique_years <- unique(years)
    
    for (yr in unique_years) {
      updateCheckboxGroupInput(session, paste0("nmr_dates_", yr),
                               selected = character(0))
    }
  })
  
  # ---- Select all for a specific year (create observers once) ----
  nmr_year_observers_created <- reactiveVal(character(0))
  
  observeEvent(rv_nmr$date_folders, {
    folder_paths <- rv_nmr$date_folders
    if (length(folder_paths) == 0) return()
    
    folder_labels <- names(folder_paths)
    years <- gsub(".*\\(([0-9]{4})\\).*", "\\1", folder_labels)
    unique_years <- sort(unique(years), decreasing = TRUE)
    
    already_created <- nmr_year_observers_created()
    new_years <- setdiff(unique_years, already_created)
    
    if (length(new_years) == 0) return()
    
    for (yr in new_years) {
      local({
        local_yr <- yr
        observeEvent(input[[paste0("nmr_select_year_", local_yr)]], {
          fp <- rv_nmr$date_folders
          fl <- names(fp)
          yrs <- gsub(".*\\(([0-9]{4})\\).*", "\\1", fl)
          idx <- which(yrs == local_yr)
          updateCheckboxGroupInput(session, paste0("nmr_dates_", local_yr),
                                   selected = fp[idx])
        }, ignoreInit = TRUE)
      })
    }
    
    nmr_year_observers_created(c(already_created, new_years))
  })
  
  
  # ---- Selection info (count across all year groups) ----
  output$nmr_date_selection_info <- renderText({
    folder_labels <- names(rv_nmr$date_folders)
    if (length(folder_labels) == 0) return("0 of 0 folders selected")
    
    years <- gsub(".*\\(([0-9]{4})\\).*", "\\1", folder_labels)
    unique_years <- unique(years)
    
    total <- length(rv_nmr$date_folders)
    n_selected <- 0
    for (yr in unique_years) {
      sel <- input[[paste0("nmr_dates_", yr)]]
      if (!is.null(sel)) n_selected <- n_selected + length(sel)
    }
    paste0(n_selected, " of ", total, " folders selected")
  })
  
  


  # ---- KPI outputs ----
  output$nmr_n_found <- renderText(as.character(rv_nmr$n_found))
  output$nmr_n_missing <- renderText(as.character(rv_nmr$n_missing))
  output$nmr_n_copied <- renderText(as.character(rv_nmr$n_copied))
  
  # ---- Log output ----
  output$nmr_log_output <- renderText({
    paste(rv_nmr$log, collapse = "\n")
  })
  
  # ---- Main process ----
  observeEvent(input$nmr_start_btn, {
    rv_nmr$log <- character(0)
    rv_nmr$n_found <- 0
    rv_nmr$n_missing <- 0
    rv_nmr$n_copied <- 0
    rv_nmr$progress <- 0
    rv_nmr$running <- TRUE
    rv_nmr$bucketing_result <- NULL
    
    source_base <- input$nmr_source_path
    dest_base <- input$nmr_dest_path
    mid_level <- input$nmr_mid_level
    target_experiments <- input$nmr_selected_experiments
    if (is.null(target_experiments) || length(target_experiments) == 0) {
      nmr_log("ERROR: No experiments selected!")
      rv_nmr$running <- FALSE
      return()
    }
    
    # Validate
    if (!dir.exists(source_base)) {
      nmr_log("ERROR: Source directory not found!")
      rv_nmr$running <- FALSE
      return()
    }
    if (is.null(dest_base) || nchar(dest_base) == 0) {
      nmr_log("ERROR: No destination directory specified!")
      rv_nmr$running <- FALSE
      return()
    }
    if (!dir.exists(dest_base)) dir.create(dest_base, recursive = TRUE)
    
    nmr_log(paste0("Source: ", source_base))
    nmr_log(paste0("Destination: ", dest_base))
    nmr_log(paste0("Experiments: ", paste(target_experiments, collapse = ", ")))
    
    # ---- Get sample list based on mode ----
    
    # Check if archive transfer is active (overrides mode)
    archive_samples <- rv_nmr_transfer$active_filter
    
    if (!is.null(archive_samples) && length(archive_samples) > 0) {
      # MODE: Archive transfer
      target_names <- unique(trimws(archive_samples))
      target_names <- target_names[!is.na(target_names) & nchar(target_names) > 0]
      nmr_log(paste0(length(target_names), " samples from Archive transfer."))
      
    } else if (input$nmr_selection_mode == "samplelist") {
      # From Excel
      req(input$nmr_excel_file)
      excel_path <- input$nmr_excel_file$datapath
      name_col <- input$nmr_name_column
      
      df_spectra <- tryCatch({
        readxl::read_excel(excel_path, col_types = "text")
      }, error = function(e) {
        nmr_log(paste0("ERROR reading Excel: ", e$message))
        return(NULL)
      })
      
      if (is.null(df_spectra)) { rv_nmr$running <- FALSE; return() }
      
      if (!name_col %in% names(df_spectra)) {
        nmr_log(paste0("ERROR: Column '", name_col, "' not found!"))
        rv_nmr$running <- FALSE
        return()
      }
      
      target_names <- unique(trimws(na.omit(df_spectra[[name_col]])))
      target_names <- target_names[nchar(target_names) > 0]
      nmr_log(paste0(length(target_names), " samples loaded from Excel."))
      
    } else if (input$nmr_selection_mode == "project") {
      # From archive by selected projects
      selected_projects <- input$nmr_selected_projects
      if (is.null(selected_projects) || length(selected_projects) == 0) {
        nmr_log("ERROR: No projects selected!")
        rv_nmr$running <- FALSE
        return()
      }
      
      df <- rv_archive$data[rv_archive$data$Project %in% selected_projects, ]
      
      # Apply type filter
      if (!isTRUE(input$nmr_project_all_types) && !is.null(input$nmr_project_type_filter)) {
        df <- df[df$Type %in% input$nmr_project_type_filter, ]
      }
      
      target_names <- unique(trimws(df$Name))
      target_names <- target_names[!is.na(target_names) & nchar(target_names) > 0]
      nmr_log(paste0(length(target_names), " samples from ", length(selected_projects),
                     " Project(s) loaded: ", paste(selected_projects, collapse = ", ")))
      
    } else {
      
      # From date folders (collect from all year groups)
      folder_labels <- names(rv_nmr$date_folders)
      years <- gsub(".*\\(([0-9]{4})\\).*", "\\1", folder_labels)
      unique_years <- unique(years)
      
      selected_dates <- character(0)
      for (yr in unique_years) {
        sel <- input[[paste0("nmr_dates_", yr)]]
        if (!is.null(sel)) selected_dates <- c(selected_dates, sel)
      }
      
      if (length(selected_dates) == 0) {
        nmr_log("ERROR: No date folders selected!")
        rv_nmr$running <- FALSE
        return()
      }
      
      target_names <- character(0)
      for (date_dir in selected_dates) {
        mid_dir <- file.path(date_dir, mid_level)
        if (dir.exists(mid_dir)) {
          samples <- basename(list.dirs(mid_dir, recursive = FALSE))
          target_names <- c(target_names, samples)
        }
      }
      target_names <- unique(target_names)
      nmr_log(paste0(length(target_names), " samples from ", length(selected_dates), " date folder(s) loaded."))
    }
    
    
    
    
    if (length(target_names) == 0) {
      nmr_log("No samples found!")
      rv_nmr$running <- FALSE
      return()
    }
    
    target_normalized <- nmr_normalize_name(target_names)
    
    # ---- Copy process ----
    nmr_log("--- Copy process started ---")
    found_targets <- logical(length(target_names))
    names(found_targets) <- target_names
    cnt_ok <- 0L
    cnt_error <- 0L
    total <- length(target_names)
    
    year_dirs <- list.dirs(source_base, recursive = FALSE, full.names = TRUE)
    
    withProgress(message = "NMR Daten kopieren...", value = 0, {
      for (year_dir in year_dirs) {
        data_dir <- file.path(year_dir, "data")
        if (!dir.exists(data_dir)) next
        
        date_dirs <- list.dirs(data_dir, recursive = FALSE, full.names = TRUE)
        
        for (date_dir in date_dirs) {
          mid_dir <- file.path(date_dir, mid_level)
          if (!dir.exists(mid_dir)) next
          
          date_folder_name <- basename(date_dir)
          candidate_dirs <- list.dirs(mid_dir, recursive = FALSE, full.names = TRUE)
          
          for (candidate_dir in candidate_dirs) {
            spectra_folder_name <- basename(candidate_dir)
            spectra_normalized <- nmr_normalize_name(spectra_folder_name)
            
            match_idx <- which(target_normalized == spectra_normalized)
            if (length(match_idx) == 0) next
            
            excel_name <- target_names[match_idx[1]]
            
            # Determine destination path based on option
            if (isTRUE(input$nmr_keep_date_structure)) {
              dest_spectra_dir <- file.path(dest_base, date_folder_name, spectra_folder_name)
            } else {
              dest_spectra_dir <- file.path(dest_base, spectra_folder_name)
            }
            
            # Skip if already copied
            if (dir.exists(dest_spectra_dir)) {
              found_targets[excel_name] <- TRUE
              next
            }
            
            found_targets[excel_name] <- TRUE
            
            # List numbered subfolders
            sub_dirs <- list.dirs(candidate_dir, recursive = FALSE, full.names = TRUE)
            sub_names <- basename(sub_dirs)
            numeric_idx <- grepl("^[0-9]+$", sub_names)
            
            if (!any(numeric_idx)) {
              nmr_log(paste0("  [NO SUBFOLDERS] ", spectra_folder_name))
              next
            }
            
            numeric_dirs <- sub_dirs[numeric_idx]
            numeric_vals <- as.integer(sub_names[numeric_idx])
            
            # Check experiments
            experiments <- sapply(numeric_dirs, nmr_get_experiment, USE.NAMES = FALSE)
            match_exp <- which(experiments %in% target_experiments)
            
            if (length(match_exp) == 0) {
              nmr_log(paste0("  [KEIN MATCH] ", spectra_folder_name, " - kein passendes Experiment"))
              next
            }
            
            # Copy ALL matching experiments
            copied_experiments <- character(0)
            for (m_idx in match_exp) {
              exp_name <- experiments[m_idx]
              
              if (exp_name %in% copied_experiments) next
              
              chosen_dir <- numeric_dirs[m_idx]
              chosen_name <- basename(chosen_dir)
              
              dest_dir <- file.path(dest_spectra_dir, chosen_name)
              
              if (dir.exists(dest_dir)) {
                copied_experiments <- c(copied_experiments, exp_name)
                next
              }
              
              result <- tryCatch({
                dir.create(dirname(dest_dir), recursive = TRUE, showWarnings = FALSE)
                fs::dir_copy(chosen_dir, dest_dir)
                "OK"
              }, error = function(e) {
                paste0("ERROR: ", e$message)
              })
              
              if (startsWith(result, "ERROR")) {
                nmr_log(paste0("  [ERROR] ", spectra_folder_name, "/", chosen_name, ": ", result))
                cnt_error <- cnt_error + 1L
              } else {
                cnt_ok <- cnt_ok + 1L
                copied_experiments <- c(copied_experiments, exp_name)
                if (isTRUE(input$nmr_keep_date_structure)) {
                  nmr_log(paste0("  [OK] ", date_folder_name, "/", spectra_folder_name, "/",
                                 chosen_name, " (", exp_name, ")"))
                } else {
                  nmr_log(paste0("  [OK] ", spectra_folder_name, "/", chosen_name,
                                 " (", exp_name, ")"))
                }
              }
            }
            
            # Update progress
            rv_nmr$n_copied <- cnt_ok
            incProgress(1 / total)
          }
        }
      }
    })
    
    
    # Report missing
    missing <- target_names[!found_targets]
    rv_nmr$n_found <- sum(found_targets)
    rv_nmr$n_missing <- length(missing)
    rv_nmr$n_copied <- cnt_ok
    
    if (length(missing) > 0) {
      nmr_log(paste0("--- ", length(missing), " samples NOT FOUND ---"))
      for (nm in head(missing, 20)) {
        nmr_log(paste0("  [NICHT GEFUNDEN] ", nm))
      }
      if (length(missing) > 20) nmr_log(paste0("  ... und ", length(missing) - 20, " weitere"))
    }
    
    nmr_log(paste0("--- Copy complete: ", cnt_ok, " copied, ",
                   length(missing), " not found, ", cnt_error, " errors ---"))
    
    # ---- PepsNMR Bucketing ----
    if (input$nmr_do_bucketing && cnt_ok > 0) {
      nmr_log("")
      nmr_log("=== PepsNMR Bucketing started ===")
      
      tryCatch({
        library(PepsNMR)
        
        sample_dirs <- list.dirs(dest_base, recursive = FALSE, full.names = TRUE)
        
        # Group by TD value
        get_td <- function(sample_path) {
          exp_dirs <- list.dirs(sample_path, recursive = FALSE, full.names = TRUE)
          for (e in exp_dirs) {
            acqus_file <- file.path(e, "acqus")
            if (!file.exists(acqus_file)) next
            lines <- readLines(acqus_file, warn = FALSE)
            td_line <- grep("^##\\$TD=", lines, value = TRUE)
            if (length(td_line) > 0) {
              return(as.integer(gsub("^##\\$TD= *", "", td_line[1])))
            }
          }
          return(NA_integer_)
        }
        
        td_per_sample <- sapply(sample_dirs, get_td)
        td_groups <- split(sample_dirs, td_per_sample)
        nmr_log(paste0("TD groups found: ", paste(names(td_groups), collapse = ", ")))
        
        all_spectra <- list()
        
        withProgress(message = "PepsNMR Processing...", value = 0, {
          n_groups <- length(td_groups)
          
          for (g_idx in seq_along(td_groups)) {
            td_val <- names(td_groups)[g_idx]
            group_dirs <- td_groups[[td_val]]
            nmr_log(paste0("  Processing TD=", td_val, " (", length(group_dirs), " samples)..."))
            
            # Create temp directory for this group
            tmp_dir <- file.path(tempdir(), paste0("td_", td_val))
            if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
            dir.create(tmp_dir, recursive = TRUE)
            
            for (s in group_dirs) {
              link_path <- file.path(tmp_dir, basename(s))
              fs::dir_copy(s, link_path)
            }
            
            # PepsNMR pipeline
            fidList <- ReadFids(path = tmp_dir, subdirs = TRUE)
            Fid_data <- fidList[["Fid_data"]]
            Fid_info <- fidList[["Fid_info"]]
            
            Fid_data <- GroupDelayCorrection(Fid_data, Fid_info)
            Fid_data <- SolventSuppression(Fid_data)
            Fid_data <- Apodization(Fid_data, Fid_info)
            Spectrum_data <- FourierTransform(Fid_data, Fid_info)
            Spectrum_data <- ZeroOrderPhaseCorrection(Spectrum_data)
            Spectrum_data <- InternalReferencing(Spectrum_data, Fid_info)
            Spectrum_data <- BaselineCorrection(Spectrum_data)
            Spectrum_data <- NegativeValuesZeroing(Spectrum_data)
            Spectrum_data <- WindowSelection(Spectrum_data,
                                             from.ws = input$nmr_ws_from,
                                             to.ws = input$nmr_ws_to)
            Spectrum_data <- Warping(Spectrum_data)
            Spectrum_data <- RegionRemoval(Spectrum_data, typeofspectra = input$nmr_spectra_type)
            
            all_spectra[[td_val]] <- Spectrum_data
            
            # Cleanup
            unlink(tmp_dir, recursive = TRUE)
            nmr_log(paste0("    -> ", nrow(Spectrum_data), " Spektren, ",
                           ncol(Spectrum_data), " Punkte"))
            
            incProgress(1 / n_groups)
          }
        })
        
        # Combine onto common ppm grid
        if (length(all_spectra) == 1) {
          final_spectra <- all_spectra[[1]]
        } else {
          nmr_log("  Interpolation auf gemeinsame ppm-Achse...")
          ppm_axes <- lapply(all_spectra, function(x) as.numeric(colnames(x)))
          densest <- which.max(sapply(ppm_axes, length))
          common_ppm <- ppm_axes[[densest]]
          
          final_spectra <- matrix(NA_real_,
                                  nrow = sum(sapply(all_spectra, nrow)),
                                  ncol = length(common_ppm))
          colnames(final_spectra) <- common_ppm
          row_names <- character(0)
          row_offset <- 0L
          
          for (sp in all_spectra) {
            sp_ppm <- as.numeric(colnames(sp))
            for (i in seq_len(nrow(sp))) {
              interpolated <- approx(x = sp_ppm, y = sp[i, ],
                                     xout = common_ppm, rule = 2)$y
              final_spectra[row_offset + i, ] <- interpolated
            }
            row_names <- c(row_names, rownames(sp))
            row_offset <- row_offset + nrow(sp)
          }
          rownames(final_spectra) <- row_names
        }
        
        # Save result
        out_df <- data.frame(
          SampleName = rownames(final_spectra),
          final_spectra,
          check.names = FALSE
        )
        
        rv_nmr$bucketing_result <- out_df
        
        # Write CSV if path specified
        csv_path <- input$nmr_output_csv
        if (!is.null(csv_path) && nchar(csv_path) > 0) {
          if (!grepl("[\\/]", csv_path)) {
            csv_path <- file.path(dest_base, csv_path)
          }
          write.csv(out_df, file = csv_path, row.names = FALSE)
          nmr_log(paste0("  CSV gespeichert: ", csv_path))
        }
        
        nmr_log(paste0("=== PepsNMR complete: ", nrow(out_df), " spectra x ",
                       ncol(out_df) - 1, " Punkte ==="))
        
      }, error = function(e) {
        nmr_log(paste0("  [ERROR] PepsNMR: ", e$message))
      })
    }
    
    rv_nmr$running <- FALSE
    nmr_log("=== Done! ===")
  })
  
  # ---- Bucketing result table ----
  output$nmr_bucketing_result <- renderDT({
    req(rv_nmr$bucketing_result)
    df <- rv_nmr$bucketing_result
    
    # Show first 20 columns for preview
    preview_cols <- min(ncol(df), 20)
    df_preview <- df[, 1:preview_cols]
    
    datatable(df_preview, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
              rownames = FALSE) %>%
      formatRound(columns = 2:preview_cols, digits = 2)
  })
  
  # ---- Download bucketing CSV ----
  output$nmr_dl_bucketing <- downloadHandler(
    filename = function() {
      paste0("signal_intensities_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(rv_nmr$bucketing_result)
      write.csv(rv_nmr$bucketing_result, file, row.names = FALSE)
    }
  )
  
  


} # END OF SERVER

# RUN APP
shinyApp(ui = ui, server = server)
