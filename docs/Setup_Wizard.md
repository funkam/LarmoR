# User Guide

Brief intro: what this guide covers, who it's for, and a pointer to
docs/setup-wizard.md for first-time users.

## Before you start
<!-- Prerequisites common to all modules: completed setup, archive
     initialised, where data is expected to live. -->

## Navigation overview
<!-- Screenshot of the navbar. Which tabs are core, which are optional
     and may not appear. How module visibility is controlled. -->

---

# Sample Submission

## Purpose
<!-- One or two sentences. What it produces and for what downstream use. -->

## Prerequisites
<!-- Projects registered? Boxes registered? Archive columns configured? -->

## Creating a submission from manual entry

### Step 1 — Select project
### Step 2 — Choose sample type
### Step 3 — Select experiments
### Step 4 — Set rack and starting position
<!-- Rack numbering, position numbering, what happens at position 96,
     how wraparound works. This is the part users get wrong. -->
### Step 5 — Enter sample metadata
### Step 6 — Generate and export

## Creating a submission from file import

### Accepted file formats
### Required columns
### Optional columns
### Column mapping
### Common import errors
<!-- Cross-reference docs/data-formats.md for the full spec. -->

## Field reference

| Field | Required | Format | Notes |
|---|---|---|---|

## Output files
<!-- What the XLSX contains, where it's saved, how ICON consumes it. -->

## What gets written to the archive
<!-- Which fields persist, and when. -->

## Common mistakes
<!-- Duplicate positions, rack overflow, missing project, wrong sample type. -->

---

# Data Extraction

## Purpose

## Prerequisites
<!-- Bruker folder structure, which IVDr methods must have been run. -->

## Supported report types

| Method | Matrix | What's extracted |
|---|---|---|
| B.I.Quant-PS | | |
| B.I.Quant-UR | | |
| B.I.LISA | | |
| B.I.Biobank-QC | | |

## Scanning a directory

### Step 1 — Select the data folder
### Step 2 — Review the scan results
<!-- What the results table columns mean. -->
### Step 3 — Interpreting missing reports
<!-- What "missing XML" means practically — not measured, failed,
     or exported elsewhere. What to do about it. -->

## Extracting data

### Selecting samples
### Choosing report types
### Export options
<!-- Multi-sheet Excel vs individual CSVs. What each sheet contains. -->

## Automatic downstream actions
<!-- QC samples → QC Monitor. Results → Measurement Archive.
     Sample list → Spectra Viewer. Make clear these happen without
     the user asking. -->

## QC panel summary
<!-- How to read it, what the flags mean. -->

## Common mistakes

---

# Sample Archive

## Purpose

## Viewing and filtering

### Filter controls
### Saved filters
### Creating a sub-archive

## Editing entries
<!-- What can be edited in-app, what can't, and why. -->

## Project reports

## Adding custom columns
<!-- Brief — point to docs/configuration.md for detail. Note the
     restart requirement. -->

## Exporting

---

# Projects

## Purpose

## Registering a project

## Project fields

| Field | Required | Notes |
|---|---|---|

## Custom project columns
<!-- Note Title and Abbreviation are protected. -->

## How projects link to other modules
<!-- Submission, biobank, archive filtering, NMR Copy. -->

---

# Measurement Archive

## Purpose
<!-- Distinguish clearly from the Sample Archive — this is
     measurement-level results, not sample metadata. -->

## How entries are created
<!-- Automatic on extraction. -->

## Browsing and filtering

## Exporting

---

# Biobank

## Purpose

## Registering boxes

## Assigning boxes to projects

## Box status and completion tracking
<!-- What the statuses mean and what changes them. -->

## Interaction with Sample Submission

---

# QC Monitor

## Purpose

## Concepts
<!-- QC sample, parameter, limit, reagent lot. Define these before
     the workflow — this module has the steepest concept curve. -->

## Initial setup

### Step 1 — Define the QC sample pattern
### Step 2 — Add parameters to track
### Step 3 — Set upper and lower limits

## Reagent lot management

### Creating a lot
### Date ranges and how trends are filtered
### Archiving and reactivating
### Deleting a lot
<!-- What happens to historical data. -->

## Reading the trend plots
<!-- Axes, outlier highlighting, what crossing a limit means. -->

## Generating a QC report

## Common mistakes

---

# NMR Copy

## Purpose

## Selecting a source folder

## Choosing what to copy

### From an uploaded sample list
### By date folder
### By project from the archive
### From a filtered archive selection

## Sequence scanning
<!-- What it detects and why it matters. -->

## Running the copy
<!-- Destination, overwrite behaviour, what happens on failure. -->

---

# Spectra Viewer

## Purpose

## Loading spectra

### From the Data Extraction handoff
### Manual selection

## Display controls
<!-- Zoom, pan, region draw. -->

## Multi-spectrum overlay

### Intensity normalisation
<!-- What it does and when it's misleading. -->

## IVDr metabolite region overlay

### What the regions represent
<!-- 310 K chemical shifts, multiplicity, J-coupling. State the
     600 MHz assumption plainly here. -->
### Colour coding by class
### Category filtering

## Acquisition parameter inspection
<!-- Which parameters, where they come from, pulse program display. -->

## Limitations
<!-- Field strength, temperature, referencing. -->

---

# Settings

## Archive columns
## Project columns
## Modules
## QC settings
## When a restart is needed
<!-- Consolidate this — users hit it across several panels. -->
