# Installation

**LarmoR** runs locally on your own computer. Files are only uploaded to the application not to a server.

There are two ways to install it:

- **Automatic** - run `install.bat`. Works on most Windows machines.
- **Manual** - for managed computers, macOS, Linux, or when the automatic installer fails.

If you are on a corporate, hospital, or university computer without administrator rights, go straight to the manual instructions. The automatic installer will probably not work for you.
You might need to get help from the IT derpartment.

If you are well versed in 

---

## Requirements

| | |
|---|---|
| **R** | Version 4.2 or newer |
| **Operating system** | Windows 10/11, macOS, or Linux |
| **Disk space** | ~500 MB for R and packages |
| **Internet** | Needed once, to download packages |
| **Bruker software** | Not required |

While LarmoR provides templates for ICON-NMR it does not require any Bruker software such as TopSpin or ICON-NMR to be installed.
Ideally, the software is located on the PC directly linked to the spectrometer, but it can also work through networked drives, but it will be slower.

## Installation for experienced R users
Here a list of required pacakges to run **LarmoR**:

### CRAN packages

-  `shiny` 
- `bslib` 
- `shinyjs`
- `DT`
- `plotly`
- `htmlwidgets`
- `htmltools`
- `data.table`
- `dplyr`
- `readxl`
- `openxlsx`
- `xml2`
- `jsonlite`
- `fs`
- `future`
- `future.apply`
- `rstudioapi` 

### Bioconductor packages



- `PepsNMR` 




---

## Automatic installation (Windows)

1. Download the LarmoR folder and extract it somewhere you can write to - your Desktop or Documents folder is fine.
2. Double-click **`install.bat`**.
3. If R is not found, the installer offers to download and install it. Accept the defaults.
4. Wait while R packages install. **This takes 5-15 minutes on first run.**
5. When finished, a shortcut appears on your Desktop.

To start LarmoR, double-click the Desktop shortcut or `start.bat`.

> **Keep the black console window open** while using LarmoR. Closing it shuts the app down.

If any step fails, use the manual instructions below.

---

## Manual installation

Use this if:

- You do not have administrator rights
- `install.bat` cannot find R, or fails partway
- You are on macOS or Linux
- Your IT department blocks batch files or PowerShell

### Step 1 - Install R

#### With administrator rights

Download from [cran.r-project.org](https://cran.r-project.org/) and run the installer.

#### Without administrator rights (Windows)

The standard R installer can install to your user folder without elevation.

1. Download the Windows installer from [CRAN](https://cran.r-project.org/bin/windows/base/).
2. Run it. If prompted for an administrator password, click **No** or **Cancel** - the installer will offer to continue as a user install.
3. When asked for the installation folder, change it to somewhere inside your user profile:

   ```
   C:\Users\YOUR-USERNAME\AppData\Local\Programs\R
   ```

4. Complete the installation.

If Windows blocks the installer entirely, ask your IT department to install R. It is widely used, open source, and standard scientific software - most IT departments will approve it.

#### If R is already installed

Check which version:

- Open R or RStudio and look at the startup message, or
- Look in `C:\Program Files\R\` or `C:\Users\YOUR-USERNAME\AppData\Local\Programs\R\`

LarmoR needs **4.2 or newer**.

### Step 2 - Find your R installation

You will need the full path to `Rscript.exe`. Common locations:

```
C:\Program Files\R\R-4.4.1\bin\Rscript.exe
C:\Users\YOUR-USERNAME\AppData\Local\Programs\R\R-4.4.1\bin\Rscript.exe
```

On macOS and Linux it is usually just `Rscript`, already on your PATH.

To confirm it works, open a Command Prompt and run:

```
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" --version
```

Adjust the path to match your installation. You should see a version number.

### Step 3 - Install the required packages

Open **R** or **RStudio** - not the Command Prompt - and paste this:

```r
install.packages(c(
  "shiny", "bslib", "DT", "plotly", "data.table",
  "dplyr", "openxlsx", "xml2", "shinyjs", "jsonlite",
  "htmltools", "future", "future.apply", "readxl",
  "fs", "htmlwidgets", "rstudioapi"
))
```

Then install `PepsNMR`, which comes from Bioconductor rather than CRAN:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("PepsNMR")
```

If asked whether to update other packages, answering **no** is usually safer.

This takes 5-15 minutes.

### Step 4 - Verify

Still in R:

```r
pkgs <- c("shiny", "bslib", "DT", "plotly", "data.table",
          "dplyr", "openxlsx", "xml2", "shinyjs", "jsonlite",
          "htmltools", "future", "future.apply", "readxl",
          "fs", "htmlwidgets", "rstudioapi", "PepsNMR")

missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]

if (length(missing) == 0) {
  cat("All packages installed.\n")
} else {
  cat("Missing:", paste(missing, collapse = ", "), "\n")
}
```

### Step 5 - Start LarmoR

#### From RStudio

Open `app.R` and click **Run App**.

#### From R

```r
setwd("C:/Users/YOUR-USERNAME/Desktop/LarmoR")
shiny::runApp()
```

Note the **forward slashes** - R does not accept single backslashes in paths.

#### From the command line

```
cd C:\Users\YOUR-USERNAME\Desktop\LarmoR
"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" -e "shiny::runApp(launch.browser=TRUE)"
```

The app opens in your default browser. Leave the window running while you use it.

---

## Corporate and managed computers

### No write access to the R library

R needs somewhere to install packages. If the main library is read-only, create a personal one:

```r
dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
.libPaths(Sys.getenv("R_LIBS_USER"))
```

Run that before installing packages. It usually resolves to something like:

```
C:\Users\YOUR-USERNAME\AppData\Local\R\win-library\4.4
```

To make it permanent, create a file called `.Renviron` in your home folder containing:

```
R_LIBS_USER=C:/Users/YOUR-USERNAME/AppData/Local/R/win-library/4.4
```

### Proxy server blocking CRAN

If package downloads fail or hang, you are probably behind a proxy. Ask IT for the proxy address, then in R:

```r
Sys.setenv(http_proxy  = "http://proxy.yourcompany.com:8080")
Sys.setenv(https_proxy = "http://proxy.yourcompany.com:8080")
```

If the proxy needs authentication:

```r
Sys.setenv(http_proxy  = "http://username:password@proxy.yourcompany.com:8080")
Sys.setenv(https_proxy = "http://username:password@proxy.yourcompany.com:8080")
```

To make this permanent, add the same lines to `.Renviron` without `Sys.setenv()`:

```
http_proxy=http://proxy.yourcompany.com:8080
https_proxy=http://proxy.yourcompany.com:8080
```

Some corporate proxies also break HTTPS certificate checks. If you see SSL errors, try:

```r
options(repos = c(CRAN = "http://cloud.r-project.org"))
```

Note this uses plain HTTP. Only do it if HTTPS genuinely does not work, and mention it to IT.

### Batch files or PowerShell are blocked

Skip `install.bat` and `start.bat` entirely. Use the manual steps above - RStudio's **Run App** button needs neither.

### Running from a network drive

LarmoR works from a network location, but:

- Startup is slower
- Directory scanning over the network can be very slow
- Some network paths break R's file handling

If possible, copy LarmoR to a local folder and point it at network data rather than running from the network itself.

### Antivirus blocking the install

Some antivirus software blocks R package downloads or flags `.bat` files. If installation stalls with no error, check your antivirus quarantine log, and ask IT to allowlist your R library folder.

### Telling the launcher where R is

If you want `start.bat` to work but it cannot find R, create a file called `.rpath` in the LarmoR folder containing the full path to `Rscript.exe` on one line:

```
C:\Users\YOUR-USERNAME\AppData\Local\Programs\R\R-4.4.1\bin\Rscript.exe
```

Alternatively, open `install.bat` in a text editor, find the line near the top reading:

```
REM set "RSCRIPT=C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
```

Remove the `REM ` and set the correct path.

---

## macOS

1. Install R from [CRAN](https://cran.r-project.org/bin/macosx/).
2. Open R or RStudio.
3. Run the package installation commands from Step 3.
4. Open `app.R` in RStudio and click **Run App**, or:

   ```r
   setwd("~/Desktop/LarmoR")
   shiny::runApp()
   ```

The `.bat` files are Windows-only and can be ignored.

---

## Linux

Install R through your package manager:

```bash
# Debian / Ubuntu
sudo apt install r-base

# Fedora
sudo dnf install R
```

Some R packages need system libraries to compile. On Debian/Ubuntu:

```bash
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev
```

Then install the R packages as in Step 3, and run:

```r
setwd("~/LarmoR")
shiny::runApp()
```

---

## First launch

On first start, LarmoR runs a setup wizard that asks for your lab name, NMR data folder, and which optional modules you want. See [the setup wizard guide](setup-wizard.md).

The wizard creates `config.json` and several CSV files in the LarmoR folder. These hold your settings and data - back them up, and do not share them publicly.

---

## Troubleshooting

### "R was not found"

R is either not installed or installed somewhere the launcher does not check. See *Telling the launcher where R is* above, or use the manual start method.

### Package installation fails

Try installing one package at a time to see which fails:

```r
install.packages("shiny")
```

Read the error. Common causes:

| Message mentions | Likely cause |
|---|---|
| `cannot open URL`, timeouts | Proxy or firewall |
| `permission denied`, `cannot create` | No write access to library |
| `compilation failed` | Missing build tools |
| `is not available for R version` | R too old, or package not yet built for a very new R |

### "compilation failed" on Windows

Some packages need [Rtools](https://cran.r-project.org/bin/windows/Rtools/) to build from source. Installing it usually requires admin rights.

Often you can avoid compiling by requesting the pre-built binary:

```r
install.packages("PACKAGE-NAME", type = "binary")
```

### PepsNMR will not install

It comes from Bioconductor, not CRAN. `install.packages("PepsNMR")` will not work. Use:

```r
BiocManager::install("PepsNMR")
```

If Bioconductor has no build for your R version, install a slightly older R, or report it as an issue.

### The browser does not open

Look in the console for a line like:

```
Listening on http://127.0.0.1:4321
```

Copy that address into your browser manually.

### The app starts but the console window closes immediately

Open a Command Prompt, navigate to the folder, and run `start.bat` from there. The error will stay visible.

### Still stuck

Open an issue on GitHub with:

- Your operating system and R version
- Which step failed
- The exact error message
- Contents of `error_log.txt` if present

**Before posting, remove any sample identifiers, project names, patient data, or internal network paths.**

---

## Upgrading

1. Back up `config.json` and all `.csv` files from the LarmoR folder.
2. Download the new version to a **new** folder.
3. Copy your `config.json` and CSV files across.
4. Run `install.bat` again, or reinstall packages manually, in case dependencies changed.

Your settings and data are preserved.

---

## Uninstalling

Delete the LarmoR folder and the Desktop shortcut.

R and the packages stay installed. To remove them, uninstall R through Windows Settings, or delete your R library folder.
