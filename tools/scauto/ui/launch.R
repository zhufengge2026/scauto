#!/usr/bin/env Rscript
# Open the PBMC Auto-Pilot GUI in a local browser.
# Helper packages (shiny, …) are installed here if missing.
# Analysis packages (Seurat, …) install when you click Start analysis.
#
#   Rscript tools/scauto/ui/launch.R
#   or double-click Launch.command

find_this <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }
  if (file.exists("tools/scauto/ui/app.R")) {
    return(normalizePath("tools/scauto/ui"))
  }
  normalizePath(getwd())
}

ui_dir <- find_this()
scauto_root <- normalizePath(file.path(ui_dir, ".."))
project <- normalizePath(file.path(scauto_root, "..", ".."))
if (!dir.exists(file.path(project, "viz"))) {
  stop("Cannot find the project root (need viz/palette.R). Start from the project: Rscript tools/scauto/ui/launch.R")
}

options(
  scauto.ui_dir = ui_dir,
  scauto.scauto_root = scauto_root,
  scauto.project = project,
  scauto.rscript = Sys.which("Rscript")
)
Sys.setenv(
  PROJECT_DIR = project,
  LOCAL_PROJECT_DIR = project,
  SCAUTO_PALETTE = Sys.getenv("SCAUTO_PALETTE", "light")
)

source(file.path(scauto_root, "R", "00_packages.R"), local = FALSE)
source(file.path(scauto_root, "R", "00_usage.R"), local = FALSE)
cat("Checking helper packages (installing any that are missing)…\n")
ensure_scauto_packages(
  which_hard = "gui",
  which_soft = character(),
  install = TRUE
)
source(file.path(ui_dir, "helpers.R"), local = FALSE)
source(file.path(scauto_root, "R", "00_utils.R"), local = FALSE)
source(file.path(scauto_root, "R", "01_ingest_qc.R"), local = FALSE)

# Rscript is not AQUA. XQuartz often sets DISPLAY to a launchd socket
# (not ":0"); utils::browseURL then treats the session as remote and
# never calls macOS `open`. Drop that DISPLAY so Launch Services can run.
if (grepl("xquartz|org\\.xquartz|/private/tmp/com\\.apple\\.launchd",
          Sys.getenv("DISPLAY"), ignore.case = TRUE)) {
  Sys.unsetenv("DISPLAY")
}

port_is_free <- function(port, host = "127.0.0.1") {
  tryCatch({
    con <- suppressWarnings(socketConnection(
      host = host, port = as.integer(port), blocking = TRUE,
      server = FALSE, open = "r+", timeout = 0.4
    ))
    close(con)
    FALSE
  }, error = function(e) TRUE)
}

pick_scauto_port <- function(preferred) {
  preferred <- as.integer(preferred)
  if (!is.finite(preferred) || preferred < 1L) preferred <- 8765L
  for (p in as.integer(preferred:(preferred + 20L))) {
    if (port_is_free(p)) return(p)
  }
  stop(
    "No free port in ", preferred, "–", preferred + 20L,
    ". Close an old Auto-Pilot Terminal window and try Launch.command again."
  )
}

# Open once, after the port is accepting. Never wait=TRUE: shiny starts
# httpuv after launch.browser, so a blocking `open` can leave a blank page.
# Do not also pass a Shiny launch.browser opener or osascript — those
# used to open two extra windows on macOS.
scauto_schedule_browser <- function(url, port) {
  if (identical(Sys.getenv("SCAUTO_NO_BROWSER"), "1")) {
    cat("SCAUTO_NO_BROWSER=1 — not opening a browser.\n")
    return(invisible(url))
  }
  cat("Opening ", url, " in your browser…\n", sep = "")
  cat("If no window appears, paste this address into Safari or Chrome:\n  ", url, "\n", sep = "")
  flush.console()
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Darwin")) {
    sh <- tempfile("scauto_open_", fileext = ".sh")
    writeLines(
      c(
        "#!/bin/bash",
        "unset DISPLAY",
        sprintf("URL=%s", shQuote(url, type = "sh")),
        sprintf("PORT=%s", as.integer(port)),
        "ok=0",
        "for _ in $(seq 1 80); do",
        "  if bash -c \"echo >/dev/tcp/127.0.0.1/${PORT}\" >/dev/null 2>&1; then ok=1; break; fi",
        "  sleep 0.25",
        "done",
        "[ \"$ok\" -eq 1 ] || exit 1",
        "sleep 0.4",
        "/usr/bin/open \"$URL\""
      ),
      sh
    )
    Sys.chmod(sh, mode = "0755")
    system(sprintf("/bin/bash %s >/dev/null 2>&1 &", shQuote(sh)))
  } else if (identical(sys, "Windows")) {
    utils::browseURL(url)
  } else {
    xdg <- Sys.which("xdg-open")
    opener <- if (nzchar(xdg)) xdg else "xdg-open"
    system(
      sprintf(
        "(for i in $(seq 1 80); do (echo >/dev/tcp/127.0.0.1/%s) >/dev/null 2>&1 && break; sleep 0.25; done; sleep 0.4; %s %s) >/dev/null 2>&1 &",
        as.integer(port),
        shQuote(opener),
        shQuote(url)
      ),
      intern = FALSE,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE,
      wait = FALSE
    )
  }
  invisible(url)
}

preferred_port <- as.integer(Sys.getenv("SCAUTO_PORT", "8765"))
port <- pick_scauto_port(preferred_port)
if (!identical(port, preferred_port)) {
  cat(
    "Port ", preferred_port, " is already in use (an old Auto-Pilot window may still be open). Using ",
    port, ".\n",
    sep = ""
  )
}
app_url <- paste0("http://127.0.0.1:", port)
cat("Open a browser at ", app_url, "\n", sep = "")
cat("Computation is local. Closing this window stops the app.\n")
cat("Start analysis installs any missing analysis packages (needs internet the first time).\n")
scauto_schedule_browser(app_url, port)
ok <- tryCatch(
  {
    shiny::runApp(
      appDir = ui_dir,
      port = port,
      host = "127.0.0.1",
      launch.browser = FALSE
    )
    TRUE
  },
  error = function(e) {
    cat("\nThe helper did not start:\n", conditionMessage(e), "\n", sep = "")
    cat("If a browser window is empty, paste:\n  ", app_url, "\n", sep = "")
    FALSE
  }
)
if (!isTRUE(ok)) quit(status = 1L)
