# Usage log: who ran Auto-Pilot, how many samples, how many cells.
# Append-only CSV under tools/scauto/logs/. No passwords or HPC accounts.

scauto_usage_user <- function() {
  u <- Sys.getenv("SCAUTO_USAGE_USER", "")
  if (!nzchar(u)) u <- Sys.getenv("USER", Sys.getenv("USERNAME", ""))
  if (!nzchar(u) || identical(u, "unknown")) {
    u <- as.character(Sys.info()[["user"]])
  }
  u <- trimws(as.character(u)[1])
  if (!nzchar(u) || is.na(u)) "unknown" else u
}

scauto_usage_host <- function() {
  h <- Sys.getenv("SCAUTO_USAGE_HOST", "")
  if (!nzchar(h)) h <- as.character(Sys.info()[["nodename"]])
  h <- trimws(as.character(h)[1])
  if (!nzchar(h) || is.na(h)) "unknown" else h
}

scauto_usage_dir <- function(project = "") {
  if (is.null(project) || !nzchar(project)) {
    project <- getOption("scauto.project", Sys.getenv("PROJECT_DIR", ""))
  }
  d <- file.path(project, "tools", "scauto", "logs")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

scauto_usage_log_path <- function(project = "") {
  file.path(scauto_usage_dir(project), "usage_log.csv")
}

scauto_usage_summary_path <- function(project = "") {
  file.path(scauto_usage_dir(project), "usage_by_user.csv")
}

scauto_usage_fields <- function() {
  c(
    "time", "event", "user", "host", "mode",
    "n_samples", "n_cells_before_qc", "n_cells_raw", "n_cells_qc",
    "samples", "outdir", "status"
  )
}

scauto_usage_clean <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  gsub("[\r\n\"]", " ", x)
}

scauto_usage_report_cfg <- function(project = "") {
  if (is.null(project) || !nzchar(project)) {
    project <- getOption("scauto.project", Sys.getenv("PROJECT_DIR", ""))
  }
  env_url <- trimws(Sys.getenv("SCAUTO_USAGE_REPORT_URL", ""))
  cfg <- list(enable = TRUE, url = env_url)
  p <- file.path(project, "tools", "scauto", "config", "usage_report.json")
  if (file.exists(p) && requireNamespace("jsonlite", quietly = TRUE)) {
    raw <- tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE), error = function(e) NULL)
    if (is.list(raw)) {
      if (!is.null(raw$enable)) cfg$enable <- isTRUE(raw$enable)
      if (!is.null(raw$url) && nzchar(trimws(as.character(raw$url)[1]))) {
        cfg$url <- trimws(as.character(raw$url)[1])
      }
    }
  }
  if (nzchar(env_url)) cfg$url <- env_url
  cfg
}

# Fire-and-forget POST to the lab sheet. Never abort analysis.
scauto_usage_send_remote <- function(row, project = "") {
  cfg <- scauto_usage_report_cfg(project)
  url <- trimws(as.character(cfg$url)[1])
  if (!isTRUE(cfg$enable) || !nzchar(url)) return(invisible(FALSE))
  if (identical(Sys.getenv("SCAUTO_NO_PKG_INSTALL"), "1")) return(invisible(FALSE))
  payload <- list(
    tool = "scauto",
    time = as.character(row$time[[1]]),
    event = as.character(row$event[[1]]),
    user = as.character(row$user[[1]]),
    host = as.character(row$host[[1]]),
    os = as.character(Sys.info()[["sysname"]]),
    mode = as.character(row$mode[[1]]),
    n_samples = row$n_samples[[1]],
    n_cells_before_qc = row$n_cells_before_qc[[1]],
    n_cells_raw = row$n_cells_raw[[1]],
    n_cells_qc = row$n_cells_qc[[1]],
    samples = as.character(row$samples[[1]]),
    outdir = basename(as.character(row$outdir[[1]])),
    status = as.character(row$status[[1]])
  )
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(FALSE))
  body <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  tmp <- tempfile(fileext = ".json")
  writeLines(body, tmp, useBytes = TRUE)
  on.exit(unlink(tmp), add = TRUE)
  curl <- Sys.which("curl")
  if (!nzchar(curl)) return(invisible(FALSE))
  sink <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  ok <- tryCatch({
    code <- system2(
      curl,
      c(
        "-sS", "-o", sink, "-w", "%{http_code}",
        "-m", "8",
        "-X", "POST",
        "-H", "Content-Type: application/json",
        "--data-binary", paste0("@", tmp),
        url
      ),
      stdout = TRUE, stderr = FALSE
    )
    grepl("^[23][0-9][0-9]$", paste(code, collapse = ""))
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

scauto_usage_read <- function(project = "") {
  p <- scauto_usage_log_path(project)
  if (!file.exists(p) || isTRUE(file.info(p)$size < 8)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  tab <- tryCatch(
    utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame(stringsAsFactors = FALSE)
  )
  if (!nrow(tab)) return(tab)
  for (nm in scauto_usage_fields()) {
    if (!nm %in% names(tab)) tab[[nm]] <- ""
  }
  tab[, scauto_usage_fields(), drop = FALSE]
}

scauto_usage_append <- function(project = "", event = "start", mode = "local",
                                n_samples = NA, n_cells_before_qc = NA,
                                n_cells_raw = NA, n_cells_qc = NA,
                                samples = character(), outdir = "",
                                status = "") {
  tryCatch({
    row <- data.frame(
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      event = scauto_usage_clean(event),
      user = scauto_usage_clean(scauto_usage_user()),
      host = scauto_usage_clean(scauto_usage_host()),
      mode = scauto_usage_clean(mode),
      n_samples = if (length(n_samples) && is.finite(as.numeric(n_samples)[1])) {
        as.integer(as.numeric(n_samples)[1])
      } else {
        NA_integer_
      },
      n_cells_before_qc = if (length(n_cells_before_qc) && is.finite(as.numeric(n_cells_before_qc)[1])) {
        as.integer(round(as.numeric(n_cells_before_qc)[1]))
      } else {
        NA_integer_
      },
      n_cells_raw = if (length(n_cells_raw) && is.finite(as.numeric(n_cells_raw)[1])) {
        as.integer(round(as.numeric(n_cells_raw)[1]))
      } else {
        NA_integer_
      },
      n_cells_qc = if (length(n_cells_qc) && is.finite(as.numeric(n_cells_qc)[1])) {
        as.integer(round(as.numeric(n_cells_qc)[1]))
      } else {
        NA_integer_
      },
      samples = scauto_usage_clean(paste(as.character(samples), collapse = ";")),
      outdir = scauto_usage_clean(outdir),
      status = scauto_usage_clean(status),
      stringsAsFactors = FALSE
    )
    p <- scauto_usage_log_path(project)
    newf <- !file.exists(p) || isTRUE(file.info(p)$size < 8)
    utils::write.table(
      row, p,
      sep = ",", row.names = FALSE, col.names = newf, append = !newf,
      qmethod = "double", fileEncoding = "UTF-8"
    )
    scauto_usage_write_summary(project)
    out_ev <- file.path(as.character(outdir)[1], "logs", "usage_event.csv")
    if (nzchar(as.character(outdir)[1])) {
      dir.create(dirname(out_ev), recursive = TRUE, showWarnings = FALSE)
      utils::write.csv(row, out_ev, row.names = FALSE, fileEncoding = "UTF-8")
    }
    scauto_usage_send_remote(row, project)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

scauto_usage_has_event <- function(project, event, outdir) {
  outdir <- trimws(as.character(outdir)[1])
  if (!nzchar(outdir)) return(FALSE)
  tab <- scauto_usage_read(project)
  if (!nrow(tab)) return(FALSE)
  any(tab$event == event & trimws(as.character(tab$outdir)) == outdir)
}

scauto_usage_write_summary <- function(project = "") {
  tab <- scauto_usage_read(project)
  if (!nrow(tab)) return(invisible(NULL))
  starts <- tab[tab$event == "start", , drop = FALSE]
  finishes <- tab[tab$event == "finish", , drop = FALSE]
  users <- unique(c(starts$user, finishes$user))
  users <- users[nzchar(users)]
  if (!length(users)) return(invisible(NULL))
  rows <- lapply(users, function(u) {
    s <- starts[starts$user == u, , drop = FALSE]
    f <- finishes[finishes$user == u, , drop = FALSE]
    data.frame(
      user = u,
      n_runs = nrow(s),
      n_finished = sum(f$status == "ok"),
      samples_sum = sum(as.numeric(s$n_samples), na.rm = TRUE),
      cells_before_qc_sum = sum(as.numeric(s$n_cells_before_qc), na.rm = TRUE),
      cells_after_qc_sum = sum(as.numeric(f$n_cells_qc), na.rm = TRUE),
      last_run = {
        tms <- c(s$time, f$time)
        tms <- tms[nzchar(tms)]
        if (length(tms)) tms[[length(tms)]] else ""
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$n_runs, out$user), , drop = FALSE]
  utils::write.csv(out, scauto_usage_summary_path(project), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out)
}

scauto_usage_record_start <- function(project, samples, n_cells_before_qc,
                                      outdir, mode = "local") {
  samples <- as.character(samples)
  scauto_usage_append(
    project = project,
    event = "start",
    mode = mode,
    n_samples = length(samples),
    n_cells_before_qc = n_cells_before_qc,
    samples = samples,
    outdir = outdir,
    status = "started"
  )
}

scauto_usage_record_finish <- function(project, outdir, ok = TRUE, mode = "") {
  outdir <- as.character(outdir)[1]
  if (!nzchar(outdir) || scauto_usage_has_event(project, "finish", outdir)) {
    return(invisible(FALSE))
  }
  n_raw <- NA_real_
  n_qc <- NA_real_
  samples <- character()
  n_samp <- NA_real_
  man <- file.path(outdir, "run_manifest.json")
  if (file.exists(man) && requireNamespace("jsonlite", quietly = TRUE)) {
    raw <- tryCatch(jsonlite::fromJSON(man, simplifyVector = TRUE), error = function(e) NULL)
    if (is.list(raw)) {
      n_raw <- suppressWarnings(as.numeric(raw$n_raw)[1])
      n_qc <- suppressWarnings(as.numeric(raw$n_qc)[1])
      samples <- unlist(raw$samples, use.names = FALSE)
      n_samp <- length(samples)
    }
  }
  ev <- file.path(outdir, "logs", "usage_event.csv")
  if ((!length(samples) || !is.finite(n_qc)) && file.exists(ev)) {
    prev <- tryCatch(utils::read.csv(ev, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(prev) && nrow(prev)) {
      if (!length(samples) && "samples" %in% names(prev)) {
        samples <- unlist(strsplit(as.character(prev$samples[[1]]), ";", fixed = TRUE))
      }
      if (!is.finite(n_samp) && "n_samples" %in% names(prev)) {
        n_samp <- suppressWarnings(as.numeric(prev$n_samples[[1]]))
      }
    }
  }
  if (!nzchar(mode)) {
    mode <- if (grepl("scauto_hpc_", basename(outdir), fixed = TRUE)) "hpc" else "local"
  }
  scauto_usage_append(
    project = project,
    event = "finish",
    mode = mode,
    n_samples = n_samp,
    n_cells_raw = n_raw,
    n_cells_qc = n_qc,
    samples = samples,
    outdir = outdir,
    status = if (isTRUE(ok)) "ok" else "failed"
  )
}

scauto_usage_summary_ui <- function(project) {
  tab <- tryCatch(scauto_usage_write_summary(project), error = function(e) NULL)
  if (is.null(tab) || !nrow(tab)) {
    return(p(
      class = "hint",
      "Usage is recorded when someone clicks Start analysis or Submit to HPC. A copy is sent to the lab usage sheet when this computer is online. Local file: ",
      tags$code("tools/scauto/logs/usage_log.csv"),
      "."
    ))
  }
  pretty <- tab
  pretty$samples_sum <- format(pretty$samples_sum, big.mark = ",")
  pretty$cells_before_qc_sum <- format(pretty$cells_before_qc_sum, big.mark = ",")
  pretty$cells_after_qc_sum <- format(pretty$cells_after_qc_sum, big.mark = ",")
  names(pretty) <- c(
    "User", "Runs started", "Runs finished",
    "Samples (sum)", "Cells before QC (sum)", "Cells after QC (sum)", "Last run"
  )
  tagList(
    p(
      class = "hint",
      "Each Start / HPC submit is one run and is also sent to the lab Google Sheet when online. Sample and cell totals add every run. Local file: ",
      tags$code("tools/scauto/logs/usage_log.csv"),
      "."
    ),
    tags$table(
      class = "table table-sm sample-table",
      tags$thead(tags$tr(lapply(names(pretty), tags$th))),
      tags$tbody(lapply(seq_len(nrow(pretty)), function(i) {
        tags$tr(lapply(as.list(pretty[i, , drop = FALSE]), tags$td))
      }))
    )
  )
}
