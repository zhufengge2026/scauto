# PBMC Auto-Pilot local GUI (Shiny). Launch with tools/scauto/ui/launch.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
})

if (!exists("list_10x_files", mode = "function")) {
  source(file.path(getOption("scauto.ui_dir"), "helpers.R"), local = FALSE)
}
if (!exists("scauto_overlay_sample_cols", mode = "function")) {
  source(file.path(getOption("scauto.scauto_root"), "R", "00_utils.R"), local = FALSE)
}
if (!exists("scauto_run_env_check", mode = "function")) {
  source(file.path(getOption("scauto.scauto_root"), "R", "00_packages.R"), local = FALSE)
}
if (!exists("scauto_usage_record_start", mode = "function")) {
  source(file.path(getOption("scauto.scauto_root"), "R", "00_usage.R"), local = FALSE)
}

INK <- "#2B2B2B"
PAPER <- "#FAF8F4"
NAVY <- "#1F3A5F"
TERR <- "#C45C26"
SAGE <- "#4F7A62"
STEEL <- "#5C6B73"
SAND <- "#C4A36A"
GOLD <- "#E0B429"

step_labels <- c("Data", "Groups", "Settings", "Run")

# Do not wrap this CSS in sprintf(): R caps the format string at 8192 chars.
css <- paste0(
  sprintf(
    ":root { --ink:%s; --paper:%s; --navy:%s; --terr:%s; --sage:%s; --steel:%s; --sand:%s; --gold:%s; --shiny-fade-opacity: 1; }\n",
    INK, PAPER, NAVY, TERR, SAGE, STEEL, SAND, GOLD
  ), "
  body, .navbar { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
  .navbar { background: var(--navy) !important; }
  .navbar-brand, .navbar-brand:hover { color: #fff !important; letter-spacing: 0.02em; }
  .step-bar { display:flex; gap:8px; margin: 6px 0 8px 0; flex-wrap:nowrap; align-items: stretch; }
  .step-pill { padding:8px 14px; border-radius: 999px; font-size:16px; font-weight: 700;
    line-height: 1.2; text-align: center; flex: 1 1 0; color: var(--steel);
    background:#fff; border:1px solid #ddd; text-decoration: none !important; cursor: pointer; }
  .step-pill:hover { border-color: var(--navy); color: var(--navy); }
  .step-pill.active:hover { color:#fff; }
  .step-pill.active { background: var(--navy); color:#fff; border-color: var(--navy); }
  .step-pill.done { background: var(--sage); color:#fff; border-color: var(--sage); }
  .hint { color: var(--steel); font-size: 14px; line-height: 1.5; }
  .settings-lead { color: var(--ink); font-size: 18px; line-height: 1.5; margin: 0 0 12px 0; }
  .data-guide {
    background: #EEF4F0; border-left: 5px solid var(--navy); border-radius: 8px;
    padding: 14px 18px 16px; margin: 0 0 14px 0;
  }
  .data-guide h4 { font-size: 22px; color: var(--navy); margin: 0 0 10px 0; }
  .data-guide-lead { font-size: 17.5px; line-height: 1.45; color: var(--ink); margin: 0 0 10px 0; }
  .data-guide-lead b { color: var(--navy); }
  .data-guide-alt { font-size: 17.5px; line-height: 1.45; color: var(--terr); margin: 0 0 14px 0;
    font-weight: 600; }
  .data-guide-alt .data-guide-prep-link {
    color: var(--navy) !important; font-weight: 700; text-decoration: underline;
    text-underline-offset: 2px; }
  .data-guide-alt .data-guide-prep-link:hover { color: var(--terr) !important; }
  .data-guide-note { font-size: 16.5px; line-height: 1.4; color: var(--navy); margin: 12px 0 0 0; }
  .data-guide .btn { font-size: 16px; padding: 8px 16px; min-height: 42px; display: inline-flex; align-items: center; }
  .data-guide .form-control { font-size: 16.5px !important; min-height: 42px; }
  .data-guide .form-control::placeholder { font-size: 16.5px; color: var(--steel); opacity: 1; }
  .data-guide .shiny-input-container, .data-guide .form-group { margin-bottom: 0 !important; }
  .howto-flow { margin: 0; width: 100%; max-width: 100%; box-sizing: border-box;
    background: #E4EDE6; border: none; border-radius: 8px;
    font-family: 'Avenir Next', Avenir, 'Segoe UI', 'Gill Sans', 'Helvetica Neue', sans-serif;
    font-weight: 400; letter-spacing: 0.01em; }
  .howto-flow, .howto-flow p, .howto-flow h4, .howto-flow span, .howto-flow div {
    font-weight: 400 !important; }
  .howto-body { padding: 10px 12px 12px; }
  .howto-lead-row { display: flex; align-items: center; gap: 12px; margin: 0 0 8px 0; flex-wrap: wrap; }
  .howto-lead { margin: 0; font-size: 22px; font-weight: 400 !important; line-height: 1.15;
    color: var(--navy); font-family: inherit; }
  .howto-flow .btn-guide {
    font-size: 18px !important; font-weight: 700 !important; line-height: 1.2 !important;
    padding: 10px 16px !important; border-radius: 8px !important; width: auto !important;
    white-space: nowrap; text-align: center;
  }
  .howto-row-scale { width: 100%; display: flex; justify-content: center; overflow: hidden; }
  .howto-row { display: flex; flex-wrap: nowrap; align-items: center; gap: 4px; width: max-content; }
  .howto-step, .howto-join {
    flex: 0 0 158px; width: 158px; height: 158px; box-sizing: border-box;
    background: #C5D9CC; border: none; border-radius: 50%;
    padding: 16px 14px; min-width: 158px;
    display: flex; align-items: center; justify-content: center; text-align: center;
  }
  .howto-ttl { font-weight: 400 !important; color: var(--ink); font-size: 20px; line-height: 1.2;
    font-family: inherit; white-space: normal; max-width: 6.8em; }
  .howto-arrow { align-self: center; color: var(--terr); font-size: 36px; font-weight: 400 !important;
    padding: 0 2px; line-height: 0.8; flex: 0 0 auto; text-shadow: none;
    font-family: inherit; }
  .howto-choice {
    flex: 0 0 auto; width: max-content; height: 158px; min-height: 158px; box-sizing: border-box;
    background: #C5D9CC; border: none; border-radius: 79px;
    padding: 16px 14px; display: flex; flex-direction: column;
    align-items: center; justify-content: center; text-align: center; gap: 2px;
  }
  .howto-choice-a, .howto-choice-b {
    margin: 0; font-size: 20px; line-height: 1.15; font-family: inherit; font-weight: 400 !important;
    white-space: nowrap;
  }
  .howto-choice-a { color: var(--navy); }
  .howto-choice-b { color: var(--navy); }
  .howto-or { margin: 2px 0; font-size: 16px; letter-spacing: 0.08em; line-height: 1;
    color: var(--ink); font-weight: 400 !important; }
  .path-box { font-family: ui-monospace, Menlo, monospace; font-size: 13px;
    background:#fff; border:1px solid #ddd; padding:10px 12px; }
  .warn-box { background:#F5EDE4; border-left: 4px solid var(--terr); padding:10px 12px; }
  .ok-box { background:#EEF4F0; border-left: 4px solid var(--sage); padding:10px 12px; }
  .log-box { background: var(--ink); color:#F5F2EB; font-family: ui-monospace, Menlo, monospace;
    font-size:12px; padding:12px; height: 280px; overflow:auto; overflow-anchor: none;
    white-space: pre-wrap; min-height: 0; }
  /* Shiny/bslib fade recalculating outputs (and their children) to 0.3. The Run
     page polls the log and file list, so the whole page would stay grey. */
  .recalculating, .recalculating > * {
    opacity: 1 !important; transition: none !important;
    pointer-events: auto !important;
  }
  .btn-stop, #stop_run, #stop_run_always, #confirm_stop_run {
    pointer-events: auto !important; opacity: 1 !important; z-index: 100001;
  }
  .log-box, .log-box .shiny-text-output, #log_text {
    background: var(--ink) !important; color: #F5F2EB !important;
    border: 0 !important; opacity: 1 !important;
  }
  .log-box .shiny-text-output, .log-box #log_text {
    display: block; overflow: visible !important; max-height: none !important;
    height: auto !important; white-space: pre-wrap;
  }
  .file-preview { background:#fff; border:1px solid #e6e0d6; padding:12px; max-height: 420px;
    overflow:auto; font-size: 12.5px; }
  .file-list { list-style: none; padding-left: 0; margin: 0 0 12px 0; max-height: 220px;
    overflow: auto; border: 1px solid #e6e0d6; background: #fff; }
  .file-list.browse-list { max-height: 138px; overflow-y: auto; overflow-x: hidden; }
  .browse-row { display:flex; align-items:center; gap:10px; padding:7px 12px;
    border-bottom:1px solid #f0ece4; background:#fff; }
  .browse-row.picked { background:#EEF4F0; }
  .browse-row .name { color: var(--navy); text-decoration:none; font-weight:600; flex:1; }
  .browse-row .open-link { font-size:12px; color: var(--steel); text-decoration:none; }
  .browse-row .open-link:hover { color: var(--navy); }
  .sample-table .shiny-input-container { margin-bottom: 0 !important; min-height: auto; }
  .file-list li { margin: 0; border-bottom: 1px solid #f0ece4; }
  .file-list a { display: block; padding: 8px 12px; text-decoration: none; color: var(--navy); }
  .file-list a:hover, .file-list a.active { background: #EEF4F0; }
  .file-list .meta { color: var(--steel); font-size: 12px; margin-left: 8px; }
  .sample-table a { color: var(--navy); text-decoration: none; font-weight: 600; }
  .sample-table tr.selected { background: #EEF4F0; }
  .gallery img, .preview-img { max-width:100%; border:1px solid #e6e0d6; background:#fff; }
  .btn-run { background: var(--sage) !important; border-color: var(--sage) !important; color:#fff !important; }
  .btn-running, .btn-running:disabled, .btn-running.disabled {
    background: #32E87A !important; border-color: #C8FFDC !important; color: #06381A !important;
    opacity: 1 !important; cursor: wait !important; pointer-events: none;
    text-shadow: 0 0 10px rgba(255,255,255,0.9);
    animation: run-glow 1.35s ease-in-out infinite;
  }
  .btn-running-steady, .btn-running-steady:disabled, .btn-running-steady.disabled {
    background: #32E87A !important; border-color: #C8FFDC !important; color: #06381A !important;
    opacity: 1 !important; cursor: wait !important; pointer-events: none;
    text-shadow: 0 0 8px rgba(255,255,255,0.75);
    animation: none;
    box-shadow: 0 0 0 2px rgba(200,255,220,0.95), 0 0 14px 4px rgba(50,232,122,0.55);
    filter: none;
  }
  .btn-running::before, .btn-running-steady::before {
    content: ''; display: inline-block; width: 0.85em; height: 0.85em;
    margin-right: 0.5em; vertical-align: -0.12em;
    border: 2px solid rgba(6,56,26,0.22); border-top-color: #06381A;
    border-radius: 50%; animation: run-spin 0.75s linear infinite;
  }
  @keyframes run-spin { to { transform: rotate(360deg); } }
  @keyframes run-glow {
    0%, 100% {
      box-shadow: 0 0 0 2px rgba(200,255,220,0.95), 0 0 16px 5px rgba(50,232,122,0.70),
        0 0 36px 10px rgba(50,232,122,0.35);
      filter: brightness(1.04);
    }
    50% {
      box-shadow: 0 0 0 3px #F2FFF7, 0 0 26px 9px rgba(50,232,122,0.95),
        0 0 52px 16px rgba(50,232,122,0.50);
      filter: brightness(1.22);
    }
  }
  .btn-navy { background: var(--navy) !important; border-color: var(--navy) !important; color:#fff !important; }
  .btn-contact { background: var(--terr) !important; border-color: var(--terr) !important; color:#fff !important; }
  .btn-adv { background: var(--gold) !important; border: 2px solid #C49212 !important; color: var(--navy) !important; }
  .btn-adv:hover { filter: brightness(1.08); color: var(--navy) !important; }
  .btn-adv-hero {
    background: var(--gold) !important; border: 3px solid #C49212 !important; color: var(--navy) !important;
    font-size: 1.4rem !important; font-weight: 800 !important; letter-spacing: 0.03em;
    padding: 0.72rem 1.9rem !important; line-height: 1.2; min-height: 56px;
    box-shadow: 0 6px 16px rgba(196, 146, 18, 0.38);
  }
  .btn-adv-hero:hover { filter: brightness(1.08); color: var(--navy) !important; }
  .btn-trial { background: var(--sage) !important; border-color: var(--sage) !important; color:#fff !important; }
  .trial-box { background:#EEF4F0; border:1px solid #c5d6cc; padding:10px 12px; margin-bottom:12px; }
  .mem-float {
    position: fixed; right: 20px; bottom: 92px; z-index: 1040;
    width: min(420px, calc(100vw - 32px));
    background: #FAF8F4; border: 2px solid var(--navy); border-radius: 12px;
    padding: 12px 14px 14px;
  }
  .mem-float.mem-ok { background: #EEF4F0; border-color: var(--sage); }
  .mem-float.mem-tight { background: #F5EDE4; border-color: var(--terr); }
  .mem-float.mem-too_large { background: #F5EDE4; border-color: #9B2C1A; }
  .mem-float.mem-none { background: #FAF8F4; border-color: var(--steel); }
  .mem-float-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 10px;
    margin: 0 0 8px 0; }
  .mem-float-head h5 { margin: 0; flex: 1; font-size: 16px; color: var(--navy); line-height: 1.3; }
  .mem-float p { margin: 0 0 8px 0; font-size: 13.5px; line-height: 1.45; color: var(--ink); }
  .mem-float .hint { color: var(--ink); font-size: 13.5px; }
  .mem-float .mem-close {
    flex: 0 0 auto; font-weight: 700; padding: 4px 12px !important;
    background: #fff !important; border: 1px solid var(--navy) !important; color: var(--navy) !important;
  }
  .mem-float .mem-close:hover { background: var(--navy) !important; color: #fff !important; }
  .mem-gb { font-variant-numeric: tabular-nums; font-weight: 700; color: var(--navy); }
  .done-banner { background:#EEF4F0; border: 3px solid var(--sage); border-radius: 12px;
    padding: 18px 20px; margin: 0 0 16px 0; }
  .done-banner h3 { color: var(--sage); margin: 0 0 8px 0; font-size: 26px; letter-spacing: 0.01em; }
  .done-banner p { margin: 0 0 8px 0; font-size: 16px; line-height: 1.45; }
  .done-banner ul { margin: 8px 0 0 0; padding-left: 1.2em; font-size: 15px; }
  .fail-banner { background:#F5EDE4; border: 3px solid var(--terr); border-radius: 12px;
    padding: 18px 20px; margin: 0 0 16px 0; }
  .fail-banner h3 { color: var(--terr); margin: 0 0 8px 0; font-size: 22px; }
  .run-split { display: grid; grid-template-columns: 1fr 2fr; gap: 16px;
    align-items: stretch; margin: 0 0 16px 0; }
  .run-split-env, .run-split-log { display: flex; flex-direction: column; min-width: 0; }
  .run-split .check-wrap, .run-split .log-box { flex: 1 1 auto; height: 420px;
    max-height: 420px; min-height: 0; overflow: auto; margin: 0; }
  .run-split .check-label { min-width: 0; }
  .run-split h5 { margin-top: 0; }
  @media (max-width: 900px) {
    .run-split { grid-template-columns: 1fr; }
  }
  .check-wrap { max-height: 380px; overflow: auto; border: 1px solid #e6e0d6; background:#fff; margin: 0 0 14px 0; }
  .check-group { font-size: 12px; font-weight: 700; color: var(--navy); padding: 8px 12px 4px; background: #f4f1ea; }
  .check-list { list-style: none; padding: 0; margin: 0; }
  .check-item { display: flex; gap: 10px; align-items: baseline; padding: 5px 12px;
    border-bottom: 1px solid #f0ece4; font-size: 13px; }
  .check-mark { font-family: ui-monospace, Menlo, monospace; font-weight: 700; min-width: 2.2em; }
  .check-ok .check-mark { color: var(--sage); }
  .check-missing .check-mark, .check-fail .check-mark { color: var(--terr); }
  .check-warn .check-mark { color: var(--terr); }
  .check-installing, .check-checking { background: #F5EDE4; }
  .check-pending { color: var(--steel); }
  .check-label { font-weight: 600; color: var(--ink); min-width: 14em; flex-shrink: 0; }
  .check-detail { color: var(--steel); font-size: 12.5px; word-break: break-word; }
  .btn-stop { background: #C0392B !important; border-color: #C0392B !important; color:#fff !important; }
  .btn-download { background: #2E5A8C !important; border-color: #2E5A8C !important; color:#fff !important; }
  .btn-hpc { background: #C45C26 !important; border-color: #C45C26 !important; color:#fff !important; }
  .btn-time { background: var(--sage) !important; border-color: var(--sage) !important; color:#fff !important; }
  .resume-banner { background:#F5EDE4; border: 2px solid var(--navy); border-radius: 12px;
    padding: 14px 16px; margin: 0 0 16px 0; }
  .resume-head { display: flex; justify-content: space-between; align-items: flex-start; gap: 12px; }
  .resume-head p { margin: 0; font-weight: 700; color: var(--navy); flex: 1; }
  .resume-close { flex: 0 0 auto; }
  .prog-wrap { margin: 10px 0 4px 0; }
  .prog-label { margin: 0 0 8px 0; font-weight: 700; color: var(--navy); font-size: 15px; }
  .prog-track { height: 14px; background: #E8E2D8; border-radius: 999px; overflow: hidden; }
  .prog-fill { height: 100%; border-radius: 999px; background: var(--navy); transition: width 0.45s ease; }
  .prog-fill.live {
    background: linear-gradient(90deg, #1F3A5F 0%, #4F7A62 50%, #1F3A5F 100%);
    background-size: 200% 100%;
    animation: prog-slide 1.8s linear infinite;
  }
  .prog-fill.done { background: var(--sage); animation: none; }
  .prog-fill.fail { background: var(--terr); animation: none; }
  @keyframes prog-slide { 0% { background-position: 200% 0; } 100% { background-position: 0 0; } }
  .prog-meta { margin: 6px 0 0 0; font-size: 13px; color: var(--steel); }
  .prog-detail { color: var(--steel); }
  .stop-banner { background:#F5EDE4; border: 3px solid var(--terr); border-radius: 12px;
    padding: 18px 20px; margin: 0 0 16px 0; }
  .stop-banner h3 { color: var(--terr); margin: 0 0 8px 0; font-size: 22px; }
  .countdown-num { font-variant-numeric: tabular-nums; font-weight: 700; font-size: 22px; color: var(--navy); }
  .run-btn-sep { width: 1px; background: #d8d2c8; align-self: stretch; margin: 4px 8px; min-height: 38px; }
  .run-toolbar .btn { font-weight: 600; }
  .page-head { padding-top: 8px; width: 100%; box-sizing: border-box; }
  .page-head-top { display: flex; justify-content: space-between; align-items: flex-start;
    gap: 16px; margin: 0 0 6px 0; flex-wrap: wrap; }
  .page-head-actions {
    display: flex; gap: 10px; flex: 0 0 auto;
    align-items: flex-start; justify-content: flex-end;
  }
  .page-head-action { display: flex; flex-direction: column; align-items: center; gap: 4px; flex: 0 0 auto; }
  .page-head-actions .btn-guide {
    font-size: 18px !important;
    font-weight: 700 !important;
    line-height: 1.2 !important;
    padding: 12px 16px !important;
    border-radius: 8px !important;
    width: auto !important;
    white-space: nowrap;
    text-align: center;
  }
  .page-head-hpc-note { margin: 0; font-size: 12.5px; line-height: 1.3; color: var(--steel);
    text-align: center; font-weight: 400; white-space: nowrap; }
  .page-title { font-size: 2.05rem; margin: 0; line-height: 1.2; flex: 1; font-weight: 400 !important; }
  .aid-ol { margin: 8px 0 0 0; padding-left: 1.3em; }
  .aid-ol li { margin: 0 0 8px 0; line-height: 1.45; }
  .aid-ol code, .aid-scroll code { background: #F7F1E6; padding: 1px 6px; border-radius: 3px; }
  .aid-scroll { max-height: 65vh; overflow-y: auto; padding-right: 10px; }
  .aid-h { color: var(--navy); font-size: 16px; font-weight: 700; margin: 16px 0 6px 0; }
  .aid-h:first-child { margin-top: 0; }
  .aid-table { width: 100%; border-collapse: collapse; margin: 8px 0 12px 0; font-size: 14.5px; }
  .aid-table th { text-align: left; color: var(--navy); background: #EEF4F0; padding: 8px 10px;
    border-bottom: 1px solid #c5d6cc; font-weight: 700; }
  .aid-table td { padding: 8px 10px; border-bottom: 1px solid #e6e0d6; vertical-align: top; line-height: 1.4; }
  .aid-pre { background: #2B2B2B; color: #F5F2EB; font-family: ui-monospace, Menlo, monospace;
    font-size: 12.5px; padding: 10px 12px; border-radius: 4px; white-space: pre-wrap;
    overflow-x: auto; margin: 8px 0 10px 0; }
  .app-card { width: 100%; box-sizing: border-box; overflow: visible !important; }
  .app-card .card-body { padding: 0.65rem 1rem 0.4rem !important; overflow: visible !important; }
  .app-card .card-body h4 { margin-top: 0; margin-bottom: 6px; }
  html, body { height: auto !important; overflow-y: auto !important; }
  .bslib-page-fluid, .bslib-page-fill, .bslib-gap-padding { height: auto !important; overflow: visible !important; }
  body { padding-bottom: 76px; }
  .nav-footer {
    position: fixed; left: 0; right: 0; bottom: 0; z-index: 40;
    display: flex; justify-content: space-between; align-items: center;
    gap: 12px; margin: 0; padding: 8px 28px 12px;
    background: var(--paper); border-top: 1px solid #e6e0d6;
    box-shadow: 0 -6px 14px rgba(31, 58, 95, 0.08);
  }
  .sample-picker-wrap {
    max-height: 140px; overflow-y: auto; border: 1px solid #e6e0d6;
    background: #fff; margin: 6px 0 0 0;
  }
  .sample-picker-wrap .sample-table { margin-bottom: 0; }
  .sample-picker-wrap thead th {
    position: sticky; top: 0; background: #fff; z-index: 1;
    box-shadow: 0 1px 0 #e6e0d6;
  }
  .group-list-wrap {
    max-height: min(52vh, 460px); overflow-y: auto; overflow-x: auto;
    border: 1px solid #e6e0d6; background: #fff; border-radius: 6px;
    padding: 0; margin: 0 0 12px 0;
  }
  .group-list-wrap .shiny-input-container { margin-bottom: 0 !important; }
  .group-table { margin-bottom: 0; min-width: 100%; }
  .group-table thead th {
    position: sticky; top: 0; background: #fff; z-index: 1; color: var(--navy);
    font-size: 14px; box-shadow: 0 1px 0 #e6e0d6; white-space: nowrap;
  }
  .group-table th.extra-group-th { color: var(--sage); }
  .group-table td { vertical-align: middle; }
  .btn-add-meta {
    background: #fff !important; border: 2px solid var(--navy) !important; color: var(--navy) !important;
    font-size: 1.05rem !important; font-weight: 800 !important; padding: 0.45rem 1.1rem !important;
  }
  .btn-add-meta:hover { background: var(--navy) !important; color: #fff !important; }
  .btn-next {
    background: var(--navy) !important; border-color: var(--navy) !important; color: #fff !important;
    font-size: 1.15rem !important; font-weight: 800 !important; letter-spacing: 0.03em;
    padding: 0.5rem 1.6rem !important; min-width: 140px; line-height: 1.2;
    box-shadow: 0 6px 16px rgba(31, 58, 95, 0.32);
  }
  .btn-next:hover { filter: brightness(1.08); }
")

howto_step <- function(title, width = "howto-w1") {
  div(
    class = paste("howto-step", width),
    div(span(class = "howto-ttl", title))
  )
}

howto_arrow <- function() {
  div(class = "howto-arrow", HTML("&#9654;"))
}

ui <- bslib::page_fluid(
  title = "Single-cell analysis autopilot",
  theme = bslib::bs_theme(
    version = 5,
    bg = PAPER,
    fg = INK,
    primary = NAVY,
    font_scale = 1.02
  ),
  shiny::useBusyIndicators(spinners = FALSE, pulse = FALSE, fade = FALSE),
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML(paste0(
      "$(document).on('shiny:disconnected', function(){",
      "if (document.getElementById('scauto-disconnected')) return;",
      "var el=document.createElement('div'); el.id='scauto-disconnected';",
      "el.style.cssText='position:fixed;inset:0;z-index:100000;background:rgba(31,58,95,0.62);",
      "display:flex;align-items:center;justify-content:center;padding:24px;';",
      "el.innerHTML='<div style=\"background:#fff;max-width:440px;padding:22px 24px;",
      "border-radius:12px;font-family:Helvetica,Arial,sans-serif;\">",
      "<h3 style=\"margin:0 0 10px 0;color:#1F3A5F\">Page lost contact</h3>",
      "<p>This browser tab can no longer click buttons (including Force Stop). ",
      "The analysis may still be running in the black Terminal window.</p>",
      "<p><b>Close that black window</b> to stop everything, then double-click Launch.command again.</p>",
      "</div>';",
      "document.body.appendChild(el);",
      "});",
      "(function(){",
      "function fitHowTo(){",
      "  var wrap=document.querySelector('.howto-row-scale');",
      "  var row=document.querySelector('.howto-row');",
      "  if(!wrap||!row) return;",
      "  row.style.transform='none';",
      "  wrap.style.height='auto';",
      "  var avail=wrap.clientWidth;",
      "  var need=row.scrollWidth;",
      "  var s=Math.min(1, avail/Math.max(need,1));",
      "  row.style.transformOrigin='top center';",
      "  row.style.transform='scale('+s+')';",
      "  wrap.style.height=row.getBoundingClientRect().height+'px';",
      "}",
      "var fitHowToTimer=null;",
      "function fitHowToSoon(){ clearTimeout(fitHowToTimer); fitHowToTimer=setTimeout(fitHowTo, 40); }",
      "window.addEventListener('resize', fitHowToSoon);",
      "$(document).on('shiny:connected', fitHowToSoon);",
      "$(fitHowToSoon);",
      "})();",
      "(function(){",
      "function logBox(){ return document.querySelector('.log-box'); }",
      "function nearBottom(el){ return el.scrollHeight - el.scrollTop - el.clientHeight < 48; }",
      "function bindLogBox(box){",
      "  if (!box || box.dataset.logScrollBound) return;",
      "  box.dataset.logScrollBound = '1';",
      "  if (box.dataset.stickBottom == null) box.dataset.stickBottom = '1';",
      "  box.addEventListener('scroll', function(){",
      "    box.dataset.stickBottom = nearBottom(box) ? '1' : '0';",
      "  }, {passive:true});",
      "}",
      "$(document).on('shiny:value', function(e){",
      "  if (e.name !== 'log_text') return;",
      "  var box = logBox();",
      "  bindLogBox(box);",
      "  if (!box) return;",
      "  if (box.dataset.stickBottom !== '0') {",
      "    requestAnimationFrame(function(){ box.scrollTop = box.scrollHeight; });",
      "  }",
      "});",
      "})();"
    )))
  ),
  shinyjs::useShinyjs(),
  div(
    class = "page-head",
    div(
      class = "page-head-top",
      h2("Single-cell analysis autopilot",
         class = "page-title",
         style = paste0("color:", NAVY, ";")),
      div(
        class = "page-head-actions",
        div(
          class = "page-head-action",
          actionButton("data_first_aid_main", "How to Prepare Data", class = "btn-navy btn btn-guide")
        ),
        div(
          class = "page-head-action",
          actionButton("hpc_first_aid_main", "How to Use HPC", class = "btn-hpc btn btn-guide"),
          p(class = "page-head-hpc-note",
            "HPC = High-Performance Computing")
        )
      )
    ),
    div(
      class = "howto-flow",
      div(
        class = "howto-body",
        div(
          class = "howto-lead-row",
          p(class = "howto-lead", "How a run works."),
          actionButton("time_first_aid_main", "How long a run takes", class = "btn-time btn btn-guide")
        ),
        div(
          class = "howto-row-scale",
          div(
            class = "howto-row",
            howto_step("Choose your data folder", "howto-w-choose"),
            howto_arrow(),
            howto_step("Tick your samples", "howto-w-tick"),
            howto_arrow(),
            howto_step("Assign Clinical Groups", "howto-w-groups"),
            howto_arrow(),
            div(
              class = "howto-choice",
              p(class = "howto-choice-a", "Start analysis on your computer"),
              p(class = "howto-or", "OR"),
              p(class = "howto-choice-b", "Start analysis with university HPC"),
            ),
            howto_arrow(),
            howto_step("Download all results", "howto-w-dl")
          )
        )
      )
    )
  ),
  uiOutput("step_bar"),
  uiOutput("hpc_resume_banner"),
  bslib::card(
    class = "app-card",
    bslib::card_body(
      uiOutput("step_body"),
      div(
        class = "nav-footer",
        actionButton("prev_step", "Back", class = "btn btn-outline-secondary"),
        uiOutput("stop_run_footer", inline = TRUE),
        actionButton("next_step", "Next →", class = "btn-next btn")
      )
    )
  ),
  uiOutput("mem_float"),
  downloadButton("dl_hpc_pack", "Download HPC pack (.zip)", class = "btn-hpc",
                 style = "position:absolute;left:-9999px;width:1px;height:1px;overflow:hidden;")
)

server <- function(input, output, session) {
  project <- getOption("scauto.project")
  scauto_root <- getOption("scauto.scauto_root")
  rscript <- getOption("scauto.rscript", Sys.which("Rscript"))

  step <- reactiveVal(1L)
  input_dir <- reactiveVal("")
  discovered <- reactiveVal(NULL)
  meta_df <- reactiveVal(NULL)
  extra_group_cols <- reactiveVal(character())
  outdir <- reactiveVal("")
  proc <- reactiveVal(NULL)
  run_alive <- reactiveVal(FALSE)
  run_exit <- reactiveVal(NA_integer_)
  finish_announced <- reactiveVal(FALSE)
  log_stamp <- reactiveVal(0L)
  selected_sample <- reactiveVal(NULL)
  selected_10x <- reactiveVal(NULL)
  selected_result <- reactiveVal(NULL)
  result_files_hash <- reactiveVal("")
  result_files <- reactiveVal(empty_run_files())
  browse_path <- reactiveVal("")
  browse_selected <- reactiveVal(character())
  pending_keep <- reactiveVal(NULL)
  mem_float_hidden <- reactiveVal(FALSE)
  mem_modal_key <- reactiveVal("")
  mem_sel_key <- reactiveVal("")
  host_ram_cache <- host_ram_gb()
  host_disk_cache <- host_disk_free_gb(if (!is.null(project) && dir.exists(project)) project else ".")
  force_local <- reactiveVal(FALSE)
  run_mode <- reactiveVal("local")
  hpc_settings <- reactiveVal(read_hpc_settings())
  hpc_aid_return <- reactiveVal(FALSE)
  hpc_resume_dismissed <- reactiveVal("")
  hpc_test_result <- reactiveVal(NULL)
  start_pending <- reactiveVal(FALSE)
  start_lock <- reactiveVal(FALSE)
  pending_jump_run <- reactiveVal(FALSE)
  run_stopped <- reactiveVal(FALSE)
  run_started_at <- reactiveVal(NULL)
  run_elapsed_sec <- reactiveVal(NA_real_)
  tissue_choice <- reactiveVal("pbmc")
  advanced_cfg <- reactiveVal(NULL)
  clinical_extra_df <- reactiveVal(NULL)
  env_rows_cache <- reactiveVal({
    if (exists("scauto_run_env_check", mode = "function")) {
      tryCatch(
        scauto_run_env_check(
          project = project,
          log = FALSE,
          incremental = FALSE,
          check_path = NULL
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
  })

  output$step_bar <- renderUI({
    s <- step()
    pills <- lapply(seq_along(step_labels), function(i) {
      cls <- "step-pill"
      if (i == s) cls <- paste(cls, "active")
      else if (i < s) cls <- paste(cls, "done")
      actionLink(
        paste0("goto_step_", i),
        paste0(i, " · ", step_labels[[i]]),
        class = cls
      )
    })
    div(class = "step-bar", pills)
  })

  style_start_button <- function(id, running, glow = FALSE) {
    if (running) {
      updateActionButton(session, id, label = "Analysing…")
      shinyjs::removeClass(id, "btn-run")
      if (isTRUE(glow)) {
        shinyjs::removeClass(id, "btn-running-steady")
        shinyjs::addClass(id, "btn-running")
      } else {
        shinyjs::removeClass(id, "btn-running")
        shinyjs::addClass(id, "btn-running-steady")
      }
      shinyjs::disable(id)
    } else {
      updateActionButton(session, id, label = "Start analysis")
      shinyjs::removeClass(id, "btn-running")
      shinyjs::removeClass(id, "btn-running-steady")
      shinyjs::addClass(id, "btn-run")
      shinyjs::enable(id)
    }
  }

  env_check_in_progress <- function() {
    od <- outdir()
    if (!nzchar(od) || !exists("scauto_read_env_check", mode = "function")) {
      return(TRUE)
    }
    live <- scauto_read_env_check(file.path(od, "logs", "env_check.tsv"))
    if (is.null(live) || !nrow(live) || !"status" %in% names(live)) return(TRUE)
    any(live$status %in% c("checking", "installing", "pending"))
  }

  set_stop_enabled <- function(on) {
    shinyjs::toggleState("stop_run", condition = isTRUE(on))
  }

  output$stop_run_footer <- renderUI({
    if (!isTRUE(run_alive())) return(NULL)
    actionButton("stop_run_always", "Force Stop", class = "btn-stop btn")
  })

  output$stop_run_toolbar <- renderUI({
    if (!isTRUE(run_alive())) return(NULL)
    actionButton("stop_run", "Force Stop", class = "btn-stop btn btn-lg")
  })

  observe({
    s <- step()
    running <- isTRUE(run_alive())
    if (running) invalidateLater(2500, session)
    glow <- running && env_check_in_progress()
    shinyjs::toggleState("prev_step", condition = s > 1L)
    shinyjs::toggle("next_step", condition = s < 4L)
    if (s == 3L) {
      style_start_button("start_from_settings", running, glow)
      shinyjs::toggleState("adv_from_settings", condition = !running)
      shinyjs::toggleState("adv_from_settings_top", condition = !running)
      shinyjs::toggleState("confirm_open_hpc", condition = !running)
    }
    if (s == 4L) {
      style_start_button("start_run", running, glow)
      shinyjs::toggleState("run_open_hpc", condition = !running)
    }
  })

  seed_meta_from_data <- function() {
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return(FALSE)
    keep <- intersect(names(d$dirs), kept_samples())
    if (!length(keep)) keep <- names(d$dirs)
    if (!length(keep)) return(FALSE)
    cur <- meta_df()
    if (is.null(cur)) {
      df <- data.frame(
        sample = keep,
        analysis_name = keep,
        group = "",
        n_barcodes = unname(d$n_barcodes[keep]),
        stringsAsFactors = FALSE
      )
      for (cc in isolate(extra_group_cols())) df[[cc]] <- ""
      meta_df(df)
    } else {
      extra <- setdiff(keep, cur$sample)
      if (length(extra)) {
        add <- cur[rep(NA_integer_, length(extra)), , drop = FALSE]
        rownames(add) <- NULL
        add$sample <- extra
        if ("group" %in% names(add)) add$group <- ""
        if ("analysis_name" %in% names(add)) add$analysis_name <- extra
        if (!"analysis_name" %in% names(cur)) cur$analysis_name <- cur$sample
        if ("n_barcodes" %in% names(add)) add$n_barcodes <- unname(d$n_barcodes[extra])
        cur <- rbind(cur, add)
      }
      cur <- cur[cur$sample %in% keep, , drop = FALSE]
      meta_df(cur)
    }
    pending_keep(keep)
    TRUE
  }

  save_groups_from_inputs <- function() {
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    if (!is.null(m) && nrow(m)) meta_df(m)
    if (!nzchar(outdir())) {
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      outdir(file.path(project, "results", paste0("scauto_", stamp)))
    }
  }

  observeEvent(input$prev_step, {
    step(max(1L, step() - 1L))
  })
  observeEvent(input$next_step, {
    s <- step()
    if (s == 1L) {
      d <- discovered()
      if (is.null(d) || !isTRUE(d$ok)) {
        showNotification("Please choose a folder that contains matrix.mtx (or sample subfolders).", type = "error")
        return()
      }
      keep <- intersect(names(d$dirs), kept_samples())
      if (!length(keep)) {
        showNotification("Select at least one sample.", type = "error")
        return()
      }
      seed_meta_from_data()
      est <- current_mem_check()
      if (!is.null(est) && isTRUE(est$valid) && isTRUE(est$prompt_hpc)) {
        show_hpc_modal(est)
      }
    }
    if (s == 2L) {
      m <- collect_meta()
      if (any(!nzchar(trimws(m$group)))) {
        showNotification("Give every sample a group name (e.g. GroupA / GroupB). If you are not comparing groups, use the same word for all samples.", type = "warning", duration = 6)
      }
      meta_df(m)
      if (!nzchar(outdir())) {
        stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        outdir(file.path(project, "results", paste0("scauto_", stamp)))
      }
    }
    step(min(4L, s + 1L))
  })

  lapply(1:4, function(i) {
    observeEvent(input[[paste0("goto_step_", i)]], {
      target <- as.integer(i)
      if (target >= 2L) {
        d <- discovered()
        if (is.null(d) || !isTRUE(d$ok)) {
          showNotification("Choose a data folder first.", type = "warning")
          return()
        }
        seed_meta_from_data()
      }
      if (target >= 3L) save_groups_from_inputs()
      step(target)
    })
  })

  default_browse_path <- function() {
    cur <- input_dir()
    if (nzchar(cur) && dir.exists(cur)) return(cur)
    for (p in c(
      file.path(project, "data", "trial_4samples"),
      file.path(project, "mtx_files_per_sample")
    )) {
      if (nzchar(p) && dir.exists(p)) return(p)
    }
    if (!is.null(project) && dir.exists(project)) return(project)
    path.expand("~")
  }

  chosen_sample_names <- function(d) {
    pk <- isolate(pending_keep())
    if (!is.null(pk)) return(intersect(pk, names(d$dirs)))
    m <- isolate(meta_df())
    if (!is.null(m) && nrow(m)) {
      hit <- intersect(m$sample, names(d$dirs))
      if (length(hit)) return(hit)
    }
    names(d$dirs)
  }

  kept_samples <- function() {
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return(character())
    nms <- names(d$dirs)
    checked <- vapply(nms, function(s) {
      v <- input[[paste0("keep_", s)]]
      if (is.null(v)) return(s %in% chosen_sample_names(d))
      isTRUE(v)
    }, logical(1))
    nms[checked]
  }

  current_mem_check <- function() {
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return(NULL)
    keep <- kept_samples()
    n_cells <- if (length(keep)) sum(d$n_barcodes[keep], na.rm = TRUE) else 0
    disk <- isolate(host_disk_cache)
    idir <- isolate(input_dir())
    if (nzchar(idir) && dir.exists(idir)) {
      disk <- host_disk_free_gb(idir)
    }
    memory_check(length(keep), n_cells, ram_gb = host_ram_cache, disk_gb = disk)
  }

  pipeline_needs_pkg_install <- function() {
    rows <- isolate(env_rows_cache())
    if (is.null(rows) || !nrow(rows) || !"status" %in% names(rows)) return(FALSE)
    role <- if ("role" %in% names(rows)) rows$role else rep("pipeline", nrow(rows))
    any(role %in% c("pipeline", "recommended") & rows$status %in% c("missing", "fail"))
  }

  start_plan_modal <- function() {
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    est <- current_mem_check()
    n_samp <- if (!is.null(m) && nrow(m)) nrow(m) else 0L
    n_cell <- if (!is.null(est) && isTRUE(est$valid)) est$n_cells else {
      if (!is.null(m) && "n_barcodes" %in% names(m)) sum(m$n_barcodes, na.rm = TRUE) else NA_real_
    }
    tmin <- estimate_pipeline_time_min(n_samp, n_cell)
    ram <- if (!is.null(est) && is.finite(est$est_gb)) est$est_gb else estimate_pipeline_ram_gb(n_samp, n_cell)
    host <- if (!is.null(est) && isTRUE(est$host_known)) est$host_gb else host_ram_cache
    od <- outdir()
    if (!is.null(input$outdir_path) && nzchar(input$outdir_path)) od <- input$outdir_path
    if (!nzchar(od)) od <- file.path(project, "results", paste0("scauto_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    pkg_extra <- pipeline_needs_pkg_install()
    time_txt <- if (all(is.finite(tmin))) {
      sprintf("about %s–%s minutes on this computer", format(tmin[["low"]]), format(tmin[["high"]]))
    } else {
      "depends on sample size (often 20–50 minutes for the 4-sample Baseline trial)"
    }
    ram_txt <- if (is.finite(ram)) sprintf("about %s GB RAM at peak", format(ram, nsmall = 1)) else "RAM will be estimated from cell count"
    host_txt <- if (is.finite(host)) sprintf("This computer reports about %s GB RAM.", format(host, nsmall = 1)) else "Could not read this computer's RAM."
    too_big <- !is.null(est) && identical(est$level, "too_large")
    offer_hpc <- !is.null(est) && isTRUE(est$valid)
    prompt_hpc <- !is.null(est) && isTRUE(est$prompt_hpc)
    box_cls <- if (too_big || (!is.null(est) && identical(est$level, "tight"))) {
      "warn-box"
    } else {
      "ok-box"
    }
    intro <- if (too_big) {
      p("This selection is above what this computer can run safely. Download the HPC pack instead of starting here. There is no automatic start.")
    } else if (!is.null(isolate(advanced_cfg()))) {
      p("You chose Advanced Settings. Review the plan, then click Continue now. There is no automatic start.")
    } else {
      p("If you do nothing, this window closes in ",
        span(id = "start-count-num", class = "countdown-num", "20"),
        " seconds and the run begins.")
    }
    countdown_js <- if (too_big || !is.null(isolate(advanced_cfg()))) {
      NULL
    } else {
      tags$script(HTML(paste0(
        "(function(){",
        "var n=20, done=false;",
        "var el=document.getElementById('start-count-num');",
        "var iv=setInterval(function(){",
        "  if(done){clearInterval(iv);return;}",
        "  n-=1;",
        "  if(el) el.textContent=String(Math.max(n,0));",
        "  if(n<=0){",
        "    done=true; clearInterval(iv);",
        "    var b=document.getElementById('confirm_start_run');",
        "    if(b) b.click();",
        "  }",
        "},1000);",
        "function stop(){done=true;clearInterval(iv);}",
        "var c=document.getElementById('confirm_start_run');",
        "var x=document.getElementById('cancel_start_run');",
        "var h=document.getElementById('plan_use_hpc');",
        "if(c) c.addEventListener('click', stop);",
        "if(x) x.addEventListener('click', stop);",
        "if(h) h.addEventListener('click', stop);",
        "})();"
      )))
    }
    modalDialog(
      title = if (too_big) "Not enough local resources — use HPC" else "Ready to start analysis?",
      tagList(
        intro,
        if (prompt_hpc) {
          p("University HPC is safer for this size. The pack has a PBS script, copy-paste commands, sample list, and a short HOWTO.")
        } else if (offer_hpc) {
          p("This size can run on this computer. University HPC is optional if you prefer the cluster.")
        },
        h5("What will run"),
        tags$ol(
          class = "start-plan",
          tags$li("Check every required component (R, project files, packages), even if they were OK last time"),
          tags$li(if (isTRUE(pkg_extra)) {
            "Download and install any missing R packages from CRAN (needs internet; Seurat can take several extra minutes)"
          } else {
            "Skip package download if everything is already installed"
          }),
          tags$li("QC each sample, then merge, PCA, Harmony (if more than one library), and Leiden clustering"),
          tags$li("Lineage annotation, five-lineage reclustering, CASSIA subtype names (when available), markers, UMAPs, and group tests"),
          tags$li("Pathway enrichment: GO, KEGG, GSEA, GSVA, and Reactome, each saved in its own folder"),
          tags$li("Write REPORT.md, tables, figures, and ", tags$code("objects/atlas_seurat.rds"),
                  " (when the cell count is not too large) into the results folder. Use that RDS if you want more analysis yourself in R.")
        ),
        h5("This computer"),
        div(
          class = box_cls,
          p(style = "margin:0;",
            sprintf("%s sample(s) · about %s cells before QC.",
                    format(n_samp, big.mark = ","),
                    if (is.finite(n_cell)) format(round(n_cell), big.mark = ",") else "—")),
          p(style = "margin:8px 0 0 0;", tags$b("Memory: "), ram_txt, ". ", host_txt),
          p(style = "margin:8px 0 0 0;", tags$b("Time: "), time_txt, "."),
          p(style = "margin:8px 0 0 0;", tags$b("CPU: "),
            "one local R session on this computer. Close Excel with large files, extra browser tabs, and other R windows."),
          p(style = "margin:8px 0 0 0;", tags$b("Save to: "), tags$code(od)),
          if (!is.null(isolate(advanced_cfg()))) {
            p(style = "margin:8px 0 0 0;", HTML(advanced_summary_html(isolate(advanced_cfg()))))
          },
          if (!is.null(est) && nzchar(est$detail)) p(style = "margin:8px 0 0 0;", est$detail)
        ),
        p(class = "hint", style = "margin-top:12px;",
          "You can force-stop after it starts. Partial files may remain. Local Start does not upload data. HPC Submit copies only the ticked samples.")
      ),
      footer = tagList(
        actionButton("cancel_start_run", "Cancel", class = "btn btn-outline-secondary"),
        if (offer_hpc) {
          actionButton("plan_use_hpc", "Use university HPC", class = "btn-hpc btn")
        },
        actionButton(
          "confirm_start_run",
          if (too_big) "Start locally anyway" else "Continue now",
          class = "btn-navy btn"
        ),
        countdown_js
      ),
      easyClose = FALSE,
      size = "l"
    )
  }

  request_start <- function(jump_to_run = FALSE, keep_advanced = FALSE) {
    if (isTRUE(run_alive())) {
      showNotification("A run is already in progress.", type = "warning")
      return(invisible(FALSE))
    }
    if (!isTRUE(keep_advanced)) advanced_cfg(NULL)
    d <- discovered()
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    if (is.null(d) || !isTRUE(d$ok) || is.null(m) || !nrow(m)) {
      showNotification("Missing data or groups. Go back to the first two steps.", type = "error")
      return(invisible(FALSE))
    }
    pending_jump_run(isTRUE(jump_to_run))
    est <- current_mem_check()
    if (!is.null(est) && isTRUE(est$recommend_hpc) && !isTRUE(force_local()) &&
        !isTRUE(input$allow_large)) {
      show_hpc_modal(est)
      return(invisible(FALSE))
    }
    start_pending(TRUE)
    start_lock(FALSE)
    showModal(start_plan_modal())
    invisible(TRUE)
  }

  begin_pipeline <- function() {
    if (isTRUE(isolate(run_alive()))) return(invisible(FALSE))
    if (isTRUE(isolate(start_lock()))) return(invisible(FALSE))
    start_lock(TRUE)
    start_pending(FALSE)
    removeModal()
    if (isTRUE(isolate(pending_jump_run()))) step(4L)
    ok <- do_start()
    start_lock(FALSE)
    invisible(isTRUE(ok))
  }

  collect_advanced_from_input <- function() {
    tissue <- if (!is.null(input$adv_tissue) && nzchar(as.character(input$adv_tissue)[1])) {
      as.character(input$adv_tissue)[1]
    } else {
      isolate(tissue_choice())
    }
    sanitize_advanced_params(list(
      tissue = tissue,
      max_percent_mt = input$adv_max_percent_mt,
      min_ncount = input$adv_min_ncount,
      max_ncount = input$adv_max_ncount,
      min_nfeature = input$adv_min_nfeature,
      max_nfeature = input$adv_max_nfeature,
      n_hvg = input$adv_n_hvg,
      npcs = input$adv_npcs,
      cluster_dims = input$adv_cluster_dims,
      cluster_resolution = input$adv_cluster_resolution,
      n_neighbors = input$adv_n_neighbors,
      harmony_enable = input$adv_harmony_enable,
      harmony_theta = input$adv_harmony_theta,
      recluster_resolution = input$adv_recluster_resolution,
      recluster_min_clusters = input$adv_recluster_min_clusters,
      cassia_enable = input$adv_cassia_enable,
      deg_enable = input$adv_deg_enable,
      deg_padj = input$adv_deg_padj,
      deg_lfc = input$adv_deg_lfc,
      save_rds = input$adv_save_rds,
      pathway_go = input$adv_pathway_go,
      pathway_kegg = input$adv_pathway_kegg,
      pathway_gsea = input$adv_pathway_gsea,
      pathway_gsva = input$adv_pathway_gsva,
      pathway_reactome = input$adv_pathway_reactome,
      cytotrace2 = input$adv_cytotrace2,
      monocle3 = input$adv_monocle3,
      traj_lineages = input$adv_traj_lineages,
      clinical_numeric_enable = isTRUE(input$adv_clin_enable),
      clinical_numeric_cols = {
        v <- input$adv_clin_cols
        extra <- isolate(clinical_extra_df())
        if (is.null(v) || !length(v)) {
          if (!is.null(extra)) scauto_numeric_meta_cols(extra, min_n = 3L) else character()
        } else {
          as.character(v)
        }
      }
    ))
  }

  show_advanced_modal <- function() {
    p <- sanitize_advanced_params(isolate(advanced_cfg()))
    showModal(modalDialog(
      title = "Advanced Settings",
      size = "l",
      easyClose = TRUE,
      tagList(
        p("Start analysis keeps the lab defaults. Change only the knobs you need. These settings are written as an overlay YAML and passed with --config."),
        h5("Tissue and quality control"),
        radioButtons(
          "adv_tissue", "Tissue (sets the mitochondrial QC cutoff)",
          choices = scauto_tissue_choices(),
          selected = {
            t <- isolate(tissue_choice())
            if (!nzchar(t)) t <- p$tissue
            t
          }
        ),
        p(class = "hint", "Liver cells often have high mitochondrial RNA. Choosing Liver sets max mitochondrial % to 50. PBMC / blood stays at 10%. You can still type a different number below."),
        fluidRow(
          column(3, numericInput("adv_max_percent_mt", "Max mitochondrial %", value = {
            t <- isolate(tissue_choice())
            if (identical(t, "liver") && as.numeric(p$max_percent_mt) <= 10) 50 else p$max_percent_mt
          }, min = 0, max = 100, step = 1)),
          column(3, numericInput("adv_min_ncount", "Min UMIs (nCount)", value = p$min_ncount, min = 0, step = 100)),
          column(3, numericInput("adv_max_ncount", "Max UMIs (nCount)", value = p$max_ncount, min = 1, step = 1000)),
          column(3, numericInput("adv_max_nfeature", "Max genes (nFeature)", value = p$max_nfeature, min = 100, step = 100))
        ),
        fluidRow(
          column(4, numericInput("adv_min_nfeature", "Min genes (nFeature; 0 = no floor)", value = p$min_nfeature, min = 0, step = 50))
        ),
        h5("HVG, PCA, clustering"),
        fluidRow(
          column(3, numericInput("adv_n_hvg", "Highly variable genes", value = p$n_hvg, min = 200, max = 8000, step = 100)),
          column(3, numericInput("adv_npcs", "PCA components", value = p$npcs, min = 10, max = 100, step = 5)),
          column(3, numericInput("adv_cluster_dims", "Leiden PCs", value = p$cluster_dims, min = 5, max = 100, step = 1)),
          column(3, numericInput("adv_n_neighbors", "Neighbors", value = p$n_neighbors, min = 5, max = 80, step = 1))
        ),
        fluidRow(
          column(4, numericInput("adv_cluster_resolution", "Leiden resolution", value = p$cluster_resolution, min = 0.05, max = 2, step = 0.1))
        ),
        h5("Harmony (batch correction by sample)"),
        fluidRow(
          column(6, selectInput("adv_harmony_enable", "Harmony", choices = c(
            "Auto (on if more than one library)" = "auto",
            "Always on" = "yes",
            "Off" = "no"
          ), selected = p$harmony_enable)),
          column(6, numericInput("adv_harmony_theta", "Harmony theta", value = p$harmony_theta, min = 0, max = 10, step = 0.5))
        ),
        h5("Lineage reclustering"),
        fluidRow(
          column(6, numericInput("adv_recluster_resolution", "Starting resolution", value = p$recluster_resolution, min = 0.05, max = 2, step = 0.1)),
          column(6, numericInput("adv_recluster_min_clusters", "Min clusters (floor)", value = p$recluster_min_clusters, min = 2, max = 20, step = 1))
        ),
        selectInput("adv_cassia_enable", "Subtype names (CASSIA)", choices = c(
          "Auto (CASSIA if CLI ready, else marker panels)" = "auto",
          "Always try CASSIA" = "yes",
          "Marker panels only" = "no"
        ), selected = p$cassia_enable),
        p(class = "hint", "CASSIA relabels secondary clusters from FindAllMarkers. UMAPs, module heatmaps, proportions, and DEGs use those names. Needs the cassia CLI plus Claude Code or an API key. HPC compute nodes fall back to panels."),
        h5("Clinical DEGs and RDS"),
        fluidRow(
          column(4, checkboxInput("adv_deg_enable", "Run Wilcoxon + MAST DEGs", value = isTRUE(p$deg_enable))),
          column(4, numericInput("adv_deg_padj", "DEG padj cutoff", value = p$deg_padj, min = 1e-6, max = 0.2, step = 0.01)),
          column(4, numericInput("adv_deg_lfc", "DEG |log2FC| cutoff", value = p$deg_lfc, min = 0, max = 2, step = 0.05))
        ),
        checkboxInput(
          "adv_save_rds",
          "Save atlas Seurat RDS (no cell-number limit; off = do not write RDS)",
          value = isTRUE(p$save_rds)
        ),
        p(class = "hint",
          "Large objects are slimmed before writing (counts, normalised data, and UMAP/PCA/Harmony are kept; scale.data and neighbor graphs are dropped). The file is written to a temporary name first so a crash cannot leave a half-written RDS. The write is skipped only if the disk does not have enough free space."),
        h5("Pathway enrichment"),
        p(class = "hint", "On by default for Start analysis. Each database is saved in its own folder. Untick a database to skip it. HPC compute nodes skip databases whose packages were not pre-installed."),
        fluidRow(
          column(2, checkboxInput("adv_pathway_go", "GO", value = isTRUE(p$pathway_go))),
          column(2, checkboxInput("adv_pathway_kegg", "KEGG", value = isTRUE(p$pathway_kegg))),
          column(2, checkboxInput("adv_pathway_gsea", "GSEA", value = isTRUE(p$pathway_gsea))),
          column(3, checkboxInput("adv_pathway_gsva", "GSVA", value = isTRUE(p$pathway_gsva))),
          column(3, checkboxInput("adv_pathway_reactome", "Reactome", value = isTRUE(p$pathway_reactome)))
        ),
        h5("Trajectory (lineages of interest)"),
        p(class = "hint", "Off unless you tick a method and at least one lineage. CytoTRACE2 needs the CytoTRACE2 R package; Monocle3 needs monocle3. Install those on this computer (or the HPC login node) before running. CytoTRACE2 is memory-heavy and subsamples above 20,000 cells."),
        fluidRow(
          column(6, checkboxInput("adv_cytotrace2", "CytoTRACE2 potency scores", value = isTRUE(p$cytotrace2))),
          column(6, checkboxInput("adv_monocle3", "Monocle3 trajectory / pseudotime", value = isTRUE(p$monocle3)))
        ),
        checkboxGroupInput(
          "adv_traj_lineages",
          "Run trajectory on",
          choices = scauto_superlineage_choices(),
          selected = p$traj_lineages,
          inline = TRUE
        ),
        h5("Sample clinical numbers"),
        p(class = "hint",
          "Optional CSV with a ", tags$b("sample"),
          " column (library IDs) plus optional numeric columns such as Age or LabValue1. Each subtype’s proportion within the lineage is correlated (Spearman) with those numbers. Need at least 4 samples with values. Columns whose names look like counts or titers also get a log10 version."),
        div(
          class = "d-flex gap-2 flex-wrap align-items-end",
          downloadButton("dl_clin_template", "Download template (.csv)", class = "btn btn-outline-secondary")
        ),
        fileInput(
          "adv_clin_upload",
          "Upload clinical numbers CSV",
          accept = c(".csv", "text/csv"),
          placeholder = "sample, Age, LabValue1, …"
        ),
        checkboxInput(
          "adv_clin_enable",
          "Correlate subtype proportions with these clinical numbers",
          value = isTRUE(p$clinical_numeric_enable) || !is.null(isolate(clinical_extra_df()))
        ),
        uiOutput("adv_clin_preview"),
        p(class = "hint", "Lineage-specific reclustering floors in default.yaml stay in place unless you raise the global min-cluster floor above them.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("adv_run_hpc", "Use university HPC with these settings", class = "btn-hpc btn"),
        actionButton("adv_run_local", "Run locally with these settings", class = "btn btn-outline-secondary"),
        actionButton("adv_confirm", "Confirm settings", class = "btn-navy btn btn-lg")
      )
    ))
  }

  memory_float_ui <- function(est) {
    if (is.null(est) || !isTRUE(est$valid)) return(NULL)
    cls <- paste("mem-float", paste0("mem-", est$level))
    ttl <- switch(
      as.character(est$level),
      ok = "This computer looks OK",
      tight = "Tight on this computer",
      too_large = "Not enough RAM — use HPC",
      "Resource check"
    )
    n_s <- format(est$n_samples, big.mark = ",")
    n_c <- format(round(est$n_cells), big.mark = ",")
    est_txt <- if (is.finite(est$est_gb)) sprintf("%s GB", format(est$est_gb, nsmall = 1)) else "—"
    host_txt <- if (isTRUE(est$host_known)) {
      sprintf("%s GB", format(est$host_gb, nsmall = 1))
    } else {
      sprintf("%s GB (assumed)", MEM_ASSUME_LAPTOP_GB)
    }
    summary <- sprintf("%s samples · %s cells · %s / %s RAM", n_s, n_c, est_txt, host_txt)
    disk_line <- NULL
    if (is.finite(est$disk_gb) && is.finite(est$disk_need_gb) && est$disk_gb < est$disk_need_gb) {
      disk_line <- p(
        sprintf("Disk: %s GB free, need ~%s GB",
                format(est$disk_gb, nsmall = 1), format(est$disk_need_gb, nsmall = 1))
      )
    }
    hpc_btn <- if (est$level %in% c("tight", "too_large")) {
      actionButton("mem_open_hpc", "Use university HPC", class = "btn-hpc btn btn-sm")
    } else {
      NULL
    }
    div(
      class = cls,
      div(
        class = "mem-float-head",
        h5(ttl),
        actionButton("mem_float_close", "Close", class = "btn btn-sm mem-close")
      ),
      p(summary),
      disk_line,
      hpc_btn
    )
  }

  data_aid_contact_note <- function() {
    os <- paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
    rver <- paste(R.version$major, R.version$minor, sep = ".")
    paste(
      "[scAuto help request]",
      paste0("Computer: ", os),
      paste0("R version: ", rver),
      "Data from: 10x Genomics / MGI DNBSEQ (10x library) / MGI DNBelab / other: ________",
      "Files I have: Cell Ranger triplet / FASTQ (.fq.gz) / .h5 / Seurat RDS / h5ad / Excel / not sure",
      "I have read How to Prepare Data, and I already tried: ________",
      "Where I am stuck / page text / error message: ________",
      sep = "\n"
    )
  }

  show_data_contact_dev <- function() {
    note <- data_aid_contact_note()
    showModal(modalDialog(
      title = "Contact the developer",
      div(
        class = "aid-scroll",
        p(style = "margin-top:0;",
          "If How to Prepare Data does not cover your files, send the note below to ",
          tags$b("the colleague who gave you this helper / the developer"),
          " (email is fine). Do not send HPC passwords, ", tags$code("hpc/config.env"),
          ", or anyone else’s account."),
        p("Fill in: sequencing company (10x Genomics or MGI), file type, and the error text."),
        tags$textarea(
          id = "data_aid_contact_text",
          style = "width:100%;min-height:220px;font-family:ui-monospace,Menlo,monospace;font-size:13px;padding:10px;border:1px solid #d8d2c8;border-radius:6px;background:#F7F1E6;",
          note
        )
      ),
      footer = tagList(
        actionButton("data_aid_back", "Back to How to Prepare Data", class = "btn btn-outline-secondary"),
        actionButton("data_aid_copy_note", "Copy this note", class = "btn-navy btn"),
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  }

  show_data_first_aid <- function() {
    showModal(modalDialog(
      title = "How to Prepare Data",
      div(
        class = "aid-scroll",
        p(style = "margin-top:0;",
          "This helper is built for data from the ",
          tags$b("10x Genomics platform"),
          " (Chromium single-cell RNA-seq, processed with ", tags$b("Cell Ranger"),
          "). That is the kit from 10x Genomics, Inc. — not Parse, BD Rhapsody, Smart-seq, or a matrix you already analysed in R/Python. ",
          "If the run was on an ", tags$b("MGI Tech (MGI / DNBSEQ)"), " sequencer, read section 3 — MGI is a different company. ",
          "If you only want to try the software, click ",
          tags$b("Use trial samples"), " — you do not need your own data for that."),
        h5(class = "aid-h", "1. What 10x Genomics output looks like"),
        p("Each sample is one folder. The folder name is the sample ID (for example ", tags$code("sampleA"), "). Inside it you need exactly these three files (names must match; ", tags$code(".gz"), " is fine):"),
        tags$ul(
          class = "aid-ol",
          tags$li(tags$code("matrix.mtx"), " or ", tags$code("matrix.mtx.gz"), " — counts"),
          tags$li(tags$code("barcodes.tsv"), " or ", tags$code("barcodes.tsv.gz"), " — cell barcodes, one per line"),
          tags$li(tags$code("features.tsv"), " or ", tags$code("genes.tsv"), " (or ", tags$code(".gz"), ") — genes")
        ),
        p("Several samples: put those folders next to each other, then choose the ", tags$b("parent"), " folder:"),
        tags$pre(class = "aid-pre", paste(
          "my_experiment/",
          "  sampleA/",
          "    matrix.mtx",
          "    barcodes.tsv",
          "    features.tsv",
          "  sampleC/",
          "    matrix.mtx",
          "    barcodes.tsv",
          "    features.tsv",
          sep = "\n"
        )),
        p("One sample only: you may choose that sample folder itself. Cell Ranger’s ",
          tags$code("filtered_feature_bc_matrix"), " (or ", tags$code("outs/filtered_feature_bc_matrix"),
          ") is detected automatically — you can choose the Cell Ranger sample folder."),
        h5(class = "aid-h", "2. If the data came from the 10x Genomics platform"),
        p("Typical path after Chromium + Cell Ranger: ",
          tags$code("…/<sample>/outs/filtered_feature_bc_matrix/"),
          " with the three files above. Space Ranger / Visium spatial data is not this helper."),
        tags$ul(
          class = "aid-ol",
          tags$li("Prefer ", tags$b("filtered"), " matrices, not raw (raw includes empty droplets)."),
          tags$li("If you have both ", tags$code(".h5"), " and the three mtx files, point the helper at the folder that contains ", tags$code("matrix.mtx"), "."),
          tags$li("If you only have ", tags$code("filtered_feature_bc_matrix.h5"), ", export the triplet (ask a colleague who uses R) with DropletUtils, then use the new folder:"),
          tags$pre(class = "aid-pre", paste(
            "library(DropletUtils)",
            "x <- read10xCounts(\"filtered_feature_bc_matrix.h5\")",
            "write10xCounts(\"sampleA\", x)",
            sep = "\n"
          ))
        ),
        h5(class = "aid-h", "3. If the data was sequenced on MGI (MGI Tech / DNBSEQ)"),
        p("MGI (MGI Tech) is ", tags$b("not"), " 10x Genomics. Two common situations look similar in a delivery folder but are prepared differently:"),
        p(tags$b("A. 10x Genomics Chromium library, sequenced on an MGI machine"),
          " (DNBSEQ-G400, T7, T10, MGISEQ, …). The kit is still 10x; only the sequencer changed."),
        tags$ul(
          class = "aid-ol",
          tags$li("If the core already ran ", tags$b("Cell Ranger"), " and gave you ",
                  tags$code("filtered_feature_bc_matrix"), " (the three files in section 1), treat it as 10x Genomics data and choose that folder. You are done."),
          tags$li("If you only have FASTQ named like ", tags$code("*.fq.gz"), " or ",
                  tags$code("*_1.fq.gz"), " / ", tags$code("*_2.fq.gz"),
                  " (MGI naming, not Illumina ", tags$code("_R1_001.fastq.gz"),
                  "): this helper cannot run Cell Ranger. Ask the core to convert MGI FASTQ to Illumina-style FASTQ and run Cell Ranger, then use the mtx folder."),
          tags$li("Do not point this helper at the MGI FASTQ zip.")
        ),
        p(tags$b("B. MGI’s own single-cell kit"),
          " (DNBelab C4 / C series / C-TaiM, or a report that says DNBelab). This is ",
          tags$b("not"), " Chromium chemistry. Cell Ranger for 10x will not barcode it correctly."),
        tags$ul(
          class = "aid-ol",
          tags$li("Use MGI’s DNBelab C series analysis software (or the pipeline the sequencing core already ran) until you have a gene-by-cell count matrix."),
          tags$li("If that software already wrote ", tags$code("matrix.mtx"), " + ",
                  tags$code("barcodes.tsv"), " + ", tags$code("features.tsv"),
                  ", choose that folder here — the helper only needs those three names."),
          tags$li("If you received RDS, h5ad, or a gene × cell table instead, convert as in section 4.")
        ),
        p(tags$b("C. MGI Stereo-seq"), " (spatial) is not this helper."),
        p("Not sure which case you have? Open the delivery PDF / README: “Chromium / Cell Ranger” → A; “DNBelab / C4” → B. If still unclear, use the contact button at the bottom of this window."),
        h5(class = "aid-h", "4. If the data is not from the 10x Genomics platform"),
        p("Another company’s kit (Parse Biosciences, BD Rhapsody, Fluidigm, SMART-seq / plate-based), or a file you already analysed (Seurat RDS, scanpy h5ad, Excel/CSV), will not open as-is. Convert ", tags$b("each sample"), " into the three-file Cell Ranger folder above, using ", tags$b("human gene symbols"), " where possible (this pipeline is for human PBMC). Ask a colleague who uses R or Python if the snippets below are unfamiliar."),
        p(tags$b("From a Seurat object"), " — split by sample first if several libraries are in one RDS:"),
        tags$pre(class = "aid-pre", paste(
          "library(Seurat)",
          "library(DropletUtils)",
          "obj <- readRDS(\"my_object.rds\")",
          "# genes as rows, cells as columns (Seurat v5; v4: slot = \"counts\")",
          "write10xCounts(\"sampleA\", GetAssayData(obj, layer = \"counts\"))",
          sep = "\n"
        )),
        p(tags$b("From AnnData / scanpy"), " (", tags$code(".h5ad"), ") — 10x mtx is genes × cells, so the matrix is transposed:"),
        tags$pre(class = "aid-pre", paste(
          "import os, pandas as pd, scanpy as sc",
          "from scipy.io import mmwrite",
          "adata = sc.read_h5ad(\"sample.h5ad\")",
          "os.makedirs(\"sampleA\", exist_ok=True)",
          "mmwrite(\"sampleA/matrix.mtx\", adata.X.T)",
          "pd.Series(adata.obs_names).to_csv(\"sampleA/barcodes.tsv\", index=False, header=False)",
          "g = adata.var_names",
          "pd.DataFrame({0: g, 1: g, 2: \"Gene Expression\"}).to_csv(",
          "    \"sampleA/features.tsv\", sep=\"\\t\", index=False, header=False)",
          sep = "\n"
        )),
        p(tags$b("From a gene × cell CSV (Excel: File → Save As → CSV)."), " Gene names in the first column; cell barcodes in the header. Excel or a Word table is not enough on its own:"),
        tags$pre(class = "aid-pre", paste(
          "library(Matrix)",
          "library(DropletUtils)",
          "m <- read.csv(\"counts.csv\", row.names = 1, check.names = FALSE)",
          "write10xCounts(\"sampleA\", as(as.matrix(m), \"dgCMatrix\"))",
          sep = "\n"
        )),
        p(tags$b("From FASTQ on the 10x Genomics platform:"), " run ", tags$b("Cell Ranger"), " first (count / multi). This helper does not align reads. Do not point it at the FASTQ zip from the core."),
        p(tags$b("From another company’s platform"), " (Parse Biosciences, BD Rhapsody, SMART-seq, MGI DNBelab, …): finish that vendor’s pipeline until you have a gene-by-cell count matrix, then write the Cell Ranger three-file folder as above. Name each folder with your library ID."),
        h5(class = "aid-h", "5. After the folders are ready"),
        tags$ul(
          class = "aid-ol",
          tags$li("Click ", tags$b("Choose folder…"), " and open the parent folder (or one sample folder)."),
          tags$li("Tick the samples to analyse. RAM and disk are checked automatically."),
          tags$li("On Groups, type the same spelling for samples in the same clinical group (for example GroupA / GroupB)."),
          tags$li("If this computer is too small, use ", tags$b("Use university HPC"), " — see How to Use HPC.")
        ),
        h5(class = "aid-h", "6. What will not work"),
        tags$ul(
          class = "aid-ol",
          tags$li("A zip of FASTQ files (Illumina or MGI ", tags$code(".fq.gz"), "), or a GEO Series matrix, dropped in without conversion."),
          tags$li("DNBelab / MGI raw output before their analysis software has written a gene-by-cell matrix."),
          tags$li("A single ", tags$code(".rds"), " / ", tags$code(".h5ad"), " / ", tags$code(".h5seurat"), " of the whole study."),
          tags$li("Pictures of the matrix, or a Word table."),
          tags$li("Mouse gene names if you expect the human PBMC lineage labels in this helper."),
          tags$li("MGI Stereo-seq spatial data.")
        )
      ),
      footer = tagList(
        actionButton(
          "data_aid_contact_dev",
          "This does not solve my problem — contact the developer",
          class = "btn-contact btn"
        ),
        modalButton("Close")
      ),
      easyClose = TRUE,
      size = "l"
    ))
  }

  show_time_first_aid <- function() {
    showModal(modalDialog(
      title = "How long a run takes",
      div(
        class = "aid-scroll",
        p(style = "margin-top:0;",
          "Times below are ", tags$b("compute time"), " after the job starts — not waiting in the HPC queue, and not the first-time package install on a new computer (that can add 10–40 minutes). This pipeline is mostly single-threaded R, so university HPC is more about RAM and not crashing than about being many times faster than a strong workstation."),
        h5(class = "aid-h", "1. Steps that usually take the longest"),
        tags$ul(
          class = "aid-ol",
          tags$li(tags$b("Clinical DEGs (Wilcoxon + MAST)."),
                  " Often the largest share. On the 4-sample trial this was about 40 minutes of a 1-hour HPC run. MAST is slower than Wilcoxon. It repeats for every lineage and every subtype."),
          tags$li(tags$b("Pathway enrichment"),
                  " (GO, KEGG, GSEA, GSVA, Reactome) once per lineage, after the main DEGs."),
          tags$li(tags$b("Harmony, atlas clustering, FindAllMarkers,"),
                  " then five-lineage reclustering and subtype markers. These scale with cell number."),
          tags$li(tags$b("Optional trajectory"),
                  " (CytoTRACE2 / Monocle3) only if you turn it on in Advanced Settings — extra time per ticked lineage."),
          tags$li(tags$b("CASSIA"),
                  " subtype naming is skipped on HPC compute nodes (no internet). On a laptop it can add time if the CLI is available.")
        ),
        h5(class = "aid-h", "2. What makes a run longer"),
        tags$ul(
          class = "aid-ol",
          tags$li(tags$b("Cell number"), " (barcodes before QC) is the main driver — Harmony, markers, MAST, and pathways all grow with cells."),
          tags$li(tags$b("Sample number"), " mainly adds ingest and Harmony. It matters less than cells: 8 small libraries can be faster than 4 very large ones."),
          tags$li(tags$b("Extra clinical group columns"), " (Groups page). The first Clinical group column is the main contrast. Each extra column that has ",
                  tags$b("two or more labels"), " repeats the DEG round (~40 minutes on the 4-sample trial). Clustering and pathways are not repeated. If every sample has the same word in that extra column, DEG is skipped and the extra time is minutes, not tens of minutes."),
          tags$li("Three or more labels in one column still run ", tags$b("one"), " DEG pair, not every pair."),
          tags$li("A busy HPC queue can add minutes to hours ", tags$b("before"), " compute starts. That wait is normal.")
        ),
        h5(class = "aid-h", "3. Typical compute times (lab defaults, trajectory off)"),
        tags$table(
          class = "aid-table",
          tags$thead(tags$tr(
            tags$th("What you run"),
            tags$th("Cells (before QC)"),
            tags$th("Clinical groupings"),
            tags$th("Typical compute")
          )),
          tags$tbody(
            tags$tr(
              tags$td("4 trial samples (2 GroupA + 2 GroupB, Baseline), university HPC or a strong workstation"),
              tags$td("~24,000"),
              tags$td("1 (GroupA / GroupB)"),
              tags$td(tags$b("~1 hour"))
            ),
            tags$tr(
              tags$td("Same 4 trial samples, plus a second grouping column with two labels"),
              tags$td("~24,000"),
              tags$td("2"),
              tags$td(tags$b("~1 hour 40 minutes"))
            ),
            tags$tr(
              tags$td("4 trial samples on a 16 GB laptop"),
              tags$td("~24,000"),
              tags$td("1"),
              tags$td("Often 1–2 hours, or it may not fit in RAM — use HPC")
            ),
            tags$tr(
              tags$td("A larger study (more samples / more cells), university HPC"),
              tags$td("Several-fold more cells"),
              tags$td("1 or more"),
              tags$td("Usually several hours. Time tracks cell count more than sample count; add ~the DEG block again for each extra 2-group column")
            )
          )
        ),
        p("Keep this computer ", tags$b("on"), " if you submitted to HPC, so results can copy back. Closing the helper does not stop a cluster job already in the queue.")
      ),
      footer = modalButton("Close"),
      easyClose = TRUE,
      size = "l"
    ))
  }

  show_hpc_first_aid <- function() {
    from_hpc <- isTRUE(isolate(hpc_aid_return()))
    footer <- tagList(
      if (from_hpc) actionButton("hpc_aid_back", "Back to HPC submit", class = "btn-hpc btn"),
      modalButton("Close")
    )
    showModal(modalDialog(
      title = "How to Use HPC",
      div(
        class = "aid-scroll",
        p(style = "margin-top:0;",
          tags$b("HPC"), " means ", tags$b("High-Performance Computing"),
          ": a shared university cluster, not this laptop. Enter your cluster hostname in the HPC window. You upload the ticked samples; the cluster runs the analysis; results copy back here. Use ",
          tags$b("your own"), " account. Do not share logins, passwords, SSH keys, or ", tags$code("hpc/config.env"), "."),
        h5(class = "aid-h", "1. Get an account (once)"),
        tags$ul(
          class = "aid-ol",
          tags$li("Ask university IT department. The university HPC office issues a ", tags$b("username"), " (usually not your email) and a first password. You cannot invent a login."),
          tags$li("Off-campus: connect the university ", tags$b("VPN"), " before SSH.")
        ),
        h5(class = "aid-h", "2. SSH key (once, in Terminal — not in this helper)"),
        p("This helper ", tags$b("cannot type a password"), ". After the key is installed, ", tags$b("Test SSH"), " and ", tags$b("Submit to HPC"), " work without one. Mac: Terminal. Windows: OpenSSH or Git Bash."),
        tags$pre(class = "aid-pre", paste(
          "ssh-keygen -t ed25519",
          "ssh YOUR_USER@hpc.example.edu",
          "ssh-copy-id YOUR_USER@hpc.example.edu",
          sep = "\n"
        )),
        p("First ", tags$code("ssh"), ": type ", tags$code("yes"), ", then your HPC password. The next ", tags$code("ssh"), " should log in without a password — then type ", tags$code("exit"), ". Replace ", tags$code("YOUR_USER"), ". The host field may be an SSH alias (for example ", tags$code("hpc"), ")."),
        p("Windows without ", tags$code("ssh-copy-id"), ", in PowerShell:"),
        tags$pre(
          class = "aid-pre",
          "type $env:USERPROFILE\\.ssh\\id_ed25519.pub | ssh YOUR_USER@hpc.example.edu \"mkdir -p .ssh && cat >> .ssh/authorized_keys\""
        ),
        h5(class = "aid-h", "3. One-click in this helper"),
        tags$ul(
          class = "aid-ol",
          tags$li("Tick samples on the Data page."),
          tags$li("Click ", tags$b("Use university HPC"), "."),
          tags$li("Fill host and username. Leave the cluster folder blank the first time."),
          tags$li(tags$b("Test SSH connection"), ". If it fails, repeat the SSH key steps above."),
          tags$li(tags$b("Submit to HPC"), " copies only the ticked 10x folders, runs on queue ",
                  tags$code("medium"), " , and copies ", tags$code("results/"), " back."),
          tags$li("Keep this computer ", tags$b("on"), " for copy-back. The cluster job keeps running if you close the laptop. Reopen the helper → ", tags$b("Continue last HPC job"), ". ", tags$b("Close"), " on that banner only hides the notice.")
        ),
        h5(class = "aid-h", "4. If one-click cannot run"),
        p("In the HPC window: ", tags$b("Download scripts only (.zip)"), ". Unzip and follow ", tags$code("README.md"), " / ", tags$code("COMMANDS.txt"),
          ": copy this helper and the ticked 10x folders to ", tags$b("your"), " cluster path; edit ", tags$code("PROJECT_DIR"), " if needed. On the ", tags$b("login node"), " (compute nodes have no internet), once:"),
        tags$pre(class = "aid-pre", paste(
          "Rscript tools/scauto/check_install.R --install",
          "chmod +x hpc/submit.sh",
          "bash hpc/submit.sh",
          "qstat -u \"$USER\"",
          sep = "\n"
        )),
        h5(class = "aid-h", "5. If something fails"),
        tags$ul(
          class = "aid-ol",
          tags$li(tags$b("Password asked again:"), " SSH key not installed — repeat the SSH key steps above. Check the username."),
          tags$li(tags$b("Timed out / host unknown:"), " VPN, or the host name is wrong."),
          tags$li(tags$b("Windows has no ssh / rsync:"), " OpenSSH and Git for Windows (Git Bash includes rsync)."),
          tags$li(tags$b("Job sits in queue:"), " normal on a shared cluster. Wait."),
          tags$li(tags$b("Job failed:"), " send ", tags$code("logs/pipeline.log"), " to whoever maintains this helper. Do not send passwords."),
          tags$li(tags$b("Force Stop"), " ends a local run, or ", tags$code("qdel"), " if this helper submitted the job. It does not delete results already copied here.")
        )
      ),
      footer = footer,
      easyClose = TRUE,
      size = "l"
    ))
  }

  show_hpc_modal <- function(est) {
    if (is.null(est) || !isTRUE(est$valid)) {
      showNotification("Tick at least one sample first.", type = "error")
      return(invisible(FALSE))
    }
    hpc_test_result(NULL)
    s <- isolate(hpc_settings())
    box_cls <- if (est$level %in% c("tight", "too_large")) "warn-box" else "ok-box"
    hpc_title <- if (identical(est$level, "too_large")) {
      est$title
    } else if (identical(est$level, "tight")) {
      est$title
    } else {
      "Run on university HPC"
    }
    hpc_intro <- if (est$level %in% c("tight", "too_large")) {
      p(style = "margin-top:12px;", est$detail)
    } else {
      p(style = "margin-top:12px;",
        "This size can run locally. Submit to the university cluster if you prefer HPC, or stay on this page and use Start analysis.")
    }
    footer <- tagList(
      actionButton("hpc_first_aid_modal", "How to Use HPC", class = "btn btn-outline-secondary"),
      actionButton("hpc_submit", "Submit to HPC (upload, run, pull results)", class = "btn-hpc btn"),
      actionButton("hpc_modal_dl", "Download scripts only (.zip)", class = "btn btn-outline-secondary"),
      modalButton("Stay on this page")
    )
    if (isTRUE(est$recommend_hpc)) {
      footer <- tagList(
        actionButton("hpc_first_aid_modal", "How to Use HPC", class = "btn btn-outline-secondary"),
        actionButton("hpc_submit", "Submit to HPC (upload, run, pull results)", class = "btn-hpc btn"),
        actionButton("hpc_modal_dl", "Download scripts only (.zip)", class = "btn btn-outline-secondary"),
        actionButton("force_hpc_local", "Run on this computer anyway (not recommended)",
                     class = "btn btn-outline-secondary"),
        modalButton("Untick samples")
      )
    }
    showModal(modalDialog(
      title = hpc_title,
      tagList(
        div(class = box_cls, p(style = "margin:0;", est$headline)),
        hpc_intro,
        p("You only need an HPC account and one-time SSH key login. This page will copy the ticked 10x folders, submit the job, wait, and copy results back here."),
        if (is.finite(est$pbs_mem_gb)) {
          p(class = "hint", sprintf("Suggested job: 8 CPUs, %s GB RAM, queue the queue name your HPC office gives you.", est$pbs_mem_gb))
        },
        h5("HPC account"),
        p(class = "hint",
          "First time: in Terminal run ", tags$code("ssh USER@HOST"),
          " once, accept the fingerprint, then ", tags$code("ssh-copy-id USER@HOST"),
          ". The helper cannot type a password."),
        textInput("hpc_host", "HPC host (SSH name or alias)",
                  value = s$host,
                  placeholder = "your.cluster.edu  or  an SSH alias"),
        textInput("hpc_user", "HPC username",
                  value = s$user, placeholder = "your university HPC login"),
        textInput("hpc_remote", "Folder on the cluster",
                  value = s$remote_project,
                  placeholder = "Leave blank, click Test, then use the suggestion"),
        div(
          class = "d-flex gap-2 flex-wrap mb-2",
          actionButton("hpc_test", "Test SSH connection", class = "btn-navy btn btn-sm")
        ),
        uiOutput("hpc_test_box")
      ),
      footer = footer,
      easyClose = TRUE,
      size = "l"
    ))
    invisible(TRUE)
  }

  maybe_memory_modal <- function() {
    est <- current_mem_check()
    if (is.null(est) || !isTRUE(est$valid)) return()
    if (!isTRUE(est$prompt_hpc)) return()
    key <- paste(est$n_samples, round(est$n_cells), est$level, sep = "|")
    if (identical(mem_modal_key(), key)) return()
    mem_modal_key(key)
    show_hpc_modal(est)
  }

  click_hpc_download <- function() {
    shinyjs::runjs("var b=document.getElementById('dl_hpc_pack'); if(b) b.click();")
  }

  output$hpc_test_box <- renderUI({
    res <- hpc_test_result()
    if (is.null(res)) {
      return(p(class = "hint", "Click Test SSH connection before Submit. Host can be an SSH alias from ~/.ssh/config."))
    }
    cls <- if (isTRUE(res$ok)) "ok-box" else "warn-box"
    div(class = cls, p(style = "margin:0;", res$message))
  })

  observeEvent(input$hpc_test, {
    res <- tryCatch(
      hpc_test_connection(input$hpc_user, input$hpc_host),
      error = function(e) list(ok = FALSE, message = conditionMessage(e), suggested_project = "")
    )
    hpc_test_result(res)
    if (isTRUE(res$ok)) {
      remote <- trimws(as.character(if (is.null(input$hpc_remote)) "" else input$hpc_remote))
      if (!nzchar(remote) && nzchar(res$suggested_project)) {
        updateTextInput(session, "hpc_remote", value = res$suggested_project)
      }
      write_hpc_settings(
        user = input$hpc_user,
        host = input$hpc_host,
        remote_project = if (nzchar(remote)) remote else res$suggested_project
      )
      hpc_settings(read_hpc_settings())
    }
  })

  observeEvent(input$data_first_aid_main, {
    show_data_first_aid()
  })
  observeEvent(input$time_first_aid_main, {
    show_time_first_aid()
  })
  observeEvent(input$data_first_aid_inline, {
    show_data_first_aid()
  })
  observeEvent(input$data_aid_contact_dev, {
    show_data_contact_dev()
  })
  observeEvent(input$data_aid_back, {
    show_data_first_aid()
  })
  observeEvent(input$data_aid_copy_note, {
    shinyjs::runjs(paste(
      "var t=document.getElementById('data_aid_contact_text');",
      "var v=t?t.value:'';",
      "navigator.clipboard.writeText(v).then(function(){}, function(){ if(t){ t.focus(); t.select(); } });"
    ))
    showNotification("Note copied. Paste it into email or a message to the developer.", type = "message", duration = 5)
  })
  observeEvent(input$hpc_first_aid_main, {
    hpc_aid_return(FALSE)
    show_hpc_first_aid()
  })
  observeEvent(input$hpc_first_aid_modal, {
    tryCatch({
      write_hpc_settings(
        user = input$hpc_user,
        host = input$hpc_host,
        remote_project = if (is.null(input$hpc_remote)) "" else input$hpc_remote
      )
      hpc_settings(read_hpc_settings())
    }, error = function(e) NULL)
    hpc_aid_return(TRUE)
    show_hpc_first_aid()
  })
  observeEvent(input$hpc_aid_back, {
    hpc_aid_return(FALSE)
    show_hpc_modal(current_mem_check())
  })

  observeEvent(input$mem_open_hpc, {
    show_hpc_modal(current_mem_check())
  })
  observeEvent(input$confirm_open_hpc, {
    show_hpc_modal(current_mem_check())
  })
  observeEvent(input$run_open_hpc, {
    show_hpc_modal(current_mem_check())
  })
  observeEvent(input$plan_use_hpc, {
    start_pending(FALSE)
    start_lock(FALSE)
    removeModal()
    show_hpc_modal(current_mem_check())
  })
  observeEvent(input$hpc_modal_dl, click_hpc_download())
  observeEvent(input$force_hpc_local, {
    force_local(TRUE)
    removeModal()
    showNotification(
      "Local run allowed for this session. Close other programs. The computer may freeze.",
      type = "warning", duration = 8
    )
  })

  do_hpc_start <- function() {
    if (isTRUE(run_alive())) {
      showNotification("A run is already in progress.", type = "warning")
      return(FALSE)
    }
    if (!hpc_has_ssh()) {
      showNotification("ssh is not installed on this computer.", type = "error")
      return(FALSE)
    }
    if (!nzchar(hpc_bash())) {
      showNotification("bash is required for HPC submit (macOS/Linux, or Git Bash on Windows).", type = "error")
      return(FALSE)
    }
    user <- trimws(as.character(if (is.null(input$hpc_user)) "" else input$hpc_user))
    host <- trimws(as.character(if (is.null(input$hpc_host)) "" else input$hpc_host))
    remote <- trimws(as.character(if (is.null(input$hpc_remote)) "" else input$hpc_remote))
    if (!nzchar(host)) {
      showNotification("Enter the HPC host (or your SSH alias).", type = "error")
      return(FALSE)
    }
    chk <- tryCatch(
      hpc_test_connection(user, host),
      error = function(e) list(ok = FALSE, message = conditionMessage(e), suggested_project = "")
    )
    hpc_test_result(chk)
    if (!isTRUE(chk$ok)) {
      showNotification(chk$message, type = "error", duration = 12)
      return(FALSE)
    }
    if (!nzchar(remote)) remote <- chk$suggested_project
    if (grepl("^~", remote)) {
      showNotification("Use the full cluster path from Test SSH (not ~).", type = "error")
      return(FALSE)
    }
    remote <- tryCatch(hpc_safe_token(remote, "cluster folder"), error = function(e) {
      showNotification(conditionMessage(e), type = "error")
      return("")
    })
    if (!nzchar(remote)) return(FALSE)
    d <- discovered()
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    if (is.null(d) || !isTRUE(d$ok) || is.null(m) || !nrow(m)) {
      showNotification("Tick samples and set groups first.", type = "error")
      return(FALSE)
    }
    keep <- as.character(m$sample)
    dirs <- as.character(d$dirs[keep])
    if (any(!dir.exists(dirs))) {
      showNotification("A ticked sample folder is missing on this computer.", type = "error")
      return(FALSE)
    }
    est <- current_mem_check()
    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    od <- outdir()
    if (!is.null(input$outdir_path) && nzchar(input$outdir_path)) od <- input$outdir_path
    if (!nzchar(od)) od <- file.path(project, "results", paste0("scauto_hpc_", stamp))
    outdir(od)
    write_hpc_settings(user, host, remote, last_outdir = od)
    hpc_settings(read_hpc_settings())
    dir.create(file.path(od, "logs"), recursive = TRUE, showWarnings = FALSE)
    meta_df(m)
    meta_path <- write_run_meta(build_run_meta(m), file.path(od, "sample_meta_gui.csv"))
    hpc_in <- paste0(remote, "/scauto_hpc_input/", stamp)
    hpc_out <- paste0(remote, "/results/", basename(od))
    hpc_meta <- paste0(hpc_in, "/sample_meta.csv")
    ncpus <- 8L
    mem_gb <- if (!is.null(est$pbs_mem_gb) && is.finite(est$pbs_mem_gb)) as.integer(est$pbs_mem_gb) else 64L
    n_cells <- if (!is.null(est$n_cells)) est$n_cells else NA_real_
    wall <- if (is.finite(n_cells) && n_cells > 120000) "72:00:00" else "48:00:00"
    r_mod <- "R"
    cfg_path <- hpc_write_config_env(
      file.path(od, "logs", "hpc_config.env"),
      user = user, host = host, hpc_project = remote, r_module = r_mod
    )
    pbs_path <- hpc_write_pbs_file(
      file.path(od, "logs", "run_scauto.pbs"),
      hpc_project = remote, hpc_in = hpc_in, hpc_out = hpc_out, hpc_meta = hpc_meta,
      samples = keep, ncpus = ncpus, mem_gb = mem_gb, wall = wall,
      config_remote = if (isTRUE(run_needs_config_overlay(isolate(advanced_cfg()), isolate(tissue_choice()))) ||
                            identical(isolate(tissue_choice()), "liver")) {
        paste0(remote, "/hpc/scauto_advanced.yaml")
      } else {
        ""
      },
      usage_user = if (exists("scauto_usage_user", mode = "function")) scauto_usage_user() else "",
      usage_host = if (exists("scauto_usage_host", mode = "function")) scauto_usage_host() else ""
    )
    adv_yaml <- prepare_run_overlay(od)
    map_path <- write_hpc_sample_map(file.path(od, "logs", "hpc_sample_map.tsv"), keep, dirs)
    env_kv <- list(
        HPC_SSH = hpc_ssh_target(user, host),
        REMOTE_PROJECT = remote,
        LOCAL_PROJECT = project,
        LOCAL_SCAUTO = scauto_root,
        LOCAL_VIZ = file.path(project, "viz"),
        LOCAL_OUT = od,
        LOCAL_META = meta_path,
        LOCAL_LOG = file.path(od, "logs", "pipeline.log"),
        LOCAL_CONFIG_ENV = cfg_path,
        LOCAL_PBS = pbs_path,
        SAMPLE_MAP = map_path,
        REMOTE_INPUT = hpc_in,
        REMOTE_OUT = hpc_out,
        SAMPLES = paste(keep, collapse = ","),
        NCPUS = ncpus,
        MEM_GB = mem_gb,
        WALLTIME = wall,
        POLL_SEC = 75,
        R_MODULE = r_mod,
        PBS_QUEUE = "medium"
    )
    if (nzchar(adv_yaml)) env_kv$LOCAL_ADVANCED_YAML <- adv_yaml
    env_path <- write_hpc_job_env(
      file.path(od, "logs", "hpc_job.env"),
      env_kv
    )
    log_path <- file.path(od, "logs", "pipeline.log")
    pid_path <- file.path(od, "logs", "hpc_watcher.pid")
    if (file.exists(log_path)) unlink(log_path)
    for (stale in c("compute.log", "pbs_job.out")) {
      sp <- file.path(od, "logs", stale)
      if (file.exists(sp)) unlink(sp)
    }
    launched <- tryCatch(
      start_hpc_one_click(
        script = file.path(scauto_root, "hpc", "one_click.sh"),
        env_file = env_path,
        log_path = log_path,
        pid_path = pid_path
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(launched)) return(FALSE)
    proc(launched$process)
    run_mode("hpc")
    run_stopped(FALSE)
    run_started_at(Sys.time())
    run_elapsed_sec(NA_real_)
    run_alive(TRUE)
    run_exit(NA_integer_)
    finish_announced(FALSE)
    selected_result(NULL)
    set_stop_enabled(TRUE)
    log_stamp(isolate(log_stamp()) + 1L)
    if (exists("scauto_usage_record_start", mode = "function")) {
      n_bc <- if ("n_barcodes" %in% names(m)) sum(as.numeric(m$n_barcodes), na.rm = TRUE) else NA_real_
      scauto_usage_record_start(project, m$sample, n_bc, od, mode = "hpc")
    }
    TRUE
  }

  observeEvent(input$hpc_submit, {
    ok <- do_hpc_start()
    if (isTRUE(ok)) {
      removeModal()
      step(4L)
      showNotification(
        "Submitted to HPC. This page uploads the ticked samples, waits for the job, then copies results back.",
        type = "message",
        duration = 10
      )
    }
  })

  current_hpc_resume <- function() {
    extra <- c(isolate(hpc_settings()$last_outdir), isolate(outdir()))
    hpc_resume_info(project, extra = extra)
  }

  do_hpc_resume <- function() {
    if (isTRUE(run_alive())) {
      showNotification("A run is already in progress.", type = "warning")
      return(FALSE)
    }
    info <- current_hpc_resume()
    if (is.null(info)) {
      showNotification("No interrupted HPC job was found on this computer.", type = "warning")
      return(FALSE)
    }
    od <- info$outdir
    env_path <- file.path(od, "logs", "hpc_job.env")
    log_path <- file.path(od, "logs", "pipeline.log")
    pid_path <- file.path(od, "logs", "hpc_watcher.pid")
    if (!file.exists(env_path)) {
      showNotification("Missing logs/hpc_job.env in that results folder.", type = "error")
      return(FALSE)
    }
    if (!dir.exists(dirname(od))) {
      showNotification(
        paste0("The results disk is not available: ", dirname(od),
               ". Plug it in, then click Continue again."),
        type = "error", duration = 12
      )
      return(FALSE)
    }
    dir.create(file.path(od, "logs"), recursive = TRUE, showWarnings = FALSE)
    outdir(od)
    write_hpc_settings(last_outdir = od)
    hpc_settings(read_hpc_settings())
    launched <- tryCatch(
      start_hpc_one_click(
        script = file.path(scauto_root, "hpc", "one_click.sh"),
        env_file = env_path,
        log_path = log_path,
        pid_path = pid_path,
        resume = TRUE
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(launched)) return(FALSE)
    proc(launched$process)
    run_mode("hpc")
    run_stopped(FALSE)
    run_started_at(Sys.time())
    run_elapsed_sec(NA_real_)
    run_alive(TRUE)
    run_exit(NA_integer_)
    finish_announced(FALSE)
    selected_result(NULL)
    set_stop_enabled(TRUE)
    log_stamp(isolate(log_stamp()) + 1L)
    TRUE
  }

  output$hpc_resume_banner <- renderUI({
    if (!isTRUE(run_alive())) invalidateLater(8000, session)
    log_stamp()
    if (isTRUE(run_alive())) return(NULL)
    info <- current_hpc_resume()
    if (is.null(info)) return(NULL)
    if (identical(hpc_resume_dismissed(), info$outdir)) return(NULL)
    st <- hpc_stage_label(info$stage)
    div(
      class = "resume-banner",
      div(
        class = "resume-head",
        p("HPC job still on the cluster"),
        actionButton("hpc_resume_close", "Close", class = "btn btn-outline-secondary btn-sm resume-close")
      ),
      p(style = "margin:8px 0 0 0;",
        "This computer was turned off (or the helper was closed) before results were copied back. ",
        "The cluster job was not cancelled. Continue will wait if it is still running, then copy results here. ",
        "It will not upload samples or submit a new job."),
      p(style = "margin:8px 0 0 0; font-size:13px; color: var(--steel);",
        tags$b("Job "), info$job_id, " · ", st, " · ", tags$code(info$outdir)),
      div(
        class = "mt-2",
        actionButton("hpc_resume", "Continue last HPC job (copy results here)",
                     class = "btn-hpc btn")
      )
    )
  })

  observeEvent(input$hpc_resume_close, {
    info <- current_hpc_resume()
    if (!is.null(info)) hpc_resume_dismissed(info$outdir)
  })

  observeEvent(input$hpc_resume, {
    ok <- do_hpc_resume()
    if (isTRUE(ok)) {
      step(4L)
      showNotification(
        "Continuing the last HPC job. Waiting on the cluster, then copying results to this computer.",
        type = "message",
        duration = 10
      )
    }
  })

  observeEvent(input$mem_float_close, {
    mem_float_hidden(TRUE)
  })

  observe({
    est <- current_mem_check()
    key <- if (is.null(est)) "" else paste(est$n_samples, round(est$n_cells), est$level, sep = "|")
    isolate({
      if (!identical(key, mem_sel_key())) {
        mem_sel_key(key)
        mem_float_hidden(FALSE)
        force_local(FALSE)
      }
    })
  })

  output$mem_float <- renderUI({
    if (step() != 1L) return(NULL)
    if (isTRUE(mem_float_hidden())) return(NULL)
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return(NULL)
    memory_float_ui(current_mem_check())
  })

  apply_chosen_dir <- function(picked, keep_names = NULL, notify = TRUE) {
    if (is.null(picked) || !nzchar(picked) || !dir.exists(picked)) {
      showNotification("That folder does not exist.", type = "error")
      return(FALSE)
    }
    removeModal()
    picked <- normalizePath(picked)
    pending_keep(keep_names)
    input_dir(picked)
    updateTextInput(session, "input_path", value = picked)
    scan_input(picked)
    nkeep <- if (is.null(keep_names)) "all samples" else paste(keep_names, collapse = ", ")
    if (isTRUE(notify)) {
      showNotification(paste("Using folder:", picked, "—", nkeep), type = "message", duration = 6)
    }
    invisible(TRUE)
  }

  apply_trial_two_samples <- function() {
    pair <- load_group_test_pair(scauto_root)
    parent <- find_trial_pair_parent(pair$sample, project, extra = browse_path())
    if (is.null(parent)) {
      showNotification(
        paste0(
          "Could not find trial samples ", paste(pair$sample, collapse = ", "),
          ". Put them in data/trial_4samples (or mtx_files_per_sample), or open that folder first."
        ),
        type = "error",
        duration = 8
      )
      return(FALSE)
    }
    if (!isTRUE(apply_chosen_dir(parent, keep_names = pair$sample, notify = FALSE))) return(FALSE)
    if (!apply_group_test_pair()) return(FALSE)
    showNotification(
      paste0("Trial samples: ", trial_set_hint(pair), ". Groups are filled."),
      type = "message",
      duration = 8
    )
    TRUE
  }

  open_folder_browser <- function() {
    browse_selected(character())
    browse_path(default_browse_path())
    native_lbl <- switch(
      Sys.info()[["sysname"]],
      Darwin = "Finder…",
      Windows = "File Explorer…",
      "System folder window…"
    )
    showModal(modalDialog(
      title = "Choose a data folder",
      p(class = "hint",
        "Click a folder name to open it. Tick the samples you want, then Use ticked samples.",
        " Or Use this folder to take every sample in the current folder."),
      div(
        class = "trial-box",
        p(class = "hint", style = "margin:0 0 8px 0;",
          paste0("First try: ", trial_set_hint(), ". No need to find folders.")),
        actionButton("browse_use_trial_pair", "Use trial samples (2 GroupA + 2 GroupB)",
                     class = "btn-trial btn")
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-2",
        actionButton("browse_go_home", "Home folder", class = "btn btn-outline-secondary btn-sm"),
        if (dir.exists("/Volumes")) actionButton("browse_go_volumes", "External drives", class = "btn btn-outline-secondary btn-sm")
      ),
      div(class = "path-box mb-2", textOutput("browse_path_label", inline = TRUE)),
      div(
        class = "d-flex gap-2 mb-2 flex-wrap",
        actionButton("browse_up", "Up one level", class = "btn btn-outline-secondary btn-sm"),
        actionButton("browse_select_all", "Tick all samples here", class = "btn btn-outline-secondary btn-sm"),
        actionButton("browse_select_none", "Clear ticks", class = "btn btn-outline-secondary btn-sm"),
        actionButton("browse_native", native_lbl, class = "btn btn-outline-secondary btn-sm")
      ),
      uiOutput("browse_note"),
      uiOutput("browse_listing"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("browse_use_selected", "Use ticked samples", class = "btn-navy btn"),
        actionButton("browse_use", "Use this folder (all samples)", class = "btn btn-outline-secondary")
      ),
      size = "l",
      easyClose = TRUE
    ))
  }

  observeEvent(input$pick_dir, {
    open_folder_browser()
  })

  output$browse_path_label <- renderText({
    p <- browse_path()
    n <- length(browse_selected())
    if (!nzchar(p)) return("(no folder)")
    if (n) paste0(p, "  ·  ", n, " ticked") else p
  })

  output$browse_note <- renderUI({
    note <- current_10x_note(browse_path())
    n <- length(browse_selected())
    tagList(
      if (n > 0) div(class = "ok-box mb-2", sprintf("%s sample(s) ticked. Click Use ticked samples.", n)),
      if (!is.null(note)) div(class = "ok-box mb-2", note)
    )
  })

  output$browse_listing <- renderUI({
    p <- browse_path()
    picked <- browse_selected()
    ents <- list_browse_entries(p)
    if (!nrow(ents)) {
      return(p(class = "hint", "No subfolders here. If this is already the 10x folder, click Use this folder (all samples)."))
    }
    is_samp <- ents$is_10x
    others <- ents[!is_samp, , drop = FALSE]
    samples <- ents[is_samp, , drop = FALSE]
    n_samp <- nrow(samples)
    show <- rbind(others, samples)
    browse_row <- function(i) {
      path <- show$path[[i]]
      checked <- path %in% picked
      if (isTRUE(show$is_10x[[i]])) {
        tags$div(
          class = paste("browse-row", if (checked) "picked"),
          tags$a(
            href = "#",
            class = "name",
            onclick = js_set_input("toggle_browse_sample", path),
            paste0(if (checked) "☑  " else "☐  ", show$name[[i]])
          ),
          tags$span(class = "meta", show$hint[[i]]),
          tags$a(
            href = "#",
            class = "open-link",
            onclick = js_set_input("click_browse_dir", path),
            "Open"
          )
        )
      } else {
        tags$div(
          class = "browse-row",
          tags$a(
            href = "#",
            class = "name",
            onclick = js_set_input("click_browse_dir", path),
            show$name[[i]]
          ),
          if (nzchar(show$hint[[i]])) tags$span(class = "meta", show$hint[[i]]) else NULL,
          tags$span(class = "open-link", "open")
        )
      }
    }
    rows <- if (nrow(show)) lapply(seq_len(nrow(show)), browse_row) else list()
    extra <- if (n_samp > 3L) {
      p(
        class = "hint",
        style = "margin:8px 0 0 0;",
        sprintf("Scroll the list to see all %s samples.", n_samp)
      )
    } else {
      NULL
    }
    tagList(div(class = "file-list browse-list", rows), extra)
  })

  observeEvent(input$toggle_browse_sample, {
    p <- input$toggle_browse_sample
    if (is.null(p) || !nzchar(p)) return()
    cur <- browse_selected()
    if (p %in% cur) browse_selected(setdiff(cur, p)) else browse_selected(c(cur, p))
  })

  observeEvent(input$click_browse_dir, {
    p <- input$click_browse_dir
    if (!is.null(p) && dir.exists(p)) {
      browse_selected(character())
      browse_path(p)
    }
  })

  observeEvent(input$browse_select_all, {
    ents <- list_browse_entries(browse_path())
    browse_selected(ents$path[ents$is_10x])
  })

  observeEvent(input$browse_select_none, {
    browse_selected(character())
  })

  observeEvent(input$browse_up, {
    p <- browse_path()
    if (!nzchar(p)) return()
    parent <- dirname(normalizePath(p, mustWork = FALSE))
    if (nzchar(parent) && dir.exists(parent)) {
      browse_selected(character())
      browse_path(parent)
    }
  })

  observeEvent(input$browse_go_home, {
    browse_selected(character())
    browse_path(path.expand("~"))
  })
  observeEvent(input$browse_go_volumes, {
    browse_selected(character())
    if (dir.exists("/Volumes")) browse_path("/Volumes")
  })

  observeEvent(input$browse_use, {
    apply_chosen_dir(browse_path(), keep_names = NULL)
  })

  observeEvent(input$browse_use_trial_pair, {
    apply_trial_two_samples()
  })

  observeEvent(input$browse_use_selected, {
    sel <- browse_selected()
    if (!length(sel)) {
      showNotification("Tick at least one 10x sample (☐ → ☑), or use Use this folder (all samples).", type = "warning", duration = 8)
      return()
    }
    apply_chosen_dir(browse_path(), keep_names = unique(basename(sel)))
  })

  observeEvent(input$browse_native, {
    showNotification("If a macOS window opened, it may be behind the browser. Check the Dock for Finder.", duration = 8)
    picked <- native_choose_dir("Select a 10x data folder (matrix.mtx, or a parent of sample folders)")
    if (is.null(picked)) {
      showNotification("No folder selected in the macOS window.", type = "message")
      return()
    }
    browse_path(picked)
    apply_chosen_dir(picked, keep_names = NULL)
  })

  observeEvent(input$scan_path, {
    p <- trimws(input$input_path)
    input_dir(p)
    scan_input(p)
  })

  scan_input <- function(path) {
    d <- safe_discover(path)
    discovered(d)
    if (!isTRUE(d$ok)) {
      showNotification(d$error, type = "error", duration = 8)
      selected_sample(NULL)
      selected_10x(NULL)
      return()
    }
    keep <- pending_keep()
    nms <- names(d$dirs)
    first <- if (!is.null(keep) && length(intersect(keep, nms))) intersect(keep, nms)[[1]] else nms[[1]]
    selected_sample(first)
    files <- list_10x_files(d$dirs[[first]])
    selected_10x(if (nrow(files)) files$path[[1]] else NULL)
    mem_float_hidden(FALSE)
  }

  observeEvent(input$click_sample, {
    selected_sample(input$click_sample)
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return()
    dir <- d$dirs[[input$click_sample]]
    if (is.null(dir)) return()
    files <- list_10x_files(dir)
    selected_10x(if (nrow(files)) files$path[[1]] else NULL)
  })

  observeEvent(input$click_10x_file, {
    selected_10x(input$click_10x_file)
  })

  observeEvent(input$click_result, {
    selected_result(input$click_result)
  })

  collect_meta <- function() {
    d <- discovered()
    keep <- kept_samples()
    if (is.null(keep) || !length(keep)) keep <- names(d$dirs)
    groups <- vapply(keep, function(s) {
      val <- input[[paste0("grp_", s)]]
      if (is.null(val)) {
        m <- meta_df()
        if (!is.null(m) && s %in% m$sample) return(m$group[match(s, m$sample)])
        return("")
      }
      trimws(as.character(val))
    }, character(1))
    anames <- vapply(keep, function(s) {
      val <- input[[paste0("aname_", s)]]
      if (is.null(val)) {
        m <- meta_df()
        if (!is.null(m) && "analysis_name" %in% names(m) && s %in% m$sample) {
          v <- trimws(as.character(m$analysis_name[match(s, m$sample)]))
          if (nzchar(v)) return(v)
        }
        return(s)
      }
      v <- trimws(as.character(val))
      if (!nzchar(v)) s else v
    }, character(1))
    out <- data.frame(
      sample = keep,
      analysis_name = unname(anames),
      group = unname(groups),
      n_barcodes = unname(d$n_barcodes[keep]),
      stringsAsFactors = FALSE
    )
    xcols <- unique(c(isolate(extra_group_cols()), character(0)))
    xcols <- xcols[nzchar(xcols)]
    mcur <- isolate(meta_df())
    for (cc in xcols) {
      vals <- vapply(keep, function(s) {
        val <- input[[paste0("xgrp_", cc, "__", s)]]
        if (is.null(val)) {
          if (!is.null(mcur) && cc %in% names(mcur) && s %in% mcur$sample) {
            return(trimws(as.character(mcur[[cc]][match(s, mcur$sample)])))
          }
          return("")
        }
        trimws(as.character(val))
      }, character(1))
      out[[cc]] <- unname(vals)
    }
    skip_overlay <- unique(c("sample", "group", "n_barcodes", "analysis_name", xcols))
    out <- scauto_overlay_sample_cols(out, meta_df(), skip = skip_overlay)
    out <- scauto_overlay_sample_cols(out, clinical_extra_df(), skip = c("sample", "group", "n_barcodes"))
    out
  }

  build_run_meta <- function(m = NULL) {
    if (is.null(m)) m <- tryCatch(collect_meta(), error = function(e) meta_df())
    m <- scauto_overlay_sample_cols(m, isolate(clinical_extra_df()), skip = c("sample", "group", "n_barcodes"))
    drop <- intersect("n_barcodes", names(m))
    if (length(drop)) m <- m[, setdiff(names(m), drop), drop = FALSE]
    m
  }

  output$step_body <- renderUI({
    s <- step()
    if (s == 1L) {
      discovered()
      return(isolate(ui_step_data()))
    }
    if (s == 2L) {
      meta_df()
      extra_group_cols()
      return(isolate(ui_step_group()))
    }
    if (s == 3L) {
      meta_df()
      advanced_cfg()
      tissue_choice()
      return(isolate(ui_step_confirm()))
    }
    advanced_cfg()
    isolate(ui_step_run())
  })

  ui_step_data <- function() {
    d <- discovered()
    tagList(
      div(
        class = "data-guide",
        h4("1. Choose sequencing data"),
        p(class = "data-guide-lead",
          "scRNA-seq data from the ", tags$b("10x Genomics platform"), " (Cell Ranger) should contain three files: ",
          tags$b("matrix.mtx"), ", ", tags$b("barcodes.tsv"), ", and ",
          tags$b("features.tsv"), " (sometimes named genes.tsv). ",
          "For several samples, choose the parent folder that contains those sample folders."),
        p(class = "data-guide-alt",
          "Not 10x Genomics, or you only have RDS / h5ad / CSV? Read ",
          actionLink("data_first_aid_inline", "How to Prepare Data", class = "data-guide-prep-link"),
          " at the top of the page."),
        div(
          class = "d-flex gap-2 align-items-center flex-wrap",
          actionButton("pick_dir", "Choose folder…", class = "btn-navy btn"),
          div(style = "min-width:360px; flex:1;",
              textInput("input_path", NULL, value = input_dir(),
                        placeholder = "Or paste the folder path here")),
          actionButton("scan_path", "Detect samples", class = "btn btn-outline-secondary")
        ),
        p(class = "data-guide-note",
          "RAM and disk are checked after you tick samples.")
      ),
      if (!is.null(d) && !isTRUE(d$ok)) div(class = "warn-box mt-3", d$error),
      if (isTRUE(d$ok)) {
        tagList(
          p(class = "hint", "Tick every sample you want to analyse. You can tick several. The helper re-checks RAM and disk for the ticks."),
          div(
            class = "d-flex gap-2 mb-2 flex-wrap",
            actionButton("select_all_samples", "Select all", class = "btn btn-outline-secondary btn-sm"),
            actionButton("select_none_samples", "Select none", class = "btn btn-outline-secondary btn-sm"),
            actionButton("use_group_test_pair", "Use 4-sample group test (2 GroupA + 2 GroupB)",
                         class = "btn btn-outline-secondary btn-sm")
          ),
          span(class = "hint", paste0("Trial uses Baseline libraries: ", trial_set_hint(), ".")),
          uiOutput("sample_picker")
        )
      }
    )
  }

  output$sample_picker <- renderUI({
    d <- discovered()
    pending_keep()
    if (is.null(d) || !isTRUE(d$ok)) return(NULL)
    keep0 <- chosen_sample_names(d)
    div(
      class = "sample-picker-wrap",
      tags$table(
        class = "table table-sm sample-table",
        tags$thead(tags$tr(
          tags$th("Use"),
          tags$th("Sample"),
          tags$th("Cells (before QC)")
        )),
        tags$tbody(lapply(names(d$dirs), function(nm) {
          tags$tr(
            tags$td(checkboxInput(paste0("keep_", nm), label = NULL, value = nm %in% keep0, width = "32px")),
            tags$td(nm),
            tags$td(format(d$n_barcodes[[nm]], big.mark = ","))
          )
        }))
      )
    )
  })

  observeEvent(input$select_all_samples, {
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return()
    pending_keep(names(d$dirs))
  })

  observeEvent(input$select_none_samples, {
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) return()
    pending_keep(character())
  })

  output$input_file_list <- renderUI({
    d <- discovered()
    s <- selected_sample()
    if (is.null(d) || !isTRUE(d$ok) || is.null(s) || !s %in% names(d$dirs)) {
      return(p(class = "hint", "Click a sample name above."))
    }
    files <- list_10x_files(d$dirs[[s]])
    if (!nrow(files)) return(p(class = "hint", "No 10x files found in this folder."))
    clickable_list(
      values = files$path,
      input_id = "click_10x_file",
      selected = selected_10x(),
      labels = files$file,
      meta = paste0("(", files$size_mb, " MB)")
    )
  })

  output$input_preview <- renderUI({
    path <- selected_10x()
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
      return(p(class = "hint", "Click a file name to preview the first lines."))
    }
    ext <- tolower(tools::file_ext(gsub("\\.gz$", "", path, ignore.case = TRUE)))
    n <- if (identical(ext, "mtx")) 20L else 40L
    lines <- tryCatch(read_head_lines(path, n), error = function(e) paste("Could not read file:", conditionMessage(e)))
    tagList(
      p(class = "hint", sprintf("Showing the first %s lines of %s (%.2f MB). The rest is not loaded.",
                                n, basename(path), file.info(path)$size / 1024^2)),
      tags$pre(class = "file-preview", paste(lines, collapse = "\n"))
    )
  })

  ui_step_group <- function() {
    m <- meta_df()
    if (is.null(m) || !nrow(m)) {
      return(p(class = "hint", "Detect samples in the previous step first."))
    }
    xcols <- isolate(extra_group_cols())
    xcols <- xcols[nzchar(xcols)]
    rows <- lapply(seq_len(nrow(m)), function(i) {
      s <- m$sample[[i]]
      an <- if ("analysis_name" %in% names(m)) {
        v <- trimws(as.character(m$analysis_name[[i]]))
        if (nzchar(v)) v else s
      } else {
        s
      }
      extra_tds <- lapply(xcols, function(cc) {
        v <- if (cc %in% names(m)) trimws(as.character(m[[cc]][[i]])) else ""
        if (is.na(v)) v <- ""
        tags$td(textInput(paste0("xgrp_", cc, "__", s), NULL, value = v,
                          placeholder = paste0("e.g. value for ", cc)))
      })
      do.call(tags$tr, c(
        list(
          tags$td(
            tags$b(s),
            div(class = "hint", paste0(format(m$n_barcodes[[i]], big.mark = ","), " cells"))
          ),
          tags$td(textInput(paste0("aname_", s), NULL, value = an,
                            placeholder = "Name used in figures")),
          tags$td(textInput(paste0("grp_", s), NULL, value = m$group[[i]],
                            placeholder = "e.g. GroupA or GroupB"))
        ),
        extra_tds
      ))
    })
    extra_ths <- lapply(xcols, function(cc) {
      tags$th(
        class = "extra-group-th",
        div(
          style = "display:flex;align-items:center;gap:8px;flex-wrap:nowrap;",
          span(cc),
          tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-secondary",
            style = "padding:1px 7px;font-size:12px;line-height:1.3;font-weight:600;",
            onclick = sprintf("Shiny.setInputValue('rm_xcol', '%s', {priority:'event'})", cc),
            "Remove"
          )
        )
      )
    })
    tagList(
      h4("2. Clinical groups"),
      p(class = "hint",
        "Use the same labels as the study, for example ", tags$b("GroupA"), " / ", tags$b("GroupB"),
        ". The pipeline will compare cell-type proportions across groups. If you are not comparing, enter the same word for every sample. ",
        tags$b("Analysis name"), " is what appears on figures and tables. Leave it as the folder name unless you want a shorter label. Each name must be unique."),
      p(class = "hint",
        "Need more than one grouping (for example Timepoint: Baseline / Followup, or Treatment)? Click ",
        tags$b("Add another clinical group column"), " and label every sample in the new column."),
      div(
        class = "mb-3 d-flex gap-2 flex-wrap align-items-center",
        actionButton("add_group_col", "Add another clinical group column", class = "btn-add-meta btn"),
        actionButton("use_group_test_pair_groups", "Fill trial groups (2 GroupA + 2 GroupB, Baseline)",
                     class = "btn btn-outline-secondary")
      ),
      div(
        class = "group-list-wrap",
        tags$table(
          class = "table table-sm sample-table group-table",
          tags$thead(do.call(tags$tr, c(
            list(
              tags$th("Sample name"),
              tags$th("Analysis name"),
              tags$th("Clinical group")
            ),
            extra_ths
          ))),
          tags$tbody(rows)
        )
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        downloadButton("dl_meta_template", "Download group template (.csv)", class = "btn btn-outline-secondary"),
        downloadButton("dl_meta_current", "Download current groups", class = "btn btn-outline-secondary")
      ),
      fileInput("meta_upload", "Or upload an existing sample_meta.csv (columns: sample, analysis_name, group; extra group columns are kept)",
                accept = c(".csv"), buttonLabel = "Upload CSV"),
      p(class = "hint", "To correlate Age, LabValue1, and similar numeric sample columns with subtype proportions, import that CSV under Advanced Settings."),
      h5("Preview"),
      uiOutput("meta_preview")
    )
  }

  output$meta_preview <- renderUI({
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    if (is.null(m) || !nrow(m)) return(NULL)
    xcols <- isolate(extra_group_cols())
    xcols <- xcols[xcols %in% names(m)]
    show_cols <- unique(c("sample", "analysis_name", "group", xcols))
    show_cols <- intersect(show_cols, names(m))
    tags$table(
      class = "table table-sm",
      tags$thead(tags$tr(
        lapply(show_cols, function(cc) {
          tags$th(switch(cc,
            sample = "Sample name",
            analysis_name = "Analysis name",
            group = "Clinical group",
            cc
          ))
        }),
        tags$th("Cells")
      )),
      tags$tbody(lapply(seq_len(nrow(m)), function(i) {
        tags$tr(
          lapply(show_cols, function(cc) {
            val <- trimws(as.character(m[[cc]][[i]]))
            if (is.na(val) || !nzchar(val)) {
              tags$td(tags$span(class = "hint", "(empty)"))
            } else {
              tags$td(val)
            }
          }),
          tags$td(format(m$n_barcodes[[i]], big.mark = ","))
        )
      }))
    )
  })

  apply_group_test_pair <- function() {
    d <- discovered()
    if (is.null(d) || !isTRUE(d$ok)) {
      showNotification("Detect samples first. Choose the parent folder (data/trial_4samples or your 10x samples).", type = "error")
      return(FALSE)
    }
    pair <- load_group_test_pair(scauto_root)
    need <- as.character(pair$sample)
    have <- intersect(need, names(d$dirs))
    if (length(have) < length(need)) {
      showNotification(
        paste0("Need all trial samples (", paste(need, collapse = ", "),
               ") under the chosen folder. Found: ",
               if (length(have)) paste(have, collapse = ", ") else "none"),
        type = "error",
        duration = 8
      )
      return(FALSE)
    }
    pair <- pair[match(have, pair$sample), , drop = FALSE]
    pending_keep(have)
    lapply(names(d$dirs), function(s) {
      id <- paste0("keep_", s)
      if (!is.null(input[[id]])) updateCheckboxInput(session, id, value = s %in% have)
    })
    cur <- meta_df()
    an <- have
    if (!is.null(cur) && "analysis_name" %in% names(cur)) {
      an <- as.character(cur$analysis_name[match(have, cur$sample)])
      miss <- is.na(an) | !nzchar(trimws(an))
      an[miss] <- have[miss]
    }
    meta_df(data.frame(
      sample = have,
      analysis_name = an,
      group = pair$group,
      n_barcodes = unname(d$n_barcodes[have]),
      stringsAsFactors = FALSE
    ))
    if (!is.null(cur)) {
      keep_x <- setdiff(names(cur), c("sample", "analysis_name", "group", "n_barcodes"))
      if (length(keep_x)) {
        newm <- meta_df()
        newm <- scauto_overlay_sample_cols(newm, cur, skip = c("sample", "analysis_name", "group", "n_barcodes"))
        meta_df(newm)
      }
    }
    newm <- meta_df()
    for (i in seq_along(have)) {
      updateTextInput(session, paste0("grp_", have[[i]]), value = pair$group[[i]])
      updateTextInput(session, paste0("aname_", have[[i]]), value = an[[i]])
      for (cc in isolate(extra_group_cols())) {
        v <- if (cc %in% names(newm)) as.character(newm[[cc]][match(have[[i]], newm$sample)]) else ""
        if (is.na(v)) v <- ""
        updateTextInput(session, paste0("xgrp_", cc, "__", have[[i]]), value = v)
      }
    }
    TRUE
  }

  observeEvent(input$use_group_test_pair, {
    if (apply_group_test_pair()) {
      showNotification(
        paste0(trial_set_phrase(), ". Check the Groups page, then continue."),
        type = "message", duration = 6
      )
      step(2L)
    }
  })

  observeEvent(input$use_group_test_pair_groups, {
    if (apply_group_test_pair()) {
      showNotification(paste0("Loaded ", trial_set_hint(), "."), type = "message")
    }
  })

  observeEvent(input$meta_upload, {
    f <- input$meta_upload
    if (is.null(f)) return()
    up <- tryCatch(utils::read.csv(f$datapath, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(up) || !"sample" %in% names(up)) {
      showNotification("The CSV needs a sample column.", type = "error")
      return()
    }
    m <- meta_df()
    gcol <- if ("group" %in% names(up)) "group" else setdiff(names(up), "sample")[1]
    if (is.na(gcol) || !nzchar(gcol)) return()
    m$group <- up[[gcol]][match(m$sample, as.character(up$sample))]
    m$group[is.na(m$group)] <- ""
    m <- scauto_overlay_sample_cols(m, up, skip = c("sample", "group", "n_barcodes"))
    if (!"analysis_name" %in% names(m)) m$analysis_name <- m$sample
    m$analysis_name[is.na(m$analysis_name) | !nzchar(trimws(as.character(m$analysis_name)))] <- m$sample[
      is.na(m$analysis_name) | !nzchar(trimws(as.character(m$analysis_name)))
    ]
    extra_up <- setdiff(names(up), c("sample", "analysis_name", "group", "n_barcodes", "folder"))
    extra_up <- extra_up[vapply(extra_up, function(cc) {
      !isTRUE(scauto_col_looks_numeric(up[[cc]]))
    }, logical(1))]
    extra_group_cols(unique(c(isolate(extra_group_cols()), extra_up)))
    meta_df(m)
    for (i in seq_len(nrow(m))) {
      updateTextInput(session, paste0("grp_", m$sample[[i]]), value = m$group[[i]])
      updateTextInput(session, paste0("aname_", m$sample[[i]]), value = m$analysis_name[[i]])
      for (cc in isolate(extra_group_cols())) {
        v <- if (cc %in% names(m)) as.character(m[[cc]][[i]]) else ""
        if (is.na(v)) v <- ""
        updateTextInput(session, paste0("xgrp_", cc, "__", m$sample[[i]]), value = v)
      }
    }
  })

  observeEvent(input$add_group_col, {
    save_groups_from_inputs()
    showModal(modalDialog(
      title = "Add another clinical group column",
      textInput(
        "new_group_col_name",
        "Column name",
        value = "",
        placeholder = "e.g. Timepoint or Treatment"
      ),
      p(class = "hint",
        "This is a second (or third) way to label the same samples. The original Clinical group column stays the main comparison. Extra columns are also used for proportion tests and DEGs under clinical/<lineage>/by_<column>/."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_group_col", "Add column", class = "btn-navy btn btn-lg")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirm_add_group_col, {
    nm <- if (exists("scauto_sanitize_meta_col", mode = "function")) {
      scauto_sanitize_meta_col(input$new_group_col_name)
    } else {
      gsub("[^A-Za-z0-9_]+", "_", trimws(as.character(input$new_group_col_name)[1]))
    }
    if (!nzchar(nm) || identical(tolower(nm), "group")) {
      showNotification("Use a short name such as Timepoint or Treatment (not sample, group, or analysis_name).", type = "error")
      return()
    }
    have <- unique(c(isolate(extra_group_cols()), names(isolate(meta_df()))))
    if (nm %in% have) {
      showNotification(paste0("A column named ", nm, " is already there."), type = "warning")
      return()
    }
    if (length(isolate(extra_group_cols())) >= 8L) {
      showNotification("Eight extra group columns is the maximum in this helper.", type = "error")
      return()
    }
    save_groups_from_inputs()
    m <- meta_df()
    if (!is.null(m) && nrow(m)) m[[nm]] <- ""
    meta_df(m)
    extra_group_cols(c(isolate(extra_group_cols()), nm))
    removeModal()
    showNotification(paste0("Added column ", nm, ". Label every sample, then continue."), type = "message", duration = 6)
  })

  observeEvent(input$rm_xcol, {
    cc <- as.character(input$rm_xcol)[1]
    if (is.null(cc) || !nzchar(cc)) return()
    save_groups_from_inputs()
    extra_group_cols(setdiff(isolate(extra_group_cols()), cc))
    m <- meta_df()
    if (!is.null(m) && cc %in% names(m)) {
      m[[cc]] <- NULL
      meta_df(m)
    }
    showNotification(paste0("Removed column ", cc, "."), type = "message", duration = 4)
  }, ignoreInit = TRUE)

  ui_step_confirm <- function() {
    m <- meta_df()
    est <- current_mem_check()
    nkeep <- if (is.null(m)) 0 else nrow(m)
    ncell <- if (is.null(m)) 0 else sum(m$n_barcodes, na.rm = TRUE)
    tagList(
      div(
        class = "d-flex gap-2 flex-wrap align-items-center justify-content-between mb-2",
        h4("3. Confirm default settings", style = "margin:0;"),
        actionButton("adv_from_settings_top", "Advanced Settings", class = "btn-adv btn btn-adv-hero")
      ),
      p(class = "settings-lead", "These are the lab standard settings. You usually do not need to change them. Choose ",
        tags$b("Liver"), " below if this is liver tissue (mitochondrial QC becomes 50%). Use ",
        tags$b("Advanced Settings"), " to change QC, clustering, Harmony, DEG cutoffs, pathway databases, lineage trajectory (CytoTRACE2 / Monocle3), extra analysis based on more clinical parameters, or RDS saving."),
      div(
        class = "ok-box",
        style = "margin:0 0 14px 0;",
        radioButtons(
          "tissue_type", NULL,
          choices = scauto_tissue_choices(),
          selected = isolate(tissue_choice())
        ),
        p(class = "hint", style = "margin:0;",
          "Liver hepatocytes are mitochondria-rich. A 10% cutoff would throw away too many real cells.")
      ),
      tags$ul(
        tags$li(if (identical(isolate(tissue_choice()), "liver")) {
          "QC: mitochondrial genes ≤ 50% (liver); UMIs per cell 1,000–40,000; genes per cell ≤ 8,000"
        } else {
          "QC: mitochondrial genes ≤ 10% (PBMC / blood); UMIs per cell 1,000–40,000; genes per cell ≤ 8,000"
        }),
        tags$li("2,000 highly variable genes (MT/RP excluded from HVGs, kept in the object)"),
        tags$li("PCA 50 components; Leiden on the first 30 PCs (resolution 0.8)"),
        tags$li("Harmony by sample when there is more than one library; never split by clinical group"),
        tags$li("Lineage subtypes: CASSIA when the cassia CLI and an agent backend (e.g. Claude Code) are available; otherwise compact marker panels. UMAPs, modules, proportions, and DEGs use those labels"),
        tags$li("Start analysis always re-checks every component (R, files, packages) on the next page, even if they were already OK, and installs anything missing"),
        tags$li("Outputs: all cluster markers, lineage, five-lineage reclustering, CASSIA (or panel) subtype names, per-subtype FindAllMarkers (all genes plus top 100), dot plots, module heatmaps, group proportions, Wilcoxon and MAST DEGs with volcano plots, plus GO / KEGG / GSEA / GSVA / Reactome in separate folders. Import sample clinical numbers in Advanced Settings for Spearman correlation heatmaps. CytoTRACE2 and Monocle3 stay off unless you turn them on in Advanced Settings for selected lineages. If you want more analysis yourself in R, use ",
                tags$code("objects/atlas_seurat.rds"), " in the results folder.")
      ),
      if (!is.null(est) && isTRUE(est$valid) && isTRUE(est$prompt_hpc)) {
        div(
          class = "warn-box",
          p(style = "margin:0; font-weight:600;", est$title),
          p(style = "margin:8px 0 0 0;", est$headline),
          p(style = "margin:8px 0 0 0;", est$detail)
        )
      } else {
        div(class = "ok-box",
            sprintf("Will analyse %s sample(s), about %s cells before QC. University HPC is available if you prefer the cluster.",
                    nkeep, format(ncell, big.mark = ",")))
      },
      textInput("outdir_path", "Save results to", value = outdir()),
      checkboxInput("allow_large", "Force a local run even if the dataset is very large (not recommended)",
                    value = isTRUE(isolate(force_local()))),
      h5("Environment check"),
      p(class = "hint", "These items were checked when this page opened. Start analysis checks all of them again and shows each step."),
      div(class = "check-wrap", uiOutput("env_check_panel")),
      div(
        class = "mt-3 d-flex gap-2 flex-wrap align-items-center",
        if (isTRUE(isolate(run_alive()))) {
          tagList(
            actionButton("start_from_settings", "Analysing…", class = "btn-running btn btn-lg", disabled = TRUE),
            actionButton("adv_from_settings", "Advanced Settings", class = "btn-adv btn btn-adv-hero", disabled = TRUE),
            actionButton("confirm_open_hpc", "Use university HPC", class = "btn-hpc btn btn-lg", disabled = TRUE)
          )
        } else {
          tagList(
            actionButton("start_from_settings", "Start analysis", class = "btn-run btn btn-lg"),
            actionButton("adv_from_settings", "Advanced Settings", class = "btn-adv btn btn-adv-hero"),
            actionButton("confirm_open_hpc", "Use university HPC", class = "btn-hpc btn btn-lg")
          )
        }
      ),
      if (!is.null(isolate(advanced_cfg()))) {
        p(style = "margin-top:10px;", HTML(advanced_summary_html(isolate(advanced_cfg()))))
      }
    )
  }

  observeEvent(input$outdir_path, {
    if (!is.null(input$outdir_path) && nzchar(input$outdir_path)) outdir(input$outdir_path)
  }, ignoreInit = TRUE)

  ui_step_run <- function() {
    tagList(
      h4("4. Run, monitor, and download"),
      p(class = "hint", "Click Start analysis for a local run, or Use university HPC. QC and other knobs were set on the previous page. Force Stop appears after the analysis starts."),
      div(
        class = "d-flex gap-2 mb-3 flex-wrap align-items-center run-toolbar",
        if (isTRUE(isolate(run_alive()))) {
          actionButton("start_run", "Analysing…", class = "btn-running btn btn-lg", disabled = TRUE)
        } else {
          actionButton("start_run", "Start analysis", class = "btn-run btn btn-lg")
        },
        if (isTRUE(isolate(run_alive()))) {
          actionButton("run_open_hpc", "Use university HPC", class = "btn-hpc btn btn-lg", disabled = TRUE)
        } else {
          actionButton("run_open_hpc", "Use university HPC", class = "btn-hpc btn btn-lg")
        },
        uiOutput("stop_run_toolbar", inline = TRUE),
        actionButton("open_out", "Open results folder", class = "btn btn-outline-secondary btn-lg"),
        span(class = "run-btn-sep", `aria-hidden` = "true"),
        downloadButton("dl_zip", "Download all results (.zip)", class = "btn-download btn btn-lg")
      ),
      if (!is.null(isolate(advanced_cfg()))) {
        p(style = "margin-top:4px;", HTML(advanced_summary_html(isolate(advanced_cfg()))))
      },
      uiOutput("run_status"),
      div(
        class = "run-split",
        div(
          class = "run-split-env",
          h5("Environment check"),
          p(class = "hint", "Every run checks all of these items, including those that are already installed. You will see each line go from waiting → checking → OK (or install)."),
          div(class = "check-wrap", uiOutput("env_check_panel"))
        ),
        div(
          class = "run-split-log",
          h5("Log"),
          p(class = "hint", "Updates while the run is in progress. Scroll up to the first line; the view stays on the latest lines unless you scroll up."),
          tags$pre(class = "log-box", textOutput("log_text"))
        )
      ),
      h5("Result files"),
      div(
        class = "ok-box",
        style = "margin:0 0 12px 0;",
        p(style = "margin:0;",
          "If you want to do more analysis yourself, use the RDS file in the results folder: ",
          tags$code("objects/atlas_seurat.rds"),
          ". Load it in R with Seurat (", tags$code("readRDS"),
          "). You do not need to re-run this helper. If the cell count is very large, RDS may be skipped; tables and figures are still written.")
      ),
      p(class = "hint", "Click a file to preview. Use Download all results (.zip) in the top row for the whole folder, or Open results folder to copy individual files."),
      uiOutput("result_file_list"),
      uiOutput("result_preview"),
      h5("Usage (this project copy)"),
      uiOutput("usage_summary")
    )
  }

  capture_run_elapsed <- function() {
    t0 <- isolate(run_started_at())
    secs <- if (inherits(t0, "POSIXt")) {
      as.numeric(difftime(Sys.time(), t0, units = "secs"))
    } else {
      NA_real_
    }
    run_elapsed_sec(secs)
    format_run_elapsed(secs)
  }

  elapsed_status_line <- function(live = FALSE) {
    secs <- if (isTRUE(live)) {
      t0 <- run_started_at()
      if (inherits(t0, "POSIXt")) as.numeric(difftime(Sys.time(), t0, units = "secs")) else NA_real_
    } else {
      run_elapsed_sec()
    }
    txt <- format_run_elapsed(secs)
    if (identical(txt, "unknown")) return(NULL)
    p(tags$b(if (isTRUE(live)) "Elapsed: " else "This run took "), txt, if (isTRUE(live)) NULL else ".")
  }

  output$run_status <- renderUI({
    log_stamp()
    od <- outdir()
    mode <- isolate(run_mode())
    prog <- tryCatch(
      pipeline_progress_from_log(od, mode = mode),
      error = function(e) NULL
    )
    bar <- progress_bar_ui(prog)
    live_elapsed <- elapsed_status_line(live = TRUE)
    if (isTRUE(run_alive()) && identical(mode, "hpc")) {
      st <- if (!is.null(prog$stage)) prog$stage else hpc_stage_from_log(od)
      job <- read_hpc_job_id(od)
      job_txt <- if (nzchar(job)) paste0(" PBS job ", job, ".") else ""
      div(
        class = "warn-box",
        p(style = "margin:0; font-weight:600;", hpc_stage_label(st)),
        bar,
        live_elapsed,
        p(style = "margin:8px 0 0 0;",
          "Uploading, running on HPC, then copying results back to this computer.", job_txt,
          " Leave this page open if you want live status. The copy-back continues even if you close the browser tab, as long as this computer stays on."),
        p(style = "margin:8px 0 0 0;",
          "Use ", tags$b("Force Stop"), " to kill the local watcher and ", tags$code("qdel"), " the cluster job.")
      )
    } else if (isTRUE(run_alive())) {
      div(
        class = "warn-box",
        p(style = "margin:0; font-weight:600;", "Running on this computer"),
        bar,
        live_elapsed,
        p(style = "margin:8px 0 0 0;",
          "You can leave this page open. Use ",
          tags$b("Force Stop"),
          " if you need to abort (partial files may remain).")
      )
    } else if (isTRUE(run_stopped())) {
      div(
        class = "stop-banner",
        h3("Stopped by you"),
        bar,
        elapsed_status_line(),
        p("All calculation was forced to stop. Partial files may already be in the results folder. You can download what is there, or click Start analysis to run again.")
      )
    } else if (!is.na(run_exit()) && identical(run_exit(), 0L)) {
      div(
        class = "done-banner",
        h3("Analysis finished"),
        bar,
        p(tags$b("The calculation is complete. You can download the results now.")),
        elapsed_status_line(),
        tags$ul(
          tags$li(tags$b("Download all results (.zip)"), " — blue button in the top row (whole folder)"),
          tags$li(tags$b("Open results folder"), " to browse or copy individual files"),
          tags$li("If you want more analysis yourself in R, open ",
                  tags$code("objects/atlas_seurat.rds"),
                  " from the results folder (", tags$code("readRDS"), " in Seurat). You do not need to re-run this helper.")
        )
      )
    } else if (!is.na(run_exit()) && run_exit() != 0L) {
      div(
        class = "fail-banner",
        h3("Analysis did not finish"),
        bar,
        elapsed_status_line(),
        p(paste("The run stopped (exit code", run_exit(), "). Send the log below to a colleague who can debug it."))
      )
    } else {
      div(class = "hint", "Not started yet. The list above is a first check. Click Start analysis to check every item again, install anything missing, then run.")
    }
  })

  output$env_check_panel <- renderUI({
    log_stamp()
    od <- outdir()
    live <- NULL
    if (nzchar(od) && exists("scauto_read_env_check", mode = "function")) {
      live <- scauto_read_env_check(file.path(od, "logs", "env_check.tsv"))
    }
    rows <- if (!is.null(live) && nrow(live)) live else env_rows_cache()
    env_check_ui(rows)
  })

  output$usage_summary <- renderUI({
    log_stamp()
    if (!exists("scauto_usage_summary_ui", mode = "function")) return(NULL)
    scauto_usage_summary_ui(project)
  })

  output$log_text <- renderText({
    log_stamp()
    od <- outdir()
    lf <- file.path(od, "logs", "pipeline.log")
    cf <- file.path(od, "logs", "compute.log")
    if (!file.exists(lf) && !file.exists(cf)) {
      if (isTRUE(run_alive())) return("Starting…")
      return("Not started yet.")
    }
    chunks <- character()
    if (file.exists(lf)) {
      chunks <- c(chunks, read_log_display_lines(lf))
    }
    if (file.exists(cf)) {
      chunks <- c(
        chunks,
        "",
        "----- compute node (logs/compute.log) -----",
        read_log_display_lines(cf)
      )
    }
    paste(chunks, collapse = "\n")
  })

  announce_run_finished <- function(ok) {
    set_stop_enabled(FALSE)
    step(4L)
    took <- capture_run_elapsed()
    took_line <- if (!identical(took, "unknown")) paste0(" This run took ", took, ".") else ""
    od <- isolate(outdir())
    lf <- file.path(od, "logs", "pipeline.log")
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    extra <- if (isTRUE(ok)) {
      paste0("\n========== ANALYSIS FINISHED ==========\n",
             stamp, " You can download the results on this page (zip, REPORT.md, or a single file).",
             took_line, "\n")
    } else {
      paste0("\n========== ANALYSIS DID NOT FINISH ==========\n",
             stamp, " Check the log above and send it to a colleague if you need help.",
             took_line, "\n")
    }
    if (nzchar(od) && dir.exists(dirname(lf))) {
      cat(extra, file = lf, append = TRUE)
    }
    took_ui <- if (!identical(took, "unknown")) {
      p(tags$b("This run took "), took, ".")
    } else {
      NULL
    }
    if (isTRUE(ok)) {
      showNotification(
        paste0("Analysis finished — this run took ", took, ". You can download the results now."),
        type = "message",
        duration = NULL
      )
      showModal(modalDialog(
        title = NULL,
        div(
          class = "done-banner",
          style = "margin:0;",
          h3("Analysis finished"),
          p(tags$b("The calculation is complete.")),
          took_ui,
          p("You can download the results on this page: the navy button ",
            tags$b("Download all results (.zip)"),
            " saves the whole folder. You can also download REPORT.md or a single file.")
        ),
        footer = modalButton("OK — go to downloads"),
        easyClose = TRUE,
        size = "m"
      ))
    } else {
      showNotification(
        paste0("Analysis did not finish (this run took ", took, "). Check the log on this page."),
        type = "error",
        duration = 12
      )
      showModal(modalDialog(
        title = NULL,
        div(
          class = "fail-banner",
          style = "margin:0;",
          h3("Analysis did not finish"),
          took_ui,
          p("The run stopped before completion. Keep this window open and send the log below to a colleague who can debug it.")
        ),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    }
  }

  observe({
    if (!isTRUE(run_alive())) return()
    invalidateLater(2000, session)
    od <- isolate(outdir())
    if (identical(isolate(run_mode()), "hpc") && nzchar(od)) {
      tryCatch(hpc_refresh_compute_log(od), error = function(e) NULL)
    }
    log_stamp(isolate(log_stamp()) + 1L)
  })

  # Re-attach to a nohup HPC watcher if this helper was reopened mid-job.
  observe({
    if (isTRUE(run_alive())) return()
    invalidateLater(4000, session)
    od <- outdir()
    if (!nzchar(od)) return()
    pid <- read_hpc_watcher_pid(od)
    if (!isTRUE(hpc_pid_alive(pid))) return()
    if (isTRUE(pipeline_finished_in_log(od)) || isTRUE(pipeline_failed_in_log(od))) return()
    run_mode("hpc")
    run_stopped(FALSE)
    run_exit(NA_integer_)
    finish_announced(FALSE)
    run_alive(TRUE)
    set_stop_enabled(TRUE)
    log_stamp(isolate(log_stamp()) + 1L)
  })

  observe({
    if (!isTRUE(run_alive())) return()
    invalidateLater(1000, session)
    if (isTRUE(isolate(finish_announced()))) return()
    od <- isolate(outdir())
    mode <- isolate(run_mode())
    if (identical(mode, "hpc")) {
      t0 <- isolate(run_started_at())
      age <- if (inherits(t0, "POSIXt")) as.numeric(difftime(Sys.time(), t0, units = "secs")) else 999
      if (age < 8) return()
      stage <- hpc_stage_from_log(od)
      done_ok <- identical(stage, "done") || (
        exists("pipeline_finished_in_log", mode = "function") &&
          isTRUE(pipeline_finished_in_log(od)) &&
          !isTRUE(pipeline_failed_in_log(od))
      )
      done_fail <- identical(stage, "failed") || isTRUE(pipeline_failed_in_log(od))
      pid <- read_hpc_watcher_pid(od)
      watcher_dead <- is.finite(pid) && !hpc_pid_alive(pid)
      start_fail <- age > 30 && !is.finite(pid)
      if (!done_ok && !done_fail && !watcher_dead && !start_fail) return()
      finish_announced(TRUE)
      code <- if (isTRUE(done_ok)) 0L else 1L
      stopped <- isTRUE(isolate(run_stopped()))
      run_alive(FALSE)
      run_exit(code)
      log_stamp(isolate(log_stamp()) + 1L)
      if (isTRUE(stopped)) return()
      if (exists("scauto_usage_record_finish", mode = "function") && nzchar(od)) {
        scauto_usage_record_finish(project, od, ok = identical(as.integer(code), 0L), mode = "hpc")
      }
      announce_run_finished(identical(as.integer(code), 0L))
      return()
    }
    px <- proc()
    dead <- is.null(px) || isFALSE(tryCatch(px$is_alive(), error = function(e) FALSE))
    done_log <- exists("pipeline_finished_in_log", mode = "function") &&
      isTRUE(pipeline_finished_in_log(od))
    if (!dead && !done_log) return()
    finish_announced(TRUE)
    code <- NA_integer_
    if (!is.null(px)) {
      code <- tryCatch(px$get_exit_status(), error = function(e) NA_integer_)
    }
    if (isTRUE(done_log)) code <- 0L
    if (is.na(code)) code <- if (isTRUE(done_log)) 0L else 1L
    if (!dead && !is.null(px)) {
      tryCatch(px$kill(), error = function(e) NULL)
    }
    stopped <- isTRUE(isolate(run_stopped()))
    run_alive(FALSE)
    run_exit(code)
    log_stamp(isolate(log_stamp()) + 1L)
    if (isTRUE(stopped)) return()
    if (exists("scauto_usage_record_finish", mode = "function") && nzchar(od)) {
      scauto_usage_record_finish(project, od, ok = identical(as.integer(code), 0L), mode = "local")
    }
    announce_run_finished(identical(as.integer(code), 0L))
  })

  observe({
    running <- isTRUE(run_alive())
    if (isTRUE(running)) {
      invalidateLater(8000, session)
      return()
    }
    invalidateLater(2500, session)
    od <- outdir()
    files <- list_run_files(od, skip_deg_bulk = FALSE)
    h <- paste(files$rel, files$size_mb, collapse = "|")
    if (identical(h, isolate(result_files_hash()))) return()
    result_files_hash(h)
    result_files(files)
    if (!nrow(files)) return()
    sel <- isolate(selected_result())
    if (is.null(sel) || !sel %in% files$rel) {
      selected_result(files$rel[[1]])
    }
  })

  output$result_file_list <- renderUI({
    files <- result_files()
    od <- outdir()
    if (isTRUE(run_alive())) {
      return(p(
        class = "hint",
        "The file list pauses while the analysis runs so Force Stop stays clickable. Files appear here when it finishes."
      ))
    }
    if (!nrow(files)) {
      return(p(class = "hint", "Result files will appear here after you click Start."))
    }
    addResourcePath("scauto_out", normalizePath(od))
    sel <- selected_result()
    clickable_list(
      values = files$rel,
      input_id = "click_result",
      selected = sel,
      labels = files$rel,
      meta = paste0("[", files$kind, ", ", files$size_mb, " MB]")
    )
  })

  output$result_preview <- renderUI({
    result_files_hash()
    od <- outdir()
    rel <- selected_result()
    if (!nzchar(od) || is.null(rel) || !nzchar(rel)) {
      return(p(class = "hint", "Click a result file to preview it."))
    }
    path <- file.path(od, rel)
    if (!file.exists(path)) {
      return(p(class = "hint", "That file is not on disk yet."))
    }
    addResourcePath("scauto_out", normalizePath(od))
    ext <- tolower(tools::file_ext(path))
    header <- p(class = "hint", sprintf("%s  ·  %.3f MB", rel, file.info(path)$size / 1024^2))
    body <- if (ext %in% c("png", "jpg", "jpeg")) {
      tags$img(src = paste0("scauto_out/", rel), class = "preview-img")
    } else if (ext %in% c("csv", "tsv")) {
      df <- preview_table_head(path, 50L)
      if (is.null(df)) {
        tags$pre(class = "file-preview", paste(read_head_lines(path, 40L), collapse = "\n"))
      } else {
        tags$pre(class = "file-preview", paste(utils::capture.output(print(df, row.names = FALSE)), collapse = "\n"))
      }
    } else if (ext %in% c("md", "txt", "log", "json")) {
      tags$pre(class = "file-preview", paste(read_head_lines(path, 80L), collapse = "\n"))
    } else if (identical(ext, "pdf")) {
      p(class = "hint", "PDF preview is not shown here. Use Download all results (.zip) or Open results folder.")
    } else if (identical(ext, "rds")) {
      p(class = "hint", "Seurat object (.rds) — not previewed here. If you want more analysis yourself, load ",
        tags$code("objects/atlas_seurat.rds"), " from the results folder in R with Seurat (",
        tags$code("readRDS"), "). It is also inside Download all results (.zip).")
    } else {
      tags$pre(class = "file-preview", paste(read_head_lines(path, 30L), collapse = "\n"))
    }
    tagList(header, body)
  })

  do_start <- function() {
    if (isTRUE(run_alive())) {
      showNotification("A run is already in progress.", type = "warning")
      return(FALSE)
    }
    run_stopped(FALSE)
    d <- discovered()
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    if (is.null(d) || !isTRUE(d$ok) || is.null(m) || !nrow(m)) {
      showNotification("Missing data or groups. Go back to the first two steps.", type = "error")
      return(FALSE)
    }
    meta_df(m)
    od <- outdir()
    if (!is.null(input$outdir_path) && nzchar(input$outdir_path)) od <- input$outdir_path
    if (!nzchar(od)) od <- file.path(project, "results", paste0("scauto_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    outdir(od)
    dir.create(file.path(od, "logs"), recursive = TRUE, showWarnings = FALSE)
    if (exists("scauto_empty_env_rows", mode = "function") &&
        exists("scauto_write_env_check", mode = "function")) {
      scauto_write_env_check(
        file.path(od, "logs", "env_check.tsv"),
        scauto_empty_env_rows()
      )
    }
    meta_path <- write_run_meta(build_run_meta(m), file.path(od, "sample_meta_gui.csv"))
    log_path <- file.path(od, "logs", "pipeline.log")
    if (file.exists(log_path)) unlink(log_path)
    writeLines(
      paste(
        format(Sys.time()),
        "GUI started the analysis. Checking every required component next (shown on this page)."
      ),
      log_path
    )

    args <- c(
      file.path(scauto_root, "run_scauto.R"),
      "--project", project,
      "--input", input_dir(),
      "--samples", paste(m$sample, collapse = ","),
      "--meta", meta_path,
      "--outdir", od
    )
    if (isTRUE(input$allow_large) || isTRUE(force_local())) args <- c(args, "--allow-large")
    cfg_overlay <- prepare_run_overlay(od)
    if (nzchar(cfg_overlay)) {
      args <- c(args, "--config", cfg_overlay)
      cat(
        paste(format(Sys.time()), "Config overlay:", cfg_overlay),
        file = log_path, sep = "\n", append = TRUE
      )
    }

    run_mode("local")
    if (exists("scauto_usage_user", mode = "function")) {
      Sys.setenv(
        SCAUTO_USAGE_HELPER = "1",
        SCAUTO_USAGE_USER = scauto_usage_user(),
        SCAUTO_USAGE_HOST = scauto_usage_host()
      )
    }
    if (exists("scauto_usage_record_start", mode = "function")) {
      n_bc <- if ("n_barcodes" %in% names(m)) sum(as.numeric(m$n_barcodes), na.rm = TRUE) else NA_real_
      scauto_usage_record_start(project, m$sample, n_bc, od, mode = "local")
    }
    px <- processx::process$new(
      rscript,
      args,
      stdout = log_path,
      stderr = "2>&1",
      wd = project,
      cleanup_tree = TRUE
    )
    proc(px)
    pid <- tryCatch(px$get_pid(), error = function(e) NA_integer_)
    if (is.finite(pid) && pid > 1L) {
      tryCatch(
        writeLines(as.character(pid), file.path(od, "logs", "pipeline.pid")),
        error = function(e) NULL
      )
    }
    run_started_at(Sys.time())
    run_elapsed_sec(NA_real_)
    run_alive(TRUE)
    run_exit(NA_integer_)
    finish_announced(FALSE)
    selected_result(NULL)
    set_stop_enabled(TRUE)
    log_stamp(isolate(log_stamp()) + 1L)
    showNotification(
      "Started. The page will tick through every environment check, then the analysis runs.",
      type = "message",
      duration = 8
    )
    TRUE
  }

  observeEvent(input$start_run, {
    request_start(jump_to_run = FALSE)
  })

  observeEvent(input$start_from_settings, {
    save_groups_from_inputs()
    request_start(jump_to_run = TRUE)
  })

  observeEvent(input$adv_from_settings, {
    save_groups_from_inputs()
    show_advanced_modal()
  })
  observeEvent(input$adv_from_settings_top, {
    save_groups_from_inputs()
    show_advanced_modal()
  })

  observeEvent(input$adv_clin_upload, {
    f <- input$adv_clin_upload
    if (is.null(f)) return()
    up <- tryCatch(
      utils::read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(up) || !"sample" %in% names(up)) {
      showNotification("Clinical CSV needs a sample column matching library IDs.", type = "error")
      return()
    }
    up$sample <- as.character(up$sample)
    clinical_extra_df(up)
    cols <- scauto_numeric_meta_cols(up, min_n = 3L)
    if (!length(cols)) cols <- scauto_numeric_meta_cols(up, min_n = 2L)
    updateCheckboxInput(session, "adv_clin_enable", value = TRUE)
    if (length(cols)) {
      updateCheckboxGroupInput(session, "adv_clin_cols", choices = cols, selected = cols)
    }
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    n_match <- if (is.null(m)) 0L else sum(up$sample %in% as.character(m$sample))
    showNotification(
      sprintf("Loaded %s rows; %s match ticked samples; %s numeric column(s).",
              nrow(up), n_match, length(cols)),
      type = "message",
      duration = 8
    )
  })

  output$adv_clin_preview <- renderUI({
    extra <- clinical_extra_df()
    if (is.null(extra) || !nrow(extra)) {
      return(p(class = "hint",
               "No clinical CSV loaded yet. Number columns on the Groups-page sample_meta.csv are used when the box above is ticked."))
    }
    cols <- scauto_numeric_meta_cols(extra, min_n = 3L)
    if (!length(cols)) cols <- scauto_numeric_meta_cols(extra, min_n = 2L)
    m <- tryCatch(collect_meta(), error = function(e) meta_df())
    n_match <- if (is.null(m)) 0L else sum(as.character(extra$sample) %in% as.character(m$sample))
    p_cfg <- sanitize_advanced_params(isolate(advanced_cfg()))
    sel <- p_cfg$clinical_numeric_cols
    if (!length(sel)) sel <- cols
    tagList(
      p(class = "hint", sprintf("Loaded %s rows; %s match ticked samples.", nrow(extra), n_match)),
      if (length(cols)) {
        checkboxGroupInput(
          "adv_clin_cols", "Use these columns",
          choices = stats::setNames(cols, cols),
          selected = intersect(sel, cols),
          inline = TRUE
        )
      } else {
        p(class = "hint", "No numeric columns detected (need several finite values and at least two unique numbers).")
      }
    )
  })

  observeEvent(input$tissue_type, {
    t <- as.character(input$tissue_type)[1]
    if (is.null(t) || !nzchar(t)) return()
    tissue_choice(t)
  }, ignoreInit = TRUE)

  observeEvent(input$adv_tissue, {
    t <- as.character(input$adv_tissue)[1]
    if (is.null(t) || !nzchar(t)) return()
    tissue_choice(t)
    mt <- isolate(input$adv_max_percent_mt)
    mt_n <- suppressWarnings(as.numeric(mt)[1])
    if (identical(t, "liver") && (!is.finite(mt_n) || mt_n <= 10)) {
      updateNumericInput(session, "adv_max_percent_mt", value = 50)
    } else if (identical(t, "pbmc") && is.finite(mt_n) && mt_n == 50) {
      updateNumericInput(session, "adv_max_percent_mt", value = 10)
    }
  }, ignoreInit = TRUE)

  apply_advanced_from_input <- function() {
    p <- collect_advanced_from_input()
    advanced_cfg(p)
    tissue_choice(p$tissue)
    p
  }

  prepare_run_overlay <- function(od) {
    tissue <- isolate(tissue_choice())
    if (!is.null(input$tissue_type) && nzchar(as.character(input$tissue_type)[1])) {
      tissue <- as.character(input$tissue_type)[1]
      tissue_choice(tissue)
    }
    adv <- isolate(advanced_cfg())
    if (!isTRUE(run_needs_config_overlay(adv, tissue))) return("")
    p <- overlay_params_for_run(adv, tissue)
    write_advanced_config(file.path(od, "logs", "scauto_advanced.yaml"), p)
  }

  observeEvent(input$adv_confirm, {
    apply_advanced_from_input()
    removeModal()
    showNotification("Advanced settings confirmed. They will be used when you start the analysis.", type = "message", duration = 6)
  })

  observeEvent(input$adv_run_local, {
    apply_advanced_from_input()
    removeModal()
    request_start(jump_to_run = TRUE, keep_advanced = TRUE)
  })

  observeEvent(input$adv_run_hpc, {
    apply_advanced_from_input()
    removeModal()
    est <- current_mem_check()
    if (is.null(est) || !isTRUE(est$valid)) {
      showNotification("Tick samples first so HPC can estimate resources.", type = "error")
      return()
    }
    show_hpc_modal(est)
  })

  observeEvent(input$confirm_start_run, {
    begin_pipeline()
  })

  observeEvent(input$cancel_start_run, {
    start_pending(FALSE)
    start_lock(FALSE)
    removeModal()
  })

  kill_pipeline_process <- function(px, od) {
    pid <- NA_integer_
    if (!is.null(px)) {
      pid <- tryCatch(px$get_pid(), error = function(e) NA_integer_)
      tryCatch(px$kill_tree(), error = function(e) NULL)
      still <- tryCatch(isTRUE(px$is_alive()), error = function(e) FALSE)
      if (isTRUE(still)) tryCatch(px$kill(), error = function(e) NULL)
    }
    pf <- file.path(od, "logs", "pipeline.pid")
    if ((!is.finite(pid) || pid < 2L) && file.exists(pf)) {
      pid <- suppressWarnings(as.integer(readLines(pf, n = 1L, warn = FALSE)[[1]]))
    }
    if (is.finite(pid) && pid > 1L) {
      tryCatch(tools::pskill(pid, tools::SIGKILL), error = function(e) NULL)
    }
  }

  force_stop_pipeline <- function() {
    if (!isTRUE(isolate(run_alive())) && is.null(isolate(proc()))) {
      showNotification("Nothing is running.", type = "warning")
      return(invisible(FALSE))
    }
    px <- isolate(proc())
    run_stopped(TRUE)
    run_alive(FALSE)
    set_stop_enabled(FALSE)
    took <- capture_run_elapsed()
    took_line <- if (!identical(took, "unknown")) paste0(" This run took ", took, ".") else ""
    od <- isolate(outdir())
    lf <- file.path(od, "logs", "pipeline.log")
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    extra <- paste0(
      "\n========== STOPPED BY USER ==========\n",
      stamp, " Force Stop: all calculation was killed. Partial files may remain.",
      took_line, "\n"
    )
    if (nzchar(od) && dir.exists(dirname(lf))) {
      cat(extra, file = lf, append = TRUE)
    }
    kill_pipeline_process(px, od)
    if (identical(isolate(run_mode()), "hpc")) {
      kill_hpc_watcher(od)
      s <- isolate(hpc_settings())
      tryCatch(
        hpc_qdel(s$user, s$host, read_hpc_job_id(od)),
        error = function(e) NULL
      )
    }
    proc(NULL)
    run_exit(9L)
    log_stamp(isolate(log_stamp()) + 1L)
    showNotification(
      paste0("Stopped. All calculation was forced to quit. This run took ", took, "."),
      type = "warning",
      duration = 8
    )
    showModal(modalDialog(
      title = NULL,
      div(
        class = "stop-banner",
        style = "margin:0;",
        h3("Stopped by you"),
        p(tags$b("All calculation was forced to stop.")),
        if (!identical(took, "unknown")) p(tags$b("This run took "), took, "."),
        p("Partial files may already be in the results folder. You can download what is there, or click Start analysis to run again.")
      ),
      footer = modalButton("OK"),
      easyClose = TRUE,
      size = "m"
    ))
    invisible(TRUE)
  }

  ask_force_stop <- function() {
    if (!isTRUE(run_alive()) && is.null(proc())) {
      showNotification("Nothing is running.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Stop all calculation?",
      tagList(
        p(tags$b("This will immediately kill the analysis on this computer.")),
        p("Partial files may remain in the results folder. You can download what is already there, or start again later."),
        p(class = "hint", "The run continues until you click Confirm stop.")
      ),
      footer = tagList(
        modalButton("Keep running"),
        actionButton("confirm_stop_run", "Confirm stop", class = "btn-stop btn")
      ),
      easyClose = TRUE,
      size = "m"
    ))
  }

  observeEvent(input$stop_run, ask_force_stop())
  observeEvent(input$stop_run_always, ask_force_stop())

  observeEvent(input$confirm_stop_run, {
    removeModal()
    force_stop_pipeline()
  })

  observeEvent(input$open_out, {
    od <- outdir()
    if (!nzchar(od) || !dir.exists(od)) {
      showNotification("There is no results folder yet.", type = "warning")
      return()
    }
    open_folder(od)
  })

  output$dl_10x_file <- downloadHandler(
    filename = function() {
      path <- selected_10x()
      if (is.null(path) || !nzchar(path)) "file.bin" else basename(path)
    },
    content = function(file) {
      path <- selected_10x()
      req(path, file.exists(path))
      file.copy(path, file, overwrite = TRUE)
    },
    contentType = NA
  )

  output$dl_10x_sample <- downloadHandler(
    filename = function() {
      s <- selected_sample()
      if (is.null(s) || !nzchar(s)) "sample_10x.zip" else paste0(s, "_10x.zip")
    },
    content = function(file) {
      d <- discovered()
      s <- selected_sample()
      req(isTRUE(d$ok), s, s %in% names(d$dirs))
      zip_folder(d$dirs[[s]], file)
    },
    contentType = "application/zip"
  )

  output$dl_clin_template <- downloadHandler(
    filename = "sample_clinical_numeric_template.csv",
    content = function(file) {
      m <- tryCatch(collect_meta(), error = function(e) meta_df())
      extra <- isolate(clinical_extra_df())
      if (!is.null(extra) && nrow(extra) && "sample" %in% names(extra)) {
        utils::write.csv(extra, file, row.names = FALSE)
        return()
      }
      samp <- if (!is.null(m) && nrow(m)) as.character(m$sample) else c("sampleB", "sampleA")
      tmpl <- data.frame(
        sample = samp,
        Age = NA_real_,
        LabValue1 = NA_real_,
        stringsAsFactors = FALSE
      )
      utils::write.csv(tmpl, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$dl_meta_template <- downloadHandler(
    filename = "sample_meta_template.csv",
    content = function(file) {
      m <- meta_df()
      if (is.null(m) || !nrow(m)) {
        m <- data.frame(sample = "SAMPLE_ID", analysis_name = "SAMPLE_ID", group = "GroupName", stringsAsFactors = FALSE)
      } else {
        m <- data.frame(
          sample = m$sample,
          analysis_name = if ("analysis_name" %in% names(m)) m$analysis_name else m$sample,
          group = "",
          stringsAsFactors = FALSE
        )
        for (cc in isolate(extra_group_cols())) m[[cc]] <- ""
      }
      utils::write.csv(m, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$dl_meta_current <- downloadHandler(
    filename = "sample_meta.csv",
    content = function(file) {
      m <- tryCatch(collect_meta(), error = function(e) meta_df())
      req(m)
      drop <- intersect("n_barcodes", names(m))
      if (length(drop)) m <- m[, setdiff(names(m), drop), drop = FALSE]
      utils::write.csv(m, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$dl_hpc_pack <- downloadHandler(
    filename = function() {
      paste0("scauto_hpc_pack_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      keep <- kept_samples()
      if (!length(keep)) {
        d <- discovered()
        if (!is.null(d) && isTRUE(d$ok)) keep <- names(d$dirs)
      }
      req(length(keep) > 0)
      m <- tryCatch(collect_meta(), error = function(e) meta_df())
      groups <- if (!is.null(m) && nrow(m) && "group" %in% names(m)) {
        as.character(m$group[match(keep, m$sample)])
      } else {
        rep("", length(keep))
      }
      groups[is.na(groups)] <- ""
      est <- current_mem_check()
      if (is.null(est)) {
        n_cells <- {
          d <- discovered()
          if (!is.null(d) && isTRUE(d$ok) && length(keep)) {
            sum(d$n_barcodes[keep], na.rm = TRUE)
          } else {
            NA_real_
          }
        }
        est <- memory_check(length(keep), n_cells, ram_gb = host_ram_cache)
      }
      od <- isolate(outdir())
      if (!is.null(input$outdir_path) && nzchar(input$outdir_path)) od <- input$outdir_path
      write_hpc_bundle(
        dest_zip = file,
        scauto_root = scauto_root,
        project = project,
        input_dir = isolate(input_dir()),
        samples = keep,
        groups = groups,
        est = est,
        outdir_hint = od,
        meta = if (!is.null(m)) {
          drop <- intersect("n_barcodes", names(m))
          if (length(drop)) m[, setdiff(names(m), drop), drop = FALSE] else m
        } else {
          NULL
        }
      )
    },
    contentType = "application/zip"
  )

  output$dl_zip <- downloadHandler(
    filename = function() {
      od <- outdir()
      if (!nzchar(od)) "scauto_results.zip" else paste0(basename(od), ".zip")
    },
    content = function(file) {
      od <- outdir()
      req(nzchar(od), dir.exists(od))
      zip_folder(od, file)
    },
    contentType = "application/zip"
  )
}

shiny::shinyApp(ui, server)
