// Paste into a Google Sheet: Extensions → Apps Script.
// Then Deploy → New deployment → Type: Web app
//   Execute as: Me
//   Who has access: Anyone
// Copy the web-app URL into tools/scauto/config/usage_report.json → "url".
//
// Optional: set NOTIFY_EMAIL to get a mail on every run.

var NOTIFY_EMAIL = ""; // e.g. "you@example.com"
var SHEET_NAME = "usage";

function doPost(e) {
  var data = {};
  try {
    data = JSON.parse(e.postData.contents);
  } catch (err) {
    return ContentService.createTextOutput("bad json").setMimeType(ContentService.MimeType.TEXT);
  }
  var ss = SpreadsheetApp.getActive();
  var sh = ss.getSheetByName(SHEET_NAME);
  if (!sh) {
    sh = ss.insertSheet(SHEET_NAME);
    sh.appendRow([
      "time", "event", "user", "host", "os", "mode",
      "n_samples", "n_cells_before_qc", "n_cells_raw", "n_cells_qc",
      "samples", "outdir", "status"
    ]);
  }
  sh.appendRow([
    data.time || "",
    data.event || "",
    data.user || "",
    data.host || "",
    data.os || "",
    data.mode || "",
    data.n_samples || "",
    data.n_cells_before_qc || "",
    data.n_cells_raw || "",
    data.n_cells_qc || "",
    data.samples || "",
    data.outdir || "",
    data.status || ""
  ]);
  if (NOTIFY_EMAIL) {
    MailApp.sendEmail({
      to: NOTIFY_EMAIL,
      subject: "scauto " + (data.event || "event") + " · " + (data.user || "?") +
        " · " + (data.n_samples || "?") + " samples",
      body: JSON.stringify(data, null, 2)
    });
  }
  return ContentService.createTextOutput("ok").setMimeType(ContentService.MimeType.TEXT);
}

function doGet() {
  return ContentService.createTextOutput("scauto usage endpoint").setMimeType(ContentService.MimeType.TEXT);
}
