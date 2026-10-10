# Send usage records to you (any computer)

A CSV in the user’s project copy **stays on their computer**. To see every run, wherever they opened the helper, the tool POSTs a small JSON record to a URL you control.

## One-time setup (you)

1. In Google Drive, create a spreadsheet (for example `scauto_usage`).
2. **Extensions → Apps Script**. Delete the stub. Paste `tools/scauto/usage_report_google_sheet.gs`.
3. Optional: set `NOTIFY_EMAIL` in that script to your address.
4. **Deploy → New deployment → Web app**. Execute as **Me**. Who has access: **Anyone** (so the helper can POST without a Google login). After deploy, open the URL once in your browser to authorise the script.
5. Copy the web-app URL.
6. Paste it into `tools/scauto/config/usage_report.json`:

```json
{
  "enable": true,
  "url": "https://script.google.com/macros/s/YOUR_DEPLOYMENT_ID/exec"
}
```

7. Distribute **this** copy of the helper (the JSON travels with the scripts). Every Start analysis / HPC submit on a machine with internet then appends a row to your sheet.

HPC **compute** nodes have no internet; the helper on the laptop still sends the start (and the finish after results come back).

No passwords or HPC accounts are sent. Payload: login name, computer name, OS, local vs HPC, sample count and names, cell counts, result folder name.

To pause reporting without editing scripts: `"enable": false`.
