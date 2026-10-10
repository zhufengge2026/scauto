# Data directory

This GitHub repository does **not** include 10x count matrices or clinical tables.

Add your own Cell Ranger sample folders here (or point the helper at any folder that contains them). Each sample folder should contain:

- `matrix.mtx` (or `matrix.mtx.gz`)
- `barcodes.tsv` (or `.gz`)
- `features.tsv` or `genes.tsv` (or `.gz`)

Then provide a metadata CSV with a `sample` column matching those folder names. See:

- `docs/HOW_TO_PREPARE_DATA.txt`
- `tools/scauto/examples/sample_meta.smoke.csv`
- `tools/scauto/examples/sample_clinical_numeric_template.csv` (dummy numbers only)
