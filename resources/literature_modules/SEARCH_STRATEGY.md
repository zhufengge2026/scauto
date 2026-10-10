# Literature retrieval strategy — CD8 / MAIT / γδT modules (this project PBMC)

**Window:** calendar years **2023–2026** (inclusive), plus retained foundational human PBMC atlases (Hao 2021, Zheng 2021 TIL for Tex only).  
**Updated:** 2026-09-07

## Goal

Build/refresh curated gene modules that are valid for **human peripheral-blood** CD8 conventional + unconventional (MAIT, γδT) single-cell analyses.

## Inclusion (must meet all)

1. **Species:** *Homo sapiens* (primary data human; dual-species OK only if human blood panels are used).
2. **Compartment:** peripheral blood / PBMC as a primary sampling site (matched blood±tissue OK if blood programs are annotated).
3. **Assay:** scRNA-seq ± CITE-seq / scTCR-seq (not mouse bulk / not microarray-only for curated compact panels).
4. **Biology:** CD8 αβ, MAIT, and/or γδT states, programs, or aging/differentiation trajectories.
5. **Date:** published (or journal-accepted online) in **2023–2026** for *new* curated modules.

## Exclusion

| Exclude | Reason |
|---------|--------|
| Mouse-only or mouse-primary aging/infection atlases (e.g. Mogilenko 2021 primary) | Not transferable as primary PBMC modules |
| Mouse LCMV / infection MSigDB C7 (e.g. Sarkar GSE10239) | Wrong species |
| Mouse-heavy TIL atlases as sole citation (ProjecTILs) | Prefer human consensus (Kotliar 2025) |
| Tumor-only TIL programs without circulating counterpart (CXCL13⁺ Tex, ENTPD1⁺) | Not PBMC-appropriate |
| Reviews without original scRNA gene panels | Use as pointers only |

## Query blocks (run separately; union hits)

```
# Block A — CD8 blood aging / memory
("CD8" OR "CD8+") AND (PBMC OR "peripheral blood" OR "human blood")
AND (scRNA-seq OR "single-cell RNA" OR CITE-seq OR "single cell")
AND (GZMK OR TEMRA OR GNLY OR KLRC2 OR "effector memory" OR inflammaging OR aging)
AND (2023:2026[dp])

# Block B — CD8 state atlases
("CD8" OR "CD8+") AND (atlas OR "reference map" OR "cell states")
AND (human) AND (scRNA-seq OR "single-cell")
AND (exhaust* OR Temra OR "innate-like" OR CMC1)
AND (2023:2026[dp])

# Block C — MAIT
(MAIT OR "mucosal-associated invariant") AND (human)
AND (blood OR PBMC OR liver) AND (scRNA-seq OR "single-cell")
AND (2023:2026[dp])

# Block D — γδT
(("gamma delta" OR γδ OR "gammadelta" OR TRDC OR Vδ2 OR "Vd2") AND T)
AND (human) AND (blood OR PBMC OR tissue) AND (scRNA-seq OR "single-cell")
AND (2023:2026[dp])

# Block E — consensus programs
("T cell" AND (program OR module OR "gene expression program") AND consensus)
AND (human) AND (single-cell) AND (2024:2026[dp])
```

Sources: PubMed / Europe PMC / Crossref; journal TOC scan of *Immunity, Nat Immunol, Sci Immunol, Nat Methods, Cell, Nature*.

## Screening checklist (per paper)

- [ ] Human primary data?
- [ ] Blood/PBMC sampled (or blood states explicitly defined)?
- [ ] Named CD8 / MAIT / γδ states with marker genes?
- [ ] Markers are human gene symbols usable in 10x PBMC?
- [ ] Not redundant with an existing module (merge citations if same biology)?

## Hit → module rule

- Prefer **compact panels (6–12 genes)** for scoring heatmaps.
- One biological program → one `module_id`; cite the **best human-blood** paper as primary `ref_id`.
- Cross-cite related 2023–2026 papers in `references*.csv` notes.

## Inventory file

See `literature_inventory_2023_2026.csv` in this folder for screened hits and include/exclude decisions.
