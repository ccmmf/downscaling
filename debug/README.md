# Debug Scripts

This folder contains small, one-off diagnostics that help inspect data alignment and downscaling behavior. They are safe to keep versioned and should be runnable without modifying repository files (except where noted).

## Contents

1) debug_site_ids.R
- Purpose: sanity-check mismatches among site_id lists across inputs.
- Reads: 000-config.R; data/design_points.csv; data/anchor_sites.csv; data/site_covariates.csv; model_outdir/ensemble_output.csv; data/ca_fields.gpkg.
- Writes: none (console logs only).
- Run: Rscript debug/debug_site_ids.R

2) debug_downscale_production_issue.R
- Purpose: compare dev vs PRODUCTION downscaling behavior; probes ensemble label issues and predictor problems without editing package code.
- Reads: 000-config.R; model_outdir/ensemble_output.csv; data/site_covariates.csv; data/ca_field_attributes.csv.
- Env overrides (optional): DEBUG_POOL, DEBUG_PFT.
- Writes: none (console logs only).
- Run: Rscript debug/debug_downscale_production_issue.R

3) patch_design_point_covariates.R
- Purpose: synthesize covariate rows for design points that don’t exist in site_covariates.csv; useful for older analysis workflows that require design-point covariates.
- Status: optional. scripts/042_downscale_analysis.R now reads site_covariates.csv directly, so you generally do NOT need to run this.
- Reads: 000-config.R; model_outdir/ensemble_output.csv; data/site_covariates.csv; data/ca_fields.gpkg.
- Writes: data/site_covariates_design_points_patched.csv and data/site_covariates_plus_dp_patch.csv.
- Run: Rscript debug/patch_design_point_covariates.R

## Conventions
- Keep reproducible, read-only diagnostics here in debug/.
- Put personal scratch or exploratory notebooks in a local scratch/ (gitignored) rather than committing them.
- Large outputs (figures, rendered sites) are intentionally ignored by .gitignore.

## Tips
- If a script depends on cluster paths or long-running data, run on an interactive node and capture logs to a file, e.g.:
  Rscript debug/debug_downscale_production_issue.R | tee logs/debug_prod_vs_dev.log

