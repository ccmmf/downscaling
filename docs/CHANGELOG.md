# Change Log

All notable changes are kept in this file. 
All changes made should be added to the section called `Unreleased`. 
Once a new release is made this file will be updated to create a new `Unreleased` section for the next release.

For more information about this file see also [Keep a Changelog](http://keepachangelog.com/) .


sections to include in release notes:

## [Unreleased]

### Added

- Management scenario support for comparing agricultural practices
  - Configuration flag `USE_PHASE_3_SCENARIOS` in `000-config.R`
  - Six scenarios: baseline, compost, reduced_till, zero_till, reduced_irrig_drip, stacked
  - Scenario-aware extraction from pre-aggregated `.Rdata` files
  - `scenario` column added to all output CSVs
  - `scenarios` array in metadata JSON
- Early exit in `031_aggregate_sipnet_output.R` for single-PFT runs
- Parquet output for large files (>100k rows) via `write_output()` helper

### Fixed

- Empty vector handling in mixed-scenario logic (`040_downscale.R`)

### Changed

### Removed



## 0.2.0-2a

### Added

- Centralized configuration in `000-config.R` for setting input/output directories and controlling whether to run workflow in production vs dev/testing modes.
- `.Renviron` and `.Rprofile` for reproducible environmental setup and R package repository config.
- R package dependency management with renv.
- Combined woody and herbaceous crop design points into a single `design_points.csv` with a new `pft` field.
- Added LandIQ data summaries in `009_update_landiq.R`.
- Added partial dependency plots for analysis of downscaling model.
- Organized inputs and outputs into a configurable location (relative to `CCMMF_DIR`) outside the repository for better reproducibility.
- More consistent variable names and logging throughout the codebase.
- This CHANGELOG!

### Fixed

- Improved reproducibility and environment consistency via new config and profile files.
- Numerous bug fixes, including issues with the herbaceous crop downscaling process and code style including more consistent use of `pkg::function()` syntax.

### Changed

- Updated documentation, comments, and logging for improved workflow clarity.
- Renamed and renumbered workflow files to make order more clear and allow for splitting scripts using 010, 011, 020 naming convention.
- Separated documentation and results generation into separate directories.
- Updated county-level aggregation and plotting in `040_downscale_and_aggregate.R`.
- Refactored code to add associated conditionals for "fast" vs "production" (slow) runs to support new dev vs production mode setting.
- Updated data handling: renamed `data_raw/anchor_sites.csv` to `data_raw/anchor_site_locations.csv`, and `data/anchor_sites_ids.csv` to `anchor_sites.csv` (as recommended in workflows#2).
- Moved downscaling code from https://github.com/ccmmf/workflows/releases/tag/v0.1.1 into this repository.

### Removed

- Legacy code and data formats related to previous crop types and outdated workflow stages.

## v0.1.1-1b

### Added

- First draft of code to select design points using environmental covariates and anchor sites.
- Data preparation scripts to extract environmental covariates for each LandIQ parcel and join anchor sites to LandIQ parcels.
- Design point selection integrating k-means clustering and anchor site merging.
- Extraction of output from multi-site ensemble SIPNET runs.
- Downscaling SIPNET output to fields using Random Forest, and aggregation of predictions to county-level summaries.
- Documentation and output summaries in `04_downscaling_documentation_results.qmd` (see HTML report attached to release).
- Core functionalities to support the MVP of Phase 1b (Scale up Statewide Perennial Woody Crop Inventory).

### Notes

- Workflow is not expected to run end-to-end without manual setup; ensure PEcAn packages are installed from the correct branches and use provided input data on geo.bu.edu.
