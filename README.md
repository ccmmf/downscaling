[![](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

# Downscaling Workflow

## Overview

The downscaling workflow predicts carbon pools (SOC and AGB) for California crop fields and aggregates predictions to the county level. It uses ensemble-based uncertainty propagation via SIPNET model runs and Random Forest downscaling.

**Key components:**
- Environmental covariate extraction from multiple sources (ERA5, SoilGrids, TWI)
- Design point selection using k-means clustering
- SIPNET model runs at design points
- Random Forest models to downscale from design points to all fields
- County-level aggregation of carbon estimates

### Concepts

- **Anchor sites:** Fields with ground truth validation data (e.g., Ameriflux sites)
- **Design points:** Representative fields selected for SIPNET simulation based on environmental clustering
- **Downscaling:** Process of extending predictions from design points to all California crop fields.

## Environment and Setup

### Key Configuration Files

- `.future.R` - Sets up parallel processing
- `000-config.R` - Configuration file for the workflow (called in each workflow script)

## Quick start

```bash
# Load modules (geo cluster example)
module load R/4.4.0 gdal proj geos sqlite udunits quarto

# Decide where the shared CCMMF area lives (or set in .Renviron)
export CCMMF_DATA_DIR=/projectnb/dietzelab/ccmmf      # or $HOME/ccmmf-dev

git clone https://github.com/ccmmf/downscale.git
cd downscale

# Restore exact packages for this workflow
R -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv"); renv::restore()'

# Run the pipeline [Not implemented]
# R -e 'targets::tar_make()'
```

## Running the Workflow

The workflow consists of these main scripts:

```bash
# 1. Prepare environmental covariates
Rscript scripts/010_prepare_covariates.R

# 2. Assign anchor sites to fields
Rscript scripts/011_prepare_anchor_sites.R

# 3. Select design points via clustering
Rscript scripts/020_cluster_and_select_design_points.R

# 4. Cluster diagnostics and visualization
Rscript scripts/021_clustering_diagnostics.R

# 5. Extract SIPNET output from model runs
Rscript scripts/030_extract_sipnet_output.R  

# 6. Downscale and aggregate to county level
Rscript scripts/040_downscale_and_aggregate.R

# 7. Downscale analysis and interpretation
Rscript scripts/041_downscale_analysis.R

# 8. Generate results documentation
quarto render reports/downscaling_results.qmd
```

## Advanced Setup and Use

### Interactive Session on BU Cluster

```sh
# On geo.bu.edu:
qrsh -l h_rt=3:00:00 -pe omp 16 -l buyin
```

### Job Submission

This is an example of how a script can be run on an HPC

```sh
qsub \
  -l h_rt=1:00:00 \
  -pe omp 8 \
  -o logs/03.out \
  -e logs/03.err \
  -b y Rscript downscale/999_workflow_step.R
```

### Building Documentation Site with Quarto

**Preview**

```bash
quarto preview
```

**Build**

```bash
quarto render
```

#### Publish to GitHub Pages (gh-pages)

These steps will publish to https://ccmmf.github.io/downscaling.

This does not commit compiled HTML to `main`. Instead, Quarto pushes the built site to a separate `gh-pages` branch.

One-time setup in GitHub:
1. Settings → Pages → Set Source to “Deploy from a branch”.
2. Select Branch: `gh-pages` and Folder: `/ (root)`.

Publish from your machine after rendering:

```bash
# Build site locally (runs R code and embeds results)
quarto render

# Publish the built _site/ to gh-pages
quarto publish gh-pages
```

### Adding Content

- Add any `.qmd` or `.md` file
- Add links in `_quarto.yml`
  - under `project.render`
  - in the appropriate section under `website.navbar.left`.


#### Notes on Quarto Configuration

- **Global HTML Settings:** 
  - `self-contained: true`, `embed-resources: true`, `df-print: paged`, `toc: true`.

- **Execution:**
  - Uses `freeze: auto` to cache outputs; re-executes only when inputs change.
  - Some pages (e.g., `reports/downscaling_results.qmd`) depend on paths in `000-config.R`. Build locally where paths exist.

- **Configuration:**
  - Quarto settings are in `_quarto.yml` (generates standalone HTML).
  - Does not use GitHub Actions due to reliance on large local datasets.

- **Home Page:**
  - Defined in `index.qmd` (includes this `README.md`).
