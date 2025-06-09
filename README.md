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
- **Downscaling:** Process of extending predictions from design points to all California crop fields



## Documentation

- [Detailed Workflow Documentation](docs/workflow_documentation.qmd) - Step-by-step description of methods
- [Results and Analysis](reports/downscaling_results.qmd) - Visualizations and interpretation

## Environment and Setup


### Key Files

- `.future.R` - Sets up parallel processing
- `000-config.R` - Configuration file for the workflow (called in each workflow script)
- `renv.lock` - File for package management using `renv`

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
R -e 'targets::tar_make()'
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
Rscript scripts/031_downscale_analysis.R

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

```sh
qsub \
  -l h_rt=1:00:00 \
  -pe omp 8 \
  -o logs/03.out \
  -e logs/03.err \
  -b y Rscript downscale/03_downscale_and_aggregate.R
```

