
# Downscaling Workflow 

<p><a href="https://www.repostatus.org/#wip"><img src="https://www.repostatus.org/badges/latest/wip.svg" alt="repo status: WIP"></a></p>

## Overview

This workflow estimates carbon pools (SOC and AGB) for California crop fields and aggregates to the county level.

Key components:

- Environmental covariates (ERA5, SoilGrids, TWI)
- Design point selection via k-means
- SIPNET simulations at design points [done externally]
- Random Forest downscaling to all fields
- County-level aggregation

Configuration: see `000-config.R` for paths, variables, and parallel settings.

## Quick start

```bash
# Load modules (geo cluster example)
module load R/4.4.0 gdal proj geos sqlite udunits quarto

# Point to the shared CCMMF directory (or set in .Renviron)
export CCMMF_DIR=/projectnb/dietzelab/ccmmf      # or $HOME/ccmmf-dev

git clone https://github.com/ccmmf/downscaling.git
cd downscaling

# Restore exact packages for this workflow
R -e 'if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv"); renv::restore()'
```

### Run Sequence

See full details in . Typical sequence:

```bash
# Data prep and clustering
Rscript scripts/010_prepare_covariates.R
Rscript scripts/011_prepare_anchor_sites.R
Rscript scripts/020_cluster_and_select_design_points.R
Rscript scripts/021_clustering_diagnostics.R

# Extract SIPNET outputs and create mixed-PFT scenarios
Rscript scripts/030_extract_sipnet_output.R
Rscript scripts/031_aggregate_sipnet_output.R

# Downscale and aggregate
Rscript scripts/040_downscale.R
Rscript scripts/041_aggregate_to_county.R

# Analysis and figures
Rscript scripts/042_downscale_analysis.R
Rscript scripts/043_county_level_plots.R
```
