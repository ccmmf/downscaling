
# Downscaling And Aggregation Workflow

::: {.callout-warning}
This proof of concept is untested and subject to change. Interpret results as illustrative.
:::


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

See full details about how to set up and run the workflows in the [Technical Documentation](docs/workflow_documentation.md#sec-tech-doc). 

The workflow runs as numbered scripts in three stages: data preparation and design-point selection (`009`..`022`), SIPNET output extraction (`030`..`031`), and downscaling with aggregation and figures (`040`..`043`). The Technical Documentation linked above lists the full command sequence with the inputs and outputs of each step.
