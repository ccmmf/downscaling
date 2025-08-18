---
title: "Downscaling Workflow Documentation"
author: "David LeBauer"
date: "`r Sys.Date()`"
format:
  html:
    self-contained: true
    embed-resources: true
    toc: true
execute:
  echo: false
---

# Workflow Overview

The downscaling workflow predicts carbon pools (Soil Organic Carbon and Aboveground Biomass) for cropland fields in California and then aggregates these predictions to the county scale.

It uses an ensemble-based approach to uncertainty propagation and analysis, maintaining ensemble structure to propagate errors through the prediction and aggregation processes.

![Spatial downscaling workflow using machine learning with environmental covariates](figures/spatial_downscaling_workflow.png){width="5in"}

## Terminology

- **Design Points**: Fields chosen via stratified random sampling using k-means clustering on environmental data layers across California crop fields.
- **Crop Fields**: All croplands in the LandIQ dataset.
- **Anchor Sites:** Sites used as ground truth for calibration and validation, including UC research stations and Ameriflux sites.

## This Repository Contains Two Workflows That Will Be Split

 TODO **Workflows (1 & 3) are in this repository and will be split**

The workflows are

1. **Site Selection**: uses environmental variables (later also management layers) to create clusters and then select representative sites. The design_points.csv are then passed to the ensemble workflow  
2. Ensemble in ccmmf/workflows repository, generates ensemble outputs
3. **Downscaling**: uses ensemble outputs to make predictions for each field in CA then aggregate to county level summaries.

## Workflow Steps

### Configuration

Workflow settings are configured in `000-config.R`, except that the CCMMF_DIR is set in `.Renviron`. 
<!-- TODO: check why CCMMF_DIR is handled separately; 
     I think there were two related motivations
        1. .Renviron vars can be overridden by `export CCMMF_DIR=...` 
        2. renv directories are set there, using the CCMMF_DIR variable. 
           but also not sure why those wouldn't be in there.
--->

The configuration script reads the CCMMF directory from the environment variable `CCMMF_DIR` (set in .Renviron), and uses it to define paths for inputs and outputs.

The `CCMMF_DIR` variable, however, is defined in `.Renviron` so that it can:
- Be used to locate and override R library (`RENV_PATHS_LIBRARY`) and cache (`RENV_PATHS_CACHE`) paths outside the home directory.
- Be easily overridden via a shell export (`export CCMMF_DIR=…`) without modifying workflow scripts.

#### Configuration setup


To set up this workflow to run on your system, follow the following steps.

**Clone Repository**

```sh
git clone git@github.com:ccmmf/downscaling
```

  - `CCMMF_DIR` should point to the shared CCMMF directory. 
    This is the location where data, inputs, and outputs will be stored, 
    as well as the location of the `renv` cache and library
  - `RENV_PATHS_CACHE` and `RENV_PATHS_LIBRARY` store the `renv` cache and library in the CCMMF directory.
    These are in a subdirectory of the CCMMF directory in order to make them available across all users 
    (and because on some computers, they exceed allocated space in the home directory).
  - `R_LIBS_USER` must point to the platform and R version specific subdirectory inside `RENV_PATHS_LIBRARY`.  
    Example: `/projectnb/dietzelab/ccmmf/renv-library/linux-almalinux-8.10/R-4.4/x86_64-pc-linux-gnu`
- `.Rprofile`
  - sets repositories from which R packages are installed
  - runs `renv/activate.R`
- `000-config.R`
  - set `pecan_outdir` based on the CCMMF_DIR.
  - confirm that relative paths (`data_raw`, `data`, `cache`) are correct.
  - detect and use resources for parallel processing (with future package); default is `available cores - 1`
  - PRODUCTION mode setting. For testing, set `PRODUCTION` to `FALSE`. This is _much_ faster and requires fewer computing resources because it subsets large datasets. Once a test run is successful, set `PRODUCTION` to `TRUE` to run the full workflow.

**Others:**

_these shouldn't need to be changed unless you want to change the default behavior of the workflow_

- `renv.lock` is used for package management with `renv`. 
See [project renv setup docs](renv_setup.md) for instructions about using `renv` for these workflows. 
See [renv package documentation](https://rstudio.github.io/renv/articles/renv.html) for more details.

<!-- 
**UdUnits dependency**

If you get an error installing the units package, this - or something similar - may help. 
We are working on an alternative to renv that will bundle system dependencies and hopefully make this and related challenges unnecessary.
install units package

```r
install.packages(
  "units",
  configure.args = "--with-udunits2-lib=/share/pkg.8/udunits/2.2.28/install/lib --with-udunits2-include=/share/pkg.8/udunits/2.2.28/install/include"
)
```
-->

### 1. Data Preparation

```sh
Rscript scripts/009_update_landiq.R
Rscript scripts/010_prepare_covariates.R
Rscript scripts/011_prepare_anchor_sites.R
```

These scripts prepare data for clustering and downscaling:

- Converts LandIQ-derived shapefiles to a geopackage with geospatial information and a CSV with other attributes
- Extracts environmental covariates (clay, organic carbon, topographic wetness, temperature, precipitation, solar radiation, vapor pressure)
- Groups fields into Cal-Adapt climate regions
- Assigns anchor sites to fields

**Inputs:**

- **LandIQ Crop Map**: `data_raw/i15_Crop_Mapping_2016_SHP/i15_Crop_Mapping_2016.shp`
- **Soilgrids**: `clay_0-5cm_mean.tif` and `ocd_0-5cm_mean.tif`
  Consider aggregating 0-5,5-15,15-30 into a single 0-30 cm layer 
- **TWI**: `TWI/TWI_resample.tiff`
- **ERA5 Met Data**: Files in `GridMET/` folder named `ERA5_met_<YYYY>.tiff`
- **Anchor Sites**: `data_raw/anchor_sites.csv`

**Outputs:** 

- `ca_fields.gpkg`: Spatial information from LandIQ
- `ca_field_attributes.csv`: Site attributes including crop type
- `site_covariates.csv`: Environmental covariates for each field
- `anchor_sites_ids.csv`: Anchor site information


**Environmental Covariates**

| Variable | Description               | Source       | Units   |
| -------- | ------------------------- | ------------ | ------- |
| temp     | Mean annual temperature   | ERA5         | °C      |
| precip   | Mean annual precipitation | ERA5         | mm/year |
| srad     | Solar radiation           | ERA5         | W/m²    |
| vapr     | Vapor pressure deficit    | ERA5         | kPa     |
| clay     | Clay content              | SoilGrids    | %       |
| ocd      | Organic carbon density    | SoilGrids    | g/kg    |
| twi      | Topographic wetness index | SRTM-derived | -       |

### 2. Design Point Selection

```sh
Rscript scripts/020_cluster_and_select_design_points.R
Rscript scripts/021_clustering_diagnostics.R
```

Uses k-means clustering to select representative fields plus anchor sites:

- Subsample LandIQ fields and include anchor sites for clustering
- Select cluster number based on the Elbow Method
- Cluster fields using k-means based on environmental covariates
- Select design points from clusters for SIPNET simulation

**Inputs:**
- `data/site_covariates.csv`
- `data/anchor_sites_ids.csv`

**Output:** 
- `data/design_points.csv`

### 3. SIPNET Model Runs

A separate workflow prepares inputs and runs SIPNET simulations for the design points.

**Inputs:**
- `design_points.csv`
- Initial conditions (from modeling workflow)

**Outputs:**
- `out/ENS-<ensemble_number>-<site_id>/YYYY.nc`: NetCDF files containing SIPNET outputs, in PEcAn standard model output format. 
- `out/ENS-<ensemble_number>-<site_id>/YYYY.nc.var`: List of variables included in SIPNET output (see table below)

 **Available Variables**

Each output file named `YYYY.nc` has an associated file named `YYYY.nc.var`. 
This file contains a list of variables included in the output.
SIPNET outputs have been converted to PEcAn standard units and stored in PEcAn standard 
NetCDF files. 
PEcAn standard units are SI, following the Climate Forecasting standards: 

- Mass pools: kg / m2
  - TotSoilCarb: Total Soil Carbon
  - AbvGrndWood: Above ground woody biomass
- Mass fluxes: kg / m2 / s-1
  - GPP: Gross Primary Productivity
  - NPP: Net Primary Productivity
- Energy fluxes: W / m2
- Other: 
   - LAI: m2 / m2

 | Variable                     | Description                          |
 | ---------------------------- | ------------------------------------ |
 | GPP                          | Gross Primary Productivity           |
 | NPP                          | Net Primary Productivity             |
 | TotalResp                    | Total Respiration                    |
 | AutoResp                     | Autotrophic Respiration              |
 | HeteroResp                   | Heterotrophic Respiration            |
 | SoilResp                     | Soil Respiration                     |
 | NEE                          | Net Ecosystem Exchange               |
 | AbvGrndWood                  | Above ground woody biomass           |
 | leaf_carbon_content          | Leaf Carbon Content                  |
 | TotLivBiom                   | Total living biomass                 |
 | TotSoilCarb                  | Total Soil Carbon                    |
 | Qle                          | Latent heat                          |
 | Transp                       | Total transpiration                  |
 | SoilMoist                    | Average Layer Soil Moisture          |
 | SoilMoistFrac                | Average Layer Fraction of Saturation |
 | SWE                          | Snow Water Equivalent                |
 | litter_carbon_content        | Litter Carbon Content                |
 | litter_mass_content_of_water | Average layer litter moisture        |
 | LAI                          | Leaf Area Index                      |
 | fine_root_carbon_content     | Fine Root Carbon Content             |
 | coarse_root_carbon_content   | Coarse Root Carbon Content           |
 | GWBI                         | Gross Woody Biomass Increment        |
 | AGB                          | Total aboveground biomass            |
 | time_bounds                  | history time interval endpoints      |


### 4. Extract SIPNET Output

```sh
Rscript scripts/030_extract_sipnet_output.R
```

Extracts and formats SIPNET outputs for downscaling:

- Extract output variables (AGB, TotSoilCarb) from SIPNET simulations
- Aggregate site-level ensemble outputs into long and 4D array formats
- Save CSV and NetCDF files following EFI standards

**Inputs:**
- `out/ENS-<ensemble_number>-<site_id>/YYYY.nc`

**Outputs:**
- `out/ensemble_output.csv`: Long format data

### 5. Downscale and Aggregate SIPNET Output

```sh
Rscript scripts/040_downscale_and_aggregate.R
Rscript scripts/041_downscale_analysis.R
```

Builds Random Forest models to predict carbon pools for all fields:

- Train models on SIPNET ensemble runs at design points
- Use environmental covariates to downscale predictions to all fields
- Aggregate to county-level estimates
- Output maps and statistics of carbon density and totals

**Inputs:**
- `out/ensemble_output.csv`: SIPNET outputs
- `data/site_covariates.csv`: Environmental covariates

**Outputs:** 
- `out/county_total_AGB.png`: County-level AGB predictions
- `out/county_total_TotSoilCarb.png`: County-level SOC predictions
- `out/county_summaries.csv`: County statistics

## Technical Reference

### Ensemble Structure

Each ensemble member represents a plausible realization given parameter and meteorological uncertainty. This ensemble structure is maintained throughout the workflow to properly propagate uncertainty. For example, downscaling is done for each ensemble member separately, and then the results are aggregated to county-level statistics.



# References

**EFI Standards**

Dietze, Michael C., R. Quinn Thomas, Jody Peters, Carl Boettiger, Gerbrand Koren, Alexey N. Shiklomanov, and Jaime Ashander. 2023. "A Community Convention for Ecological Forecasting: Output Files and Metadata Version 1.0." Ecosphere 14 (11): e4686. https://doi.org/10.1002/ecs2.4686.

**Data Sources**

Land IQ, LLC. California Crop Mapping (2014). California Department of Water Resources, 2017. https://data.cnra.ca.gov/dataset/statewide-crop-mapping.

Hengl, T. et al. 2017. "SoilGrids250m: Global Gridded Soil Information Based on Machine Learning." PLoS ONE 12(2): e0169748. https://doi.org/10.1371/journal.pone.0169748

Hersbach, H. et al. 2020. "The ERA5 Global Reanalysis." Quarterly Journal of the Royal Meteorological Society 146: 1999–2049. https://doi.org/10.1002/qj.3803

**Models**

Braswell, Bobby H., William J. Sacks, Ernst Linder, and David S. Schimel. 2005. "Estimating Diurnal to Annual Ecosystem Parameters by Synthesis of a Carbon Flux Model with Eddy Covariance Net Ecosystem Exchange Observations." Global Change Biology 11 (2): 335–55. https://doi.org/10.1111/j.1365-2486.2005.00897.x.

Liaw, Andy, and Matthew Wiener. 2002. "Classification and Regression by randomForest." R News 2 (3): 18–22. https://CRAN.R-project.org/doc/Rnews/.
