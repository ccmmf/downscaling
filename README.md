
# Downscaling And Aggregation Workflow

## Overview

This workflow estimates carbon pools (SOC and AGB) for California crop fields and
aggregates to the county level. It's driven by `magic-downscaling`, a CLI that
invokes the numbered R scripts in `scripts/` with explicit flags, controlled through
a user YAML config file (see `example_user_config.yaml`) — that's the file you edit
to point the workflow at your own data and switch between demo/dev/production
modes. Key components:

- Environmental covariates (ERA5, SoilGrids, TWI)
- Design point selection via k-means
- SIPNET simulations at design points [run externally, via `magic-ensemble`]
- Random Forest downscaling to all fields
- County-level aggregation and diagnostic plots

## Prerequisites

- The most recent `pecan-all` conda environment.
- If running `get-demo-data`, AWS credentials configured under a profile named
  `magic`.

## Quick start: demo workflow

The demo runs the full pipeline — `get-demo-data` through `analyze` — against a
small, pre-packaged dataset, driven by the checked-in `example_user_config.yaml`
(`downscaling.mode: demo`).

```bash
git clone https://github.com/ccmmf/downscaling.git
cd downscaling

# Activate your pecan-all conda environment first, e.g.:
conda activate pecan-all-1.16

./magic-downscaling get-demo-data --config example_user_config.yaml   # downloads demo-data/ (gitignored cache)
./magic-downscaling prepare       --config example_user_config.yaml   # stages demo-data/ into the run_dir
./magic-downscaling extract       --config example_user_config.yaml   # reads SIPNET output, reshapes to EFI format
./magic-downscaling downscale     --config example_user_config.yaml   # Random Forest downscaling + county aggregation
./magic-downscaling analyze       --config example_user_config.yaml   # diagnostics, uncertainty, plots
```

Each command reads/writes under `global.run_dir` (`demo-run-dir/` in the example
config) and prints its own progress; run any command with `--verbose` to also echo
the underlying `Rscript` invocations. `example_user_config.yaml` is commented with
what each key does — copy it as the starting point for a real config, updating:

- `downscaling.mode` — set to `production`
- `downscaling.pecan_output_dir` — your PEcAn/SIPNET ensemble output
- `downscaling.data_layers_dir` — your spatial data layers
- `downscaling.anchor_site_locations` — your anchor site locations CSV

Run `./magic-downscaling --help` at any time for the authoritative, current list of
commands and config keys.

## Commands

| Command         | What it does |
|-----------------|--------------|
| `get-demo-data` | Downloads and extracts the demo data bundle to `./demo-data/` (relative to your invocation directory). Run once, before `prepare`. |
| `prepare`       | Stages ensemble output and spatial data layers into `global.run_dir`. Run after `magic-ensemble run-ensembles`, before `extract`. |
| `extract`       | Reads SIPNET output, reshapes to EFI format, aggregates by scenario. |
| `downscale`     | Random Forest downscaling to all LandIQ fields; aggregates to county. |
| `analyze`       | Diagnostic summaries, uncertainty quantification, plots. |

## Full technical background

For the underlying science and data flow (covariate sources, model details,
aggregation methodology), see the
[Technical Documentation](docs/workflow_documentation.md#sec-tech-doc). Note that
document predates the `magic-downscaling` CLI and describes the workflow in terms
of running the numbered scripts directly — for how to actually run the pipeline,
use this README and `./magic-downscaling --help`.
