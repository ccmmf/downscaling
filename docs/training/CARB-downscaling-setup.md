# Environment setup

Conda, the AWS CLI and the S3 profile are set up once for all of MAGiC, and are
documented in
[CARB PEcAn Environment Setup](https://github.com/ccmmf/magic-training/blob/main/CARB-PEcAn-setup.md).
Follow that first. This page covers only what is specific to downscaling.

## Clone the repository

```bash
git clone https://github.com/ccmmf/downscaling
cd downscaling
```

The rest of the instructions assume that you are working inside of the downscaling
repository directory.

## Every new shell

Return to the downscaling repository directory and activate conda before continuing.

```bash
cd /path/to/downscaling
conda activate ~/.conda/envs/pecan-all/
export AWS_PROFILE=magic
```

## The configuration file

The downscaling CLI uses a configuration file to control the workflow.

For the tutorial, the configuration file is provided with the data inputs that will be downloaded
at the beginning of the [inventory notebook](downscaling_inventory.qmd#fetch-demo-data).

For your own runs, you can copy and edit `example_user_config.yaml` as a starting point.

`./magic-downscaling --help` lists each configuration key and its default values. 

There are three configuration modes: `demo`, `dev`, and `production` that differ in
scope : `production` covers all variables and all scenarios,
`dev` and `demo` reduce the design size and variable list, and `demo` only runs
a single scenario. 
A `demo` run exercises the same code path as `production` on less data.

## Confirm setup

```bash
./magic-downscaling --help          # CLI runs
aws s3 ls s3://carb/data_raw/       # bucket reads
```

If either fails, fix it before starting the inventory notebook.

The downscale step is heavy and belongs on a compute node rather than a login node.
Resource requirements are in the notebook.

---

**Next:** [Inventory downscaling](downscaling_inventory.qmd).
