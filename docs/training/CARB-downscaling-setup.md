# Environment setup

Conda, the AWS CLI and the S3 profile are set up once for all of MAGiC, and are
documented in
[CARB PEcAn Environment Setup](https://github.com/ccmmf/magic-training/blob/main/CARB-PEcAn-setup.md).
Follow that first. This page covers only what is specific to downscaling.

## Clone the repository

```bash
git clone https://github.com/ccmmf/downscaling.git
cd downscaling
```

The rest of the instructions assume that you are working inside of the downscaling
repository directory.

## Every new shell

Return to the downscaling repository directory and activate conda before continuing.

```bash
cd /path/to/downscaling
conda activate <your pecan-all env>
export AWS_PROFILE=magic
```

## Your config file

Copy the commented example and edit that copy, so you can always diff against it. The
demo bundle ships a filled-in config, so for the training you will not need to write one
from scratch.

```bash
cp example_user_config.yaml my-config.yaml
```

`./magic-downscaling --help` lists every key and its default. The three modes differ in
scope rather than quality: `production` covers all four variables and all scenarios,
`dev` and `demo` reduce the design size and variable list, and `demo` also drops to a
single scenario. A `demo` run exercises the same code path as `production` on less data.

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
