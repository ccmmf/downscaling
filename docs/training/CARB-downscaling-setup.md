# Environment setup

**What this session is for.** Later sessions assume a working software stack, the
downscaling repo on disk, S3 access, and a config file pointed at your data. This
session gets that ready, including what to run every time you open a new shell.

Nothing here downloads model output or runs the pipeline. That starts in
[the inventory notebook](downscaling_inventory.md).

---

## 0.1 Environment (once)

The workflow runs inside the `pecan-all` conda environment, which supplies R, the PEcAn
stack and the CLI's dependencies. Use that rather than assembling one by hand.

```bash
module load awscli_v2       # site specific; skip if aws is already on PATH
export AWS_PROFILE=magic
aws s3 cp s3://carb/deploy/setup-pecan-env.sh ./
bash setup-pecan-env.sh 1.18 ~/.conda/envs/pecan-all-1.18
```

That download and install takes 30 minutes or more. Do it ahead of the session.

Activate it and confirm the two things the CLI checks for before it will run anything:

```bash
conda activate ~/.conda/envs/pecan-all-1.18
```

---

## 0.2 Clone (once)

Set `$DS_DIR` to wherever you want the repo and your runs to live.

```bash
export DS_DIR=/path/to/workdir

mkdir -p "$DS_DIR" && cd "$DS_DIR"
git clone https://github.com/ccmmf/downscaling.git
cd downscaling
```

Everything after this assumes you are inside that clone, because the CLI lives at its
root and finds the manifest beside itself.

```bash
ls magic-downscaling downscaling_manifest.yaml example_user_config.yaml
```

---

## 0.3 Every new shell

```bash
conda activate ~/.conda/envs/pecan-all-1.18   # same env as 0.1
export AWS_PROFILE=magic
export DS_DIR=/path/to/workdir                # same as 0.2
cd "$DS_DIR/downscaling"
```

The downscale step is heavy and belongs on a compute node, not a login node. Sizing and
batch submission are covered in the scale-up section of the [inventory notebook](downscaling_inventory.md); the demo runs in
the demo bundle is small enough to run interactively.

---

## 0.4 S3 access

Demo data and shared inputs live in an S3 bucket hosted at NCSA. It is not AWS, so the
profile needs an explicit endpoint.

Add to `~/.aws/credentials`:

```text
[magic]
aws_access_key_id = YOUR_KEY_ID
aws_secret_access_key = YOUR_SECRET_KEY
```

Add to `~/.aws/config`:

```text
[profile magic]
region = garage
endpoint_url = https://s3.garage.ccmmf.ncsa.cloud
```

Lock the files down and confirm the bucket reads:

```bash
chmod 600 ~/.aws/credentials ~/.aws/config
export AWS_PROFILE=magic
aws s3 ls s3://carb/data_raw/
```

If that listing fails, stop and fix it here. `get-demo-data` in the inventory notebook is the first
thing that needs it.

---

## 0.5 How the CLI is wired

Worth five minutes before you run anything, because it explains why every step takes the
arguments it does.

```bash
./magic-downscaling --help
```

Three files control the workflow, and they have clearly separated jobs:

| File | Owns | Do you edit it? |
| ---- | ---- | --------------- |
| `magic-downscaling` | Argument parsing, dependency checks, dispatch | No |
| `downscaling_manifest.yaml` | Which scripts each command runs, and every path and parameter they receive | Rarely |
| your config `.yaml` | Where your data is, which mode, how many cores | **Yes, every run** |

Open the manifest and look at the `steps` block:

```bash
less downscaling_manifest.yaml
```

Key points:

* Each command is a list of steps, and each step calls exactly one script.
* A step declares its `inputs` (files), `outputs` (files), and `params` (everything
  else) as `{cli_flag: value_key}` pairs. The map key is the literal command-line flag
  the script expects; the map value names where the value comes from.
* `inputs` and `outputs` values must be keys in the manifest's `paths` block, which are
  all relative to your `run_dir`. That indirection is what lets you move a run somewhere
  else without touching any script.
* `fixed_values` are domain constants that are not user-overridable, like the random
  seed and the CRS. `mode_params` override them per mode.
* Nothing is passed to scripts through environment variables. Every argument is explicit
  on the command line, which is why `--verbose` is useful: it echoes the exact `Rscript`
  invocation for each step.

---

## 0.6 Your config file

Copy the commented example and edit that copy. Do not edit the example in place, so you
can always diff against it.

```bash
cp example_user_config.yaml my-config.yaml
```

The keys you will set:

| Key | Meaning |
| --- | ------- |
| `global.run_dir` | Working directory for this run. Every output path is relative to it. Use the same value as your `magic-ensemble` run. |
| `global.aws_profile` | Profile for `get-demo-data`. Defaults to `magic`. |
| `downscaling.mode` | `production`, `dev`, or `demo`. Controls variables, scenarios, and design size. |
| `downscaling.n_cores` | Parallel workers. Defaults to `SLURM_JOB_CPUS_PER_NODE` if set, otherwise 1. Set it explicitly on a reserved compute session where that variable is not exported. |
| `downscaling.pecan_output_dir` | Your finished `magic-ensemble` output. Expects `output_<scenario>/` subdirectories plus `site_info.csv`. |
| `downscaling.data_layers_dir` | Spatial data layers. Must contain `site_covariates.csv`. |
| `downscaling.anchor_site_locations` | Anchor site CSV. The repo ships a default under `data_raw/`. |

What the three modes actually change:

| Mode | Design size | Variables | Scenarios |
| ---- | ----------- | --------- | --------- |
| `production` | 1000 design points, pool 10000 | TotSoilCarb, AGB, N2O_flux, CH4_flux | all six |
| `dev` | 100 design points, pool 500 | TotSoilCarb only | all six |
| `demo` | 100 design points, pool 500 | TotSoilCarb only | baseline only |

Mode is not a quality setting, it is a scope setting. A `demo` run exercises the same
code path as `production` on less data.

---

## 0.7 Confirm setup

```bash
conda activate ~/.conda/envs/pecan-all-1.18
cd "$DS_DIR/downscaling"

./magic-downscaling --help          # CLI runs
aws s3 ls s3://carb/data_raw/       # bucket reads
ls my-config.yaml                   # your config exists
```

If any of those fail, fix it before starting the inventory notebook.

---

**Next:** [Inventory downscaling](downscaling_inventory.md).

**Spine:** [tree README](README.md).
