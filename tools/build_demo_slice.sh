#!/bin/bash

# Build the demo slice tarball for the downscaling pipeline.
# Ships only what 030 -> 043 reads under --mode demo:
#   - site_info.csv
#   - output_baseline/pecan.CONFIGS.xml
#   - output_baseline/run/runs.txt          (full file, all entries kept)
#   - output_baseline/out/ENS-00001-<site>/<DEMO_YEAR>.nc   (one per site)
#
# Usage: build_demo_slice.sh <source_modelout_dir> [output_tarball]
#   <source_modelout_dir>  path to a phase-3 modelout directory
#   [output_tarball]       defaults to ccmmf_phase_3_scenarios_v2_n2o_ch4_demo.tgz in CWD
#
# Env: DEMO_YEAR (default 2024)

SRC=${1:?usage: $0 <source_modelout_dir> [output_tarball]}
OUT=${2:-ccmmf_phase_3_scenarios_v2_n2o_ch4_demo.tgz}
DEMO_YEAR=${DEMO_YEAR:-2024}
NAME="ccmmf_phase_3_scenarios_v2_n2o_ch4"

if [[ ! -d "$SRC/output_baseline/out" ]]; then
    echo "$0: $SRC/output_baseline/out not found" >&2
    exit 1
fi

STAGE=$(mktemp -d "${TMPDIR:-/tmp}/demo_slice.XXXXXX")
trap "rm -rf '$STAGE'" EXIT

DST="$STAGE/$NAME"
mkdir -p "$DST/output_baseline/run" "$DST/output_baseline/out"

cp "$SRC/site_info.csv"                       "$DST/site_info.csv"
cp "$SRC/output_baseline/pecan.CONFIGS.xml"   "$DST/output_baseline/pecan.CONFIGS.xml"
cp "$SRC/output_baseline/run/runs.txt"        "$DST/output_baseline/run/runs.txt"

n=0
for d in "$SRC/output_baseline/out/"ENS-00001-*; do
    nc="$d/${DEMO_YEAR}.nc"
    [[ -f "$nc" ]] || continue
    site=$(basename "$d")
    mkdir -p "$DST/output_baseline/out/$site"
    cp "$nc" "$DST/output_baseline/out/$site/${DEMO_YEAR}.nc"
    n=$((n + 1))
done

echo "Copied ${DEMO_YEAR}.nc from $n ENS-00001-* dirs"

tar czf "$OUT" -C "$STAGE" "$NAME"

echo "Built: $OUT"
ls -lh "$OUT"
echo "Total entries: $(tar tzf "$OUT" | wc -l)"
