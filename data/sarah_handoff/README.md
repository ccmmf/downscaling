# Sarah handoff: LandIQ v4.1 SUBCLASS gap-fill kit

Built 2026-04-24 against `/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1/crops_all_years.parq`
(16,259,292 rows, 602,196 distinct parcels, years 2016 + 2018-2023).

## The deliverable: `landiq_v4.1_join_ready_lookup.csv`

A 78-row CSV mapping every distinct (CLASS, SUBCLASS) combo that actually
appears in v4.1 to a friendly crop name and three PFT schemes. Designed as a
drop-in `left_join` partner for the parquet so NA SUBCLASS rows are no longer
silently dropped on inner joins.

Columns:

- `CLASS` (chr): LandIQ top-level class code (V, C, D, T, P, G, F, R, X, U, etc.)
- `SUBCLASS` (chr, may be NA): LandIQ subclass code, NA where DWR did not assign one
- `n_parcel_years` (int): count of parcel-years with this combo across all years
- `class_name` (chr): plain-English class name
- `crop_name` (chr): plain-English subclass name, or sentinel ("Vineyards, subclass not specified") for NA SUBCLASS
- `pft_group` (chr): granular PFT (woody, row, rice, hay, idle, urban, semi-ag, non-crop)
- `ccmmf_pft` (chr): CCMMF lumping (annual crop, woody perennial crop, urban, idle, non-crop)
- `pecan_pft` (chr): PEcAn data.land PFT name (temperate.deciduous, grass, soil)

## Coverage

100% of the 4,606,772 parcel-years with non-NA CLASS in v4.1 resolve to a
non-NA crop_name and PFT after this join. Every year, every CLASS, every
SUBCLASS including NA. Zero orphan rows in the parquet.

## Source provenance

- The (CLASS, SUBCLASS) → crop_name mapping comes from
  `/projectnb/dietzelab/abv1/ccmmf/cropidentity/data-raw/inputs/landiq_crop_mapping_codes.tsv`
  (curated from the 2022 DWR Standard Land Use Legend, Remote Sensing Version PDF).
  Cropidentity treats every CLASS with NA SUBCLASS as a valid sentinel
  ("Vineyards, subclass not specified", "Citrus and subtropical, subclass not
  specified", etc.) rather than a missing value.

- The PFT scheme comes from
  `/projectnb/dietzelab/abv1/ccmmf/03_pecan/modules/data.land/data-raw/carb_landiq_crop_pft.R`
  (CCMMF authoritative PFT mapping, used by SIPNET runs).

- CCMMF lumping (woody → "woody perennial crop"; row + rice + hay → "annual
  crop") follows the in-project convention discussed in the David / Akash
  meeting on 2026-04-24.

## How to use

```r
library(arrow); library(readr); library(dplyr)
landiq <- read_parquet("crops_all_years.parq")
lookup <- read_csv("landiq_v4.1_join_ready_lookup.csv",
                   col_types = cols(SUBCLASS = col_character()))
joined <- landiq |>
  mutate(SUBCLASS = as.character(SUBCLASS)) |>
  left_join(lookup, by = c("CLASS", "SUBCLASS"))
# every parcel-year now has crop_name, pft_group, ccmmf_pft, pecan_pft
```

## Diagnostic CSVs (companions, not required for the join)

- `na_subclass_by_year_class.csv` (85 rows): per-year per-CLASS NA SUBCLASS
  counts and rates. Useful as a v1-vs-v2 validation target.
- `landiq_class_subclass_combos.csv` (78 rows): every distinct combo with
  parcel-year counts. Same combos as the join-ready lookup, no PFT info.
- `phen_missing_2020_by_class_subclass.csv` (51 rows): 2020-only breakdown
  showing which combos lost what % of parcels to the v1 phenology join.
