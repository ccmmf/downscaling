# california statewide phenology v2.0

eight parquets in this directory, one per year:

```
phenology_statewide_2016.parquet  529,311 rows
phenology_statewide_2017.parquet  335,723 rows  (new in v2)
phenology_statewide_2018.parquet  557,089 rows
phenology_statewide_2019.parquet  561,182 rows
phenology_statewide_2020.parquet  543,954 rows
phenology_statewide_2021.parquet  532,658 rows
phenology_statewide_2022.parquet  529,742 rows
phenology_statewide_2023.parquet  539,226 rows
```

total 4,128,885 rows. v1 was 2,076,215 across 7 years. roughly 2x.

## schema

```
site_id           string    LandIQ parcel_id (matches v1)
year              double
leafonday         string    "YYYY-MM-DD"
leafoffday        string    "YYYY-MM-DD"
phenology_source  string    OBSERVED | TEMPORAL_FILL | STATISTICAL_FILL | LITERATURE_FILL
```

four columns from v1 plus one new `phenology_source`. filter
`WHERE phenology_source = 'OBSERVED'` for the strict v1 subset, byte-for-byte
identical to the previous release.

## what v2 changes

### 1. NA SUBCLASS no longer drops parcels

before joining LandIQ to MSLSP, every NA SUBCLASS row gets resolved:
keep the observed SUBCLASS if present; otherwise look up the dominant
CDL code in the parcel polygon and accept it when the CDL CLASS matches
LandIQ CLASS; otherwise fall back to the ag-class default table
(D-NA -> D10, C-NA -> C7, etc.). vineyards and idle keep `**` as the
documented sentinel.

non-ag classes (U, NR, UL) are not in scope and don't appear in the
deliverable.

### 2. multi-cycle rows preserved exactly

every v1 row passes through unchanged. for 2020 the cycle distribution
is 241,613 single + 32,780 double in v1 and 241,613 + 32,780 in v2
OBSERVED. byte-exact across all 7 v1 years.

### 3. four-method gap-fill for parcels MSLSP didn't retrieve

priority order:

1. **TEMPORAL_FILL** - same parcel had MSLSP retrieval in another year
   within +/-3 yr AND same SUBCLASS. donor month-day rebased to target
   year. winter-cycle structure preserved by carrying donor's year
   offset (so a wheat donor with leafon Oct yr1 / leafoff Jun yr2
   rebases to Oct target / Jun target+1).

2. **STATISTICAL_FILL** - median DOY of v1 retrievals at the same
   (year, county, CLASS, SUBCLASS), N >= 30 floor. seeds use OBSERVED
   rows only and within-year + GSL > 0 only, so winter cycles don't
   pollute the median pool. falls back to statewide subclass median,
   then statewide CLASS median.

3. **LITERATURE_FILL** - vineyards. MSLSP retrieval rate on V is below
   0.1% (sub-pixel trellis breaks logistic-curvature retrieval).
   for V parcels with no v1, assign DOY 90 leafon and DOY 305 leafoff,
   statewide mid-range from published vine life-cycle references. the
   162 v1 V retrievals across all years are kept as OBSERVED.

zero unresolved ag rows after the cascade.

### 4. 2017 covered via CDL-derived parcel inventory

v2 reads a CDL-derived 2017 parcel set into stage 1. there's no MSLSP
for 2017, so all 2017 rows go straight through the gap-fill cascade.
result: 335,723 rows in 2017, none labeled OBSERVED, all
TEMPORAL_FILL / STATISTICAL_FILL / LITERATURE_FILL.

## how the years stack up

| year | v1 rows  | v2 rows  | added   | OBSERVED | TEMPORAL | STATISTICAL | LITERATURE |
|------|----------|----------|---------|----------|----------|-------------|------------|
| 2016 |  255,035 |  529,311 | 274,276 |  255,035 |    1,065 |     170,502 |    102,709 |
| 2017 |        0 |  335,723 | 335,723 |        0 |  124,724 |     157,877 |     53,122 |
| 2018 |  310,411 |  557,089 | 246,678 |  310,411 |    5,149 |     155,616 |     85,913 |
| 2019 |  315,990 |  561,182 | 245,192 |  315,990 |    2,903 |     156,601 |     85,688 |
| 2020 |  307,173 |  543,954 | 236,781 |  307,173 |    3,484 |     152,427 |     80,870 |
| 2021 |  292,962 |  532,658 | 239,696 |  292,962 |    5,370 |     155,382 |     78,944 |
| 2022 |  292,073 |  529,742 | 237,669 |  292,073 |    4,729 |     155,978 |     76,962 |
| 2023 |  302,571 |  539,226 | 236,655 |  302,571 |    1,837 |     158,290 |     76,528 |

2,076,215 OBSERVED rows preserved verbatim. 2,052,670 newly filled
rows added across the 8 years.

## coverage vs LandIQ v4.1

for 2020 LandIQ has 602,196 distinct parcels with non-NA CLASS. v2
covers 511,174 of those. the 91,022 not in v2 are the non-ag categories
(U / NR / UL) plus idle (X / I) and immature young perennials (YP),
which don't get a phenology value by design. zero phantom site_ids
(every v2 site_id exists in LandIQ).

## known limits

- 2017 is entirely gap-fill. treat 2017 phen as inferred, not measured.
  the medians it relies on come from the high-quality 2018+ retrievals,
  so it sits roughly at the same confidence as the STATISTICAL_FILL
  rows in adjacent years.

- vineyard literature is one statewide value (DOY 90 / DOY 305).
  per-AVA refinement requires a paywalled phenology dataset to defend
  specific per-region DOYs; on the roadmap, not in v2.

- citrus is evergreen. MSLSP "leafonday" / "leafoffday" for citrus
  represent the timing of the strongest annual greenness curvature, not
  true leaf-on / leaf-off. interpret with that caveat.

- the 162 v1 vineyard OBSERVED rows are kept verbatim. they're <0.03%
  of all V parcels but they're real source data and consistent with the
  upstream woody-trust convention; not overridden.

## pipeline overview

four stage pipeline: resolve subclass -> join to v1 -> gap-fill ->
export. inputs: LandIQ harmonized v4.1, the v1.0 phenology product,
USDA NASS Cropland Data Layer fractions, a curated DWR legend lookup,
and published vineyard life-cycle references.
