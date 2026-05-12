# Design Point Validation

Generated: 2026-05-12 11:48:02
Total design points: 1000
Seed: 42
Features: temp, precip, srad, vapr, clay, ocd, twi, eof_1, eof_10, eof_2, eof_3, eof_4, eof_5, eof_6, eof_7, eof_8, eof_9, leafon_doy, leafoff_doy, leafon_doy_sd, leafoff_doy_sd, tillage_rank, tillage_freq, irr_canopy, irr_flood

## PFT allocation

|pft                  |  n_pop| area_pop| pct_pop_n| pct_pop_area| n_design| pct_design|
|:--------------------|------:|--------:|---------:|------------:|--------:|----------:|
|woody perennial crop | 329401|  1763197|      59.9|         48.1|      480|         48|
|annual crop          | 220859|  1906107|      40.1|         51.9|      520|         52|

## Feature space coverage (MSSD, scaled)

|pft                  |  n_pop| n_design|    mssd|
|:--------------------|------:|--------:|-------:|
|annual crop          | 220859|      520|  9.7009|
|woody perennial crop | 329194|      480| 11.4785|

## Cluster balance (Gini of cluster sizes)

|pft                  | n_clusters| min_size| median_size| max_size|  gini|
|:--------------------|----------:|--------:|-----------:|--------:|-----:|
|annual crop          |       5195|        1|          32|      382| 0.443|
|woody perennial crop |       4805|        1|          42|      683| 0.520|

## Anchor recovery

| n_anchors| in_design| missing|
|---------:|---------:|-------:|
|        25|        23|       2|

## Monitoring layer coverage

|feature        |  pop_n| pop_pct| design_n| design_pct|
|:--------------|------:|-------:|--------:|----------:|
|tillage_rank   | 285674|    51.9|      605|       60.5|
|tillage_freq   | 286083|    52.0|      605|       60.5|
|leafon_doy     | 550053|   100.0|     1000|      100.0|
|leafoff_doy    | 550053|   100.0|     1000|      100.0|
|leafon_doy_sd  | 550053|   100.0|     1000|      100.0|
|leafoff_doy_sd | 550053|   100.0|     1000|      100.0|
|irr_canopy     | 498829|    90.7|      719|       71.9|
|irr_flood      |  32739|     5.9|      173|       17.3|

## Climate region coverage

| climregion_id|  n_pop| pct_pop| n_design| pct_design|
|-------------:|------:|-------:|--------:|----------:|
|             1|  55220|    10.0|       48|        4.8|
|             2|  18990|     3.5|      130|       13.0|
|             3| 166898|    30.3|      255|       25.5|
|             4|   5698|     1.0|       18|        1.8|
|             5|  16699|     3.0|       40|        4.0|
|             6|   7614|     1.4|       19|        1.9|
|             7|  73774|    13.4|      132|       13.2|
|             8|  41279|     7.5|       81|        8.1|
|             9|  16903|     3.1|       24|        2.4|
|            10|   3195|     0.6|       15|        1.5|
|            11| 143990|    26.2|      238|       23.8|

## Figures

![Variable importance (eta-squared)](../figures/cluster_variable_importance.svg)

![Design points on California cropland](../figures/design_points.webp)

![Nearest-neighbor distances](../figures/nn_distance.svg)

![CDF overlay (annual crop)](../figures/cdf_overlay_annual_crop.webp)

![PCA scatter (annual crop)](../figures/pca_scatter_annual_crop.webp)

![Sorted cluster sizes (annual crop)](../figures/cluster_sizes_annual_crop.webp)

![CDF overlay (woody perennial crop)](../figures/cdf_overlay_woody_perennial_crop.webp)

![PCA scatter (woody perennial crop)](../figures/pca_scatter_woody_perennial_crop.webp)

![Sorted cluster sizes (woody perennial crop)](../figures/cluster_sizes_woody_perennial_crop.webp)

