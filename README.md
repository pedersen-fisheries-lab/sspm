
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sspm <img src='man/figures/logo.png' align="right" height="150" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT/)
[![R-CMD-check](https://github.com/pedersen-fisheries-lab/sspm/workflows/R-CMD-check/badge.svg)](https://github.com/pedersen-fisheries-lab/sspm/actions)
[![Codecov test
coverage](https://codecov.io/gh/pedersen-fisheries-lab/sspm/branch/main/graph/badge.svg)](https://codecov.io/gh/pedersen-fisheries-lab/sspm?branch=main)
<!-- [![Downloads](https://cranlogs.r-pkg.org/badges/sspm?color=brightgreen)](https://CRAN.R-project.org/package=sspm/)
[![Latest Release](https://img.shields.io/github/v/release/pedersen-fisheries-lab/sspm?label=Latest%20Release)](https://github.com/pedersen-fisheries-lab/sspm/releases/latest)
[![CRAN Version](https://img.shields.io/cran/v/sspm?label=CRAN%20Version)](https://CRAN.R-project.org/package=sspm)
[![GitHub Version](https://img.shields.io/github/r-package/v/pedersen-fisheries-lab/sspm?label=GitHub%20Version)](https://github.com/pedersen-fisheries-lab/sspm/blob/dev/DESCRIPTION) -->
<!-- badges: end -->

The goal of `sspm` is to implement a gam-based spatial surplus
production model, aimed at modeling northern shrimp population in Canada
but potentially to any stock in any location. The package is opinionated
in its implementation of SPMs as it internally makes the choice to use
penalized spatial gams with time lags based on Pedersen et al. (2020).
However, it also aims to provide options for the user to customize their
model.

## Installation

<!-- You can install the released version of sspm from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("sspm") -->
<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pedersen-fisheries-lab/sspm")
```

## Example

The following example shows the typical `sspm` workflow. The API is
subject to changes as the package is still in development.

Let’s first load the test data.

``` r
library(sspm)
#> Loading required package: sf
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.
library(mgcv)

borealis <- sspm:::borealis_simulated
predator <- sspm:::predator_simulated
catch <- sspm:::catch
sfa_boundaries <- sspm:::sfa_boundaries
```

1.  Start by creating a base `sapspm` object with a base dataset called
    “Biomass”.

``` r
sspm_base <- sspm(model_name = "My Model",
                  boundaries = sfa_boundaries) %>% 
  map_biomass(borealis, "borealis", 
              time_column = "year_f",
              coords = c('lon_dec','lat_dec'),
              uniqueID = "uniqueID")
#> !  Warning: sspm is assuming that the CRS of boundaries is to be used for casting
#> ℹ  Casting data matrix into simple feature collection using columns: lon_dec, lat_dec
sspm_base
#> 
#> ‒‒ SSPM object 'My Model' ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Datasets      : 1 dataset
#>    ٭ borealis (biomass) — [1541 observations, 18 variables]
```

2.  Then, discretize the object using a method for discretization, the
    default behind voronoi tesselation. See `?spm_methods()` for the
    list of methods available.

``` r
sspm_discrete <- sspm_base %>%
  spm_discretize(with_dataset = "borealis", 
                 discretization_method = "tesselate_voronoi")
#> ℹ  Discretizing using method tesselate_voronoi with dataset borealis
sspm_discrete
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 1 dataset
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
```

The results of the discretization can be explored with `spm_patches()`
and `spm_points()`.

``` r
spm_patches(sspm_discrete)
#> Simple feature collection with 69 features and 3 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -64.5 ymin: 46.00004 xmax: -46.6269 ymax: 61
#> geographic CRS: WGS 84
#> # A tibble: 69 x 4
#>    sfa   patch_id                                              geometry area_km2
#>  * <chr> <fct>                                            <POLYGON [°]>    <dbl>
#>  1 4     V1       ((-64.4227 60.27125, -64.5 60.30667, -64.5 60.31667,…   20236.
#>  2 4     V2       ((-63 60.55308, -63 61, -62.96667 61, -62.93333 61, …   14675.
#>  3 4     V3       ((-59.91648 58.8388, -59.9052 58.80785, -59.9468 58.…    4127.
#>  4 4     V4       ((-59.88688 57.66667, -59.9 57.66667, -59.93333 57.6…    2742.
#>  5 4     V5       ((-62.00478 61, -62 61, -61.96667 61, -61.93333 61, …    5560.
#>  6 4     V6       ((-59.95436 58.6478, -59.9052 58.61004, -59.79427 58…    1515.
#>  7 4     V7       ((-61.86024 57.86301, -61.86024 57.87204, -61.94343 …    3819.
#>  8 4     V8       ((-61.37285 57.66667, -61.4 57.66667, -61.43333 57.6…    4376.
#>  9 4     V9       ((-61.36857 58.4628, -61.66155 59.11637, -60.6936 58…    2702.
#> 10 4     V10      ((-60.25027 59.52148, -60.18251 59.45455, -60.16864 …    2445.
#> # … with 59 more rows
spm_points(sspm_discrete)
#> Simple feature collection with 75 features and 18 fields
#> geometry type:  POINT
#> dimension:      XY
#> bbox:           xmin: -61.77175 ymin: 46.37211 xmax: -48.19061 ymax: 59.70288
#> geographic CRS: WGS 84
#> # A tibble: 75 x 19
#> # Groups:   sfa [4]
#>     year vessel  trip div_nafo season area_swept_km2 year_f n_samples lon_dec
#>  * <dbl>  <dbl> <dbl> <chr>    <chr>           <dbl> <fct>      <dbl>   <dbl>
#>  1  1995     39    21 3K       Fall           0.0250 1995           5   -56.4
#>  2  1996     39    37 2G       Fall           0.0250 1996           5   -53.0
#>  3  1996     39    37 2G       Fall           0.0250 1996           5   -61.8
#>  4  1998     39    73 2H       Fall           0.0250 1998           5   -56.1
#>  5  1998     39    73 2J       Fall           0.0343 1998           5   -53.2
#>  6  1998     39    76 3L       Fall           0.0250 1998           5   -53.7
#>  7  1999     39    86 2J       Fall           0.0281 1999           5   -57.8
#>  8  1999     39    88 3K       Fall           0.0250 1999           5   -53.6
#>  9  2011     39    98 3K       Fall           0.0281 2011           5   -61.2
#> 10  2006     48   101 2G       Summer         0.0281 2006           5   -56.1
#> # … with 65 more rows, and 10 more variables: lat_dec <dbl>, depth <dbl>,
#> #   temp_at_bottom <dbl>, weight <dbl>, weight_per_km2 <dbl>,
#> #   recruit_weight <dbl>, row <int>, uniqueID <chr>, geometry <POINT [°]>,
#> #   sfa <chr>
```

3.  Then, other datasets may be loaded alongside the biomass dataset,
    for example, predator and catch data.

``` r
sspm_mapped <- sspm_discrete %>%
  map_predictor(predator,
                name = "pred_data",
                time_column = "year",
                uniqueID = "uniqueID",
                coords = c("lon_dec", "lat_dec")) 
#> !  Warning: sspm is assuming that the CRS of boundaries is to be used for casting
#> ℹ  Casting data matrix into simple feature collection using columns: lon_dec, lat_dec
sspm_mapped
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 2 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
```

4.  Smoothing formulas for any of the datasets may be specified, using
    special terms `smooth_time()`, `smooth_space` and
    `smooth_space_time`. NOTE: the testing dataset is dummy data and
    wont converge if `smooth_time()` or `smooth_space_time` is used (to
    be fixed soon).

``` r
sspm_with_formulas <- sspm_mapped %>%
  map_formula(weight_per_km2~smooth_time(), "borealis", ) %>% 
  map_formula(weight_per_km2~smooth_space(), "pred_data")
sspm_with_formulas
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 2 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>       – weight_per_km2 ~ smooth_time()
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
#>       – weight_per_km2 ~ smooth_space()
```

5.  Finally, formulas can be fitted with `fit_smooths()`

``` r
sspm_smooth_fitted <- sspm_with_formulas %>%
  fit_smooths()
#> ℹ  Fitting formula: weight_per_km2 ~ smooth_time() for dataset 'borealis'
#> ℹ  Fitting formula: weight_per_km2 ~ smooth_space() for dataset 'pred_data'
sspm_smooth_fitted
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 2 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_time()
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_space()
#> →  Smoothed data : [1656 observations, 8 variables]
#>    ٭ smoothed vars: borealis_smooth — pred_data_smooth
```

6.  We also need to register catch data. For catch data, it is important
    to precise a data column which will be aggregated at the `patch_id`
    level.

``` r
sspm_smooth_fitted_wcatch <-  sspm_smooth_fitted %>% 
  map_catch(catch, name = "catch_data", 
            time_column = "year_f", 
            uniqueID = "obs_id", 
            coords = c("lon_start", "lat_start"), 
            catch_column = "catch", 
            biomass_column = "borealis_smooth")
#> !  Warning: sspm is assuming that the CRS of boundaries is to be used for casting
#> ℹ  Casting data matrix into simple feature collection using columns: lon_start, lat_start
sspm_smooth_fitted_wcatch
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 3 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_time()
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_space()
#>    ٭ catch_data (catch) — [491973 observations, 13 variables]
#> →  Smoothed data : [1656 observations, 11 variables]
#>    ٭ smoothed vars: borealis_smooth — borealis_smooth_with_catch — borealis_smooth_with_catch_change — pred_data_smooth
```

6.  To fit the final SPM, we might need to add lagged values. This is
    easily done with `spm_lag`.

``` r
sspm_smooth_fitted_wcatch_lagged <- sspm_smooth_fitted_wcatch %>% 
  spm_lag(c("borealis_smooth", "pred_data_smooth"), 1)
sspm_smooth_fitted_wcatch_lagged
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 3 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_time()
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_space()
#>    ٭ catch_data (catch) — [491973 observations, 13 variables]
#> →  Smoothed data : [1656 observations, 13 variables]
#>    ٭ smoothed vars: borealis_smooth — borealis_smooth_with_catch — borealis_smooth_with_catch_change — pred_data_smooth
#>    ٭ lagged vars: borealis_smooth_lag_1 — pred_data_smooth_lag_1
```

7.  Before fitting the model, we split data into test/train.

``` r
sspm_smooth_fitted_wcatch_lagged <-
  spm_split(sspm_smooth_fitted_wcatch_lagged, year_f %in% c(1996:2000))
sspm_smooth_fitted_wcatch_lagged
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 3 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_time()
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_space()
#>    ٭ catch_data (catch) — [491973 observations, 13 variables]
#> →  Smoothed data : [1656 observations, 14 variables]
#>    ٭ smoothed vars: borealis_smooth — borealis_smooth_with_catch — borealis_smooth_with_catch_change — pred_data_smooth
#>    ٭ lagged vars: borealis_smooth_lag_1 — pred_data_smooth_lag_1
#>       – [345 train, 1311 test]
```

8.  We can now map a final spm formula onto the smoothed data

``` r
sspm_smooth_with_formulas <- sspm_smooth_fitted_wcatch_lagged %>% 
  map_formula(borealis_smooth_with_catch ~ pred_data_smooth_lag_1 + smooth_lag("borealis_smooth"))
sspm_smooth_with_formulas
#> 
#> ‒‒  SSPM object 'My Model' [DISCRETIZED]  ‒‒
#> →  Boundaries    : [4 observations, 2 variables]
#> →  Discretized   : 
#>    ٭ Points — [75 features, 19 variables]
#>    ٭ Patches — [69 features, 4 variables]
#> →  Datasets      : 3 datasets
#>    ٭ borealis (biomass) — [1026 observations, 21 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_time()
#>    ٭ pred_data (predictor) — [1979 observations, 18 variables]
#>       – (SMOOTHED) weight_per_km2 ~ smooth_space()
#>    ٭ catch_data (catch) — [491973 observations, 13 variables]
#> →  Smoothed data : [1656 observations, 14 variables]
#>    ٭ smoothed vars: borealis_smooth — borealis_smooth_with_catch — borealis_smooth_with_catch_change — pred_data_smooth
#>    ٭ lagged vars: borealis_smooth_lag_1 — pred_data_smooth_lag_1
#>       – borealis_smooth_with_catch ~ pred_data_s...
#>       – [345 train, 1311 test]
```

8.  We can now fit the SPM.

``` r
sspm_fit <- 
  sspm_smooth_with_formulas %>% fit_spm()
sspm_fit
```

9.  We can also extract predictions.

``` r
# TODO
```
