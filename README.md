
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spaspm <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/pedersen-fisheries-lab/spaspm/workflows/R-CMD-check/badge.svg)](https://github.com/pedersen-fisheries-lab/spaspm/actions)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Codecov test
coverage](https://codecov.io/gh/pedersen-fisheries-lab/spaspm/branch/main/graph/badge.svg)](https://codecov.io/gh/pedersen-fisheries-lab/spaspm?branch=main)
<!-- badges: end -->

The goal of `spaspm` is to implement a gam-based spatial surplus
production model, aimed at modeling northern shrimp population in Canada
but potentially to any stock in any location. The package is opinionated
in its implementation of SPMs as it internally makes the choice to use
penalized spatial gams with time lags based on Pedersen et al. (2020).
However, it also aims to provide options for the user to customize their
model.

## Installation

<!-- You can install the released version of spaspm from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("spaspm") -->

<!-- ``` -->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pedersen-fisheries-lab/spaspm")
```

## Example

The following example shows the typical `spaspm` workflow. The API is
subject to changes as the package is still in development.

Let’s first load the test data.

``` r
library(spaspm)
#> Loading required package: sf
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1
library(mgcv)
#> Loading required package: nlme
#> This is mgcv 1.8-33. For overview type 'help("mgcv-package")'.

borealis <- spaspm:::borealis_simulated
predator <- spaspm:::predator_simulated
sfa_boundaries <- spaspm:::sfa_boundaries
```

1.  Start by creating a base `sapspm` object with a base dataset called
    “Biomass”.

<!-- end list -->

``` r
spaspm_base <- spaspm(model_name = "My Model",
                      data = borealis,
                      name = "Biomass",
                      time_col = "year_f",
                      coords = c('lon_dec','lat_dec'),
                      uniqueID = "uniqueID",
                      boundaries = sfa_boundaries)
#> !  Warning: spaspm is assuming that the CRS of boundaries is to be used for casting
#> ℹ  Casting data matrix into simple feature collection using columns: lon_dec, lat_dec
spaspm_base
#> 
#> ── SPASPM object 'My Model' ──
#> 
#> ── Base dataset 'Biomass'
#> ●  Data matrix        : Simple feature collection with 1541 feature(s) and 18 variable(s)
#> ●  Data unique ID     : uniqueID
#> ●  Time column        : year_f
#> ●  Coordinates cols   : lon_dec, lat_dec
#> 
#> ── Boundaries
#> ●  Boundary data      : Simple feature collection with 4 feature(s) and 2 variable(s)
```

2.  Then, discretize the object using a method for discretization, the
    default behind voronoi tesselation. See `?spm_methods()` for the
    list of methods available.

<!-- end list -->

``` r
spaspm_discrete <- spaspm_base %>%
  spm_discretize(discretization_method = "tesselate_voronoi")
#> ℹ  Discretizing using method 'tesselate_voronoi'
spaspm_discrete
#> 
#> ── SPASPM object 'My Model' (DISCRETIZED) ──
#> 
#> ── Base dataset 'Biomass'
#> ●  Data matrix        : Simple feature collection with 1541 feature(s) and 21 variable(s)
#> ●  Data unique ID     : uniqueID
#> ●  Time column        : year_f
#> ●  Coordinates cols   : lon_dec, lat_dec
#> 
#> ── Boundaries
#> ●  Boundary data      : Simple feature collection with 4 feature(s) and 2 variable(s)
#> 
#> ── Discretization info
#> ●  Method name        : 'tesselate_voronoi'
#> ●  Patches            : Simple feature collection with 69 patches (and 4 field(s))
#> ●  Points             : Simple feature collection with 75 points (and 19 field(s))
```

The results of the discretization can be explored with `spm_patches()`
and `spm_points()`.

``` r
spm_patches(spaspm_discrete)
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
spm_points(spaspm_discrete)
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

3.  Then, other datasets may be mapped onto the base dataset, for
    example, predator data.

<!-- end list -->

``` r
spaspm_discrete_mapped <- spaspm_discrete %>%
  map_dataset(predator,
              name = "pred_data",
              time_col = "year",
              uniqueID = "uniqueID",
              coords = c("lon_dec", "lat_dec"))
#> ℹ  Casting data matrix into simple feature collection using columns: lon_dec, lat_dec
spaspm_discrete_mapped
#> 
#> ── SPASPM object 'My Model' (DISCRETIZED) ──
#> 
#> ── Base dataset 'Biomass'
#> ●  Data matrix        : Simple feature collection with 1541 feature(s) and 21 variable(s)
#> ●  Data unique ID     : uniqueID
#> ●  Time column        : year_f
#> ●  Coordinates cols   : lon_dec, lat_dec
#> 
#> ── Boundaries
#> ●  Boundary data      : Simple feature collection with 4 feature(s) and 2 variable(s)
#> 
#> ── Discretization info
#> ●  Method name        : 'tesselate_voronoi'
#> ●  Patches            : Simple feature collection with 69 patches (and 4 field(s))
#> ●  Points             : Simple feature collection with 75 points (and 19 field(s))
#> 
#> ── Mapped Datasets
#> ●  1 mapped dataset(s): pred_data
```

4.  Smoothing formulas for any of the datasets may be specified, using
    special terms `smooth_time()`, `smooth_space` and
    `smooth_space_time`. NOTE: the testing dataset is dummy data and
    wont converge if `smooth_time()` or `smooth_space_time` is used (to
    be fixed soon).

<!-- end list -->

``` r
spaspm_discrete_mapped_with_forms <- spaspm_discrete_mapped %>%
  map_formula("Biomass", weight_per_km2~smooth_space())
spaspm_discrete_mapped_with_forms
#> 
#> ── SPASPM object 'My Model' (DISCRETIZED) ──
#> 
#> ── Base dataset 'Biomass'
#> ●  Data matrix        : Simple feature collection with 1541 feature(s) and 21 variable(s)
#> ●  Data unique ID     : uniqueID
#> ●  Time column        : year_f
#> ●  Coordinates cols   : lon_dec, lat_dec
#> 
#> ── Boundaries
#> ●  Boundary data      : Simple feature collection with 4 feature(s) and 2 variable(s)
#> 
#> ── Discretization info
#> ●  Method name        : 'tesselate_voronoi'
#> ●  Patches            : Simple feature collection with 69 patches (and 4 field(s))
#> ●  Points             : Simple feature collection with 75 points (and 19 field(s))
#> 
#> ── Mapped Datasets
#> ●  1 mapped dataset(s): pred_data
#> 
#> ── Mapped formulas
#> 1) weight_per_km2 ~ smooth_space()... for dataset 'Biomass'
```

5.  Finally, formulas can be fitted with `fit_smooths()`

<!-- end list -->

``` r
spaspm_discrete_mapped_fitted <- spaspm_discrete_mapped_with_forms %>%
  fit_smooths()
#> ℹ Fitting formula: 1 out of 1
#> ℹ Fitting formula: weight_per_km2 ~ smooth_space() for dataset 'Biomass'
spaspm_discrete_mapped_fitted
#> [[1]]
#> 
#> Family: Tweedie(p=1.701) 
#> Link function: log 
#> 
#> Formula:
#> weight_per_km2 ~ s(patch_id, k = 30, bs = "mrf", xt = list(penalty = pen_mat_space))
#> 
#> Parametric coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  8.50419    0.02162   393.3   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>                 edf Ref.df F p-value
#> s(patch_id) 0.00818     29 0   0.627
#> 
#> R-sq.(adj) =  3.43e-06   Deviance explained = 0.000952%
#> -REML = 2350.2  Scale est. = 6.1013    n = 1026
```

6.  Last step: full SPM implementation (TBA).

<!-- end list -->

``` r
spaspm_discrete_mapped_fitted_SPM <- spaspm_discrete_mapped_fitted %>% 
  fit_spm()
```
