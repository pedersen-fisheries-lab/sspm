
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

Let’s first load the packages and the test data.

``` r
library(sspm)
#> Loading required package: sf
#> Linking to GEOS 3.9.0, GDAL 3.2.2, PROJ 7.2.1
#> Loading required package: mgcv
#> Loading required package: nlme
#> This is mgcv 1.8-38. For overview type 'help("mgcv-package")'.
library(mgcv)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:nlme':
#> 
#>     collapse
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

borealis <- sspm:::borealis_simulated
predator <- sspm:::predator_simulated %>%
  mutate(year = as.factor(year))
catch <- sspm:::catch_simulated

sfa_boundaries <- sspm:::sfa_boundaries
```

1.  The first step of the `sspm` workflow is to create a `sspm_boundary`
    from an `sf` object, providing the `boundary_column` that delineates
    the boundary regions. The object can then be plotted with `spm_plot`
    (as can most `sspm` objects).

``` r
bounds <- spm_as_boundary(boundaries = sfa_boundaries, 
                          boundary_column = "sfa")

spm_plot(bounds)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

2.  The second step consists in wrapping a `data.frame`, `tibble` or
    `sf` object into a `sspm_data` object, with a few other pieces of
    relevant information, such as the name, dataset type (biomass,
    predictor or catch, depending on the type of information contained),
    time column and coordinates column (i not `sf`) and unique row
    identifier. Here we wrap the borealis dataset that contains the
    biomass information.

``` r
biomass_dataset <- 
  spm_as_dataset(borealis, name = "borealis", 
                 time_column = "year_f",
                 coords = c('lon_dec','lat_dec'), uniqueID = "uniqueID")
#> ℹ  Casting data matrix into simple feature collection using columns: lon_dec, lat_dec
#> !  Warning: sspm is assuming WGS 84 CRS is to be used for casting

biomass_dataset
#> 
#> ‒‒ SSPM Dataset: borealis ‒‒
#> →  Data              : [1541 observations, 18 variables]
#> →  Data unique ID    : uniqueID
#> →  Time col.         : year_f
#> →  Coordinates cols. : lon_dec, lat_dec
```

3.  We do the same with the predator data, which are of the predictor
    type.

``` r
predator_dataset <- 
  spm_as_dataset(predator, name = "all_predators", 
                 time_column = "year",
                 uniqueID = "uniqueID", coords = c("lon_dec", "lat_dec"))
#> ℹ  Casting data matrix into simple feature collection using columns: lon_dec, lat_dec
#> !  Warning: sspm is assuming WGS 84 CRS is to be used for casting

predator_dataset
#> 
#> ‒‒ SSPM Dataset: all_predators ‒‒
#> →  Data              : [4833 observations, 15 variables]
#> →  Data unique ID    : uniqueID
#> →  Time col.         : year
#> →  Coordinates cols. : lon_dec, lat_dec
```

4.  The `sspm` workflow relies on the discretization of the boundary
    objects, the default method being voronoi tesselation.

``` r
bounds_voronoi <- bounds %>% 
  spm_discretize(method = "tesselate_voronoi",
                 with = biomass_dataset, 
                 nb_samples = 10)
#> ℹ  Discretizing using method tesselate_voronoi

bounds_voronoi
#> 
#> ‒‒ SSPM Boundary (Discrete) ‒‒
#> →  Boundaries         : [4 observations, 3 variables]
#> →  Boundary col.      : sfa
#> →  Boundary area col. : area_sfa
#> →  Discretized        : 
#>    ٭ Points — [40 features, 20 variables]
#>    ٭ Patches — [38 features, 4 variables]
```

The other available method is `triangulate_delaunay` for delaunay
triangulation. Here the `a` argument is used to set the size of the mesh
(see `RTriangle::triangulate` for more details).

``` r
bounds_delaunay <- bounds %>% 
  spm_discretize(method = "triangulate_delaunay", a = 1, q = 30)
#> ℹ  Discretizing using method triangulate_delaunay
```

5.  Plotting the object shows the polygons that have been created.

``` r
spm_plot(bounds_voronoi)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
spm_plot(bounds_delaunay)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

6.  The results of the discretization can also be explored with
    `spm_patches()` and `spm_points()`.

``` r
spm_patches(bounds_voronoi)
#> Simple feature collection with 38 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -64.5 ymin: 46.00004 xmax: -46.6269 ymax: 61
#> Geodetic CRS:  WGS 84
#> # A tibble: 38 x 4
#>    sfa   patch_id                                              geometry     area
#>  * <chr> <fct>                                            <POLYGON [°]>   [km^2]
#>  1 4     P1       ((-64.42169 60.27125, -64.42 60.27206, -64.41666 60.… 20367.3…
#>  2 4     P2       ((-59.95566 58.64882, -60.25261 57.73692, -59.67678 …  1718.5…
#>  3 4     P3       ((-61.89804 57.6918, -61.34602 58.43681, -61.36857 5…  3882.6…
#>  4 4     P4       ((-60.50931 57.66667, -60.87958 58.41518, -61.34602 …  4684.4…
#>  5 4     P5       ((-61.34602 58.43681, -60.87958 58.41518, -60.68359 …  2687.3…
#>  6 4     P6       ((-63 60.62184, -61.66155 59.11637, -60.6936 58.9027… 14504.2…
#>  7 4     P7       ((-60.26194 59.52858, -60.73068 59.34113, -60.6936 5…  2449.3…
#>  8 4     P8       ((-59.91649 58.83888, -60.68359 58.8793, -60.87958 5…  6258.8…
#>  9 4     P9       ((-61.96379 61, -60.73068 59.34113, -60.26194 59.528…  5308.1…
#> 10 5     P10      ((-59.55703 55.21506, -59.53354 57.66667, -59.56667 … 27939.6…
#> # … with 28 more rows
spm_points(bounds_voronoi)
#> Simple feature collection with 40 features and 19 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -61.77175 ymin: 46.16256 xmax: -48.20016 ymax: 59.70288
#> Geodetic CRS:  WGS 84
#> # A tibble: 40 x 20
#> # Groups:   sfa [4]
#>     year vessel  trip div_nafo season area_swept_km2 year_f n_samples lon_dec
#>  * <dbl>  <dbl> <dbl> <chr>    <chr>           <dbl> <fct>      <dbl>   <dbl>
#>  1  1995     39    23 2J       Fall           0.0250 1995           5   -51.9
#>  2  1996     39    37 2G       Fall           0.0250 1996           5   -61.8
#>  3  2011     39    95 2J       Fall           0.0250 2011           5   -49.1
#>  4  2011     39    97 2J       Fall           0.0187 2011           5   -53.7
#>  5  2011     39    98 3K       Fall           0.0281 2011           5   -61.2
#>  6  2006     48   101 2G       Summer         0.0187 2006           5   -57.1
#>  7  2009     48   104 2G       Summer         0.0156 2009           5   -61.3
#>  8  2012     39   107 2H       Fall           0.0187 2012           5   -56.3
#>  9  2012     39   109 3K       Fall           0.0218 2012           5   -50.0
#> 10  2014     63   109 2G       Summer         0.0281 2014           5   -55.4
#> # … with 30 more rows, and 11 more variables: lat_dec <dbl>, depth <dbl>,
#> #   temp_at_bottom <dbl>, weight <dbl>, weight_per_km2 <dbl>,
#> #   recruit_weight <dbl>, row <int>, uniqueID <chr>, geometry <POINT [°]>,
#> #   sfa <chr>, area_sfa [km^2]
```

7.  The next step in this workflow is to smooth the variables to be used
    in the final `sspm` model, by using spatial-temporal smoothers, by
    passing each dataset through `spm_smooth`. Here we first smooth
    `weight_per_km2` as well as `temp_at_bottom`. Note that the boundary
    column `sfa` can be used in the formula as the data will be first
    joined to the provided boundaries.

``` r
biomass_smooth <- biomass_dataset %>%  
  spm_smooth(weight_per_km2 ~ sfa + smooth_time(k = 3, bs = "mrf") + smooth_space() + 
               smooth_space_time(k=c(NA, 30)),
             boundaries = bounds_voronoi,
             drop.unused.levels = F, family=tw, method= "fREML", keep_fit = T) %>% 
  spm_smooth(temp_at_bottom ~ smooth_time(k = 2) + smooth_space(),
             drop.unused.levels = F, family=gaussian, method= "fREML", keep_fit = T)
#> ℹ  Fitting formula: weight_per_km2 ~ sfa + smooth_time(k = 3, bs = 'mrf') + smooth_space() + smooth_space_time(k = c(NA, 30)) for dataset 'borealis'
#> ℹ  Fitting formula: temp_at_bottom ~ smooth_time(k = 2) + smooth_space() for dataset 'borealis'

biomass_smooth
#> 
#> ‒‒ SSPM Dataset: borealis ‒‒
#> →  Data (MAPPED)     : [1025 observations, 21 variables]
#> →  Data unique ID    : uniqueID
#> →  Time col.         : year_f
#> →  Coordinates cols. : lon_dec, lat_dec
#> →  Formulas          : 
#>       – weight_per_km2 ~ sfa + smooth_time(k = 3, bs = 'mrf') + smooth_space() + smooth_space_time(k = c(NA, 30))
#>       – temp_at_bottom ~ smooth_time(k = 2) + smooth_space()
#> →  Boundaries         : [4 observations, 3 variables]
#> →  Smoothed Data     : [912 observations, 8 variables]
#>    ٭ smoothed vars: temp_at_bottom_smooth — weight_per_km2_smooth
```

8.  The smoothed results for any smoothed variables (listed in “smoothed
    vars” above) can be easily plotted:

``` r
spm_plot(biomass_smooth, "weight_per_km2_smooth")
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

9.  We also smooth the `weight_per_km2` variable in the predator data.

``` r
predator_smooth <- predator_dataset %>%  
  spm_smooth(weight_per_km2 ~ smooth_time(k = 3) + smooth_space(),
             boundaries = bounds_voronoi,
             drop.unused.levels = F, family=tw, method= "fREML")
#> ℹ  Fitting formula: weight_per_km2 ~ smooth_time(k = 3) + smooth_space() for dataset 'all_predators'

predator_smooth
#> 
#> ‒‒ SSPM Dataset: all_predators ‒‒
#> →  Data (MAPPED)     : [1965 observations, 18 variables]
#> →  Data unique ID    : uniqueID
#> →  Time col.         : year
#> →  Coordinates cols. : lon_dec, lat_dec
#> →  Formulas          : 
#>       – weight_per_km2 ~ smooth_time(k = 3) + smooth_space()
#> →  Boundaries         : [4 observations, 3 variables]
#> →  Smoothed Data     : [912 observations, 7 variables]
#>    ٭ smoothed vars: weight_per_km2_smooth
```

10. Before we assemble the full model with our newly smoothed data, we
    need to deal with the catch data. We first load the dataset.

``` r
catch_dataset <- 
  spm_as_dataset(catch, name = "catch_data", 
                 time_column = "year_f", 
                 uniqueID = "uniqueID", coords = c("lon_start", "lat_start"))
#> ℹ  Casting data matrix into simple feature collection using columns: lon_start, lat_start
#> !  Warning: sspm is assuming WGS 84 CRS is to be used for casting

catch_dataset
#> 
#> ‒‒ SSPM Dataset: catch_data ‒‒
#> →  Data              : [88579 observations, 8 variables]
#> →  Data unique ID    : uniqueID
#> →  Time col.         : year_f
#> →  Coordinates cols. : lon_start, lat_start
```

11. We then need to aggregate this data. This illustrate using the
    `spm_aggregate` functions. Here we use `spm_aggregate_catch`:

``` r
biomass_smooth_w_catch <- 
  spm_aggregate_catch(biomass = biomass_smooth, 
                      catch = catch_dataset, 
                      biomass_variable = "weight_per_km2_smooth",
                      catch_variable = "catch",
                      fill = mean)
#> ℹ  Offsetting biomass with catch data using columns: weight_per_km2_smooth, catch
biomass_smooth_w_catch
#> 
#> ‒‒ SSPM Dataset: borealis ‒‒
#> →  Data (MAPPED)     : [1025 observations, 21 variables]
#> →  Data unique ID    : uniqueID
#> →  Time col.         : year_f
#> →  Coordinates cols. : lon_dec, lat_dec
#> →  Formulas          : 
#>       – weight_per_km2 ~ sfa + smooth_time(k = 3, bs = 'mrf') + smooth_space() + smooth_space_time(k = c(NA, 30))
#>       – temp_at_bottom ~ smooth_time(k = 2) + smooth_space()
#> →  Boundaries         : [4 observations, 3 variables]
#> →  Smoothed Data     : [912 observations, 13 variables]
#>    ٭ smoothed vars: temp_at_bottom_smooth — weight_per_km2_smooth
#>    ٭ vars with catch: weight_per_km2_smooth_borealis_with_catch
```

12. Once data has been smoothed, we can assemble a `sspm` model object,
    using one dataset of type biomass, one dataset of type predictor and
    (optionnaly) a dataset of type catch.

``` r
sspm_model <- sspm(biomass = biomass_smooth_w_catch, 
                   predictors = predator_smooth)
#> ℹ  Joining smoothed data from all datasets

sspm_model
#> 
#> ‒‒ SSPM Model (2 datasets) ‒‒
#> →  Smoothed Data     : [912 observations, 14 variables]
#>    ٭ smoothed vars: temp_at_bottom_smooth — weight_per_km2_smooth_all_predators — weight_per_km2_smooth_borealis
#>    ٭ vars with catch: weight_per_km2_smooth_borealis_with_catch
```

13. Before fitting the model, we must split data into test/train with
    `spm_split`.

``` r
sspm_model <- sspm_model %>% 
  spm_split(year_f %in% c(1990:2017))

sspm_model
#> 
#> ‒‒ SSPM Model (2 datasets) ‒‒
#> →  Smoothed Data     : [912 observations, 15 variables] / [874 train, 38 test]
#>    ٭ smoothed vars: temp_at_bottom_smooth — weight_per_km2_smooth_all_predators — weight_per_km2_smooth_borealis
#>    ٭ vars with catch: weight_per_km2_smooth_borealis_with_catch
```

14. To fit the model, we might be interested in including lagged values.
    This is done with `spm_lag`.

``` r
sspm_model <- sspm_model %>% 
  spm_lag(vars = c("weight_per_km2_smooth_borealis_with_catch", 
                   "weight_per_km2_smooth_all_predators"), 
          n = 1)

sspm_model
#> 
#> ‒‒ SSPM Model (2 datasets) ‒‒
#> →  Smoothed Data     : [912 observations, 17 variables] / [874 train, 38 test]
#>    ٭ smoothed vars: temp_at_bottom_smooth — weight_per_km2_smooth_all_predators — weight_per_km2_smooth_borealis
#>    ٭ vars with catch: weight_per_km2_smooth_borealis_with_catch — weight_per_km2_smooth_borealis_with_catch_lag_1
#>    ٭ lagged vars: weight_per_km2_smooth_all_predators_lag_1 — weight_per_km2_smooth_borealis_with_catch_lag_1
```

15. We can now fit the final spm model with `spm`.

``` r
sspm_model_fit <- sspm_model %>% 
  spm(log_productivity ~ 
        sfa +
        weight_per_km2_smooth_all_predators_lag_1 +
        smooth_lag("weight_per_km2_smooth_borealis_with_catch") + 
        temp_at_bottom_smooth + 
        smooth_space(), family = mgcv::scat)
#> ℹ  Fitting SPM formula: log_productivity ~ sfa + weight_per_km2_smooth_all_predators_lag_1 + smooth_lag('weight_per_km2_smooth_borealis_with_catch') + temp_at_bottom_smooth + smooth_space()

sspm_model_fit
#> 
#> ‒‒ SSPM Model Fit ‒‒
#> →  Smoothed Data     : [912 observations, 17 variables] / [874 train, 38 test]
#> →  Fit summary       : 
#> 
#> Family: Scaled t(3,0.001) 
#> Link function: identity 
#> 
#> Formula:
#> log_productivity ~ sfa + weight_per_km2_smooth_all_predators_lag_1 + 
#>     temp_at_bottom_smooth + s(lag_matrix, k = 5, m = 1, by = by_matrix) + 
#>     s(patch_id, k = 30, bs = "mrf", xt = list(penalty = pen_mat_space))
#> 
#> Parametric coefficients:
#>                                             Estimate Std. Error t value
#> (Intercept)                               -8.186e-01  3.923e-02 -20.869
#> sfa5                                       6.448e-02  1.777e-03  36.285
#> sfa6                                       7.904e-02  2.291e-03  34.504
#> sfa7                                       9.250e-02  2.757e-03  33.552
#> weight_per_km2_smooth_all_predators_lag_1  1.921e-06  6.878e-06   0.279
#> temp_at_bottom_smooth                      1.767e-03  2.847e-04   6.207
#>                                           Pr(>|t|)    
#> (Intercept)                                < 2e-16 ***
#> sfa5                                       < 2e-16 ***
#> sfa6                                       < 2e-16 ***
#> sfa7                                       < 2e-16 ***
#> weight_per_km2_smooth_all_predators_lag_1     0.78    
#> temp_at_bottom_smooth                     8.64e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Approximate significance of smooth terms:
#>                            edf Ref.df       F  p-value    
#> s(lag_matrix):by_matrix  4.932      5 505.723  < 2e-16 ***
#> s(patch_id)             16.338     29   1.895 3.03e-07 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> R-sq.(adj) =  0.884   Deviance explained = 71.2%
#> -REML = 1227.9  Scale est. = 1         n = 836
```

16. Plotting the object produces a actual vs predicted plot (with
    TEST/TRAIN data highlighted.

``` r
spm_plot(sspm_model_fit)
#> Warning: Removed 38 rows containing missing values (geom_point).
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="100%" />

17. We can also extract the predictions, including for biomass.

``` r
preds <- spm_predict(sspm_model_fit)
head(preds)
#>       pred_log     pred patch_id year_f sfa                       geometry
#> 1 0.0008397704 1.000840       P1   1995   4 POLYGON ((-64.42169 60.2712...
#> 2 0.0003481465 1.000348       P1   1996   4 POLYGON ((-64.42169 60.2712...
#> 3 0.0003242303 1.000324       P1   1997   4 POLYGON ((-64.42169 60.2712...
#> 4 0.0002014760 1.000201       P1   1998   4 POLYGON ((-64.42169 60.2712...
#> 5 0.0004777427 1.000478       P1   1999   4 POLYGON ((-64.42169 60.2712...
#> 6 0.0006854760 1.000686       P1   2000   4 POLYGON ((-64.42169 60.2712...
```

``` r
biomass_preds <- spm_predict_biomass(sspm_model_fit, "weight_per_km2_smooth_borealis")
head(biomass_preds)
#> Simple feature collection with 6 features and 8 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -64.5 ymin: 58.4628 xmax: -61.36857 ymax: 60.62184
#> Geodetic CRS:  WGS 84
#>   patch_id year_f sfa            area biomass_density_with_catch
#> 1       P1   1995   4 20367.39 [km^2]                   5457.326
#> 2       P1   1996   4 20367.39 [km^2]                   5454.644
#> 3       P1   1997   4 20367.39 [km^2]                   5454.517
#> 4       P1   1998   4 20367.39 [km^2]                   5453.848
#> 5       P1   1999   4 20367.39 [km^2]                   5455.352
#> 6       P1   2000   4 20367.39 [km^2]                   5456.485
#>   biomass_density biomass_with_catch   biomass                       geometry
#> 1        5456.046          111151511 111125446 POLYGON ((-64.42169 60.2712...
#> 2        5453.432          111096891 111072206 POLYGON ((-64.42169 60.2712...
#> 3        5454.079          111094295 111085371 POLYGON ((-64.42169 60.2712...
#> 4        5451.752          111080672 111037989 POLYGON ((-64.42169 60.2712...
#> 5        5452.025          111111296 111043534 POLYGON ((-64.42169 60.2712...
#> 6        5448.507          111134383 110971897 POLYGON ((-64.42169 60.2712...
```

18. We can also produce a biomass plot (note that this simulated data is
    very monotonous).

``` r
spm_plot_biomass(sspm_model_fit, 
                 biomass = "weight_per_km2_smooth_borealis")
```

<img src="man/figures/README-unnamed-chunk-23-1.png" width="100%" />

``` r
spm_plot_biomass(sspm_model_fit, 
                 biomass = biomass_smooth, 
                 biomass_var = NULL, 
                 biomass_var_predict = "weight_per_km2_smooth_borealis",
                 biomass_var_smooth = "weight_per_km2_smooth",
                 biomass_var_origin = "weight_per_km2")
```

<img src="man/figures/README-unnamed-chunk-24-1.png" width="100%" />
