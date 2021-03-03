library(testthat)
library(checkmate)
library(dplyr)
library(sf)
library(mgcv)
library(spaspm)

# Objects used for tests
borealis_simulated <- spaspm:::borealis_simulated
predator_simulated <- spaspm:::predator_simulated

borealis_spatial <- st_as_sf(borealis_simulated, coords = c('lon_dec','lat_dec'))
predator_spatial <- st_as_sf(predator_simulated, coords = c('lon_dec','lat_dec'))

sfa_boundaries <- spaspm:::sfa_boundaries
borealis_patches <- spaspm:::borealis_patches
borealis_points <- spaspm:::borealis_points

# Base objects
spaspm_data <- new("spaspm_data",
                   name = "Biomass",
                   data = borealis_spatial,
                   time_col = "year_f",
                   uniqueID = "uniqueID",
                   coords = c('lon_dec','lat_dec'),
                   representation = "Simple feature collection")

spaspm_data_pred <- new("spaspm_data",
                        name = "Predator",
                        data = predator_spatial,
                        time_col = "year",
                        uniqueID = "uniqueID",
                        coords = c('lon_dec','lat_dec'),
                        representation = "Simple feature collection")

spaspm_base <- new("spaspm",
                   name="Model test",
                   data=spaspm_data,
                   boundaries=sfa_boundaries)

discret_method <- new("discretization_method",
                      name ="voronoi_method",
                      method = tesselate_voronoi)

spaspm_discrete <- new("spaspm_discrete",
                       name = spm_name(spaspm_base),
                       data = spm_base_dataset(spaspm_base),
                       boundaries = spm_boundaries(spaspm_base),
                       method = discret_method,
                       patches = borealis_patches,
                       points = borealis_points)

spaspm_discrete_mapped <- new("spaspm_discrete",
                              name = spm_name(spaspm_base),
                              data = spm_base_dataset(spaspm_base),
                              boundaries = spm_boundaries(spaspm_base),
                              method = discret_method,
                              patches = borealis_patches,
                              points = borealis_points,
                              mapped_datasets = list(Predator = spaspm_data_pred))

spaspm_formula <- new("spaspm_formula",
                      raw_formula = as.formula("weight_per_km2 ~ smooth_time() +
                                               smooth_space() + smooth_space_time()"),
                      translated_formula = as.formula("weight_per_km2 ~ s(year_f,
                      k = 24L, bs = 're', xt = list(penalty = pen_mat_time)) +
                      s(patch_id, k = 30, bs = 'mrf', xt = list(penalty = pen_mat_space)) +
                      ti(year_f, patch_id, k = c(24, 30), bs = c('re', 'mrf'),
                      xt = list(year_f = list(penalty = pen_mat_time),
                      patch_id = list(penalty = pen_mat_space)))"),
                      dataset = "Biomass",
                      vars = list(pen_mat_time = matrix(),
                                  pen_mat_space = matrix()))

spaspm_discrete_mapped_forms <- spaspm_discrete_mapped
spaspm_discrete_mapped_forms@mapped_formulas <- list(spaspm_formula)
