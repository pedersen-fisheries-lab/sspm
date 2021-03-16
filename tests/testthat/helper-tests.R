library(testthat)
library(checkmate)
library(dplyr)
library(sf)
library(mgcv)
library(sspm)

# Objects used for tests
borealis_simulated <- sspm:::borealis_simulated
predator_simulated <- sspm:::predator_simulated
sfa_boundaries <- sspm:::sfa_boundaries
borealis_patches <- sspm:::borealis_patches
borealis_points <- sspm:::borealis_points
borealis_spatial <- sspm:::borealis_simulated_spatial
predator_spatial <- sspm:::predator_simulated_spatial

# Base objects
sspm_data <- new("sspm_data",
                 name = "Biomass",
                 data = borealis_spatial,
                 time_column = "year_f",
                 uniqueID = "uniqueID",
                 coords = c('lon_dec','lat_dec'),
                 representation = "Simple feature collection")

sspm_data_pred <- new("sspm_data",
                      name = "Predator",
                      data = predator_spatial,
                      time_column = "year",
                      uniqueID = "uniqueID",
                      coords = c('lon_dec','lat_dec'),
                      representation = "Simple feature collection")

sspm_base <- new("sspm",
                 name="Model test",
                 data=sspm_data,
                 boundaries=sfa_boundaries)

discret_method <- new("discretization_method",
                      name ="voronoi_method",
                      method = tesselate_voronoi)

sspm_discrete <- new("sspm_discrete",
                     name = spm_name(sspm_base),
                     # NOTE: patch_id absent
                     data = spm_base_dataset(sspm_base),
                     boundaries = spm_boundaries(sspm_base),
                     method = discret_method,
                     patches = borealis_patches,
                     points = borealis_points)

sspm_discrete_mapped <- new("sspm_discrete",
                            name = spm_name(sspm_base),
                            data = spm_base_dataset(sspm_base),
                            boundaries = spm_boundaries(sspm_base),
                            method = discret_method,
                            patches = borealis_patches,
                            points = borealis_points,
                            mapped_datasets = list(Predator = sspm_data_pred))

sspm_formula <- new("sspm_formula",
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

sspm_discrete_mapped_forms <- sspm_discrete_mapped
# sspm_discrete_mapped_forms@mapped_formulas <- list(sspm_formula)
