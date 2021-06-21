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
sspm_dataset <- new("sspm_dataset",
                 name = "Biomass",
                 data = borealis_spatial,
                 type = "biomass",
                 time_column = "year_f",
                 uniqueID = "uniqueID",
                 coords = c('lon_dec','lat_dec'))

sspm_dataset_pred <- new("sspm_dataset",
                      name = "Predator",
                      type = "predictor",
                      data = predator_spatial,
                      time_column = "year",
                      uniqueID = "uniqueID",
                      coords = c('lon_dec','lat_dec'))

dataset_list <- list(sspm_dataset)
names(dataset_list) <- sapply(dataset_list, spm_name)
sspm_base <- new("sspm",
                 name="Model test",
                 datasets=dataset_list,
                 boundaries=sfa_boundaries)

discret_method <- new("discretization_method",
                      name ="voronoi_method",
                      method = tesselate_voronoi)

sspm_discrete <- new("sspm_discrete",
                     name = spm_name(sspm_base),
                     # NOTE: patch_id absent
                     datasets = spm_datasets(sspm_base),
                     boundaries = spm_boundaries(sspm_base),
                     method = discret_method,
                     patches = borealis_patches,
                     points = borealis_points)

dataset_list <- list(sspm_dataset, sspm_dataset_pred)
names(dataset_list) <- sapply(dataset_list, spm_name)
sspm_discrete_mapped <- new("sspm_discrete",
                            name = spm_name(sspm_base),
                            # NOTE: patch_id absent
                            datasets = dataset_list,
                            boundaries = spm_boundaries(sspm_base),
                            method = discret_method,
                            patches = borealis_patches,
                            points = borealis_points)

sspm_formula <- new("sspm_formula",
                    raw_formula = as.formula("weight_per_km2 ~ smooth_time() +
                                               smooth_space() + smooth_space_time()"),
                    translated_formula = as.formula("weight_per_km2 ~ s(year_f,
                      k = 24L, bs = 're', xt = list(penalty = pen_mat_time)) +
                      s(patch_id, k = 30, bs = 'mrf', xt = list(penalty = pen_mat_space)) +
                      ti(year_f, patch_id, k = c(24, 30), bs = c('re', 'mrf'),
                      xt = list(year_f = list(penalty = pen_mat_time),
                      patch_id = list(penalty = pen_mat_space)))"),
                    vars = list(pen_mat_time = matrix(),
                                pen_mat_space = matrix()))

sspm_discrete_mapped_forms <- sspm_discrete_mapped
sspm_discrete_mapped_forms@datasets$Biomass@formulas <- list(sspm_formula)
