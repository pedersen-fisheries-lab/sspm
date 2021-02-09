library(checkmate)
library(dplyr)
library(sf)

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
                   data = borealis_simulated,
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
