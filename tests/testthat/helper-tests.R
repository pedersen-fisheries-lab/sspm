# Objects used for tests
borealis <- as.data.frame(readRDS("../../data/borealis_full_summary.rds")) %>%
  filter(season!="summer") %>%
  arrange(trip)
borealis_spatial <- st_as_sf(borealis, coords = c('lon_dec','lat_dec'))

sfa <- st_read("../../data/sfa/sfa_boundaries.shp", quiet=TRUE)
the_patches <- st_read("../../data/patches/patches.shp", quiet=TRUE)
the_points <- st_read("../../data/patches/patches.shp", quiet=TRUE)

# Base objects
spaspm_base <- new("spaspm",
                   name="Model test",
                   data=borealis,
                   boundaries=sfa)

the_method <- new("discretization_method",
                  name ="voronoi_method",
                  method = tesselate_voronoi)

spaspm_discrete <- new("spaspm_discrete",
                       name = spm_name(spaspm_base),
                       data = spm_data(spaspm_base),
                       boundaries = spm_boundaries(spaspm_base),
                       data_spatial = borealis_spatial,
                       method = the_method,
                       patches = the_patches,
                       points = the_points)
