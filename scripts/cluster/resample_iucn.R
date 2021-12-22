# Della script for resampling IUCN Habitat map (Jung et al. 2020) for each coarse land cover type (Yin et al. 2020),
# in order to conduct a direct assignment of habitat types to land cover classes.

# This involves the following steps:

# -------------------------------------- 0. load files ---------------------------------------------------- #
# 0. Load files


# For each of four coarse land cover types, do the following in a loop:
# ------ begin loop
# -------------------------------------- 1. reclassify ---------------------------------------------------- #
# 1. Reclassify the IUCN habitat map (at native 100m resolution) to include only those habitat types in each of 
# the four coarse land cover classes (as indicated by the crosswalk document).
# 1b. Reclassify the land cover maps to include only one of the four land cover classes (for each of the 31 years)

# -------------------------------------- 2. focal resampling ---------------------------------------------- #
# 2. Resample each of these four habitat types to fill NA values in each, using a focal moving window (3x3) function
# to fill NA values based on the modal value (i.e., most common value). 
# This step should be conducted on the original ~100 m resolution, to save processing time.

# -------------------------------------- 3. downscale ----------------------------------------------------- #
# 3. Downscale, or disaggregate the filled, focal resampled IUCN habitat raster (~100m) down to the finer resolution matching
# the land cover maps (~30m). 

# -------------------------------------- 4. mask ---------------------------------------------------------- #
# 4. Mask the reclassified land cover maps (i.e., just 1, just 2, just 3, just 4) with the filled habitat type maps
# for each of the coarse land cover maps. 
# ------ end loop 

# -------------------------------------- 5. combine ------------------------------------------------------- #
# 5. Combine the masked maps that show the resampled IUCN habitat types for each of the coarse land cover types, 
# by either converting NAs to 0s in each, and adding them together, or by using the function terra::cover()
# a. load in the masked, filled habitat type maps.
# b. combine

# -------------------------------------- 6. save ---------------------------------------------------------- #
# 6. save new raster to file.


# ------ Set up parameters ------- #
site_index <- 9 # for testing
lc_class <- 2 # for testing



# -------------------------------------- 0. load files ---------------------------------------------------- #
# 0. Load files

iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv"))

# cropped, native resolution IUCN habitat map:
site_jung_l2 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l2_buff.tif"))
})
names(site_jung_l2) <- site_df$site

# prepared input rasters, passed through temporal filter on December 10th, 2021
lcc <- lapply(1:11, function(i) {
  terra::rast(paste0(p_dat_derived, "lc_r_clean/", site_df$site[i], "_clean.tif"))
})
names(lcc) <- site_df$site


# For each of four coarse land cover types, do the following in a loop:

# Start for loop across all four land cover classes
# for (lc_class in 1:4) {
# ------ begin loop

# crop for testing
site_jung_l2[[site_index]]
lcc[[site_index]]


# crop_ext <- draw()

iucn_test_crop <- crop(site_jung_l2[[site_index]], crop_ext)
lcc_test_crop <- crop(lcc[[site_index]], crop_ext)
plot(iucn_test_crop)
plot(lcc_test_crop[[28:31]])


# -------------------------------------- 1. reclassify ---------------------------------------------------- #
# 1. Reclassify the IUCN habitat map (at native 100m resolution) to include only those habitat types in each of 
# the four coarse land cover classes (as indicated by the crosswalk document).


# reclassify 100 m IUCN habitat map to just those habitats in one of the coarse lc classes.
tic()
rcl_iucn_100 <- 
  # lapply(1:4, function(lc_class) {
  classify(
  # site_jung_l2[[site_index]],
  iucn_test_crop,
  # rcl = iucn_crosswalk %>% filter(lc != 2) %>% select(is = map_code) %>% mutate(becomes = NA), # alternative way to write this
  rcl = iucn_crosswalk %>% filter(lc == lc_class) %>% select(is = map_code, becomes = map_code),
  othersNA = TRUE,
  filename = paste0(p_tmp, site_df$site[site_index], "_iucn_100_rcl_", lc_class,".tif"),
  overwrite=TRUE, names = paste0("lc_", lc_class)
)
# })
toc()
plot(rcl_iucn_100)

# rcl_iucn_100 <- c(rcl_iucn_100[[1]], rcl_iucn_100[[2]], rcl_iucn_100[[3]], rcl_iucn_100[[4]])
plot(rcl_iucn_100)

# 1b. Reclassify the land cover maps to include only one of the four land cover classes (for each of the 31 years)

tic()
rcl_lc <- classify(
  # lcc[[site_index]][["y2015"]], # for just one year, testing
  # lcc[[site_index]],
  lcc_test_crop,
  # simple rcl, e.g., where 2 stays 2 (or becomes 0), everything else is converted to NA
  rcl = tibble(is = lc_class, becomes = 0 #lc_class
  ), 
  othersNA = TRUE,
  filename = paste0(p_tmp, site_df$site[site_index], "_lcc_rcl_", lc_class,".tif"),
  overwrite = TRUE, names = paste0(site_df$site[site_index], "_lcc_rcl_", lc_class)
)
toc()

plot(rcl_lc[[c(1, 10, 20, 31)]])



# -------------------------------------- 2. focal resampling ---------------------------------------------- #
# 2. Resample each of these four habitat types to fill NA values in each, using a focal moving window (3x3) function
# to fill NA values based on the modal value (i.e., most common value). 
# This step should be conducted on the original ~100 m resolution, to save processing time.
# Note that focal only works with one layer at a time.

tic()
rcl_iucn_100_focal <- terra::focal(rcl_iucn_100, w = 3, fun = "modal", 
                                   na.rm=TRUE, na.only=TRUE, expand=TRUE,
                                   filename = paste0(p_tmp, site_df$site[site_index], "_iucn_100_rcl_",
                                                     lc_class,"_focal.tif"),
                                   overwrite=TRUE)
toc()

tic()
for (i in 1:100) {
  rcl_iucn_100_focal <- terra::focal(
    rcl_iucn_100_focal, w = 3, fun = "modal", 
    na.rm=TRUE, na.only=TRUE, expand=TRUE,
    filename = paste0(p_tmp, site_df$site[site_index], "_iucn_100_rcl_",
                      lc_class,"_focal.tif"),
    overwrite=TRUE
  )
}
toc()

# for as long as there are NA cells, run a for loop in chunks of 20.
while(
  is.na(as.vector(unlist(global(rcl_iucn_100_focal, fun = "mean"))))
  # minmax(is.na(x))[2,] == 1 # alternative method
) {
  for (i in 1:20) {
    rcl_iucn_100_focal <- terra::focal(
      rcl_iucn_100_focal, w = 3, fun = "modal", 
      na.rm=TRUE, na.only=TRUE, expand=TRUE,
      filename = paste0(p_tmp, site_df$site[site_index], "_iucn_100_rcl_",
                        lc_class,"_focal.tif"),
      overwrite=TRUE
    )
  }
}

plot(rcl_iucn_100_focal)


# -------------------------------------- 3. downscale ----------------------------------------------------- #
# 3. Downscale, or disaggregate the filled, focal resampled IUCN habitat raster (~100m) down to the finer resolution matching
# the land cover maps (~30m). 

# site_jung_l2_30 <- lapply(1:11, function(i) {
#   terra::resample(
#     x = site_jung_l2[[i]], y = lc[[i]]$y2017, method = "near",
#     filename = paste0(p_derived, "site_jung/", names(lc[i]), "_jung_l2_30.tif"),
#     names = paste0(names(lc[i]), "_jung_l2_30"),
#     overwrite = TRUE)
# }
# )


rcl_iucn_30_focal <- terra::resample(
  x = rcl_iucn_100_focal, y = lcc[[site_index]]$y2017, method = "near",
  filename = paste0(p_tmp, site_df$site[site_index], "_iucn_focal_30_", lc_class, ".tif"),
  names = paste0(site_df$site[site_index], "_iucn_focal_30_", lc_class),
  overwrite = TRUE)





# -------------------------------------- 4. mask ---------------------------------------------------------- #
# 4. Mask the reclassified land cover maps (i.e., just 1, just 2, just 3, just 4) with the filled habitat type maps
# for each of the coarse land cover maps. 
# ------ end loop 
# }

# -------------------------------------- 5. combine ------------------------------------------------------- #
# 5. Combine the masked maps that show the resampled IUCN habitat types for each of the coarse land cover types, 
# by either converting NAs to 0s in each, and adding them together, or by using the function terra::cover()
# a. load in the masked, filled habitat type maps.
# b. combine

# -------------------------------------- 6. save ---------------------------------------------------------- #
# 6. save new raster to file.



