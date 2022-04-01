# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 12th, 2020

# Script to calculate various landscape metrics, 
# using the R package landscapemetrics
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
# file_in <- "/scratch/network/clc6/abandonment_trajectories/data/belarus.tif"
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

# set site:
site <- "belarus"
print("Calculating landscapemetrics for: ")
print(site)


tic.clearlog()
tic("Full script")
# load the data
# -------------------------------------------------------- #

# # unprojected rasters
# tic("Load unprojected rasters")
# input_r <- brick(paste0(p_dat_derived, site, ".tif")) # merged version
# toc(log = TRUE)
# 
# # reproject rasters:
# tic("Reproject rasters")
# reprojected_r <- projectRaster(input_r, 
#                                crs = crs("+proj=bonne +lat_1=10 +lon_0=30"), 
#                                method = "ngb")
# toc(log = TRUE)
# 
# # write reprojected raster to file:
# tic("Write reprojected raster to file.")
# writeRaster(reprojected_r, filename = paste0(p_dat_derived, site, "_reproj.tif"))
# toc(log = TRUE)


# (re)load reprojected rasters
tic("reload the reprojected rasters")
reprojected_r <- brick(paste0(p_dat_derived, site, "_reproj.tif"))
toc(log = TRUE)
names(reprojected_r) <- paste0("y", 1987:2017) # rename

# # check_landscapes
# tic("check landscape")
# check_reprojected_r <- check_landscape(reprojected_r)
# print(check_reprojected_r)
# toc(log = TRUE)

# select landscape metrics to run:
metrics_list <- c(
  # aggregation metrics
  "lsm_l_ai", # aggregation index, landscape level
  "lsm_c_ai", # aggregation index, class level
  "lsm_l_cohesion", # patch cohesion index, landscape
  "lsm_c_cohesion", # patch cohesion index, class
  "lsm_c_enn_mn", # euclidean nearest neighbor distance, mean, class
  
  "lsm_l_contag", # connectance
  "lsm_l_np", # number of patches
  "lsm_l_pd", # patch density
  
  # shape metric
  "lsm_l_contig_mn", # contiguity index
  
  # area and edge metrics
  "lsm_l_area_mn", # patch area, mean, landscape overall
  "lsm_c_area_mn", # patch area, mean, per class
  "lsm_c_ca", # total (class) area
  "lsm_l_lpi", # largest patch index, landscape
  "lsm_c_lpi" # largest patch index, class
)

# list the metrics
print(list_lsm(what = metrics_list))



tic("calculate lsm")
frag_metrics <- reprojected_r %>%
  calculate_lsm(
    what = metrics_list,
    classes_max = 5,
    verbose = TRUE, progress = TRUE
  ) %>%
  dplyr::left_join(x = .,
                   y = lsm_abbreviations_names, 
                   by = c("metric", "level"))
toc(log = TRUE)

# Save file
tic()
write_rds(frag_metrics, paste0(p_output, "frag_metrics_", site, ".rds"))
toc(log = TRUE)


toc(log = TRUE) # final toc

print(tic.log())
