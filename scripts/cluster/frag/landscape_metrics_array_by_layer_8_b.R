# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 17th, 2020

# Script to calculate various landscape metrics, 
# using the R package landscapemetrics.
# Calculate an individual lsm (chosen manually), 
# but processing each year in the raster layer by layer, using the array variable.
# -------------------------------------------------------- #
args <- commandArgs(TRUE) # access the slurm array variable
print(args)
print(args[1])
# run <- as.numeric(args[1])
run <- 8 # run entry 8 in metric_list
layer <- as.numeric(args[1])
site <- "belarus"


# -------------------------------------------------------- #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

print(paste0("Calculating landscapemetrics for ", site, ", run #", run, ", layer #", layer))

tic.clearlog()
tic("Full script")
# -------------------------------------------------------- #
# (re)load reprojected rasters
tic("load the cleaned reprojected rasters")
reprojected_r <- brick(paste0(p_dat_derived, site, "_reproj_0rm.tif"))
toc(log = TRUE)

names(reprojected_r) <- paste0("y", 1987:2017) # rename

# # check_landscapes
# tic("check landscape")
# check_reprojected_r <- check_landscape(reprojected_r)
# print(check_reprojected_r)
# toc(log = TRUE)

# Metrics to run
metrics_list <- c(
  "lsm_c_ai", # aggregation index, class level (RS has used this one)
  "lsm_c_clumpy", # clumpiness index, class (maybe)
  
  "lsm_c_np", # number of patches, class
  "lsm_c_area_cv", # patch area, cv, per class
  "lsm_c_area_mn", # patch area, mean, per class
  "lsm_c_area_sd", # patch area, sd, per class
  "lsm_c_ca", # total (class) area
  
  "lsm_c_te", # 8. total edge
  "lsm_c_para_cv", # 9. perimeter-area ratio, cv
  "lsm_c_para_mn", # 10. perimeter-area ratio, mean
  "lsm_c_para_sd" # 11. perimeter-area ratio, sd
)

print("Calculating the following metric:")
print(list_lsm(what = metrics_list[run]))

tic(msg = paste0("calculate: ", metrics_list[run]))
frag_temp <- reprojected_r[[layer]] %>%
  calculate_lsm(
    what = metrics_list[run],
    classes_max = 4, # with 0 removed
    verbose = TRUE, progress = TRUE
  ) %>%
  dplyr::left_join(x = .,
                   y = lsm_abbreviations_names,
                   by = c("metric", "level"))
toc(log = TRUE)

frag_temp$layer <- layer

# rename file
assign(paste0("frag_", site, run, "_", layer), frag_temp)

# Save file
tic(msg = paste0("save: ", metrics_list[run], ", layer: ", layer))
save(list = c(paste0("frag_", site, run, "_", layer)),
     file = paste0(p_output, "frag_", site, run, "_", layer, ".rds")
)
toc(log = TRUE)


toc(log = TRUE) # final toc

print(tic.log())
