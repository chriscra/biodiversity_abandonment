# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, October 13th, 2020

# Script to remove 0s from rasters, rewrite to file.
# -------------------------------------------------------- #
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

site_list <- c("shaanxi", "belarus") # list of all sites
site <- site_list[indx] # set site:

print("Filtering raster for: ")
print(site)

# remove 0s from the raster, which should make the whole thing run faster, with less memory


# load libraries
cluster_packages <- c("data.table", "tictoc", "raster",
                      "landscapemetrics", "landscapetools",
                      "tidyverse", "rgdal")
install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"


tic.clearlog()
tic("Full script")
# load the data
# -------------------------------------------------------- #

# (re)load reprojected rasters
tic("load the reprojected rasters")
reprojected_r <- brick(paste0(p_dat_derived, site, "_reproj.tif"))
toc(log = TRUE)
names(reprojected_r) <- paste0("y", 1987:2017) # rename

# remove 0s:
tic("remove 0s")
reprojected_r[reprojected_r == 0] <- NA
toc(log = TRUE)

# write cleaned reprojected raster to file:
tic("Write cleaned reprojected raster to file.")
writeRaster(reprojected_r, filename = paste0(p_dat_derived, site, "_reproj_0rm.tif"))
toc(log = TRUE)

toc(log = TRUE) # final toc

print(tic.log())
