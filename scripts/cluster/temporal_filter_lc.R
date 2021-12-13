# ------------------------------------------------------------ #
# Run temporal filter for raw land cover maps (Yin et al. 2020)
# 
# Christopher Crawford, Princeton University, December 12th, 2021
# ------------------------------------------------------------ #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      "sf", "fasterize", "dtraster",
                      "tidyverse", "rgdal")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat           <- "/scratch/gpfs/clc6/data/"
p_derived       <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_range         <- "/scratch/gpfs/clc6/data/bd/"
p_input_rasters <- "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_tmp           <- "/scratch/gpfs/clc6/biodiversity_abn/derived/tmp/"


# source functions:
source("/home/clc6/biodiversity_abn/scripts/util/_util_functions.R")

# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)


# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
site_index <- as.numeric(args[1])
# site_index <- 11

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

# load the raw land cover data.table
lc_dt <- fread(
  input = paste0(
    p_input_rasters,
    site_df$site[site_index], ".csv"))

print(paste0("ncell, before filter: ", nrow(lc_dt)))

# pass temporal filter
tic("pass temporal filter")
cc_temporal_filter_lc(lc_dt)
toc()

print(paste0("ncell, after filter: ", nrow(lc_dt)))

# write to file
fwrite(lc_dt, 
       file = paste0(
         p_input_rasters,
         site_df$site[site_index], "_clean.csv"))

cat(paste0("Success! Passed temporal filter on land cover data.table for site: ", site_df$site[site_index]), fill = TRUE)

# now, convert the cleaned data.table back to a spatraster:
tic("translate cleaned lc_dt to raster")
# r <- dt_to_spatraster(dt = lc_dt)
r <- dt_to_raster(lc_dt, raster::crs("+proj=longlat +datum=WGS84 +no_defs"))
toc()

print(paste0("ncell of raster: ", ncell(r)))

# write new cleaned lc raster to file
tic("write new cleaned lc raster to file")
raster::writeRaster(r,
            filename = paste0(p_input_rasters,
                              site_df$site[site_index], "_clean.tif"),
            overwrite = TRUE)
toc()

cat(paste0("Success! Converted cleaned dt back to raster, and saved as file, for site: ", site_df$site[site_index]), fill = TRUE)
