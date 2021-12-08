# ------------------------------------------------------------ #
# Run temporal filter for raw land cover maps (Yin et al. 2020)
# 
# Christopher Crawford, Princeton University, December 6th, 2021
# ------------------------------------------------------------ #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      "sf", "fasterize",
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

# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
site_index <- as.numeric(args[1])

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))


# load the raw land cover data.table
lc_dt <- fread(
  input = paste0(
    p_input_rasters,
    site_df$site[site_index], ".csv"))

# pass temporal filter
cc_temporal_filter_lc(lc_dt)

# write to file
fwrite(lc_dt, 
       file = paste0(
         p_input_rasters,
         site_df$site[site_index], "_clean.csv"))

cat(paste0("Success! Passed temporal filter on land cover data.table for site: ", site_df$site[site_index]))



