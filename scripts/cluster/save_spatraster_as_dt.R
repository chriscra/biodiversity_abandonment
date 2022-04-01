# ------------------------------------------------------------ #
# Convert various SpatRasters to data.tables, for use in AOH functions
# 
# Christopher Crawford, Princeton University, March 30th, 2022
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
source("/home/clc6/biodiversity_abn/scripts/_util/_util_functions.R")

run_label <- "_2022_02_07"

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))


# iterate through all 11 sites, saving SpatRasters as data.tables

lapply(1:11, function(i) { 
  # ----------------------- #
  # 1. Convert abn_lcc (land cover class of abandoned land)
  cc_save_spatraster_as_dt(
    paste0(p_derived, "abn_lcc/",
           site_df$site[i], "_abn_lcc", run_label, ".tif")
  )

  # ----------------------- #
  # 2. Convert lcc_iucn_habitat (IUCN habitat classes interpolated to lc classes)
  cc_save_spatraster_as_dt(
    paste0(p_derived, "lcc_iucn_habitat/",
           site_df$site[i], "_lcc_iucn_habitat.tif")
  )

  # ----------------------- #
  # 3. Convert abn_lcc_iucn_habitat (IUCN habitat classes, interpolated, restricted to abandoned lands)
  cc_save_spatraster_as_dt(
    paste0(p_derived, "lcc_iucn_habitat/",
           site_df$site[i], "_abn_lcc_iucn_habitat", run_label, ".tif")
  )
   
  # ----------------------- #
  # 4. Convert *potential* abn_lcc_iucn_habitat (IUCN habitat classes, interpolated, restricted to *potential* abandoned lands)
  cc_save_spatraster_as_dt(
    paste0(p_derived, "lcc_iucn_habitat/",
           site_df$site[i], "_potential_abn_lcc_iucn_habitat", run_label, ".tif")
  )
})





