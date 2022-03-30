# ------------------------------------------------------------ #
# Convert abn_lcc to data.table, pass temporal filter
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
source("/home/clc6/biodiversity_abn/scripts/_util/_util_functions.R")


# # array set up -------
# args <- commandArgs(TRUE) # access the slurm array variable
# site_index <- as.numeric(args[1])


# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

# ----------------------- #
# --- land cover class of abandoned land --- #
# ----------------------- #
abn_lcc <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lcc/",
              site_df$site[i], "_abn_lcc", run_label, ".tif"))
})
names(abn_lcc) <- site_df$site

for (i in 8:11) {
# convert abn_lcc SpatRaster to data.table
  site_index <- i
  dt <- spatraster_to_dt(abn_lcc[[site_index]])

  # write to file
  fwrite(dt, 
         file = paste0(
           p_derived, "abn_lcc/",
           site_df$site[site_index], "_abn_lcc", run_label, ".csv")
         )

  cat(paste0("Success! Converted abn_lcc to data.table for site: ", site_df$site[site_index], "(", run_label, ")"),
      fill = TRUE)
}
