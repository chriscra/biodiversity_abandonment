# ------------------------------------------------------------ #
# Extract mask of all noncrop periods that occur prior to cultivation.
# noncrop_precrop_mask
# 
# Christopher Crawford, Princeton University, April 15th, 2022
# ------------------------------------------------------------ #
# load libraries
cluster_packages <- c("data.table", "tictoc", "terra",
                      "sf", "tidyverse", "rgdal", "arrow", "dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat           <- "/scratch/gpfs/clc6/data/"
p_derived       <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_range         <- "/scratch/gpfs/clc6/data/bd/"
p_input_rasters <- "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_tmp           <- "/scratch/gpfs/clc6/biodiversity_abn/derived/tmp/"

terraOptions(tempdir = p_tmp)

# source functions:
source("/home/clc6/biodiversity_abn/scripts/_util/_util_functions.R")
source("/home/clc6/abandonment_trajectories/scripts/_util/_util_functions.R")

# ------- load files -------- #

# Set up parameters -----

# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])
# indx <- 9

# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))
site <- site_df$site[indx] # set site:

run_label <- "_2022_02_07"

skip <- TRUE # switch in order to skip the first part of the script

if (!skip) {

  # lapply(site_df$site, function(site){
  cat(fill = TRUE, "producing noncrop_precrop_mask for:", site)
  # Step 1. Load dt_binary
  dt <- read_parquet(paste0(p_input_rasters, site, "_binary", run_label, ".parquet"))
  
  # Step 2. cc_calc_age()
  # ------- The codes I'll want to remove are the ones that correspond to 
  # ------- the column index.
  tic("cc_calc_age()")
  cc_calc_age(dt)
  toc()
  
  # dt[]
  
  # Step 3. 
  
  tic("cc_extract_noncrop_precrop()")
  cc_extract_noncrop_precrop(dt)
  toc()
  
  # dt[]
  
  tic("write_parquet()")
  
  write_parquet(dt, paste0(p_input_rasters, site, "_noncrop_precrop_mask", run_label, ".parquet"))
  # dt <- read_parquet(paste0(p_input_rasters, site, "_noncrop_precrop_mask", run_label, ".parquet"))
  rm(dt)
  toc()
  
  
  # Step 4. Convert this into a SpatRaster:
  tic("convert dt to spatraster ('noncrop_precrop_mask_tmp')")
  
  cc_save_dt_as_raster(site_ = site,
                       type = paste0("_noncrop_precrop_mask", run_label),
                       input_path = p_input_rasters,
                       output_path = p_input_rasters,
                       input_file_ext = ".parquet")
  
  toc()

}

# ------------------------------------------------------------------ #
# load relevant rasters:
# ------------------------------------------------------------------ #

noncrop_precrop_mask <- 
  lapply(1:11, function(i) {
  terra::rast(paste0(p_input_rasters, 
                     site_df$site[i],
                     "_noncrop_precrop_mask", run_label,".tif"))
})


max_abn_lcc_iucn_habitat <-
  lapply(1:11, function(i) {
  terra::rast(paste0(p_derived, "lcc_iucn_habitat/",
                     site_df$site[i],
                     "_max_abn_lcc_iucn_habitat", run_label, ".tif"))
})


max_potential_abn_lcc_iucn_habitat <-
  lapply(1:11, function(i) {
  terra::rast(paste0(p_derived, "lcc_iucn_habitat/",
                     site_df$site[i],
                     "_max_potential_abn_lcc_iucn_habitat", run_label, ".tif"))
})



# ------------------------------------------------------------------ #
# Step 5. Mask the max_abn_lcc_iucn_habitat_tmp and max_potential_abn_lcc_iucn_habitat_tmp SpatRasters
# ------------------------------------------------------------------ #

tic("Mask the max_abn_lcc_iucn_habitat SpatRaster by noncrop_precrop_mask")
lapply(1:11, function(i) {
terra::mask(
  max_abn_lcc_iucn_habitat[[i]], noncrop_precrop_mask[[i]],
  maskvalues = NA, inverse = TRUE,
  names = names(max_abn_lcc_iucn_habitat[[i]]),
  filename = paste0(p_derived, "lcc_iucn_habitat/", 
                    site_df$site[i],
                    "_crop_to_abn_iucn_observed", run_label, ".tif"),
  overwrite = TRUE
)
})
toc()

tic("Mask the max_potential_abn_lcc_iucn_habitat SpatRaster by noncrop_precrop_mask")
lapply(1:11, function(i) {
terra::mask(
  max_potential_abn_lcc_iucn_habitat[[i]], noncrop_precrop_mask[[i]],
  maskvalues = NA, inverse = TRUE,
  names = names(max_potential_abn_lcc_iucn_habitat[[i]]),
  filename = paste0(p_derived, "lcc_iucn_habitat/", 
                    site_df$site[i],
                    "_crop_to_abn_iucn_potential", run_label, ".tif"),
  overwrite = TRUE
)
})
toc()


# ------------------------------------------------------------------ #
# Step 6, save rasters as csvs:
# ------------------------------------------------------------------ #
lapply(1:11, function(i) {
  cc_save_spatraster_as_dt(
    paste0(p_derived, "lcc_iucn_habitat/",
           site_df$site[i],
           "_crop_to_abn_iucn_observed", run_label, ".tif")
  )
  
  cc_save_spatraster_as_dt(
    paste0(p_derived, "lcc_iucn_habitat/",
           site_df$site[i],
           "_crop_to_abn_iucn_potential", run_label, ".tif")
  )
})



cat(fill = TRUE, "Completed! Site:", site)



