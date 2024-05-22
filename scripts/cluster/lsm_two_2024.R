# -------------------------------------------------------- #
# Christopher Crawford, Princeton University, May 2024.

# Script to calculate various landscape metrics using the R package 
# landscapemetrics to extract the signal of abandonment:
# For the year 2017, comparing the observed landscape with a hypothetical landscape
# in which all abandoned croplands remained in cultivation through 2017.
# -------------------------------------------------------- #

# load libraries
cluster_packages <- c("data.table", 
                      "tictoc", #"raster", 
                      "terra",
                      "landscapemetrics", #"landscapetools", 
                      ## "sp", "rgdal",
                      # "sf", "fasterize",
                      # "arrow"
                      "tidyverse"
)

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat           <- "/scratch/gpfs/clc6/data/"
p_derived       <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_range         <- "/scratch/gpfs/clc6/data/bd/"
p_input_rasters <- "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_tmp           <- "/scratch/gpfs/clc6/biodiversity_abn/derived/tmp/"

# p_dat_derived <- "/scratch/network/clc6/abandonment_trajectories/data_derived/"
# p_output <- "/scratch/network/clc6/abandonment_trajectories/output/"

# source functions:
source("/home/clc6/biodiversity_abn/scripts/_util/_util_functions.R")

# set terra autosave options:
terraOptions(tempdir = p_tmp)

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

# Set up parameters -----
run_label <- "_2022_02_07"

# # array set up -------
# args <- commandArgs(TRUE) # access the slurm array variable
# indx <- as.numeric(args[1])

# core <- 17
# run <- 1
# site <- "shaanxi"
# site <- site_df$site[indx] # set site


# hab_index <- as.numeric(args[2]) # this refers to the value provided in the slurm file, e.g., the 11 in the below
# Rscript ./aoh.R $SLURM_ARRAY_TASK_ID 11

# misc:
time_stamp <- format(Sys.time(), "_%Y_%m_%d")

# ------------ load data ----------- #

# load prepared input rasters, passed through temporal filter
lcc <- lapply(1:11, function(i) {
  terra::rast(paste0(p_input_rasters, site_df$site[i], "_clean.tif"))
})
names(lcc) <- site_df$site


# load abandonment mask
abn_mask <- lapply(1:11, function(i){
  rast(paste0(p_input_rasters, site_df$site[i], "_abn_5_30_mask", run_label,".tif"))
})



# determine the correct projection to use for each site:
site_projections <- 
  lapply(lcc, function(x) as.vector(ext(x))) %>% 
  bind_rows() %>%
  bind_cols(site_df, .) %>%
  mutate(lon = round((xmin + xmax) / 2, 0),
         lat = round((ymin + ymax) / 2, 0),
         lat = case_when(
           site %in% c("goias", "mato_grosso") ~ 1,
           TRUE ~ lat
         ),
         proj = paste0("+proj=bonne +lon_0=", lon, " +lat_1=", lat)) %>%
  select(site, proj)

# indx <- 7
lapply(1:11, function(indx) {
  
  # derive landscape in 2017 without abandonment (i.e., all abandoned croplands remained cropland through 2017)
  hypo_2017 <- mask(lcc[[indx]]$y2017, abn_mask[[indx]]$y2017, 
                    inverse=FALSE, maskvalues=0, 
                    updatevalue=3, # update anything overlapping with the abandonment mask back to cropland (lc 3)
                    filename=paste0(p_input_rasters, site_df$site[indx], "_hypo_no_abn_2017.tif"),
                    overwrite = TRUE)
  
  
  # reproject raster
  tic("Reproject rasters")
  hypo_2017_reproj <- terra::project(
    hypo_2017,
    # test_r,
    y = site_projections$proj[indx], # update to 
    method = "near")
  toc(log = TRUE)
  
  
  # remove 0s from these reprojected raster
  tic("remove 0s")
  hypo_2017_reproj[hypo_2017_reproj == 0] <- NA
  toc(log = TRUE)
  
  
  # write cleaned reprojected raster to file:
  tic("Write cleaned reprojected raster to file.")
  # cat("Writing reproj hypo SpatRaster to file:", site_df$site[i], "...", fill = TRUE)
  writeRaster(hypo_2017_reproj, 
              filename = paste0(p_input_rasters, site_df$site[indx], "_hypo_no_abn_2017_reproj.tif"),
              overwrite = TRUE, names = names(hypo_2017_reproj))
  toc(log = TRUE)
  
  
  
  # -------------------------------------------------------- #
  # (re)load reprojected rasters
  tic("load the cleaned reprojected rasters")
  hypo_2017_reproj <- terra::rast(paste0(p_input_rasters, site_df$site[indx], "_hypo_no_abn_2017_reproj.tif"))
  toc(log = TRUE)
  
  # # check_landscapes
  check_hypo_2017_reproj <- check_landscape(hypo_2017_reproj)
  print(check_hypo_2017_reproj)
  
  # Metrics to run
  # list_lsm(level = "class") %>% print(n = 55)
  
  metrics_list <- c(
    "lsm_c_clumpy", # clumpiness index, class (aggregation metric)
    "lsm_c_area_mn", # patch area, mean, per class (area and edge metric)
    "lsm_c_para_mn" # perimeter-area ratio, mean (shape metric) # note, shape metrics take much more memory than others
  )
  
  print("Calculating the following metric:")
  print(list_lsm(what = metrics_list))
  
  tic(msg = paste0("calculate: ", metrics_list))
  frag_temp <- hypo_2017_reproj %>%
    calculate_lsm(
      what = metrics_list,
      classes_max = 4, # with 0 removed
      verbose = TRUE, progress = TRUE
    ) %>%
    dplyr::left_join(x = .,
                     y = lsm_abbreviations_names,
                     by = c("metric", "level"))
  toc(log = TRUE)
  
  # rename file
  
  # Save file
  write_csv(frag_temp, paste0(p_derived, "frag_", site_df$site[indx], "_hypo_no_abn_2017.csv"))
  
  toc(log = TRUE) # final toc
  
  print(tic.log())
  
}) # end of lapply() call

# ------------------------------------------------------------- #
# Notes:

# note, metrics 8 (total edge), 9-11 (perimeter-area ratio) and 12 (cohesion) require more memory than the others.

# metrics_list <- c(
# "lsm_c_ai", # aggregation index, class level (RS has used this one)
# "lsm_c_clumpy", # clumpiness index, class (maybe)
# 
# "lsm_c_np", # number of patches, class
# "lsm_c_area_cv", # patch area, cv, per class
# "lsm_c_area_mn", # patch area, mean, per class
# "lsm_c_area_sd", # patch area, sd, per class
# "lsm_c_ca", # total (class) area
# 
# "lsm_c_te", # total edge
# "lsm_c_para_cv", # perimeter-area ratio, cv
# "lsm_c_para_mn", # perimeter-area ratio, mean
# "lsm_c_para_sd", # perimeter-area ratio, sd
# 
# # new additions, # 12-15
# "lsm_c_cohesion", # COHESION is an 'Aggregation metric'. It characterises the connectedness of patches belonging to class i. 
#                   # It can be used to assess if patches of the same class are located aggregated or rather isolated and thereby 
#                   # COHESION gives information about the configuration of the landscape. 
# 
# "lsm_c_contig_mn", # Shape metric - Measures the "contiguity" of cells within patches (the class level metric is the mean across all patches in a class)
# "lsm_c_contig_cv",
# "lsm_c_contig_sd"
# )
