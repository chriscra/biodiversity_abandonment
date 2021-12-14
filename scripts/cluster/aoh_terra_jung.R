# ------------------------------------------------------------ #
# Calculate AOH, using {terra} AOH function, 
# for Jung et al. 2020 IUCN Habitat map (for the year 2015) 
# 
# Christopher Crawford, Princeton University, December 13th, 2021
# ------------------------------------------------------------ #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      #"landscapemetrics", "landscapetools", "sp",
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

# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])
# indx <- 3
if(indx == 1) {cores <- c(1:6)}
if(indx == 2) {cores <- c(7:12)}
if(indx == 3) {cores <-c(13:18)}

print(cores)

# core <- 12

# 
# # set up parameters:
# # load in a list of species_list, with a column for "cluster_run"
# della_index
# 
# site <- site_df$site[indx] # set site:
# site_label <- site_df$label[indx] # set label
# 
# # time stamp
time_stamp <- format(Sys.time(), "_%Y_%m_%d") #paste0("_", Sys.Date()) # format(Sys.time(), "_%Y-%m-%d_%H%M%S")


# load IUCN files
# cropped range maps
load(file = paste0(p_derived, "species_ranges.RData"), verbose = TRUE)

# list of unique species-site combinations at my sites, with the index
# species_list_tmp <- read_csv(paste0(p_derived, "species_list.csv")) %>% 
#   filter(core_index == core)


# load IUCN crosswalk:
iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv"))
habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))

# distribution of habitat types at each site, for adjusting area of habitat estimates
jung_hab_type_area_df <- read_csv(file = paste0(p_derived, "jung_hab_type_area_df.csv"))

site_jung_l2_30 <- lapply(1:11, function(i) {
  rast(paste0(p_dat, "habitats/", site_df$site[i], "_jung_l2_30.tif"))
}
)
names(site_jung_l2_30) <- site_df$site

# load area and elevation rasters
elevation_map <- lapply(1:11, function(i) {
  rast(paste0(p_dat, "elevation/",
              site_df$site[i], "_srtm_crop.tif")
  )
})

# site_area_ha <- lapply(
#   list.files(paste0(p_dat, "site_area_ha"), full.names = TRUE), 
#   function(i) rast(i)
# )

# ------------------------------------------------------------ #
# -------------------- load large files ---------------------- #
# ------------------------------------------------------------ #
# site_index <- grep(unique(species_list_tmp$site), site_df$site)

# ------------ #
for (core in cores) {
  # list of unique species-site combinations at my sites, with the index
  species_list_tmp <- read_csv(paste0(p_derived, "species_list.csv")) %>% 
    filter(core_index == core)
  
  site_index <- grep(unique(species_list_tmp$site), site_df$site)
  
  # run AOH function
  aoh_tmp_jung <- lapply(1:nrow(species_list_tmp), 
                         function(i) {
                           cc_AOH_terra(index = i, 
                                        site_index = site_index,
                                        year_index = 2015, #1987:2017, 
                                        calc_lc = FALSE, 
                                        include_time = TRUE,
                                        tmp_location = p_tmp,
                                        sp_ranges = species_ranges, 
                                        sp_list = species_list_tmp
                           )
                         }) %>% bind_rows()
  
  # write to file
  # print(aoh_tmp_jung)
  write_csv(aoh_tmp_jung,
            file = paste0(p_derived, "aoh_tmp_jung", time_stamp, "_c", core, ".csv")
  )
  
  cat(paste0("Wrote aoh_tmp file for core ", core, " (",site_df$site[site_index], ") to:", 
             p_derived, "aoh_tmp_jung", time_stamp, "_c", core, ".csv"), fill = TRUE)
  # aoh_dt_11_test[, mean(time)/31, by = "site"]
  
}
