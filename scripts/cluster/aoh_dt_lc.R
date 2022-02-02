# ------------------------------------------------------------ #
# Calculate AOH, using data.table function, 
# for Yin et al. 2020 land cover maps. 
# 
# Christopher Crawford, Princeton University, December 1st, 2021
# ------------------------------------------------------------ #
# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      #"landscapemetrics", "landscapetools", "sp",
                      "sf", "fasterize",
                      "tidyverse", "rgdal", "dtraster")

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
core <- as.numeric(args[1])
# core <- 8

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
species_list_tmp <- read_csv(paste0(p_derived, "species_list.csv")) %>% 
  filter(core_index == core)


# load IUCN crosswalk:
iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv"))
habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))

# distribution of habitat types at each site, for adjusting area of habitat estimates
jung_hab_type_area_df <- read_csv(file = paste0(p_derived, "jung_hab_type_area_df.csv"))


# load area and elevation rasters
elevation_map <- lapply(1:11, function(i) {
  rast(paste0(p_dat, "elevation/", 
              site_df$site[i], "_srtm_crop.tif")
       )
  })

site_area_ha <- lapply(
  list.files(paste0(p_dat, "site_area_ha"), full.names = TRUE), 
  function(i) rast(i)
)

# ------------------------------------------------------------ #
# -------------------- load large files ---------------------- #
# ------------------------------------------------------------ #

site_index <- grep(unique(species_list_tmp$site), site_df$site)

# ------------ #
# load and prep data.table: habitat (land cover), elevation, area

hab_dt <- fread(input = paste0(p_input_rasters, 
                               site_df$site[site_index], "_clean.csv"))

el_area_dt <- spatraster_to_dt(
  spt = c(site_area_ha[[site_index]],
          elevation_map[[site_index]])#, xy_switch = FALSE
)

setnames(el_area_dt, 
         old = grep("area", names(el_area_dt), value = T), 
         new = "area_ha")

stopifnot(
  all.equal(hab_dt$x, el_area_dt$x),
  all.equal(hab_dt$y, el_area_dt$y)
)

# combine hab_dt and el_area_dt into a single data.table
hab_dt[, ':='(area_ha = el_area_dt$area_ha,
              elevation = el_area_dt$elevation)]


# run AOH function
aoh_tmp <- lapply(1:nrow(species_list_tmp), function(i) {
  cc_AOH_data.table(index = i, 
                    site_index = site_index,
                    year_index = 1987:2017, 
                    calc_lc = TRUE, 
                    include_time = TRUE,
                    hab_dt = hab_dt,
                    sp_ranges = species_ranges, 
                    sp_list = species_list_tmp
                    )
}) %>% bind_rows()

# write to file
write_csv(aoh_tmp,
          file = paste0(p_derived, "aoh_tmp", time_stamp, "_c", core, ".csv")
          )

cat(paste0("Wrote aoh_tmp file for core ", core, " (",site_df$site[site_index], ") to:", 
           p_derived, "aoh_tmp", time_stamp, "_c", core, ".csv"), fill = TRUE)
# aoh_dt_11_test[, mean(time)/31, by = "site"]

