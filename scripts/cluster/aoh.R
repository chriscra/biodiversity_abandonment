# ------------------------------------------------------------ #
# Calculate AOH, using data.table function, 
# for Yin et al. 2020 land cover maps. 
# 
# Christopher Crawford, Princeton University, updated March 31st, 2022
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
source("/home/clc6/biodiversity_abn/scripts/_util/_util_functions.R")

# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

# Set up parameters -----

# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
core <- as.numeric(args[1])
# core <- 8

hab_index <- as.numeric(args[2])
# hab_index <- 3
# hab_index refer to .csvs of 
# lc (1, 2, 3) or iucn habitat maps (4, 5, 6, 7),
# and full (1, 4), abn (2, 5), max_abn (3, 6), or potential_abn (7) types.
# See aoh_type_df below, which contains start year, label etc.
# Full indicates that all pixels are considered, not just abandonment.

cat(fill = TRUE, "Core index:", core)
cat(fill = TRUE, "hab_dt index:", hab_index)

# misc:
time_stamp <- format(Sys.time(), "_%Y_%m_%d")

run_label <- "_2022_02_07"


# load IUCN files
# cropped range maps
load(file = paste0(p_derived, "species_ranges.RData"), verbose = TRUE)

# list of unique species-site combinations at my sites, with the index
species_list_tmp <- read_csv(paste0(p_derived, "species_list.csv")) %>% 
  filter(core_index == core)


# load IUCN crosswalk:
iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv")) %>%
  mutate(code = as.character(code)) %>%
  # fix 5.10 being converted to 5.1 issue:
  mutate(code = ifelse(map_code == 510, "5.10", code))

habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))

# distribution of habitat types at each site, for adjusting area of habitat estimates
jung_hab_type_area_df <- read_csv(file = paste0(p_derived, "jung_hab_type_area_df.csv"))


# load area and elevation rasters
elevation_map <- lapply(1:11, function(i) {
  rast(paste0(p_dat, "elevation/", site_df$site[i], "_srtm_crop.tif"))
  })

site_area_ha <- lapply(1:11, function(i) {
  rast(paste0(p_dat, "site_area_ha/", site_df$site[i], "_area_ha.tif"))
})

# ------------------------------------------------------------ #
# -------------------- load large files ---------------------- #
# ------------------------------------------------------------ #

site_index <- grep(unique(species_list_tmp$site), site_df$site)


# ------------ #
# load and prep data.table: habitat (land cover), elevation, area:

# different options for AOH function largely depend on the hab_dt that is loaded:
# options are:
# ------------ #

aoh_type_df <- 
  tibble(index = 1:7,
         class_type = c("lc", "lc", "lc", "iucn", "iucn", "iucn", "iucn"),
         map_type = c("full", "abn", "max_abn", "full", "abn", "max_abn", "potential_abn"),
         start_year = case_when(map_type %in% c("full", "max_abn") ~ 1987, 
                                map_type %in% c("abn", "potential_abn") ~ 1992),
         label = paste0(map_type, "_", class_type),
         path = c(
           # 1. land cover (i.e., 1, 2, 3, 4), to be adjusted proportionally
           paste0(p_input_rasters, site_df$site[site_index], "_clean.csv"),
           
           # 2. land cover, for only abandonment periods themselves
           paste0(p_derived, "abn_lcc/", site_df$site[site_index], "_abn_lcc", run_label, ".csv"),
           
           # 3. land cover, in all pixels that are abandoned at any time (including lc before and after abandonment)
           paste0(p_derived, "abn_lcc/", site_df$site[site_index], "_max_abn_lcc", run_label, ".csv"),
           
           # 4. IUCN habitat directly mapped to land cover classes, using focal(fun = "modal")
           paste0(p_derived, "lcc_iucn_habitat/", site_df$site[site_index], "_lcc_iucn_habitat.csv"),
           
           # 5. IUCN habitat, only in abandoned pixels
           paste0(p_derived, "lcc_iucn_habitat/", site_df$site[site_index], "_abn_lcc_iucn_habitat", run_label, ".csv"),
           
           # 6. IUCN habitat, in all pixels that are abandoned at any time (including habitat before and after abandonment)
           paste0(p_derived, "lcc_iucn_habitat/", site_df$site[site_index], "_max_abn_lcc_iucn_habitat", run_label, ".csv"),
           
           # 7. IUCN habitat, in *potential* abandoned pixels only
           paste0(p_derived, "lcc_iucn_habitat/", site_df$site[site_index], "_potential_abn_lcc_iucn_habitat", run_label, ".csv")
         )
         )

# testing <- 
# aoh_type_df %>%
#   mutate(
#     s = case_when(
#       class_type == "lc" & map_type == "full" ~ p_input_rasters,
#       class_type == "lc" & grepl("abn", map_type) ~ paste0(p_derived, "abn_lcc/"),
#       class_type == "iucn" ~ paste0(p_derived, "lcc_iucn_habitat/")
#     ),
#     m = case_when(
#       map_type == "full" ~ "",
#       map_type == "abn" ~ "_abn",
#       map_type == "max_abn" ~ "_max_abn",
#       map_type == "potential_abn" ~ "_potential_abn",
#     ),
#     e = case_when(
#       class_type == "lc" & map_type == "full" ~ "_clean.csv",
#       class_type == "lc" & grepl("abn", map_type) ~ paste0("_lcc", run_label, ".csv"),
#       class_type == "iucn" ~ paste0("_lcc_iucn_habitat", if_else(map_type == "full", "", run_label), ".csv")),
#     p = paste0(s, site_df$site[site_index], m, e)
#   ) %>%
#   mutate(test = path == p)


hab_dt <- fread(filter(aoh_type_df, index == hab_index)$path)

# hab_dt <- fread("/Users/christophercrawford/Downloads/chongqing_abn_lcc_2021_03_13.csv",
#                 select = c(1, 2, 31))
# names(hab_dt)


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


# run AOH function:
# 1987:2017 for full lc maps:
# 1992:2017 for only lc in abandonment (since 1992 is the first year of abandonment)

aoh_tmp <- lapply(
  # 1:4,
  1:nrow(species_list_tmp),
  function(i) {
  cc_AOH_data.table(index = i, 
                    site_index = site_index,
                    year_index = filter(aoh_type_df, index == hab_index)$start_year:2017, 
                    calc_lc = if (filter(aoh_type_df, index == hab_index)$class_type == "lc") TRUE else FALSE, 
                    include_time = TRUE,
                    hab_dt = hab_dt,
                    sp_ranges = species_ranges, 
                    sp_list = species_list_tmp
                    )
}) %>% bind_rows()

aoh_output_path <- paste0(p_derived, "aoh/aoh_tmp_", 
                          filter(aoh_type_df, index == hab_index)$label,
                          time_stamp, "_c", core, ".csv")
# write to file
write_csv(aoh_tmp, file = aoh_output_path)

cat(paste0("Wrote aoh_tmp file for core ", core, 
           " (", site_df$site[site_index], ") to:\n", aoh_output_path), fill = TRUE)
# aoh_dt_11_test[, mean(time)/31, by = "site"]

