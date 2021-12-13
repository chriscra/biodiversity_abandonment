# --------------------------------------------------------------- #
#
# Loading required files
# 
# --------------------------------------------------------------- #

site_df <- read.csv(file = paste0(p_derived, "site_df.csv"))
run_label <- "_2021_03_13"


# ------------------------------------------------------------ # 
# -------------------------- Site Extent --------------------- 
# ------------------------------------------------------------ # 

site_sf <- st_read(paste0(p_derived, "site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()

# plot(site_sf$geometry)
# plot(site_sf %>% filter(site == "shaanxi") %>% st_geometry())



# ------------------------------------------------------------ # 
# ------------------- Load Basic Rasters ---------------------  
# ------------------------------------------------------------ # 


# Land use class codes:
#       1. Non-vegetated area (e.g. water, urban, barren land)
#       2. Woody vegetation
#       3. Cropland 
#       4. Herbaceous land (e.g. grassland)

# ------------------------------- load land cover maps --------------------------------------- #
# prepared input rasters (derived by Chris)
site_input_raster_files <- list.files(paste0(p_dat_derived, "input_rasters"), full.names = TRUE) %>%
  grep(".tif", ., value = TRUE) #%>% grep("age", ., value = TRUE, invert = TRUE)

lc <- lapply(site_input_raster_files, function(i) {terra::rast(i)})
names(lc) <- site_df$site

# rename raster layers:
for (i in 1:11) {
  if (names(lc[i]) == "nebraska") {
    names(lc[[i]]) <- paste0("y", 1986:2018)
  } else {
    if (names(lc[i]) == "wisconsin") {
      names(lc[[i]]) <- paste0("y", 1987:2018)
    } else {
      # everything else, just 1987:2017
      names(lc[[i]]) <- paste0("y", 1987:2017)
    }}}


# ------------------------------- load cleaned land cover maps --------------------------------------- #
# prepared input rasters, passed through temporal filter on December 10th, 2021
lcc <- lapply(1:11, function(i) {
  terra::rast(paste0(p_dat_derived, "lc_r_clean/", site_df$site[i], "_clean.tif"))
})
names(lcc) <- site_df$site

# rename raster layers:
for (i in 1:11) {
  if (names(lcc[i]) == "nebraska") {
    names(lcc[[i]]) <- paste0("y", 1986:2018)
  } else {
    if (names(lcc[i]) == "wisconsin") {
      names(lcc[[i]]) <- paste0("y", 1987:2018)
    } else {
      # everything else, just 1987:2017
      names(lcc[[i]]) <- paste0("y", 1987:2017)
    }}}


# ----------------------------- load abandonment age rasters ---------------------------- #

# abandonment age maps (produced by Chris)
age_files <- list.files(paste0(p_dat_derived, "age_rasters/", run_label), 
                        full.names = TRUE) %>%
  grep(".tif", ., value = TRUE) #%>% grep("age", ., value = TRUE, invert = FALSE)


# age_r <- lapply(seq_along(age_files), function(i) {raster::brick(age_files[i])})
# names(age_r) <- site_df$site
# for (i in seq_along(age_r)) {names(age_r[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017


age_t <- lapply(seq_along(age_files), function(i) {terra::rast(age_files[i])})
names(age_t) <- site_df$site
for (i in seq_along(age_t)) {names(age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017



# age bins
age_t_bins <- lapply(1:11, function(i) {rast(paste0(p_dat_derived, "age_rasters/2017_bins/", site_df$site[i], "_y2017_bins.tif"))})
names(age_t_bins) <- site_df$site


# ----------------------- #
# --- abandonment mask (>5 years) --- #
# ----------------------- #

abn_mask <- lapply(1:11, function(i){
  rast(paste0("/Users/christophercrawford/work/projects/abandonment_trajectories/data_derived/age_rasters/abn_mask/", 
              site_df$site[i], "_abn_5_30_mask.tif"))
})

names(abn_mask) <- site_df$site

# ----------------------- #
# --- land cover class of abandoned land --- #
# ----------------------- #
abn_lc <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lc_rasters/",
              site_df$site[i], "_abn_lc.tif"))
})
names(abn_lc) <- site_df$site

# ----------------------- #
# --- land cover class of abandoned land in 2017 only --- #
# ----------------------- #
abn_lc_2017 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lc_rasters/", 
              site_df$site[i], "_abn_lc_2017.tif"))
}
)
names(abn_lc_2017) <- site_df$site


# ----------------------- #
# --- max_age of abandonment --- #
# ----------------------- #

max_age_files <- list.files(paste0(p_dat_derived, "max_age/", run_label), 
                            full.names = TRUE) %>%
  grep(".tif", ., value = TRUE)

# max_age_r <- lapply(seq_along(max_age_files), function(i) {
#   brick(max_age_files[i])
#   })
# names(max_age_r) <- site_df$site

max_age_t <- lapply(seq_along(max_age_files), function(i) {
  rast(max_age_files[i])
})
names(max_age_t) <- site_df$site
for (i in seq_along(max_age_t)) {names(max_age_t[[i]]) <- "max_age"} # remember: these are just 1987:2017


# ------------------------------------------------------------ # 
# ---------------- Derived Habitat Rasters --------------------------- 
# ------------------------------------------------------------ # 


# ----------------------- #
# -------- PNV ---------- #
# ----------------------- #
site_pnv_30 <- lapply(
  list.files(paste0(p_derived, "site_pnv"), full.names = TRUE) %>% grep("_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_pnv_30) <- site_df$site



# ----------------------- #
# -------- Jung IUCN Habitat Types ---------- #
# ----------------------- #
# level 2, at ~ 100m resolution (i.e., not resampled to 30m)
site_jung_l2 <- lapply(
  list.files(paste0(p_derived, "site_jung"), full.names = TRUE) %>%
    grep("_l2_buff", ., value = TRUE), 
  function(i) rast(i)
)
names(site_jung_l2) <- site_df$site

# resampled to ~30 m resolution
# level 1
site_jung_l1_30 <- lapply(
  list.files(paste0(p_derived, "site_jung"), full.names = TRUE) %>% grep("l1_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_jung_l1_30) <- site_df$site

# level 2
site_jung_l2_30 <- lapply(
  list.files(paste0(p_derived, "site_jung"), full.names = TRUE) %>% grep("l2_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_jung_l2_30) <- site_df$site


# distribution of habitat types at each site, for adjusting area of habitat estimates
jung_hab_type_area_df <- read_csv(file = paste0(p_derived, "jung_hab_type_area_df.csv"))

# 34 habitats that occur at my sites:
site_habitats <- jung_hab_type_area_df %>%
  select(lc, habitat_type, code, Coarse_Name, IUCNLevel) %>% 
  unique() %>% arrange(habitat_type) #%>% .$code

# ----------------------- #
# ---- forest carbon ---- #
# ----------------------- #
site_forest_c_30 <- lapply(
  list.files(paste0(p_derived, "site_forest_carbon"), full.names = TRUE) %>% grep("_forest_c_30.tif", ., value = TRUE), 
  function(i) rast(i)
)
names(site_forest_c_30) <- site_df$site


# ----------------------- #
# --- pixel area (ha) --- #
# ----------------------- #
site_area_ha <- lapply(
  list.files(paste0(p_derived, "site_area_ha"), full.names = TRUE), 
  function(i) rast(i)
)
names(site_area_ha) <- site_df$site

# ----------------------- #
# --- pixel elevation (m) --- #
# ----------------------- #
elevation_map <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "elevation/", 
              site_df$site[i], "_srtm_crop.tif")
  )
})
names(elevation_map) <- site_df$site





# ------------------------------------------------------------ # 
# ---------------------- IUCN Data --------------------------- 
# ------------------------------------------------------------ # 

# spatial data
# cropped range maps
load(file = paste0(p_derived, "species_ranges/vert_sites.RData"), verbose = TRUE)
load(file = paste0(p_derived, "species_ranges/species_ranges.RData"), verbose = TRUE)

# list of unique species-site combinations at my sites
species_list <- read_csv(file = paste0(p_derived, "/species_list.csv"))

iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv"))

habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))
habitat_details <- read_csv(file = paste0(p_derived, "iucn_habitat_details_subset.csv"))
species_synonyms <- read_csv(file = paste0(p_derived, "iucn_species_synonyms_subset.csv"))
common_names <- read_csv(file = paste0(p_derived, "iucn_common_names_subset.csv"))

habitat_age_req <- read_csv(file = paste0(p_derived, "iucn_habitat_age_req.csv"))




# ------------------------------------------------------------ # 
# ----------------------------- Basemaps --------------------- 
# ------------------------------------------------------------ # 
# 
# world <- ne_countries(scale = 110, returnclass = "sf") #%>% st_make_valid() # can set returnclass to sf or sp.
# plot(world$geometry)
# 
# world_10 <- ne_countries(scale = 10, returnclass = "sf")# %>% st_make_valid() # can set returnclass to sf or sp.
# 
# # plot(world_10$geometry, graticule = TRUE, axes = TRUE)
# 
# # plot world
# # plot(world_sf$geometry)
# 
# eastern_europe <- ne_countries(scale = 110, returnclass = "sf", continent = "europe")
# # plot(eastern_europe$geometry)
# st_crs(eastern_europe$geometry)
# eastern_europe$geometry %>%
#   st_transform(., crs("+proj=longlat +ellps=WGS84 +lon_0=70")) %>%
#   plot(border = "red")
# 
# crs(usa)
# usa <- ne_countries(scale = 110, country = "United States of America", returnclass = "sf") # can set returnclass to sf or sp.
# 
# china <- ne_countries(scale = 110, returnclass = "sf", country = "china")
# # plot(china$geometry, graticule = TRUE, axes = TRUE)
# 
# # plot(usa$geometry, graticule = TRUE, axes = TRUE)
# 
# l <- eastern_europe$geometry %>% st_wrap_dateline()
# l <- st_transform(eastern_europe$geometry, "+proj=laea +lon_0=30")
# l %>%
#   plot()
# 
# plot(box$geometry)
# ecoregions
# 
# r <- b_age_r$y2017
# r
# rt <- projectRaster(r, crs = "+proj=laea")
# plot(projectRaster(extent(b_age_r), crs = "+proj=laea"), add = T, col = "red")
# plot(extent(s_age_r), add = T, col = "red")
