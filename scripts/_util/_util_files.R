# --------------------------------------------------------------- #
#
# Loading required files ("biodiversity_abandonment")
# 
# --------------------------------------------------------------- #

# Switches ----
load_habitats_aoh <- FALSE
load_abn_lcc_and_masks <- FALSE
load_ecoregions <- FALSE

site_df <- read.csv(file = paste0(p_derived, "site_df.csv"))

run_label # check "_util_main.R"


# ------------------------------------------------------------ # 
# -------------------------- Site Extent --------------------- 
# ------------------------------------------------------------ # 

site_sf <- st_read(paste0(p_derived, "sf/site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()

# plot(site_sf$geometry)
# plot(site_sf %>% filter(site == "shaanxi") %>% st_geometry())



# biomes
if(load_ecoregions) {
site_ecoregions2017 <- st_read(paste0(p_derived, "sf/site_ecoregions2017.shp"))
site_biomes2017 <- st_read(paste0(p_derived, "sf/site_biomes2017.shp"))

ecoregions2017_simple <- st_read(paste0(p_derived, "sf/ecoregions2017_simple.shp"))
biomes2017_simple <- st_read(paste0(p_derived, "sf/biomes2017_simple.shp"))
}


# ------------------------------------------------------------ # 
# ------------------- Derived Abandonment datasets ---------------------  
# ------------------------------------------------------------ # 

area_summary_df <- read_csv(file = paste0(p_derived2, "area_summary_df", run_label, ".csv")) %>%
  left_join(site_labels)

# area_summary_df %>% 
#   select(site, total_site_area_ha_2017, area_abn_ha_2017, total_crop_extent_ha) %>%
#   arrange(total_site_area_ha_2017)

area_dat <- read_csv(file = paste0(p_derived2, "area_dat", run_label, ".csv"))

abn_lc_area_2017 <- read_csv(file = paste0(p_derived2, "abn_lc_area_2017", run_label, ".csv")) %>%
  left_join(site_labels)
abn_prop_lc_2017 <- read_csv(file = paste0(p_derived2, "abn_prop_lc_2017", run_label, ".csv")) %>%
  left_join(site_labels)

# ------------------------------------------------------------ # 
# -------------------- Raster data --------------------
# ------------------------------------------------------------ # 

# Land use class codes:
#       1. Non-vegetated area (e.g. water, urban, barren land)
#       2. Woody vegetation
#       3. Cropland 
#       4. Herbaceous land (e.g. grassland)

# small test rasters:
# bs <- brick(paste0(p_dat, "Abandonment/belarus_small.tif"))
# bt <- brick(paste0(p_dat_derived, "belarus_subset.tif"))
# names(bs) <- paste0("y", 1987:2017)
# names(bt) <- paste0("y", 1987:2017)

# ------------------------------- load land cover maps --------------------------------------- #
# prepared input rasters (derived by Chris)

# lc <- lapply(1:11, function(i) {
#   terra::rast(paste0(p_dat_derived, "input_rasters/", site_df$site[i], ".tif"))
#   })
# names(lc) <- site_df$site

# ------------------------------- load cleaned land cover maps --------------------------------------- #
# prepared input rasters, passed through temporal filter
lcc <- lapply(1:11, function(i) {
  terra::rast(paste0(p_dat_derived, "input_rasters/", site_df$site[i], "_clean.tif"))
})
names(lcc) <- site_df$site

lcc$volgograd %>% ncell
lcv <- rast(paste0(p_dat_derived, "input_rasters/", site_df$site[10], ".tif"))
lcv %>% ncell

sapply(lcc, ncell)
site_df <- site_df %>% mutate(ncell = sapply(lcc, ncell))
write_csv(site_df, paste0(p_dat_derived, "site_df.csv"))

# ----------------------------- load abandonment age rasters ---------------------------- #

# abandonment age maps (produced by Chris)
age_t <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "age_rasters/", run_label, "/",
           site_df$site[i], "_age", run_label, ".tif")
  )
})

names(age_t) <- site_df$site
for (i in seq_along(age_t)) {names(age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017



# ------------------ bins ------------------ #
# age bins

age_t_bins <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "age_rasters/", run_label, "/2017_bins/",
           site_df$site[i], "_y2017_bins", run_label, ".tif")
  )
})

names(age_t_bins) <- site_df$site


# ----------------------- #
# --- max_age of abandonment --- #
# ----------------------- #

max_age_t <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "max_age/", run_label, "/",
           site_df$site[i], "_max_age", run_label, ".tif")
  )
})
names(max_age_t) <- site_df$site
for (i in seq_along(max_age_t)) {names(max_age_t[[i]]) <- "max_age"} # remember: these are just 1987:2017


# max age bins
max_age_t_bins <- lapply(1:11, function(i) {
  rast(paste0(p_dat_derived, "max_age/", run_label, "/bins/", 
              site_df$site[i], "_max_age_bins", run_label, ".tif"))
  })

names(max_age_t_bins) <- site_df$site


# potential abandonment age maps, assuming no recultivation
potential_age_t <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "age_rasters/", run_label, "/",
           site_df$site[i], "_potential_age", run_label, ".tif")
  )
})

names(potential_age_t) <- site_df$site
for (i in seq_along(potential_age_t)) {names(potential_age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017


# ----------------------------------------------------------- #
# 3. Load the maximum extent of all cropland ever cultivated during time series
# ----------------------------------------------------------- #
lcc_total_crop_mask <- lapply(
  1:11, 
  function(i) {
    rast(paste0(p_dat_derived, "total_crop_mask/", 
                site_df$site[i], "_total_crop_mask_clean",
                run_label,
                ".tif"))
  })

names(lcc_total_crop_mask) <- site_df$site


# ----------------------- #
# --- abandonment mask (>5 years) --- #
# ----------------------- #

# see "/Users/christophercrawford/work/projects/biodiversity_abn/scripts/AOH.Rmd"

abn_mask <- lapply(1:11, function(i){
  rast(paste0(p_dat_derived, "age_rasters/", run_label, "/",
              site_df$site[i], "_abn_5_30_mask", run_label,".tif"))
})
names(abn_mask) <- site_df$site


potential_abn_mask <- lapply(1:11, function(i){
  rast(paste0(p_dat_derived, "age_rasters/", run_label, "/",
              site_df$site[i], "_potential_abn_5_30_mask", run_label,".tif"))
})

names(potential_abn_mask) <- site_df$site

max_age_mask <- lapply(1:11, function(i){
  rast(paste0(p_dat_derived, "max_age/", run_label, "/",
              site_df$site[i], "_max_age_5_30_mask", run_label,".tif"))
})
names(max_age_mask) <- site_df$site



# ----------------------- #
# --- land cover class of abandoned land --- #
# ----------------------- #

abn_lcc <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lcc/",
              site_df$site[i], "_abn_lcc", run_label, ".tif"))
})
names(abn_lcc) <- site_df$site


max_abn_lcc <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "abn_lcc/",
              site_df$site[i], "_max_abn_lcc", run_label, ".tif"))
})
names(max_abn_lcc) <- site_df$site


# ----------------------- #
# --- land cover class of abandoned land in 2017 only --- #
# ----------------------- #
# abn_lc_2017 <- lapply(1:11, function(i) {
#   rast(paste0(p_derived, "abn_lc_rasters/", 
#               site_df$site[i], "_abn_lc_2017.tif"))
# }
# )
# names(abn_lc_2017) <- site_df$site


# ------------------------------------------------------------ # 
# ---------------- Derived Habitat Rasters --------------------------- 
# ------------------------------------------------------------ # 

# ----------------------- #
# -------- IUCN habitat types (Jung et al. 2021) 
# directly mapped onto Yin et al. 2020 land cover classes, using
# focal(fun = "modal"), masked to each land cover class, and knit 
# back together. -------- #
# ----------------------- #

lcc_iucn_habitat <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_lcc_iucn_habitat.tif"))
})
names(lcc_iucn_habitat) <- site_df$site


# IUCN habitat types interpolated to only abandoned pixels
abn_lcc_iucn_habitat <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_abn_lcc_iucn_habitat", run_label, ".tif"))
})
names(abn_lcc_iucn_habitat) <- site_df$site

# IUCN habitat types in the max_abn extent, showing IUCN habitat types for each pixel that was 
# abandoned at any point during the time series.
max_abn_lcc_iucn_habitat <- 
  lapply(1:11, function(i) {
    rast(paste0(p_derived, "lcc_iucn_habitat/",
                site_df$site[i], "_max_abn_lcc_iucn_habitat", run_label, ".tif"))
  })
names(max_abn_lcc_iucn_habitat) <- site_df$site


# IUCN habitat types interpolated to only *potential* abandoned pixels
potential_abn_lcc_iucn_habitat <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_potential_abn_lcc_iucn_habitat",
              run_label, ".tif"))
})
names(potential_abn_lcc_iucn_habitat) <- site_df$site


# IUCN habitat types for the scenario of potential abandonment, for the full max_abn extent
# therefore allowing for the before and after comparison of abandonment
max_potential_abn_lcc_iucn_habitat <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_max_potential_abn_lcc_iucn_habitat",
              run_label, ".tif"))
})
names(max_potential_abn_lcc_iucn_habitat) <- site_df$site


# ----------------------- #
# -------- PNV ---------- #
# ----------------------- #
site_pnv_30 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_pnv/",  
              site_df$site[i], "_pnv_30.tif"))
  })


names(site_pnv_30) <- site_df$site



# ----------------------- #
# -------- Jung IUCN Habitat Types ---------- #
# ----------------------- #
# level 2, at ~ 100m resolution (i.e., not resampled to 30m)
site_jung_l2 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l2_buff.tif"))
  })
names(site_jung_l2) <- site_df$site

# resampled to ~30 m resolution
# level 1
site_jung_l1_30 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l1_30.tif"))
  })
names(site_jung_l1_30) <- site_df$site

# level 2
site_jung_l2_30 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l2_30.tif"))
  })
names(site_jung_l2_30) <- site_df$site


# distribution of habitat types at each site, for adjusting area of habitat estimates
jung_hab_type_area_df <- read_csv(file = paste0(p_derived, "jung_hab_type_area_df.csv")) %>%
  mutate(code = as.character(code)) %>%
  # fix 5.10 being converted to 5.1 issue:
  mutate(code = ifelse(habitat_type == 510, "5.10", code)) 


# 34 habitats that occur at my sites:
site_habitats <- jung_hab_type_area_df %>%
  select(lc, habitat_type, code, Coarse_Name, IUCNLevel) %>% 
  unique() %>% arrange(habitat_type) #%>% .$code





# ----------------------- #
# --- pixel area (ha) --- #
# ----------------------- #
site_area_ha <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_area_ha/", site_df$site[i], "_area_ha.tif"))
  })
names(site_area_ha) <- site_df$site

# site_df %>%
#   mutate(area_ha = sapply(1:11, function(i) {
#     terra::global(site_area_ha[[i]], fun = "sum", na.rm = FALSE)
#   })) %>%
#   select(site, area_ha) %>% arrange(area_ha)


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
old_species_list <- read_csv(file = paste0(p_derived, "/species_list_2021_12_08.csv"))

iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv")) %>%
  mutate(code = as.character(code)) %>%
  # fix 5.10 being converted to 5.1 issue:
  mutate(code = ifelse(map_code == 510, "5.10", code))

iucn_status <- read_csv(file = paste0(p_derived, "iucn_status.csv"))

habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))
habitat_details <- read_csv(file = paste0(p_derived, "iucn_habitat_details_subset.csv"))
# species_synonyms <- read_csv(file = paste0(p_derived, "iucn_species_synonyms_subset.csv"))
common_names <- read_csv(file = paste0(p_derived, "iucn_common_names_subset.csv"))

habitat_age_req <- read_csv(paste0(p_derived, "habitat_age_req/iucn_habitat_age_req.csv"))
habitat_age_req_coded <- read_csv(paste0(p_derived, "habitat_age_req/", "habitat_age_req_coded.csv"))
#

# ------------------------------------------------------------ # 
# ----------------------------- AOH --------------------- 
# ------------------------------------------------------------ # 

aoh_type_df <- 
  tibble(index = 1:10,
         class_type = c("lc", "lc", "lc", "iucn", "iucn",
                        "iucn", "iucn", "iucn", "iucn", "iucn"),
         map_type = c("full", "abn", "max_abn", "full", "abn", 
                      "max_abn", "potential_abn", "max_potential_abn", 
                      "crop_abn", "crop_abn_potential"),
         start_year = case_when(
           map_type %in% c("full", "max_abn", "max_potential_abn",
                           "crop_abn", "crop_abn_potential") ~ 1987, 
           map_type %in% c("abn", "potential_abn") ~ 1992),
         label = paste0(map_type, "_", class_type),
         p1 = case_when(
           class_type == "lc" ~ "Yin land cover (proportional)", 
           class_type == "iucn" ~ "IUCN habitats (mapped directly to lc)"
         ),
         p2 = case_when(
           map_type == "full" ~ "entire landscape, 1987-2017", 
           map_type == "max_abn" ~ "abandoned pixels only, 1987-2017",
           map_type == "max_potential_abn" ~ "abandoned pixels only, 1987-2017, potential",
           map_type == "crop_abn" ~ "abandoned pixels, cultivation through 2017",
           map_type == "crop_abn_potential" ~ "abandoned pixels, cultivation through 2017, potential",
           map_type == "abn" ~ "abandonment periods only",
           map_type == "potential_abn" ~ "abandonment periods only, potential"
           ),
         
         p3 = case_when(
           class_type == "lc" ~ "LC", 
           class_type == "iucn" ~ "IUCN"
         ),
         p4 = case_when(
           map_type == "full" ~ "Full Landscape\n(1987-2017)", 
           map_type == "max_abn" ~ "Abandonment\n(1987-2017)",
           map_type == "max_potential_abn" ~ "Abandonment\n(1987-2017, potential)",
           map_type == "crop_abn" ~ "Abandonment\n(cultivation-2017)",
           map_type == "crop_abn_potential" ~ "Potential Abandonment\n(crop-2017)", #"Cropland through\nabandonment\n(potential)",
           map_type == "abn" ~ "Abn periods only",
           map_type == "potential_abn" ~ "Abn periods only (pot.)"),
         
         p5 = case_when(
           map_type == "full" ~ "Full Landscape (1987-2017)", 
           map_type == "max_abn" ~ "Abandonment (1987-2017)",
           map_type == "max_potential_abn" ~ "Abandonment (1987-2017, potential)",
           map_type == "crop_abn" ~ "Abandonment (cultivation-2017)",
           map_type == "crop_abn_potential" ~ "Potential Abandonment (crop-2017)", #"Cropland through\nabandonment\n(potential)",
           map_type == "abn" ~ "Abn periods only",
           map_type == "potential_abn" ~ "Abn periods only (pot.)"),
         
         short_desc = paste0(p4),
         desc = paste0(p1, "; ", p2)
         )


aoh_type_labels <- c(aoh_type_df$short_desc, "Birds", "Mammals", "Amphibians")
names(aoh_type_labels) <- c(aoh_type_df$label, "bird", "mam", "amp")

aoh_type_labels <- c(aoh_type_labels, 
  "TRUE" = "Mature Forest Obligates",
  "FALSE" = "Non-Mature Forest Obligates",
  "Range size <= global median" = "Range size <= global median",
  "Range size > global median" = "Range size > global median")


aoh_l <- read_parquet(paste0(p_derived, "aoh_l.parquet"))
aoh_species_list <- read_parquet(paste0(p_derived, "aoh_species_list.parquet"))
aoh_filter <- read_parquet(paste0(p_derived, "aoh_filter.parquet"))
aoh <- read_parquet(paste0(p_derived, "aoh.parquet"))
aoh_lm <- read_parquet(paste0(p_derived, "aoh_lm.parquet"))
aoh_trends <- read_parquet(paste0(p_derived, "aoh_trends.parquet"))
aoh_trends_by_sp <- read_parquet(paste0(p_derived, "aoh_trends_by_sp.parquet"))
run_indices <- read_parquet(paste0(p_derived, "aoh_run_indices.parquet"))
aoh_change_df <- read_parquet(paste0(p_derived, "aoh_change_df.parquet"))



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
