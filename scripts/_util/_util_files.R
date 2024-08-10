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

# # to create site extent as sf object
# # must load age_t from below
# site_sf <- lapply(age_t, FUN = function(i) {
#   i %>%
#     terra::as.polygons(extent=TRUE) %>% 
#     sf::st_as_sf()
# }) %>%
#   bind_rows() %>%
#   mutate(site = site_df$site,
#          label = gsub("_", "", site_df$label),
#          lab_desc = paste0("(", label, ") ", site_df$description),
#          desc = site_df$description)
# st_write(site_sf, dsn = paste0(p_dat_derived, "site_sf.shp"), append = FALSE)

site_sf <- st_read(paste0(p_derived, "sf/site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()

# plot(site_sf$geometry)
# plot(site_sf %>% filter(site == "shaanxi") %>% st_geometry())



# Ecoregions & Biomes
# Data available from: https://ecoregions.appspot.com/
# Dinerstein, E. et al. An Ecoregion-Based Approach to Protecting Half the Terrestrial Realm. BioScience 67, 534–545 (2017).

# see script: "habitats.Rmd"
if(load_ecoregions) {
site_ecoregions2017 <- st_read(paste0(p_derived, "sf/site_ecoregions2017.shp"))
# site_biomes2017 <- st_read(paste0(p_derived, "sf/site_biomes2017.shp")) # no longer necessary
ecoregions2017_simple <- st_read(paste0(p_derived, "sf/ecoregions2017_simple.shp"))
# biomes2017_simple <- st_read(paste0(p_derived, "sf/biomes2017_simple.shp")) # no longer necessary
}


# ------------------------------------------------------------ # 
# ------------------- Derived Abandonment datasets ---------------------  
# ------------------------------------------------------------ # 
# Data available from: https://zenodo.org/records/5348287

area_summary_df <- read_csv(file = paste0(p_derived2, "area_summary_df", run_label, ".csv")) %>%
  left_join(site_labels)

area_summary_df %>%
  select(site, total_site_area_ha_2017, area_abn_ha_2017, total_crop_extent_ha, area_2017_as_prop_site, area_2017_as_prop_total_crop) %>%
  arrange(
    area_2017_as_prop_total_crop
    # area_2017_as_prop_site
    # total_site_area_ha_2017
    ) #%>%
  # mutate(total = round(total_site_area_ha_2017 / 10^6, digits = 2)) %>% .$total

area_dat <- read_csv(file = paste0(p_derived2, "area_dat", run_label, ".csv"))
abn_lc_area_2017 <- read_csv(file = paste0(
  p_derived2, "abn_lc_area_2017", run_label, ".csv")) %>%
  left_join(site_labels)
abn_prop_lc_2017 <- read_csv(file = paste0(
  p_derived2, "abn_prop_lc_2017", run_label, ".csv")) %>%
  left_join(site_labels)

# ------------------------------------------------------------ # 
# -------------------- Raster data --------------------
# ------------------------------------------------------------ # 

# Land use class codes:
#       1. Non-vegetated area (e.g. water, urban, barren land)
#       2. Woody vegetation
#       3. Cropland 
#       4. Herbaceous land (e.g. grassland)

# ---
# Cleaned land cover maps
# ---
# Data available from: https://zenodo.org/records/5348287

# prepared input rasters, passed through temporal filter
lcc <- lapply(1:11, function(i) {
  terra::rast(paste0(p_dat_derived, "input_rasters/", site_df$site[i], "_clean.tif"))
})
names(lcc) <- site_df$site

# ----------------------------- load abandonment age rasters ---------------------------- #

# ----------------------- #
# --- age_ of abandonment --- #
# ----------------------- #
# Data available from: https://zenodo.org/records/5348287

age_t <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "age_rasters/", run_label, "/",
           site_df$site[i], "_age", run_label, ".tif")
  )
})

names(age_t) <- site_df$site
for (i in seq_along(age_t)) {names(age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017



# ----------------------- #
# --- max_age of abandonment --- #
# ----------------------- #
# Data available from: https://zenodo.org/records/5348287

max_age_t <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "max_age/", run_label, "/",
           site_df$site[i], "_max_age", run_label, ".tif")
  )
})
names(max_age_t) <- site_df$site
for (i in seq_along(max_age_t)) {names(max_age_t[[i]]) <- "max_age"} # remember: these are just 1987:2017


# ----------------------- #
# --- potenttial_age of abandonment --- #
# ----------------------- #
# potential abandonment age maps, assuming no recultivation
# Data available from: https://zenodo.org/records/5348287

potential_age_t <- lapply(1:11, function(i) {
  terra::rast(
    paste0(p_dat_derived, "age_rasters/", run_label, "/",
           site_df$site[i], "_potential_age", run_label, ".tif")
  )
})

names(potential_age_t) <- site_df$site
for (i in seq_along(potential_age_t)) {names(potential_age_t[[i]]) <- paste0("y", 1987:2017)} # remember: these are just 1987:2017


# ------------------------------------------------ #
# Note: these "bins" files are no longer necessary. 
# To see how to create these files from scratch, see "4_figures.Rmd" from https://zenodo.org/records/6383127
# # ------------------ bins ------------------ #
# # age bins
# age_t_bins <- lapply(1:11, function(i) {
#   terra::rast(
#     paste0(p_dat_derived, "age_rasters/", run_label, "/2017_bins/",
#            site_df$site[i], "_y2017_bins", run_label, ".tif")
#   )
# })
# names(age_t_bins) <- site_df$site
# 
# # max age bins
# max_age_t_bins <- lapply(1:11, function(i) {
#   rast(paste0(p_dat_derived, "max_age/", run_label, "/bins/", 
#               site_df$site[i], "_max_age_bins", run_label, ".tif"))
#   })
# names(max_age_t_bins) <- site_df$site

# ----------------------------------------------------------- #
# 3. Load the maximum extent of all cropland ever cultivated during time series
# ----------------------------------------------------------- #

# ------------------------------------------------ #
# Note: the total crop mask is not strictly necessary. 
# To see how to create these files from scratch, see "1_summary_stats.Rmd" from https://zenodo.org/records/6383127
# lcc_total_crop_mask <- lapply(
#   1:11, 
#   function(i) {
#     rast(paste0(p_dat_derived, "total_crop_mask/", 
#                 site_df$site[i], "_total_crop_mask_clean",
#                 run_label,
#                 ".tif"))
#   })
# names(lcc_total_crop_mask) <- site_df$site


# ----------------------- #
# --- abandonment masks (>5 years) --- #
# ----------------------- #

# created in script: "AOH.Rmd" code chunk {r create-abn-age-masks}

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
# created in script: "AOH.Rmd" chunk {r land-cover-of-abn-pixels} 

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



# ------------------------------------------------------------ # 
# ---------------- Fragmentation data --------------------------- 
# ------------------------------------------------------------ # 
# created in script: "habitats.Rmd" ## "Fragmentation" section

frag_df <- 
  lapply(site_df$site, function(i) {
    read_csv(file = paste0(p_derived, "frag/frag_", i, ".csv")) %>%
      mutate(site = i)
    }) %>% 
  bind_rows() %>%
  mutate(year = case_when(
    site == "nebraska" ~ layer + 1985,
    TRUE ~ layer + 1986)
  ) #%>% filter(year > 1986, year < 2018)

# hypo no abandonment through 2017
frag_hypo_df <- 
  lapply(site_df$site, function(i) {
    read_csv(file = paste0(p_derived, "frag/frag_", i, "_hypo_no_abn_2017.csv")) %>%
      mutate(site = i)
  }) %>% 
  bind_rows() %>%
  mutate(year = 2017)





# ------------------------------------------------------------ # 
# ---------------- Derived Habitat Rasters --------------------------- 
# ------------------------------------------------------------ # 

# ----------------------- #
# -------- IUCN habitat types (Jung et al. 2021) 
# directly mapped onto Yin et al. 2020 land cover classes, using
# focal(fun = "modal"), masked to each land cover class, and knit 
# back together. -------- #
# ----------------------- #

# Created in "habitats.Rmd" chunk {r lcc-iucn-habitat-composite}
lcc_iucn_habitat <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_lcc_iucn_habitat.tif"))
})
names(lcc_iucn_habitat) <- site_df$site


# IUCN habitat types interpolated to only abandoned pixels
# Created in "habitats.Rmd" chunk {r mask-lcc-iucn-habitat-to-abn}
abn_lcc_iucn_habitat <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_abn_lcc_iucn_habitat", run_label, ".tif"))
})
names(abn_lcc_iucn_habitat) <- site_df$site

# IUCN habitat types in the max_abn extent, showing IUCN habitat types for each pixel that was abandoned at any point during the time series.
# Created in "habitats.Rmd" chunk {r mask-lcc-iucn-habitat-to-abn}
max_abn_lcc_iucn_habitat <- 
  lapply(1:11, function(i) {
    rast(paste0(p_derived, "lcc_iucn_habitat/",
                site_df$site[i], "_max_abn_lcc_iucn_habitat", run_label, ".tif"))
  })
names(max_abn_lcc_iucn_habitat) <- site_df$site


# IUCN habitat types interpolated to only *potential* abandoned pixels
# {r *create-potential-abn-iucn}
potential_abn_lcc_iucn_habitat <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_potential_abn_lcc_iucn_habitat",
              run_label, ".tif"))
})
names(potential_abn_lcc_iucn_habitat) <- site_df$site


# IUCN habitat types for the scenario of potential abandonment, for the full max_abn extent, therefore allowing for the before and after comparison of abandonment
# Created in "habitats.Rmd" chunk {r *potential_max}
max_potential_abn_lcc_iucn_habitat <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_max_potential_abn_lcc_iucn_habitat",
              run_label, ".tif"))
})
names(max_potential_abn_lcc_iucn_habitat) <- site_df$site

# Developed in "habitats.Rmd" chunk {r *potential-lcc-full},
# Run directly Princeton's computing cluster Della, see script: "potential_full_iucn.R"
# lcc_iucn_habitat_potential
# e.g., /scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/belarus_lcc_iucn_habitat_potential_2022_02_07.tif


# ----------------------- #
# -------- Jung IUCN Habitat Types ---------- #
# ----------------------- #
# created in "habitats.Rmd" chunk {r jung-crop-resample}

# level 1, at ~ 100m resolution (i.e., not resampled to 30m), no buffer
site_jung_l1_no_buff <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l1.tif"))
})
names(site_jung_l1_no_buff) <- site_df$site

# level 2, at ~ 100m resolution (i.e., not resampled to 30m), no buffer
site_jung_l2_no_buff <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l2.tif"))
})
names(site_jung_l2_no_buff) <- site_df$site


# level 1, at ~ 100m resolution (i.e., not resampled to 30m)
site_jung_l1 <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_jung/", site_df$site[i], "_jung_l1_buff.tif"))
})
names(site_jung_l1) <- site_df$site

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


# created in "habitats.Rmd" chunk {r jung-freq}
site_jung_l2_freq <- read_csv(file = paste0(p_derived, "site_jung_l2_30_freq.csv"))


# distribution of habitat types at each site, for adjusting area of habitat estimates
# created in "habitats.Rmd" chunk {r calc-area-per-habitat-type}
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
# See "AOH.Rmd" chunk {r calculate-area-ha}
site_area_ha <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "site_area_ha/", site_df$site[i], "_area_ha.tif"))
  })
names(site_area_ha) <- site_df$site


# ----------------------- #
# --- pixel elevation (m) --- #
# ----------------------- #
# See "AOH.Rmd" chunk {r crop_elevation_map}
elevation_map <- lapply(1:11, function(i) {
  rast(paste0(p_derived, "elevation/", 
              site_df$site[i], "_srtm_crop.tif")
  )
})
names(elevation_map) <- site_df$site


# ------------------------------------------------------------ # 
# ---------------------- IUCN Data --------------------------- 
# ------------------------------------------------------------ # 

# -- spatial data -- #

# cropped range maps
# see "IUCN.Rmd" chunk {r load-and-bind-cropped-ranges}
load(file = paste0(p_derived, "species_ranges/vert_sites.RData"), verbose = TRUE)

# see "IUCN.Rmd" chunk {r species-ranges}
load(file = paste0(p_derived, "species_ranges/species_ranges.RData"), verbose = TRUE) # a subset of vert_sites called "species_ranges", for use in AOH code.


# list of unique species-site combinations at my sites
species_list <- read_csv(file = paste0(p_derived, "/species_list.csv"))
final_species_list <- read_csv(file = paste0(p_proj, "zenodo/final_species_list.csv")) # see "AOH.Rmd" chunk {r generate-final-species-list}

# old_species_list <- read_csv(file = paste0(p_derived, "/species_list_2021_12_08.csv"))


# -- assessment data -- #

iucn_crosswalk <- read_csv(paste0(p_derived, "iucn_lc_crosswalk.csv")) %>%
  mutate(code = as.character(code)) %>%
  # fix 5.10 being converted to 5.1 issue:
  mutate(code = ifelse(map_code == 510, "5.10", code))

# based on downloaded IUCN assessment data.
# see "IUCN.Rmd" chunk {r IUCN-status}
iucn_status <- read_csv(file = paste0(p_derived, "iucn_status.csv"))

# see "IUCN.Rmd" chunk {r habitat_prefs}
habitat_prefs <- read_csv(file = paste0(p_derived, "iucn_habitat_prefs_subset.csv"))
habitat_details <- read_csv(file = paste0(p_derived, "iucn_habitat_details_subset.csv"))

# see "IUCN.Rmd" chunk {r elevation_prefs}
elevation_prefs <- read_csv(file = paste0(p_derived, "iucn_elevation_prefs_subset.csv"))

# see "IUCN.Rmd" chunk {r common-names}
# species_synonyms <- read_csv(file = paste0(p_derived, "iucn_species_synonyms_subset.csv"))
common_names <- read_csv(file = paste0(p_derived, "iucn_common_names_subset.csv"))


# -- habitat age requirement determinations -- #
# created by Alex Wiebe and Christopher Crawford
habitat_age_req <- read_csv(paste0(p_derived, "habitat_age_req/iucn_habitat_age_req.csv"))
habitat_age_req_coded <- read_csv(paste0(p_derived, "habitat_age_req/", "habitat_age_req_coded.csv"))
#
# habitat_age_req_coded %>%
  # filter(str_detect(mature_forest_obl, "NA") | is.na(mature_forest_obl))

# habitat_age_req_coded %>%

centroids_df <- read_csv(file = paste0(p_derived, "/sf/centroids_df.csv")) %>% 
  as.data.frame() %>%
  tibble::as_tibble()

# ------------------------------------------------------------ # 
# ----------------------------- AOH --------------------- 
# ------------------------------------------------------------ # 

aoh_type_df <- 
  tibble(index = 1:11,
         class_type = c("lc", "lc", "lc", "iucn", "iucn", "iucn", 
                        "iucn", "iucn", "iucn", "iucn", "iucn"),
         map_type = c("full", "abn", "max_abn", "full", "abn", 
                      "max_abn", "potential_abn", "max_potential_abn", 
                      "crop_abn", "crop_abn_potential",
                      "full_potential"),
         start_year = case_when(
           map_type %in% c("full", "max_abn", "max_potential_abn",
                           "crop_abn", "crop_abn_potential",
                           "full_potential") ~ 1987, 
           map_type %in% c("abn", "potential_abn") ~ 1992),
         label = paste0(map_type, "_", class_type),
         p1 = case_when(
           class_type == "lc" ~ "Yin land cover (proportional)", 
           class_type == "iucn" ~ "IUCN habitats (mapped directly to lc)"
         ),
         p2 = case_when(
           map_type == "full" ~ "entire landscape, 1987-2017", 
           map_type == "full_potential" ~ "entire landscape, 1987-2017 (potential)", 
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
           map_type == "full" ~ "3a. Entire\nspatial extent\n(1987-2017)", 
           map_type == "full_potential" ~ "3b. Entire\nspatial extent,\nno recultivation\n(1987-2017)", 
           map_type == "max_abn" ~ "2a. Net change in\nabandoned crop-\nlands (1987-2017)",
           map_type == "max_potential_abn" ~ "2b. Net change in\nabandoned crop-\nlands, no recult.\n(1987-2017)",
           map_type == "crop_abn" ~ "1a. Abandonment\n(cultivation-2017)",
           map_type == "crop_abn_potential" ~ "1b. Abandonment,\nno recultivation\n(cultivation-2017)", #"Cropland through\nabandonment\n(potential)",
           map_type == "abn" ~ "Abn periods only",
           map_type == "potential_abn" ~ "Abn periods only (pot.)"),
         
         p4_2022 = case_when(
           map_type == "full" ~ "Entire Landscape\n(1987-2017)", 
           map_type == "full_potential" ~ "Entire Landscape\n(1987-2017, potential)", 
           map_type == "max_abn" ~ "Abandonment\n(1987-2017)",
           map_type == "max_potential_abn" ~ "Potential Abandonment\n(1987-2017)",
           map_type == "crop_abn" ~ "Abandonment\n(cultivation-2017)",
           map_type == "crop_abn_potential" ~ "Potential Abandonment\n(cultivation-2017)",
           map_type == "abn" ~ "Abn periods only",
           map_type == "potential_abn" ~ "Abn periods only (pot.)"),
         
         p5 = case_when(
           map_type == "full" ~ "Entire Landscape (1987-2017)", 
           map_type == "full_potential" ~ "Entire Landscape (1987-2017, potential)", 
           map_type == "max_abn" ~ "Abandonment (1987-2017)",
           map_type == "max_potential_abn" ~ "Potential Abandonment (1987-2017)",
           map_type == "crop_abn" ~ "Abandonment (cultivation-2017)",
           map_type == "crop_abn_potential" ~ "Potential Abandonment (cultivation-2017)", #"Cropland through\nabandonment\n(potential)",
           map_type == "abn" ~ "Abn periods only",
           map_type == "potential_abn" ~ "Abn periods only (pot.)"),
         
         short_desc = paste0(p4),
         # short_desc = paste0(p4_2022), # added to replicate dissertation, July 2023
         desc = paste0(p1, "; ", p2)
         )

aoh_types <- aoh_type_df$label[c(4:11)]

aoh_type_labels <- c(aoh_type_df$short_desc)
names(aoh_type_labels) <- c(aoh_type_df$label)

aoh_type_labels <-
  c(aoh_type_labels,
    "bird" = "Birds", "mam" = "Mammals", "amp" = "Amphibians",
    "TRUE" = "Mature Forest Obligates",
    "FALSE" = "Non-Mature Forest Obligates",
    "mature_forest_obligate" = "Mature Forest Obligates",
    "not_obligate" = "All species but Mature Forest Obligates",
    "Range size <= global median" = "Range size <= global median",
    "Range size > global median" = "Range size > global median",
    "Threatened" = "Threatened", 
    "Not Threatened" = "Not Threatened", 
    "gain" = "Gain", 
    "loss" = "Loss",
    "no trend" = "No Trend")

aoh_type_labels_minimal <- 
  c("full_iucn" = "Calc. 3a", 
    "full_potential_iucn" = "Calc. 3b", 
    "max_abn_iucn" = "Calc. 2a",
    "max_potential_abn_iucn" = "Calc. 2b",
    "crop_abn_iucn" = "Calc. 1a",
    "crop_abn_potential_iucn" = "Calc. 1b")

# see "AOH.Rmd" chunk {r load-all-AOH} (and "cluster/aoh.R")
aoh_l <- read_parquet(paste0(p_derived, "aoh_l.parquet"))
aoh_species_list <- read_parquet(paste0(p_derived, "aoh_species_list.parquet"))

# see "AOH.Rmd" chunk {r load-all-AOH}
aoh_filter <- read_parquet(paste0(p_derived, "aoh_filter.parquet"))

# see "AOH.Rmd" chunk {r calc-aoh}
aoh <- read_parquet(paste0(p_derived, "aoh.parquet"))

# added 2022.11.28 after confirming the use of fixest::feols() and the Newey-West estimator to calculate standard errors
aoh_feols <- read_parquet(paste0(p_derived, "aoh_feols.parquet"))
aoh_feols_trends <- read_parquet(paste0(p_derived, "aoh_feols_trends.parquet"))
aoh_feols_trends_by_sp <- read_parquet(paste0(p_derived, "aoh_feols_trends_by_sp.parquet"))

aoh_lm <- read_parquet(paste0(p_derived, "aoh_lm.parquet"))
aoh_trends <- read_parquet(paste0(p_derived, "aoh_trends.parquet"))
aoh_trends_by_sp <- read_parquet(paste0(p_derived, "aoh_trends_by_sp.parquet"))

run_indices <- read_parquet(paste0(p_derived, "aoh_run_indices.parquet"))

# effect sizes (estimated from model trends)
aoh_change_df <- read_parquet(paste0(p_derived, "aoh_change_df.parquet"))

# observed effect sizes (derived directly from observations from the start and end of the time series)
aoh_start_end_l <- read_parquet(paste0(p_derived, "aoh_start_end_l.parquet"))
aoh_start_end_trends_l <- read_parquet(paste0(p_derived, "aoh_start_end_trends_l.parquet"))


aoh_p_change_obs_v_pot_ols <- read_csv(paste0(p_derived, "aoh_p_change_obs_v_pot_ols.csv"))
aoh_p_change_obs_v_pot_summary_ols <- read_csv(paste0(p_derived, "aoh_p_change_obs_v_pot_summary_ols.csv"))
aoh_p_change_obs_v_pot_feols <- read_csv(paste0(p_derived, "aoh_p_change_obs_v_pot_feols.csv"))
aoh_p_change_obs_v_pot_summary_feols <- read_csv(paste0(p_derived, "aoh_p_change_obs_v_pot_summary_feols.csv"))

i <- "crop_abn_iucn"


# ------------------------------------------------------------ # 
# ------- Temp AOH files for MS and AOH.Rmd --------------------- 
# ------------------------------------------------------------ # 

aoh_ms_tmp_trends_incl_mature <- 
  aoh_feols_trends %>%
  mutate(
    aoh_type = as_factor(aoh_type),
    vert_class = as_factor(vert_class),
    trend = as_factor(trend),
    threatened = case_when(
      redlistCategory %in% c("Critically Endangered", "Endangered", "Vulnerable") ~ "Threatened",
      TRUE ~ "Not Threatened")) %>%
  
  filter(
    # exclude all species not affected by abandonment:
    binomial %in% unique(aoh_feols_trends_by_sp %>%
                           filter(aoh_type == "crop_abn_iucn", 
                                  passage_type == "exclude_passage",
                                  vert_class != "amp", mature_forest_obl < 0.5) %>%
                           pull(binomial)),
    passage_type == "exclude_passage", # exclude passage areas from AOH calculations
    vert_class != "amp",
    !aoh_type %in% c("abn_iucn", "potential_abn_iucn"), # exclude unused scenarios
    # !grepl("potential", aoh_type)
  )

# exclude mature forest obligates
aoh_ms_tmp_trends <- aoh_ms_tmp_trends_incl_mature %>%
  filter(mature_forest_obl < 0.5)


aoh_ms_tmp_trends_by_sp_incl_mature <- 
  aoh_feols_trends_by_sp %>%
  mutate(
    aoh_type = as_factor(aoh_type),
    vert_class = as_factor(vert_class),
    overall_trend = as_factor(overall_trend),
    threatened = case_when(
      redlistCategory %in% c("Critically Endangered", "Endangered", "Vulnerable") ~ "Threatened",
      TRUE ~ "Not Threatened")) %>%
  
  filter(
    # exclude all species not affected by abandonment:
    binomial %in% unique(aoh_feols_trends_by_sp %>%
                           filter(aoh_type == "crop_abn_iucn", passage_type == "exclude_passage",
                                  vert_class != "amp", mature_forest_obl < 0.5) %>%
                           pull(binomial)),
    passage_type == "exclude_passage", # exclude passage areas from AOH calculations
    vert_class != "amp",
    !aoh_type %in% c("abn_iucn", "potential_abn_iucn"), # exclude unused scenarios
    # !grepl("potential", aoh_type)
  )

aoh_ms_tmp_trends_by_sp <- aoh_ms_tmp_trends_by_sp_incl_mature %>%
  filter(mature_forest_obl < 0.5)

# ------------------------------------------------------------ # 
# ------- Traits: temp files ------- 
# ------------------------------------------------------------ # 
taxonomy_df <- read_parquet(paste0(p_derived, "taxonomy_df.parquet"))
etard_updated <- read_parquet(paste0(p_derived, "etard_updated.parquet"))

habitat_occurrence_df2 <- read_parquet(paste0(p_derived, "habitat_occurrence_df.parquet")) #%>%
  # left_join(habitat_prefs %>% 
  #             separate(code, into = c("lvl1", "lvl2"), sep = "\\.", extra = "merge") %>%
  #             select(binomial, lvl1) %>% unique() %>% mutate(lvl1 = as.integer(lvl1)) %>%
  #             group_by(binomial) %>%
  #             summarise(n_lvl1_habitats = n(), lvl1_habitats = str_flatten(lvl1, collapse = ", ")) %>% ungroup()
  # )


habitat_occurrence_df <-
  habitat_prefs %>% 
  filter(suitability == "Suitable") %>%
  separate(code, into = c("lvl1", "lvl2"), sep = "\\.", extra = "merge") %>%
  select(binomial, lvl1) %>% unique() %>% 
  mutate(lvl1 = as.integer(lvl1), occurrence = TRUE) %>% arrange(lvl1) %>%
  pivot_wider(id_cols = binomial, names_from = lvl1, names_prefix = "type_", values_from = occurrence) %>%
  arrange(binomial) %>%
  select(binomial, 
         forest_occ = type_1,
         savanna_occ = type_2,
         shrubland_occ = type_3,
         grassland_occ = type_4,
         wetlands_occ = type_5,
         rocky_occ = type_6,
         caves_occ = type_7,
         desert_occ = type_8,
         # marine_neritic_occ = type_9,
         # marine_oceanic_occ = type_10,
         marine_intertidal_occ = type_12,
         marine_coastal_occ = type_13,
         artificial_terrestrial_occ = type_14,
         artificial_aquatic_occ = type_15,
         introduced_occ = type_16,
         # other_occ = type_17,
         # unknown_occ = type_18
         ) %>%
  replace(is.na(.), FALSE) %>%
  mutate(n_suitable_habitats = rowSums(across(contains("occ")), na.rm = TRUE)) %>%
  left_join(
    habitat_prefs %>% 
      filter(suitability == "Suitable") %>%
      select(binomial, code) %>% unique() %>%
      group_by(binomial) %>% summarise(n_suitable_habitats_lvl2 = n()) %>% ungroup()
    ) %>%
  left_join(habitat_occurrence_df2 %>% select(binomial, arable_occ, ag_occ, farmland_occ, urban_occ))


# ---------------------------------------------- #
# Categorical trends in AOH (one trend per species)
# ---------------------------------------------- #
aoh_mod_tmp_trends_by_sp_all_incl_mature <- 
# aoh_mod_tmp_trends_by_sp_all <-
  aoh_feols_trends_by_sp %>% 
  filter(passage_type == "exclude_passage", 
         vert_class != "amp", 
         # mature_forest_obl < 0.5
         # str_starts(aoh_type, "crop|max|full")
  ) %>% 
  droplevels() %>%
  mutate(
    aoh_type = as_factor(aoh_type),
    overall_trend = as_factor(overall_trend),
    # threatened = case_when(
    # redlistCategory %in% c("Critically Endangered", "Endangered", "Vulnerable") ~ "Threatened",
    # TRUE ~ "Not Threatened"),
    trend_categorical = case_when(
      overall_trend == "gain" ~ "gain", # strict only
      # str_detect(overall_trend, "gain") ~ "gain", # including weak gains too
      TRUE ~ "non_gain"),
    binary_trend_gain = case_when(overall_trend == "gain" ~ 1, TRUE ~ 0),
    binary_trend_loss = case_when(overall_trend == "loss" ~ 1, TRUE ~ 0),
    binary_trend_no_trend = case_when(overall_trend == "no trend" ~ 1, TRUE ~ 0),
    binary_gain_v_loss = case_when(overall_trend == "gain" ~ 1, overall_trend == "loss" ~ 0)
  ) %>%
  left_join(taxonomy_df) %>% 
  left_join(select(etard_updated,
                   #dataset, etard_id, 
                   vert_class, binomial, Body_mass_g, Trophic_level, Habitat_breadth_IUCN),
            by = c("vert_class", "binomial")) %>% 
  left_join(centroids_df) %>% 
  # add habitat specialities
  left_join(habitat_occurrence_df) %>%
  mutate(vert_class = as_factor(case_when(vert_class == "bird" ~ "Birds", vert_class == "mam" ~ "Mammals")))

aoh_mod_tmp_trends_by_sp_all <- 
  aoh_mod_tmp_trends_by_sp_all_incl_mature %>%
  filter(mature_forest_obl < 0.5)
  

aoh_mod_tmp_trends_by_sp <- 
  aoh_mod_tmp_trends_by_sp_all %>%
  filter(aoh_type == "crop_abn_iucn")

aoh_mod_tmp_trends_by_sp %>%
  select(vert_class, binomial, Habitat_breadth_IUCN, contains("n_suit")) %>%
  filter(is.na(Habitat_breadth_IUCN))

aoh_mod_tmp_trends_by_sp %>%
  select(vert_class, binomial, Habitat_breadth_IUCN, n_suitable_habitats_lvl2) %>%
  filter(Habitat_breadth_IUCN != n_suitable_habitats_lvl2)

# ---------------------------------------------- #
# Estimated changes in AOH based on ols regression with NW estimator.
# For response variables:
# Absolute change in AOH, and scaled to site
# ---------------------------------------------- #

aoh_est_change_tmp_all_incl_mature <-
  aoh_change_df %>% 
  filter(est_type == "estimate", 
         str_detect(passage_type, "excl"),
         vert_class != "amp", 
         # mature_forest_obl < 0.5
  ) %>%
  left_join(taxonomy_df) %>%
  left_join(select(etard_updated,
                   #dataset, etard_id, 
                   vert_class, binomial, Body_mass_g, Trophic_level, Habitat_breadth_IUCN),
            by = c("vert_class", "binomial")
  ) %>%
  left_join(aoh_mod_tmp_trends_by_sp_all %>%
              select(binomial, total_range_area) %>%
              unique()) %>%
  left_join(centroids_df) %>%
  # add habitat specialities
  left_join(habitat_occurrence_df) %>%
  mutate(
    vert_class = as_factor(case_when(vert_class == "bird" ~ "Birds", vert_class == "mam" ~ "Mammals")),
    max_abn_extent_div_site_area =        
      area_ever_abn_ha / total_site_area_ha_2017,
    max_abn_ext_percent_site = 100* max_abn_extent_div_site_area,
    binary_trend_gain = case_when(trend == "gain" ~ 1, TRUE ~ 0),
    binary_trend_loss = case_when(trend == "loss" ~ 1, TRUE ~ 0),
    binary_trend_no_trend = case_when(trend == "no trend" ~ 1, TRUE ~ 0),
    
    # create a binary variable with 1 for gain, 0 for loss, and NA for no trend
    binary_gain_v_loss = case_when(trend == "gain" ~ 1, trend == "loss" ~ 0),
    slope_prop_site = slope / total_site_area_ha_2017,
    abs_change_percent_site = abs_change_as_prop_site_area * 100
    ) %>%
  arrange(aoh_type, vert_class, site, passage_type, run_index)

aoh_est_change_tmp_all <- aoh_est_change_tmp_all_incl_mature %>%
  filter(mature_forest_obl < 0.5)

# subset these based on aoh_type
aoh_est_change_tmp <- 
  aoh_est_change_tmp_all %>%
  filter(aoh_type == "crop_abn_iucn")

aoh_est_change_potential_tmp <-
  aoh_est_change_tmp_all %>%
  filter(aoh_type == "crop_abn_potential_iucn")


# ---------------------------------------------- #
# Observed change in AOH based on mean values for 
# start and end of time series (for various window sizes).
# This is used for response variables such as:
# abs_change (change in AOH)
# abs_change_as_prop_site_area (change in AOH as proportion of site area)
# abs_change_percent_site (change in AOH as percent of site area)
# Ratio change in AOH
# # ---------------------------------------------- #

aoh_obs_change_tmp_all <-
  aoh_start_end_l %>%
  filter(str_detect(passage_type, "excl"),
         vert_class != "amp", 
         mature_forest_obl < 0.5,
         window_size == 5 # filter to only five year window size
  ) %>%
  left_join(taxonomy_df) %>%
  left_join(select(etard_updated,
                   #dataset, etard_id, 
                   vert_class, binomial, Body_mass_g, Trophic_level, Habitat_breadth_IUCN),
            by = c("vert_class", "binomial")
  ) %>%
  left_join(aoh_mod_tmp_trends_by_sp_all %>% 
              select(binomial, total_range_area) %>%
              unique()) %>%
  left_join(centroids_df) %>%
  # add habitat specialities
  left_join(habitat_occurrence_df) %>%
  mutate(
    vert_class = as_factor(case_when(vert_class == "bird" ~ "Birds", vert_class == "mam" ~ "Mammals")),
    max_abn_extent_div_site_area = 
      area_ever_abn_ha / total_site_area_ha_2017,
    max_abn_ext_percent_site = 100* max_abn_extent_div_site_area,
    abs_change_percent_site = abs_change_as_prop_site_area * 100,
    binary_trend_gain = case_when(trend == "gain" ~ 1, TRUE ~ 0),
    binary_trend_loss = case_when(trend == "loss" ~ 1, TRUE ~ 0),
    binary_trend_no_trend = case_when(trend == "no trend" ~ 1, TRUE ~ 0),
    
    # create a binary variable with 1 for gain, 0 for loss, and NA for no trend
    binary_gain_v_loss = case_when(trend == "gain" ~ 1, trend == "loss" ~ 0)
    ) %>%
  arrange(aoh_type, vert_class, site, passage_type, run_index)

# subset based on aoh_type
aoh_obs_change_tmp <- 
  aoh_obs_change_tmp_all %>%
  filter(aoh_type == "crop_abn_iucn")

aoh_obs_change_potential_tmp <-
  aoh_obs_change_tmp_all %>%
  filter(aoh_type == "crop_abn_potential_iucn")

p_proj
# save model input files for traits analysis:
write_csv(aoh_est_change_tmp_all,
          file = paste0(p_proj, "zenodo/", 
                        "aoh_est_change_tmp_all", ".csv"))
write_csv(aoh_obs_change_tmp_all,
          file = paste0(p_proj, "zenodo/", 
                        "aoh_obs_change_tmp_all", ".csv"))
aoh_obs_change_tmp_all

# -------------------------- #
# model results: see traits.Rmd
# note, this is a big file, so only load it if you need it!
# this model version contains urban_occ, arable_occ, and n_suitable_habitats_lvl2, but not artificial_terrestrial_occ

mod_label <- "_modx1"

# trait_mod_df <- readRDS(file = paste0(p_derived, "traits/", "trait_mod_df", mod_label, ".rds"))
