# ------------------------------------------------------------ #
# Script to merge validated bird ranges into a single sf object
# save that full file, then crop range maps to site locations.
# Christopher Crawford, March 13, 2022
# ------------------------------------------------------------ #

library(tidyverse)
library(sf)
library(lwgeom)
library(tictoc)
library(mapview)

# sf settings:
# sf_use_s2()
sf_use_s2(FALSE)


# ------- load files -------- #

# for cluster:
p_dat <- "/scratch/gpfs/clc6/data/"
p_derived <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_range <- "/scratch/gpfs/clc6/data/bd/ranges_2022_01_04/"
# for testing:
# p_dat <- p_derived
# p_dat <- "/Volumes/GoogleDrive/My Drive/data/"
# p_range <- "/Volumes/GoogleDrive/My Drive/data/bd/IUCN/ranges_2022_01_04/"



# load site_sf
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

site_sf <- st_read(paste0(p_dat, "sf/site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()

# plot(site_sf$geometry)


# my new stuff::
validation_runs <- 
  c(
    "_1_70", "_70_80", "_80_90", # done
    paste0("_", 90:98, "_", 90:98 + 1),
    paste0("_", 990:998, "_", 990:998 + 1),
    paste0("_", 9990:9999, "_", 9990:9999 + 1)
  )

runs_df <- 
  tibble(my_label = validation_runs) %>%
  mutate(width = str_count(my_label),
         index = case_when(
           width == 5 | width == 6 ~ 1,
           width == 8  ~ 2,
           width == 10 | width == 11 ~ 3,
         ))


# load and merge
bird_sf_validated <- lapply(runs_df$my_label, function(i) {
  load(paste0(p_dat, "bd/birds/bird_sf_validated", 
              i, ".RData"), verbose = TRUE)
  bird_sf_validated %>% 
    mutate(block = i)
}) %>% bind_rows()


# check that there are the right number of rows: should have 17397 rows
bird_sf_validated %>% 
  st_drop_geometry() %>%
  nrow()


# Save the full, validated version to a file:
save(bird_sf_validated, file = paste0(p_range, "bird_sf_validated", ".RData"))


# ------- crop -------- #
cat("crop", fill = TRUE)
tic()
bird_sites <- bird_sf_validated %>%
  st_intersection(., site_sf)
toc()

# how many species occur at each site?
bird_sites %>%
  st_drop_geometry() %>% 
  # filter(marine == "false") %>%
  group_by(site) %>%
  select(site, sci_name) %>%
  summarise(num_sp = length(unique(sci_name)))

# ------- save -------- #
cat("save files: birds", fill = TRUE)
write_csv(range_sites %>% st_drop_geometry(), 
          file = paste0(p_derived, "bird_sites.csv"))
save(bird_sites, file = paste0(p_derived, "bird_sites.RData"))



