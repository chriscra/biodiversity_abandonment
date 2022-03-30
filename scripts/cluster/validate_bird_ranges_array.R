# ------------------------------------------------------------ #
# Script for cropping vertebrate range maps to site locations
# Christopher Crawford, November 9th, 2021 (updated March 10, 2022)
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
list.files(p_range)

# 
# bird_sf <- st_read(dsn = paste0(p_range, "BOTW/BOTW.gdb"), layer = "All_Species")
# bird_sf_clip <- st_read(dsn = paste0(p_range, "bird_clip_qgis.shp"))
# # bird_sf <- st_read(dsn = paste0(p_derived, "sf/bird_clip_qgis.shp"))
# 
# 
# # add column for number of vertices:
# cat("add column for number of vertices", fill = TRUE)
# tic()
# bird_sf <- bird_sf %>%
#   mutate(vertices = mapview::npts(., by_feature = TRUE))
# toc()
# 
# # save attribute table:
# bird_table <- bird_sf %>% st_drop_geometry() %>% as_tibble()
# write_csv(bird_table, paste0(p_dat, 'bd/birds/bird_table.csv'))
# 
# bird_sf %>% st_drop_geometry() %>% as_tibble() %>%
#   mutate(block = case_when(
#     vertices <= quantile(bird_sf$vertices, 0.9) ~ 1,
#     vertices > quantile(bird_sf$vertices, 0.9) & 
#       vertices <= quantile(bird_sf$vertices, 0.91) ~ 2,
#     
#     
#   )) %>% 
#   filter(block > 0)
# rep(3:10, each = 10)

# tic()
# load(file = paste0(p_dat, "bd/birds/bird_sf_vertices.RData"), verbose = TRUE)
# toc()


# load site_sf
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

site_sf <- st_read(paste0(p_dat, "sf/site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()

# plot(site_sf$geometry)

# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])

# my new stuff::
validation_runs <- 
  c(
    # "_1_70", "_70_80", "_80_90", # done
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



for (my_label in runs_df %>% filter(index == indx) %>% .$my_label) {

# load the data subset
load(paste0(p_dat, "bd/birds/bird_sf_vertices", 
            my_label, ".RData"), 
     verbose = TRUE)

assign(x = "bird_sf", 
       bird_tmp
       # value = get(paste0("bird", my_label))
       )
rm(bird_tmp)
# rm(list = paste0("bird", my_label))


# ------- validate -------- #
# ------- and test which features are valid -------- #
cat("are the geometries valid?", fill = TRUE)
tic()
bird_sf <- bird_sf %>%
  mutate(is_valid = st_is_valid(., reason = TRUE))
toc() # 36 seconds, much longer for 80-90 (322 seconds)


# save(bird_sf, file = paste0(p_dat, "bd/birds/bird_sf_is_valid", my_label, ".RData"))

# separate into valid or not valid
bird_sf_valid <- bird_sf %>% filter(is_valid == "Valid Geometry")
bird_sf_invalid <- bird_sf %>% filter(is_valid != "Valid Geometry" | is.na(is_valid))

tic()
bird_sf_fixed <- bird_sf_invalid %>%
  st_cast("MULTIPOLYGON") %>% # to deal with MULTISURFACE issues
  st_make_valid()
toc() # 26 seconds for 1-70, 351 for 80-90

# combine both valid and invalid
bird_sf_validated <- bind_rows(bird_sf_valid, bird_sf_fixed)

# check that no rows were lost along the way
cat("Did all rows make it?", nrow(bird_sf) == nrow(bird_sf_validated))

save(bird_sf_validated, file = paste0(p_dat, "bd/birds/bird_sf_validated", my_label, ".RData"))
# 
# # ------- crop -------- #
# cat("crop", fill = TRUE)
# bird_sites <- bird_sf_validated %>%
#   # test_sites <- test_sf %>%
#   st_intersection(., site_sf)
# 
# # ------- save -------- #
# cat("save files: birds", fill = TRUE)
# 
# save(bird_sites, file = paste0(p_dat, "bd/birds/bird_sites", my_label, ".RData"))

}


