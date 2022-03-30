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

# load site_sf
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

site_sf <- st_read(paste0(p_dat, "sf/site_sf.shp"))
st_crs(site_sf) <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
site_sf <- site_sf %>% st_make_valid()

# plot(site_sf$geometry)

# # array set up -------
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])


bird_sf <- st_read(dsn = paste0(p_range, "BOTW/BOTW.gdb"), layer = "All_Species")
bird_sf_clip <- st_read(dsn = paste0(p_range, "bird_clip_qgis.shp"))
# bird_sf <- st_read(dsn = paste0(p_derived, "sf/bird_clip_qgis.shp"))


# save attribute table:
bird_table <- bird_sf %>% st_drop_geometry() %>% as_tibble()
write_csv(bird_table, paste0(p_dat, 'bd/birds/bird_table.csv'))

save(bird_sf, file = paste0(p_dat, "bd/birds/bird_sf.RData"))

# add column for number of vertices:
cat("add column for number of vertices", fill = TRUE)
tic()
bird_sf <- bird_sf %>%
  mutate(vertices = mapview::npts(., by_feature = TRUE))
toc()

# save new file with vertices
save(bird_sf, file = paste0(p_dat, "bd/birds/bird_sf_vertices.RData"))


bird_sf <- bird_sf %>% 
  arrange(desc(vertices)) %>% 
  mutate(vert_id = 1:n()) %>%
  select(vert_id, everything())

# Save chunk based on number of vertices
summary(bird_sf$vertices)

# save groups of polygons to process later:

bird_tmp <- bird_sf %>% 
  filter(vertices <= quantile(.$vertices, 0.7))
save(bird_tmp, file = paste0(p_dat, "bd/birds/bird_sf_vertices_1_70.RData"))

bird_tmp <- bird_sf %>% 
  filter(vertices > quantile(bird_sf$vertices, 0.7),
         vertices <= quantile(bird_sf$vertices, 0.8))

save(bird_tmp, file = paste0(p_dat, "bd/birds/bird_sf_vertices_70_80.RData"))

bird_tmp <- bird_sf %>% 
  filter(vertices > quantile(bird_sf$vertices, 0.8),
         vertices <= quantile(bird_sf$vertices, 0.9))

save(bird_tmp, file = paste0(p_dat, "bd/birds/bird_sf_vertices_80_90.RData"))

# 90-98th percentiles
for(i in 90:98) {
  bird_tmp <- bird_sf %>% 
    filter(vertices > quantile(bird_sf$vertices, i/100),
           vertices <= quantile(bird_sf$vertices, (i + 1)/100))
  
  save(bird_tmp, 
       file = paste0(p_dat, "bd/birds/bird_sf_vertices_", i, "_", i+1,".RData"))
}

# 990-998th percentiles
for(i in 990:998) {
  bird_tmp <- bird_sf %>% 
    filter(vertices > quantile(bird_sf$vertices, i/1000),
           vertices <= quantile(bird_sf$vertices, (i + 1)/1000))
  
  save(bird_tmp, 
       file = paste0(p_dat, "bd/birds/bird_sf_vertices_", i, "_", i+1,".RData"))
}

# 9990-9999th percentiles
for(i in 9990:9999) {
  bird_tmp <- bird_sf %>% 
    filter(vertices > quantile(bird_sf$vertices, i/10000),
           vertices <= quantile(bird_sf$vertices, (i + 1)/10000))
  
  save(bird_tmp, 
       file = paste0(p_dat, "bd/birds/bird_sf_vertices_", i, "_", i+1,".RData"))
}



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

