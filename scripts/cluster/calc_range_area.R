# ------------------------------------------------------------ #
# Script for calculating area of each species' range, range size quantiles, 
# cropping, and saving new files
# Christopher Crawford, March 22, 2022
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

# load vertebrate 
vert_list <- c("amp", "bird", "mam")
my_label <- "" #format(Sys.time(), "_%Y_%m_%d") # removing this after testing and confirming matches


for (vert in vert_list) {
  print(vert)
  # load sf file, after validation
  load(paste0(p_range,
              vert, "_sf_validated.RData"), 
       verbose = TRUE)
  
  # assign to common name:
  assign("range_sf_validated",  # ...and assign it to this
         get(paste0(vert, "_sf_validated")) # first, take this... ^^^
         )
  
  # update geometry column for birds
  if(vert == "bird") {
    range_sf_validated <- range_sf_validated %>%
      rename("geometry" = "Shape")
    }
  
  
  # calculate total range size across all polygons:
  tic()
  range_sf_validated <- range_sf_validated %>% 
    mutate(., area_km2 = st_area(.["geometry"]) %>% units::set_units(km^2))# area, calculated from polygons in long lat projection
  toc()
  
  
  # calculate total range area and range_size_quantile:
  range_tmp_total <- range_sf_validated %>%
    st_drop_geometry() %>%
    {if(vert == "bird") group_by(., sci_name) else group_by(., binomial)} %>%
    summarise(total_range_area = sum(area_km2, na.rm = TRUE), 
              n_polygons = n()) %>%
    arrange(total_range_area) %>%
    mutate(range_rank = row_number(),
           n_species = max(row_number()),
           range_size_quantile = range_rank / n_species)
  
  # join back to original sf 
  range_sf_validated <- left_join(range_sf_validated, range_tmp_total)
  
  # ------- crop -------- #
  cat("crop", fill = TRUE)
  range_sites <- range_sf_validated %>%
    # test_sites <- test_sf %>%
    st_intersection(., site_sf)
  
  # save
  # change names back
  assign(paste0(vert, "_sf_validated"), range_sf_validated)
  assign(paste0(vert, "_sites"), range_sites)
  
  
  write_csv(get(paste0(vert, "_sites")) %>% st_drop_geometry(), 
            file = paste0(p_derived, vert, "_sites", my_label, ".csv"))
  save(list = paste0(vert, "_sites"), 
       file = paste0(p_derived, vert, "_sites", my_label, ".RData"))
  save(list = paste0(vert, "_sf_validated"), 
       file = paste0(p_range, vert, "_sf_validated", my_label, ".RData"))
}
