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

# load vertebrate 
vert_list <- c("amp", "bird", "mam", "rep", "gard")


# i <- 3
for (i in c(3)) {
  
  vert <- vert_list[i]

  # original shapefiles.
  
  # amp_sf <- st_read(dsn = paste0(p_range,"AMPHIBIANS/AMPHIBIANS.shp"))
  # bird_sf <- st_read(dsn = paste0(p_range,"BOTW/BOTW.gdb"), layer = "All_Species")
  # mam_sf <- st_read(paste0(p_range,"MAMMALS/MAMMALS.shp"))
  # rep_sf <- st_read(dsn = paste0(p_range,"REPTILES/REPTILES.shp"))
  # gard <- st_read(dsn = paste0(p_dat, "/bd/GARD/GARD1.1_dissolved_ranges/modeled_reptiles.shp"))
  
  # Amphibians
  if(vert == "amp") {
    # load(paste0(p_range, "amp_valid_prepped.RData"), verbose = TRUE) 
    # assign("range_sf", amp_valid)
    # rm(amp_valid)
    range_sf <- st_read(dsn = paste0(p_range,"AMPHIBIANS/AMPHIBIANS.shp"))
  }
  
  # Birds
  if(vert == "bird") {
    # load(paste0("/scratch/gpfs/clc6/data/bd/", "bird_valid_prepped.RData"), verbose = TRUE) 
    # assign("range_sf", bird_valid)
    # rm(bird_valid)
    range_sf <- st_read(dsn = paste0(p_range,"BOTW/BOTW.gdb"), layer = "All_Species")
  }
  
  # Mammals
  if(vert == "mam") {
    # load(paste0(p_range, "mam_valid_prepped.RData"), verbose = TRUE) 
    # assign("range_sf",mam_valid)
    # rm(mam_valid)
    range_sf <- st_read(paste0(p_range,"MAMMALS/MAMMALS.shp"))
  }
  
  # Reptiles (IUCN)
  if(vert == "rep") {
    range_sf <- st_read(dsn = paste0(p_range,"REPTILES/REPTILES.shp"))
  }
  
  # Reptiles (GARD)
  if(vert == "gard") {
    load(paste0(p_dat, "bd/gard_prep.RData"), verbose = TRUE) 
    assign("range_sf", gard_prep)
    rm(gard_prep)
    range_sf <- range_sf %>% rename("binomial" = Binomial)
  }
  
  names(range_sf)
  # st_crs(range_sf) <- st_crs(range_sf)
  # st_crs(range_sf) <- st_crs(range_sf)
  
  
  # ------- filter to those species I'm interested in ------------- #
  # see: https://nc.iucnredlist.org/redlist/resources/files/1539614211-Mapping_attribute_codes_v1.16_2018.pdf
  # 1a. filter to presence, defaults to code 1 (extant species only). Other unused codes are:
  # ------ 2. Probably Extant (discontinued, ambiguous), 3. Possibly Extant,
  # ------ 4. Possibly Extinct, 5. Extinct, 6. Presence Uncertain.
  # 1b. filter to origin, defaults to codes 1 & 2 (native and reintroduced species respectively).
  # ------ Other unused codes are: 3. Introduced, 4. Vagrant, 5. Origin Uncertain, 6. Assisted Colonisation.
  # 1c. for non-bird species, filter by marine, defaults to "FALSE" (all species but marine ones)
  
  # perhaps leave this filtering step to later on in the process, so I can test multiple scenarios 
  # 1d. filter species with range seasonality. Defaults to c(1,2,3).
  # ------ Codes are: "Resident" (1), "Breeding" (2), "Non-breeding Season" (3),
  # ------ Passage (4), and Seasonal Occurrence Uncertain (5).
  # 1e. filter out Extinct or Extinct in the Wild species ("EX", "EW")
  
  cat("filter ranges", fill = TRUE)
  cat("before filtering ranges, nrow =", nrow(range_sf), fill = TRUE)
  
  if(vert != "gard") {
    range_sf <- range_sf %>%
      filter(presence == 1,
             origin %in% c(1, 2), 
             seasonal %in% c(1, 2, 3, 4) # now including passage areas too
      ) %>%
      {if (vert != "bird") filter(., 
                                  !category %in% c("EW", "EX"),
                                  marine == "false") else .}  # should double check that this is the same for all vert groups
  }
  
  cat("after filtering ranges, nrow =", nrow(range_sf), fill = TRUE)
  
  
  # ------- validate -------- #
  # ------- and test which features are valid -------- #
  cat("validate", fill = TRUE)
  tic()
  range_sf <- range_sf %>%
    # test_sf <- range_sf[1:100,] %>%
    mutate(vertices = mapview::npts(., by_feature = TRUE),
           pre_valid_reasons = st_is_valid(., reason = TRUE)) %>%
    st_make_valid() %>%
    mutate(post_valid_reasons = st_is_valid(., reason = TRUE))
  toc()
  
  cat("check out results:", fill = TRUE)
  range_sf %>%
    # test_sf %>%
    st_drop_geometry() %>%
    select(binomial, pre_valid_reasons, post_valid_reasons)
  
  range_sf %>%
    # test_sf %>%
    st_drop_geometry() %>% select(pre_valid_reasons) %>% unique()
  
  range_sf %>%
    # test_sf %>%
    st_drop_geometry() %>% select(post_valid_reasons) %>% unique()
  
  # ------- crop -------- #
  cat("crop", fill = TRUE)
  range_sites <- range_sf %>%
    # test_sites <- test_sf %>%
    st_intersection(., site_sf)
  
  
  cat("How many", vert, "species occur at each site?", fill = TRUE)
  
  # how many species occur at each site?
  range_sites %>%
    st_drop_geometry() %>% 
    # filter(marine == "false") %>%
    group_by(site) %>%
    select(site, binomial) %>%
    summarise(num_sp = length(unique(binomial)))
  
  
  # ------- save -------- #
  cat("save files", fill = TRUE)
  
  write_csv(range_sites %>% st_drop_geometry(), 
            file = paste0(p_derived, vert, "_sites.csv"))
  # st_write(range_sites, paste0(p_derived, vert, "_sites.shp")) # 
  save(range_sites, file = paste0(p_derived, vert, "_sites.RData"))
  save(range_sf, file = paste0(p_range, vert, "_sf_valid.RData"))
  
}



