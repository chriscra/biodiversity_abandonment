# Create potential full IUCN layer:


# load libraries
cluster_packages <- c("data.table", "tictoc", "terra",
                      "sf", "tidyverse", "rgdal", "arrow", "dtraster")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat           <- "/scratch/gpfs/clc6/data/"
p_derived       <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_range         <- "/scratch/gpfs/clc6/data/bd/"
p_input_rasters <- "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_tmp           <- "/scratch/gpfs/clc6/biodiversity_abn/derived/tmp/"

terraOptions(tempdir = p_tmp)

site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))
site <- site_df$site[indx] # set site:

run_label <- "_2022_02_07"


lcc_iucn_habitat <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/", 
              # site, 
              site_df$site[i],
              "_lcc_iucn_habitat.tif"))
})
names(lcc_iucn_habitat) <- site_df$site


# IUCN habitat types interpolated to only *potential* abandoned pixels
potential_abn_lcc_iucn_habitat <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_potential_abn_lcc_iucn_habitat",
              run_label, ".tif"))
})
names(potential_abn_lcc_iucn_habitat) <- site_df$site


# 
tic()
lapply(c(7, 11), function(site_index) {
  cat("Build potential habitat layer for full site extent using terra::cover(), for",
      site_df$site[site_index], fill = TRUE)
  lcc_iucn_habitat_potential <- 
    cover(x = potential_abn_lcc_iucn_habitat[[site_index]], 
          y = lcc_iucn_habitat[[site_index]][[paste0("y", 1987:2017)]],
          filename = paste0(p_derived, "lcc_iucn_habitat/",
                            site_df$site[site_index], 
                            "_lcc_iucn_habitat_potential",
                            run_label, ".tif"), 
          overwrite = TRUE,
          names = names(potential_abn_lcc_iucn_habitat[[site_index]]))
  cat(site_df$site[site_index], "done!", fill = TRUE)
})
toc()

# load back in:
lcc_iucn_habitat_potential <- lapply(1:11, function(i){
  rast(paste0(p_derived, "lcc_iucn_habitat/",
              site_df$site[i], "_lcc_iucn_habitat_potential",
              run_label, ".tif"))
})
names(lcc_iucn_habitat_potential) <- site_df$site

plot(lcc_iucn_habitat_potential[[11]][[31]])
plot(lcc_iucn_habitat[[11]][[31]])
