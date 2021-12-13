# Troubleshooting script
# Christopher Crawford, Princeton University, December 12th, 2021

# load libraries
cluster_packages <- c("data.table", "tictoc", "raster", "terra",
                      "sf", "fasterize", "dtraster",
                      "tidyverse", "rgdal")

install_pkg <- lapply(cluster_packages, library, character.only = TRUE)

# set paths:
p_dat           <- "/scratch/gpfs/clc6/data/"
p_derived       <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_range         <- "/scratch/gpfs/clc6/data/bd/"
p_input_rasters <- "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_tmp           <- "/scratch/gpfs/clc6/biodiversity_abn/derived/tmp/"


# source functions:
source("/home/clc6/biodiversity_abn/scripts/util/_util_functions.R")

# set terra autosave options:
terraOptions(tempdir = p_tmp)
rasterOptions(tmpdir = p_tmp)

site_index <- 7

# ------- load files -------- #
site_df <- read.csv(file = paste0(p_dat, "site_df.csv"))

# load lc and lcc rasters
lc <- lapply(1:11, function(i) {terra::rast(paste0(p_input_rasters, site_df$site[i], ".tif"))})
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
lcc <- lapply(1:11, function(i) {terra::rast(paste0(p_input_rasters, site_df$site[i], "_clean.tif"))})
names(lcc) <- site_df$site

plot(lc[[9]][["y2000"]])
plot(lcc[[9]][["y2000"]])

# compare the rasters
for(i in 1:11) {
  cat(site_df$site[i], fill = T)
  # print(compareGeom(lc[[i]], age_t[[i]]))
  print(ext(lc[[i]]))
  print(ext(lcc[[i]]))
  print(ncell(lc[[i]]))
  print(ncell(lcc[[i]]))
}



# trimming seems to make them equal extents and everything again, 
# so might be easiest to just use the dt_to_raster() function I had been using before
r <- lc[[7]][[31]]
r2 <- lcc[[7]][[31]]

rt <- terra::trim(r)
r2t <- terra::trim(r2)
ncell(r)
ncell(r2)

ncell(rt)
ncell(r2t)


site_index <- 11

# read in the cleaned data.tables
# for(i in 1:11) {
#   site_index <- i
print(site_df$site[site_index])
# lc_dt <- fread(file = paste0(p_input_rasters, site_df$site[site_index], ".csv"),
#                select = c(1,2))

lcc_dt <- fread(file = paste0(p_input_rasters, site_df$site[site_index], ".csv"), 
                # "_clean.csv")#, 
                select = c(1,2))



print(nrow(lc_dt) == nrow(lcc_dt))

print(all.equal(lc_dt[,.(x)], lcc_dt[,.(x)]))
# }

# are they the same number of cells?
lc_dt # now, this is the temporal_filtered version
lc_dt[, .N, by = y1987]
lcc_dt




all.equal(lc_dt[,.(x, y)], lcc_dt[,.(x, y)])



# now, convert the cleaned data.table back to a spatraster:
tic("translate cleaned lc_dt to raster")
# rt <- dt_to_spatraster(dt = lc_dt)
rt
toc()

tic()
rr <- dt_to_raster(lc_dt, crs("+proj=longlat +datum=WGS84 +no_defs"))
toc()

ncell(r)
ncell(rt)
ncell(rr)


# write new cleaned lc raster to file
tic("write new cleaned lc raster to file")
writeRaster(r, 
            filename = paste0(p_input_rasters,
                              site_df$site[site_index], "_clean.tif"),
            overwrite = TRUE,
            names = names(r))
toc()

cat(paste0("Success! Converted cleaned dt back to SpatRaster, and saved as file, for site: ", site_df$site[site_index]), fill = TRUE)




