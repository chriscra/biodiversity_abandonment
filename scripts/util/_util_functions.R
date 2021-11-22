# --------------------------------------------------------------- #
#
# Spatial functions
# 
# --------------------------------------------------------------- #

# ------------------------- #
# convert a data.table to a SpatRaster, via an intermediary data.frame, trimming NA border
# ------------------------- #
# analogous to Lyndon's function dtraster::dt_to_raster()
# note that this process introduces a single cell border on the top and right edge of the SpatRaster filled with NA values. 
# introducing terra::trim() fixes this issue. 
# I have confirmed that the spatraster %>% dt_to_spatraster() %>% spatraster_to_dt() workflow results in an identical spatraster.

dt_to_spatraster <- function(dt){
  spt <- dt %>%
    as.data.frame() %>%
    terra::rast(
      type = "xyz",
      crs = "+proj=longlat +datum=WGS84 +no_defs"
    ) %>% 
    terra::trim()
  
  spt
}


# ------------------------- #
# convert a SpatRaster into a data.table, via an intermediary data.frame
# ------------------------- #
# analogous to Lyndon's function dtraster::as.data.table.raster()

spatraster_to_dt <- function(spt, xy_switch = TRUE) {
  dt <- spt %>%
    terra::as.data.frame(
      na.rm = FALSE, # na.rm=TRUE is default
      xy = xy_switch) %>% 
    as.data.table()
  
  dt
}



#


# --------------------------------------------------------------- #
#
# Miscellaneous functions
# 
# --------------------------------------------------------------- #


# ------------------------- #
# calculate the sizes of items in the environment
# ------------------------- #
env_size <- function(workspace = ls()) {
  size = 0
  for (x in workspace){
    thisSize = obj_size(get(x))
    size = size + thisSize
    message(x, " = ", appendLF = F); print(thisSize, units='auto')
  }
  message("total workspace is ", appendLF = F); print(size, units='auto')
}

# ------------------------- #
# return a tibble of objects and their sizes
# ------------------------- #

get_sizes <- function(stuff) {
  lapply(stuff, function(i) {
    tibble(object = i, size = obj_size(get(i)))
  }) %>% bind_rows() %>% arrange(desc(size)) %>% mutate(mb = size / 1024^2)
}

# 
# object_sizes <- lapply(ls(), function(i) {
#   tibble(object = i, size = obj_size(get(i)))
# }) %>% bind_rows()
# 

# ------------------------- #
# capitalize name
# ------------------------- #
capwords <- function(s, strict = FALSE) {
  cap <- function(s) {
    paste(
      toupper(substring(s, 1, 1)), 
      {s <- substring(s, 2); if(strict) tolower(s) else s},
      sep = "", collapse = " "
    )
  }
  sapply(strsplit(s, split = " "), 
         cap, 
         USE.NAMES = !is.null(names(s))
  )
}