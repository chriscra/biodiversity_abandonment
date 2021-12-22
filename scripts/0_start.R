# --------------------------------------------------------------- #
#
# Start File: load packages, functions, and pathnames
# Abandonment & Biodiversity
# 
# --------------------------------------------------------------- #
p_proj <- "/Users/christophercrawford/work/projects/biodiversity_abn/"

# ------------------------- #
# list the needed packages and load the libraries ----
# ------------------------- #

needed_packages <- c(
  "data.table", "raster", "terra", "rgdal", "sp", "sf", "mapview", 
  "gdalUtils", "gdata", "GISTools", "rgeos", "lwgeom", "fasterize",
  "ncdf4", # for opening NetCDF spatial files
  
  "tidyverse", "lobstr", #"pryr", 
  "reshape2", "rlang",
  # tidyverse includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
  
  "rticles",
  "XML",
  
  # modeling:
  "tidymodels", # metapackage including: rsample, parsnip, recipes, workflows, tune, yardstick, broom, & dials
  "lme4", "brms", 
  "broom.mixed", # nb: broom comes preloaded in the tidymodels; this package is specifically intended for mixed effects models
  "lamW", # to calculate the Lambert's W function, for calculating time to various proportions in abn decay models
  "equatiomatic",
  "olsrr", # for model diagnostics
  "car", "multcomp", # recommended by Oscar Torres-Reyna for stats diagnostics
  
  # visualization:
  "rasterVis", "RColorBrewer", "viridis", "scales", "colorspace",
  "patchwork", "cowplot", # for combining multiple plots
  "ggridges",
  "ggpubr", # for extracting legends
  "animation", "magick", "ggrepel",
  "plotly", # plotly makes interactive graphics!
  
  # development
  "tictoc",  "magrittr", "parallel", "knitr", 
  "devtools", "Hmisc", "acepack",
  
  # spatial, ecological packages, extra:
  "rnaturalearth", "smoothr", "spatialEco", "maps", #"rangeBuilder",
  # "rredlist",
  "landscapemetrics", "landscapetools",
  
  # rmarkdown
  "rmarkdown", "bookdown", "knitr", "kableExtra"
  )

extra_packages <- c("")


# cluster_packages <- c(
#   "data.table", "raster", "rgdal", "sp", "sf",
#   "gdalUtils", "gdata", "GISTools", "rgeos", "lwgeom", "fasterize",
#   "tidyverse", "lobstr", "pryr", "reshape2",  # tidyverse includes ggplot2, dplyr, tidyr, readr, purrr, tibble, stringr, and forcats
#   "tictoc",  "magrittr", "parallel",
#   "devtools", "Hmisc", "dtraster"
# )

# nice to have, but not needed:
# c("ggnewscale", )

# ------------------------- #
# function to install only missing packages
# ------------------------- #
install_missing_packages <- function(list_of_packages) {
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[ , "Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, repo = 'https://cloud.r-project.org/')
  } 
  sapply(list_of_packages, library, character.only = TRUE)
}

install_missing_packages(needed_packages)
# update.packages(needed_packages) # every now and then a good idea to update the packages



#install.packages(needed_packages) # old method
# install <- lapply(needed_packages, library, character.only = TRUE) # load them



# ------------------------- #
# Additional development packages, to be installed with devtools: ----
# ------------------------- #

# ---- #
# Lyndon's packages

# devtools::install_github("ldemaz/dtraster")
# devtools::install_github("PrincetonUniversity/lmisc")
# devtools::install_github("PrincetonUniversity/agroEcoTradeoff@devel")

# ---- #
# rnaturalearth extras

# devtools::install_github("ropensci/rnaturalearthdata")
# devtools::install_github("ropensci/rnaturalearthhires")

# ---- #
# others
# devtools::install_github('BigelowLab/dismotools')

# colorblindr 
# remotes::install_github("wilkelab/cowplot")
# remotes::install_github("clauswilke/colorblindr")

# ---- #
# load
github_packages <- c(
  "dtraster", "lmisc", # "agroEcoTradeoff",
  "rnaturalearthdata", "rnaturalearthhires",
  # "dismotools", 
  "cowplot", 
  "colorblindr")

github_packages_inst <- sapply(github_packages, library, character.only = TRUE) # load them



# ----- load other functions ----
source(paste0(p_proj, "scripts/util/_util_main.R"))

