# --------------------------------------------------------------- #
#
# Main Util script: load all functions in other util scripts
# 
# --------------------------------------------------------------- #

run_label <- 
  "_2022_02_07"
# "_2022_01_31" 
# "_2021_03_13"

aoh_run_date <- "_2022_04_16" #"_2021_12_12" # "_2021_12_05"


# pathnames -----
p_proj <- "/Users/christophercrawford/work/projects/biodiversity_abn/"
p_derived <- paste0(p_proj, "derived/")
p_scripts <- paste0(p_proj, "scripts/")
p_output <- paste0(p_proj, "output/")
p_plots <- paste0(p_proj, "output/plots/")
p_tmp <- paste0(p_proj, "derived/tmp/")

p_dat <- "/Users/christophercrawford/Library/CloudStorage/GoogleDrive-clc6@princeton.edu/My Drive/data/"

p_ee <- "/Users/christophercrawford/Library/CloudStorage/GoogleDrive-chris.l.crawford@gmail.com/My Drive/ee/"


# from the original abandonment_trajectories directory
p_dat_derived <- "/Users/christophercrawford/work/projects/abandonment_trajectories/data_derived/"
p_derived2 <- paste0(p_dat_derived, run_label, "/derived_data/")




# load other util scripts ----
source(paste0(p_proj, "scripts/_util/_util_misc.R"))
source(paste0(p_proj, "scripts/_util/_util_functions.R"))
