# ---------------------------------------------------------------
#
# Master Util script: load all functions in other util scripts
# 
# ---------------------------------------------------------------

# pathnames
p_proj <- "/Users/christophercrawford/work/projects/biodiversity_abn/"
p_derived <- paste0(p_proj, "derived/")
p_scripts <- paste0(p_proj, "scripts/")
p_output <- paste0(p_proj, "output/")
p_plots <- paste0(p_proj, "output/plots/")
p_tmp <- paste0(p_proj, "derived/tmp/")

p_dat <- "/Volumes/GoogleDrive/My Drive/data/"
p_ee <- "/Volumes/GoogleDrive-107266184156135828486/My Drive/ee/"

# from the original abandonment_trajectories directory
p_dat_derived <- "/Users/christophercrawford/work/projects/abandonment_trajectories/data_derived/"




# load other util scripts ----
source(paste0(p_proj, "scripts/util/_util_misc.R"))
source(paste0(p_proj, "scripts/util/_util_functions.R"))
