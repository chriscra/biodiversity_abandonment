# saving large .csv files as .parquet files

library(arrow)
library(data.table)
library(tictoc)
library(tidyverse)

# set paths:
p_dat_derived   <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/"
p_derived       <- "/scratch/gpfs/clc6/biodiversity_abn/derived/"
p_input_rasters <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/"
p_output        <-    "/scratch/gpfs/clc6/abandonment_trajectories/output/"
p_raw_rasters   <-    "/scratch/gpfs/clc6/abandonment_trajectories/raw_rasters/"
p_tmp           <-    "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/tmp/"


# source functions:
source("/home/clc6/abandonment_trajectories/scripts/_util/_util_functions.R")


# set up parameters:
# data.frame of all sites contains information about sites
site_df <- read.csv(file = paste0(p_dat_derived, "site_df.csv"))
args <- commandArgs(TRUE) # access the slurm array variable
indx <- as.numeric(args[1])


file_paths <- c(
  list.files(path = "/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters",
             full.names = TRUE) %>%
    grep(".csv", ., value = TRUE) %>%
    gsub(".csv$", "", .),
  
  list.files(path = paste0(p_derived, "lcc_iucn_habitat"),
             full.names = TRUE) %>%
    # grep("shaanxi", ., value = TRUE) %>%
    grep(".csv", ., value = TRUE) %>%
    gsub(".csv$", "", .),
  
  list.files(path = paste0(p_derived, "abn_lcc"),
             full.names = TRUE) %>%
    # grep("shaanxi", ., value = TRUE) %>%
    grep(".csv", ., value = TRUE) %>%
    gsub(".csv$", "", .)
)


# Just loop through all .csvs and convert to parquet
lapply(
  file_paths %>%
    grep(site_df$site[indx], ., value = TRUE), 
  function(path) {
    
    tic.clearlog()
    tic(paste0("convert:", path))
    # load .csv
    dt <- fread(paste0(path, ".csv"))
    
    # save as parquet
    write_parquet(x = dt, paste0(path, ".parquet"))
    toc(quiet = TRUE, log = TRUE)
    
    print(tic.log(format = TRUE))
    # cat(paste0("converted:", path), fill = TRUE)
    })

