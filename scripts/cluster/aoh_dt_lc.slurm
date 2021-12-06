#!/bin/bash
#SBATCH --job-name=aoh_dt_lc_full31                              # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=12:00:00 			                              # total run time limit (HH:MM:SS)
#SBATCH --array=1-27                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=80G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

# load any required modules
module load R/4.0.5 # load an older version of R to deal with {raster} issues
module load gdal/2.2.4
module load geos/3.7.2
module load proj/6.3.2

# set working directory
cd /home/clc6/biodiversity_abn/scripts

# r script passing array job number to the script
Rscript ./aoh_dt_lc.R $SLURM_ARRAY_TASK_ID

# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_functions.R is up to date

# # send scripts
# # cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/scripts/cluster/aoh_dt_lc* clc6@della:/home/clc6/biodiversity_abn/scripts/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/scripts/util/_util_functions.R clc6@della:/home/clc6/biodiversity_abn/scripts/util/
# 
# # send files
# # cd /Users/christophercrawford/work/projects/biodiversity_abn/derived
# scp /Users/christophercrawford/work/projects/biodiversity_abn/derived/species_ranges/vert_sites.RData clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/derived/species_ranges/species_ranges.RData clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/derived/species_list.csv clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/derived/iucn*.csv clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/derived/jung_hab_type_area_df.csv clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp /Users/christophercrawford/work/projects/biodiversity_abn/derived/elevation/*_srtm_crop.tif clc6@della:/scratch/gpfs/clc6/data/elevation/
# scp -r /Users/christophercrawford/work/projects/biodiversity_abn/derived/site_area_ha/ clc6@della:/scratch/gpfs/clc6/data/

# $ seff <JobID>

# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# Just make sure to turn off the array and mem per cpu variables
# You can do a similar thing to set specific memory allocation.
# Use the following in the future to manually set memory:

# final memory allocation:
# sbatch --array [1,8,10] --mem-per-cpu=150G aoh_dt_lc.slurm
# sbatch --array [2,4,5,7,11] --mem-per-cpu=125G aoh_dt_lc.slurm
# sbatch --array [3,6,9] --mem-per-cpu=80G aoh_dt_lc.slurm


# download from della with:
# scp clc6@della:/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/*result*2021_03_13* /Users/christophercrawford/Google\ Drive/_Projects/abandonment_trajectories/data_derived/_2021_03_13
# scp clc6@della:/scratch/network/clc6/abandonment_trajectories/data_derived/input_rasters/length_distill_df_2021_03_13.csv /Users/christophercrawford/Google\ Drive/_Projects/abandonment_trajectories/data_derived/_2021_03_13

# paste0(p_derived, "aoh_tmp", time_stamp, "_c", core, ".csv")

# download slurm_out:
# scp -r clc6@della:/home/clc6/biodiversity_abn/scripts/slurm_out/ /Users/christophercrawford/work/projects/biodiversity_abn/scripts/cluster

# download results
# scp clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/aoh_tmp_2021_12_05_c* /Users/christophercrawford/work/projects/biodiversity_abn/derived/aoh/


