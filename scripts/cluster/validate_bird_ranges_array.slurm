#!/bin/bash
#SBATCH --job-name=validate_bird_ranges_array                         # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=11:59:00 			                              # total run time limit (HH:MM:SS)
#SBATCH --array=1-3                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=50G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

# set working directory
cd /home/clc6/biodiversity_abn/scripts

# r script passing array job number to the script
Rscript ./validate_bird_ranges_array.R $SLURM_ARRAY_TASK_ID


# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_functions.R is up to date

# sbatch --array [2] --time=1:59:00 --mem-per-cpu=45G crop_sp_ranges_array.slurm


# # Send scripts
# cd /Users/christophercrawford/work/projects/biodiversity_abn/
# scp ./scripts/cluster/validate_bird_ranges_array* clc6@della:/home/clc6/biodiversity_abn/scripts/

# # Send data
# scp ./derived/sf/bird_clip_qgis.* clc6@della:/scratch/gpfs/clc6/data/bd/ranges_2022_01_04

## download data:
# cd /Users/christophercrawford/work/projects/biodiversity_abn/derived/species_ranges
# scp clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/*sites.* ./
# scp clc6@della:/scratch/gpfs/clc6/data/bd/ranges_2022_01_04/*valid.RData ./


# download slurm_out:
# cd /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/
# scp -r clc6@della:/home/clc6/abandonment_trajectories/scripts/slurm_out/ ./cluster