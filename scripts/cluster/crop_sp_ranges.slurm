#!/bin/bash
#SBATCH --job-name=crop_sp_ranges                         # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=11:59:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=70G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=cluster/slurm_out_%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

# load any required modules
module load gdal/2.2.4
module load geos/3.7.2
module load proj/6.3.2


# set working directory
cd /home/clc6/biodiversity_abn/scripts

# r script passing array job number to the script
Rscript ./crop_sp_ranges.R


# before running, make sure the 
# 1. Rscript name is correct
# 2. site_df is uploaded
# 3. util_functions.R is up to date