#!/bin/bash
#SBATCH --job-name=lsm_array_12 # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=07:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=1-2                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=600G			                          # memory per cpu-core (4 GB is default). 
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%A" is replaced by the job ID and "%a" with the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# r script passing array job number to the script
Rscript /home/clc6/abandonment_trajectories/scripts/landscape_metrics_array_12.R $SLURM_ARRAY_TASK_ID


# Completed belarus on Della at 600 G. (needed about 4 hours, about 500 gb)
# Completed shaanxi on Adroit at 350 G.