#!/bin/bash
#SBATCH --job-name=lsm_array_vhimem_b # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=7:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=8-11                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=800G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%A" is replaced by the job ID and "%a" with the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# r script passing array job number to the script
Rscript /home/clc6/abandonment_trajectories/scripts/landscape_metrics_array_b.R $SLURM_ARRAY_TASK_ID




# email digest:
# Job ID: 32425953
# Array Job ID: 32425953_11
# Cluster: della
# User/Group: clc6/spia
# State: COMPLETED (exit code 0)
# Cores: 1
# CPU Utilized: 03:54:30
# CPU Efficiency: 99.61% of 03:55:25 core-walltime
# Job Wall-clock time: 03:55:25
# Memory Utilized: 484.99 GB
# Memory Efficiency: 60.62% of 800.00 GB