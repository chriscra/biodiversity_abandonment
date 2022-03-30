#!/bin/bash
#SBATCH --job-name=lsm_array_b # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=07:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=13-15                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=100G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%A" is replaced by the job ID and "%a" with the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# r script passing array job number to the script
Rscript /home/clc6/abandonment_trajectories/scripts/landscape_metrics_array_b.R $SLURM_ARRAY_TASK_ID




# Job notes:
# Job ID: 32465538
# Array Job ID: 32465538_15
# Cluster: della
# User/Group: clc6/spia
# State: COMPLETED (exit code 0)
# Cores: 1
# CPU Utilized: 01:12:33
# CPU Efficiency: 99.93% of 01:12:36 core-walltime
# Job Wall-clock time: 01:12:36
# Memory Utilized: 7.14 GB
# Memory Efficiency: 7.14% of 100.00 GB