#!/bin/bash
#SBATCH --job-name=lsm_array_by_layer_8_b # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=03:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=1-3                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=350G			                          # memory per cpu-core (4 GB is default)
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%x" is the job name, "%A" is the job ID and "%a" is the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# r script passing array job number to the script
Rscript /home/clc6/abandonment_trajectories/scripts/landscape_metrics_array_by_layer_8_b.R $SLURM_ARRAY_TASK_ID


## note, did the time last long enough?
# The time wasn't an issue, but still don't have enough memory for the edge, perimeter metrics. 
# Consistently get an error message of: 
## Error: cannot allocate vector of size 193.7 Gb
## Execution halted
## Warning message:
## system call failed: Cannot allocate memory 
