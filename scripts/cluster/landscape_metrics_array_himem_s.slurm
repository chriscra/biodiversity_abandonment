#!/bin/bash
#SBATCH --job-name=lsm_array_himem_s # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=23:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=8-11                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=350G			                          # memory per cpu-core (4 GB is default). Can alternatively set total memory with the command '--mem=61G'. Make sure not to use both
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%A" is replaced by the job ID and "%a" with the array index.

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load gdal # this line loads gdal, which is required for sf

# r script passing array job number to the scrip
Rscript /home/clc6/abandonment_trajectories/scripts/landscape_metrics_array_s.R $SLURM_ARRAY_TASK_ID
