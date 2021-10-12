#!/bin/bash
#SBATCH --job-name="remove_0s_array"                   # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=3:59:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --array=1-2                                       # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=80G			                            # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm/slurm_%x_%A_%a.out                # "%x" is the job name, "%A" is the job ID and "%a" is the array index.


echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)


module load gdal # this line loads gdal, which is required for sf

Rscript /home/clc6/abandonment_trajectories/scripts/remove_0s_array.R $SLURM_ARRAY_TASK_ID

# Notes ----------------------------------
# Run this with 
# sbatch /home/clc6/abandonment_trajectories/scripts/remove_0s_array.slurm

# took about 25 minutes and 49 gb for belarus, and 15 minutes and 22 gb for shaanxi. 
