#!/bin/bash
#SBATCH --job-name="Calculate landscape metrics; Shaanxi, run 1" # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=4:00:00 			                            # total run time limit (HH:MM:SS)
#SBATCH --mem-per-cpu=50G			                            # memory per cpu-core (4 GB is default). Can alternatively set total memory with the command '--mem=61G'. Make sure not to use both
#SBATCH --mail-type=begin			                            # send email when job begins
#SBATCH --mail-type=end				                            # send email when job ends
#SBATCH --mail-user=clc6@princeton.edu

module load gdal # this line loads gdal, which is required for sf

Rscript /home/clc6/abandonment_trajectories/scripts/landscape_metrics_s1.R

# Notes ----------------------------------
# Run this with 
# sbatch /home/clc6/abandonment_trajectories/scripts/landscape_metrics_s1.slurm

