#!/bin/bash
#SBATCH --job-name=lsm_two_2024                              # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=1:00:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=50G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
# echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

module load geos/3.12.0
module load proj/9.2.1
module load gdal/3.7.1

# set working directory
cd /home/clc6/biodiversity_abn/scripts

# r script passing array job number to the script
Rscript ./lsm_two_2024.R

# sbatch /home/clc6/biodiversity_abn/scripts/lsm_two_2024.slurm

# ----------------------------------------------------------------------------------------- #
# # send scripts
# cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/
# scp ./cluster/lsm_two_2024* clc6@della:/home/clc6/biodiversity_abn/scripts/
# scp ./_util/_util_functions.R clc6@della:/home/clc6/biodiversity_abn/scripts/_util/
# scp ./_util/_util_misc.R clc6@della:/home/clc6/biodiversity_abn/scripts/_util/
# 
# 
# # send / download files
# cd /Users/christophercrawford/work/projects/biodiversity_abn/derived/
# scp clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/frag*hypo* /Users/christophercrawford/work/projects/biodiversity_abn/derived/frag
# scp /Users/christophercrawford/work/projects/abandonment_trajectories/data_derived/age_rasters/_2022_02_07/*mask* clc6@della:/scratch/gpfs/clc6/abandonment_trajectories/data_derived/input_rasters/
# scp ./abn_lcc/*abn_lcc* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/abn_lcc/
# scp ./abn_lcc/*max*abn_lcc* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/abn_lcc/
# scp ./lcc_iucn_habitat/*lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/
# scp ./lcc_iucn_habitat/*max*lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/
# scp ./lcc_iucn_habitat/*potential_abn_lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/
# scp ./lcc_iucn_habitat/*max_potential_abn_lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/


# download slurm_out:
# cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/
# scp -r clc6@della:/home/clc6/biodiversity_abn/scripts/slurm_out/ ./cluster


