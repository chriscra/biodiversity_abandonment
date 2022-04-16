#!/bin/bash
#SBATCH --job-name=noncrop_precrop_mask                              # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=6:00:00 			                              # total run time limit (HH:MM:SS)
# SBATCH --array=1-11                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=60G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

# set working directory
cd /home/clc6/biodiversity_abn/scripts

# r script passing array job number to the script
Rscript ./noncrop_precrop_mask.R $SLURM_ARRAY_TASK_ID


# sbatch --array [1,8,10] --mem-per-cpu=220G noncrop_precrop_mask.slurm
# sbatch --array [2,7,11] --mem-per-cpu=170G noncrop_precrop_mask.slurm
# sbatch --array [4,5] --mem-per-cpu=130G noncrop_precrop_mask.slurm
# sbatch --array [6] --mem-per-cpu=110G noncrop_precrop_mask.slurm
# sbatch --array [3] --mem-per-cpu=80G noncrop_precrop_mask.slurm
# sbatch --array [9] --mem-per-cpu=60G noncrop_precrop_mask.slurm

# second attempt, after OOM errors
# sbatch --array [1,6,7,8,10,11] --mem-per-cpu=150G noncrop_precrop_mask.slurm

# notes:
# 1 - OOM - but during masking.
# 2 - worked fine.
# 3 - worked fine.
# 4 - worked fine.
# 5 - worked fine.
# 6 - OOM, but seems to have written the mask to SpatRaster successfully? I got a tic message ("convert dt to spatraster ('noncrop_precrop_mask_tmp'): 225.182 sec elapsed") and I'm getting message for loading Max rasters. Weird.
# 7 - seems to have largely worked fine except for a weird error at the end.
# 8 - OOM.
# 9 - worked fine
# 10 - OOM, but again, seems to have concerted the dt to spatraster... see if it'll load. OOM might have happened during masking.
# 11 - OOM.

# ----------------------------------------------------------------------------------------- #
# # send scripts
# cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/
# scp ./cluster/noncrop_precrop_mask* clc6@della:/home/clc6/biodiversity_abn/scripts/
# scp ./_util/_util_functions.R clc6@della:/home/clc6/biodiversity_abn/scripts/_util/
# scp ./_util/_util_misc.R clc6@della:/home/clc6/biodiversity_abn/scripts/_util/
# scp /Users/christophercrawford/work/projects/abandonment_trajectories/scripts/_util/_util_functions.R clc6@della:/home/clc6/abandonment_trajectories/scripts/_util/


# 
# # send files
# cd /Users/christophercrawford/work/projects/biodiversity_abn/derived/
# scp ./abn_lcc/*abn_lcc* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/abn_lcc/
# scp ./abn_lcc/*max*abn_lcc* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/abn_lcc/
# scp ./lcc_iucn_habitat/*lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/
# scp ./lcc_iucn_habitat/*max*lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/
# scp ./lcc_iucn_habitat/*potential_abn_lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/
# scp ./lcc_iucn_habitat/*max_potential_abn_lcc_iucn_habitat* clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/lcc_iucn_habitat/


# download slurm_out:
# cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/
# scp -r clc6@della:/home/clc6/biodiversity_abn/scripts/slurm_out/ ./cluster


