#!/bin/bash
#SBATCH --job-name=aoh_hab3                              # create a name for the job
#SBATCH --nodes=1 					                              # node count
#SBATCH --ntasks=1	 				                              # total number of tasks across all nodes
#SBATCH --cpus-per-task=1                                 # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --time=12:00:00 			                              # total run time limit (HH:MM:SS)
#SBATCH --array=1-21                                      # array ID variables (i.e. like "i")
#SBATCH --mem-per-cpu=80G			                          # memory per cpu-core (4 GB is default).
#SBATCH --mail-type=all				                            
#SBATCH --mail-user=clc6@princeton.edu
#SBATCH --output=slurm_out/%x_%A_%a.txt             # "%x" = job name, "%A" = job ID, "%a" = array index

echo "My SLURM_ARRAY_JOB_ID is $SLURM_ARRAY_JOB_ID."
echo "My SLURM_ARRAY_TASK_ID is $SLURM_ARRAY_TASK_ID"
echo "Executing on the machine:" $(hostname)

# set working directory
cd /home/clc6/biodiversity_abn/scripts

# r script passing array job number to the script, along with 
# a simple numeric index to determine which AOH run:
# 1) "full_lc" - 2) "abn_lc" - 3) "full_iucn" - 4) "abn_iucn" - 5)"potential_abn_iucn"
Rscript ./aoh.R $SLURM_ARRAY_TASK_ID 3


# # send scripts
# cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/
# scp ./cluster/aoh[1-5].* clc6@della:/home/clc6/biodiversity_abn/scripts/
# scp ./_util/_util_functions.R clc6@della:/home/clc6/biodiversity_abn/scripts/_util/
# 
# # send files
# cd /Users/christophercrawford/work/projects/biodiversity_abn/derived/
# scp ./species_ranges/vert_sites.RData clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp ./species_ranges/species_ranges.RData clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp ./species_list.csv clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp ./iucn*.csv clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/
# scp ./jung_hab_type_area_df.csv clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/

# scp ./elevation/*_srtm_crop.tif clc6@della:/scratch/gpfs/clc6/data/elevation/
# scp -r ./site_area_ha/ clc6@della:/scratch/gpfs/clc6/data/

# $ seff <JobID>

# To submit a specific set of array sub-jobs, use the comma delimiter in the array index list.
# Just make sure to turn off the array and mem per cpu variables
# You can do a similar thing to set specific memory allocation.
# Use the following in the future to manually set memory:

# final memory allocation:
# sbatch --array [1,8,10] --mem-per-cpu=150G aoh.slurm
# sbatch --array [2,4,5,7,11] --mem-per-cpu=125G aoh.slurm
# sbatch --array [3,6,9] --mem-per-cpu=80G aoh.slurm



# paste0(p_derived, "aoh_tmp", time_stamp, "_c", core, ".csv")

# download slurm_out:
# cd /Users/christophercrawford/work/projects/biodiversity_abn/scripts/cluster
# scp -r clc6@della:/home/clc6/biodiversity_abn/scripts/slurm_out/ .

# download results from della
# replace [] with time_stamp
# cd /Users/christophercrawford/work/projects/biodiversity_abn/derived/
# scp clc6@della:/scratch/gpfs/clc6/biodiversity_abn/derived/aoh_abn_lc_tmp_[]_c* ./aoh/


