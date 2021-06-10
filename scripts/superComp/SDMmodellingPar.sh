#!/bin/bash
#SBATCH --account=def-sapna # specify account
#SBATCH --time=03:59:00      # time for operation to run 
#SBATCH --mem-per-cpu=8G    ## specify memory for operation
#SBATCH --cpus-per-task=3   # Specify processors
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=SDMurban
#SBATCH --error=SDMurban.%J_%a.stdout
#SBATCH --output=SDMurban.%J_%a.stderr
#SBATCH --array=0-754

module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12
module load netcdf
module load udunits
module load r
module load grass

IDX=$(( SLURM_ARRAY_TASK_ID * 3 ))
declare -a speciespaths=( ~/projects/def-sapna/afila/GreatUrbanShift/data/speciesOcc/*.csv )

## list files to process
TO_PROC=${speciespaths[@] :${IDX}:3}

## Run parallel 
parallel --jobs 3 Rscript ~/projects/def-sapna/afila/GreatUrbanShift/scripts/SDMmodellingSeq.R {} ::: ${TO_PROC}