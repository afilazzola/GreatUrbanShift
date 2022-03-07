#!/bin/bash
#SBATCH --account=def-sapna # specify account
#SBATCH --time=04:59:00      # time for operation to run 
#SBATCH --mem-per-cpu=12G    ## specify memory for operation
#SBATCH --cpus-per-task=3   # Specify processors
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=SDMurban
#SBATCH --error=SDMurban.%J_%a.stdout
#SBATCH --output=SDMurban.%J_%a.stderr
#SBATCH --array=0-760

parallel --record-env 

## Load modules
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.14
module load netcdf
module load udunits
module load r/4.1.2
module load grass

# ## Export dependencies
# export R_LIBS=( ~/R/x86_64-pc-linux-gnu-library/4.0)
export _JAVA_OPTIONS="-Xmx8g" ## specify memory heap for Java environment

IDX=$(( SLURM_ARRAY_TASK_ID * 3 ))

## list remaining files to process files to process
declare -a speciespaths=( ~/projects/def-sapna/afila/GreatUrbanShift/data/speciesOcc/*.csv ) ## full list
# speciespaths=($(grep -v -f ~/projects/def-sapna/afila/GreatUrbanShift/out/New.txt ~/projects/def-sapna/afila/GreatUrbanShift/out/AllSpeciesFiles.txt)) ## remaining list

TO_PROC=${speciespaths[@] :${IDX}:3}

## Run parallel 
parallel --env _ --jobs 3 Rscript ~/projects/def-sapna/afila/GreatUrbanShift/scripts/SDMmodelling.R {} ::: ${TO_PROC}