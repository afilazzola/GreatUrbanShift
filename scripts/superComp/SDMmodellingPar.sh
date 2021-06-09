#!/bin/bash
#SBATCH --account=def-sapna   # specify account
#SBATCH --ntasks=4                # specify number of tasks/processes to run
#SBATCH --time=02:59:00         # time for operation to run 
#SBATCH --mem-per-cpu=12G 				## specify memory for operation
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=SDMurban
#SBATCH --error=SDMurban.%J.stdout
#SBATCH --output=SDMurban.%J.stderr

module load netcdf
module load udunits
module load r
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12
module load grass


## list files to process
speciespaths=( ls ~/projects/def-sapna/afila/GreatUrbanShift/data/speciesOcc/*.csv) 

## Run parallel 
parallel --gnu --null Rscript ~/projects/def-sapna/afila/GreatUrbanShift/scripts/SDMmodellingSeq.R ::: "${speciespaths[@]}"


