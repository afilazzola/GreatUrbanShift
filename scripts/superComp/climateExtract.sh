#!/bin/bash
#SBATCH --account=def-sapna # specify account
#SBATCH --ntasks=1    ## specify more nodes to be available
#SBATCH --time=00:59:00         # time for operation to run 
#SBATCH --mem=16G 				## specify memory for operation
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=ClimateExtract
#SBATCH --error=ClimateExtract.%J_%a.stdout
#SBATCH --output=ClimateExtract.%J_%a.stderr

## Load modules
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12
module load netcdf
module load udunits
module load r/4.1.0
module load grass

Rscript ~/projects/def-sapna/afila/GreatUrbanShift/scripts/ExtractWorldclim.r


scontrol update jobid=58351990 TimeLimit=01:00:00