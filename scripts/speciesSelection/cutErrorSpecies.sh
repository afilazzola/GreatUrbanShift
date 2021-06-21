#!/bin/bash
#SBATCH --account=def-sapna   # specify account
#SBATCH --nodes=1              # number of cluster
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16    ## specify more nodes to be available
#SBATCH --time=01:59:00         # time for operation to run 
#SBATCH --mem-per-cpu=4G 				## specify memory for operation
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=CutErrors
#SBATCH --error=CutErrors.%J.stdout
#SBATCH --output=CutErrors.%J.stderr




module load netcdf
module load udunits
module load r
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12
module load grass


Rscript ~/projects/def-sapna/afila/GreatUrbanShift/scripts/cutErrorSpecies.r
