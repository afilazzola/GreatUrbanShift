#!/bin/bash
#SBATCH --account=def-sapna
#SBATCH --ntasks=9
#SBATCH --time=6:00:00
#SBATCH --mem-per-cpu=2G
#SBATCH --mail-user=alex.filazzola@outlook.com
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL


module load grass
module load netcdf
module load udunits
module load r
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12


Rscript ~/projects/def-sapna/afila/GreatUrbanShift/scripts/speciesListCreation.r