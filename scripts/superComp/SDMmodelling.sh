#!/bin/bash
#SBATCH --account=def-sapna
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=30
#SBATCH --time=48:00:00
#SBATCH --mem=0
#SBATCH --mail-user=alex.filazzola@outlook.com
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL


module load grass
module load netcdf
module load udunits
module load r
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12


Rscript ~/projects/def-sapna/afila/UrbanSensitivity/scripts/SDMmodelling.r
