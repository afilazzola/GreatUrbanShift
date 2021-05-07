#!/bin/bash
#SBATCH --account=def-sapna   # specify account
#SBATCH --ntasks=1    ## specify more nodes to be available
#SBATCH --time=02:59:00         # time for operation to run 
#SBATCH --mem=50G 				## specify memory for operation
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=SplitCSV
#SBATCH --error=SplitCSV.%J.stdout
#SBATCH --output=SplitCSV.%J.stderr


module load python
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12


python  ~/projects/def-sapna/afila/GreatUrbanShift/scripts/splitCSV.py
