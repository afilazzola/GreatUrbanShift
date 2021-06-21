

## List file paths of basename only

## All species
ls -d ~/projects/def-sapna/afila/GreatUrbanShift/data/speciesOcc/*.csv > AllSpeciesFiles.txt
## Processed species
ls -d ~/projects/def-sapna/afila/GreatUrbanShift/out/models/*.csv | xargs -n 1 basename > ~/projects/def-sapna/afila/GreatUrbanShift/out/ProcessedSpecies.txt

## replace model text in files
awk  'gsub("Model", "", $0)'  ~/projects/def-sapna/afila/GreatUrbanShift/out/ProcessedSpecies.txt > ~/projects/def-sapna/afila/GreatUrbanShift/out/New.txt

grep -v -f New.txt AllSpeciesFiles.txt

