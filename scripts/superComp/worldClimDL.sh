#!/bin/sh


ssp=(ssp126 ssp370 ssp585)
gcm=(CanESM5 MRI-ESM2-0 MIROC6 CNRM-ESM2-1 IPSL-CM6A-LR BCC-CSM2-MR)

## Download files for future climate
for ssp in ssp126 ssp370 ssp585;do for gcm in CanESM5 HadGEM3-GC31-LL MIROC6 CNRM-ESM2-1 IPSL-CM6A-LR BCC-CSM2-MR;do curl -OL https://geodata.ucdavis.edu/cmip6/30s/${gcm}/${ssp}/wc2.1_30s_bioc_${gcm}_${ssp}_2081-2100.tif ;done;done

## download historical climate data
curl -OL https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_bio.zip