## load libraries
import pandas as pd
import numpy as np

## specify columns to keep
keepCols = ["gbifID","datasetKey","phylum","class","order","family","genus","species","day","month","year","decimalLatitude","decimalLongitude"]
dtypeDic = {"gbifID": int, "datasetKey": "string","phylum": "string","class": "string" ,"order": "string" ,"family": "string", 
           "genus": "string","species": "string","day": "string","month": "string","year": "string","string": "string","string": "string"}

### Load in species list
sppList = pd.read_csv("~/projects/def-sapna/afila/GreatUrbanShift/data//cityData/masterSpeciesList.csv")

### Loop through each species into a separate file
for i in sppList["species"]:
    main_csv = pd.read_csv("~/projects/def-sapna/afila/GreatUrbanShift/data/gbifSpp/GBIFmaster/GBIFmaster/mainGBIF.csv", sep="\t",
        usecols= keepCols , dtype=  dtypeDic , ## specify data types to improve memory
        header=0)
    dfSppTemp =  main_csv[main_csv["species"] == str(i)]
    
    #csv to write data to a new file with indexed name. input_1.csv etc.
    out_csv = '~/projects/def-sapna/afila/GreatUrbanShift/data/gbifSpp/' + str(i) + '.csv'
    dfSppTemp.to_csv(out_csv,
          index=False,
          header=0,
          mode='w')