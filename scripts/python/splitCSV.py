## load libraries
import pandas as pd
import glob
import os 
import gc

## Load all columns
headerCols = ["gbifID","datasetKey","occurrenceID","kingdom","phylum","class","order","family","genus","species","infraspecificEpithet","taxonRank","scientificName","verbatimScientificName","verbatimScientificNameAuthorship","countryCode","locality","stateProvince","occurrenceStatus","individualCount","publishingOrgKey","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision","elevation","elevationAccuracy","depth","depthAccuracy","eventDate","day","month","year","taxonKey","speciesKey","basisOfRecord","institutionCode","collectionCode","catalogNumber","recordNumber","identifiedBy","dateIdentified","license","rightsHolder","recordedBy","typeStatus","establishmentMeans","lastInterpreted","mediaType","issue"]

### Columns to keep
keepCols = ["gbifID","datasetKey","phylum","class","order","family","genus","species","decimalLatitude","decimalLongitude","day","month","year"]
# dtypeDic = {"gbifID": int, "datasetKey": "string","phylum": "string","class": "string" ,"order": "string" ,"family": "string", 
#            "genus": "string","species": "string","day": "string","month": "string","year": "string","string": "string","string": "string"}

### Load in species lists without header from split files
csvFiles = glob.glob("/scratch/afila/GBIFdatabase/splitFiles/[!aa]*")

### Loop through each zip file to save as a separate CSV
for i in csvFiles:
    iter_csv = pd.read_csv(i, sep="\t",
        usecols= keepCols , 
        dtype=  str , ## specify data types to improve memory
        names=headerCols,
        header=0)
    
    ## identify unique species in the list
    uniqueSpp = list(set(iter_csv["species"]))
    
    ## Loop through all the unique species
    for j in uniqueSpp:
        tempSpp = iter_csv[iter_csv["species"] == j]
        
        #csv to write data to a new file with indexed name. input_1.csv etc.
        out_csv = '/project/6035904/afila/GreatUrbanShift/data/speciesOcc/' + str(j) + '.csv'
        
        ## Check to see if file exists, if not write one
        if not os.path.isfile(out_csv):
            tempSpp.to_csv(out_csv, index=False,header=keepCols, mode='w')
        else: # else it exists so append without writing the header
            tempSpp.to_csv(out_csv, index=False, header=False, mode='a')
        print(i)
            
        
## clear memory
del iter_csv
del tempSpp
gc.collect()

### Load in species lists with header from split files
csvFiles = glob.glob("/scratch/afila/GBIFdatabase/splitFiles/*aa.csv")

### Loop through each zip file to save as a separate CSV
for i in csvFiles:
    iter_csv = pd.read_csv(i, sep="\t",
        usecols= keepCols , 
        dtype=  str , ## specify data types to improve memory
        names=headerCols,
        header=0)
    
    ## identify unique species in the list
    uniqueSpp = list(set(iter_csv["species"]))
    
    ## Loop through all the unique species
    for j in uniqueSpp:
        tempSpp = iter_csv[iter_csv["species"] == j]
        
        #csv to write data to a new file with indexed name. input_1.csv etc.
        out_csv = '/project/6035904/afila/GreatUrbanShift/data/speciesOcc/' + str(j) + '.csv'
        
        ## Check to see if file exists, if not write one
        if not os.path.isfile(out_csv):
            tempSpp.to_csv(out_csv, index=False,header=keepCols, mode='w')
        else: # else it exists so append without writing the header
            tempSpp.to_csv(out_csv, index=False, header=False, mode='a')
        print(i)

## clear memory
del iter_csv
del tempSpp
gc.collect()     
          
## Load in files that weren't split

### Load in species lists without header
csvFiles = glob.glob("/scratch/afila/GBIFdatabase/*.csv")

### Loop through each zip file to save as a separate CSV
for i in csvFiles:
    iter_csv = pd.read_csv(i, sep="\t",
        usecols= keepCols , 
        dtype=  str , ## specify data types to improve memory
        names=headerCols,
        header=0)
    
    ## identify unique species in the list
    uniqueSpp = list(set(iter_csv["species"]))
    
    ## Loop through all the unique species
    for j in uniqueSpp:
        tempSpp = iter_csv[iter_csv["species"] == j]
        
        #csv to write data to a new file with indexed name. input_1.csv etc.
        out_csv = '/project/6035904/afila/GreatUrbanShift/data/speciesOcc/' + str(j) + '.csv'
        
        ## Check to see if file exists, if not write one
        if not os.path.isfile(out_csv):
            tempSpp.to_csv(out_csv, index=False,header=keepCols, mode='w')
        else: # else it exists so append without writing the header
            tempSpp.to_csv(out_csv, index=False, header=False, mode='a')
        print(i)


