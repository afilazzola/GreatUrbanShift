### Add headers to Python files

## load libraries
import pandas as pd
import glob
import os 


## load in header
segHeader = pd.read_csv("/scratch/afila/GBIFdatabase/0294639aa.csv", sep="\t", header=0, chunksize=1000)
outHeader = segHeader.get_chunk(10)


## write header 
outHeader.to_csv("/scratch/afila/GBIFdatabase/header.csv", index=False,  mode='w')


## load in header
segHeader = pd.read_csv("/scratch/afila/GBIFdatabase/segmentaa.csv", sep="\t", header=0, chunksize=1000)
outHeader = segHeader.get_chunk(10)


### Load in species lists
csvFiles = glob.glob("/scratch/afila/GBIFdatabase/[!aa]*")
