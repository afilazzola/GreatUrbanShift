

### Download master zip files from GBIF
wget https://api.gbif.org/v1/occurrence/download/request/0294685-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294684-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294683-200613084148143.zip
wget https://api.gbif.org/v1/occurrence/download/request/0294674-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294672-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294671-200613084148143.zip
wget https://api.gbif.org/v1/occurrence/download/request/0294660-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294659-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294658-200613084148143.zip
wget https://api.gbif.org/v1/occurrence/download/request/0294640-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294639-200613084148143.zip https://api.gbif.org/v1/occurrence/download/request/0294638-200613084148143.zip

### unzip the GBIF files
unzip \*.zip

### split the larger files into more managable sizes

split -b 10485760k 0294638-200613084148143.csv segment ## split into 10 GB
split -b 10485760k 0294639-200613084148143.csv 0294639 ## split into 10 GB

## remove unsplit files
rm 0294638-200613084148143.csv
rm 0294639-200613084148143.csv
