This folder contains data for use by stack-data-uploader, and should be copied into  `stack-data-uploader/inputs/data`.
The data in the sub-folders are obtained from data.gov, then parse with `parse_geojson.py`.

- `./heritage` contains the GeoJSON for heritage trees
- `./historicsites` contains the GeoJSON for historic sites
- `./monuments` contains the GeoJSON for monuments
- `./museums` contains the GeoJSON for museums
- `./touristattractions` contains the GeoJSON for tourist attractions
- `parse_geojson.py` is the Python script for parsing the raw GeoJSON from data.gov, which have all the information as HTML string inside the 'Description' column. The script will parse the information from the the 'Description' column into distinct columns. To use the script simply call  `python parse_geojson.py file_path`, where `file_path` is the path to the raw GeoJSON from data.gov.
- `culture.obda` is the OBDA mappings for the culture sites contained in this folder

