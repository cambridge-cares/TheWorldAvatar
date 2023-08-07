# Digest of UK Energy Statistics (DUKES)

Once a year, the UK government publishes a Digest of UK Energy Statistics (DUKES);  note this was formally published by the Department for Business, Energy and Industrial Strategy (BEIS) before it was dissolved, subsequent publications should be from the new Department for Energy Security and Net Zero (DESNZ).

Unfortunately we've discovered a number of issues with the 2023 data set (the latest available at the time of writing); these include:

* No confirmation of the projection used (we assume `EPSG:27700`)
* Locations do not match satellite images from Google Maps
  * In most cases, the locations given in the data are offset by about 0.5km to 1.5km
  * In other cases locations are listed as in one country, but the given coordinates are in another
* Provided OS grid references do not match the Easting/Northing given for the same plant 
* Provided postcodes do not match the Easting/Northing given for the same plant

## Acquiring the data

The UK Government publishes the DUKES data set once a year on their [data portal website](https://www.gov.uk/government/collections/digest-of-uk-energy-statistics-dukes). At the time of writing, the UK Base World visualisation is using document 5.11 of the 2023 DUKES data set, entitled "Power stations in the United Kingdom, May 2023 (DUKES 5.11)" from the "Chapter 5: Electricity" section.

A copy of the raw XLSX file, and the cleaned-up CSV file, have been archived at CMCL on their Pavilion server (get in touch with the CMCL team for more details).

### OS Code Points

For the clean-up script (see more below), a number of CSVs detailing the rough locations of UK Postcodes is used to cross-check the details provided by DUKES. These are provided by the UK Ordnance Survey and are available from their [Code-Point Open](https://osdatahub.os.uk/downloads/open/CodePointOpen) page.

Users wishing to run the script will need to download the ZIP file and move all the CSVs in the archive's `./data` directory to the `scripts/postcodes` directory.

## Cleaning up the data

In an attempt to provide the most up-to-date set of data (something we could arguably refer to as an "adjusted DUKES 2023" dataset), Michael at CMCL has produced a clean-up script to read the DUKES XLSX file and produced a CSV file in which the best guess for actual plant locations is determined using a simple algorithm,

Before this algorithm was written, we have confirmed with the DESNZ that the primary information (i.e. what was provided by the actual plant owners) is the OS Grid Reference or Postcode; the Easting/Northing is later added by the department. As such, the Easting/Northing is considered less reliable that the other location details (where present).

The `scripts/dukes_generate_csv.py` script performs the below steps. Note that `BNG` refers to the [British National Grid](https://epsg.io/27700) projection (EPSG:27700), `GRID` refers to the OS Grid Reference, and `PC` the UK postcode.

1. Read and cache the provided postcode CSVs.
2. Read the DUKES spreadsheet.
3. For each plant (row) in the sheet:
   1. Update the plant name (replacing commas, leading and trailing spaces)
   2. Check for the presence of any geographical info (BNG, GRID, PC)
      1. If **all** are missing, consider entry invalid and skip it
      2. If BNG is present, convert it to latitude/longitude (EPSG:4326)
      3. If GRID is present, convert it to latitude/longitude (EPSG:4326)
      4. If PC is present, convert it to latitude/longitude (EPSG:4326)
      5. Using a hierarchical check, determine the most trusted location:
         1. If BNG is the only datum present, use this
         2. If BNG **and** GRID are provided, compute their distance then:
            1. If distance cannot be determined, use the GRID datum for the final location
            2. If less than 15KM apart, use the GRID datum for the final location
            3. If more than 15KM apart, attempt to geolocate the region/country for each location, then:
               1. If the region/country for the GRID datum matches the value in the DUKES data, use the GRID datum for the final location
               2. If the region/country for the BNG datum matches the value in the DUKES data, use the GRID datum for the final location
               3. If geolocation fails or there is no match, use the GRID datum for the final location
         3. If BNG **and** PC are provided, compute their distance then:
            1. If distance cannot be determined, use the PC datum for the final location
            2. If less than 15KM apart, use the PC datum for the final location
            3. If more than 15KM apart, attempt to geolocate the region/country for each location, then:
               1. If the region/country for the PC datum matches the value in the DUKES data, use the PC datum for the final location
               2. If the region/country for the BNG datum matches the value in the DUKES data, use the PC datum for the final location
               3. If geolocation fails or there is no match, use the PC datum for the final location
   3. Output the final CSV

This leaves us with a list of `1166` sites, down from the `1333` sites listed in the original 2023 data set. Without manually reviewing all the sites against satellite images from Google, this is likely the best quality we'll get for this data; unfortunately DESNZ has confirmed that they perform no quality checking on any DUKES data before publishing.

The command line arguments for the `scripts/dukes_generate_csv.py` should be self-explanatory, a sample command for running the script from the root directory can be seen below.

```
python ./scripts/dukes_generate_csv.py --input dukes-2023.xlsx --output dukes-2023.csv --postcodes ./scripts/postcodes
```