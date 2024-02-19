# README for BMRS-API-Input-JA-#.py ("#" = version)
## Description
This document is a README for:
> BMRS-API-Input-JA-#-ReadAll.py

Three csv files are required. These are not version controlled, but can be found at: 
> \Dropbox (Cambridge CARES)\CoMo shared\ja685\BMRS\Script-BMRS-API
Their names are: 
> Input-Template.csv
> generator.tripple.csv
> powerplanttripple.csv

These should be copied to:
> \ukpowergeneration\ScriptMapQuery
> This way, this API script is on the same level as these. 

This script matches per generation (station) information from the BMRS database, to the list of stations from the DUKES database. This is done via information from the EIC database. All three database contents are required as inputs in an csv spreadsheet. This spreadsheet will be edited by the script, adding (or updating) the contents of some of the columns in order to note the chosen mapped BMRS station for each DUKES station (where possible). An example of thie contents of such a spreadsheet (after running the script) is copied to a version controlled text file named: 
> csv_notepad_example.txt

In addition to making this edit, the script also outputs a run log to: 
> csvMatchOutputLog.log

Please first read README-csvMatch-JA-#.md, as this details the ingoing data (csv example). This will not be repeated here. Instead, only the following columns will be discussed in the csv. 

These columns are: 
Year, Month, Day, Period, Output

## Inputs
The script runs the Primary Function. The Primary Function's inputs are: 
- csv Spreadsheet Name (contents similar to the notepad example noted above). 
- Key (API Key for BMRS). 
- Year (The year to search or begin searching). 
- Month (The year to search or begin searching). 
- Day (The year to search or begin searching). 
- Period (The year to search or begin searching). 
- Search (0 or 1 or 2. Whether to just look into the given date (0 - don't search ahead), or to try sequential future dates (1 - search ahead). If set to 1 this will incriment in periods and see if more recent data exists. If so, it will use that date. This search begins from the above provided date, and will terminate if the current date is reached, or obtain a full day (2 - Fill in a 24 hour window), which will IGNORE THE PERIOD INPUT (as it goes from 1 to 48 for a given day, given that 1 period corresponds to one half hour). 

The Key is the BMRS API key to access the website. 
The Time is the time of the query is broken down into the Year, Month, Day, and Period (eg. 2021, 01, 01, 01). The year is four characters. The Month, and Day are two characters (starting with a 0 if the value is <10). Finally the period is the half hour into the given day (1-48). Starting with a 0 if it is <10 is optional. 

## Outputs
The csv file is edited and a log file is also created. So the outputs are: 
- Edits to the inputted csv file (same name). If there is no data found for any of the tested times (whether that be 1 time in Search == 0, a number of future times in Search == 1, or 48 times in Search == 2), then this won't be edited. If data was found then for Search == 0 or Search == 1, then the 'Output' column will be edited. If not (for Search == 2), then 'Output1', 'Output2', ..., 'Output48', are edited. 

The "primary function" (see python script) should also be noted to return a 1 or 0. 1 is returned if the query recieved data, 0 if it did not (eg. an invalid time was used). 

## Process
The BMRS data gives output per generation unit. Each station has multiple generation units. DUKES stations should be mapped to BMRS station data. Using the IDs from this, the information (via the naming convention, eg. Station POWER with generation units POWER-1, and POWER-2). These outputs are summed for each station. 
