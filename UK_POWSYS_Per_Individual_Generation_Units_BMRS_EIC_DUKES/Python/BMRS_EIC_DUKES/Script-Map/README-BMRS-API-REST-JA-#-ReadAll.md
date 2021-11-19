# README for ExcelMatch-#.py ("#" = version)
## Description
This document is a README for:
 > BMRS-API-REST-JA-#-ReadAll.py

This script matches per generation (station) information from the BMRS database, to the list of stations from the DUKES database. This is done via information from the EIC database. All three database contents are required as inputs in an excel spreadsheet. This spreadsheet will be edited by the script, adding (or updating) the contents of some of the columns in order to note the chosen mapped BMRS station for each DUKES station (where possible). An example of thie contents of such a spreadsheet (after running the script) is copied to a version controlled text file named: 
> ExcelNotePadExample.txt

In addition to making this edit, the script also outputs a run log to: 
> ExcelMatchOutputLog.log

Please first read README-ExcelMatch-JA-#.md, as this details the ingoing data (excel example). This will not be repeated here. Instead, only the following columns will be discussed in the excel. 

These columns are: 
Year, Month, Day, Period, Output

## Inputs
The script takes two inputs (both the primary function, and running it through __main__ with args). These are the name of the excel spreadsheet, and a sensitivity value. So: 
- Excel Spreadsheet Name (contents similar to the notepad example noted above). 
- Key. 
- Year. 
- Month. 
- Day. 
- Period. 

The Key is the BMRS API key to access the website. 
The Time is the time of the query is broken down into the Year, Month, Day, and Period (eg. 2021, 01, 01, 01). The year is four characters. The Month, and Day are two characters (starting with a 0 if the value is <10). Finally the period is the half hour into the given day (1-48). Starting with a 0 if it is <10 is optional. 

## Outputs
The excel file is edited and a log file is also created. So the outputs are: 
- Edits to the inputted excel file (same name). 
The "primary function" (see python script) should also be noted to return a 1 or 0. 1 is returned if the query recieved data, 0 if it did not (eg. an invalid time was used). 

## Process
The BMRS data gives output per generation unit. Each station has multiple generation units. DUKES stations should be mapped to BMRS station data. Using the IDs from this, the information (via the naming convention, eg. Station POWER with generation units POWER-1, and POWER-2). These outputs are summed for each station. 
