# README for Mapping-Match-JA-#.py ("#" = version)
## Description
This document is a README for:
 > Mapping-Match-JA-#.py

Three csv files are required. These are not version controlled, but can be found at: 
> dropbox\CoMo shared\ja685\Modelling-Resources\BMRS\Script-BMRS-API
Their names are: 
> Input-Template.csv
> generatortripple.csv
> powerplanttripple.csv

These should be copied to:
> \ukpowergeneration\ScriptMapQuery
> This way, this Mapping script is on the same level as these. 

This script matches per generation (station) information from the BMRS database, to the list of stations from the DUKES database. This is done via information from the EIC database. All three database contents are required as inputs in an csv spreadsheet. This spreadsheet will be edited by the script, adding (or updating) the contents of some of the columns in order to note the chosen mapped BMRS station for each DUKES station (where possible). An example of thie contents of such a spreadsheet (after running the script) is copied to a version controlled text file named: 
> csvNotePadExample.txt

In addition to making this edit, the script also outputs a run log to: 
> csvMatchOutputLog.log

In this csv, from the BMRS database are the following columns of information: 
BMRS, Business Type, Registered Resource EIC code, BM Unit ID, NGC BM Unit ID, Registered Resource Name, PSR Type, Effective From (Date), Capacity MW. 

From the EIC database are the columns (for stations): 
EICStation, ESO W Type Codes - Stations, Display Name - Stations, Used by X BMUs - Stations, Automatic ID - Stations, 15 characters, Energy Identification Code - Stations, Function - Stations, EIC Name - Asset Name - Stations. 

Also from the EIC database are the columns (for generation units in stations):
EICUnit, ESO W Type Codes - BMUs, Display Name - Units, STATION, Automatic ID - Units, Energy Identification Code - Units, Function - Units, EIC Name - Asset Name - Units. 

From the DUKES database are the columns: 
DUKES, Company Name, Station Name, Fuel, Type, Installed Capacity (MW), Year of commission or year generation began, "Location: Scotland, Wales, Northern Ireland or English region". 

It should be noted for the above, that the first column is simple for the convenience of identifying the sections in csv, and is just the name of the source (BMRS data, EIC station - data, EIC unit - data, DUKES - data). Checking the notepad csv copy noted above you will see that this columns simply contains the title in every value for readability. You don't actually need these first columns. 

Similarly, columns with asterixis in them are also just for formatting, eg. "*", "**", etc... Once again this can be seen in the notepad csv copy. These are just to break up the sections. 

Finally, the following columns are used for the output (to start from scratch you need the column titles (first row) to be there, but can leave the rest blank to be filled in. If values already exist, running the script will replace them. 

Firstly, the following columns are used for each BMRS generator's suggested DUKES station (if applicable). Multiple can note the same DUKES station. This is just for your interest as the important match will come later: 
outputBMRSUnitToDUKESStation, tMatchType. 

For this important match (DUKES to BMRS), the following columns are used: 
outputDUKESToBMRSEIC, outputDUKESToBMRSID. 
...And...
CapacityDiff, MatchType, MatchYear, MatchGenType, ConfidenceScore, ConfidenceResult

Many of these are important if you are interested in the internal workings, however, if you just want the output, then use "outputDUKESToBMRS" (if it exists), ONLY IF "ConfidenceResult" = 1. This will give a suggested EIC code for each mapped DUKES station that is deemed 'close enough' (as per your sensitivity input). These are exclusive. So use this for your final output. 

The two remaining columns are not used by this script, but are used elsewhere to find the output at a given time, for a DUKES station (obtained from BMRS through this mapping): 
Year, Month, Day, Period, Output

## Inputs
The script takes two inputs (both the primary function, and running it through __main__ with args). These are the name of the csv spreadsheet, and a sensitivity value. So: 
- csv Spreadsheet Name (contents similar to the notepad example noted above). 
- Sensitivity Value. 

When running it through the primary function (BMRSEICDUKESMap in the python script), you will need to provide both values as per: def BMRSEICDUKESMap(csvName, x):, where "csvName" is the name of the csv spreadsheet it will read and edit, and "x" is the sensitivity value. You will need to provide both. At the time of writing an csv spreadsheet name of "Input-Template.csv" is used, as well as a sensitivity value of "10". As can be seen above, the csv name is the first function input, followed by the sensitivity value. When running as __main__ (which in turn runs the primary function), these can be given in either order, with the one that is a valid integer being used for the sensitivity. If a value is not given for one or both of the inputs, then the default values of "Input-Template.csv" and "10" are used. 

The sensitivity value is recommended to be from 5-50 (integer). This gives a level of tolerance for each match. 5 is the most sensitive, and 50 is the least sensitive. If in doubt, a value of 10 is recommended. 

## Outputs
The csv file is edited and a log file is also created. So the outputs are: 
- Edits to the inputted csv file (same name). 
- A log file (name noted earlier). 

The log file will note potential data issues (non-fatal, and are not unusual to have), such as repeat values in the BMRS or EIC data. These are for you to be aware of, but do not necessarily mean the code cannot properly run, it is simply some notes on the inputted data. 

After listing these the log file will also describe the decision making process for each DUKES station. Rather than just outputting numbers for the criteria considered (which are in brackets), it describes in plain English what is calculated. This should hopefully assist in your understanding. 

## Process
So how is are matches calculated. 

Firstly, from the BMRS data there is an EIC value, which is the same as in the EIC data. Then, in the EIC data there is a name. This is often different from the name in DUKES (DUKES does not have EIC data), but can often sill be matched. 

The following steps are made in attempting to match the names: 
- Exact match of names. 
- Roman numeral vs decimal number difference only match of names (also considered in all below where applicable). 
- Simplified match (eg. strip: '*', ' ', '-', ''', '‘', '’', '_', '&', etc...) of names (also considered in all below where applicable). 
- Name extension change match (sometimes naming conventions might differ slightly, such as including a "GT" after a name, or a single letter - eg. Station Name GT, or Station Name A). A match is attempted accounting for this being used in only one of the two names. 
- First word (if exclusive) word in EIC matches DUKES name. 
- First words (if exclusive) of EIC and DUKES names match. 
- Any (exclusive) word matches. 

"Exclusive" words are those which are not generic. Examples of generic words include "North", "Station", "Bridge", etc... They simply appear in multiple names. Some of these are hardcoded, and others can be found by a check the code runs. Depending on your sensitivity level, if a word appears too many times, then it will be excluded from the valid steps. A word which appears multiple times, however, may still be useful if that's all that exists. Thus, this should not be taken too far and the final name match step can be used. If multiple BMRS names match a DUKES name, capacity is used as a tie breaker (closest wins). The outcome of the name match gives one input value to the final confidence score. 

Next, the capacity difference is noted, this being a major input into the confidence score. The most important step is still the name match, as you can't score an attempted match if it does not exist, but this is the most important part of the score. 

Following this, the installation / first operation year is considered. This is not an important metric as these values can vary widely, even if the stations clearly match. Thus, this has a low impact on the final score. 

Finally, there generation type (eg. nuclear, offshore wind) is considered. This is not always provided, but if it is, then two names of the same type eg. "Offshore Wind" and "Offshore Wind Station", then this increases the chance of a positive outcome. Also, if they types are verified to be different then this bodes poorly for the final result (though the capacity difference is still more important, with this being of similar importance to the name match type). 

These scores are then all summed and if the result is less than or equal to the sensitivity value (x), then the confidence result is a match (1, as opposed to 0 for not a match). 

For valid tests, if a match is clearly false, (or cannot be attempted), it gives a 999 output, which (unless you lowered the sensitivity significantly by setting a very high input value for x) will fail the resulting match result. Again, note that a recommended value for the sensitivity threshold is recommended to be between 5-50 (10 default if in doubt), with 5 being the most sensitive, and 50 being the least sensitive. Test outcomes and minor points of functionality may differ based on the sensitivity value (such as the number of tolerated repeat words, type match results, type mismatch results, etc...). 

Some manual checking of results near this threshold is recommended. Also, in one case, DUKES appeared to split a station, resulting in a poor outcome, even though the aggregate was clearly the same, so automation cannot solve discrepancies using this script.
