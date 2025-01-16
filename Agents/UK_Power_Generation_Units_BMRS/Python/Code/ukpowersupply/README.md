# UK Power Supply Analysis Tool #

## Generic Description ##
This tool contains a number of API scripts for retrieving information regarding the UK’s power system. 
These are used to determine the marginal seller (using bidding data), wind farm curtailment data (using 
the difference between the bidding and exports of wind farms), wind farm export data, and aggregate 
generation and price data.  

## Data Source ##
This script imports data from the BMRS (https://www.bmreports.com/bmrs/?q=eds/main) which includes 
information on the UK’s energy network including generation and market data. This broad source of 
information is queried by the scripts in this module.

## Query Scripts ##
Above are four different query scripts with more specific functionality. These query the BMRS via the API and apply some processing 
to output. They require excel/csv files for input and output. These files all take an input range of dates to begin and end, as well
as an initial index (0 or 7, as documented within the script), and use this to fill the excel used for output. 

These (csv/excel) files may be found in the
"\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\data\ukpowersupply" folder.
There the names of the folders will match those found in this directory - containing the excel / csv files within. 

While performing this task they will save the results monthly as well as when the final date is reached (printing the next index).
If there is some error (eg. in querying) that halts their operation, then this query (beginning with the first day of the uploaded 
month, and the index) may be used as a new start point. 
Note that the index is equivalent to the (excel column - 2), this is because excel counts from 1, while python's dataframe counts
from 0 (which explains -1), and that the first column's value is used as the column's name in python, rather than an entry 
(which explains the other -1). 

Description of functionalities: 
Below the functionalities of these scripts will be noted. 

Script-BMRS-Marginal-Seller: This script queries BMRS detailed system (bidding) market data. For each period (1/2 hour window) in 
a given range of days it finds the producer offering a price most similar to the final system price - i.e. the marginal seller.
You could also think of this seller as the first to not be accepted if the price were to fall, or the one closest to bidding a
price at market equilibrium. 

Script-BMRS-API-Per-Wind-Unit-Curtailment: This script, for each period in a given range of days, attempts to determine the
curtailment for defined wind farms. 

Script-BMRS-API-Per-Wind-Unit-Export: This script, for each period in a given range of days, attempts to determine the per 
unit generation (presumed export) for defined wind farms. This script is therefore very similar to the API script used in
"ukpowergeneration", but is specifically for wind farms, and can provide an output over multiple time periods in the one output file. 

Script-BMRS-API-Price-And-Aggregate-By-Type: This script, for each period in a given range of days, attempts to extract the price
of energy, and aggregate output by generation type. 
