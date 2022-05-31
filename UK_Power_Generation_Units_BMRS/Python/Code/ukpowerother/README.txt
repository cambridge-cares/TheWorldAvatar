Above are four different query scripts with more specific functionality. These query the BMRS via the API and apply some processing to output. They require excel/csv files for input and output. 

These files may be found in "\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\data\ukpowergeneration" in the "OTHER_COPY_FILES" folder. There the names of the folders will match those found in this directory - containing the excel / csv files within. 

Description of functionalities: 
Below the functionalities of these scripts will be noted. 
Script-BMRS-Marginal-Seller: This script queries BMRS detailed system (bidding) market data. For each period (1/2 hour window) in a given range of days it finds the producer offering a price most similar to the final system price - i.e. the marginal seller. You could also think of this seller as the first to not be accepted if the price were to fall, or the one closest to bidding a price at market equilibrium. 

Script-BMRS-API-Per-Wind-Unit-Curtailment: This script, for each period in a given range of days, attempts to determine the curtailment for defined wind farms. 

Script-BMRS-API-Per-Wind-Unit-Export: This script, for each period in a given range of days, attempts to determine the per unit generation (presumed export) for defined wind farms. This script is therefore very similar to the API script used in "ukpowergeneration", but is specifically for wind farms, and can provide an output over multiple time periods in the one output file. 

Script-BMRS-API-Price-And-Aggregate-By-Type: This script, for each period in a given range of days, attempts to extract the price of energy, and aggregate output by generation type. 
