# ESS Attachment Analysis Model #

## Generic Description ##

This is a battery dispatch optimisation model.

## Data Source ##

This model takes battery specifications and a time series of price data as inputs.
These are specified within the script.

## Implementation and Execution Details ##

'ESSDispatchInput.txt' and 'ESSDispatchOutput.txt' serve as the input and output
text files for the 'ESS-pulp-JA-Split-26.py' script.
This script optimises the behaviour of an ESS attachment to a site, which
can have local generation, consumption, and curtailment.

This is based on previous work at these locations:
\Dropbox (Cambridge CARES)\CoMo shared\ja685\BMRS\Wind-and-ESS\ESSPythonScript
\Dropbox (Cambridge CARES)\CoMo shared\ja685\BMRS\Wind-and-ESS

\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\JATHERTON\Final Version - JA-ESS (Battery Scheduling Model - 11.2.2)\Python 48 Version
\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\JATHERTON\Final Version - JA-ESS (Battery Scheduling Model - 11.2.2)

The inputs provide required information, as well as configuration for the script
(this also configures what is set as the output, so to understand output, just
understand input configuration).

Example values are currently in the script (unused, just for reference), and in
the text files (the input ones will be used if the script is run, so note these
will be overwritten by running the script).

The 'Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\data\essattachment\CopyExcel\TrialInputData.xlsx'
file notes example inputs, their names, and descriptions. You may use these for testing purposes.
I.E. By copying them into the input text file, running the script, and seeing
the printed output and contents of the output text file.
