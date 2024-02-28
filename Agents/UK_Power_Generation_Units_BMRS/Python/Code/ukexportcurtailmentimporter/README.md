# UK Export Curtailment Importer #

## Generic Description ##

This module retrieves energy exports and curtailments for UK generation units.

## Data Source ##

This script imports data from the BMRS (<https://www.bmreports.com/bmrs/?q=eds/main>) which includes
information on wind farm exports and energy bids (where wind farms bid their full generation amounts).
The former allows for exports to be known, and the difference allows for curtailments to be known.

## Code Execution Details ##

With 'export_curtailment_data_importer.py' as the main script,
these files import data from BMRS/Excels of BMRS data to input into the KG.

EXCEL SETUP:
This process requires excel files to be copied from:

"\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\data\ukexportcurtailmentimporter\CopyExcels" into the "data" folder.

Create a folder called ScripMapQuery at the same level as the export_curtailment_data_importer.py module and
copy the BMRS_API_Input_JA_7.py module from the path of "Dropbox (Cambridge University)\CoMo shared\ja685\BMRS\Script-BMRS-API" into it.
