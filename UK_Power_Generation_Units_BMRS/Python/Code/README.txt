Here are some folders corresponding to BMRS data querying, processing, visualisation, etc... 

ukpowergeneration: The "ukpowergeneration" folder contains the original per generation unit 
querying, processing (incl. KG), and visualisation. 
For information of the main files within, and visualisation, see the README(s) within. 
Within this "ukpowergeneration" folder, in the "ScriptMapQuery" folder, is the API query script, 
and the original DUKES <-> EIC <-> BMRS mapping script (which is partially run manually). 

ukopowersupply: The "ukopowersupply" folder contains query scripts. These all generally operate
via CSV / Excel files (which in turn benefit from the mapping script(noted above)'s output). 
Additional details about the "ukopowersupply" folder are noted in the README(s) within. 

essattachment: In "essattatchment" are scripts related to the attachment of an ESS to a VRE site 
(e.g. wind). Using linear optimisation to maximise profit it charges and discharges from an ESS. 
This is also subject to further parameters for energy curtailment / export, which can work 
alongside the associated data inputs from
"Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\data\ukpowerother", which
query BMRS to obtain these. 

ukexportcurtailmentimporter: In "ukexportcurtailmentimporter" there is a script to input 
curtailment and export data into the KG. This corresponds to the querying in 
"Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\data\ukpowerother", 
and data that can be used to run "essattatchment".
