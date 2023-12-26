# The file contains configuraitons of file paths.
# ===============================================================================
#Files of Model & value mappings ==========================================================
XLSMMODELPATH = './model/Excel_MacKay_Carbon_Calculator_v209.xlsm' # Path to the model
OUTPUTMAPPINGPATH = './model/output.csv'  # Path to the charts value mapping csv
CONTROLMAPPINGPATH = './model/controls.csv' # Path to the lever option value mapping csv
SINGLEVALUEMAPPINGPATH = './model/single_values.csv' # Path to single values outputs mapping csv

#locations related to frontend webpages ==========================================
INITIALDATAJSONPATH = './frontend/src/assets/json/initial_plotdata.json'#initial charts value json,  auto-created by prepare_initial.py in this location
LEVERDESCRIPTIONPATH = './frontend/src/assets/json/descriptions.json'#json that contains level desription texts (extracted from Excel), auto-created by prepare_initial.py in this location
ONEPAGERPATH = 'frontend/onepagers'#folder of the one-pagers


#CONNECTION TO KG
#location for the json that contains info of the all queries to send to the KG to get up-to-date data
QUERYFILEPATH = './model/queries.json'
KG_UPDATE_LIST = {"hhv":"I5009", "lhv":"I5010","dwellingunit":"J1276","population":"J1257"}
NAME_NETZERO = "reduction2100" # as correspond to the single_values.csv