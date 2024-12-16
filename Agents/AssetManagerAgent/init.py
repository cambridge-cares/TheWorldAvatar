# Initial script for instantiating assets from the .xlsx file
# Assumed that all the assets are not instantiated
import pandas as pd
import numpy as np
import requests
import json
import logging
import string

logging.basicConfig(filename='./init.log', encoding='utf-8', level=logging.DEBUG)

filepath = "C:\\Users\\MLAK01\\Desktop\\AssetOntology\\CARES Operational Asset(Mar 2024) for Yong Ren - merged.xlsx"
sheet = "data"
df = pd.read_excel(filepath, sheet)
df = df.replace({np.nan:None})
logging.info(f"Imported data from: {filepath} ; sheet: {sheet}...")

def handleNumericData (row, name) :
    numString = str(row[name])
    if numString == "None" or numString == 'N.A.':
        numString = None 
    return numString

for index, row in df.iterrows():
    print("HANDLING ID: " + str(row["ID"]))
    logging.info("HANDLING ID: " + str(row["ID"]))
    rawData = {}
    #client = http.client.HTTPConnection('http://127.0.0.1:1015') #Change to agent endpoint later
    if(row["Ontology"] != None and row["OntoAssetClass"]!= 0):
        if ("IGNORE" not in row["Ontology"]):
            #asset data
            rawData["Prefix"] = row["Ontology"]
            rawData["AssetClass"] = row["OntoAssetClass"]
            rawData["ID"] = row["Inventory ID"].strftime("%Y-%m-%d") + "/" + str(row["ID"])
            rawData["Name"] = string.capwords(row["Name/Description"].strip())
            #rawData["Name"] = unicodedata.normalize("NFKD", row["Name/Description"]).strip()
            rawData["serialNum"] = handleNumericData(row, "Serial No.")
            rawData["modelNumber"] = handleNumericData(row, "Model No.")

            #ORGS
            rawData["SupplierName"] = row["Producer/\nVendor"]
            if (rawData["SupplierName"] != None):
                rawData["SupplierName"]=string.capwords(row["Producer/\nVendor"])
            rawData["ManufacturerName"] = row ["Manufacturer"]
            if (rawData["ManufacturerName"] != None):
                rawData["ManufacturerName"]=string.capwords(row["Manufacturer"])
            #manufURL = row["Manufacture URLs"]

            #Manual and Spec sheets
            rawData["SpecSheet"] = row["Spec Sheet"]
            rawData["SpecSheetPage"] = row["Manual / Spec Sheet Number"]
            rawData["Manual"] = row["Manuals"]
            rawData["ManualURL"] = row["Manufacture URLs"]

            #LOCATIONS
            rawData["BuildingLocation"] = row["Building Location"]
            rawData["FacilityLocation"] = row["Facility Location"]
            rawData["RoomLocation"] = row["Room Location"]
            rawData["WorkspaceName"] = row["Seat Location"]

            #STORAGE
            rawData["storage"] = row["Storage Location"]

            #PERSON
            rawData["AssignedTo"] = row["Assigned to"]

            #PURCH DOCS
            rawData["Comments"] = row["Comments"]
            rawData["invoiceNum"] = handleNumericData(row, "Invoice")
            rawData["DeliveryOrderNum"] = handleNumericData(row, "DO")
            rawData["PurchaseOrderNum"] = handleNumericData(row, "Comments")
            rawData["BudgetCat"] = row["Service Code"]
            rawData["ServiceCode"] = row["Service Code Description"]
            if (rawData["ServiceCode"] != None):
                rawData["ServiceCode"]=rawData["ServiceCode"].replace("\'", "")

            #PRICE
            rawData["price"] = handleNumericData(row, "Purchase  Price")
            rawData = {"assetData": rawData}
            rawDataString = str(rawData).replace('\\xa0', ' ').replace("\\n", ";")
            
            jsonData=json.loads(rawDataString.replace("\"", "\\\"").replace("None", "\"\"").replace("\'", "\""))
            
            logging.info("-----")
            logging.info(jsonData)
            logging.info("")

            req = requests.post("http://localhost:1015/asset-manager-agent/instantiate", data = json.dumps(jsonData))
            logging.info(req.text)
            logging.info("")

