
# Asset Manager Agent
An agent designed to manage the asset instances in the knowledge graph. The agent is designed to work together with the [Asset Management App](LINK TO APP REPO HERE).

Currently the agent is capable of :
- Instantiating asset data
- Print asset QR code
- Retrieving asset data
- Deleting asset data (Endpoint not integrated to app yet)


## Structure
The agent is composed of 2 parts, the agent and the printing server. 
The agent handles all interaction between the client and both the knowledge graph and printing server, while the printing server will handle the interaction with the printer. The interaction between the client, agent, printer and knowledge graph is outlined in the following graph:

[PUT GRAPH HERE]

*NOTE: The QR printing system is chosen as it is not possible to install the printer inside the Docker container. The agent may also not be able to access the printer from the container. Even if it could, the printer may not be available for the host machine's OS or it may not be connected to the printer.*

Currently the agent is designed specifically to handle assets of CARES. The agent is designed to handle assets on 2 different locations, CARES office and the Research Wing.
Hence the agent relies on inputting the assets into 2 namespaces: the `lab` and `office` namespace in the knowlege graph. Assets on other locations are put into the `office` namespace as default, where their location will only be recorded as a literal.




## Installation
### Printing Server
The target printer need to be installed first in the host of the server. The intallation of this printer may vary between printer types, but generally a printer driver is needed to connect to the printer. Please check with your respective printer manufacturer for the printer driver. Keep in mind some printer driver may only be available for certain OS. Once the printer is installed, the printer can be identified under specific names. Currently the printer server is designed to run on a Windows environment. If a linux environment is used instead, a CUPS server is recommended instead.

The printing server could be packaged as either a .exe file or run as is a Flask server. The .exe format is provided in case running the server as a Python application is unfavorable. **It is recommended to create a new virutal environement before installing/ running the server.**
A requirement.txt file is included in the repository to use:
```bash
pip install ./PrintingServer/requirements.txt
```
To run the printing server, go to `./PrintingServer/`:
```bash
python PrinterServer.py
```
If successful, a terminal showing the IP address of the printing server will be shown.*Do keep in mind this server produced from this is a development server.*

To convert the server to a .exe, cx_freeze can be used using the following command:
```

```
A `./PrinterServer/build` folder will be generated with a folder inside it, named depending on the host OS and machine: `./PrinterServer/build/exe.[HOST_OS]-[HOST_ARCHITECTURE]-[PYTHON_VER]`. Inside this folder the executable will be available.
*NOTE: To transport the executable, please transport the whole build folder.*


The printing server will look for an environment variable called `PRINTERSERVER_PRINTER`. This value of the variable should be the target printer name. 

### Agent
Update the following files in the `./config` folder:
- `/agent.properties`:
    - `endpoint.kg.[NAMESPACE]`: The SPARQL endpoint of each respective NAMEPSACE.
    - `auth.kg.user` and `auth.kg.pass`: Blazegraph authentication. Can be left blank if no authentication is needed.
    - `endpoint.printer`: Printer server endpoint.
    - `target_qr_size`: QR code size target in cm.
    - `url.manual`: URL for the accessing the asset's manual. The URL produced will have the manual name appended to the end of the URL.

- `/ontologyMap.properties`: A map of the asset type and their IRIs. Refer to he available values in [AssetClass](#instantiation)


Update the following files in the `./credentials` folder:
 - Add your git credentials with your git username and token in respectively `repo_username.txt` and `repo_password.txt`.

Update `./docker-compose.yml`:
 - Volumes: Change the bind mount folder location to your need for the agent to store the manuals. In case the Stack is used, this may need to be changed

#### For use in The Stack
Update  `./stack-manager-input-config-service/asset-manager-agent.json`:
 - Update the bind mount to where the manuals are to be stored


The agent could be built using the following command:
```bash
docker-compose up -d
```



    
## Features
### Agent
All the following parameters will be enclosed in a JSONObject with key `AssetData`.
#### Instantiation
##### - `/instantiate`
For instantiating assets. 
Takes in asset data in the request parameter. **If any data is missing please use blank (`""`) in the request**. The required parameter are:


| Parameter        | Mandatory  Field                        | 
|:----------------:|:---------------------------------------:|  
| Prefix (DEPRECATED) |                                      |           
| AssetClass       | Y                                       |             
| ID               | Y                                       |
| deliveryDate     |                                         |            
| Name             | Y                                       |
| serialNum        |                                         |
| modelNumber      |                                         |
| SupplierName     |                                         |
| ManufacturerName |                                         |
| SpecSheet        |                                         |
| SpecSheetPage    |                                         |
| Manual           |                                         |
| ManualURL        |                                         |
| BuildingLocation |                                         |
| FacilityLocation | Y                                       |
| RoomLocation     | If in CARES office or Research Wing     |
| WorkspaceName    |                                         |
| storage          |                                         |
| AssignedTo       |                                         |
| ItemComment      |                                         |
| InvoiceNum       |                                         |
| DeliveryOrderNum |                                         |
| PurchaseOrderNum |                                         |
| BudgetCat        |                                         |
| ServiceCode      |                                         |
| price            |                                         |
| existingIRIs     |                                         |

The parameters:
- Prefix (DEPRECATED): 
    **NOTE: As of the newest version the `Prefix` key is deleted as the asset type is now instantiated differently. Requests for the previous version of the agent will have the `Prefix` key ignored, but should still be updated.**

    The prefix to be used to describe the type of the asset. The list of accepted prefixes are: 
        - ontodevice
        - ontolab
        - ontosystem
        - ontoinma
        - ontoelectricalpowerequipment
        - ontoems

        The prefix can be left blank on request and the asset instantiated will have the prefix mapped from `.\config\ontologyMap.properties`

    
- AssetClass : The class of the asset type. Available classes are:
    - LabEquipment
    - Laptop
    - Monitor
    - OtherIT
    - Printer
    - Workstation

- ID : Asset ID. The format accepted for the ID is `YYYY-MM-DD/[ID#]`, where the `ID#` is an integer. If an asset is part of another asset the format for `ID#` of `[int].[int].[int]. ...` is also accepted, where the hierarchy of the item is suggested by the point. This is usually used for systems of devices. When left blank, the delivery date  will be used as provided in `deliveryDate` and the largest number of `ID# + 1` will be used. If `deliveryDate` is left blank, then the date of when the command was sent will be used instead.

If field is to be left empty, please use `""`.

- Name : The name of the asset or a brief description of the asset
- deliveryDate: The date of delivery of the assets. To be used when generating ID..

- serialNum : The serial number of the asset
- modelNumber : The model number of the asset
- SupplierName : The company that provide the asset. Where the asset is bought from
- ManufacturerName  : The company that manufacture the asset
- SpecSheet : The specification sheet location. Usually a URL to a .pdf.
- SpecSheetPage : The page on which the specification of the asset is located on the spec sheet.
- Manual : File name for the manual.
- ManualURL : The URL from where the manual can be accessible
- BuildingLocation : Main location of the asset. Which building is it located in. Mainly accepts CARES Research Wing and CREATE Tower. If the input is any building other than these two, the building will be instantiated as literals
- FacilityLocation : The facility on which the asset is located, for example: labs, office areas. As only the lab and office locations in CARES Research Wing and CREATE Tower are instantitated, only when the building location is between the two of those and valid facilities locations were used will the agent instantiate the asset 
- RoomLocation : The room in the facility in which the asset is located.
- WorkspaceName : The workspcace in which the asset is located in.
- storage : Assets or cabinets in which the assets are stored in.
- AssignedTo : The name of the person whoim the asset is assigned to.
- ItemComment : Any additional remark regarding the asset
- InvoiceNum : ID# of the invoice from when the asset is purchased
- DeliveryOrderNum : ID# of the delivery order from when the asset is purchased
- PurchaseOrderNum : ID# of the purchase order from when the asset is purchased
- BudgetCat : The budget source category used for purchasing the asset
- ServiceCode : The service code of the budget category
- price : asset price on purchase
- existingIRIs : Uses provided existing IRIs instead of generating a new one. Can be left empty or not included if all IRIs are to be generated. As it is a hotfix, it is currently only implemented for the follwoing: device IRI, item IRI,  workspace IRI.

The example request and successful result:
- Request:
```
POST /asset-manager-agent/instantiate HTTP/1.1
Content-Type: application/json

{"assetData": {"Prefix": "ontolab", "AssetClass": "Pump", "ID": "2021-04-01/973", "Name": "Precision Metering Instrument (pump)", "serialNum": "", "modelNumber": "WT300-1JA/MG204", "SupplierName": "Baoding Longer Precision Pumpco. Ltd", "ManufacturerName": "Baoding Longer Precision Pumpco. Ltd", "SpecSheet": "https://www.theworldavatar.com:1010/careslab/specsheets/PumpWT3000-1JA_SpecSheet.pdf", "SpecSheetPage": "N.A.", "Manual": "https://www.theworldavatar.com:1010/careslab/manuals/PumpWT3000-1JA_Manual.pdf", "ManualURL": "https://www.longerpump.com/index.php/GearPump/show/94.html", "BuildingLocation": "CREATE Research Wing", "FacilityLocation": "CARES Lab 1", "RoomLocation": "Open Lab Area", "WorkspaceName": "", "storage": "", "AssignedTo": "Alexei Alexandrovich LAPKIN", "ItemComment": "", "invoiceNum": "210325AS592", "DeliveryOrderNum": "210325AS592", "PurchaseOrderNum": "POEC000361", "BudgetCat": "", "ServiceCode": "EC02 MID RANGE RESEARCH EQPT", "price": "3472.49"}}
```
- Result:
```json
{"Result":[{"deviceIRI":"https://www.theworldavatar.com/kg/ontodevice/Pump_4e01c511-9618-47c4-855b-94d0d944ee52","ID":"2021-04-01/973"},"Command Success"]}
```

The endpoint also accepts instantiating a set of assets where the ID generated will have the `[int].[int].[int]. ...` format. The main ID number will have the largest number of `ID# + 1`. The ID generated in order from the first item in the request body :`largestID# + 1 . 0`, `largestID# + 1 . 1`, `largestID# + 1 . 2`, ...

The content of the request are contained in a key `setData`, which value is a JSONArray containing the above format for asset-wise instantiation. An example of this request:
```json

```

##### - `/addmaintenance`
For adding maintenance instances of the assets.
The parameters:
- ID: ID of the asset to have the maintenance schedule
- LastService: When the service is last performed
- NextService: When the next service will be performed
- Interval: If the service will be regular, how often will the service be performed **in months**.
- ServiceProvider: The organization who performs the maintenance

The request at least has to have either/both LastService or NextService. ID and ServiceProvider is mandatory.

All time data regarding maintenance will be stored in scale of days (`yyyy-MM-dd`) and for interval in months. In case the next service is not given, but the interval and last service time is given, the next service time will automatically be inferred as `last service time + interval`.

This endpoint creates new maintenance data on every call to allow for maintenance by several party and adding irregular maintenance on top of regular ones. Hence the responsibility of maintaining the maintenance data will lie on the user.

##### - `/addmanual`
For instantiating manual instances to the assets.
The parameters:
- targetID : The ID of the asset that the manual is meant for
- comments : A remark regarding the manual
- documentType : Whether the document is a `Manual` or a `SpecSheet`
- encoded : Base-64 encoding of the datasheet pdf
- fileName : The name of the file the manual is to be saved as

Example request and successful result:
- Request:
```
POST /asset-manager-agent/addmanualpdf HTTP/1.1
Content-Type: application/json
{"assetData":{"fileName":"test.pdf",
    "encodedPDF":"JVBERi0xLjUNCiW1tbW1DQox..."}}
```
- Result:
```json
{"Result":["Command Success"]}
```

#### Printing
##### - `/printbulk`
Print QR code for a list of assets. Accepts either IRI or ID or mix of both
The parameters are:
- IRI : A JSONArray of IRIs or ID
Example request and succesful response:
- Request:
```
POST /asset-manager-agent/printbulk HTTP/1.1
Content-Type: application/json

{
    "assetData":{
        "IRI":["https://www.theworldavatar.com/kg/ontodevice/Laptop_a9b476a1-55f3-417d-8477-0d03f35b809a", "https://www.theworldavatar.com/kg/ontodevice/Workstation_5113ea7d-ee32-42f6-bb7f-b00b927a8d3a", "https://www.theworldavatar.com/kg/ontodevice/Monitor_2bc85bfa-181e-4599-bbb3-5374c9eacfad", ...]
    }
}
```
- Response:
```json
{"Result":["Bulk printing job initiated for ID: ...", "Command Success"]}
```


##### - `/print`
Print QR code for a single asset. 
Uses the same functionality with `/printbulk`, but accepts a single IRI/ID as the request parameter.
Example:
- Request:
```
POST /asset-manager-agent/print HTTP/1.1
Content-Type: application/json
{
    "assetData" : {
        "IRI" : "Monitor_6fff83e0-f47b-4a72-a325-ba1e840cbcb5"
    }
}
```

- Response:
```json
{"Result":["Bulk printing job initiated for ID: ...", "Command Success"]}
```

#### Retrieval
##### - `/retrieve`
Retrieve asset information regarding an asset given ID/IRI.

The request is composed of the ID or IRI of the target device.
The response will be composed of:
- ID: JSONArray, the first element is the ID and the second the IRI.
- Result : The retrieved data. Composed of three element, in order:
    - A JSONArray of the literals and IRIs of the agent-created instances.
    - A boolean of whether the asset has a related timeseries in the KG. Uses the BMS endpoint as default for this purpose.
    - Confirmation command succeed.
Example:
- Request
```
POST /asset-manager-agent/retrieve HTTP/1.1
Content-Type: application/json

{
    "assetData":{
        "ID":"https://www.theworldavatar.com/kg/ontodevice/Furnace_3e38a41e-97ec-4c94-a84b-03ded5bf093d"
    }
}
```
- Response
```json
{
    "ID":["2018-07-20/791.2","https://www.theworldavatar.com/kg/ontodevice/Furnace_3e38a41e-97ec-4c94-a84b-03ded5bf093d"],
    "Result":[
        {"locationIRI":"http://www.theworldavatar.com/kg/lab/Building_f1d256ed-5d3d-4cbd-b3d9-a608d610b9bb","Manual":"","IFCReprIRI":"http://www.theworldavatar.com/kg/lab/IfcRoomRepresentation_37efb789-97a4-4b4a-8272-04fb60d1a30d","ServiceCategoryName":"A.4.1 ","ManufacturerNameIRI":"","SupplierNameIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/OrganizationName_8a995417-fc3a-4235-9620-31c832cb6ec8","priceIRI":"","personIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Person_d4454dab-7395-47b8-9db2-bc19276bb751","PurchaseOrderNum":"8100011191","itemName":"Customised Furnace","facilityIRI":"http://www.theworldavatar.com/kg/lab/lab_4590421a-898b-4ee0-b30e-2193f1a3345a","priceMeasureIRI":"","price":"","cabinetIRI":"","ServiceCategoryIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/ServiceCategory_cb60abd2-9eda-4824-bbaa-20b0f43444e1","DeliveryOrderLineIRI":"","ManualIRI":"","workspaceIRI":"","itemComment":"","serialNum":"","ManufacturerName":"","SupplierOrgIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/FormalOrganization_9a15dabb-2dff-4e80-8711-a9179a76c27d","deviceIRI":"https://www.theworldavatar.com/kg/ontodevice/Furnace_3e38a41e-97ec-4c94-a84b-03ded5bf093d","roomName":"Thermal Gravimetric Facility","locationIFCReprIRI":"http://www.theworldavatar.com/kg/lab/IfcBuildingRepresentation_df66964b-b3c4-4452-bace-0aa81940e872","buildingName":"CREATE Research Wing","manufacturerURL":"","InvoiceLineIRI":"","DeliveryOrderNum":"","roomIRI":"http://www.theworldavatar.com/kg/lab/Room_5709a0ca-9a06-410c-9e1a-da3e0de1a087","SpecSheetIRI":"","SpecSheetPage":"","personNameIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PersonName_d4e73225-ff26-4d62-bfb0-ff4c20b252e7","storage":"","facilityTypeIRI":"https://www.theworldavatar.com/kg/ontolab/Laboratory","assignedTo":"Dogancan KARAN","ManufacturerIRI":"","DeliveryOrderIRI":"","InvoiceIRI":"","ServiceCategoryType":"IRP1 SMALL LAB EQUIPMENT (<6K)","workspaceName":"","facilityName":"CARES Lab 1","PriceDetailsIRI":"","cabinetTypeIRI":"","amtMoney":"","deviceTypeIRI":"https://www.theworldavatar.com/kg/ontodevice/Furnace","label":"Customised Furnace","PurchaseOrderLineIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PurchaseOrderLineIRI_19196240-1a9e-4258-a6fe-cfda810f5fa8","storageIRI":"","itemIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Item_031a37dd-87f8-4215-a1a0-4281ac46e4a8","roomTypeIRI":"https://www.theworldavatar.com/kg/ontobim/Room","SupplierName":"Tiantian Resources Pte.ltd","currencyIRI":"","modelNumber":"","PurchaseOrderIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PurchaseOrder_5be65895-8a3d-4d77-9e92-45eeb2c28d60","SpecSheet":"","Location":"","storageID":"","InvoiceNum":""},
        false,
        "Command Success"]}
```

##### - `/retrievebydocs`
Retrieve all asset IRI associated with the given set of financial documents.

The parameter of the request:
- ID:
    - DO : Delivery Order number
    - invoice : invoice number
    - PO : Purchase Order number
    These parameters are optional, allowing any to be left blank. The agent will search for any asset with any given document, with or without the blank parameter. For example, if the DO is left blank, it will return assets with given invoice and PO number whether they have a DO or not. The agent currently does not have a restrictive option on this endpoint as its deemed unneccesarry.

An example of a request and successful response:
- Request:
```
POST /asset-manager-agent/retrievebydocs HTTP/1.1
Content-Type: application/json

{
    "assetData": {
        "ID": {
            "DO": "https://www.theworldavatar.com/kg/ontoassetmanagement/DeliveryOrder_f8b82930-bae3-4ff8-a001-5c5a57030dbb",
            "invoice": "https://www.theworldavatar.com/kg/ontoassetmanagement/Invoice_8e59a755-2110-42f6-874b-2b0df985c832",
            "PO": "https://www.theworldavatar.com/kg/ontoassetmanagement/PurchaseOrder_17358fa9-f2bd-438a-9a7e-c919eff6305d"
        }
    }
}
```
- Response : 
```json
{"Result":
    [
        [
            {"assetIRI":"https://www.theworldavatar.com/kg/ontodevice/Monitor_4ae9d7f2-99eb-4f30-a8ab-dab5d02c8ca6","assetID":"2021-12-06/1087","itemIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Item_718c9ded-997b-4212-b062-3b376808d8d2"},
            {"assetIRI":"https://www.theworldavatar.com/kg/ontodevice/Monitor_a0bfb761-716a-4ca1-a017-a74fd229dd30","assetID":"2021-12-06/1088","itemIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Item_6bf026b2-8931-4374-8f5d-45eac3bff496"},
            ...
        ],
        "Command Success"
    ]
}
```

##### - `/getuidata`
Retrieve information for the UI of the AssetManagementApp on load. The information retrieved includes (Names and IRIs):
- User : All person in the knowledge graph
- Workspace : All pair of room and workspace
- Element : All pair of asset and workspace
- Supplier : Supplier organizations
- Manufacturer : Manufacturerers organizations
- Invoice : All invoice
- PurchaseOrder : All Purchase Order
- DeliveryORder : All Delivery Order

This endpoint does not accept any request parameter

#### Delete
##### - `/delete`
Deletes asset information from all namespace. As if the asset is never instantiated.

The delete is done by using `/retrieve` to retrieve asset-related IRI of the asset. Some of these IRIs are then used to find triples with the asset IRI and/or item IRI as either subject or object in the provided namespace to be deleted. (*NOTE:This is why it takes ~ 10s per delete. A better delete query could be made, but it will be more specific and require development time. As this was a hotfix for a previous mistake, this is the easiest and fastly developed universal delete I can come up with at the moment*). Therefore this does not delete instances not directly affiliated with the asset, for example, the person instances of the user and the organizations. This is done in case the instance is affiliated with other triples shich is immportant for other assets / agents.

The delete command records the deleted asset in a log file in `\app\deleteLog.log` in the container. Each triple deleted will be associated to a UUID in the following format:
```
[UUID]::[endpoint triple is deleted from]::{"subject":[IRI], "predicate":[IRI], "object":[IRI]}
```
This is meant as a precaution of mis-delete, an undo feature. The actual undo however is not yet implemented as currently the deletion works fine (Its best to have the undo implemented eventually). The deleted triple recorded is obtained by doing a SELECT query with a similar query to the delete query before each deletion. (*NOTE: Again, also why each delete takes ~10s per delete.*). 


***You may want to backup `\app\deleteLog.log` before updating/deleting the container***


The parameter accepted:
- ID : The ID or IRI of the device.

Example request:
- Request
```
POST /asset-manager-agent/delete HTTP/1.1
Content-Type: application/json

{
    "assetData":{
        "ID": "https://www.theworldavatar.com/kg/ontoelecpowerequipment/ACPowerQualityAnalyser_8bb4aef6-459e-49e7-9a79-e0c313455e15"
    }
}
```

This endpoint does not return any response except a `"Command Success"` confirmation when successful.
