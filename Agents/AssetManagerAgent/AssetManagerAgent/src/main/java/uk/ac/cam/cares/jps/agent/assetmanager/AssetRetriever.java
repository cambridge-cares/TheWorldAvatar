package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.GroupBy;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

public class AssetRetriever {
    private RemoteStoreClient storeClientAsset, storeClientDevice, storeClientPurchDoc;
    JSONObject result = new JSONObject();
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);
    public AssetRetriever (RemoteStoreClient clientAsset, RemoteStoreClient clientDevice, RemoteStoreClient clientPurchDoc) {
        storeClientAsset = clientAsset;
        storeClientDevice = clientDevice;
        storeClientPurchDoc = clientPurchDoc;
    }




        /**
     * =============================================================================================================================================================
     * RETRIEVE
     * =============================================================================================================================================================
     * Retrieve asset based on data from given request
     */
    JSONObject retrieve (String ID) {
        retrieveAssetNamespace(ID);

        Iri deviceIRI, itemIRI;
        deviceIRI = iri(result.getString("deviceIRI"));
        itemIRI = iri(result.getString("itemIRI"));

        retrieveDeviceNamespace(deviceIRI);
        retrievePurchaseDocNamespace(itemIRI);

        return result;
    }


    private void retrieveAssetNamespace(String ID) {

        //Asset namespace query
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable deviceIRIVar = SparqlBuilder.var("deviceIRI");
        Variable deviceTypeIRI = SparqlBuilder.var("deviceTypeIRI");
        Variable itemIRIVar = SparqlBuilder.var("itemIRI");
        Variable labelLiteralVar = SparqlBuilder.var("label");
        //Owner and human
        Variable PersonIRI = SparqlBuilder.var("personIRI");
        Variable personNameIRI = SparqlBuilder.var("personNameIRI");
        Variable deviceOwnerLiteral = SparqlBuilder.var("assignedTo");
        //Serial and model number
        Variable serialNumberLiteral = SparqlBuilder.var("serialNum");
        Variable modelNumberLiteral = SparqlBuilder.var("modelNumber");
        //price and money
        Variable amountOfMoneyVar = SparqlBuilder.var("amtMoney");
        Variable priceMeasureIRI = SparqlBuilder.var("priceMeasureIRI");
        Variable priceLiteralVar = SparqlBuilder.var("price");
        Variable priceCurrencyIRI = SparqlBuilder.var("currencyIRI");
        //Spec sheets and manual
        Variable manualURL = SparqlBuilder.var("manualURL");
        Variable SpecSheetIRI = SparqlBuilder.var("SpecSheetIRI"); 
        Variable SpecSheetFileLiteral = SparqlBuilder.var("SpecSheet"); 
        Variable SpecSheetPageLiteral = SparqlBuilder.var("SpecSheetPage");
        Variable ManualIRI = SparqlBuilder.var("ManualIRI"); 
        Variable ManualFileLiteral = SparqlBuilder.var("Manual"); 
        //Supplier and manuf
        Variable SupplierOrgIRI = SparqlBuilder.var("SupplierOrgIRI");
        Variable SupplierNameIRI = SparqlBuilder.var("SupplierNameIRI");
        Variable SupplierNameLiteral = SparqlBuilder.var("SupplierName");
        Variable ManufacturerOrgIRI = SparqlBuilder.var("ManufacturerIRI");
        Variable ManufacturerNameIRI = SparqlBuilder.var("ManufacturerNameIRI");
        Variable ManufacturerNameLiteral = SparqlBuilder.var("ManufacturerName");
        //Workspace
        Variable WorkspaceIRI = SparqlBuilder.var("workspaceIRI");
        Variable WorkspaceIDLiteral = SparqlBuilder.var("workspaceName");
        Variable storageIRI = SparqlBuilder.var("storage");

        /*
         * RETRIEVE QUERY
         */
        //Get the "entrypoint" from ID to the 3 namespaces in asset namespace
        //For asset the "entrypoint" is ID, we can get Device IRI (hasInventoryID) and Item IRI ( references device iri)


        //get device on "deviceVar" from asset namespace
        query.where(deviceIRIVar.has(hasItemInventoryIdentifier, ID));
        query.where(deviceIRIVar.isA(deviceTypeIRI));
        //get Item IRI from device IRI from asset namespace
        query.where(itemIRIVar.has(references, deviceIRIVar));


        //While we're querying the asset namespace, query for other available info too
        //Device name from asset list
        query.where(deviceIRIVar.has(RDFS.LABEL, labelLiteralVar));
        //Device owner from asset list
        query.where(deviceIRIVar.has(assignedTo, PersonIRI));
        query.where(PersonIRI.has(hasName, personNameIRI));
        query.where(personNameIRI.has(hasPersonName, deviceOwnerLiteral));
        //Optional IRIs
        //Workspace
        query.where(GraphPatterns.optional(PersonIRI.has(hasAllocatedWorkspace, WorkspaceIRI)));
        query.where(GraphPatterns.optional(WorkspaceIRI.has(hasWorkspaceIdentifier, WorkspaceIDLiteral)));
        //Serial number
        query.where(GraphPatterns.optional(deviceIRIVar.has(serialNumber, serialNumberLiteral)));
        //model number
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasModel, modelNumberLiteral)));
        //manual URL
        query.where(GraphPatterns.optional(deviceIRIVar.has(RDFS.SEEALSO, manualURL)));
        //Price
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasPrice, amountOfMoneyVar)));
        query.where(GraphPatterns.optional(amountOfMoneyVar.has(hasValue, priceMeasureIRI)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasNumericalValue, priceLiteralVar)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasUnit, priceCurrencyIRI)));

        //Specsheet 
        query.where(GraphPatterns.optional(SpecSheetIRI.isA(SpecSheet)));
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasDataSheet, SpecSheetIRI)));
        query.where(GraphPatterns.optional(SpecSheetIRI.has(availableAt, SpecSheetFileLiteral)));
        query.where(GraphPatterns.optional(SpecSheetIRI.has(RDFS.COMMENT, SpecSheetPageLiteral)));
        
        //Manual
        query.where(GraphPatterns.optional(ManualIRI.isA(SpecSheet)));
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasDataSheet, ManualIRI)));
        query.where(GraphPatterns.optional(ManualIRI.has(availableAt, ManualFileLiteral)));
        //Supplier & manufacturer
        query.where(GraphPatterns.optional(deviceIRIVar.has(isSuppliedBy, SupplierOrgIRI)));
        query.where(GraphPatterns.optional(SupplierOrgIRI.has(hasName, SupplierNameIRI)));
        query.where(GraphPatterns.optional(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral)));
        query.where(GraphPatterns.optional(deviceIRIVar.has(isManufacturedBy, SupplierOrgIRI)));
        query.where(GraphPatterns.optional(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI)));
        query.where(GraphPatterns.optional(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral)));

        handleAssetData(storeClientAsset.executeQuery(query.getQueryString()));
        
    }
    
    private void handleAssetData (JSONArray requestResult) {
        String[] keyArray = {
                "deviceIRI","deviceTypeIRI","itemIRI","label","personIRI","personNameIRI","assignedTo","serialNum",
                "modelNumber", "amtMoney", "priceMeasureIRI", "price", "currencyIRI", "manualURL", "SpecSheetIRI",
                "SpecSheet", "SpecSheetPage", "ManualIRI", "Manual", "SupplierOrgIRI", "SupplierNameIRI",
                "SupplierName","ManufacturerIRI", "ManufacturerNameIRI","ManufacturerName","workspaceIRI",
                "workspaceName", "storage"
            };

        inputDataToResult(keyArray, requestResult);
    }

    void retrieveDeviceNamespace (Iri deviceIRI) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable deviceTypeIRI = SparqlBuilder.var("deviceTypeIRI");
        Variable roomIRI = SparqlBuilder.var("roomIRI");
        Variable roomTypeIRI = SparqlBuilder.var("roomTypeIRI");
        Variable IFCReprIRI = SparqlBuilder.var("IFCReprIRI");
        Variable facilityIRI = SparqlBuilder.var("facilityIRI");
        Variable facilityTypeIRI = SparqlBuilder.var ("facilityTypeIRI");
        Variable locationIRI = SparqlBuilder.var("locationIRI");
        Variable locationIFCReprIRI = SparqlBuilder.var("locationIFCReprIRI");
        Variable WorkspaceIRI = SparqlBuilder.var("WorkspaceIRI");
        Variable WorkspaceIDLiteral = SparqlBuilder.var("WorkspaceID");
        Variable LocationString = SparqlBuilder.var("Location");
        Variable roomName = SparqlBuilder.var("roomName");
        Variable facilityName = SparqlBuilder.var("facilityName");
        Variable buildingName = SparqlBuilder.var("buildingName");
        //Storage
        Variable storageIRI = SparqlBuilder.var("storageIRI");
        Variable cabinetIRI = SparqlBuilder.var("cabinetIRI");
        Variable cabinetTypeIRI =SparqlBuilder.var("cabinetTypeIRI");
        Variable storageIDLiteral = SparqlBuilder.var("storageID");
        
        //Query
        //get device type
        query.where(deviceIRI.has(RDF.TYPE, deviceTypeIRI));
        //get location
        query.where(GraphPatterns.optional(roomIRI.has(containsElement, deviceIRI)));
        query.where(GraphPatterns.optional(roomIRI.isA(roomTypeIRI)));

        query.where(GraphPatterns.optional(deviceIRI.has(isStoredIn, storageIRI)));

        query.where(GraphPatterns.optional(IFCReprIRI.has(RDFS.LABEL, roomName)));
        query.where(GraphPatterns.optional(roomIRI.has(hasIfcRepresentation, IFCReprIRI)));
        query.where(GraphPatterns.optional(roomIRI.isA(roomTypeIRI)));
        query.where(GraphPatterns.optional(facilityIRI.has(hasRoom, roomIRI)));
        query.where(GraphPatterns.optional(facilityIRI.isA(facilityTypeIRI)));
        query.where(GraphPatterns.optional(facilityIRI.has(RDFS.LABEL, facilityName)));
        query.where(GraphPatterns.optional(locationIRI.has(hasFacility, facilityIRI)));
        query.where(GraphPatterns.optional(locationIRI.has(hasIfcRepresentation, locationIFCReprIRI)));
        query.where(GraphPatterns.optional(locationIFCReprIRI.has(RDFS.LABEL, buildingName)));
        query.where(GraphPatterns.optional(cabinetIRI.isA(cabinetTypeIRI)));
        query.where(GraphPatterns.optional(deviceIRI.has(isStoredIn, cabinetIRI)));
        query.where(GraphPatterns.optional(cabinetIRI.has(hasFurnitureIdentifier, storageIDLiteral)));

        query.where(GraphPatterns.optional(deviceIRI.has(hasCurrentLocation, LocationString)));


        //Workspace
        query.where(GraphPatterns.optional(deviceIRI.has(isLocatedAt, WorkspaceIRI)));
        query.where(GraphPatterns.optional(WorkspaceIRI.has(hasIdentifier, WorkspaceIDLiteral)));


        
        handleDeviceData(storeClientDevice.executeQuery(query.getQueryString()));
    }

    void handleDeviceData (JSONArray requestResult) {
        String[] keyArray = {
            "deviceTypeIRI",
            "roomIRI",
            "roomTypeIRI",
            "IFCReprIRI",
            "facilityIRI",
            "facilityTypeIRI",
            "locationIRI",
            "locationIFCReprIRI",
            "WorkspaceIRI",
            "WorkspaceID",
            "Location",
            "roomName",
            "facilityName",
            "buildingName",
            "storageIRI",        
            "cabinetIRI",
            "cabinetTypeIRI",
            "storageID"
        };

        inputDataToResult(keyArray, requestResult);

    }

    void retrievePurchaseDocNamespace (Iri itemIRI) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable itemNameLiteral = SparqlBuilder.var("itemName");
        Variable itemCommentLiteral = SparqlBuilder.var("itemComment");
        Variable ServiceCategoryIRI = SparqlBuilder.var("ServiceCategoryIRI");
        Variable ServiceCategoryNameLiteral = SparqlBuilder.var("ServiceCategoryName");
        Variable ServiceCategoryTypeLiteral = SparqlBuilder.var("ServiceCategoryType");
        //invoice instances
        Variable InvoiceIRI = SparqlBuilder.var("InvoiceIRI");
        Variable InvoiceNumLiteral = SparqlBuilder.var("InvoiceNum");
        Variable invoiceLineIRI = SparqlBuilder.var("InvoiceLineIRI");
        Variable DeliveryOrderIRI = SparqlBuilder.var("DeliveryOrderIRI");
        Variable PurchaseOrderIRI = SparqlBuilder.var("PurchaseOrderIRI");
        Variable DeliveryOrderLineIRI = SparqlBuilder.var("DeliveryOrderLineIRI");
        Variable DeliveryOrderNumLiteral = SparqlBuilder.var("DeliveryOrderNum");
        Variable PurchaseOrderLineIRI = SparqlBuilder.var("PurchaseOrderLineIRI");
        Variable PurchaseOrderNumLiteral = SparqlBuilder.var("PurchaseOrderNum");

        //price instances
        Variable priceDetailsIRI = SparqlBuilder.var("PriceDetailsIRI");
        Variable priceIRI = SparqlBuilder.var("priceIRI");
        Variable priceMeasureIRI = SparqlBuilder.var("priceMeasureIRI");
        Variable priceLiteral = SparqlBuilder.var("price");
        Variable priceCurrencyIRI = SparqlBuilder.var("currencyIRI");
        //QUERY
        //item data
        query.where(itemIRI.has(itemName, itemNameLiteral));
        query.where(itemIRI.has(RDFS.COMMENT, itemCommentLiteral));
        query.where(ServiceCategoryIRI.has(attributeName, ServiceCategoryTypeLiteral));
        query.where(ServiceCategoryIRI.has(attributeValue, ServiceCategoryNameLiteral));
        //OPTIONAL QUERIES

        //Invoice, DO and PO
        //Invoice
        query.where(GraphPatterns.optional(invoiceLineIRI.has(hasItem, itemIRI)));
        query.where(GraphPatterns.optional(InvoiceIRI.has(hasInvoiceLine, invoiceLineIRI)));
        query.where(GraphPatterns.optional(InvoiceIRI.has(invoiceNumber, InvoiceNumLiteral)));
        //DO
        query.where(GraphPatterns.optional(DeliveryOrderLineIRI.has(hasItem, itemIRI)));
        query.where(GraphPatterns.optional(DeliveryOrderIRI.has(hasDeliveryOrderLine, DeliveryOrderLineIRI)));
        query.where(GraphPatterns.optional(DeliveryOrderIRI.has(deliveryOrderNumber, DeliveryOrderNumLiteral)));

        //PO
        query.where(GraphPatterns.optional(PurchaseOrderLineIRI.has(hasItem, itemIRI)));
        query.where(GraphPatterns.optional(PurchaseOrderIRI.has(hasPurchaseOrderLine, PurchaseOrderLineIRI)));
        query.where(GraphPatterns.optional(PurchaseOrderIRI.has(purchaseOrderNumber, PurchaseOrderNumLiteral)));
        
        //Price
        query.where(GraphPatterns.optional(invoiceLineIRI.has(hasPriceDetails, priceDetailsIRI)));
        query.where(GraphPatterns.optional(priceDetailsIRI.has(hasPrice, priceIRI)));
        query.where(GraphPatterns.optional(priceIRI.has(hasValue, priceMeasureIRI)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasNumericalValue, priceLiteral)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasUnit, priceCurrencyIRI)));
        

        handlePurchaseDocData(storeClientPurchDoc.executeQuery(query.getQueryString()));
    }

    void handlePurchaseDocData (JSONArray requestResult) {
        String[] keyArray = {
            "itemName",
            "itemComment",
            "ServiceCategoryIRI",
            "ServiceCategoryName",
            "ServiceCategoryType",
            "InvoiceIRI",
            "InvoiceNum",
            "InvoiceLineIRI",
            "DeliveryOrderIRI",
            "PurchaseOrderIRI",
            "DeliveryOrderLineIRI",
            "DeliveryOrderNum",
            "PurchaseOrderLineIRI",
            "PurchaseOrderNum",
            "PriceDetailsIRI",
            "priceIRI",
            "priceMeasureIRI",
            "price",
            "currencyIRI"
        };

        inputDataToResult(keyArray, requestResult);

    }


    void inputDataToResult (String[] keyArray, JSONArray requestResult){
        /*
         * Format to follow
         * {"locationIRI":"https://www.theworldavatar.com/kg/caresOffice/Building_ad6bad2b-74d7-43d0-ade9-cb1efaa31557",
         * "Manual":"","PONum":" POC4T2RE_00659","ServiceCategoryName":"","ManufacturerNameIRI":"",
         * "SupplierNameIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/OrganizationName_ec9fc13c-8a46-487d-a7c0-76b36d776fc9",
         * "priceIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Price_cbb3d8bb-4b48-43b7-a0fd-e83059176168",
         * "personIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Person_624a471d-e9e7-4851-b4c5-652b4ea985c6",
         * "RoomIRI":"https://www.theworldavatar.com/kg/caresOffice/Room_97b7faec-8961-4211-adec-464599e99eb0",
         * "facilityIRI":"https://www.theworldavatar.com/kg/caresOffice/Office_333f4a32-dc2a-4c6d-9226-3750b14c577e",
         * "priceMeasureIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Measure_6e3ce011-506d-4db2-9349-586e27ca517e",
         * "price":"285", "cabinetIRI":"","DONum":"911356038",
         * "ServiceCategoryIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/ServiceCategory_5c7658c8-d9c3-4ba8-95e3-6df2c5f8460b",
         * "ManufacturerOrgIRI":"","ID":"2021-12-06/1087",
         * "DeliveryOrderLineIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/DeliveryOrderLineIRI_8a752a38-6d64-46b8-9d28-6180264a7970",
         * "itemComment":" POC4T2RE_00659","workspaceIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Workspace_cd3e9e7d-03ed-4d35-9930-de844c35c9c8",
         * "ManualIRI":"","ManufacturerName":"","serialNum":"1FV6RD3",
         * "SupplierOrgIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/FormalOrganization_3b9d0896-ca0f-4971-a2ee-fc969673be1e",
         * "deviceIRI":"https://www.theworldavatar.com/kg/ontodevice/Monitor_4ae9d7f2-99eb-4f30-a8ab-dab5d02c8ca6",
         * "InvoiceLineIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/InvoiceLine_de8b2db4-bd29-4cc8-b581-fbda711e739b",
         * "SpecSheetIRI":"","SpecSheetPage":"","personNameIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PersonName_574c1be6-68e9-4800-ae0f-18055ecaf78f",
         * "assignedTo":"John Chan","DeliveryOrderIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/DeliveryOrder_f8b82930-bae3-4ff8-a001-5c5a57030dbb",
         * "InvoiceIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Invoice_8e59a755-2110-42f6-874b-2b0df985c832",
         * "ServiceCategoryType":"P2_REPLACEMENTS OF DESKTOP COMPUTERS","workspaceName":"CAMW34",
         * "manualURL":"","PriceDetailsIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PriceDetails_43acbfb9-ef7b-4599-a570-058d974e2f0a",
         * "cabinetTypeIRI":"","amtMoney":"https://www.theworldavatar.com/kg/ontodevice/AmountOfMoney_e0992219-6118-413b-871a-9858019b9546",
         * "deviceTypeIRI":"https://www.theworldavatar.com/kg/ontodevice/Monitor","label":"Dell 23 Monitor - P2319h",
         * "PurchaseOrderLineIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PurchaseOrderLineIRI_284ccfcb-cd0b-40bc-98b1-09c0a9fb8317",
         * "storageIRI":"","itemIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/Item_718c9ded-997b-4212-b062-3b376808d8d2",
         * "SupplierName":"Lazada.sg","currencyIRI":"http://www.ontology-of-units-of-measure.org/resource/om-2/SingaporeDollar",
         * "modelNumber":"","PurchaseOrderIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PurchaseOrder_17358fa9-f2bd-438a-9a7e-c919eff6305d",
         * "SpecSheet":"","storageID":"","Location":"CREATE Tower","InvoiceNum":"4401041794"}
         * 
         * 
         * 
         * Assume there is only one result possible. Any more than 1 is considered a duplicate and throws an error
         */

        switch (requestResult.length()) {
            case 0:
                result = null;
                break;
            case 1:
            
             for(String key : keyArray){
                result.put(key, requestResult.getJSONObject(0).getString(key));
             }

            default:
                throw new JPSRuntimeException("Duplicate data on retrieve. Check the knowledge graph for duplicates.");
        }
    }

    public JSONObject getRequiredIriUI () {
        JSONObject result = new JSONObject();
        //For type will need to check with the ontology insetad. Not yet implemented
        //as the ontology for some of the stuff are not finalised

        //user -- Also retrieve PersonNameIRI
        result.put("User", getAllPersonIRI());
        //location -- Retrieving all the rooms is a bit more complicated than I thought. Not yet implemented ~MTL
        //Room
        //Workspace
        //Element in workspace
        result.put("Element", getIriListByPredicate(containsElement, storeClientDevice));
        //supplier
        result.put("Supplier", getAllSupplierIRI());
        //Manufacturer
        result.put("Manufacturer", getAllManufacturerIRI());
        //invoice
        result.put("Invoice", getIriListByPredicate(invoiceNumber, storeClientPurchDoc));
        //PurchOrd
        result.put("PurchaseOrder", getIriListByPredicate(purchaseOrderNumber, storeClientPurchDoc));
        //DeliveryOrder
        result.put("DeliveryOrder", getIriListByPredicate(deliveryOrderNumber, storeClientPurchDoc));

        return result;
    }

    private JSONArray getAllPersonIRI() {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable PersonIRI = SparqlBuilder.var("PersonIRI");
        Variable PersonNameIRI = SparqlBuilder.var("PersonNameIRI");
        Variable PersonNameLiteral = SparqlBuilder.var("PersonName");
        query.where(PersonIRI.has(hasName, PersonNameIRI));
        query.where(PersonNameIRI.has(hasPersonName, PersonNameLiteral));

        return storeClientAsset.executeQuery(query.getQueryString());
    }

    private JSONArray getAllSupplierIRI() {
        Variable SupplierOrgIRI = SparqlBuilder.var("SupplierOrgIRI");
        Variable SupplierNameIRI = SparqlBuilder.var("SupplierNameIRI");
        Variable SupplierNameLiteral = SparqlBuilder.var("SupplierName");
        SelectQuery query = Queries.SELECT(SupplierOrgIRI, SupplierNameIRI, SupplierNameLiteral);
        GroupBy group = SparqlBuilder.groupBy();
        group.by(SupplierNameLiteral, SupplierOrgIRI, SupplierNameIRI);
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(isSuppliedBy, SupplierOrgIRI));
        query.where(SupplierOrgIRI.has(hasName, SupplierNameIRI));
        query.where(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));
        query.groupBy(group);

        return storeClientPurchDoc.executeQuery(query.getQueryString());

    }
    private JSONArray getAllManufacturerIRI() {
        Variable ManufacturerOrgIRI = SparqlBuilder.var("ManufacturerIRI");
        Variable ManufacturerNameIRI = SparqlBuilder.var("ManufacturerNameIRI");
        Variable ManufacturerNameLiteral = SparqlBuilder.var("ManufacturerName");
        GroupBy group = SparqlBuilder.groupBy();
        group.by(ManufacturerNameLiteral, ManufacturerOrgIRI, ManufacturerNameIRI);

        SelectQuery query = Queries.SELECT(ManufacturerOrgIRI, ManufacturerNameIRI, ManufacturerNameLiteral);
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(isManufacturedBy, ManufacturerOrgIRI));
        query.where(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
        query.where(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral));
        query.groupBy(group);

        return storeClientPurchDoc.executeQuery(query.getQueryString());

    }

    public JSONArray getItemListByDocIRI (String InvoiceIRI, String POiri, String DOiri){

        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable itemIRI = SparqlBuilder.var("itemIRI");
        Variable InvoiceLineIRI = SparqlBuilder.var("invoiceLineIRI");
        Variable POLineIRI = SparqlBuilder.var("POLineIRI");
        Variable DOLineIRI = SparqlBuilder.var("DOLineIRI");

        if(!(InvoiceIRI == null || InvoiceIRI.isBlank())){
            query.where(iri(InvoiceIRI).has(hasInvoiceLine, InvoiceLineIRI));
            query.where(InvoiceLineIRI.has(hasItem, itemIRI));
        }
        if(!(POiri == null || POiri.isBlank())){
            query.where(iri(POiri).has(hasPurchaseOrderLine, POLineIRI));
            query.where(POLineIRI.has(hasItem, itemIRI));
        }
        if(!(DOiri == null || DOiri.isBlank())){
            query.where(iri(DOiri).has(hasDeliveryOrderLine, DOLineIRI));
            query.where(DOLineIRI.has(hasItem, itemIRI));
        }

        return storeClientPurchDoc.executeQuery(query.getQueryString());
    }

}
