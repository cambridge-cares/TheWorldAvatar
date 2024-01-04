package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.Triple;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.query.algebra.Service;
import org.eclipse.rdf4j.sparqlbuilder.core.GroupBy;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfObject;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfPredicate;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfSubject;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

import java.util.ArrayList;

public class AssetRetriever {
    private RemoteStoreClient storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab;
    JSONObject result = new JSONObject();
    String assetNamespace, deviceNamespace, purchaseDocsNamespace, labNamespace;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);
    public AssetRetriever (RemoteStoreClient clientAsset, RemoteStoreClient clientOffice, RemoteStoreClient clientPurchDoc, RemoteStoreClient clientLab, 
                            String assetKGNamespace, String deviceKGNamespace, String purchaseDocsKGNamespace, String labKGNamespace) {
        storeClientAsset = clientAsset;
        storeClientOffice = clientOffice;
        storeClientPurchDoc = clientPurchDoc;
        storeClientLab = clientLab;

        assetNamespace = assetKGNamespace;
        deviceNamespace = deviceKGNamespace;
        purchaseDocsNamespace = purchaseDocsKGNamespace;
        labNamespace = labKGNamespace;
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
        Variable manufacturerURL = SparqlBuilder.var("manufacturerURL");
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
        query.where(GraphPatterns.optional(deviceIRIVar.has(isLocatedAt, WorkspaceIRI),
            WorkspaceIRI.has(hasIdentifier, WorkspaceIDLiteral)
        ));
        //Serial number
        query.where(GraphPatterns.optional(deviceIRIVar.has(serialNumber, serialNumberLiteral)));
        //model number
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasModel, modelNumberLiteral)));
        //manual URL
        query.where(GraphPatterns.optional(deviceIRIVar.has(RDFS.SEEALSO, manufacturerURL)));
        //Price
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasPrice, amountOfMoneyVar),
            amountOfMoneyVar.has(hasValue, priceMeasureIRI),
            priceMeasureIRI.has(hasNumericalValue, priceLiteralVar),
            priceMeasureIRI.has(hasUnit, priceCurrencyIRI)
        ));

        //Specsheet 
        query.where(GraphPatterns.optional(
            deviceIRIVar.has(hasDataSheet, SpecSheetIRI),
            SpecSheetIRI.isA(SpecSheet),
            
            GraphPatterns.optional(SpecSheetIRI.has(availableAt, SpecSheetFileLiteral)),
            GraphPatterns.optional(SpecSheetIRI.has(RDFS.COMMENT, SpecSheetPageLiteral))
        ));
        
        //Manual
        query.where(GraphPatterns.optional(
            deviceIRIVar.has(hasDataSheet, ManualIRI),
            ManualIRI.isA(Manual),

            GraphPatterns.optional(ManualIRI.has(availableAt, ManualFileLiteral))
        ));
        //Supplier & manufacturer
        query.where(GraphPatterns.optional(deviceIRIVar.has(isSuppliedBy, SupplierOrgIRI),
            SupplierOrgIRI.has(hasName, SupplierNameIRI),
            SupplierNameIRI.has(hasLegalName, SupplierNameLiteral)));
        query.where(GraphPatterns.optional(deviceIRIVar.has(isManufacturedBy, ManufacturerOrgIRI),
            ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI),
            ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral)));

        handleAssetData(storeClientAsset.executeQuery(query.getQueryString()));
        
    }
    
    private void handleAssetData (JSONArray requestResult) {
        String[] keyArray = {
                "deviceIRI","deviceTypeIRI","itemIRI","label","personIRI","personNameIRI","assignedTo","serialNum",
                "modelNumber", "amtMoney", "priceMeasureIRI", "price", "currencyIRI", "manufacturerURL", "SpecSheetIRI",
                "SpecSheet", "SpecSheetPage", "ManualIRI", "Manual", "SupplierOrgIRI", "SupplierNameIRI",
                "SupplierName","ManufacturerIRI", "ManufacturerNameIRI","ManufacturerName","workspaceIRI",
                "workspaceName", "storage"
            };

        inputDataToResult(keyArray, requestResult);
    }

    void retrieveDeviceNamespace (Iri deviceIRI) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
        query.where(deviceIRI.isA(deviceTypeIRI));
        //get location
        query.where(
                GraphPatterns.optional(roomIRI.has(containsElement, deviceIRI),
                roomIRI.isA(roomTypeIRI),
                IFCReprIRI.has(RDFS.LABEL, roomName),
                roomIRI.has(hasIfcRepresentation, IFCReprIRI),
                roomIRI.isA(roomTypeIRI),

                facilityIRI.has(hasRoom, roomIRI),
                facilityIRI.isA(facilityTypeIRI),
                GraphPatterns.union(
                    locationIRI.has(hasFacility, facilityIRI),
                    locationIRI.has(hasStorey, facilityIRI)
                ),
                
                facilityIRI.has(RDFS.LABEL, facilityName),
                
                locationIRI.has(hasIfcRepresentation, locationIFCReprIRI),
                locationIFCReprIRI.has(RDFS.LABEL, buildingName)
                )
                //Add union for assets outside of CARES?
                //deviceIRI.has(hasCurrentLocation, LocationString)
        );
        query.where(
                GraphPatterns.optional(
                deviceIRI.has(hasCurrentLocation, LocationString)
        ));
        query.where(GraphPatterns.optional(cabinetIRI.isA(cabinetTypeIRI),
            deviceIRI.has(isStoredIn, cabinetIRI),
            GraphPatterns.optional(cabinetIRI.has(hasFurnitureIdentifier, storageIDLiteral))
        ));

        JSONArray result = storeClientOffice.executeQuery(query.getQueryString());
        if (result.length() == 0){
            result = storeClientLab.executeQuery(query.getQueryString());
        }

        
        handleDeviceData(result);
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
        query.where(itemIRI.has(hasAttribute, ServiceCategoryIRI));
        query.where(ServiceCategoryIRI.has(attributeName, ServiceCategoryTypeLiteral));
        query.where(ServiceCategoryIRI.has(attributeValue, ServiceCategoryNameLiteral));
        //OPTIONAL QUERIES

        //Invoice, DO and PO
        //Invoice
        query.where(GraphPatterns.optional(invoiceLineIRI.has(hasItem, itemIRI), 
            InvoiceIRI.has(hasInvoiceLine, invoiceLineIRI),
            InvoiceIRI.has(invoiceNumber, InvoiceNumLiteral),
            GraphPatterns.optional(invoiceLineIRI.has(hasPriceDetails, priceDetailsIRI),
            priceDetailsIRI.has(hasPrice, priceIRI),
            priceIRI.has(hasValue, priceMeasureIRI),
            priceMeasureIRI.has(hasNumericalValue, priceLiteral),
            priceMeasureIRI.has(hasUnit, priceCurrencyIRI))
        ));
        //DO
        query.where(GraphPatterns.optional(DeliveryOrderLineIRI.has(hasItem, itemIRI),
            DeliveryOrderIRI.has(hasDeliveryOrderLine, DeliveryOrderLineIRI),
            DeliveryOrderIRI.has(deliveryOrderNumber, DeliveryOrderNumLiteral),
            GraphPatterns.optional(DeliveryOrderLineIRI.has(hasPriceDetails, priceDetailsIRI),
            priceDetailsIRI.has(hasPrice, priceIRI),
            priceIRI.has(hasValue, priceMeasureIRI),
            priceMeasureIRI.has(hasNumericalValue, priceLiteral),
            priceMeasureIRI.has(hasUnit, priceCurrencyIRI))
        ));

        //PO
        query.where(GraphPatterns.optional(PurchaseOrderLineIRI.has(hasItem, itemIRI),
            PurchaseOrderIRI.has(hasPurchaseOrderLine, PurchaseOrderLineIRI),
            PurchaseOrderIRI.has(purchaseOrderNumber, PurchaseOrderNumLiteral),
            GraphPatterns.optional(PurchaseOrderLineIRI.has(hasPriceDetails, priceDetailsIRI),
            priceDetailsIRI.has(hasPrice, priceIRI),
            priceIRI.has(hasValue, priceMeasureIRI),
            priceMeasureIRI.has(hasNumericalValue, priceLiteral),
            priceMeasureIRI.has(hasUnit, priceCurrencyIRI))
        ));
        
        

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
         * "manufacturerURL":"","PriceDetailsIRI":"https://www.theworldavatar.com/kg/ontoassetmanagement/PriceDetails_43acbfb9-ef7b-4599-a570-058d974e2f0a",
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
        LOGGER.debug("RetrieveResultLength::"+ requestResult.length());
        LOGGER.debug("RetrieveResult::" + requestResult);
        
        switch (requestResult.length()) {
            case 0:
                for(String key : keyArray){
                    result.put(key, "");
                }
                break;
            case 1:
                for(String key : keyArray){
                    try {
                        result.put(key, requestResult.getJSONObject(0).getString(key));
                    } catch (JSONException e) {
                        result.put(key, "");
                    }
                }
                break;
            default:
                throw new JPSRuntimeException("Duplicate data on retrieve. Check the knowledge graph for duplicates.");
        }
    }

    public JSONObject getRequiredIriUI (String assetNamespace, String deviceNamepsace) {
        JSONObject result = new JSONObject();
        //For type will need to check with the ontology insetad. Not yet implemented
        //as the ontology for some of the stuff are not finalised

        //user -- Also retrieve PersonNameIRI
        result.put("User", getAllPersonIRI());
        //location -- Retrieving all the rooms is a bit more complicated than I thought. Not yet implemented ~MTL
        //Room
        //Workspace
        result.put("Workspace", getAllRoomWorkspacePair());
        //Element in workspace
        result.put("Element", getAllElementInWorkspace());
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(isManufacturedBy, ManufacturerOrgIRI));
        query.where(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
        query.where(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral));
        query.groupBy(group);

        return storeClientPurchDoc.executeQuery(query.getQueryString());

    }

    public JSONArray getItemListByDocIRI (String InvoiceIRI, String POiri, String DOiri){

        Variable itemIRI = SparqlBuilder.var("itemIRI");
        Variable InvoiceLineIRI = SparqlBuilder.var("invoiceLineIRI");
        Variable POLineIRI = SparqlBuilder.var("POLineIRI");
        Variable DOLineIRI = SparqlBuilder.var("DOLineIRI");
        Variable assetIRI = SparqlBuilder.var("assetIRI");
        Variable assetID = SparqlBuilder.var("assetID");

        SelectQuery query = Queries.SELECT(itemIRI, assetIRI, assetID);
        GroupBy group = SparqlBuilder.groupBy();
        group.by(itemIRI, assetIRI, assetID);
        query.groupBy(group);
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );

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

        String queryWithService = query.getQueryString();
        //queryWithService = queryWithService.substring(0, queryWithService.length()-1);
        String queryWithServiceStart = queryWithService.substring(0,queryWithService.lastIndexOf("}"));
        String queryWithServiceEnd = queryWithService.substring(queryWithService.lastIndexOf("}"), queryWithService.length());
        queryWithService = queryWithServiceStart + 
                            "\n SERVICE <"+assetNamespace+"> {\n" +
                            "       ?itemIRI ontoassetmanagement:references ?assetIRI}\n"+
                            "\n SERVICE <"+assetNamespace+"> {\n" +
                            "       ?assetIRI ontoassetmanagement:hasItemInventoryIdentifier ?assetID}\n"+ 
                            queryWithServiceEnd
        ;

        return storeClientPurchDoc.executeQuery(queryWithService);
    }

    public JSONArray getAllRoomWorkspacePair (){
        Variable roomIRI = SparqlBuilder.var("roomIRI");
        Variable IFCReprIRI = SparqlBuilder.var("IFCReprIRI");
        Variable workspaceIRI = SparqlBuilder.var("workspaceIRI");
        Variable workspaceID = SparqlBuilder.var("workspaceID");
        Variable roomName = SparqlBuilder.var("roomName");

        JSONArray result = new JSONArray();
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(IFCReprIRI.has(RDFS.LABEL, roomName));
        query.where(roomIRI.has(hasIfcRepresentation, IFCReprIRI));
        query.where(workspaceIRI.has(isLocatedIn, roomIRI));
        query.where(workspaceIRI.has(hasWorkspaceIdentifier, workspaceID));

        result.put(storeClientOffice.executeQuery(query.getQueryString()));
        result.put(storeClientLab.executeQuery(query.getQueryString()));
        JSONArray concatenatedRes = concatArray(storeClientOffice.executeQuery(query.getQueryString()), storeClientLab.executeQuery(query.getQueryString()));
        return concatenatedRes;
    }

    public JSONArray getAllElementInWorkspace(){
        /*TODO modify to rdf4j format
         * Yes, I'm being lazy again here. But it works (sometimes), so who cares
         */
        JSONArray result = new JSONArray();
        String[] namespaceToCheck = {deviceNamespace, labNamespace};
        for (String ns : namespaceToCheck){
            String query = "PREFIX ontoassetmanagement: <" + ONTOASSET + ">\n"+
                "SELECT *\n" + 
                "WHERE {\n" + 
                "   SERVICE <"+ns+"> {\n" + 
                "       ?assetIRI ontoassetmanagement:isLocatedAt ?workspaceIRI.\n" + 
                "       ?workspaceIRI ontoassetmanagement:hasWorkspaceIdentifier ?workspaceID.\n" + 
                "       SERVICE <"+assetNamespace+"> {\n" + 
                "           ?assetIRI ontoassetmanagement:hasItemInventoryIdentifier ?assetID.\n" + 
                "       }\n" + 
                "   }\n" + 
                "}";

        result.put(storeClientOffice.executeQuery(query));
        }

        JSONArray concatenatedRes = concatArray(result.getJSONArray(0), result.getJSONArray(1));
        return concatenatedRes;
    }

    private JSONArray concatArray(JSONArray... arrs)
            throws JSONException {
        JSONArray result = new JSONArray();
        for (JSONArray arr : arrs) {
            for (int i = 0; i < arr.length(); i++) {
                result.put(arr.get(i));
            }
        }
        return result;
    }

    public Boolean getMeasuresExistence (RemoteStoreClient storeClientDB, String IRI, JSONArray pred, int searchDepth) {
        //Do (pseudo-)BFS to the specified depth with all the checked predicates
        //We don't know what the asset type is for general use, so need to do this
        //If the asset is known, please input from the request parameter

        //WHy don't I do normal BFS? -> Every single check will require a query which may take time
        //This method allows us to check the breadth in 1 query, albeit the agent may not find the timeseries despite existing

        //RemoteStoreClient storeClientDB = new RemoteStoreClient(dbName);
        LOGGER.info("Starting TS search...");
        for (int d = 1; d<= searchDepth; d++){
            SelectQuery query = Queries.SELECT();
            GraphPattern[] tripleArr = new GraphPattern[pred.length()];
            ArrayList<GraphPattern> tripleList = new ArrayList<GraphPattern>();

            Variable lastObj;
            Variable object;

            object = query.var();
            lastObj = object;
            for (int i = 0; i <d; i++){
                RdfSubject subject;
                RdfPredicate predicate;
                
                object = query.var();
                if (i == 0){
                    subject = iri(IRI);
                }
                else{
                    subject = lastObj;
                }

                if (i == (d-1)){
                    //Use predicate from the JSONArray
                    for (int predInd = 0; predInd < pred.length(); predInd++){
                        predicate = iri(pred.getString(predInd));
                        tripleList.add(GraphPatterns.tp(subject, predicate, object));
                    }
                    query.where(GraphPatterns.union(tripleList.toArray(tripleArr)));
                }
                else{
                    predicate = query.var();
                    query.where(GraphPatterns.tp(subject, predicate, object));
                }
                LOGGER.debug("TS SEARCH QUERY::" + query.getQueryString());
                
                JSONArray reqResult = storeClientDB.executeQuery(query.getQueryString());
                if (reqResult.length() > 1){
                    return true;
                }

                lastObj = object;
                //NOTE
                //Could also run the algo backward wiht IRI as object instead.
                //Keep in mind the graph is "technically bidirectional"

            }
        }
        return false;

    }

}
