package uk.ac.cam.cares.jps.agent.assetmanager;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.json.JSONArray;
import org.json.JSONObject;

import com.bigdata.bop.Var;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class for creating queries and instantiating asset triples
 */
public class AssetKGInterface {
    private RemoteStoreClient storeClientAsset, storeClientDevice, storeClientPurchDoc;
    private AssetExistenceChecker existenceChecker;
    private AssetRetriever assetRetriever;


    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public AssetKGInterface(String kgEndpointAsset, String kgEndpointDevice, String kgEndpointPurchDoc) {
        storeClientAsset = new RemoteStoreClient(kgEndpointAsset, kgEndpointAsset);
        storeClientDevice = new RemoteStoreClient(kgEndpointDevice, kgEndpointDevice);
        storeClientPurchDoc = new RemoteStoreClient(kgEndpointPurchDoc, kgEndpointPurchDoc);
    
        existenceChecker =  new AssetExistenceChecker (storeClientAsset, storeClientDevice, storeClientPurchDoc);
        assetRetriever =  new AssetRetriever (storeClientAsset, storeClientDevice, storeClientPurchDoc);
    }

    /**
     * =============================================================================================================================================================
     * INSTANTIATE
     * =============================================================================================================================================================
     * Instantiate asset based on data from given request
     */
    public void instantiate (JSONObject AssetDataRaw) throws Exception{
        //Get IRI from ID
        String deviceIRIString = existenceChecker.getIRIStringbyID(AssetDataRaw.getString("ID"));
        if(deviceIRIString != null){
            throw new Exception("Instance already exist for id: " + AssetDataRaw.getString("ID") + 
                ". Please use /update instead for updating data."
            );
        }

        JSONObject AssetData = new JSONObject();
        //Create IRIs
        //Create Device IRI
        //TODO create safety check here -- class must exist in the given ontology
        String devicePrefix = getPrefixStringFromName(AssetDataRaw.getString("Prefix"));
        deviceIRIString = genIRIString(AssetDataRaw.getString("AssetClass"), devicePrefix);
        String itemIRI = genIRIString("Item", P_ASSET);
        String deviceTypeIRI = devicePrefix+AssetDataRaw.getString("AssetClass");
        
        AssetData.put("deviceIRI", deviceIRIString);
        AssetData.put("deviceTypeIRI", deviceTypeIRI);
        AssetData.put("ID", AssetDataRaw.getString("ID"));
        AssetData.put("label", AssetDataRaw.getString("Name").replaceAll("(\\r|\\n)", " ").replace("\\", "\\\\"));
        AssetData.put("itemIRI", itemIRI);
        AssetData.put("itemComment", AssetDataRaw.getString("PurchaseOrderNum"));
        AssetData.put("ServiceCategoryIRI", genIRIString("ServiceCategory", P_ASSET));
        AssetData.put("ServiceCategoryName", AssetDataRaw.getString("BudgetCat"));
        AssetData.put("ServiceCategoryType", AssetDataRaw.getString("ServiceCode"));


        //Persons IRI
        String assigneeName = AssetDataRaw.getString("AssignedTo");
        String workspaceName = AssetDataRaw.getString("WorkspaceName");
        String personIRI = "";
        String personNameIRI = "";
        String workspaceIRI = "";

        if(!(assigneeName.isBlank() || assigneeName==null)){
            LOGGER.info("Handling Person IRI:" + assigneeName);
            JSONObject PersonIRIs = existenceChecker.getPersonTriples(assigneeName, true);
            personIRI = PersonIRIs.getString("PersonIRI");
            personNameIRI = PersonIRIs.getString("PersonNameIRI");
            LOGGER.debug("PersonName:" + assigneeName);
            LOGGER.debug("personIRI:" + personIRI);
            LOGGER.debug("personNameIRI:" + personNameIRI);
        }

        if(!workspaceName.isBlank()){
            LOGGER.info("Handling Workspace IRI:" + workspaceName);
            workspaceIRI = existenceChecker.getWorkspaceIRIStringByName(workspaceName, true);
            LOGGER.debug("workspaceIRI:" + workspaceIRI);
        }
        AssetData.put("assignedTo", assigneeName);
        AssetData.put("personIRI", personIRI);
        AssetData.put("personNameIRI", personNameIRI);
        AssetData.put("workspaceName", workspaceName);
        AssetData.put("workspaceIRI", workspaceIRI);

        //Manual and Datasheets
        //SpecSheet
        String SpecSheetIRI = "";
        String SpecSheetFile = AssetDataRaw.getString("SpecSheet");
        String SpecSheetPage = AssetDataRaw.getString("SpecSheetPage");
        if (SpecSheetFile != null && !SpecSheetFile.isBlank()) {
            SpecSheetIRI = genIRIString("SpecSheet", P_ASSET);
        }
        if (SpecSheetPage == null || SpecSheetPage.isBlank()) {
            SpecSheetPage = "";
        }
        AssetData.put("SpecSheetIRI", SpecSheetIRI);
        AssetData.put("SpecSheetPage", SpecSheetPage);
        AssetData.put("SpecSheet", SpecSheetFile);
        //Manual
        String ManualFile = AssetDataRaw.getString("Manual");
        String ManualURL = AssetDataRaw.getString("ManualURL");
        String ManualIRI = "";
        if(ManualFile == null || ManualFile.isBlank()){
            ManualFile = "";
        }
        else{
            ManualIRI = genIRIString("Manual", P_ASSET);
        }
        if (ManualURL == null || ManualURL.isBlank()) {ManualURL = "";}
        AssetData.put("Manual", ManualFile);
        AssetData.put("ManualIRI", ManualIRI);
        AssetData.put("manualURL", ManualURL);

        //Supplier and Manuf
        String SupplierName = AssetDataRaw.getString("SupplierName");
        String ManufacturerName = AssetDataRaw.getString("ManufacturerName");
        String SupplierNameIRI = "";
        String SupplierOrgIRI = "";
        String ManufacturerNameIRI = "";
        String ManufacturerOrgIRI = "";
        LOGGER.info("Handling IRI for supplier and manuf: " + SupplierName +" & " + ManufacturerName +" .");
        if (!(SupplierName.isBlank() || SupplierName == null )){
            JSONObject orgIRI = existenceChecker.getOrganizationTriples(SupplierName, true);
            SupplierNameIRI = orgIRI.getString("OrgNameIRI");
            SupplierOrgIRI = orgIRI.getString("OrgIRI");
            LOGGER.debug("SupplierInfo:");
            LOGGER.debug("SupplierName:" + SupplierName);
            LOGGER.debug("SupplierNameIRI" + SupplierNameIRI);
            LOGGER.debug("SupplierOrgIRI" + SupplierOrgIRI);
        }
        //To ensure if the supplier and manufacturer are the same
        //AND the organization does not appear before this asset instance
        //the instance is not generated twice
        if (SupplierName.equals(ManufacturerName)){
            LOGGER.debug("Manuf and supplier are the same Org. Using same IRIs for both.");
            ManufacturerNameIRI = SupplierNameIRI;
            ManufacturerOrgIRI = SupplierOrgIRI;
        }
        else{
            if(!(ManufacturerName.isBlank() || ManufacturerName == null )) {
                JSONObject orgIRI = existenceChecker.getOrganizationTriples(ManufacturerName, true);
                ManufacturerNameIRI = orgIRI.getString("OrgNameIRI");
                ManufacturerOrgIRI = orgIRI.getString("OrgIRI");
                LOGGER.debug("ManufInfo:");
                LOGGER.debug("ManufName:" + ManufacturerName);
                LOGGER.debug("ManufNameIRI" + ManufacturerNameIRI);
                LOGGER.debug("ManufOrgIRI" + ManufacturerOrgIRI);
            }
        }


        AssetData.put("SupplierName", SupplierName);
        AssetData.put("SupplierNameIRI", SupplierNameIRI);
        AssetData.put("SupplierOrgIRI", SupplierOrgIRI);

        AssetData.put("ManufacturerName", ManufacturerName);
        AssetData.put("ManufacturerNameIRI", ManufacturerNameIRI);
        AssetData.put("ManufacturerOrgIRI", ManufacturerOrgIRI);

        //serial and model number
        String SerialNum = AssetDataRaw.getString("serialNum");
        String ModelNum = AssetDataRaw.getString("modelNumber").replace("\\", "\\\\");
        if(SerialNum == null){
            SerialNum = "";
        }
        if(ModelNum == null){
            ModelNum = "";
        }
        AssetData.put("serialNum", SerialNum);
        AssetData.put("modelNumber", ModelNum);

        //storage
        String storageName = AssetDataRaw.getString("storage");
        AssetData.put("storageID", storageName);
        if (storageName.isBlank() || storageName == null) {
            AssetData.put("cabinetIRI", "");
            AssetData.put("storageIRI", "");
            AssetData.put("cabinetTypeIRI", "");
        }
        else{
            if (storageName.toLowerCase().contains("cabinet") || storageName.toLowerCase().contains("cupboard") || storageName.contains("MS") || storageName.contains("BS") || storageName.contains("LABC")){
                //TODO handle cabinet type
                String cabinetIRI = "";
                JSONObject reqResult = existenceChecker.queryStorageFurnitureIRIbyName(storageName);
                if (reqResult == null) {
                    cabinetIRI = genIRIString("Cabinet", P_ASSET);
                }
                else{
                    cabinetIRI = reqResult.getString("cabinetIRI");
                }
                AssetData.put("cabinetIRI", cabinetIRI);
                AssetData.put("storageIRI", "");

                if (storageName.contains("MS")) {
                    AssetData.put("cabinetTypeIRI", PedestalCabinetString);
                }
                else{
                    if (storageName.contains("BS")) {
                        AssetData.put("cabinetTypeIRI", BuiltInCabinetString);
                    }
                    else{
                        AssetData.put("cabinetTypeIRI", CabinetString);
                    }
                }
            }
            else{
                //Assumed to be other assets or fumehoods
                AssetData.put("storageIRI", existenceChecker.queryStorageIRIbyID(storageName).getString("storageIRI"));
                AssetData.put("cabinetIRI", "");
                AssetData.put("cabinetTypeIRI", "");
            }
        }

        //Location and rooms
        //Workspace IRI is handled at "Person"
        String location = AssetDataRaw.getString("BuildingLocation");
        String facility = AssetDataRaw.getString("FacilityLocation");
        String room = AssetDataRaw.getString("RoomLocation");
        String roomIRI = "";
        String facilityIRI = "";
        String locationIRI = location;
        JSONObject locationIRIs = existenceChecker.getLocationTriples (location, facility, room);
        if (locationIRIs != null){
            locationIRI = locationIRIs.getString("locationIRI");
            facilityIRI = locationIRIs.getString("facilityIRI");
            roomIRI = locationIRIs.getString("RoomIRI");
        }
        AssetData.put("locationIRI", locationIRI);
        AssetData.put("facilityIRI", facilityIRI);
        AssetData.put("RoomIRI", roomIRI);
        AssetData.put("Location", location);

        //Purchase docs
        //Item and service code are handled above together with device
        //Assumed that a new document line is created when called and no asset has the same document line
        String invoiceNum = AssetDataRaw.getString("invoiceNum");
        String PONum = AssetDataRaw.getString("PurchaseOrderNum");
        String DONum = AssetDataRaw.getString("DeliveryOrderNum");

        JSONObject reqResultDocs = existenceChecker.getPurchaseDocsTriples(invoiceNum, PONum, DONum);
        //Invoice
        AssetData.put("InvoiceNum", invoiceNum);
        String invoiceIRI = reqResultDocs.getJSONObject("invoice").getString("InvoiceIRI");
        if(!invoiceIRI.isBlank()){AssetData.put("InvoiceIRI",invoiceIRI);}
        else{AssetData.put("InvoiceIRI",genIRIString("Invoice", P_ASSET));}
        AssetData.put("InvoiceLineIRI",genIRIString("InvoiceLine", P_ASSET));
        
        //PO
        AssetData.put("PONum", PONum);
        String POIRI = reqResultDocs.getJSONObject("PO").getString("InvoiceIRI");
        if(!POIRI.isBlank()){AssetData.put("PurchaseOrderIRI",POIRI);}
        else{AssetData.put("PurchaseOrderIRI",genIRIString("PurchaseOrder", P_ASSET));}
        AssetData.put("PurchaseOrderLineIRI",genIRIString("PurchaseOrderLineIRI", P_ASSET));

        //DO
        AssetData.put("DONum", DONum);
        String DOIRI = reqResultDocs.getJSONObject("DO").getString("InvoiceIRI");
        if(!DOIRI.isBlank()){AssetData.put("DeliveryOrderIRI",DOIRI);}
        else{AssetData.put("DeliveryOrderIRI",genIRIString("DeliveryOrder", P_ASSET));}
        AssetData.put("DeliveryOrderLineIRI",genIRIString("DeliveryOrderLineIRI", P_ASSET));

        //handle pricing
        JSONObject reqResPricing = null;
        
        String amtMoneyIRI = "";
        String PriceDetailsIRI = "";
        String priceIRI = "";
        String MeasureIRI = "";
        String currencyIRI = "";
        String price = AssetDataRaw.getString("price");
        //generate pricing instance IRI
        //TODO handle different currencies, currently default to SGD
        if(price != null && !price.isBlank()){
            MeasureIRI = genIRIString("Measure", P_ASSET);
            PriceDetailsIRI = genIRIString("PriceDetails", P_ASSET);
            priceIRI = genIRIString("Price", P_ASSET);
            //Handle currency here later
            //currencyIRI = SingaporeDollar.getQueryString(); #-- Generate saref:SingaporeDollar instead??
            currencyIRI = SingaporeDollarString;
            //Handle device namepsace pricing
            amtMoneyIRI = genIRIString("AmountOfMoney", P_DEV);
        }



        AssetData.put("PriceDetailsIRI", PriceDetailsIRI);
        AssetData.put("priceIRI", priceIRI);
        AssetData.put("priceMeasureIRI", MeasureIRI);
        AssetData.put("price", price);
        AssetData.put("currencyIRI", currencyIRI);
        AssetData.put("amtMoney", amtMoneyIRI);

        LOGGER.info(AssetData);
        createInstance(AssetData);
    }


    /*
     * Create new instances
     */
    private void createInstance(JSONObject assetData) {
        createAssetNameSpace(assetData);
        createDeviceNameSpace(assetData);
        createPurchaseDocNamespace(assetData);
    }

    private void createAssetNameSpace (JSONObject data){
        //Asset namespace query
        ModifyQuery  query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        //Device
        Iri deviceIRIVar = iri(data.getString("deviceIRI"));
        Iri deviceTypeIRI = iri(data.getString("deviceTypeIRI"));
        String ID = data.getString("ID");
        Iri itemIRIVar = iri(data.getString("itemIRI"));
        String labelLiteralVar = data.getString("label");
        //Owner and human
        Iri PersonIRI = iri(data.getString("personIRI"));
        Iri personNameIRI = iri(data.getString("personNameIRI"));
        String deviceOwnerLiteral = data.getString("assignedTo");
        //Workspace
        Iri WorkspaceOwnerIRI = iri(data.getString("personIRI"));
        String WorkspaceOwnerIRIString = data.getString("personIRI");
        Iri WorkspaceIRI = iri(data.getString("workspaceIRI"));
        String WorkspaceIDLiteral = data.getString("workspaceName");
        //Serial and model number
        String serialNumberLiteral = data.getString("serialNum");
        String modelNumber = data.getString("modelNumber");
        //price and money
        Iri amountOfMoneyVar = iri(data.getString("amtMoney"));
        Iri priceMeasureIRI = iri(data.getString("priceMeasureIRI"));
        String priceLiteral = data.getString("price");
        Iri priceCurrencyIRI = iri(data.getString("currencyIRI"));
        //Spec sheets and manual
        String manualURL = data.getString("manualURL");
        Iri SpecSheetIRI = iri(data.getString("SpecSheetIRI")); 
        String SpecSheetFileLiteral = data.getString("SpecSheet"); 
        String SpecSheetPageLiteral = data.getString("SpecSheetPage");

        Iri ManualIRI = iri(data.getString("ManualIRI")); 
        String ManualFileLiteral = data.getString("Manual"); 


        //Supplier and manuf
        //Iri supplierIRIVar = iri(data.getString("suppliedBy"));
        Iri SupplierOrgIRI = iri(data.getString("SupplierOrgIRI"));
        Iri SupplierNameIRI = iri(data.getString("SupplierNameIRI"));
        String SupplierNameLiteral = data.getString("SupplierName");
        Iri ManufacturerOrgIRI = iri(data.getString("ManufacturerOrgIRI"));
        Iri ManufacturerNameIRI = iri(data.getString("ManufacturerNameIRI"));
        String ManufacturerNameLiteral = data.getString("ManufacturerName");

        //Projects
        Iri assignedProjectIRI = iri(data.getString("assignedProject"));


        /*
         * INSTANTIATE QUERY
         */
        query.insert(deviceIRIVar.isA(deviceTypeIRI));
        query.insert(itemIRIVar.isA(Item));
        query.insert(itemIRIVar.has(allocatedTo, assignedProjectIRI));

        //Device
        query.insert(deviceIRIVar.has(hasItemInventoryIdentifier, Rdf.literalOf(ID)));
        //get Item IRI from device IRI from asset namespace
        query.insert(itemIRIVar.has(references, deviceIRIVar));
        //Device name from asset list
        query.insert(deviceIRIVar.has(RDFS.LABEL, Rdf.literalOf(labelLiteralVar)));
        //Device owner from asset list
        query.insert(PersonIRI.isA(Person));
        query.insert(personNameIRI.isA(PersonName));
        query.insert(deviceIRIVar.has(assignedTo, PersonIRI));
        query.insert(PersonIRI.has(hasName, personNameIRI));
        query.insert(personNameIRI.has(hasPersonName, Rdf.literalOf(deviceOwnerLiteral)));
        //Optional IRIs
        //Workspace
        if(!WorkspaceIDLiteral.isBlank() && !WorkspaceOwnerIRIString.isBlank()){
            query.insert(WorkspaceIRI.isA(Workspace));
            query.insert(WorkspaceOwnerIRI.has(hasAllocatedWorkspace, WorkspaceIRI));
            query.insert(WorkspaceIRI.has(hasWorkspaceIdentifier, WorkspaceIDLiteral));
        }


        //Serial number
        if (!serialNumberLiteral.isBlank()){
            query.insert(deviceIRIVar.has(serialNumber, Rdf.literalOf(serialNumberLiteral)));
        }
        //model number
        if (!modelNumber.isBlank()){
            query.insert(deviceIRIVar.has(hasModel, modelNumber));
        }
        //manual URL
        if (!manualURL.isBlank()){
            query.insert(deviceIRIVar.has(RDFS.SEEALSO, Rdf.literalOf(manualURL)));
        }
        //Price
        if(!priceLiteral.isBlank()){
            query.insert(amountOfMoneyVar.isA(AmountOfMoney));
            query.insert(priceMeasureIRI.isA(Measure));
            query.insert(deviceIRIVar.has(hasPrice, amountOfMoneyVar));
            query.insert(amountOfMoneyVar.has(hasValue, priceMeasureIRI));
            query.insert(priceMeasureIRI.has(hasNumericalValue, Rdf.literalOf(priceLiteral)));
            query.insert(priceMeasureIRI.has(hasUnit, priceCurrencyIRI));
        }
        
        //Datasheet
        if(!SpecSheetFileLiteral.isBlank()){
            query.insert(SpecSheetIRI.isA(SpecSheet));
            query.insert(deviceIRIVar.has(hasDataSheet, SpecSheetIRI));
            query.insert(SpecSheetIRI.has(availableAt, SpecSheetFileLiteral));
            if (!SpecSheetPageLiteral.isBlank()){
                query.insert(SpecSheetIRI.has(RDFS.COMMENT, Rdf.literalOf(SpecSheetPageLiteral)));
            }
        }
        //Manual
        if(!ManualFileLiteral.isBlank()){
            query.insert(ManualIRI.isA(Manual));
            query.insert(deviceIRIVar.has(hasDataSheet, ManualIRI));
            query.insert(ManualIRI.has(availableAt, ManualFileLiteral));
        }

        //Supplier & manufacturer
        if (!ManufacturerNameLiteral.isBlank()){
            query.insert(ManufacturerOrgIRI.isA(FormalOrganization));
            query.insert(ManufacturerNameIRI.isA(OrganizationName));
            query.insert(deviceIRIVar.has(isManufacturedBy, ManufacturerOrgIRI));
            query.insert(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
            query.insert(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral));
        }
        if(!SupplierNameLiteral.isBlank()){
            query.insert(SupplierOrgIRI.isA(FormalOrganization));
            query.insert(SupplierNameIRI.isA(OrganizationName));
            query.insert(deviceIRIVar.has(isSuppliedBy, SupplierOrgIRI));
            query.insert(SupplierOrgIRI.has(hasName, SupplierNameIRI));
            query.insert(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));
        }

        storeClientAsset.executeUpdate(query.getQueryString());
    }

    private void createDeviceNameSpace (JSONObject data){
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Iri deviceIRI = iri(data.getString("deviceIRI"));
        Iri deviceTypeIRI = iri(data.getString("deviceTypeIRI"));
        Iri roomIRI = iri(data.getString("RoomIRI"));
        Iri WorkspaceIRI = iri(data.getString("workspaceIRI"));
        String WorkspaceIDLiteral = data.getString("workspaceName");
        String LocationString = data.getString("Location");
        //Storage
        String storageIRIString = data.getString("storageIRI");
        //String furnitureIRIString = data.getString("furnitureIRI");
        String cabinetIRIString = data.getString("cabinetIRI");
        String cabinetTypeIRIString =data.getString("cabinetTypeIRI");
        String storageIDLiteral = data.getString("storageID");
        
        //Query
        //get device type
        query.insert(deviceIRI.isA(deviceTypeIRI));
        //Stored in -- Assumes the storage IRI exist somewhere
        if(!storageIRIString.isBlank()){
            query.insert(deviceIRI.has(isStoredIn, iri(storageIRIString)));
        }

        if(!cabinetIRIString.isBlank()){
            query.insert(iri(cabinetIRIString).isA(iri(cabinetTypeIRIString)));
            query.insert(deviceIRI.has(isStoredIn, iri(cabinetIRIString)));
            query.insert(iri(cabinetIRIString).has(hasFurnitureIdentifier, storageIDLiteral));
        }
        //get location
        if (LocationString.equals( "Research Wing") || LocationString.equals("CREATE Tower")){
            if(roomIRI != null){
                query.insert(roomIRI.has(containsElement, deviceIRI));

                //Workspace
                if(!WorkspaceIDLiteral.isBlank()){
                    query.insert(WorkspaceIRI.isA(Workspace));
                    query.insert(deviceIRI.has(isLocatedAt, WorkspaceIRI));
                    query.insert(roomIRI.has(containsElement, deviceIRI));
                    query.insert(WorkspaceIRI.has(isLocatedIn, roomIRI));
                    query.insert(WorkspaceIRI.has(hasWorkspaceIdentifier, WorkspaceIDLiteral));
                }
            }
        }
        else{
            query.insert(deviceIRI.has(hasCurrentLocation, Rdf.literalOf(LocationString)));
        }
        

        storeClientDevice.executeUpdate(query.getQueryString());
    }

    private void createPurchaseDocNamespace (JSONObject data){
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Iri itemIRI = iri(data.getString("itemIRI"));
        String itemNameLiteral = data.getString("label");
        String itemCommentLiteral = data.getString("itemComment");
        Iri ServiceCategoryIRI = iri(data.getString("ServiceCategoryIRI"));
        String ServiceCategoryNameLiteral = data.getString("ServiceCategoryName");
        String ServiceCategoryTypeLiteral = data.getString("ServiceCategoryType");

        //invoice instances
        Iri InvoiceIRI = iri(data.getString("InvoiceIRI"));
        String InvoiceNumLiteral = data.getString("InvoiceNum");
        Iri invoiceLineIRI = iri(data.getString("InvoiceLineIRI"));
        Iri DeliveryOrderIRI = iri(data.getString("DeliveryOrderIRI"));
        Iri PurchaseOrderIRI = iri(data.getString("PurchaseOrderIRI"));
        Iri DeliveryOrderLineIRI = iri(data.getString("DeliveryOrderLineIRI"));
        String DeliveryOrderNumLiteral = data.getString("DONum");
        Iri PurchaseOrderLineIRI = iri(data.getString("PurchaseOrderLineIRI"));
        String PurchaseOrderNumLiteral = data.getString("PONum");

        //price instances
        Iri priceDetailsIRI = iri(data.getString("PriceDetailsIRI"));
        Iri priceIRI = iri(data.getString("priceIRI"));
        Iri priceMeasureIRI = iri(data.getString("priceMeasureIRI"));
        String priceLiteral = data.getString("price");
        Iri priceCurrencyIRI = iri(data.getString("currencyIRI"));
        //supplier and manuf
        Iri SupplierOrgIRI = iri(data.getString("SupplierOrgIRI"));
        Iri SupplierNameIRI = iri(data.getString("SupplierNameIRI"));
        String SupplierNameLiteral = data.getString("SupplierName");
        Iri ManufacturerOrgIRI = iri(data.getString("ManufacturerOrgIRI"));
        Iri ManufacturerNameIRI = iri(data.getString("ManufacturerNameIRI"));
        String ManufacturerNameLiteral = data.getString("ManufacturerName");

        //QUERY
        //item data
        query.insert(itemIRI.isA(Item));
        query.insert(ServiceCategoryIRI.isA(ServiceCategory));
        query.insert(itemIRI.has(itemName, Rdf.literalOf(itemNameLiteral)));
        query.insert(itemIRI.has(RDFS.COMMENT, Rdf.literalOf(itemCommentLiteral)));
        query.insert(itemIRI.has(hasAttribute, ServiceCategoryIRI));

        //OPTIONAL QUERIES

        //Invoice, DO and PO
        //Invoice
        if(!InvoiceNumLiteral.isBlank()){
            query.insert(InvoiceIRI.isA(E_Invoice));
            query.insert(invoiceLineIRI.isA(InvoiceLine));
            query.insert(invoiceLineIRI.has(hasItem, itemIRI));
            query.insert(InvoiceIRI.has(hasInvoiceLine, invoiceLineIRI));
            query.insert(InvoiceIRI.has(invoiceNumber, InvoiceNumLiteral));
            query.insert(invoiceLineIRI.has(hasItem, itemIRI));

        }

        //DO
        if(!DeliveryOrderNumLiteral.isBlank()){
            query.insert(DeliveryOrderIRI.isA(DeliveryOrder));
            query.insert(DeliveryOrderLineIRI.isA(DeliveryOrderLine));
            query.insert(DeliveryOrderLineIRI.has(hasItem, itemIRI));
            query.insert(DeliveryOrderIRI.has(hasDeliveryOrderLine, DeliveryOrderLineIRI));
            query.insert(DeliveryOrderIRI.has(deliveryOrderNumber, DeliveryOrderNumLiteral));

        }

        //PO
        if(!PurchaseOrderNumLiteral.isBlank()){
            query.insert(PurchaseOrderIRI.isA(PurchaseOrder));
            query.insert(PurchaseOrderLineIRI.isA(PurchaseOrderLine));
            query.insert(PurchaseOrderLineIRI.has(hasItem, itemIRI));
            query.insert(PurchaseOrderIRI.has(hasPurchaseOrderLine, PurchaseOrderLineIRI));
            query.insert(PurchaseOrderIRI.has(purchaseOrderNumber, PurchaseOrderNumLiteral));

        }
        
        //Price
        if(!priceLiteral.isBlank()){
            query.insert(priceDetailsIRI.isA(PriceDetails));
            query.insert(priceIRI.isA(HomeTotalDiscountedAfterTaxPrice));
            query.insert(priceMeasureIRI.isA(Measure));
            query.insert(invoiceLineIRI.has(hasPriceDetails, priceDetailsIRI));
            query.insert(priceDetailsIRI.has(hasPrice, priceIRI));
            query.insert(priceIRI.has(hasValue, priceMeasureIRI));
            query.insert(priceMeasureIRI.has(hasNumericalValue, Rdf.literalOf(priceLiteral)));
            query.insert(priceMeasureIRI.has(hasUnit, priceCurrencyIRI));
            if(!InvoiceNumLiteral.isBlank()){query.insert(invoiceLineIRI.has(hasPriceDetails, priceDetailsIRI));}
            if(!DeliveryOrderNumLiteral.isBlank()){query.insert(DeliveryOrderLineIRI.has(hasPriceDetails, priceDetailsIRI));}
            if(!PurchaseOrderNumLiteral.isBlank()){query.insert(PurchaseOrderLineIRI.has(hasPriceDetails, priceDetailsIRI));}
        }

        //Supplier & manufacturer
        if (!ManufacturerNameLiteral.isBlank()){
            query.insert(ManufacturerOrgIRI.isA(FormalOrganization));
            query.insert(ManufacturerNameIRI.isA(OrganizationName));
            query.insert(itemIRI.has(isManufacturedBy, ManufacturerOrgIRI));
            query.insert(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
            query.insert(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral));
        }
        if(!SupplierNameLiteral.isBlank()){
            query.insert(SupplierOrgIRI.isA(FormalOrganization));
            query.insert(SupplierNameIRI.isA(OrganizationName));
            query.insert(itemIRI.has(isSuppliedBy, SupplierOrgIRI));
            query.insert(SupplierOrgIRI.has(hasName, SupplierNameIRI));
            query.insert(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));
        }

        //Projects and service codes

        query.insert(ServiceCategoryIRI.has(attributeName, ServiceCategoryTypeLiteral));
        query.insert(ServiceCategoryIRI.has(attributeValue, ServiceCategoryNameLiteral));

        storeClientPurchDoc.executeUpdate(query.getQueryString());
    }
    /*
     * =============================================================================================================================================================
     */

     /*
      * Retrieve asset info based on ID
      */

      public JSONObject retrieve(String ID) {
        return assetRetriever.retrieve(ID);
      }

      public JSONObject getRequiredIriUI () {
        return assetRetriever.getRequiredIriUI();
      }

      public JSONArray getItemListByDocIRI (String InvoiceIRI, String POiri, String DOiri) {
        return assetRetriever.getItemListByDocIRI(InvoiceIRI, POiri, DOiri);
      }

}