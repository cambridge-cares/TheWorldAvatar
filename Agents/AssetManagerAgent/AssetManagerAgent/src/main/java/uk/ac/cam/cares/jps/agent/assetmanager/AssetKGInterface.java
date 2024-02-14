package uk.ac.cam.cares.jps.agent.assetmanager;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.eclipse.rdf4j.sparqlbuilder.util.SparqlBuilderUtils;
import org.jooq.InsertQuery;

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
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.json.JSONArray;
import org.json.JSONObject;

import java.rmi.Remote;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import com.bigdata.bop.Var;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class for creating queries and instantiating asset triples
 */
public class AssetKGInterface {
    private String endpointAsset, endpointOffice, endpointPurchDoc, endpointLab;
    private RemoteStoreClient storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab;
    public AssetExistenceChecker existenceChecker;
    public AssetRetriever assetRetriever;
    public AssetDelete assetDeleter;

    private String user = null;
    private String pass = null;


    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public AssetKGInterface(String kgEndpointAsset, String kgEndpointOffice, String kgEndpointPurchDoc, String kgEndpointLab) {
        endpointAsset = kgEndpointAsset;
        endpointOffice = kgEndpointOffice;
        endpointPurchDoc = kgEndpointPurchDoc;
        endpointLab = kgEndpointLab;

        storeClientAsset = new RemoteStoreClient(kgEndpointAsset, kgEndpointAsset);
        storeClientOffice = new RemoteStoreClient(kgEndpointOffice, kgEndpointOffice);
        storeClientPurchDoc = new RemoteStoreClient(kgEndpointPurchDoc, kgEndpointPurchDoc);
        storeClientLab = new RemoteStoreClient(kgEndpointLab, kgEndpointLab);
            
        existenceChecker =  new AssetExistenceChecker (storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab);
        assetRetriever =  new AssetRetriever (storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab, endpointAsset, endpointOffice, endpointPurchDoc, endpointLab);
        assetDeleter = new AssetDelete(storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab, "./deleteLog.log");
    }

    public AssetKGInterface(String kgEndpointAsset, String kgEndpointOffice, String kgEndpointPurchDoc, String kgEndpointLab, String username, String password) {
        endpointAsset = kgEndpointAsset;
        endpointOffice = kgEndpointOffice;
        endpointPurchDoc = kgEndpointPurchDoc;
        endpointLab = kgEndpointLab;

        storeClientAsset = new RemoteStoreClient(kgEndpointAsset, kgEndpointAsset);
        storeClientOffice = new RemoteStoreClient(kgEndpointOffice, kgEndpointOffice);
        storeClientPurchDoc = new RemoteStoreClient(kgEndpointPurchDoc, kgEndpointPurchDoc);
        storeClientLab = new RemoteStoreClient(kgEndpointLab, kgEndpointLab);

        storeClientAsset.setUser(username);
        storeClientAsset.setPassword(password);
        storeClientOffice.setUser(username);
        storeClientOffice.setPassword(password);
        storeClientPurchDoc.setUser(username);
        storeClientPurchDoc.setPassword(password);
        storeClientLab.setUser(username);
        storeClientLab.setPassword(password);

        user = username;
        pass = password;
            
        existenceChecker =  new AssetExistenceChecker (storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab);
        assetRetriever =  new AssetRetriever (storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab, endpointAsset, endpointOffice, endpointPurchDoc, endpointLab);
        assetDeleter = new AssetDelete(storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab, "./deleteLog.log");
    }

    /**
     * =============================================================================================================================================================
     * INSTANTIATE
     * =============================================================================================================================================================
     * Instantiate asset based on data from given request
     */

    public JSONObject setInstantiate(JSONObject setDataRaw) throws Exception{
        JSONArray resArr = new JSONArray();
        String desiredID = setDataRaw.getString("desiredID");
        String deliveryDate = setDataRaw.getString("deliveryDate");
        if (desiredID.isBlank() || desiredID == null){
            
            String date;
            DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            if (deliveryDate.isBlank() || deliveryDate == null){
                LocalDateTime now = LocalDateTime.now();
                date = dtf.format(now);
            }
            else{
                try {
                    deliveryDate = dtf.format(LocalDate.parse(deliveryDate, dtf));
                } catch (Exception e) {
                    // TODO: handle exception
                    throw new JPSRuntimeException("Failed to parse deliveryDate", e);
                }
                date = deliveryDate;
            }
            String idNum = String.valueOf(getLatestIDNum() +  1);
            desiredID = date +"/"+ idNum;
        }
        JSONArray assetSet = setDataRaw.getJSONArray("setData");
        for (int i=0;i<assetSet.length();i++){
            JSONObject assetData = assetSet.getJSONObject(i);
            assetData.put("ID", desiredID + "." + String.valueOf(i+1));
            resArr.put(instantiate(assetData));
        }

        return new JSONObject().put("InstanceResults", resArr);
    }


    public JSONObject instantiate (JSONObject AssetDataRaw) throws Exception{
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
        //String devicePrefix = getPrefixStringFromName(AssetDataRaw.getString("Prefix"));
        String devicePrefix = AssetDataRaw.getString("Prefix");
        //deviceIRIString = genIRIString(AssetDataRaw.getString("AssetClass"), devicePrefix);
        deviceIRIString = genIRIString("Device", P_DEV);
        String itemIRI = genIRIString("Item", P_ASSET);
        //String deviceTypeIRI = devicePrefix+AssetDataRaw.getString("AssetClass");
        String deviceTypeIRI = devicePrefix;
        //String deviceTypeIRI = DeviceString;

        AssetData.put("deviceIRI", deviceIRIString);
        AssetData.put("deviceTypeIRI", deviceTypeIRI);
        String id = AssetDataRaw.getString("ID");
        String deliveryDate = AssetDataRaw.getString("deliveryDate");
        if (id.isBlank()){
            String date;
            DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd");
            if (deliveryDate.isBlank() || deliveryDate == null){
                LocalDateTime now = LocalDateTime.now();
                date = dtf.format(now);
            }
            else{
                try {
                    deliveryDate = dtf.format(LocalDate.parse(deliveryDate, dtf));
                } catch (Exception e) {
                    // TODO: handle exception
                    throw new JPSRuntimeException("Failed to parse deliveryDate", e);
                }
                date = deliveryDate;
            }
            

            String idNum = String.valueOf(getLatestIDNum() +  1);
            id = date +"/"+ idNum;
        }
        AssetData.put("Prefix", devicePrefix);
        AssetData.put("ID", id);
        AssetData.put("label", AssetDataRaw.getString("Name").replaceAll("(\\r|\\n)", " ").replace("\\", "\\\\"));
        AssetData.put("itemIRI", itemIRI);
        AssetData.put("itemComment", AssetDataRaw.getString("ItemComment"));
        //AssetData.put("ServiceCategoryIRI", genIRIString("ServiceCategory", P_ASSET));
        JSONObject budgetTriples = existenceChecker.getBudgetTriples(AssetDataRaw.getString("BudgetCat"), AssetDataRaw.getString("ServiceCode"));
        AssetData.put("ServiceCategoryIRI", budgetTriples.getString("serviceCategoryIRI"));
        AssetData.put("budgetCategoryIRI", budgetTriples.getString("budgetCategoryIRI"));
        AssetData.put("ServiceCodeIRI", budgetTriples.getString("serviceCodeIRI"));
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

        //Location and rooms
        //Workspace IRI is handled at "Person"
        String location = AssetDataRaw.getString("BuildingLocation");
        String facility = AssetDataRaw.getString("FacilityLocation");
        String room = AssetDataRaw.getString("RoomLocation");
        String roomIRI = "";
        String facilityIRI = "";
        String locationIRI = "";

        RemoteStoreClient preferredClient = storeClientOffice;

        if(location.contains("Research Wing")){
            preferredClient = storeClientLab;
            LOGGER.info("Switching to Lab store client...");
        }

        JSONObject locationIRIs = existenceChecker.getLocationTriples (location, facility, room, preferredClient);
        if (locationIRIs != null){
            locationIRI = locationIRIs.getString("locationIRI");
            facilityIRI = locationIRIs.getString("facilityIRI");
            roomIRI = locationIRIs.getString("RoomIRI");
        }
        AssetData.put("locationIRI", locationIRI);
        AssetData.put("facilityIRI", facilityIRI);
        AssetData.put("RoomIRI", roomIRI);
        AssetData.put("Location", location);

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
                JSONObject reqResult = existenceChecker.queryStorageFurnitureIRIbyName(storageName, preferredClient);
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
                AssetData.put("storageIRI", existenceChecker.queryStorageIRIbyID(storageName, preferredClient).getString("storageIRI"));
                AssetData.put("cabinetIRI", "");
                AssetData.put("cabinetTypeIRI", "");
            }
        }


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
        if(price != null && !price.isBlank()){
            MeasureIRI = genIRIString("Measure", P_ASSET);
            PriceDetailsIRI = genIRIString("PriceDetails", P_ASSET);
            priceIRI = genIRIString("Price", P_ASSET);
            //Handle device namepsace pricing
            amtMoneyIRI = genIRIString("AmountOfMoney", P_DEV);
            //Handle currency here later
            //currencyIRI = SingaporeDollarString;
            if (AssetDataRaw.has("currency")){
                String currency = AssetDataRaw.getString("currency").toUpperCase();
                LOGGER.debug("Currency received:: " + currency);
                switch (currency) {
                    case "SGD":
                        currencyIRI = SingaporeDollarString;
                        break;
                    case "GBP":
                        currencyIRI = GreatBritishPoundSterlingString;
                        break;
                    case "EUR":
                        currencyIRI = EuroString;
                        break;
                    case "JPY":
                        currencyIRI = JapaneseYenString;
                        break;
                    case "CNY":
                        currencyIRI = ChineseYuanString;
                        break;
                    case "USD":
                        currencyIRI = UnitedStatesDollarString;
                        break;
                    default:
                        throw new JPSRuntimeException("Currency unrecognized. currently supported currencies are: "+
                            "SGD, GBP, EUR, JPY, CNY, USD"
                        );
                }
            }
            else{
                currencyIRI = SingaporeDollarString;
            }
        }

        AssetData.put("PriceDetailsIRI", PriceDetailsIRI);
        AssetData.put("priceIRI", priceIRI);
        AssetData.put("priceMeasureIRI", MeasureIRI);
        AssetData.put("price", price);
        AssetData.put("currencyIRI", currencyIRI);
        AssetData.put("amtMoney", amtMoneyIRI);

        LOGGER.info(AssetData);
        createInstance(AssetData, preferredClient);
                
        JSONObject idPair = new JSONObject();
        idPair.put("ID", AssetData.getString("ID"));
        idPair.put("deviceIRI", AssetData.getString("deviceIRI"));
        return idPair;
    }

    /*
     * Get the latest ID number and increment by 1
     */
    private int getLatestIDNum() {
        int result;
        /*
         * TODO Write in proper rdf4j format as any change in the triple will require manual change the way it is
         * No, this is not written on a Friday evening... Its Thursday evening... and I'm on leave on Friday...
         * ~MTL 
         */

        String query=
        "PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>\n"+
        "select (STRAFTER(str(?idValue), '/')as ?idNum)\n"+
        "WHERE {\n"+
        "?id <"+P_ASSET+"hasItemInventoryIdentifier"+"> ?idValue .}\n"+
        "ORDER BY DESC(xsd:integer(?idNum))";
        JSONArray reqRes = storeClientAsset.executeQuery(query);
        if (reqRes.length() == 0) {
            return -1; //Added 1 above so the first ID will be 0
        }
        result = Integer.parseInt(reqRes.getJSONObject(0).getString("idNum"));

        return result;
    }


    /*
     * Create new instances
     */
    private void createInstance(JSONObject assetData, RemoteStoreClient preferredClient) {
        createAssetNameSpace(assetData);
        createDeviceNameSpace(assetData, preferredClient);
        createPurchaseDocNamespace(assetData);
    }

    private void createAssetNameSpace (JSONObject data){
        //Asset namespace query
        ModifyQuery  query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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


        /*
         * INSTANTIATE QUERY
         */
        query.insert(deviceIRIVar.isA(Device));
        query.insert(deviceIRIVar.has(isCategorizedUnder, deviceTypeIRI));
        query.insert(deviceTypeIRI.isA(UserDefinedCatergory));
        query.insert(itemIRIVar.isA(Item));

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
            query.insert(ManufacturerNameIRI.has(RDFS.LABEL, Rdf.literalOf(ManufacturerNameLiteral)));
        }
        if(!SupplierNameLiteral.isBlank()){
            query.insert(SupplierOrgIRI.isA(FormalOrganization));
            query.insert(SupplierNameIRI.isA(OrganizationName));
            query.insert(deviceIRIVar.has(isSuppliedBy, SupplierOrgIRI));
            query.insert(SupplierOrgIRI.has(hasName, SupplierNameIRI));
            query.insert(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));
            query.insert(SupplierNameIRI.has(RDFS.LABEL, Rdf.literalOf(SupplierNameLiteral)));
        }

        storeClientAsset.executeUpdate(query.getQueryString());
    }

    private void createDeviceNameSpace (JSONObject data, RemoteStoreClient preferredClient){
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        String devicePrefix = data.getString("Prefix");

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
        query.insert(deviceIRI.isA(Device));
        query.insert(deviceIRI.has(isCategorizedUnder, deviceTypeIRI));
        query.insert(deviceTypeIRI.isA(UserDefinedCatergory));
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
        if (LocationString.contains( "Research Wing") || LocationString.contains("CREATE Tower")){
            if(roomIRI != null){
                //TODO This is dead logic as the prefix is now different
                //Now all items are devices and systems are not used, so the logic is still valid in the sense taht it defaults to the `else` clause anyways
                //Currently debating if it should still be kept for backward comaptibility or should it be deleted as the prev version is not valid now anyways
                if (devicePrefix.equals(P_SYS)){
                    query.insert(roomIRI.has(containsSystem, deviceIRI));
                }
                else{
                    query.insert(roomIRI.has(containsElement, deviceIRI));
                }
                

                //Workspace
                if(!WorkspaceIDLiteral.isBlank()){
                    query.insert(WorkspaceIRI.isA(Workspace));
                    query.insert(deviceIRI.has(isLocatedAt, WorkspaceIRI));
                    query.insert(WorkspaceIRI.has(isLocatedIn, roomIRI));
                    query.insert(WorkspaceIRI.has(hasWorkspaceIdentifier, WorkspaceIDLiteral));
                }
            }
        }
        else{
            query.insert(deviceIRI.has(hasCurrentLocation, Rdf.literalOf(LocationString)));
        }
        

        preferredClient.executeUpdate(query.getQueryString());
    }

    private void createPurchaseDocNamespace (JSONObject data){
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Iri itemIRI = iri(data.getString("itemIRI"));
        String itemNameLiteral = data.getString("label");
        String itemCommentLiteral = data.getString("itemComment");
        Iri ServiceCategoryIRI = iri(data.getString("ServiceCategoryIRI"));
        String ServiceCategoryNameLiteral = data.getString("ServiceCategoryName");
        String ServiceCategoryTypeLiteral = data.getString("ServiceCategoryType");
        Iri BudgetCategoryIRI = iri(data.getString("budgetCategoryIRI"));
        Iri ServiceCodeIRI = iri(data.getString("ServiceCodeIRI"));

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
        query.insert(itemIRI.has(purchasedUnder, BudgetCategoryIRI));
        query.insert(BudgetCategoryIRI.has(hasServiceCategory, ServiceCategoryIRI));
        query.insert(BudgetCategoryIRI.has(hasServiceCode, ServiceCodeIRI));
        query.insert(ServiceCategoryIRI.has(hasServiceCategoryIdentifier, ServiceCategoryNameLiteral));
        query.insert(ServiceCodeIRI.has(hasServiceCodeIdentifier, ServiceCategoryTypeLiteral));

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
            query.insert(ManufacturerNameIRI.has(RDFS.LABEL, Rdf.literalOf(ManufacturerNameLiteral)));
        }
        if(!SupplierNameLiteral.isBlank()){
            query.insert(SupplierOrgIRI.isA(FormalOrganization));
            query.insert(SupplierNameIRI.isA(OrganizationName));
            query.insert(itemIRI.has(isSuppliedBy, SupplierOrgIRI));
            query.insert(SupplierOrgIRI.has(hasName, SupplierNameIRI));
            query.insert(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));
            query.insert(SupplierNameIRI.has(RDFS.LABEL, Rdf.literalOf(SupplierNameLiteral)));
        }

        //Projects and service codes

        //query.insert(ServiceCategoryIRI.has(attributeName, ServiceCategoryTypeLiteral));
        //query.insert(ServiceCategoryIRI.has(attributeValue, ServiceCategoryNameLiteral));

        storeClientPurchDoc.executeUpdate(query.getQueryString());
    }

    /*
     * Maintenance Data
     */
    public void addMaintenanceData(JSONObject maintenanceData){
        String ID = maintenanceData.getString("ID");

        //Validation
        String deviceIRI = existenceChecker.getIRIStringbyID(ID);
        if (deviceIRI == null || deviceIRI.isBlank()){
            throw new JPSRuntimeException(String.format("Device is unregistered for ID:%s", ID));
        }
        JSONObject maintenanceIRI = existenceChecker.getMaintenanceIRI(deviceIRI, true);

        addMaintenanceData(maintenanceIRI, maintenanceData);

    }

    //TODO Fix this mesy method. This one exist so the update can be done more smoothly when the IRI exist, but it still is a mess
    public void addMaintenanceData(JSONObject maintenanceIRI, JSONObject maintenanceData){
        String ID = maintenanceData.getString("ID");
        String lastService = maintenanceData.getString("LastService");
        String nextService = maintenanceData.getString("NextService");
        String interval = maintenanceData.getString("Interval");
        String serviceProvider = maintenanceData.getString("ServiceProvider");
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        LocalDate lastServiceDate = null;
        LocalDate nextServiceDate = null;
        int year, month;

        String lastServiceIRI = "";
        String nextServiceIRI = "";
        String intervalIRI = "";
        String durationIRI = "";
        String serviceProviderIRI = "";
        Iri serviceProviderTypeIRI;

        //Validation
        String deviceIRI = existenceChecker.getIRIStringbyID(ID);
        if (deviceIRI == null || deviceIRI.isBlank()){
            throw new JPSRuntimeException(String.format("Device is unregistered for ID:%s", ID));
        }
        //JSONObject maintenanceIRI = existenceChecker.getMaintenanceIRI(deviceIRI, true);

        String maintenanceScheduleIRI = maintenanceIRI.getString("maintenanceScheduleIRI");
        String maintenanceTaskIRI = maintenanceIRI.getString("maintenanceTaskIRI");
        
        //preprocessing
        //TODO Figure out what to do with multiple maintenance schedule. Currently multiple is allowed
        //TODO When figured out, also change in ExistenceChecker
        if(!(lastService.isBlank() || lastService==null)){
            lastServiceIRI = maintenanceIRI.getString("lastServiceIRI");
            try {
                lastServiceDate = LocalDate.parse(lastService, dtf);
            } catch (DateTimeParseException e) {
                throw new JPSRuntimeException("Failed to parse service times, ensure the format is correct: yyyy-mm-dd", e);
            }
            
            if (lastServiceDate.isAfter(LocalDate.now())){
                throw new JPSRuntimeException("Last service date cannot be in the future. Please use next service date instead");
            }

        }

        JSONObject existingServiceIRIs = existenceChecker.getPersonTriples(serviceProvider, false);
        if (existingServiceIRIs == null) {
            existingServiceIRIs = existenceChecker.getOrganizationTriples(serviceProvider);
            if (existingServiceIRIs == null){
                existingServiceIRIs = existenceChecker.getIndependentPartyTriples(serviceProvider, true);
                //serviceProviderIRI = genIRIString("ServiceProvider", P_ASSET);
                serviceProviderIRI = existingServiceIRIs.getString("ServiceProviderIRI");
                serviceProviderTypeIRI = IndependentParty;
            }
            else{
                serviceProviderIRI = existingServiceIRIs.getString("OrgIRI");
                serviceProviderTypeIRI = FormalOrganization;
            }
        }
        else{
            serviceProviderIRI = existingServiceIRIs.getString("PersonIRI");
            serviceProviderTypeIRI = Person;
        }

        // Transport data in months?
        if(!(interval.isBlank() || interval==null)){
            intervalIRI = maintenanceIRI.getString("intervalIRI");
            durationIRI = maintenanceIRI.getString("durationIRI");
            if((nextService.isBlank() || nextService==null) && lastServiceDate!=null){
                nextServiceDate = lastServiceDate.plusMonths(Long.valueOf(interval));
                nextService = dtf.format(nextServiceDate);
            }
        }

        if(!(nextService.isBlank() || nextService==null)){
            nextServiceIRI = maintenanceIRI.getString("nextServiceIRI");
            try {
                nextServiceDate = LocalDate.parse(nextService, dtf);
            } catch (DateTimeParseException e) {
                throw new JPSRuntimeException("Failed to parse service times, ensure the format is correct: yyyy-mm-dd", e);
            }
        }

        if(nextServiceDate!= null && lastServiceDate!= null){
            if(nextServiceDate.isBefore(lastServiceDate)){
                throw new JPSRuntimeException("Next service date is before last service date. We don't allow time travel agencies when servicing assets.");
            }
        }

        //instantiate
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, 
            Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE,
            Pref_TIME
        );

        query.insert(iri(deviceIRI).has(hasMaintenanceSchedule, iri(maintenanceScheduleIRI)));
        query.insert(iri(maintenanceScheduleIRI).has(hasTask, iri(maintenanceTaskIRI)));
        query.insert(iri(maintenanceScheduleIRI).isA(MaintenanceSchedule));
        query.insert(iri(maintenanceTaskIRI).isA(MaintenanceTask));
        if(lastServiceDate!= null) {
            query.insert(iri(maintenanceTaskIRI).has(performedAt, iri(lastServiceIRI)));
            query.insert(iri(lastServiceIRI).isA(Instant));
            query.insert(iri(lastServiceIRI).has(inXSDDateTimeStamp, Rdf.literalOfType(lastService, XSD.DATE)));
        }
        if(nextServiceDate!= null) {
            query.insert(iri(maintenanceTaskIRI).has(scheduledFor, iri(nextServiceIRI)));
            query.insert(iri(nextServiceIRI).isA(Instant));
            query.insert(iri(nextServiceIRI).has(inXSDDateTimeStamp, Rdf.literalOfType(nextService, XSD.DATE)));
        }

        query.insert(iri(maintenanceTaskIRI).has(isPerformedBy, iri(serviceProviderIRI)));
        
        if (serviceProviderTypeIRI == FormalOrganization){
            Iri serviceProviderNameIRI = iri(existingServiceIRIs.getString("OrgNameIRI"));
            query.insert(iri(serviceProviderIRI).isA(FormalOrganization));
            query.insert(serviceProviderNameIRI.isA(OrganizationName));
            query.insert(iri(serviceProviderIRI).has(hasName, serviceProviderNameIRI));
            query.insert(serviceProviderNameIRI.has(hasLegalName, serviceProvider));
            query.insert(serviceProviderNameIRI.has(RDFS.LABEL, Rdf.literalOf(serviceProvider)));
        }
        else if (serviceProviderTypeIRI == Person){
            Iri serviceProviderNameIRI = iri(existingServiceIRIs.getString("PersonNameIRI"));
            query.insert(iri(serviceProviderIRI).isA(Person));
            query.insert(serviceProviderNameIRI.isA(PersonName));
            query.insert(iri(serviceProviderIRI).has(hasName, serviceProviderNameIRI));
            query.insert(serviceProviderNameIRI.has(hasPersonName, Rdf.literalOf(serviceProvider)));
        }
        else{
            query.insert(iri(serviceProviderIRI).isA(serviceProviderTypeIRI));
            query.insert(iri(serviceProviderIRI).has(RDFS.LABEL, Rdf.literalOf(serviceProvider)));
        }
        

        if(!(interval.isBlank() || interval==null)){
            year = Integer.valueOf(interval)/12;
            month = Integer.valueOf(interval)%12;
            query.insert(iri(maintenanceTaskIRI).has(hasInterval, iri(intervalIRI)));
            query.insert(iri(intervalIRI).has(hasDurationDescription, iri(durationIRI)));
            query.insert(iri(durationIRI).has(months, Rdf.literalOf(month)).andHas(years, Rdf.literalOf(year)));
            query.insert(iri(intervalIRI).isA(Interval));
            query.insert(iri(durationIRI).isA(DurationDescription));
        }
        
        storeClientAsset.executeUpdate(query.getQueryString());

    }

    public JSONArray updateMaintenanceTimeData(){
        JSONArray maintenanceList = assetRetriever.getAllMaintenanceScheduleIRI();
        DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate callTime = LocalDate.now();
        JSONArray result = new JSONArray();
        for (int i = 0; i < maintenanceList.length(); i ++){
            JSONObject maintenanceData = maintenanceList.getJSONObject(i);
            assetDeleter.deleteMaintenanceData(maintenanceList.getJSONObject(i));
            if (maintenanceData.has("nextServiceIRI")){
                LocalDate nextService = LocalDate.parse(maintenanceData.getString("nextServiceTime"));
                if (callTime.isAfter(nextService)){

                    maintenanceData.put("lastServiceTime", dtf.format(nextService));
                    if (!maintenanceData.has("lastServiceIRI")){
                        maintenanceData.put("lastServiceIRI", genIRIString("ServiceTime", P_TIME));
                    }

                    if (maintenanceData.has("intervalIRI")){
                        Long totalIntervalMonth = Long.valueOf(maintenanceData.getString("durationYear")) * 12 + 
                                                    Long.valueOf(maintenanceData.getString("durationMonth"));
                        maintenanceData.put("nextServiceTime", dtf.format(nextService.plusMonths(totalIntervalMonth)));
                    }
                }
            }

            LOGGER.info("New maintenance time for maintenance schedule::" + maintenanceData);
            result.put(maintenanceData);
            //updateGeneralMaintenanceData(maintenanceData, maintenanceList.getJSONObject(i));
            addMaintenanceData(maintenanceData, fitMaintenanceIRItoMaintenanceData(maintenanceData));
        }
        return result;
    }

    //TODO sort this method out as it seems unnneccesarry
    //This method is the result of *trying* to write competent code after 2 weeks break and it produces a messy mess
    //This method is the ice pack you use the morning after banging your head on a drunken night's hangover
    //Need to fix addMaintenance before anything really...
    private JSONObject fitMaintenanceIRItoMaintenanceData (JSONObject maintenanceIRI) {
        JSONObject maintenanceData = new JSONObject();
        Long totalIntervalMonth = Long.valueOf(maintenanceIRI.getString("durationYear")) * 12 + 
                                                    Long.valueOf(maintenanceIRI.getString("durationMonth"));
        maintenanceData.put("ID", existenceChecker.getIDbyIRIString(maintenanceIRI.getString("deviceIRI")));
        maintenanceData.put("LastService", maintenanceIRI.getString("lastServiceTime"));
        maintenanceData.put("NextService", maintenanceIRI.getString("nextServiceTime"));
        maintenanceData.put("Interval", Long.toString(totalIntervalMonth));
        maintenanceData.put("ServiceProvider", maintenanceIRI.getString("performerName"));
        return maintenanceData;
    }

    public void deleteMaintenance(String maintenanceScheduleIRI){
        JSONObject maintenanceData = assetRetriever.getAssetMaintenanceIRI(maintenanceScheduleIRI);
        if (maintenanceData == null){
            throw new JPSRuntimeException("Maintenance schedule data does not exist for IRI" + maintenanceScheduleIRI);
        }
        maintenanceData.put("ID", existenceChecker.getIDbyIRIString(maintenanceData.getString("deviceIRI")));
        assetDeleter.deleteMaintenanceData(maintenanceData);
    }

    public void addDataSheet(String docFilename, String docType, String docComment, String deviceID) {
        String deviceIRI = existenceChecker.getIRIStringbyID(deviceID);
        String documentIRI = "";

        documentIRI = existenceChecker.getDataSheetIRI(docFilename, docType, true).getString("DocIRI");
        
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.insert(iri(documentIRI).isA(iri(docType)));
        query.insert(iri(deviceIRI).has(hasDataSheet, iri(documentIRI)));
        query.insert(iri(documentIRI).has(availableAt, docFilename));
        if (!docComment.isBlank()){
            query.insert(iri(documentIRI).has(RDFS.COMMENT, Rdf.literalOf(docComment)));
        }
        storeClientAsset.executeUpdate(query.getQueryString());
    }

    public void addAssetImage(String docFileURL, String deviceID) {
        String deviceIRI = existenceChecker.getIRIStringbyID(deviceID);
        
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.insert(iri(deviceIRI).has(hasImageURI, docFileURL));
        storeClientAsset.executeUpdate(query.getQueryString());
    }

    /*TODO is this one neccesarry? Should the document exist before the asset? This opens to possiblity that the asset has more than 1 of the same purch doc type which doesn make sense
    Currently it is considered to exist before hand and if it doesn't throw an error, as the feature is only for adding file to the docs, not adding one to asset.
    */
    public void addPurchaseDocFile(String documentIRI, String docFilename, String docTypeIRI, String docComment) {
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        //TODO proper purchase docs instantiation
        query.insert(iri(documentIRI).isA(iri(docTypeIRI)));
        query.insert(iri(documentIRI).has(availableAt, docFilename));
        storeClientAsset.executeUpdate(query.getQueryString());
    }

    public String[] getPurchaseDocumentIRI (String docNum, String docType){
        String documentIRI = "";
        String documentType = "";
        String documentTypeIRI = "";
        if (docType.contains("Invoice")){
            documentType = "Invoice";
            documentTypeIRI = E_Invoice.getQueryString();
        } else if (docType.contains("PurchaseOrder")){
            documentType = "PO";
            documentTypeIRI = PurchaseOrder.getQueryString();
        } else if (docType.contains("DeliveryOrder")){
            documentType = "DO";
            documentTypeIRI = DeliveryOrder.getQueryString();
        }
        else{
            throw new JPSRuntimeException("Invalid DocType! Only accepts: Invoice, PurchaseOrder and DeliveryOrder.", null);
        }
        documentTypeIRI = documentTypeIRI.substring(1, documentTypeIRI.length()-1);
        documentIRI = existenceChecker.getSpecificPurchaseDocsTriples(docNum, documentType).getString("InvoiceIRI");
        if (documentIRI.isBlank()){
            throw new JPSRuntimeException("Purchase document is not recorded."+
            " Please ensure the purchase document exist before adding file.", null);
        }

        return new String[]{documentIRI, documentTypeIRI};
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
        return assetRetriever.getRequiredIriUI(endpointAsset, endpointOffice);
      }

      public JSONArray getItemListByDocIRI (String InvoiceIRI, String POiri, String DOiri) {
        return assetRetriever.getItemListByDocIRI(InvoiceIRI, POiri, DOiri);
      }

      public Boolean itemMeasuresBool(String dbName, String IRI, JSONArray pred, int searchDepth){
        RemoteStoreClient dbStoreClient = new RemoteStoreClient(dbName, dbName);

        if (user != null && pass != null){
            storeClientAsset.setUser(user);
            storeClientAsset.setPassword(pass);
        }
        return assetRetriever.getMeasuresExistence (dbStoreClient, IRI, pred, searchDepth);
      }

    /*
     * =============================================================================================================================================================
     */

     /*
      * Delete asset info based on ID
      */

      public void delete(String ID){
        JSONObject retrievedData = assetRetriever.retrieve(ID);
        retrievedData.put("ID", ID);
        LOGGER.info("DELTEING::"+ID +"::" + retrievedData);
        assetDeleter.deleteByAssetData(retrievedData);
      }

}