package uk.ac.cam.cares.jps.agent.assetmanager;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.jooq.False;
import org.jooq.UpdateQuery;

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
import org.eclipse.rdf4j.query.algebra.Str;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.bigdata.bop.Var;
import com.github.jsonldjava.core.RDFDataset.Literal;
import com.martiansoftware.jsap.Switch;

import java.rmi.Remote;
import java.util.*;

import org.apache.commons.digester.substitution.VariableAttributes;
import org.apache.jena.atlas.json.JSON;
import org.apache.jena.sparql.function.library.print;
import org.apache.jena.sparql.pfunction.library.versionARQ;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;


/**
 * Class for creating queries and instantiating asset triples
 */
public class AssetKGInterface {
    private RemoteStoreClient storeClientAsset, storeClientDevice, storeClientPurchDoc;

    // prefix
	private static final String ONTODEV = "https://www.theworldavatar.com/kg/ontodevice/";
    private static final String ONTOLAB = "https://www.theworldavatar.com/kg/ontolab/";
    private static final String ONTOSYSTEM = "https://www.theworldavatar.com/kg/ontosystem/";
    private static final String ONTOINMA = "https://www.theworldavatar.com/kg/ontoinma/";
    private static final String ONTOEPE = "https://www.theworldavatar.com/kg/ontoelecpowerequipment/";
    private static final String ONTOASSET = "https://www.theworldavatar.com/kg/ontoassetmanagement/";
    private static final String ONTOBIM = "https://www.theworldavatar.com/kg/ontobim/";
    
    private static final Prefix Pref_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix Pref_LAB = SparqlBuilder.prefix("ontolab",iri(ONTOLAB));
    private static final Prefix Pref_SYS = SparqlBuilder.prefix("ontosystem",iri(ONTOSYSTEM));
    private static final Prefix Pref_INMA = SparqlBuilder.prefix("ontoinma",iri(ONTOINMA));
    private static final Prefix Pref_ASSET = SparqlBuilder.prefix("ontoassetmanagement",iri(ONTOASSET));
    private static final Prefix Pref_EPE = SparqlBuilder.prefix("ontoelecpowerequipment",iri(ONTOEPE));
    private static final Prefix Pref_BIM = SparqlBuilder.prefix("ontobim", iri(ONTOBIM));
    private static final Prefix Pref_SAREF = SparqlBuilder.prefix("saref", iri("https://saref.etsi.org/core/"));
    private static final Prefix Pref_OM = SparqlBuilder.prefix("saref", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    private static final Prefix Pref_FIBO_AAP = SparqlBuilder.prefix("FIBOaap", iri("https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/"));
    private static final Prefix Pref_FIBO_ORG = SparqlBuilder.prefix("FIBOorg",iri("https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/"));
    private static final Prefix Pref_BOT = SparqlBuilder.prefix("bot", iri("https://w3id.org/bot#"));
    private static final Prefix Pref_P2P_ITEM = SparqlBuilder.prefix("P2Pitem", iri("https://purl.org/p2p-o/item#")); 
    private static final Prefix Pref_P2P_DOCLINE = SparqlBuilder.prefix("P2Pdocline", iri("https://purl.org/p2p-o/documentline#")); 
    private static final Prefix Pref_P2P_INVOICE = SparqlBuilder.prefix("P2Pinvoice", iri("https://purl.org/p2p-o/invoice#")); 

    private static final String P_DEV = ONTODEV;
    private static final String P_LAB = ONTOLAB;
    private static final String P_SYS = ONTOSYSTEM;
    private static final String P_INMA = ONTOINMA;
    private static final String P_ASSET = ONTOASSET;
    private static final String P_EPE = ONTOEPE;
    private static final String P_BIM = ONTOBIM;
    private static final String P_SAREF = "https://saref.etsi.org/core/";
    private static final String P_OM = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    private static final String P_FIBO_AAP = "https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/People/";
    private static final String P_FIBO_ORG = "https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/";
    private static final String P_BOT = "https://w3id.org/bot#";
    private static final String P_P2P_ITEM ="https://purl.org/p2p-o/item#"; 
    private static final String P_P2P_DOCLINE = "https://purl.org/p2p-o/documentline#"; 
    private static final String P_P2P_INVOICE = "https://purl.org/p2p-o/invoice#"; 

    //properties
    private static final Iri consistsOf = Pref_SAREF.iri("consistsOf");
    private static final Iri hasModel = Pref_SAREF.iri("hasModel");

    private static final Iri hasItemInventoryIdentifier = Pref_ASSET.iri("hasItemInventoryIdentifier");
    private static final Iri references = Pref_ASSET.iri("references");
    private static final Iri assignedTo = Pref_ASSET.iri("assignedTo");
    private static final Iri serialNumber = Pref_ASSET.iri("serialNumber");
    private static final Iri isStoredIn = Pref_ASSET.iri("isStoredIn");
    private static final Iri availableAt = Pref_ASSET.iri("availableAt");
    private static final Iri isSuppliedBy = Pref_ASSET.iri("isSuppliedBy");
    private static final Iri isManufacturedBy = Pref_ASSET.iri("isManufacturedBy");
    private static final Iri isLocatedIn = Pref_ASSET.iri("isLocatedIn");
    private static final Iri isLocatedAt = Pref_ASSET.iri("isLocatedAt");
    private static final Iri hasAllocatedWorkspace = Pref_ASSET.iri("hasAllocatedWorkspace");
    private static final Iri hasIdentifier = Pref_ASSET.iri("hasIdentifier");
    private static final Iri hasDeliveryOrderLine = Pref_ASSET.iri("hasDeliveryOrderLine");
    private static final Iri hasPurchaseOrderLine = Pref_ASSET.iri("hasPurchaseOrderLine");
    private static final Iri deliveryOrderNumber = Pref_ASSET.iri("deliveryOrderNumber");
    private static final Iri purchaseOrderNumber = Pref_ASSET.iri("purchaseOrderNumber");
    private static final Iri hasPriceDetails = Pref_ASSET.iri("hasPriceDetails");
    private static final Iri hasWorkspaceIdentifier = Pref_ASSET.iri("hasWorkspaceIdentifier");
    private static final Iri hasCurrentLocation = Pref_ASSET.iri("hasCurrentLocation");
    private static final Iri hasFurnitureIdentifier = Pref_ASSET.iri("hasFurnitureIdentifier");

    private static final Iri hasDataSheet = Pref_DEV.iri("hasDataSheet");
    private static final Iri hasPrice = Pref_DEV.iri("hasPrice");
    
    private static final Iri hasRoom = Pref_BIM.iri("hasRoom");
    private static final Iri hasIfcRepresentation = Pref_BIM.iri("hasIfcRepresentation");
    private static final Iri hasFacility = Pref_BIM.iri("hasFacility");

    private static final Iri hasValue = Pref_OM.iri("hasValue");
    private static final Iri hasUnit = Pref_OM.iri("hasUnit");
    private static final Iri hasNumericalValue = Pref_OM.iri("hasNumericalValue");

    private static final Iri itemName = Pref_P2P_ITEM.iri("itemName");
    private static final Iri hasAttribute = Pref_P2P_ITEM.iri("hasAttribute");
    private static final Iri attributeName = Pref_P2P_ITEM.iri("attributeName");
    private static final Iri attributeValue = Pref_P2P_ITEM.iri("attributeValue");
    private static final Iri invoiceNumber = Pref_P2P_INVOICE.iri("invoiceNumber");
    private static final Iri hasInvoiceLine = Pref_P2P_INVOICE.iri("hasInvoiceLine");
    private static final Iri hasItem = Pref_P2P_DOCLINE.iri("hasItem");
    private static final Iri InvoicedQuantity = Pref_P2P_DOCLINE.iri("InvoicedQuantity"); 

    private static final Iri hasPersonName = Pref_FIBO_AAP.iri("hasPersonName");
    private static final Iri hasLegalName = iri("https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/hasLegalName");

    private static final Iri hasName = iri("https://www.omg.org/spec/Commons/Designators/hasName");

    private static final Iri containsElement = Pref_BOT.iri("containsElement");

    //External classes
    private static final Iri Item = Pref_P2P_ITEM.iri("Item");
    
    private static final Iri SpecSheet = Pref_ASSET.iri("SpecSheet");
    private static final Iri Manual = Pref_ASSET.iri("Manual");
    private static final Iri Workspace = Pref_ASSET.iri("Workspace");
    private static final Iri ServiceCategory = Pref_ASSET.iri("ServiceCategory");
    private static final Iri DeliveryOrder = Pref_ASSET.iri("DeliveryOrder");
    private static final Iri DeliveryOrderLine = Pref_ASSET.iri("DeliveryOrderLine"); 
    private static final Iri PurchaseOrder = Pref_ASSET.iri("PurchaseOrder");
    private static final Iri PurchaseOrderLine = Pref_ASSET.iri("PurchaseOrderLine");
    private static final Iri E_Invoice = Pref_ASSET.iri("E-Invoice");
    private static final Iri InvoiceLine = Pref_ASSET.iri("InvoiceLine");
    private static final Iri PriceDetails = Pref_ASSET.iri("PriceDetails");
    private static final Iri HomeTotalDiscountedAfterTaxPrice = Pref_ASSET.iri("HomeTotalDiscountedAfterTaxPrice");

    private static final Iri Person = Pref_FIBO_AAP.iri("Person");
    private static final Iri PersonName = Pref_FIBO_AAP.iri("PersonName");
    private static final Iri FormalOrganization = Pref_FIBO_ORG.iri("FormalOrganization");
    private static final Iri OrganizationName = Pref_FIBO_ORG.iri("OrganizationName");

    private static final Iri AmountOfMoney = Pref_OM.iri("AmountOfMoney");
    private static final Iri Measure = Pref_OM.iri("Measure");
    private static final Iri SingaporeDollar = Pref_OM.iri("SingaporeDollar");
    private static final String SingaporeDollarString = P_OM + "SingaporeDollar";

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    //constructor
    public AssetKGInterface(String kgEndpointAsset, String kgEndpointDevice, String kgEndpointPurchDoc) {
        storeClientAsset = new RemoteStoreClient(kgEndpointAsset, kgEndpointAsset);
        storeClientDevice = new RemoteStoreClient(kgEndpointDevice, kgEndpointDevice);
        storeClientPurchDoc = new RemoteStoreClient(kgEndpointPurchDoc, kgEndpointPurchDoc);
    }

    /**
     * Instantiate asset based on data from given request
     */
    public void instantiate (JSONObject AssetDataRaw) throws Exception{
        //Get IRI from ID
        Iri deviceIRI = getIRIbyID(AssetDataRaw.getString("ID"), storeClientAsset);
        if(deviceIRI != null){
            throw new Exception("Instance already exist for id: " + AssetDataRaw.getString("ID") + 
                ". Please use /update instead for updating data."
            );
        }

        JSONObject AssetData = new JSONObject();
        //Create IRIs
        //Create Device IRI
        //TODO create safety check here -- class must exist in the given ontology
        String devicePrefix = getPrefixStringFromName(AssetDataRaw.getString("Prefix"));
        String deviceIRIString = genIRIString(AssetDataRaw.getString("AssetClass"), devicePrefix);
        String itemIRI = genIRIString("Item", P_ASSET);
        String deviceTypeIRI = genIRIString(AssetDataRaw.getString("AssetClass"), devicePrefix);
        
        AssetData.put("deviceIRI", deviceIRIString);
        AssetData.put("deviceTypeIRI", deviceTypeIRI);
        AssetData.put("ID", AssetDataRaw.getString("ID"));
        AssetData.put("label", AssetDataRaw.getString("Name").replaceAll("(\\r|\\n)", " "));
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

        if(assigneeName != null && !assigneeName.isBlank()){
            JSONObject PersonIRIs = getPersonTriples(assigneeName);
            personIRI = PersonIRIs.getString("PersonIRI");
            personNameIRI = PersonIRIs.getString("PersonNameIRI");

            if(workspaceName != null && !workspaceName.isBlank()){
                workspaceIRI = getWorkspaceIRIByName(workspaceName).getQueryString();
            }
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
        if (SupplierName != null && !SupplierName.isBlank()){
            JSONObject orgIRI = getOrganizationTriples(SupplierName);
            SupplierNameIRI = orgIRI.getString("OrgNameIRI");
            SupplierOrgIRI = orgIRI.getString("OrgIRI");
        }
        if(ManufacturerName != null && !ManufacturerName.isBlank()) {
            JSONObject orgIRI = getOrganizationTriples(ManufacturerName);
            ManufacturerNameIRI = orgIRI.getString("OrgNameIRI");
            ManufacturerOrgIRI = orgIRI.getString("OrgIRI");
        }

        AssetData.put("SupplierName", SupplierName);
        AssetData.put("SupplierNameIRI", SupplierNameIRI);
        AssetData.put("SupplierOrgIRI", SupplierOrgIRI);

        AssetData.put("ManufacturerName", ManufacturerName);
        AssetData.put("ManufacturerNameIRI", ManufacturerNameIRI);
        AssetData.put("ManufacturerOrgIRI", ManufacturerOrgIRI);

        //serial and model number
        String SerialNum = AssetDataRaw.getString("serialNum");
        String ModelNum = AssetDataRaw.getString("modelNumber");
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
        }
        else{
            if (storageName.toLowerCase().contains("cabinet") || storageName.toLowerCase().contains("cupboard") || storageName.contains("MS")){
                //TODO handle cabinet type
                String cabinetIRI = "";
                JSONObject reqResult = queryStorageFurnitureIRIbyName(storageName);
                if (reqResult == null) {
                    cabinetIRI = genIRIString("Cabinet", P_ASSET);
                }
                else{
                    cabinetIRI = reqResult.getString("cabinetIRI");
                }
                AssetData.put("cabinetIRI", cabinetIRI);
            }
            else{
                //Assumed to be other assets or fumehoods
                AssetData.put("storageIRI", queryStorageIRIbyID(storageName).getString("storageIRI"));
                
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
        JSONObject locationIRIs = getLocationTriples (location, facility, room);
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
        String invoiceNum = AssetDataRaw.getString("invoiceNum");
        String PONum = AssetDataRaw.getString("PurchaseOrderNum");
        String DONum = AssetDataRaw.getString("DeliveryOrderNum");

        JSONObject reqResultDocs = getPurchaseDocsTriples(invoiceNum, PONum, DONum);
        //Invoice
        AssetData.put("InvoiceNum", invoiceNum);
        String invoiceLineIRI = reqResultDocs.getJSONObject("invoice").getString("InvoiceLineIRI");
        String invoiceIRI = reqResultDocs.getJSONObject("invoice").getString("InvoiceIRI");
        if(!invoiceIRI.isBlank()){AssetData.put("InvoiceIRI",invoiceIRI);}
        else{AssetData.put("InvoiceIRI",genIRIString("Invoice", P_ASSET));}
        if(!invoiceLineIRI.isBlank()){AssetData.put("InvoiceLineIRI",invoiceLineIRI);}
        else{AssetData.put("InvoiceLineIRI",genIRIString("InvoiceLine", P_ASSET));}
        
        //PO
        AssetData.put("PONum", PONum);
        String POLineIRI = reqResultDocs.getJSONObject("PO").getString("InvoiceLineIRI");
        String POIRI = reqResultDocs.getJSONObject("PO").getString("InvoiceIRI");
        if(!POIRI.isBlank()){AssetData.put("PurchaseOrderIRI",POIRI);}
        else{AssetData.put("PurchaseOrderIRI",genIRIString("PurchaseOrder", P_ASSET));}
        if(!POLineIRI.isBlank()){AssetData.put("PurchaseOrderLineIRI",POLineIRI);}
        else{AssetData.put("PurchaseOrderLineIRI",genIRIString("PurchaseOrderLineIRI", P_ASSET));}

        //DO
        AssetData.put("DONum", DONum);
        String DOLineIRI = reqResultDocs.getJSONObject("DO").getString("InvoiceLineIRI");
        String DOIRI = reqResultDocs.getJSONObject("DO").getString("InvoiceIRI");
        if(!DOIRI.isBlank()){AssetData.put("DeliveryOrderIRI",DOIRI);}
        else{AssetData.put("DeliveryOrderIRI",genIRIString("DeliveryOrder", P_ASSET));}
        if(!DOLineIRI.isBlank()){AssetData.put("DeliveryOrderLineIRI",POLineIRI);}
        else{AssetData.put("DeliveryOrderLineIRI",genIRIString("DeliveryOrderLineIRI", P_ASSET));}

        //handle pricing
        JSONObject reqResPricing = null;
        if(invoiceIRI != ""){
            reqResPricing = queryPricingDetailsIRIbyInvoiceIRI(invoiceIRI);
        }else{
            if(POIRI != ""){
                reqResPricing = queryPricingDetailsIRIbyInvoiceIRI(POIRI);
            }else{
                if(DOIRI != ""){
                    reqResPricing = queryPricingDetailsIRIbyInvoiceIRI(DOIRI);
                }
            }
        }
        
        String amtMoneyIRI = "";
        String PriceDetailsIRI = "";
        String priceIRI = "";
        String MeasureIRI = "";
        String currencyIRI = "";
        String price = AssetDataRaw.getString("price");
        if (reqResPricing== null){
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

        }
        else {
            //use existing instance
            PriceDetailsIRI = reqResPricing.getString("PriceDetailsIRI");
            priceIRI = reqResPricing.getString("priceIRI");
            MeasureIRI = reqResPricing.getString("MeasureIRI");
            currencyIRI = reqResPricing.getString("currencyIRI");
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


    Prefix getPrefixFromString (String ontology) throws Exception {
        switch (ontology) {
            case "ontodevice":
                return Pref_DEV;

            case "ontolab":
                return Pref_LAB;

            case "ontosystem":
                return Pref_SYS;

            case "ontoinma":
                return Pref_INMA;
            
            case "ontoelecpowerequipment":
                return Pref_EPE;
            
            default:
                throw new Exception("Unrecognized ontology: " +  ontology, null);
        }
    }

    String getPrefixStringFromName(String ontology) throws Exception {
        switch (ontology) {
            case "ontodevice":
                return P_DEV;

            case "ontolab":
                return P_LAB;

            case "ontosystem":
                return P_SYS;

            case "ontoinma":
                return P_INMA;
            
            case "ontoelecpowerequipment":
                return P_EPE;
            
            default:
                throw new Exception("Unrecognized ontology: " +  ontology, null);
        }
    }

    private JSONObject getPersonTriples(String name){
        JSONObject result = new JSONObject();
        JSONArray reqResult = getIRIbyLiteral (name, hasPersonName, storeClientAsset);

        switch (reqResult.length()) {
            case 0:
                //Create IRI and add to result
                String PersonIRI = genIRIString("Person", P_ASSET);
                String PersonNameIRI = genIRIString("PersonName", P_ASSET);
                result.put("PersonIRI", PersonIRI);
                result.put("PersonNameIRI", PersonNameIRI);
                return result;
            case 1:
                //Add the existing IRI to result
                String personNameIRIString = reqResult.getJSONObject(0).getString("x0");
                result.put("PersonNameIRI", personNameIRIString);
                //Query Person instance from person name
                result.put("PersonIRI", getIRIbyIRIObject(iri(personNameIRIString), hasName, storeClientAsset));
                return result;
            default:
                throw new JPSRuntimeException("A person have more than 1 instance: " + name + ". Check the knowledge graph for duplicates.", null);
        }

    }

    private JSONObject getOrganizationTriples (String orgName) {
        JSONObject result = new JSONObject();
        JSONArray reqResult = getIRIbyLiteral (orgName, hasLegalName, storeClientAsset);
        String OrgNameIRI, OrgIRI;
        switch (reqResult.length()) {
            case 0:
                //Create IRI and add to result
                OrgNameIRI = genIRIString("OrganizationName", P_ASSET);
                OrgIRI = genIRIString("FormalOrganization", P_ASSET);
                result.put("OrgNameIRI", OrgNameIRI);
                result.put("OrgIRI", OrgIRI);
                return result;
            case 1:
                //Add the existing IRI to result
                OrgNameIRI = reqResult.getJSONObject(0).getString("x0");
                result.put("OrgNameIRI", OrgNameIRI);
                //Query Person instance from person name
                result.put("OrgIRI", getIRIbyIRIObject(iri(OrgNameIRI), hasName, storeClientAsset));
                return result;
            default:
                throw new JPSRuntimeException("An organization has more than 1 instance: " + orgName + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private Iri getIRIbyID (String ID, RemoteStoreClient storeClient){
        JSONArray reqResult = getIRIbyLiteral (ID, hasItemInventoryIdentifier, storeClient);

        switch (reqResult.length()) {
            case 0:
                return null;
            case 1:
                return iri(reqResult.getJSONObject(0).getString("x0"));
            default:
                throw new JPSRuntimeException("More than 1 asset instance have the same ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }

    }

    private Iri getWorkspaceIRIByName (String name) {
        JSONArray reqResult = getIRIbyLiteral(name, hasWorkspaceIdentifier, storeClientAsset);
        switch (reqResult.length()) {
            case 0:
                //Does not seem right to use Asest prefix here?
                return genIRI("Workspace", P_ASSET);
                
            case 1:
                return iri(reqResult.getJSONObject(0).getString("x0"));
            default:
                throw new JPSRuntimeException("Workspace has more than 1 instances: " + name + ". Check the knowledge graph for duplicates.", null);
        }
    }
    /*
    Query used is wrong, but might be useful for later, so delete once everything runs

    @Deprecated
    private JSONObject getLocationTriples (String roomIRI, String sectionIRI) {
        return getLocationTriples (iri(roomIRI), iri(sectionIRI));
    }
    @Deprecated
    private JSONObject getLocationTriples (Iri roomIRI, Iri sectionIRI) {
        JSONObject result = new JSONObject();
        
        //query if item is not in registered rooms (home, NTU, etc.)
        result = queryUnregisteredLocation();
        if(result == null){
            //query if item is in registered rooms (Research Wing and CREATE Tower)
            result = queryRegisteredLocation(roomIRI, sectionIRI); //The query is wrong for queryRegisteredLocation
            if (result == null) {
                //Instance does not exist yet, create new IRIs
                return null;
            }
            return result;
        }
        return result;
    }
    */

    private JSONObject getLocationTriples (String buildingName, String facilityName, String roomName){
        if (buildingName == "Research Wing" || buildingName == "CREATE Tower"){
            return  queryLocationIRIByName(buildingName, facilityName, roomName);
        }
        else{
            //Use String literal for the location as its not registered
            return null;
        }
    }

    //WHY IS THIS NECESSARRY? WHY DID I WRITE THIS???
    private JSONObject queryUnregisteredLocation (String location) {
        JSONObject result = new JSONObject();
        SelectQuery queryUnregistered = Queries.SELECT();
        Variable locationLiteral = SparqlBuilder.var("locationLiteral");

        queryUnregistered.where(queryUnregistered.var().has(hasCurrentLocation, locationLiteral));
        JSONArray reqResult = storeClientDevice.executeQuery(queryUnregistered.getQueryString());
        switch (reqResult.length()) {
            case 0:
                //Not of this location type
                return null;
                
            case 1:
                result.put("Location", reqResult.getJSONObject(0).getString("locationLiteral"));
                return result;

            default:
                throw new JPSRuntimeException("Multiple off-CREATE campus location detected for the same asset:" );
        }
    }

    @Deprecated
    private JSONObject queryRegisteredLocation (Iri roomIRI, Iri sectionIRI) {
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable roomtypeIRI = SparqlBuilder.var("RoomTypeIRI");
        Variable SectionTypeIRI = SparqlBuilder .var("SectionTypeIRI");
        Variable WorkspaceIRI = SparqlBuilder.var("WorkspaceIRI");
        Variable WorkspaceIDLiteral = SparqlBuilder.var("WorkspaceID");
        
        //get location
        query.where(roomIRI.has(containsElement, query.var()));
        query.where(roomIRI.has(RDF.TYPE, roomtypeIRI));
        query.where(sectionIRI.has(hasRoom, roomIRI));
        query.where(sectionIRI.isA(SectionTypeIRI));
        //Workspace
        query.where(GraphPatterns.optional(query.var().has(isLocatedAt, WorkspaceIRI)));
        query.where(GraphPatterns.optional(WorkspaceIRI.has(hasIdentifier, WorkspaceIDLiteral)));

        JSONArray reqResult = storeClientDevice.executeQuery(query.getQueryString());
        switch (reqResult.length()) {
            case 0:
                //Not of this location type
                return null;
                
            case 1:
                result.put("RoomIRI", reqResult.getJSONObject(0).getString("RoomIRI"));
                result.put("SectionIRI", reqResult.getJSONObject(0).getString("SectionIRI"));
                result.put("RoomTypeIRI", reqResult.getJSONObject(0).getString("RoomTypeIRI"));
                result.put("SectionTypeIRI", reqResult.getJSONObject(0).getString("SectionTypeIRI"));

                return result;
            default:
                throw new JPSRuntimeException("Location has more than 1 IRIs: " + sectionIRI + ":" + roomIRI + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private JSONObject queryLocationIRIByName (String buildingName, String facilityName, String roomName) {
        JSONObject result = new JSONObject();
        Variable roomIRI = SparqlBuilder.var("roomIRI");
        Variable roomTypeIRI = SparqlBuilder.var("roomTypeIRI");
        Variable IFCReprIRI = SparqlBuilder.var("IFCReprIRI");
        Variable facilityIRI = SparqlBuilder.var("facilityIRI");
        Variable facilityTypeIRI = SparqlBuilder.var ("facilityTypeIRI");
        Variable locationIRI = SparqlBuilder.var("locationIRI");
        
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(IFCReprIRI.has(RDFS.LABEL, Rdf.literalOf(roomName)));
        query.where(roomIRI.has(hasIfcRepresentation, IFCReprIRI));
        query.where(roomIRI.isA(roomTypeIRI));
        query.where(facilityIRI.has(hasRoom, roomIRI));
        query.where(facilityIRI.isA(facilityTypeIRI));
        if (facilityName != null){
            query.where(facilityIRI.has(RDFS.LABEL, Rdf.literalOf(facilityName)));
        }
        query.where(locationIRI.has(hasFacility, facilityIRI));
        query.where(locationIRI.has(RDFS.LABEL, Rdf.literalOf(buildingName)));

        JSONArray reqResult = storeClientDevice.executeQuery(query.getQueryString());
        switch (reqResult.length()) {
            case 0:
                //location does not exist.
                //Location presumed to exist as creating new locations is out of bound for the agent's responsibility
                //as it opens room for more error (duplicates, wrong naming etc.)
                throw new JPSRuntimeException("Specified location is not recorded in the knowledge graph. "
                    +"Ensure the room is instantiated."
                );
                
            case 1:
                result.put("RoomIRI", reqResult.getJSONObject(0).getString("roomIRI"));
                result.put("facilityIRI", reqResult.getJSONObject(0).getString("facilityIRI"));
                result.put("RoomTypeIRI", reqResult.getJSONObject(0).getString("roomTypeIRI"));
                result.put("facilityTypeIRI", reqResult.getJSONObject(0).getString("facilityTypeIRI"));
                result.put("locationIRI", reqResult.getJSONObject(0).getString("locationIRI"));

                return result;
            default:
                throw new JPSRuntimeException("Location has more than 1 IRI set: "+ buildingName+ ", "
                    + facilityName+ ", "+ roomName + ". Check the knowledge graph for duplicates."
                );
        }

    }

    private JSONObject queryWorkspaceIRIbyName (String workspaceName) {
        JSONObject result = new JSONObject();
        Variable workspaceIRI = SparqlBuilder.var("workspaceIRI");
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(workspaceIRI.has(RDFS.LABEL, Rdf.literalOf(workspaceName)));

        JSONArray reqResult = storeClientDevice.executeQuery(query.getQueryString());
        switch (reqResult.length()) {
            case 0:
                return null;
                
            case 1:
                result.put("workspaceIRI", reqResult.getJSONObject(0).getString("workspaceIRI"));

                return result;
            default:
                throw new JPSRuntimeException("Workspace has more than 1 IRI: " + workspaceName + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private JSONObject queryStorageIRIbyID(String ID) {
        //For Asset storing other assets or FH/WFH
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable storageIRI = SparqlBuilder.var("storageIRI");

        query.where(storageIRI.has(RDFS.LABEL, Rdf.literalOf(ID)));
        JSONArray reqResult = storeClientDevice.executeQuery(query.getQueryString());
        switch (reqResult.length()) {
            case 0:
                throw new JPSRuntimeException("Storage asset does not exist for storage: " + ID  +". Ensure the storage asset is instantiated first.");
                
            case 1:
                result.put("workspaceIRI", reqResult.getJSONObject(0).getString("workspaceIRI"));

                return result;
            default:
                throw new JPSRuntimeException("Storage has more than 1 IRI for ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }

    }

    private JSONObject queryStorageFurnitureIRIbyName(String name) {
        //FOr cabinets or other furnitures
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable cabinetIRI = SparqlBuilder.var("cabinetIRI");

        query.where(cabinetIRI.has(hasFurnitureIdentifier, Rdf.literalOf(name)));
        JSONArray reqResult = storeClientDevice.executeQuery(query.getQueryString());
        switch (reqResult.length()) {
            case 0:
                return null;
                
            case 1:
                result.put("cabinetIRI", reqResult.getJSONObject(0).getString("cabinetIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Storage has more than 1 IRI for ID: " + name + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private JSONObject queryPricingDetailsIRIbyInvoiceIRI (String invoiceIRI){
        return queryPricingDetailsIRIbyInvoiceIRI(iri(invoiceIRI));
    }

    private JSONObject queryPricingDetailsIRIbyInvoiceIRI (Iri invoiceIRI){
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable POLineIRI = SparqlBuilder.var("POLineIRI");
        Variable PriceDetailsIRI = SparqlBuilder.var("PriceDetailsIRI");
        Variable priceIRI = SparqlBuilder.var("priceIRI");
        Variable MeasureIRI = SparqlBuilder.var("MeasureIRI");
        Variable currencyIRI = SparqlBuilder.var("currencyIRI");
        Variable priceLiteral = SparqlBuilder.var("price");
        
        query.where(invoiceIRI.has(hasPurchaseOrderLine, POLineIRI));
        query.where(POLineIRI.has(hasPriceDetails, PriceDetailsIRI));
        query.where(PriceDetailsIRI.has(hasPrice, priceIRI));
        query.where(priceIRI.has(hasValue, MeasureIRI));
        query.where(MeasureIRI.has(hasUnit, currencyIRI));
        query.where(MeasureIRI.has(hasNumericalValue, priceLiteral));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(query.getQueryString());
        switch (reqResult.length()) {
            case 0:
                return null;
                
            case 1:
                result.put("PriceDetailsIRI", reqResult.getJSONObject(0).getString("PriceDetailsIRI"));
                result.put("priceIRI", reqResult.getJSONObject(0).getString("priceIRI"));
                result.put("MeasureIRI", reqResult.getJSONObject(0).getString("MeasureIRI"));
                result.put("currencyIRI", reqResult.getJSONObject(0).getString("currencyIRI"));
                result.put("price", reqResult.getJSONObject(0).getString("price"));
                return result;
            default:
                throw new JPSRuntimeException("Invoice has more than 1 price IRI set: " + invoiceIRI + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private JSONObject getPurchaseDocsTriples (String InvoiceNum , String PONum, String DONum) {
        JSONObject result = new JSONObject();

        //NOTE I'm just being lazy here doing 1 query for each docs (this is written on a Friday evening), 
        //but in the future, a single query may be more efficient
        //~MTL

        //Handle Invoice
        result.put("invoice", queryDocumentFromDocNum(InvoiceNum, invoiceNumber, hasInvoiceLine));
        result.put("DO", queryDocumentFromDocNum(DONum, deliveryOrderNumber, hasDeliveryOrderLine));
        result.put("PO", queryDocumentFromDocNum(PONum, purchaseOrderNumber, hasPurchaseOrderLine));

        return result;
    }


    private JSONObject queryDocumentFromDocNum (String DocNum, Iri predicateToID, Iri predicateToDocLine) {
        JSONObject result = new JSONObject();
        SelectQuery queryInvoice = Queries.SELECT();
        queryInvoice.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable InvoiceIRI = SparqlBuilder.var("InvoiceIRI");
        Variable InvoiceLineIRI = SparqlBuilder.var("InvoiceLineIRI");
        queryInvoice.where(InvoiceIRI.has(predicateToID, DocNum));
        queryInvoice.where(InvoiceIRI.has(predicateToDocLine, InvoiceLineIRI));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(queryInvoice.getQueryString());
        switch (reqResult.length()) {
            case 0:
                //Doc doesn't exist. Make new IRIs
                result.put("InvoiceIRI", "");
                result.put("InvoiceLineIRI", "");
                return result;
                
            case 1:
                result.put("InvoiceIRI", reqResult.getJSONObject(0).getString("InvoiceIRI"));
                result.put("InvoiceLineIRI", reqResult.getJSONObject(0).getString("InvoiceLineIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Document has more than 1 IRIs: " + DocNum + ". Check the knowledge graph for duplicates.", null);
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
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(isSuppliedBy, SupplierOrgIRI));
        query.where(SupplierOrgIRI.has(hasName, SupplierNameIRI));
        query.where(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));

        return storeClientPurchDoc.executeQuery(query.getQueryString());

    }
        private JSONArray getAllManufacturerIRI() {
        Variable ManufacturerOrgIRI = SparqlBuilder.var("ManufacturerIRI");
        Variable ManufacturerNameIRI = SparqlBuilder.var("ManufacturerNameIRI");
        Variable ManufacturerNameLiteral = SparqlBuilder.var("ManufacturerName");
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(isManufacturedBy, ManufacturerOrgIRI));
        query.where(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
        query.where(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral));

        return storeClientPurchDoc.executeQuery(query.getQueryString());

    }

    public JSONArray getItemListByDocIRI (String InvoiceIRI, String POiri, String DOiri){
        return getItemListByDocIRI(iri(InvoiceIRI), iri(POiri), iri(DOiri));
    }

    public JSONArray getItemListByDocIRI (Iri InvoiceIRI, Iri POiri, Iri DOiri){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable itemIRI = SparqlBuilder.var("itemIRI");
        if(InvoiceIRI != null){
            query.where(InvoiceIRI.has(hasItem, itemIRI));
        }
        if(POiri != null){
            query.where(POiri.has(hasItem, itemIRI));
        }
        if(DOiri != null){
            query.where(DOiri.has(hasItem, itemIRI));
        }

        return storeClientPurchDoc.executeQuery(query.getQueryString());
    }

    private JSONArray getIRIbyLiteral (String literal, RemoteStoreClient storeClient) {
        //It is assumed that the ID is unique and no duplicate ID exist
        //Cause thats what IDs do (at least supposed to)
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(query.var(), literal));
        return storeClient.executeQuery(query.getQueryString());
    }

    private JSONArray getIRIbyLiteral (String literal, Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(predicate, literal));
        JSONArray reqResult = storeClient.executeQuery(query.getQueryString());
        return reqResult;
        
    }

    private Iri getIRIbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(predicate, object));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return iri(reqResult.getString("x0"));
    }

    private JSONArray getIRIListbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(predicate, object));
        return storeClient.executeQuery(query.getQueryString());
    }

    private Iri getIRIbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(predicate, query.var()));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return iri(reqResult.getString("x0"));
    }

    private JSONArray getIRIListbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(predicate, query.var()));
        return storeClient.executeQuery(query.getQueryString());
    }

    private JSONArray getIriListByPredicate (Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(predicate, query.var()));

        return storeClient.executeQuery(query.getQueryString());
    }


    /*
     * Check the existance of the IRI in the kg
     */
    Boolean checkConceptExistence(Iri target, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        
        //Query for all class acting as subject or object
        //query.where(query.var().has(query.var(),target));
        query.where(target.has(query.var(), query.var()));

        //Look for class in ontodevice
        JSONArray result = storeClient.executeQuery(query.getQueryString());
        //System.out.println(result);
        if (result.length() >= 1) {return true;}

        return false; 
    }

    Boolean checkLiteralExistence(String target, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        
        //Query for all class acting as subject or object
        //query.where(query.var().has(query.var(),target));
        query.where(query.var().has(query.var(), target));

        //Look for class in ontodevice
        JSONArray result = storeClient.executeQuery(query.getQueryString());
        //System.out.println(result);
        if (result.length() >= 1) {return true;}

        return false; 
    }

    /*
     * Create new IRI
     */
    private Iri genIRI (String ID, Prefix prefix) {
        return prefix.iri(ID + "_" + UUID.randomUUID());
    }

    private Iri genIRI (String ID, String prefix) {
        return iri(genIRIString(ID, prefix));
    }

    private String genIRIString (String ID, Prefix prefix) {
        return genIRI(ID, prefix).getQueryString();
    }
    private String genIRIString (String ID, String prefix) {
        return prefix + ID + "_" + UUID.randomUUID();
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
        query.insert(deviceIRIVar.isA(deviceTypeIRI));
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
        if(!WorkspaceIDLiteral.isBlank()){
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
        Iri WorkspaceOwnerIRI = iri(data.getString("personIRI"));
        Iri WorkspaceIRI = iri(data.getString("workspaceIRI"));
        String WorkspaceIDLiteral = data.getString("workspaceName");
        String LocationString = data.getString("Location");
        //Storage
        String storageIRIString = data.getString("storageIRI");
        //String furnitureIRIString = data.getString("furnitureIRI");
        String cabinetIRIString = data.getString("cabinetIRI");
        String storageIDLiteral = data.getString("storageID");
        
        //Query
        //get device type
        query.insert(deviceIRI.isA(deviceTypeIRI));
        //Stored in -- Assumes the storage IRI exist somewhere
        if(!storageIRIString.isBlank()){
            query.insert(deviceIRI.has(isStoredIn, iri(storageIRIString)));
        }
        else{
            if(!cabinetIRIString.isBlank()){
                query.insert(deviceIRI.has(isStoredIn, iri(cabinetIRIString)));
                query.insert(iri(cabinetIRIString).has(hasFurnitureIdentifier, storageIDLiteral));
            }
        }
        //get location
        if (LocationString == "Research Wing" || LocationString == "CREATE Tower"){
            if(roomIRI != null){
                query.insert(roomIRI.has(containsElement, deviceIRI));

                //Workspace
                if(!WorkspaceIDLiteral.isBlank()){
                    query.insert(WorkspaceIRI.isA(Workspace));
                    query.insert(deviceIRI.has(isLocatedAt, WorkspaceIRI));
                    query.insert(roomIRI.has(containsElement, deviceIRI));
                    query.insert(WorkspaceIRI.has(isLocatedIn, roomIRI));
                    query.insert(WorkspaceOwnerIRI.has(hasAllocatedWorkspace, WorkspaceIRI));
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
        query.insert(ServiceCategoryIRI.has(attributeName, ServiceCategoryTypeLiteral));
        query.insert(ServiceCategoryIRI.has(attributeValue, ServiceCategoryNameLiteral));
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

        storeClientPurchDoc.executeUpdate(query.getQueryString());
    }

    /**
     * Retrieve asset info based on ID
     */
    JSONObject retrieve (Iri ID) {
        JSONObject result = new JSONObject();
        JSONObject assetNamespaceResult = retrieveAssetNamespace(ID);

        Iri deviceIRI, itemIRI;
        deviceIRI = iri(assetNamespaceResult.getString("deviceIRI"));
        itemIRI = iri(assetNamespaceResult.getString("itemIRI"));

        JSONObject deviceNamespaceResult = retrieveDeviceNamespace(deviceIRI);
        JSONObject PurchDocNamespaceResult = retrievePurchaseDocNamespace(itemIRI);

        return result;
    }


    private JSONObject retrieveAssetNamespace(Iri ID) {

        //Asset namespace query
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable deviceIRIVar = SparqlBuilder.var("deviceIRI");
        Variable itemIRIVar = SparqlBuilder.var("itemIRI");
        Variable labelLiteralVar = SparqlBuilder.var("label");
        //Owner and human
        Variable PersonIRI = SparqlBuilder.var("personIRI");
        Variable personNameIRI = SparqlBuilder.var("personNameIRI");
        Variable deviceOwnerLiteral = SparqlBuilder.var("assignedTo");
        //Serial and model number
        Variable serialNumberIRI = SparqlBuilder.var("serialNum");
        Variable modelNumber = SparqlBuilder.var("modelNumber");
        //price and money
        Variable amountOfMoneyVar = SparqlBuilder.var("amtMoney");
        Variable priceMeasureIRI = SparqlBuilder.var("priceMeasureIRI");
        Variable priceLiteralVar = SparqlBuilder.var("price");
        Variable priceCurrencyIRI = SparqlBuilder.var("currencyIRI");
        Variable storageIRI = SparqlBuilder.var("storage");
        //Spec sheets and manual
        Variable manualURL = SparqlBuilder.var("manualURL");
        Variable SpecSheetIRI = SparqlBuilder.var("SpecSheetIRI"); //also includes manual
        Variable SpecSheetFileLiteral = SparqlBuilder.var("SpecSheet"); //also includes manual
        Variable SpecSheetPageLiteral = SparqlBuilder.var("SpecSheetPage");
        Variable SpecSheetTypeIRI = SparqlBuilder.var("SpecSheetType");//Should be between manual and specsheet
        //Supplier and manuf
        Variable SupplierOrgIRI = SparqlBuilder.var("SupplierOrgIRI");
        Variable SupplierNameIRI = SparqlBuilder.var("SupplierNameIRI");
        Variable SupplierNameLiteral = SparqlBuilder.var("SupplierName");
        Variable ManufacturerOrgIRI = SparqlBuilder.var("ManufacturerIRI");
        Variable ManufacturerNameIRI = SparqlBuilder.var("ManufacturerNameIRI");
        Variable ManufacturerNameLiteral = SparqlBuilder.var("ManufacturerName");


        /*
         * RETRIEVE QUERY
         */
        //Get the "entrypoint" from ID to the 3 namespaces in asset namespace
        //For asset the "entrypoint" is ID, we can get Device IRI (hasInventoryID) and Item IRI ( references device iri)


        //get device on "deviceVar" from asset namespace
        query.where(deviceIRIVar.has(hasItemInventoryIdentifier, ID));
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
        //Serial number
        query.where(GraphPatterns.optional(deviceIRIVar.has(serialNumber, serialNumberIRI)));
        //model number
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasModel, modelNumber)));
        //manual URL
        query.where(GraphPatterns.optional(deviceIRIVar.has(RDFS.SEEALSO, manualURL)));
        //Price
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasPrice, amountOfMoneyVar)));
        query.where(GraphPatterns.optional(amountOfMoneyVar.has(hasValue, priceMeasureIRI)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasNumericalValue, priceLiteralVar)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasUnit, priceCurrencyIRI)));
        //Stored in
        query.where(GraphPatterns.optional(deviceIRIVar.has(isStoredIn, storageIRI)));
        //Datasheet and manual
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasDataSheet, SpecSheetIRI)));
        query.where(GraphPatterns.optional(SpecSheetIRI.has(availableAt, SpecSheetFileLiteral)));
        query.where(GraphPatterns.optional(SpecSheetIRI.has(RDFS.COMMENT, SpecSheetPageLiteral)));
        query.where(GraphPatterns.optional(SpecSheetIRI.has(RDF.TYPE, SpecSheetTypeIRI)));
        //Supplier & manufacturer
        query.where(GraphPatterns.optional(deviceIRIVar.has(isSuppliedBy, SupplierOrgIRI)));
        query.where(GraphPatterns.optional(SupplierOrgIRI.has(hasName, SupplierNameIRI)));
        query.where(GraphPatterns.optional(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral)));
        query.where(GraphPatterns.optional(deviceIRIVar.has(isManufacturedBy, SupplierOrgIRI)));
        query.where(GraphPatterns.optional(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI)));
        query.where(GraphPatterns.optional(ManufacturerNameIRI.has(hasLegalName, ManufacturerNameLiteral)));

        return handleAssetData(storeClientAsset.executeQuery(query.getQueryString()));
        
    }
    
    JSONObject handleAssetData (JSONArray requestResult) {
        JSONObject result = new JSONObject();
        //TODO Handle asset data here. Later
        return result;
    }

    JSONObject retrieveDeviceNamespace (Iri deviceIRI) {
        //TODO repair this usint the queryRegisteredLocation and queryUnregisteredLocation
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable deviceTypeIRI = SparqlBuilder.var("deviceTypeIRI");
        Variable roomtypeIRI = SparqlBuilder.var("RoomTypeIRI");
        Variable roomIRI = SparqlBuilder.var("RoomIRI");
        Variable WorkspaceIRI = SparqlBuilder.var("WorkspaceIRI");
        Variable WorkspaceIDLiteral = SparqlBuilder.var("WorkspaceID");
        
        //Query
        //get device type
        query.where(deviceIRI.has(RDF.TYPE, deviceTypeIRI));
        //get location
        query.where(roomIRI.has(containsElement, deviceIRI));
        query.where(roomIRI.has(RDF.TYPE, roomtypeIRI));
        //Workspace
        //NOTE when transcribing to update query, add isLocatedIn, hasAllocatedWorkspace
        query.where(GraphPatterns.optional(deviceIRI.has(isLocatedAt, WorkspaceIRI)));
        query.where(GraphPatterns.optional(WorkspaceIRI.has(hasIdentifier, WorkspaceIDLiteral)));


        
        return handleDeviceData(storeClientDevice.executeQuery(query.getQueryString()));
    }

    JSONObject handleDeviceData (JSONArray requestResult) {
        JSONObject result = new JSONObject();
        //TODO Handle device data here. Later
        return result;
    }

    JSONObject retrievePurchaseDocNamespace (Iri itemIRI) {
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
        Variable priceDetailsIRI = SparqlBuilder.var("priceDetailsIRI");
        Variable priceIRI = SparqlBuilder.var("priceIRI");
        Variable priceMeasureIRI = SparqlBuilder.var("priceMeasureIRI");
        Variable priceLiteral = SparqlBuilder.var("price");
        Variable priceCurrencyIRI = SparqlBuilder.var("currencyIRI");

        //NOTE
        //When transcribing to update query, add isSuppliedBy and isManufacturedBy to ItemIRI
        //Invoice, PO, and DO are OPTIONAL


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
        //NOTE When transcribing to update query, add connections to DO and PO
        /*
         * Temporarily omitted as the data sould come from Synergix, not KG 
         *
        query.where(GraphPatterns.optional(invoiceLineIRI.has(hasPriceDetails, priceDetailsIRI)));
        query.where(GraphPatterns.optional(priceDetailsIRI.has(hasPrice, priceIRI)));
        query.where(GraphPatterns.optional(priceIRI.has(hasValue, priceMeasureIRI)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasNumericalValue, priceLiteral)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasUnit, priceCurrencyIRI)));
        */

        return handlePurchaseDocData(storeClientPurchDoc.executeQuery(query.getQueryString()));
    }

    JSONObject handlePurchaseDocData (JSONArray requestResult) {
        JSONObject result = new JSONObject();
        //TODO Handle Purchase Docs data here. Later
        return result;
    }
}