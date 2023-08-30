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
    
    private static final Prefix P_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix P_LAB = SparqlBuilder.prefix("ontolab",iri(ONTOLAB));
    private static final Prefix P_SYS = SparqlBuilder.prefix("ontosystem",iri(ONTOSYSTEM));
    private static final Prefix P_INMA = SparqlBuilder.prefix("ontoinma",iri(ONTOINMA));
    private static final Prefix P_ASSET = SparqlBuilder.prefix("ontoassetmanagement",iri(ONTOASSET));
    private static final Prefix P_EPE = SparqlBuilder.prefix("ontoelecpowerequipment",iri(ONTOEPE));
    private static final Prefix P_BIM = SparqlBuilder.prefix("ontobim", iri(ONTOBIM));
    private static final Prefix P_SAREF = SparqlBuilder.prefix("saref", iri("https://saref.etsi.org/core/"));
    private static final Prefix P_OM = SparqlBuilder.prefix("saref", iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    private static final Prefix P_FIBO_AAP = SparqlBuilder.prefix("FIBOaap", iri("https://spec.edmcouncil.org/fibo/ontology/FND/AgentsAndPeople/"));
    private static final Prefix P_FIBO_ORG = SparqlBuilder.prefix("FIBOorg",iri("https://spec.edmcouncil.org/fibo/ontology/FND/Organizations/"));
    private static final Prefix P_BOT = SparqlBuilder.prefix("bot", iri("https://w3id.org/bot#"));
    private static final Prefix P_P2P_ITEM = SparqlBuilder.prefix("P2Pitem", iri("https://purl.org/p2p-o/item#")); 
    private static final Prefix P_P2P_DOCLINE = SparqlBuilder.prefix("P2Pdocline", iri("https://purl.org/p2p-o/documentline#")); 
    private static final Prefix P_P2P_INVOICE = SparqlBuilder.prefix("P2Pinvoice", iri("https://purl.org/p2p-o/invoice#")); 


    //properties
    private static final Iri consistsOf = P_SAREF.iri("consistsOf");
    private static final Iri hasModel = P_SAREF.iri("hasModel");

    private static final Iri hasItemInventoryIdentifier = P_ASSET.iri("hasItemInventoryIdentifier");
    private static final Iri references = P_ASSET.iri("references");
    private static final Iri assignedTo = P_ASSET.iri("assignedTo");
    private static final Iri serialNumber = P_ASSET.iri("serialNumber");
    private static final Iri isStoredIn = P_ASSET.iri("isStoredIn");
    private static final Iri availableAt = P_ASSET.iri("availableAt");
    private static final Iri isSuppliedBy = P_ASSET.iri("isSuppliedBy");
    private static final Iri isManufacturedBy = P_ASSET.iri("isManufacturedBy");
    private static final Iri isLocatedIn = P_ASSET.iri("isLocatedIn");
    private static final Iri isLocatedAt = P_ASSET.iri("isLocatedAt");
    private static final Iri hasAllocatedWorkspace = P_ASSET.iri("hasAllocatedWorkspace");
    private static final Iri hasIdentifier = P_ASSET.iri("hasIdentifier");
    private static final Iri hasDeliveryOrderLine = P_ASSET.iri("hasDeliveryOrderLine");
    private static final Iri hasPurchaseOrderLine = P_ASSET.iri("hasPurchaseOrderLine");
    private static final Iri deliveryOrderNumber = P_ASSET.iri("deliveryOrderNumber");
    private static final Iri purchaseOrderNumber = P_ASSET.iri("purchaseOrderNumber");
    private static final Iri hasPriceDetails = P_ASSET.iri("hasPriceDetails");
    private static final Iri hasWorkspaceIdentifier = P_ASSET.iri("hasWorkspaceIdentifier");
    private static final Iri hasCurrentLocation = P_ASSET.iri("hasCurrentLocation");

    private static final Iri hasDataSheet = P_DEV.iri("hasDataSheet");
    private static final Iri hasPrice = P_DEV.iri("hasPrice");
    
    private static final Iri hasRoom = P_BIM.iri("hasRoom");

    private static final Iri hasValue = P_OM.iri("hasValue");
    private static final Iri hasUnit = P_OM.iri("hasUnit");
    private static final Iri hasNumericalValue = P_OM.iri("hasNumericalValue");

    private static final Iri itemName = P_P2P_ITEM.iri("itemName");
    private static final Iri hasAttribute = P_P2P_ITEM.iri("hasAttribute");
    private static final Iri attributeName = P_P2P_ITEM.iri("attributeName");
    private static final Iri attributeValue = P_P2P_ITEM.iri("attributeValue");
    private static final Iri invoiceNumber = P_P2P_INVOICE.iri("invoiceNumber");
    private static final Iri hasInvoiceLine = P_P2P_INVOICE.iri("hasInvoiceLine");
    private static final Iri hasItem = P_P2P_DOCLINE.iri("hasItem");
    private static final Iri InvoicedQuantity = P_P2P_DOCLINE.iri("InvoicedQuantity"); 

    private static final Iri hasPersonName = P_FIBO_AAP.iri("People/hasPersonName");
    private static final Iri hasLegalName = iri("https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/hasLegalName");

    private static final Iri hasName = iri("https://www.omg.org/spec/Commons/Designators/hasName");

    private static final Iri containsElement = P_BOT.iri("containsElement");

    //External classes
    private static final Iri Item = P_P2P_ITEM.iri("Item");
    
    private static final Iri SpecSheet = P_ASSET.iri("SpecSheet");
    private static final Iri Manual = P_ASSET.iri("Manual");
    private static final Iri Workspace = P_ASSET.iri("Workspace");
    private static final Iri ServiceCategory = P_ASSET.iri("ServiceCategory");
    private static final Iri DeliveryOrder = P_ASSET.iri("DeliveryOrder");
    private static final Iri DeliveryOrderLine = P_ASSET.iri("DeliveryOrderLine"); 
    private static final Iri PurchaseOrder = P_ASSET.iri("PurchaseOrder");
    private static final Iri PurchaseOrderLine = P_ASSET.iri("PurchaseOrderLine");
    private static final Iri E_Invoice = P_ASSET.iri("E-Invoice");
    private static final Iri InvoiceLine = P_ASSET.iri("InvoiceLine");
    private static final Iri PriceDetails = P_ASSET.iri("PriceDetails");
    private static final Iri HomeTotalDiscountedAfterTaxPrice = P_ASSET.iri("HomeTotalDiscountedAfterTaxPrice");

    private static final Iri Person = P_FIBO_AAP.iri("Person");
    private static final Iri PersonName = P_FIBO_AAP.iri("PersonName");
    private static final Iri FormalOrganization = P_FIBO_ORG.iri("FormalOrganization");
    private static final Iri OrganizationName = P_FIBO_ORG.iri("OrganizationName");

    private static final Iri AmountOfMoney = P_OM.iri("AmountOfMoney");
    private static final Iri Measure = P_OM.iri("Measure");
    private static final Iri SingaporeDollar = P_OM.iri("SingaporeDollar");

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
     * Instantiate asset based on data from excel file
     */
    public void instantiate (JSONObject AssetDataRaw) throws Exception{
        //Get IRI from ID
        Iri deviceIRI = getIRIbyID(AssetDataRaw.getString("ID"), storeClientAsset);
        if(deviceIRI != null){
            throw new Exception("Instance already exist for id: " + AssetDataRaw.getString("ID") + 
                "Please use /update instead for updating data."
            );
        }

        JSONObject AssetData = new JSONObject();
        //Create IRIs
        //Create Device IRI
        //TODO create safety check here -- class must exist in the given ontology
        Prefix devicePrefix = getPrefixFromString(AssetDataRaw.getString("Prefix"));
        deviceIRI = genIRI(AssetDataRaw.getString("AssetClass"), devicePrefix);
        String itemIRI = genIRIString("Item", P_ASSET);
        String deviceTypeIRI = devicePrefix.iri(AssetDataRaw.getString("AssetClass")).toString();
        
        AssetData.put("deviceIRI", deviceIRI.toString());
        AssetData.put("deviceType", deviceTypeIRI.toString());
        AssetData.put("ID", AssetDataRaw.getString("ID"));
        AssetData.put("label", AssetDataRaw.getString("Name"));
        AssetData.put("itemIRI", itemIRI);
        AssetData.put("itemComment", AssetDataRaw.getString("Comments"));
        AssetData.put("ServiceCategoryIRI", genIRIString("ServiceCategory", P_ASSET));
        AssetData.put("ServiceCategoryName", AssetDataRaw.getString("BudgetCat"));
        AssetData.put("ServiceCategoryType", AssetDataRaw.getString("ServiceCode"));


        //Persons IRI
        String assigneeName = AssetDataRaw.getString("AssignedTo");
        String workspaceName = AssetDataRaw.getString("WorkspaceName");
        String personIRI = "";
        String personNameIRI = "";
        String workspaceIRI = "";

        if(assigneeName != null){
            JSONObject PersonIRIs = getPersonTriples(assigneeName);
            personIRI = PersonIRIs.getString("PersonIRI");
            personNameIRI = PersonIRIs.getString("PersonNameIRI");

            if(workspaceName != null){
                workspaceIRI = getWorkspaceIRIByName(workspaceName).toString();
            }
        }
        AssetData.put("assignedTo", assigneeName);
        AssetData.put("personIRI", personIRI);
        AssetData.put("personNameIRI", personNameIRI);
        AssetData.put("workspaceName", workspaceName);
        AssetData.put("workspaceIRI", workspaceIRI);

        
        //Handle pricing
        //TODO handle different currencies, currently default to SGD
        String AmtOfMoneyIRI = ""; 
        String MeasureIRI = "";
        String currencyIRI = "";
        String price = AssetDataRaw.getString("price");
        if(price != null){
            AmtOfMoneyIRI = genIRIString("AmountOfMoney", P_ASSET);
            MeasureIRI = genIRIString("Measure", P_ASSET);
            //Handle currency here
            currencyIRI = SingaporeDollar.toString();
        }
        AssetData.put("amtMoney", AmtOfMoneyIRI);
        AssetData.put("priceMeasureIRI", MeasureIRI);
        AssetData.put("price", price);
        AssetData.put("currencyIRI", currencyIRI);

        //Manual and Datasheets
        //SpecSheet
        String SpecSheetIRI = "";
        String SpecSheetFile = AssetDataRaw.getString("SpecSheet");
        String SpecSheetPage = AssetDataRaw.getString("SpecSheetPage");
        if (SpecSheetFile != null) {
            SpecSheetIRI = genIRIString("SpecSheet", P_ASSET);
        }
        if (SpecSheetPage == null) {
            SpecSheetPage = "";
        }
        AssetData.put("SpecSheetIRI", SpecSheetIRI);
        AssetData.put("SpecSheetPage", SpecSheetPage);
        AssetData.put("SpecSheet", SpecSheetFile);
        //Manual
        String ManualFile = AssetData.getString("Manual");
        String ManualURL = AssetData.getString("ManualURL");
        String ManualIRI = "";
        if(ManualFile == null){
            ManualFile = "";
        }
        else{
            ManualIRI = genIRIString("Manual", P_ASSET);
        }
        if (ManualURL == null) {ManualURL = "";}
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
        if (SupplierName != null){
            JSONObject orgIRI = getOrganizationTriples(SupplierName);
            SupplierNameIRI = orgIRI.getString("orgNameIRI");
            SupplierOrgIRI = orgIRI.getString("orgIRI");
        }
        if(ManufacturerName != null) {
            JSONObject orgIRI = getOrganizationTriples(ManufacturerName);
            ManufacturerNameIRI = orgIRI.getString("orgNameIRI");
            ManufacturerOrgIRI = orgIRI.getString("orgIRI");
        }

        AssetData.put("SupplierName", SupplierName);
        AssetData.put("SupplierNameIRI", SupplierNameIRI);
        AssetData.put("SupplierOrgIRI", SupplierOrgIRI);

        AssetData.put("ManufacturerName", ManufacturerName);
        AssetData.put("ManufacturerNameIRI", ManufacturerNameIRI);
        AssetData.put("ManufacturerOrgIRI", ManufacturerOrgIRI);

        //serial and model number
        String SerialNum = AssetData.getString("serialNum");
        String ModelNum = AssetData.getString("modelNumber");
        if(SerialNum == null){
            SerialNum = "";
        }
        if(ModelNum == null){
            ModelNum = "";
        }
        AssetData.put("serialNum", SerialNum);
        AssetData.put("modelNumber", ModelNum);

        //storage
        AssetData.put("storage", AssetDataRaw.getString("StorageIRI"));

        //Location and rooms
        //Workspace IRI is handled at "Person"
        String location = "";
        String room = "";
        String workspace = "";
        String roomIRI = "";
        String sectionIRI = "";
        JSONObject locationIRIs = getLocationTriplesByDevice(roomIRI, sectionIRI);
        if (locationIRIs == null) {
            //Create IRIs here
            //TODO handle existing rooms and locations.
            
        }

        //Purchase docs
        //Item and service code are handled above together with device
        String invoiceNum = AssetDataRaw.getString("invoiceNum");
        String PONum = AssetDataRaw.getString("PurchaseOrderNum");
        String DONum = AssetDataRaw.getString("DeliveryOrderNum");

        JSONObject reqResultDocs = getPurchaseDocsTriples(invoiceNum, PONum, DONum);
        //Invoice
        AssetData.put("invoiceNum", invoiceNum);
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

        createInstance(AssetData);


    }


    Prefix getPrefixFromString (String ontology) throws Exception {
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
        JSONObject reqResult = getIRIbyLiteral (name, hasPersonName, storeClientAsset);

        switch (reqResult.length()) {
            case 0:
                //Create IRI and add to result
                Iri PersonIRI = genIRI("Person", P_ASSET);
                Iri PersonNameIRI = genIRI("PersonName", P_ASSET);
                result.put("PersonIRI", PersonIRI);
                result.put("PersonNameIRI", PersonNameIRI);
                return result;
            case 1:
                //Add the existing IRI to result
                String personNameIRIString = reqResult.getString("0x01");
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
        JSONObject reqResult = getIRIbyLiteral (orgName, hasLegalName, storeClientAsset);
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
                OrgNameIRI = reqResult.getString("0x01");
                result.put("OrgNameIRI", OrgNameIRI);
                //Query Person instance from person name
                result.put("OrgIRI", getIRIbyIRIObject(iri(OrgNameIRI), hasName, storeClientAsset));
                return result;
            default:
                throw new JPSRuntimeException("An organization has more than 1 instance: " + orgName + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private Iri getIRIbyID (String ID, RemoteStoreClient storeClient){
        JSONObject reqResult = getIRIbyLiteral (ID, hasItemInventoryIdentifier, storeClient);

        switch (reqResult.length()) {
            case 0:
                return null;
            case 1:
                return iri(reqResult.getString("0x01"));
            default:
                throw new JPSRuntimeException("More than 1 asset instance have the same ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }

    }

    private Iri getWorkspaceIRIByName (String name) {
        JSONObject reqResult = getIRIbyLiteral(name, hasWorkspaceIdentifier, storeClientDevice);
        switch (reqResult.length()) {
            case 0:
                //Does not seem right to use Asest prefix here?
                return genIRI("Workspace", P_ASSET);
                
            case 1:
                return iri(reqResult.getString("0x01"));
            default:
                throw new JPSRuntimeException("Workspace has more than 1 instances: " + name + ". Check the knowledge graph for duplicates.", null);
        }
    }

    private JSONObject getLocationTriplesByDevice (String roomIRI, String sectionIRI) {
        return getLocationTriplesByDevice (iri(roomIRI), iri(sectionIRI));
    }

    private JSONObject getLocationTriplesByDevice (Iri roomIRI, Iri sectionIRI) {
        JSONObject result = new JSONObject();
        
        //query if item is not in registered rooms (home, NTU, etc.)
        result = queryUnregisteredLocation();
        if(result == null){
            //query if item is in registered rooms (Research Wing and CREATE Tower)
            result = queryRegisteredLocation(roomIRI, sectionIRI);
            if (result == null) {
                //Instance does not exist yet, create new IRIs
                return null;
            }
            return result;
        }
        return result;
    }

    private JSONObject queryUnregisteredLocation () {
        JSONObject result = new JSONObject();
        SelectQuery queryUnregistered = Queries.SELECT();
        Variable locationLiteral = SparqlBuilder.var("locationLiteral");

        queryUnregistered.where(queryUnregistered.var().has(hasCurrentLocation, locationLiteral));
        JSONArray reqResult = storeClientDevice.executeQuery(queryUnregistered.getQueryString());
        switch (reqResult.length()) {
            case 0:
                //Not of this location type
                return null;
                
            default:
                result.put("Location", iri(reqResult.getJSONObject(0).getString("locationLiteral")));
                return result;
        }
    }

    private JSONObject queryRegisteredLocation (Iri roomIRI, Iri sectionIRI) {
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
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
                result.put("RoomIRI", iri(reqResult.getJSONObject(0).getString("RoomIRI")));
                result.put("SectionIRI", iri(reqResult.getJSONObject(0).getString("SectionIRI")));
                result.put("RoomTypeIRI", iri(reqResult.getJSONObject(0).getString("RoomTypeIRI")));
                result.put("SectionTypeIRI", iri(reqResult.getJSONObject(0).getString("SectionTypeIRI")));

                return result;
            default:
                throw new JPSRuntimeException("Location has more than 1 IRIs: " + sectionIRI + ":" + roomIRI + ". Check the knowledge graph for duplicates.", null);
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
                result.put("InvoiceIRI", iri(reqResult.getJSONObject(0).getString("InvoiceIRI")));
                result.put("InvoiceLineIRI", iri(reqResult.getJSONObject(0).getString("InvoiceLineIRI")));
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
        query.where(query.var().has(query.var(), literal));
        return storeClient.executeQuery(query.getQueryString());
    }

    private JSONObject getIRIbyLiteral (String literal, Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, literal));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return reqResult;
        
    }

    private Iri getIRIbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, object));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return iri(reqResult.getString("x01"));
    }

    private JSONArray getIRIListbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, object));
        return storeClient.executeQuery(query.getQueryString());
    }

    private Iri getIRIbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(subject.has(predicate, query.var()));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return iri(reqResult.getString("x01"));
    }

    private JSONArray getIRIListbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(subject.has(predicate, query.var()));
        return storeClient.executeQuery(query.getQueryString());
    }

    private JSONArray getIriListByPredicate (Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, query.var()));

        return storeClient.executeQuery(query.getQueryString());
    }


    /*
     * Check the existance of the IRI in the kg
     */
    Boolean checkConceptExistence(Iri target, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        
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
        return iri(prefix + ID + "_" + UUID.randomUUID());
    }

    private String genIRIString (String ID, Prefix prefix) {
        return prefix.iri(ID + "_" + UUID.randomUUID()).toString();
    }
    private String genIRIString (String ID, String prefix) {
        return iri(prefix + ID + "_" + UUID.randomUUID()).toString();
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
        //Device
        Iri deviceIRIVar = iri(data.getString("deviceIRI"));
        Iri deviceTypeIRI = iri(data.getString("deviceType"));
        String ID = data.getString("ID");
        Iri itemIRIVar = iri(data.getString("itemIRI"));
        String labelLiteralVar = data.getString("label");
        //Owner and human
        Iri PersonIRI = iri(data.getString("personIRI"));
        Iri personNameIRI = iri(data.getString("personNameIRI"));
        String deviceOwnerLiteral = data.getString("assignedTo");
        //Workspace
        Iri workspaceIRI = iri(data.getString("workspaceIRI"));
        //Serial and model number
        String serialNumberLiteral = data.getString("serialNum");
        String modelNumber = data.getString("modelNumber");
        //price and money
        Iri amountOfMoneyVar = iri(data.getString("amtMoney"));
        Iri priceMeasureIRI = iri(data.getString("priceMeasureIRI"));
        String priceLiteral = data.getString("price");
        Iri priceCurrencyIRI = iri(data.getString("currencyIRI"));
        //Storage
        Iri storageIRI = iri(data.getString("storage"));
        //Spec sheets and manual
        String manualURL = data.getString("manualURL");
        Iri SpecSheetIRI = iri(data.getString("SpecSheetIRI")); 
        String SpecSheetFileLiteral = data.getString("SpecSheet"); 
        String SpecSheetPageLiteral = data.getString("SpecSheetPage");

        Iri ManualIRI = iri(data.getString("ManualIRI")); 
        String ManualFileLiteral = data.getString("Manual"); 


        //Supplier and manuf
        Iri supplierIRIVar = iri(data.getString("suppliedBy"));
        Iri SupplierOrgIRI = iri(data.getString("SupplierOrgIRI"));
        Iri SupplierNameIRI = iri(data.getString("SupplierNameIRI"));
        String SupplierNameLiteral = data.getString("SupplierName");
        Iri ManufacturerOrgIRI = iri(data.getString("ManufacturerIRI"));
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
        query.insert(workspaceIRI.isA(Workspace));
        query.insert(deviceIRIVar.has(assignedTo, PersonIRI));
        query.insert(PersonIRI.has(hasName, personNameIRI));
        query.insert(personNameIRI.has(hasPersonName, Rdf.literalOf(deviceOwnerLiteral)));
        query.insert(PersonIRI.has(hasAllocatedWorkspace, workspaceIRI));

        //Optional IRIs
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

        //Stored in -- Assumes the storage IRI exist somewhere
        if(storageIRI != null){
            query.insert(deviceIRIVar.has(isStoredIn, storageIRI));
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
            query.insert(ManufacturerNameIRI.has(hasName, ManufacturerNameLiteral));
        }
        if(!SupplierNameLiteral.isBlank()){
            query.insert(SupplierOrgIRI.isA(FormalOrganization));
            query.insert(SupplierNameIRI.isA(OrganizationName));
            query.insert(deviceIRIVar.has(isSuppliedBy, ManufacturerOrgIRI));
            query.insert(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
            query.insert(ManufacturerNameIRI.has(hasName, ManufacturerNameLiteral));
        }

        storeClientAsset.executeUpdate(query.getQueryString());
    }

    private void createDeviceNameSpace (JSONObject data){
        ModifyQuery query = Queries.MODIFY();
        Iri deviceIRI = iri(data.getString("deviceIRI"));
        Iri deviceTypeIRI = iri(data.getString("deviceTypeIRI"));
        Iri roomtypeIRI = iri(data.getString("RoomTypeIRI"));
        Iri roomIRI = iri(data.getString("RoomIRI"));
        Iri SectionIRI = iri(data.getString("SectionIRI"));
        Iri SectionTypeIRI = iri(data.getString("SectionTypeIRI"));
        Iri WorkspaceOwnerIRI = iri(data.getString("WorkspaceOwner"));
        Iri WorkspaceIRI = iri(data.getString("WorkspaceIRI"));
        String WorkspaceIDLiteral = data.getString("WorkspaceID");
        String LocationString = data.getString("Location");
        
        //Query
        //get device type
        query.insert(deviceIRI.isA(deviceTypeIRI));
        //get location
        if (LocationString == "Research Wing" || LocationString == "CREATE Tower"){
            if(roomIRI != null){
                query.insert(roomIRI.isA(roomtypeIRI));
                query.insert(roomIRI.has(containsElement, deviceIRI));
                query.where(SectionIRI.has(hasRoom, roomIRI));
                query.where(SectionIRI.isA(SectionTypeIRI));
                //Workspace
                //NOTE when transcribing to update query, add isLocatedIn, hasAllocatedWorkspace
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
        

        storeClientDevice.executeQuery(query.getQueryString());
    }

    private void createPurchaseDocNamespace (JSONObject data){
        ModifyQuery query = Queries.MODIFY();
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
        String DeliveryOrderNumLiteral = data.getString("DeliveryOrderNum");
        Iri PurchaseOrderLineIRI = iri(data.getString("PurchaseOrderLineIRI"));
        String PurchaseOrderNumLiteral = data.getString("PurchaseOrderNum");

        //price instances
        Iri priceDetailsIRI = iri(data.getString("priceDetailsIRI"));
        Iri priceIRI = iri(data.getString("priceIRI"));
        Iri priceMeasureIRI = iri(data.getString("priceMeasureIRI"));
        String priceLiteral = data.getString("price");
        Iri priceCurrencyIRI = iri(data.getString("currencyIRI"));
        //supplier and manuf
        Iri SupplierOrgIRI = iri(data.getString("SupplierOrgIRI"));
        Iri SupplierNameIRI = iri(data.getString("SupplierNameIRI"));
        String SupplierNameLiteral = data.getString("SupplierName");
        Iri ManufacturerOrgIRI = iri(data.getString("ManufacturerIRI"));
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
            query.insert(ManufacturerNameIRI.has(hasName, ManufacturerNameLiteral));
        }
        if(!SupplierNameLiteral.isBlank()){
            query.insert(SupplierOrgIRI.isA(FormalOrganization));
            query.insert(SupplierNameIRI.isA(OrganizationName));
            query.insert(itemIRI.has(isSuppliedBy, ManufacturerOrgIRI));
            query.insert(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI));
            query.insert(ManufacturerNameIRI.has(hasName, ManufacturerNameLiteral));
        }

        storeClientPurchDoc.executeQuery(query.getQueryString());
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