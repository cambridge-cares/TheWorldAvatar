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
    
    private static final Prefix P_DEV = SparqlBuilder.prefix("ontodevice",iri(ONTODEV));
    private static final Prefix P_LAB = SparqlBuilder.prefix("ontolab",iri(ONTOLAB));
    private static final Prefix P_SYS = SparqlBuilder.prefix("ontosystem",iri(ONTOSYSTEM));
    private static final Prefix P_INMA = SparqlBuilder.prefix("ontoinma",iri(ONTOINMA));
    private static final Prefix P_ASSET = SparqlBuilder.prefix("ontoassetmanagement",iri(ONTOASSET));
    private static final Prefix P_EPE = SparqlBuilder.prefix("ontoelecpowerequipment",iri(ONTOEPE));
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

    private static final Iri hasDataSheet = P_DEV.iri("hasDataSheet");
    private static final Iri hasPrice = P_DEV.iri("hasPrice");
     
    private static final Iri hasValue = P_OM.iri("hasValue");
    private static final Iri hasUnit = P_OM.iri("hasUnit");
    private static final Iri hasNumericalValue = P_OM.iri("hasNumericalValue");

    private static final Iri itemName = P_P2P_ITEM.iri("itemName");
    private static final Iri itemAttribure = P_P2P_ITEM.iri("itemAttribute");
    private static final Iri attributeName = P_P2P_ITEM.iri("attributeName");
    private static final Iri attributeValue = P_P2P_ITEM.iri("attributeValue");
    private static final Iri invoiceNumber = P_P2P_INVOICE.iri("invoiceNumber");
    private static final Iri hasInvoiceLine = P_P2P_INVOICE.iri("hasInvoiceLine");
    private static final Iri hasItem = P_P2P_DOCLINE.iri("hasItem");
    private static final Iri InvoicedQuantity = P_P2P_DOCLINE.iri("InvoicedQuantity"); 

    private static final Iri hasGivenName = P_FIBO_AAP.iri("People/hasGivenName");
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
    void instantiate (JSONObject AssetData) throws Exception{
        ModifyQuery modify = Queries.MODIFY();
        Prefix prefix;
        Iri device;
        String id;
        Iri desc;
        Iri vendor, manuf;


        //Get ID
        //ID here is already combined with the Inventory ID yyyy-mm-dd/{ID}
        id = AssetData.getString("id");
        Iri deviceIRI = getIRIbyID(id, storeClientAsset);
        if (deviceIRI==null){
            //TODO create instance here
            //createInstance(AssetData);
        }
        else{
            retrieve(deviceIRI);
        }


        //Get ID, check if exist, if exist, retrieve instead of create, otherwise create new IRI

        //Get prefix and class

        prefix = getPrefixFromString(AssetData.getString("ontology"));

        device = AssetData.getString("class");

        

        //Name and Description
        desc = AssetData.getString("description");

        //Get vendor & manuf


        //Get serial num
        //get model num
        //price
        //invoice
        //Owner
        //Location
        //Datasheet
        //Manual & spec sheet


        //Create instances and check if it exists

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

    private Iri getIRIbyID (String ID, RemoteStoreClient storeClient){
        JSONObject reqResult = getIRIbyLiteral (ID, hasItemInventoryIdentifier, storeClient);

        switch (reqResult.length()) {
            case 0:
                return null;
            case 1:
                return iri(reqResult.getString("x01"));
            default:
                throw new JPSRuntimeException("More than 1 asset instance have the same ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }

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
     * Create new instances
     */
    private void createInstance(JSONObject assetData) {

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
        //Serial and model number
        String serialNumberLiteral = data.getString("serialNum");
        String modelNumber = data.getString("modelNumber");
        //price and money
        Iri amountOfMoneyVar = iri(data.getString("amtMoney"));
        Iri priceMeasureIRI = iri(data.getString("priceMeasureIRI"));
        String priceLiteral = data.getString("price");
        Iri priceCurrencyIRI = iri(data.getString("currencyIRI"));
        //Storage
        Iri storageIRI = iri(data.getString(("storage")));
        //Spec sheets and manual
        String manualURL = data.getString("manualURL");
        Iri SpecSheetIRI = iri(data.getString("SpecSheetIRI")); 
        String SpecSheetFileLiteral = data.getString("SpecSheet"); 
        String SpecSheetPageLiteral = data.getString("SpecSheetPage");

        Iri ManualIRI = iri(data.getString("SpecSheetIRI")); 
        String ManualFileLiteral = data.getString("SpecSheet"); 


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
        //Classes
        query.insert(deviceIRIVar.isA(deviceTypeIRI));
        query.insert(itemIRIVar.isA(Item));



        //Device
        query.insert(deviceIRIVar.has(hasItemInventoryIdentifier, Rdf.literalOf(ID)));
        //get Item IRI from device IRI from asset namespace
        query.insert(itemIRIVar.has(references, deviceIRIVar));

        //While we're querying the asset namespace, query for other available info too
        //Device name from asset list
        query.insert(deviceIRIVar.has(RDFS.LABEL, Rdf.literalOf(labelLiteralVar)));
        //Device owner from asset list
        query.insert(PersonIRI.isA(Person));
        query.insert(personNameIRI.isA(PersonName));
        query.insert(deviceIRIVar.has(assignedTo, PersonIRI));
        query.insert(PersonIRI.has(hasName, personNameIRI));
        query.insert(personNameIRI.has(hasGivenName, Rdf.literalOf(deviceOwnerLiteral)));

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
        Iri WorkspaceOwnerIRI = iri(data.getString("WorkspaceOwner"));
        Iri WorkspaceIRI = iri(data.getString("WorkspaceIRI"));
        String WorkspaceIDLiteral = data.getString("WorkspaceID");
        
        //Query
        //get device type
        query.insert(deviceIRI.isA(deviceTypeIRI));
        //get location
        if(roomIRI != null){
            query.insert(roomIRI.isA(roomtypeIRI));
            query.insert(roomIRI.has(containsElement, deviceIRI));
            //Workspace
            //NOTE when transcribing to update query, add isLocatedIn, hasAllocatedWorkspace
            if(!WorkspaceIDLiteral.isBlank()){
                query.insert(WorkspaceIRI.isA(Workspace));
                query.insert(deviceIRI.has(isLocatedAt, WorkspaceIRI));
                query.insert(WorkspaceIRI.has(isLocatedIn, roomIRI));
                query.insert(WorkspaceOwnerIRI.has(hasAllocatedWorkspace, WorkspaceIRI));
            }
        }

        storeClientDevice.executeQuery(query.getQueryString());
    }

    private void createPurchaseDocNamespace (JSONObject data){
        ModifyQuery query = Queries.MODIFY();
        Iri itemIRI = iri(data.getString("itemIRI"));
        String itemNameLiteral = data.getString("itemName");
        String itemCommentLiteral = data.getString("itemComment");
        Iri ServiceCategoryIRI = iri(data.getString("ServiceCategoryIRI"));
        String ServiceCategoryNameLiteral = data.getString("ServiceCategoryName");
        String ServiceCategoryTypeLiteral = data.getString("ServiceCategoryType");
        //invoice instances
        Iri InvoiceIRI = iri(data.getString("InvoiceIRI"));
        String InvoiceNumLiteral = data.getString("InvoiceNum");
        Iri invoiceLineIRI = iri(data.getString("InvoiceLineIRI"));
        Variable DeliveryOrderIRI = SparqlBuilder.var("DeliveryOrderIRI");
        Variable PurchaseOrderIRI = SparqlBuilder.var("PurchaseOrderIRI");
        Variable DeliveryOrderLineIRI = SparqlBuilder.var("DeliveryOrderLineIRI");
        Variable DeliveryOrderNumLiteral = SparqlBuilder.var("DeliveryOrderNum");
        Variable PurchaseOrderLineIRI = SparqlBuilder.var("PurchaseOrderLineIRI");
        Variable PurchaseOrderNumLiteral = SparqlBuilder.var("PurchaseOrderNum");
        Variable invoicedQuantityLiteral = SparqlBuilder.var("invoicedQUantity");
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
        query.insert(itemIRI.isA(Item));
        query.insert(ServiceCategoryIRI.isA(ServiceCategory));
        query.insert(itemIRI.has(itemName, Rdf.literalOf(itemNameLiteral)));
        query.insert(itemIRI.has(RDFS.COMMENT, Rdf.literalOf(itemCommentLiteral)));
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
            query.insert(invoiceLineIRI.has(InvoicedQuantity, invoicedQuantityLiteral));
        }

        //DO
        if(!DeliveryOrderNumLiteral.isBlank()){
            query.insert(DeliveryOrderIRI.isA(DeliveryOrder));
            query.insert(DeliveryOrderLineIRI.isA(DeliveryOrderLine));
            query.insert(DeliveryOrderLineIRI.has(hasItem, itemIRI));
            query.insert(DeliveryOrderIRI.has(hasDeliveryOrderLine, DeliveryOrderLineIRI));
            query.insert(DeliveryOrderIRI.has(deliveryOrderNumber, DeliveryOrderNumLiteral));
            query.insert(DeliveryOrderIRI.has(InvoicedQuantity,invoicedQuantityLiteral));
        }

        //PO
        if(!PurchaseOrderNumLiteral.isBlank()){
            query.insert(PurchaseOrderIRI.isA(PurchaseOrder));
            query.insert(PurchaseOrderLineIRI.isA(PurchaseOrderLine));
            query.insert(PurchaseOrderLineIRI.has(hasItem, itemIRI));
            query.insert(PurchaseOrderIRI.has(hasPurchaseOrderLine, PurchaseOrderLineIRI));
            query.insert(PurchaseOrderIRI.has(purchaseOrderNumber, PurchaseOrderNumLiteral));
            query.insert(PurchaseOrderIRI.has(InvoicedQuantity,invoicedQuantityLiteral));
        }
        
        //Price
        //NOTE When transcribing to update query, add connections to DO and PO
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
        

        storeClientPurchDoc.executeQuery(query.getQueryString());
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
        query.where(personNameIRI.has(hasGivenName, deviceOwnerLiteral));
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
        query.where(GraphPatterns.optional(SupplierNameIRI.has(hasName, SupplierNameLiteral)));
        query.where(GraphPatterns.optional(deviceIRIVar.has(isManufacturedBy, SupplierOrgIRI)));
        query.where(GraphPatterns.optional(ManufacturerOrgIRI.has(hasName, ManufacturerNameIRI)));
        query.where(GraphPatterns.optional(ManufacturerNameIRI.has(hasName, ManufacturerNameLiteral)));

        return handleAssetData(storeClientAsset.executeQuery(query.getQueryString()));
        
    }
    
    JSONObject handleAssetData (JSONArray requestResult) {
        JSONObject result = new JSONObject();
        //TODO Handle asset data here. Later
        return result;
    }

    JSONObject retrieveDeviceNamespace (Iri deviceIRI) {
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
        Variable invoicedQuantityLiteral = SparqlBuilder.var("invoicedQUantity");
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
        query.where(GraphPatterns.optional(DeliveryOrderIRI.has(invoiceNumber, InvoiceNumLiteral)));
        //PO
        query.where(GraphPatterns.optional(PurchaseOrderLineIRI.has(hasItem, itemIRI)));
        query.where(GraphPatterns.optional(PurchaseOrderIRI.has(hasDeliveryOrderLine, PurchaseOrderLineIRI)));
        query.where(GraphPatterns.optional(PurchaseOrderIRI.has(deliveryOrderNumber, PurchaseOrderNumLiteral)));
        query.where(GraphPatterns.optional(PurchaseOrderIRI.has(invoiceNumber, InvoiceNumLiteral)));
        //Price
        //NOTE When transcribing to update query, add connections to DO and PO
        query.where(GraphPatterns.optional(invoiceLineIRI.has(hasPriceDetail, priceDetailsIRI)));
        query.where(GraphPatterns.optional(priceDetailsIRI.has(hasPrice, priceIRI)));
        query.where(GraphPatterns.optional(priceIRI.has(hasValue, priceMeasureIRI)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasNumericalValue, priceLiteral)));
        query.where(GraphPatterns.optional(priceMeasureIRI.has(hasUnit, priceCurrencyIRI)));


        return handlePurchaseDocData(storeClientPurchDoc.executeQuery(query.getQueryString()));
    }

    JSONObject handlePurchaseDocData (JSONArray requestResult) {
        JSONObject result = new JSONObject();
        //TODO Handle Purchase Docs data here. Later
        return result;
    }
}