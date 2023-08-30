package uk.ac.cam.cares.jps.agent.assetmanager;

import org.eclipse.rdf4j.model.vocabulary.RDF;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.jooq.False;
import org.jooq.UpdateQuery;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class AssetRetriever {
    RemoteStoreClient storeClientAsset, storeClientDevice, storeClientPurchDoc;

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

    AssetRetriever(RemoteStoreClient storeClientAsset, RemoteStoreClient storeClientDevice, RemoteStoreClient storeClientPurchDoc;) {

    }


    public JSONObject getPersonTriples(String name){
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

    public JSONObject getOrganizationTriples (String orgName) {
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

    public Iri getIRIbyID (String ID, RemoteStoreClient storeClient){
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

    public Iri getWorkspaceIRIByName (String name) {
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

    public JSONObject getLocationTriplesByDevice (String roomIRI, String sectionIRI) {
        return getLocationTriplesByDevice (iri(roomIRI), iri(sectionIRI));
    }

    public JSONObject getLocationTriplesByDevice (Iri roomIRI, Iri sectionIRI) {
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

    public JSONObject queryUnregisteredLocation () {
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

    public JSONObject queryRegisteredLocation (Iri roomIRI, Iri sectionIRI) {
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

    public JSONObject getPurchaseDocsTriples (String InvoiceNum , String PONum, String DONum) {
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


    public JSONObject queryDocumentFromDocNum (String DocNum, Iri predicateToID, Iri predicateToDocLine) {
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

    public JSONArray getAllPersonIRI() {
        SelectQuery query = Queries.SELECT();
        Variable PersonIRI = SparqlBuilder.var("PersonIRI");
        Variable PersonNameIRI = SparqlBuilder.var("PersonNameIRI");
        Variable PersonNameLiteral = SparqlBuilder.var("PersonName");
        query.where(PersonIRI.has(hasName, PersonNameIRI));
        query.where(PersonNameIRI.has(hasPersonName, PersonNameLiteral));

        return storeClientAsset.executeQuery(query.getQueryString());
    }

    public JSONArray getAllSupplierIRI() {
        Variable SupplierOrgIRI = SparqlBuilder.var("SupplierOrgIRI");
        Variable SupplierNameIRI = SparqlBuilder.var("SupplierNameIRI");
        Variable SupplierNameLiteral = SparqlBuilder.var("SupplierName");
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(isSuppliedBy, SupplierOrgIRI));
        query.where(SupplierOrgIRI.has(hasName, SupplierNameIRI));
        query.where(SupplierNameIRI.has(hasLegalName, SupplierNameLiteral));

        return storeClientPurchDoc.executeQuery(query.getQueryString());

    }
    public JSONArray getAllManufacturerIRI() {
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

    public JSONArray getIRIbyLiteral (String literal, RemoteStoreClient storeClient) {
        //It is assumed that the ID is unique and no duplicate ID exist
        //Cause thats what IDs do (at least supposed to)
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(query.var(), literal));
        return storeClient.executeQuery(query.getQueryString());
    }

    public JSONObject getIRIbyLiteral (String literal, Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, literal));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return reqResult;
        
    }

    public Iri getIRIbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, object));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return iri(reqResult.getString("x01"));
    }

    public JSONArray getIRIListbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, object));
        return storeClient.executeQuery(query.getQueryString());
    }

    public Iri getIRIbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(subject.has(predicate, query.var()));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return iri(reqResult.getString("x01"));
    }

    public JSONArray getIRIListbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.where(subject.has(predicate, query.var()));
        return storeClient.executeQuery(query.getQueryString());
    }

    public JSONArray getIriListByPredicate (Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.where(query.var().has(predicate, query.var()));

        return storeClient.executeQuery(query.getQueryString());
    }
}
