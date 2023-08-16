package uk.ac.cam.cares.jps.agent.assetmanager;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.jooq.False;

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
import com.martiansoftware.jsap.Switch;

import java.rmi.Remote;
import java.util.*;

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

    //properties
    private static final Iri consistsOf = P_SAREF.iri("consistsOf");
    private static final Iri hasModel = P_SAREF.iri("hasModel");

    private static final Iri hasItemInventoryIdentifier = P_ASSET.iri("hasItemInventoryIdentifier");
    private static final Iri references = P_ASSET.iri("references");
    private static final Iri assignedTo = P_ASSET.iri("assignedTo");
    private static final Iri suppliedBy = P_ASSET.iri("suppliedBy");
    private static final Iri serialNumber = P_ASSET.iri("serialNumber");
    private static final Iri isStoredIn = P_ASSET.iri("isStoredIn");
    private static final Iri availableAt = P_ASSET.iri("availableAt");
    private static final Iri isSuppliedBy = P_ASSET.iri("isSuppliedBy");
    private static final Iri isManufacturedBy = P_ASSET.iri("isManufacturedBy");

    private static final Iri hasDataSheet = P_DEV.iri("hasDataSheet");
    private static final Iri hasPrice = P_DEV.iri("hasPrice");
     
    private static final Iri hasValue = P_OM.iri("hasValue");
    private static final Iri hasUnit = P_OM.iri("hasUnit");
    private static final Iri hasNumericalValue = P_OM.iri("hasNumericalValue");

    private static final Iri hasGivenName = P_FIBO_AAP.iri("People/hasGivenName");
    private static final Iri hasLegalName = iri("https://spec.edmcouncil.org/fibo/ontology/FND/Relations/Relations/hasLegalName");

    private static final Iri hasName = iri("https://www.omg.org/spec/Commons/Designators/hasName");


    //External classes
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

        return result;
    }


    private JSONObject retrieveAssetNamespace(Iri ID) {

        //Asset namespace query
        SelectQuery query = Queries.SELECT();
        Variable deviceIRIVar = SparqlBuilder.var("deviceVar");
        Variable itemIRIVar = SparqlBuilder.var("itemVar");
        Variable deviceTypeIRIVar = SparqlBuilder.var("deviceType");
        Variable labelLiteralVar = SparqlBuilder.var("label");
        Variable deviceOwnerVar = SparqlBuilder.var("assignedTo");
        Variable supplierIRIVar = SparqlBuilder.var("suppliedBy");
        Variable serialNumberIRI = SparqlBuilder.var("serialNum");
        Variable modelNumber = SparqlBuilder.var("modelNumber");
        Variable manualURL = SparqlBuilder.var("manualURL");
        Variable amountOfMoneyVar = SparqlBuilder.var("amtMoney");
        Variable priceMeasure = SparqlBuilder.var("priceMeasure");
        Variable priceLiteralVar = SparqlBuilder.var("price");
        Variable priceUnitIRIVar = SparqlBuilder.var("priceUnitIRI");
        Variable storageIRI = SparqlBuilder.var("storage");
        Variable SpecSheetIRI = SparqlBuilder.var("SpecSheetIRI"); //also includes manual
        Variable SpecSheetFileLiteral = SparqlBuilder.var("SpecSheet"); //also includes manual
        Variable SpecSheetPageLiteral = SparqlBuilder.var("SpecSheetPage");
        Variable SpecSheetTypeIRI = SparqlBuilder.var("SpecSheetType");//Should be between manual and specsheet
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
        query.where(deviceIRIVar.has(assignedTo, deviceOwnerVar));
        //Device supplier
        query.where(deviceIRIVar.has(suppliedBy, supplierIRIVar));
        //Optional IRIs
        //Serial number
        query.where(GraphPatterns.optional(deviceIRIVar.has(serialNumber, serialNumberIRI)));
        //model number
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasModel, modelNumber)));
        //manual URL
        query.where(GraphPatterns.optional(deviceIRIVar.has(RDFS.SEEALSO, manualURL)));
        //Price
        query.where(GraphPatterns.optional(deviceIRIVar.has(hasPrice, amountOfMoneyVar)));
        query.where(GraphPatterns.optional(amountOfMoneyVar.has(hasValue, priceMeasure)));
        query.where(GraphPatterns.optional(priceMeasure.has(hasNumericalValue, priceLiteralVar)));
        query.where(GraphPatterns.optional(priceMeasure.has(hasUnit, priceUnitIRIVar)));
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
    
}