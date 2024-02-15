package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.json.JSONObject;

import com.google.gson.JsonObject;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

import java.rmi.Remote;

public class AssetExistenceChecker {
    private RemoteStoreClient storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab;
    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);
    public AssetExistenceChecker (RemoteStoreClient clientAsset, RemoteStoreClient clientDevice, RemoteStoreClient clientPurchDoc, RemoteStoreClient clientLab) {
        storeClientAsset = clientAsset;
        storeClientOffice = clientDevice;
        storeClientPurchDoc = clientPurchDoc;
        storeClientLab = clientLab;
    }
    
    public JSONObject getPersonTriples(String name){
        return getPersonTriples(name, false);
    }
    public JSONObject getPersonTriples(String name, Boolean generate){
        JSONObject result = new JSONObject();
        JSONArray reqResult = getIRIbyLiteral (name, hasPersonName, storeClientPurchDoc);

        switch (reqResult.length()) {
            case 0:
                if (generate) {
                    //Create IRI and add to result
                    String PersonIRI = genIRIString("Person", P_ASSET);
                    String PersonNameIRI = genIRIString("PersonName", P_ASSET);
                    result.put("PersonIRI", PersonIRI);
                    result.put("PersonNameIRI", PersonNameIRI);
                    return result;
                }
                return null;
                
            case 1:
                //Add the existing IRI to result
                String personNameIRIString = reqResult.getJSONObject(0).getString("Subject");
                result.put("PersonNameIRI", personNameIRIString);
                //Query Person instance from person name
                result.put("PersonIRI", getIRIStringbyIRIObject(iri(personNameIRIString), hasName, storeClientAsset));
                return result;
            default:
                throw new JPSRuntimeException("A person have more than 1 instance: " + name + ". Check the knowledge graph for duplicates.", null);
        }

    }

    public JSONObject getOrganizationTriples (String orgName) {
        return getOrganizationTriples (orgName, false);
    }
    public JSONObject getOrganizationTriples (String orgName, Boolean generate) {
        JSONObject result = new JSONObject();
        JSONArray reqResult = getIRIbyLiteral (orgName, hasLegalName, storeClientPurchDoc);
        String OrgNameIRI, OrgIRI;
        LOGGER.info("Organization check query for name::" + orgName + ": " + reqResult);
        switch (reqResult.length()) {
            case 0:
                if (generate){
                    //Create IRI and add to result
                    OrgNameIRI = genIRIString("OrganizationName", P_ASSET);
                    OrgIRI = genIRIString("FormalOrganization", P_ASSET);
                    result.put("OrgNameIRI", OrgNameIRI);
                    result.put("OrgIRI", OrgIRI);
                    return result;
                }
                return null;
                
            case 1:
                //Add the existing IRI to result
                OrgNameIRI = reqResult.getJSONObject(0).getString("Subject");
                result.put("OrgNameIRI", OrgNameIRI);
                //Query Person instance from person name
                result.put("OrgIRI", getIRIStringbyIRIObject(iri(OrgNameIRI), hasName, storeClientAsset));
                return result;
            default:
                throw new JPSRuntimeException("An organization has more than 1 instance: " + orgName + ". Check the knowledge graph for duplicates.", null);
        }
    }
    public JSONObject getIndependentPartyTriples (String orgName) {
        return getOrganizationTriples (orgName, false);
    }
    public JSONObject getIndependentPartyTriples (String orgName, Boolean generate) {
        JSONObject result = new JSONObject();
        JSONArray reqResult = getIRIbyLiteral(orgName, iri(RDFS.LABEL), storeClientAsset);
        String serviceProviderIRI;
        LOGGER.info("IndependentParty check query for name::" + orgName + ": " + reqResult);
        switch (reqResult.length()) {
            case 0:
                if (generate){
                    //Create IRI and add to result
                    serviceProviderIRI = genIRIString("ServiceProvider", P_ASSET);
                    result.put("ServiceProviderIRI", serviceProviderIRI);
                    return result;
                }
                return null;
                
            case 1:
                //Add the existing IRI to result
                serviceProviderIRI = reqResult.getJSONObject(0).getString("Subject");
                result.put("ServiceProviderIRI", serviceProviderIRI);
                return result;
            default:
                throw new JPSRuntimeException("An independent party has more than 1 instance: " + orgName + ". Check the knowledge graph for duplicates.", null);
        }
    }

    public String getIRIStringbyID (String ID){
        JSONArray reqResult = getIRIbyLiteral (ID, hasItemInventoryIdentifier, storeClientAsset);

        switch (reqResult.length()) {
            case 0:
                return null;
            case 1:
                return reqResult.getJSONObject(0).getString("Subject");
            default:
                throw new JPSRuntimeException("More than 1 asset instance have the same ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }

    }

    public String getIDbyIRIString (String IRI){
        JSONArray reqResult = getLiteralbyIRI (iri(IRI), hasItemInventoryIdentifier, storeClientAsset);
        switch (reqResult.length()) {
            case 0:
                return null;
            case 1:
                return reqResult.getJSONObject(0).getString("object");
            default:
                throw new JPSRuntimeException("More than 1 ID for the same IRI: " + IRI + ". Check the knowledge graph for duplicates.", null);
        }
    }

    public String getWorkspaceIRIStringByName (String name) {
        return getWorkspaceIRIStringByName (name, false); 
    }
    public String getWorkspaceIRIStringByName (String name, Boolean generate) {
        JSONArray reqResult = getIRIbyLiteral(name, hasWorkspaceIdentifier, storeClientAsset);
        LOGGER.info("Workspace query check result for workspace::" + name + ": " + reqResult);
        switch (reqResult.length()) {
            case 0:
                if(generate){
                    //Does not seem right to use Asset prefix here?
                    return genIRIString("Workspace", P_ASSET);
                }
                return null;
                
            case 1:
                return reqResult.getJSONObject(0).getString("Subject");
            default:
                throw new JPSRuntimeException("Workspace has more than 1 instances: " + name + ". Check the knowledge graph for duplicates.", null);
        }
    }

    public JSONObject getLocationTriples (String buildingName, String facilityName, String roomName, RemoteStoreClient storeClient){
        if (buildingName.contains("Research Wing") || buildingName.contains("CREATE Tower")){
            return  queryLocationIRIByName(buildingName, facilityName, roomName, storeClient);
        }
        else{
            //Use String literal for the location as its not registered
            return null;
        }
    }

    public JSONObject queryLocationIRIByName (String buildingName, String facilityName, String roomName, RemoteStoreClient storeClient) {
        JSONObject result = new JSONObject();
        Variable roomIRI = SparqlBuilder.var("roomIRI");
        Variable roomTypeIRI = SparqlBuilder.var("roomTypeIRI");
        Variable IFCReprIRI = SparqlBuilder.var("IFCReprIRI");
        Variable facilityIRI = SparqlBuilder.var("facilityIRI");
        Variable facilityTypeIRI = SparqlBuilder.var ("facilityTypeIRI");
        Variable locationIRI = SparqlBuilder.var("locationIRI");
        Variable locationIFCReprIRI = SparqlBuilder.var("locationIFCReprIRI");

        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(IFCReprIRI.has(RDFS.LABEL, Rdf.literalOf(roomName)));
        query.where(roomIRI.has(hasIfcRepresentation, IFCReprIRI));
        query.where(roomIRI.isA(roomTypeIRI));
        query.where(facilityIRI.has(hasRoom, roomIRI));
        query.where(facilityIRI.isA(facilityTypeIRI));
        if (!(facilityName.isBlank())){
            query.where(facilityIRI.has(RDFS.LABEL, Rdf.literalOf(facilityName)));
        }
        query.where(GraphPatterns.union(
            locationIRI.has(hasFacility, facilityIRI),
            locationIRI.has(hasStorey, facilityIRI)
        ));
        query.where(locationIRI.has(hasIfcRepresentation, locationIFCReprIRI));
        query.where(locationIFCReprIRI.has(RDFS.LABEL, Rdf.literalOf(buildingName)));

        JSONArray reqResult = storeClient.executeQuery(query.getQueryString());
        LOGGER.debug("Location check result::" + reqResult);
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

    public JSONObject queryStorageIRIbyID(String ID, RemoteStoreClient storeClient) {
        //For Asset storing other assets or FH/WFH
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable storageIRI = SparqlBuilder.var("storageIRI");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(GraphPatterns.union(storageIRI.has(RDFS.LABEL, Rdf.literalOf(ID)), storageIRI.has(hasItemInventoryIdentifier, Rdf.literalOf(ID))));
        
        JSONArray reqResult = storeClientAsset.executeQuery(query.getQueryString());
        LOGGER.debug("Storage Asset existence check for ID:"+ ID+ " :" + reqResult);
        switch (reqResult.length()) {
            case 0:
                break;
                
            case 1:
                result.put("storageIRI", reqResult.getJSONObject(0).getString("storageIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Storage has more than 1 IRI for ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }
        //Fumehoods are in lab namespace
        reqResult = storeClient.executeQuery(query.getQueryString());
        LOGGER.debug("Storage Asset existence check for ID:"+ ID+ " :" + reqResult);
        switch (reqResult.length()) {
            case 0:
                throw new JPSRuntimeException("Storage asset does not exist for storage: " + ID  +". Ensure the storage asset is instantiated first.");
                
            case 1:
                result.put("storageIRI", reqResult.getJSONObject(0).getString("storageIRI"));

                return result;
            default:
                throw new JPSRuntimeException("Storage has more than 1 IRI for ID: " + ID + ". Check the knowledge graph for duplicates.", null);
        }
        

    }

    public JSONObject queryStorageFurnitureIRIbyName(String name, RemoteStoreClient storeClient) {
        //FOr cabinets or other furnitures
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable cabinetIRI = SparqlBuilder.var("cabinetIRI");

        query.where(cabinetIRI.has(hasFurnitureIdentifier, Rdf.literalOf(name)));
        JSONArray reqResult = storeClient.executeQuery(query.getQueryString());
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

    private JSONObject queryPricingDetailsIRIbyInvoiceIRI (String POLineIRI){
        return queryPricingDetailsIRIbyInvoiceIRI(iri(POLineIRI));
    }

    private JSONObject queryPricingDetailsIRIbyInvoiceIRI (Iri POLineIRI){
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable PriceDetailsIRI = SparqlBuilder.var("PriceDetailsIRI");
        Variable priceIRI = SparqlBuilder.var("priceIRI");
        Variable MeasureIRI = SparqlBuilder.var("MeasureIRI");
        Variable currencyIRI = SparqlBuilder.var("currencyIRI");
        Variable priceLiteral = SparqlBuilder.var("price");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        
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
                throw new JPSRuntimeException("InvoiceLine has more than 1 price IRI set: " + POLineIRI + ". Check the knowledge graph for duplicates.", null);
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

    public JSONObject getSpecificPurchaseDocsTriples(String docNum, String docType) {
        switch (docType) {
            case "Invoice":
                return queryDocumentFromDocNum(docNum, invoiceNumber, hasInvoiceLine);
            case "DO":
                return queryDocumentFromDocNum(docNum, deliveryOrderNumber, hasDeliveryOrderLine);
            case "PO":
                return queryDocumentFromDocNum(docNum, purchaseOrderNumber, hasPurchaseOrderLine);
            default:
                throw new JPSRuntimeException("DocType unrecognized.", null);
        }
    }


    private JSONObject queryDocumentFromDocNum (String DocNum, Iri predicateToID, Iri predicateToDocLine) {
        JSONObject result = new JSONObject();
        SelectQuery queryInvoice = Queries.SELECT();
        queryInvoice.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable InvoiceIRI = SparqlBuilder.var("InvoiceIRI");
        queryInvoice.where(InvoiceIRI.has(predicateToID, DocNum));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(queryInvoice.getQueryString());
        LOGGER.info("Query document check result for DOCNUM::"+DocNum+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Doc doesn't exist. Make new IRIs
                result.put("InvoiceIRI", "");
                return result;
                
            case 1:
                result.put("InvoiceIRI", reqResult.getJSONObject(0).getString("InvoiceIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Document has more than 1 IRIs: " + DocNum + ". Check the knowledge graph for duplicates.", null);
        }
    }

    public JSONObject getProjectFundingTriples (String projectName) {
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable projectIRI = SparqlBuilder.var("projectIRI");
        Variable researchGrantIRI = SparqlBuilder.var("researchGrantIRI");
        Variable accountIRI = SparqlBuilder.var("accountIRI");
        Variable budgetIRI = SparqlBuilder.var("budgetIRI");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(projectIRI.has(hasProjectIdentifier, projectName));
        query.where(projectIRI.has(hasGrant, researchGrantIRI));
        query.where(researchGrantIRI.has(hasAccount, accountIRI));
        query.where(accountIRI.has(hasBudget, budgetIRI));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(query.getQueryString());
        LOGGER.info("Query project funding info check result for project::"+projectName+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Project doesn't exist. Make new IRIs
                result.put("projectIRI", genIRIString("Project", P_ASSET));
                result.put("researchGrantIRI", genIRIString("ResearchGrant", P_ASSET));
                result.put("accountIRI", genIRIString("Account", P_ASSET));
                result.put("budgetIRI", genIRIString("Budget", P_ASSET));
                return result;
                
            case 1:
                result.put("projectIRI", reqResult.getJSONObject(0).getString("projectIRI"));
                result.put("researchGrantIRI", reqResult.getJSONObject(0).getString("researchGrantIRI"));
                result.put("accountIRI", reqResult.getJSONObject(0).getString("accountIRI"));
                result.put("budgetIRI", reqResult.getJSONObject(0).getString("budgetIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Project has more than 1 IRIs: " + projectName + ". Check the knowledge graph for duplicates.", null);
        }

    }

    public JSONObject getBudgetTriples(String serviceCategory, String serviceCode){
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable budgetCategoryIRI = SparqlBuilder.var("budgetCategoryIRI");
        Variable serviceCategoryIRI = SparqlBuilder.var("serviceCategoryIRI");
        Variable serviceCodeIRI = SparqlBuilder.var("serviceCodeIRI");
        query.where(budgetCategoryIRI.has(hasServiceCategory, serviceCategoryIRI));
        query.where(serviceCategoryIRI.has(hasServiceCategoryIdentifier, serviceCategory));
        query.where(budgetCategoryIRI.has(hasServiceCode, serviceCodeIRI));
        query.where(serviceCodeIRI.has(hasServiceCodeIdentifier, serviceCode));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(query.getQueryString());
        LOGGER.info("Query service category info check result for project::"+serviceCategory+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Project doesn't exist. Make new IRIs
                result.put("budgetCategoryIRI", genIRIString("BudgetCategory", P_ASSET));
                result.put("serviceCategoryIRI", genIRIString("ServiceCategory", P_ASSET));
                result.put("serviceCodeIRI", genIRIString("ServiceCode", P_ASSET));
                return result;
                
            case 1:
                result.put("budgetCategoryIRI", reqResult.getJSONObject(0).getString("budgetCategoryIRI"));
                result.put("serviceCategoryIRI", reqResult.getJSONObject(0).getString("serviceCategoryIRI"));
                result.put("serviceCodeIRI", reqResult.getJSONObject(0).getString("serviceCodeIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Budget has more than 1 IRIs: " + serviceCategory + ". Check the knowledge graph for duplicates.", null);
        }
    }

    //TBox were updated. Kept just in case
    @Deprecated
    public JSONObject getBudgetTriplesDeprecated(String projectName, String serviceCategory, String serviceCode){
         JSONObject result = new JSONObject();
        //Retrieve Budget Category IRI and service category first. 
        //Create service category if doesn't exist
        JSONObject reqResultServiceCat = getServiceCategoryTriples(projectName, serviceCategory);
        String budgetCatIRI = reqResultServiceCat.getString("budgetCatIRI");
        String serviceCategoryIRI = reqResultServiceCat.getString("serviceCategoryIRI");
        //Retrieve service code, if doesn't exist, create new one
        JSONObject reqResultServiceCode = getServiceCodeTriples(budgetCatIRI, serviceCode);
        String serviceCodeIRI = reqResultServiceCode.getString("serviceCodeIRI");

        result.put("budgetCatIRI", budgetCatIRI);
        result.put("serviceCategoryIRI", serviceCategoryIRI);
        result.put("serviceCodeIRI", serviceCodeIRI);

        return result;
    }

    public JSONObject getServiceCategoryTriples(String projectName, String serviceCategory){
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable budgetCategoryIRI = SparqlBuilder.var("budgetCategoryIRI");
        Variable serviceCategoryIRI = SparqlBuilder.var("serviceCategoryIRI");
        Variable projectIRI = SparqlBuilder.var("projectIRI");
        Variable researchGrantIRI = SparqlBuilder.var("researchGrantIRI");
        Variable accountIRI = SparqlBuilder.var("accountIRI");
        Variable budgetIRI = SparqlBuilder.var("budgetIRI");

        query.where(projectIRI.has(hasProjectIdentifier, projectName));
        query.where(projectIRI.has(hasGrant, researchGrantIRI)); 
        query.where(researchGrantIRI.has(hasAccount, accountIRI));
        query.where(accountIRI.has(hasBudget, budgetIRI));
        query.where(budgetIRI.has(hasBudgetCategory, budgetCategoryIRI));
        query.where(budgetCategoryIRI.has(hasServiceCategory, serviceCategoryIRI));
        query.where(serviceCategoryIRI.has(hasServiceCategoryIdentifier, serviceCategory));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(query.getQueryString());
        LOGGER.info("Query service category info check result for project::"+serviceCategory+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Project doesn't exist. Make new IRIs
                result.put("budgetCatIRI", genIRIString("BudgetCategory", P_ASSET));
                result.put("serviceCategoryIRI", genIRIString("ServiceCategory", P_ASSET));
                return result;
                
            case 1:
                result.put("budgetCatIRI", reqResult.getJSONObject(0).getString("budgetCatIRI"));
                result.put("serviceCategoryIRI", reqResult.getJSONObject(0).getString("serviceCategoryIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Service category has more than 1 IRIs: " + serviceCategory + ". Check the knowledge graph for duplicates.", null);
        }

    }

    public JSONObject getServiceCodeTriples(String budgetCategoryIRI, String serviceCode){
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable serviceCodeIRI = SparqlBuilder.var("serviceCodeIRI");
        query.where(iri(budgetCategoryIRI).has(hasServiceCode, serviceCodeIRI));
        query.where(serviceCodeIRI.has(hasServiceCodeIdentifier, serviceCode));

        JSONArray reqResult = storeClientPurchDoc.executeQuery(query.getQueryString());
        LOGGER.info("Query service code info check result for project::"+serviceCode+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Project doesn't exist. Make new IRIs
                result.put("serviceCodeIRI", genIRIString("ServiceCode", P_ASSET));
                return result;
                
            case 1:
                result.put("serviceCodeIRI", reqResult.getJSONObject(0).getString("serviceCodeIRI"));
                return result;
            default:
                throw new JPSRuntimeException("Service code has more than 1 IRIs: " + serviceCode + ". Check the knowledge graph for duplicates.", null);
        }
        
    }

    public JSONObject getDataSheetIRI(String fileLoc, String fileType){
        return getDataSheetIRI(fileLoc, fileType, false);
    }

    public JSONObject getDataSheetIRI(String fileLoc, String fileType, Boolean generate){
        JSONObject result = new JSONObject();
        JSONArray reqResult = getIRIbyLiteral(fileLoc, availableAt, storeClientAsset);
        switch (reqResult.length()) {
            case 0:
                //document doesn't exist. Make new IRIs
                if (generate){
                    result.put("DocIRI", genIRIString(fileType, P_ASSET));
                    return result;
                }
                return null;
                
                
            case 1:
                result.put("DocIRI", reqResult.getJSONObject(0).getString("Subject"));
                return result;
            default:
                //TODO Properly handle multiple cases 
                result.put("DocIRI", reqResult.getJSONObject(0).getString("Subject"));
                return result;
        }
    }

    public JSONObject getMaintenanceIRI (String deviceIRI, Boolean generate){ 
        return getMaintenanceIRI(iri(deviceIRI), generate);
    }

    public JSONObject getMaintenanceIRI (Iri deviceIRI, Boolean generate){
        JSONObject result = new JSONObject();
        SelectQuery query = Queries.SELECT();
        Variable maintenanceScheduleIRI = SparqlBuilder.var("maintenanceScheduleIRI");
        Variable maintenanceTaskIRI = SparqlBuilder.var("maintenanceTaskIRI");
        Variable lastServiceIRI = SparqlBuilder.var("lastServiceIRI");
        Variable nextServiceIRI = SparqlBuilder.var("nextServiceIRI");
        Variable intervalIRI = SparqlBuilder.var("intervalIRI");
        Variable durationIRI = SparqlBuilder.var("durationIRI");
        Variable performerIRI = SparqlBuilder.var("performerIRI");
        Variable performerName = SparqlBuilder.var("performerName");

        Variable orgNameIRI = SparqlBuilder.var("orgNameIRI"); 
        Variable personNameIRI = SparqlBuilder.var("personNameIRI");

        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, 
            Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE,
            Pref_TIME
        );
        query.where(deviceIRI.has(hasMaintenanceSchedule, maintenanceScheduleIRI));
        query.where(maintenanceScheduleIRI.has(hasTask, maintenanceTaskIRI));
        query.where(maintenanceTaskIRI.has(isPerformedBy, performerIRI));
        query.where(GraphPatterns.optional(
            performerIRI.has(hasName, orgNameIRI),
            orgNameIRI.has(hasLegalName, performerName)
            ));
        query.where(GraphPatterns.optional(
            performerIRI.has(hasName, personNameIRI),
            personNameIRI.has(hasPersonName, performerName)
            ));
        query.where(GraphPatterns.optional(
            performerIRI.has(RDFS.LABEL, performerName)
            ));
        query.where(GraphPatterns.optional(maintenanceTaskIRI.has(performedAt, lastServiceIRI)));
        query.where(GraphPatterns.optional(maintenanceTaskIRI.has(scheduledFor, nextServiceIRI)));
        query.where(GraphPatterns.optional(
            maintenanceTaskIRI.has(hasInterval, intervalIRI),
            intervalIRI.has(hasDurationDescription, durationIRI)
            ));

        LOGGER.debug("Maintenance existence check query:: " + query.getQueryString());

        JSONArray reqResult = storeClientAsset.executeQuery(query.getQueryString());
        LOGGER.debug("Maintenance existence check result:: " + reqResult);
        //TODO Figure out what to do with multiple maintenance schedule. Currently multiple is allowed
        //Hence this dumb switch statement below
        //switch (reqResult.length()) {
        switch (0) {
            case 0:
                if (generate){
                    result.put("maintenanceScheduleIRI", genIRIString("MaintenanceSchedule", P_ASSET));
                    result.put("maintenanceTaskIRI", genIRIString("MaintenanceTask", P_ASSET));
                    result.put("lastServiceIRI", genIRIString("ServiceTime", P_TIME));
                    result.put("nextServiceIRI", genIRIString("ServiceTime", P_TIME));
                    result.put("intervalIRI", genIRIString("Interval", P_TIME));
                    result.put("durationIRI", genIRIString("DurationDescription", P_TIME));
                    //Performer IRI is not generated as it is supposed to be updated as requested
                    //Hence it does not matter if its retrieved or not
                    result.put("performerIRI", "");
                    result.put("performerName", "");
                    return result;
                }
                return null;
                
            case 1:
                result.put("maintenanceScheduleIRI", reqResult.getJSONObject(0).getString("maintenanceScheduleIRI"));
                result.put("maintenanceTaskIRI", reqResult.getJSONObject(0).getString("maintenanceTaskIRI"));
                result.put("performerIRI", reqResult.getJSONObject(0).getString("performerIRI"));
                result.put("performerName", reqResult.getJSONObject(0).getString("performerName"));
                
                //In case some of the maintenance component does not exist before, create new IRI here:
                if (reqResult.getJSONObject(0).has("lastServiceIRI")){
                    result.put("lastServiceIRI", reqResult.getJSONObject(0).getString("lastServiceIRI"));
                }
                else{
                    if (generate){
                        result.put("lastServiceIRI", genIRIString("ServiceTime", P_TIME));
                    }
                }

                if (reqResult.getJSONObject(0).has("nextServiceIRI")){
                    result.put("nextServiceIRI", reqResult.getJSONObject(0).getString("nextServiceIRI"));
                }
                else{
                    if (generate){
                        result.put("nextServiceIRI", genIRIString("ServiceTime", P_TIME));
                    }
                }

                if (reqResult.getJSONObject(0).has("intervalIRI")){
                    result.put("intervalIRI", reqResult.getJSONObject(0).getString("intervalIRI"));
                    result.put("durationIRI", reqResult.getJSONObject(0).getString("durationIRI"));
                }
                else{
                    if (generate){
                        result.put("intervalIRI", genIRIString("Interval", P_TIME));
                        result.put("durationIRI", genIRIString("DurationDescription", P_TIME));
                    }
                }
                

                return result;
            default:
                throw new JPSRuntimeException("Multiple maintenance data found. Currently multiple maintenance data is not supported.");
        }
    }

    public RemoteStoreClient getNameSpaceByID (String ID){
        JSONArray reqRes = getIRIbyLiteral(ID, hasItemInventoryIdentifier, storeClientLab);
        if (reqRes.length() == 0){
            reqRes = getIRIbyLiteral(ID, hasItemInventoryIdentifier, storeClientOffice);
            if (reqRes.length() == 0){
                throw new JPSRuntimeException("Failed to retrieve the proper remote store client");
            }
            return storeClientOffice;
        }
        return storeClientLab;
    }

    public String getLabelbyIRIString(String IRI){
        JSONArray reqResult = getLiteralbyIRI(iri(IRI), iri(RDFS.LABEL), storeClientLab);
        LOGGER.info("Query label::"+IRI+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Asset doesn't exist on bms, office and lab namespace
                return null;
                
            case 1:
                return reqResult.getJSONObject(0).getString("object");
            default:
                throw new JPSRuntimeException("Asset IRI is connected to several ID: " + IRI + ". Check the knowledge graph for duplicates.", null);
        }
    }

    public String getIRIbyLabelString(String ID){
        JSONArray reqResult = getIRIbyLiteral(ID, iri(RDFS.LABEL), storeClientLab);
        LOGGER.info("Query label::"+ID+" : "+reqResult);
        switch (reqResult.length()) {
            case 0:
                //Asset doesn't exist on bms, office and lab namespace
                return null;
            case 1:
                return reqResult.getJSONObject(0).getString("Subject");
            default:
                throw new JPSRuntimeException("Asset ID is connected to several IRI: " + ID + ". Check the knowledge graph for duplicates.", null);
        }
    }

}
