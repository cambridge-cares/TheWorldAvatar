package uk.ac.cam.cares.jps.agent.assetmanager;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

public class QueryUtil {
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
    
}
