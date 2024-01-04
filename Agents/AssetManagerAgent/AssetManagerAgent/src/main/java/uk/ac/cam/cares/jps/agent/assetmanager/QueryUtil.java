package uk.ac.cam.cares.jps.agent.assetmanager;
import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class QueryUtil {
    public static Prefix getPrefixFromString (String ontology) throws Exception {
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

            case "ontoems":
                return Pref_EMS;
            
            default:
                throw new Exception("Unrecognized ontology: " +  ontology, null);
        }
    }

    public static String getPrefixStringFromName(String ontology) throws Exception {
        switch (ontology.strip()) {
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

            case "ontoems":
                return P_EMS;
            
            default:
                throw new Exception("Unrecognized ontology: " +  ontology, null);
        }
    }

    public static JSONArray getIRIbyLiteral (String literal, RemoteStoreClient storeClient) {
        //It is assumed that the ID is unique and no duplicate ID exist
        //Cause thats what IDs do (at least supposed to)
        SelectQuery query = Queries.SELECT();
        Variable subject = SparqlBuilder.var("Subject");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(query.var(), literal));
        return storeClient.executeQuery(query.getQueryString());
    }

    public static JSONArray getIRIbyLiteral (String literal, Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        Variable subject = SparqlBuilder.var("Subject");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(predicate, literal));
        JSONArray reqResult = storeClient.executeQuery(query.getQueryString());
        return reqResult;
        
    }

    public static JSONArray getLiteralbyIRI (Iri subject, RemoteStoreClient storeClient) {
        //It is assumed that the ID is unique and no duplicate ID exist
        //Cause thats what IDs do (at least supposed to)
        SelectQuery query = Queries.SELECT();
        Variable object = SparqlBuilder.var("object");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(query.var(), object));
        return storeClient.executeQuery(query.getQueryString());
    }

    public static JSONArray getLiteralbyIRI (Iri subject, Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        Variable object = SparqlBuilder.var("object");
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(predicate, object));
        JSONArray reqResult = storeClient.executeQuery(query.getQueryString());
        return reqResult;
        
    }

    public static String getIRIStringbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable subject = SparqlBuilder.var("Subject");
        query.where(subject.has(predicate, object));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return reqResult.getString("Subject");
    }

    public static JSONArray getIRIListbyIRIObject (Iri object, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(query.var().has(predicate, object));
        return storeClient.executeQuery(query.getQueryString());
    }

    public static String getIRIStringbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(predicate, query.var()));
        JSONObject reqResult = storeClient.executeQuery(query.getQueryString()).getJSONObject(0);
        return reqResult.getString("x0");
    }

    public static JSONArray getIRIListbyIRISubject (Iri subject, Iri predicate, RemoteStoreClient storeClient){
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.where(subject.has(predicate, query.var()));
        return storeClient.executeQuery(query.getQueryString());
    }

    public static JSONArray getIriListByPredicate (Iri predicate, RemoteStoreClient storeClient) {
        SelectQuery query = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
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
    public static Iri genIRI (String ID, Prefix prefix) {
        return prefix.iri(ID + "_" + UUID.randomUUID());
    }

    public static Iri genIRI (String ID, String prefix) {
        return iri(genIRIString(ID, prefix));
    }

    public static String genIRIString (String ID, Prefix prefix) {
        return genIRI(ID, prefix).getQueryString();
    }
    public static String genIRIString (String ID, String prefix) {
        return prefix + ID + "_" + UUID.randomUUID();
    }

    public QueryUtil () {

    }

    public static boolean isIRI(String input) {
        try {
            // Attempt to create a URI from the input string
            new URI(input);

            // If the URI is created without throwing an exception,
            // it is a valid IRI.
            return true;
        } catch (URISyntaxException e) {
            // If a URISyntaxException is thrown, the input is not a valid IRI.
            return false;
        }
    }

}
