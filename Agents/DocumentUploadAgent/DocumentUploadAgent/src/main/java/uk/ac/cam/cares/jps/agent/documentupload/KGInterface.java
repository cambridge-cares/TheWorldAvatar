package uk.ac.cam.cares.jps.agent.documentupload;


import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static uk.ac.cam.cares.jps.agent.documentupload.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.documentupload.QueryUtil.genIRIString;

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
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.json.JSONArray;
import org.json.JSONObject;

import java.rmi.Remote;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Class for creating queries and instantiating asset triples
 */
public class KGInterface {
    private String endpoint;
    private RemoteStoreClient storeClient;

    private String user = null;
    private String pass = null;


    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(DocumentUploadAgent.class);

    //constructor
    public KGInterface(String kgEndpoint) {
        this(kgEndpoint, null, null);
    }

    public KGInterface(String kgEndpoint, String username, String password) {
        endpoint = kgEndpoint;

        storeClient = new RemoteStoreClient(kgEndpoint, kgEndpoint);
        user = username;
        pass = password;
        if (user!=null && pass != null){
            storeClient.setUser(username);
            storeClient.setPassword(password);
        }
    }

    public String addDocument(String docFilename, String docType){
        String documentIRI = genIRIString("Document", P_FOAF);
        return addDocument(documentIRI, docFilename, docType);
    }

    public String addDocument(String documentIRI, String docFilename, String docType) {
        if (docType.isBlank() || docType==null){
            docType = DocumentString;
        }
        
        ModifyQuery query = Queries.MODIFY();
        query.prefix(Pref_ASSET, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        query.insert(iri(documentIRI).isA(iri(docType)));
        query.insert(iri(documentIRI).has(availableAt, docFilename));
        storeClient.executeUpdate(query.getQueryString());
        
        return documentIRI;
    }



}