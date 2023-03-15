package uk.ac.cam.cares.jps.agent.esphome;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;


/**
 * Class to construct queries to retrieve IRIs and information from the triple store.
 * @author  */
public class QueryBuilder {
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(ESPHomeAgent.class);

    /**
     * The store client to interact with the knowledge graph
     */
    RemoteStoreClient kbClient = new RemoteStoreClient();

    /**
     * Log messages
     */
    private static final String GETDEVICE_ERROR_MSG = "Unable to query for device IRI via saref:hasState!" ;
    private static final String GETSETPOINT_ERROR_MSG = "Unable to query for setpoint IRI via ontodevice:hasSetpoint!" ;
    private static final String GETQUANTITY_ERROR_MSG = "Unable to query for quantity IRI via ontodevice:hasQuantity!" ;
    private static final String GETMEASURE_ERROR_MSG = "Unable to query for measure IRI via om:hasValue!" ;
    private static final String GETNUMERICALVALUE_ERROR_MSG = "Unable to query for numerical value via om:hasNumericalValue!" ;

    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/";
    
	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));

	/**
     * Relationships
     */ 
	private static final Iri hasSetpoint = PREFIX_ONTODEVICE.iri("hasSetpoint");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasQuantity = PREFIX_ONTODEVICE.iri("hasQuantity");
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");
    private static final Iri hasNumericalValue = PREFIX_OM.iri("hasNumericalValue");

    /**
     * Standard constructor
     * @param filepath1 The filepath that contains the sparql endpoints of where the status data IRI is located in.
     */
    public QueryBuilder(String filepath1) throws IOException {
        // Check whether properties file exists at specified location of filepath1
        File file = new File(filepath1);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath1);
        }
        
        try (InputStream input = new FileInputStream(file)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            if (prop.containsKey("sparql.query.endpoint")) {
                this.kbClient.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                this.kbClient.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
        }
    }
    
    //SELECT ?device WHERE { ?device saref:hasState <IRIString> }
    public String queryForDeviceWithStateIRI(String IRIString) {
         String result = null;
         Variable device = SparqlBuilder.var("device");
         SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = device.has(hasState, iri(IRIString));
        query.prefix(PREFIX_ONTODEVICE, PREFIX_SAREF).select(device).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient.executeQuery().getJSONObject(0));
            result = kbClient.executeQuery().getJSONObject(0).getString("device");
        }
    } catch (Exception e){
        throw new JPSRuntimeException(GETDEVICE_ERROR_MSG);
    }
    return result;
    }
    
    //SELECT ?setpoint WHERE { <IRIString> ontodevice:hasSetpoint ?setpoint }
    public String queryForSetpointWithHasSetpoint(String IRIString) {
        String result = null;
        Variable setpoint = SparqlBuilder.var("setpoint");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasSetpoint, setpoint);
        query.prefix(PREFIX_ONTODEVICE).select(setpoint).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient.executeQuery().getJSONObject(0));
            result = kbClient.executeQuery().getJSONObject(0).getString("setpoint");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETSETPOINT_ERROR_MSG);
    }
    return result;
    }

    //SELECT ?quantity WHERE { <IRIString> ontodevice:hasQuantity ?quantity }
    public String queryForQuantityWithHasQuantity(String IRIString) {
        String result = null;
        Variable quantity = SparqlBuilder.var("quantity");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasQuantity, quantity);
        query.prefix(PREFIX_ONTODEVICE).select(quantity).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient.executeQuery().getJSONObject(0));
            result = kbClient.executeQuery().getJSONObject(0).getString("quantity");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETQUANTITY_ERROR_MSG);
    }
    return result;
    }

    //SELECT ?measure WHERE { <IRIString> om:hasValue ?measure }
    public String queryForMeasureWithHasValue(String IRIString) {
        String result = null;
        Variable measure = SparqlBuilder.var("measure");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasValue, measure);
        query.prefix(PREFIX_OM).select(measure).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient.executeQuery().getJSONObject(0));
            result = kbClient.executeQuery().getJSONObject(0).getString("measure");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETMEASURE_ERROR_MSG);
    }
    return result;
    }

    //SELECT ?numericalValue WHERE { <IRIString> om:hasNumericalValue ?numericalValue }
    public Double queryForNumericalValueWithHasNumericalValue(String IRIString) {
        Double result = null;
        Variable numericalValue = SparqlBuilder.var("numericalValue");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasNumericalValue, numericalValue);
        query.prefix(PREFIX_OM).select(numericalValue).where(queryPattern);
        kbClient.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient.executeQuery().getJSONObject(0));
            result = kbClient.executeQuery().getJSONObject(0).getDouble("numericalValue");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETNUMERICALVALUE_ERROR_MSG);
    }
    return result;
    }
}

