package uk.ac.cam.cares.jps.agent.fumehood;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.io.IOException;
import java.util.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;


/**
 * Class to construct queries to retrieve IRIs and information from the triple store.
 * @author  */
public class QueryStore {
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(FHSashAndOccupancyAgent.class);

    /**
     * Log messages
     */
    private static final String GETFHANDWFHDEVICES_ERROR_MSG = "Unable to query for fumehood and/or walkin-fumehood devices and their labels!";
    private static final String GETOCCUPANCYSTATE_ERROR_MSG = "Unable to query for occupancy state for the following device IRI:";
    private static final String GETSASHOPENING_ERROR_MSG = "Unable to query for sash opening percentage for the following device IRI:";
    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
	public static final String ONTOBMS_NS = "https://www.theworldavatar.com/kg/ontobms/";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    public static final String OM_NS = "http://www.ontology-of-units-of-measure.org/resource/om-2/"; 

	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
	private static final Prefix PREFIX_ONTOBMS = SparqlBuilder.prefix("ontobms", iri(ONTOBMS_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix("om", iri(OM_NS));
    
	/**
     * Relationships
     */ 
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasSashOpenPercentage = PREFIX_ONTOBMS.iri("hasSashOpenPercentage");
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");

    /**
     * Classes
     */
    private static final Iri OccupiedState = PREFIX_ONTODEVICE.iri("OccupiedState");
    private static final Iri Fumehood = PREFIX_ONTOBMS.iri("FumeHood");
    private static final Iri WalkinFumehood = PREFIX_ONTOBMS.iri("WalkInFumeHood");
    private static final Iri Percentage = PREFIX_OM.iri("Percentage");

    RemoteStoreClient kbClient;

    /**
     * Standard constructor
     * @param updateEndpoint The sparql update endpoint
     * @param queryEndpoint The sparql query endpoint
     * @param username username for blazegraph authentication
     * @param password password for blazegraph authentication
     */
    public QueryStore(String updateEndpoint, String queryEndpoint, String username, String password) throws IOException {
        kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(updateEndpoint);
        kbClient.setQueryEndpoint(queryEndpoint);
        kbClient.setUser(username);
        kbClient.setPassword(password);
    }

    /*
     * Query for all instances with rdf:type ontobms:FumeHood and ontobms:WalkInFumeHood
     */
    public Map<String, List<String>> queryForFHandWFHDevices() {
        Map<String, List<String>> map = new HashMap<>();
        map.put("FHandWFH", new ArrayList<>());
        map.put("Label", new ArrayList<>());
        Variable FHandWFHVar = SparqlBuilder.var("FHandWFHVar");
        Variable Label = SparqlBuilder.var("Label");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = FHandWFHVar.isA(Fumehood);
        TriplePattern queryPattern2 = FHandWFHVar.has(label, Label);
        TriplePattern queryPattern3 = FHandWFHVar.isA(WalkinFumehood);

        /*
         * SELECT ?FHandWFHVar ?Label WHERE { {
            ?FHandWFHVar rdf:type	<https://www.theworldavatar.com/kg/ontobms/FumeHood> .
            ?FHandWFHVar rdfs:label ?Label . } UNION
            { ?FHandWFHVar rdf:type	<https://www.theworldavatar.com/kg/ontobms/WalkInFumeHood> .
              ?FHandWFHVar rdfs:label ?Label .}}
         */
        query.prefix(PREFIX_ONTOBMS, PREFIX_RDFS).select(FHandWFHVar,Label).where(queryPattern.and(queryPattern2).union(queryPattern3.and(queryPattern2)));
        kbClient.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info(kbClient.executeQuery().getJSONObject(0));
                for (int i = 0; i < queryResult.length(); i++) {
                map.get("Label").add(queryResult.getJSONObject(i).getString("Label"));
                map.get("FHandWFH").add(queryResult.getJSONObject(i).getString("FHandWFHVar"));
                }
            }
        } catch (Exception e){
            throw new JPSRuntimeException(GETFHANDWFHDEVICES_ERROR_MSG);
        }
        return map;
    }
    
    //SELECT ?OccupancyState WHERE { <IRIString> saref:hasState ?OccupancyState ;
    //                               ?OccupanyState rdf:type ontodevice:OccupancyState }
    /*
     * Query for the occupied state IRIs of all instances with rdf:type ontobms:FumeHood and ontobms:WalkInFumeHood
     */
    public String queryForOccupancyState(String IRI) {
        String result = null;
        Variable occupancyState = SparqlBuilder.var("OccupancyState");
        SelectQuery query = Queries.SELECT();
        TriplePattern queryPattern = iri(IRI).has(hasState, occupancyState);
        TriplePattern queryPattern2 = occupancyState.isA(OccupiedState);
        query.prefix(PREFIX_ONTODEVICE, PREFIX_SAREF).select(occupancyState).where(queryPattern,queryPattern2);
        kbClient.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info(kbClient.executeQuery().getJSONObject(0));
                result = kbClient.executeQuery().getJSONObject(0).getString("OccupancyState");      
            } else {
                result = "This device does not have a occupied state.";
            }
        } catch (Exception e) {
            result = "This device does not have a occupied state.";
        }
        return result;
    }

    /*
     * Query for the sash opening IRIs  of all instances with rdf:type ontobms:FumeHood and ontobms:WalkInFumeHood
     */
    public String queryForSashOpening(String IRI) {
        String result = null;

        SelectQuery query = Queries.SELECT();
        Variable SashOpeningPercentage = SparqlBuilder.var("SashOpeningPercentage");
        Variable SashOpeningMeasure = SparqlBuilder.var("SashOpeningMeasure");

        TriplePattern queryPattern = iri(IRI).has(iri("https://www.theworldavatar.com/kg/ontobms/hasSashOpenPercentage"), SashOpeningPercentage);
        TriplePattern queryPattern2 = SashOpeningPercentage.isA(Percentage);
        TriplePattern queryPattern3 = SashOpeningPercentage.has(hasValue, SashOpeningMeasure);
        query.prefix(PREFIX_OM).select(SashOpeningMeasure).where(queryPattern,queryPattern2,queryPattern3);
        kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                if(!queryResult.isEmpty()){
                    LOGGER.info(kbClient.executeQuery().getJSONObject(0));
                    result = kbClient.executeQuery().getJSONObject(0).getString("SashOpeningMeasure");
                }
                else {
                    result = "This device does not have a Sash Opening Percentage.";
                }
            } catch (Exception e){
                result = "This device does not have a Sash Opening Percentage.";
            }
            return result;
        }
    }

