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
    private static final Iri OccupiedState = PREFIX_ONTOBMS.iri("OccupiedState");
    private static final Iri Fumehood = PREFIX_ONTOBMS.iri("FumeHood");
    private static final Iri WalkinFumehood = PREFIX_ONTOBMS.iri("WalkInFumeHood");

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

    public Map<String, List<String>> queryForFHandWFHDevices() {
        Map<String, List<String>> map = new HashMap<>();
        map.put("FHandWFH", new ArrayList<>());
        map.put("Label", new ArrayList<>());
        Variable FHandWFHVar = SparqlBuilder.var("FHandWFHVar");
        Variable Label = SparqlBuilder.var("Label");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = FHandWFHVar.isA(Fumehood);
        TriplePattern queryPattern2 = GraphPatterns.tp(FHandWFHVar, label, Label);
        TriplePattern queryPattern3 = FHandWFHVar.isA(WalkinFumehood);

        /*
         * SELECT ?FHandWFHVar ?Label WHERE { {
            ?FHandWFHVar rdf:type	<https://www.theworldavatar.com/kg/ontobms/FumeHood> .
            ?FHandWFHVar rdfs:label ?Label . } UNION
            { ?FHandWFHVar rdf:type	<https://www.theworldavatar.com/kg/ontobms/WalkInFumeHood> .
              ?FHandWFHVar rdfs:label ?Label .}}
         */
        query.prefix(PREFIX_ONTOBMS).select(FHandWFHVar,Label).where(queryPattern.and(queryPattern2).union(queryPattern3.and(queryPattern2)));
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
    public Map<String, List<String>> queryForOccupancyState(Map<String, List<String>> map) {
        String result = null;
        map.put("OccupancyIRIs", new ArrayList<>());
        Variable occupancyState = SparqlBuilder.var("OccupancyState");
        SelectQuery query = Queries.SELECT();
        for (int i = 0; i < map.get("FHandWFH").size(); i++){
            TriplePattern queryPattern = iri(map.get("FHandWFH").get(i)).has(hasState, occupancyState);
            TriplePattern queryPattern2 = occupancyState.isA(OccupiedState);
            query.prefix(PREFIX_ONTODEVICE, PREFIX_SAREF).select(occupancyState).where(queryPattern,queryPattern2);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                if(!queryResult.isEmpty()){
                    LOGGER.info(kbClient.executeQuery().getJSONObject(0));
                    result = kbClient.executeQuery().getJSONObject(0).getString("OccupancyState");
                    map.get("OccupancyIRIs").add(result);
                }
                else {
                    map.get("OccupancyIRIs").add("This device does not have an occupied state.");
                }
            } catch (Exception e){
                throw new JPSRuntimeException(GETOCCUPANCYSTATE_ERROR_MSG + map.get("FHandWFH").get(i));
            }
        }
        return map;
    }

    //SELECT ?label WHERE {<IRIString> rdfs:label ?label}
    //SELECT ?comment WHERE {<IRIString> rdfs:comment ?comment}
    public Map<String, List<String>> queryForSashOpening(Map<String, List<String>> map) {
        map.put("SashOpeningIRIs", new ArrayList<>());
        SelectQuery query = Queries.SELECT();
        String result = null;
        Variable SashOpeningPercentage = SparqlBuilder.var("SashOpeningPercentage");
        Variable SashOpeningMeasure = SparqlBuilder.var("SashOpeningMeasure");

        for (int i = 0; i < map.get("FHandWFH").size(); i++){
            TriplePattern queryPattern = iri(map.get("FHandWFH").get(i)).has(hasSashOpenPercentage, SashOpeningPercentage);
            TriplePattern queryPattern2 = SashOpeningPercentage.has(hasValue, SashOpeningMeasure);
            query.prefix(PREFIX_ONTOBMS, PREFIX_OM).select(SashOpeningMeasure).where(queryPattern,queryPattern2);
            kbClient.setQuery(query.getQueryString());
            try {
                JSONArray queryResult = kbClient.executeQuery();
                if(!queryResult.isEmpty()){
                    LOGGER.info(kbClient.executeQuery().getJSONObject(0));
                    result = kbClient.executeQuery().getJSONObject(0).getString("SashOpeningMeasure");
                    map.get("SashOpeningIRIs").add(result);
                }
                else {
                    map.get("SashOpeningIRIs").add("This device does not have a Sash Opening Percentage.");
                }
            } catch (Exception e){
                throw new JPSRuntimeException(GETSASHOPENING_ERROR_MSG + map.get("FHandWFH").get(i));
            }
        }
        return map;
    }
}

