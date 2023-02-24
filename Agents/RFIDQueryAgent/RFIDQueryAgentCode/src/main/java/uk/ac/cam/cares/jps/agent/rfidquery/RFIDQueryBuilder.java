package uk.ac.cam.cares.jps.agent.rfidquery;

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
 * Class to query for chemical species information.
 * @author  */
public class RFIDQueryBuilder {
	/**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(RFIDQueryAgentLauncher.class);

    /**
     * The store client to interact with the knowledge graph
     */
    RemoteStoreClient kbClient1 = new RemoteStoreClient();
    RemoteStoreClient kbClient2 = new RemoteStoreClient();

    /**
     * Log messages
     */
    private static final String GETSTATE_ERROR_MSG = "Unable to query for state IRI!" ;
    private static final String GETTAG_ERROR_MSG = "Unable to query for tag IRI!" ;
    private static final String GETBOTTLE_ERROR_MSG = "Unable to query for bottle IRI!" ;
    private static final String GETCHEMICALAMOUNT_ERROR_MSG = "Unable to query for chemical amount IRI!" ;
    private static final String GETCHEMICAL_ERROR_MSG = "Unable to query for chemical IRI!" ;
    private static final String GETPHASE_ERROR_MSG = "Unable to query for phase IRI!" ;
    private static final String GETPHASECOMPONENT_ERROR_MSG = "Unable to query for phase component IRI!" ;
    private static final String GETSPECIES_ERROR_MSG = "Unable to query for species IRI!" ;
    private static final String GETMOLECULARFORMULA_ERROR_MSG = "Unable to query for molecular formula IRI!" ;
    private static final String GETELEMENTNUMBER_ERROR_MSG = "Unable to query for element number IRI!" ;
    private static final String GETNUMBEROFELEMENT_ERROR_MSG = "Unable to query for number of elements!" ;
    private static final String GETLABEL_ERROR_MSG = "Unable to query for label via rdfs:label!";
    private static final String GETGHSHAZARDSTATEMENTS_ERROR_MSG = "Unable to query for GHS Hazard Statements!";
    private static final String GETSTATEMENTLABELANDCOMMENT_ERROR_MSG = "Unable to query for GHS Hazard Statements labels and comments!";
    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
	public static final String ONTOLAB_NS = "https://www.theworldavatar.com/kg/ontolab/";
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";
    public static final String ONTOSPECIES_NS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String SAREF_NS = "https://saref.etsi.org/core/";
    
	/**
     * Prefixes
     */ 
	private static final Prefix PREFIX_ONTODEVICE = SparqlBuilder.prefix("ontodevice", iri(ONTODEVICE_NS));
	private static final Prefix PREFIX_ONTOLAB = SparqlBuilder.prefix("ontolab", iri(ONTOLAB_NS));
	private static final Prefix PREFIX_ONTOCAPE_CPS_BEHAVIOR = SparqlBuilder.prefix("ontocape_cps_behavior", iri(ONTOCAPE_CPS_BEHAVIOR_NS));
    private static final Prefix PREFIX_ONTOCAPE_PHASE_SYSTEM = SparqlBuilder.prefix("ontocape_cps_phase_system", iri(ONTOCAPE_PHASE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOCAPE_MATERIAL = SparqlBuilder.prefix("ontocape_material", iri(ONTOCAPE_MATERIAL_NS));
    private static final Prefix PREFIX_ONTOCAPE_SYSTEM = SparqlBuilder.prefix("ontocape_system", iri(ONTOCAPE_SYSTEM_NS));
    private static final Prefix PREFIX_ONTOSPECIES = SparqlBuilder.prefix("ontospecies", iri(ONTOSPECIES_NS));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));
    private static final Prefix PREFIX_SAREF = SparqlBuilder.prefix("saref", iri(SAREF_NS));
    
	/**
     * Relationships
     */ 
	private static final Iri isAttachedTo = PREFIX_ONTODEVICE.iri("isAttachedTo");
	private static final Iri isFilledWith = PREFIX_ONTOLAB.iri("isFilledWith");
	private static final Iri refersToMaterial = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("refersToMaterial");
	private static final Iri thermodynamicBehavior = PREFIX_ONTOCAPE_MATERIAL.iri("thermodynamicBehavior");
    private static final Iri isComposedOfSubsystem = PREFIX_ONTOCAPE_SYSTEM.iri("isComposedOfSubsystem");
    private static final Iri representsOccurenceOf = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("representsOccurenceOf");
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasGHSHazardStatement = PREFIX_ONTOSPECIES.iri("hasGHSHazardStatements");
    private static final Iri comment = PREFIX_RDFS.iri("comment");

    /**
     * Classes
     */ 
	// private static final Iri MolecularFormula = PREFIX_ONTOSPECIES.iri("MolecularFormula");

    /**
     * Standard constructor
     * @param filepath1 The filepath that contains the sparql endpoints of where the tag status data IRI is located in.
     * @param filepath2 The filepath that contains the sparql endpoints of where the chemical information is located at. 
     */
    public RFIDQueryBuilder(String filepath1, String filepath2) throws IOException {
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
                this.kbClient1.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                this.kbClient1.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
        }
        
        // Check whether properties file exists at specified location of filepath2
        file = new File(filepath2);
        if (!file.exists()) {
            throw new FileNotFoundException("No properties file found at specified filepath: " + filepath1);
        }
        
        try (InputStream input = new FileInputStream(file)) {
            // Load properties file from specified path
            Properties prop = new Properties();
            prop.load(input);
            if (prop.containsKey("sparql.query.endpoint")) {
                this.kbClient2.setQueryEndpoint(prop.getProperty("sparql.query.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.query.endpoint=<sparql_query_endpoint>\"");
            }
            if (prop.containsKey("sparql.update.endpoint")) {
                this.kbClient2.setUpdateEndpoint(prop.getProperty("sparql.update.endpoint"));
            } else {
                throw new IOException("Properties file is missing \"sparql.update.endpoint=<sparql_update_endpoint>\"");
            }
        }
    }
    
    //SELECT ?tag WHERE { ?tag saref:hasState <IRIString> }
    public String queryForTagWithStateIRI(String IRIString) {
         String result = null;
         Variable tag = SparqlBuilder.var("tag");
         SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = tag.has(hasState, iri(IRIString));
        query.prefix(PREFIX_ONTODEVICE, PREFIX_SAREF).select(tag).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("tag");
        }
    } catch (Exception e){
        throw new JPSRuntimeException(GETTAG_ERROR_MSG);
    }
        return result;
    }
    
    //SELECT ?Bottle WHERE { <IRIString> ontodevice:isAttachedTo ?Bottle }
    public String queryForBottleWithIsAttachedTo(String IRIString) {
        String result = null;
        Variable bottle = SparqlBuilder.var("bottle");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(isAttachedTo, bottle);
        query.prefix(PREFIX_ONTODEVICE).select(bottle).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("bottle");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETBOTTLE_ERROR_MSG);
    }
        return result;
    }

    //SELECT ?chemicalAmount WHERE { <IRIString> ontodevice:isFilledWith ?chemicalAmount }
    public String queryForChemicalAmountWithIsFilledWith(String IRIString) {
        String result = null;
        Variable chemicalAmount = SparqlBuilder.var("chemicalAmount");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(isFilledWith, chemicalAmount);
        query.prefix(PREFIX_ONTOLAB).select(chemicalAmount).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("chemicalAmount");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETCHEMICALAMOUNT_ERROR_MSG);
    }
        return result;
    }

    //SELECT ?chemical WHERE { <IRIString> ontoCAPE_CPS_Behavior:refersToMaterial ?chemical }
    public String queryForChemicalWithRefersToMaterial(String IRIString) {
        String result = null;
        Variable chemical = SparqlBuilder.var("chemical");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(refersToMaterial, chemical);
        query.prefix(PREFIX_ONTOCAPE_CPS_BEHAVIOR).select(chemical).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("chemical");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETCHEMICAL_ERROR_MSG);
    }
        return result;
    }

    //SELECT ?phase WHERE { <IRIString> ontoCAPE_Material:thermodynamicBehavior ?phase }
    public String queryForPhaseWithThermodynamicBehavior(String IRIString) {
        String result = null;
        Variable phase = SparqlBuilder.var("phase");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(thermodynamicBehavior, phase);
        query.prefix(PREFIX_ONTOCAPE_MATERIAL).select(phase).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("phase");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETPHASE_ERROR_MSG);
    }
        return result;
    }

    //SELECT ?phaseComponent WHERE { <IRIString> ontoCAPE_System:isComposedOfSubsystem ?phaseComponent }
    public String queryForPhaseComponentWithIsComposedOfSubsystem(String IRIString) {
        String result = null;
        Variable phaseComponent = SparqlBuilder.var("phaseComponent");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(isComposedOfSubsystem, phaseComponent);
        query.prefix(PREFIX_ONTOCAPE_SYSTEM).select(phaseComponent).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("phaseComponent");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG);
    }
        return result;
    }

    //SELECT ?Species WHERE { ?PhaseComponent ontoCAPE_Phase_System:representsOccurenceOf ?Species }
    public String queryForSpeciesWithRepresentsOccurenceOf(String IRIString) {
        String result = null;
        Variable species = SparqlBuilder.var("species");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(representsOccurenceOf, species);
        query.prefix(PREFIX_ONTOCAPE_PHASE_SYSTEM).select(species).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("species");
        }
    } catch (Exception e) {
            throw new JPSRuntimeException(GETSPECIES_ERROR_MSG);
        }
        return result;
    }

    //SELECT ?label WHERE { <IRIString> rdfs:label ?label }
    public String queryForLabel(String IRIString) {
        String result = null;
        Variable labelOfChemicalSpecies = SparqlBuilder.var("label");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(label, labelOfChemicalSpecies);
        query.prefix(PREFIX_RDFS).select(labelOfChemicalSpecies).where(queryPattern);
        kbClient2.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient2.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient2.executeQuery());
            result = kbClient2.executeQuery().getJSONObject(0).getString("label");
        } 
    } catch (Exception e) {
        throw new JPSRuntimeException(GETLABEL_ERROR_MSG, e);
    }
        return result;
    }

    //SELECT ?GHSHazardStatements WHERE {<IRIString> ontospecies:hasGHSHazardStatements ?GHSHazardStatements}
    public JSONArray queryForGHSHazardStatements(String IRIString) {
        Variable ghsHazardStatements = SparqlBuilder.var("GHSHazardStatements");
        JSONArray queryResult;
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasGHSHazardStatement, ghsHazardStatements);
        query.prefix(PREFIX_ONTOSPECIES).select(ghsHazardStatements).where(queryPattern);
        kbClient2.setQuery(query.getQueryString());
        queryResult = kbClient2.executeQuery();
        if(queryResult.isEmpty()){
            queryResult = null;
        }
        return queryResult;
    }

    //SELECT ?label WHERE {<IRIString> rdfs:label ?label}
    //SELECT ?comment WHERE {<IRIString> rdfs:comment ?comment}
    public Map<String, List<String>> queryForLabelAndCommentForGHSHazardStatements(JSONArray IRIArray) {
        Map<String, List<String>> map = new HashMap<>();
        if (IRIArray != null) {
            map.put("label", new ArrayList<>());
            map.put("comment", new ArrayList<>());
            String iriString = null;
            Variable statementLabel = SparqlBuilder.var("label");
            Variable statementComment = SparqlBuilder.var("comment");
            for (int i = 0; i <= IRIArray.length() - 1; i++) {
                iriString = IRIArray.getJSONObject(i).getString("GHSHazardStatements");
                LOGGER.info("The ghs hazard statement IRI is " + iriString);
                SelectQuery query = Queries.SELECT();
                //create triple pattern
                TriplePattern queryPattern = iri(iriString).has(label,statementLabel);
                TriplePattern queryPattern1 = iri(iriString).has(comment, statementComment);
                query.prefix(PREFIX_RDFS).select(statementLabel, statementComment).where(queryPattern, queryPattern1);
                kbClient2.setQuery(query.getQueryString());
                try {
                    JSONArray queryResult = kbClient2.executeQuery();
                    if(!queryResult.isEmpty()){
                        LOGGER.info("The label is " + queryResult.getJSONObject(0).getString("label"));
                        map.get("label").add(queryResult.getJSONObject(0).getString("label"));
                        LOGGER.info("The comment is " + queryResult.getJSONObject(0).getString("comment"));
                        map.get("comment").add(queryResult.getJSONObject(0).getString("comment"));
                    }
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETSTATEMENTLABELANDCOMMENT_ERROR_MSG, e);
                }
            }
        } else {
            map = null;
        }
        return map;
    }
}

