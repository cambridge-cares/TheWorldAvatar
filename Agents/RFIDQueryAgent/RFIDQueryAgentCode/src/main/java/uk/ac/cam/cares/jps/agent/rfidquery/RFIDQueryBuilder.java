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
    private static final String GETTAG_ERROR_MSG = "Unable to query for tag IRI!" ;
    private static final String GETTAGGEDOBJECT_ERROR_MSG = "Unable to query for tagged object IRI!" ;
    private static final String GETCHEMICALAMOUNT_ERROR_MSG = "Unable to query for chemical amount IRI!" ;
    private static final String GETCHEMICAL_ERROR_MSG = "Unable to query for chemical IRI!" ;
    private static final String GETPHASE_ERROR_MSG = "Unable to query for phase IRI!" ;
    private static final String GETPHASECOMPONENT_ERROR_MSG = "Unable to query for phase component IRI!" ;
    private static final String GETSPECIES_ERROR_MSG = "Unable to query for species IRI!" ;
    private static final String GETLABEL_ERROR_MSG = "Unable to query for label via rdfs:label!";
    private static final String GETCHEMICALCOMPONENT_ERROR_MSG = "Unable to query for chemical component IRI!";
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
    public static final String ONTOCAPE_CPS_SUBSTANCE = "http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#";
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
    private static final Prefix PREFIX_ONTOCAPE_CPS_SUBSTANCE = SparqlBuilder.prefix("ontocape_cps_substance", iri(ONTOCAPE_CPS_SUBSTANCE));
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
    private static final Iri intrinsicCharacteristics = PREFIX_ONTOCAPE_MATERIAL.iri("intrinsicCharacteristics");
    private static final Iri containsDirectly = PREFIX_ONTOCAPE_SYSTEM.iri("containsDirectly");
    /**
     * Classes
     */
    private static final Iri multiPhaseSystem = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("MultiphaseSystem");
    private static final Iri singlePhase = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("SinglePhase");
    private static final Iri mixture = PREFIX_ONTOCAPE_CPS_SUBSTANCE.iri("Mixture");

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
    
    //SELECT ?TaggedObject WHERE { <IRIString> ontodevice:isAttachedTo ?TaggedObject }
    public String queryForTaggedObjectWithIsAttachedTo(String IRIString) {
        String result = null;
        Variable taggedObject = SparqlBuilder.var("taggedObject");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(isAttachedTo, taggedObject);
        query.prefix(PREFIX_ONTODEVICE).select(taggedObject).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("taggedObject");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETTAGGEDOBJECT_ERROR_MSG);
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



    //Check whether <IRIString> rdf:type ontoCAPE_System:MultiphaseSystem, if so
    //SELECT ?phaseComponent WHERE { <IRIString> ontoCAPE_System:isComposedOfSubsystem ?singlePhase .
    //                               ?singlePhase ontoCAPE_System:isComposedOfSubsystem ?phaseComponent . }
    // else
    //SELECT ?phaseComponent WHERE {<IRIString> ontoCAPE_System:isComposedOfSubsystem ?phaseComponent }
    public String queryForPhaseComponentWithIsComposedOfSubsystem(String IRIString) {
        String result = null;
        Variable variable = SparqlBuilder.var("variable");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).isA(variable);
        query.prefix(PREFIX_ONTOCAPE_SYSTEM).select(variable).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(queryResult.getJSONObject(0).getString("variable").contains("MultiphaseSystem")){
            Variable singlePhase = SparqlBuilder.var("singlePhase");
            TriplePattern queryPattern3 = iri(IRIString).has(isComposedOfSubsystem, singlePhase);
            Variable phaseComponent = SparqlBuilder.var("phaseComponent");
            TriplePattern queryPattern4 = singlePhase.has(isComposedOfSubsystem, phaseComponent);
            query.prefix(PREFIX_ONTOCAPE_SYSTEM).select(phaseComponent).where(queryPattern3, queryPattern4);
            kbClient1.setQuery(query.getQueryString());
            try {
                queryResult = kbClient1.executeQuery();
                if(!queryResult.isEmpty()){
                    LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
                    result = kbClient1.executeQuery().getJSONObject(0).getString("phaseComponent");
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG);
            }
        } else {
            Variable phaseComponent = SparqlBuilder.var("phaseComponent");
            TriplePattern queryPattern5 = iri(IRIString).has(isComposedOfSubsystem, phaseComponent);
            query.prefix(PREFIX_ONTOCAPE_SYSTEM).select(phaseComponent).where(queryPattern5);
            kbClient1.setQuery(query.getQueryString());
            try {
                queryResult = kbClient1.executeQuery();
                if(!queryResult.isEmpty()){
                    LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
                    result = kbClient1.executeQuery().getJSONObject(0).getString("phaseComponent");
                }
            } catch (Exception e) {
                throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG);
            }
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG);
    }
        return result;
    }

    //SELECT ?Species WHERE { <IRIString> ontoCAPE_Phase_System:representsOccurenceOf ?Species }
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
    public String queryForSpeciesLabel(String IRIString) {
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

    //SELECT ?label WHERE { <IRIString> rdfs:label ?Label }
    public String queryForTaggedObjectLabel(String IRIString) {
        String result = null;
        Variable taggedObjectLabel = SparqlBuilder.var("label");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(label, taggedObjectLabel);
        query.prefix(PREFIX_RDFS).select(taggedObjectLabel).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient1.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info(kbClient1.executeQuery());
                result = kbClient1.executeQuery().getJSONObject(0).getString("label");
            } 
        } catch (Exception e) {
            throw new JPSRuntimeException(GETLABEL_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?substance WHERE {<IRIString> ontoCAPE_Material:intrinsicCharacteristics ?substance}
    public String queryForChemicalComponentWithIntrinsicCharacteristics(String IRIString) {
        Variable substance = SparqlBuilder.var("substance");
        JSONArray queryResult;
        String chemicalComponent;
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(intrinsicCharacteristics, substance);
        query.prefix(PREFIX_ONTOCAPE_MATERIAL).select(substance).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        queryResult = kbClient1.executeQuery();
        if(queryResult.isEmpty()){
            chemicalComponent = null;
        } else {
            String a = queryResult.getJSONObject(0).getString("substance");
            Variable variable = SparqlBuilder.var("variable");
            //create triple pattern
            TriplePattern queryPattern1 = iri(a).isA(variable);
            query.prefix(PREFIX_ONTOCAPE_MATERIAL).select(variable).where(queryPattern1);
            kbClient1.setQuery(query.getQueryString());
            queryResult = kbClient1.executeQuery();
            if(queryResult.getJSONObject(0).getString("variable").contains("Mixture")){
                chemicalComponent = queryForChemicalComponentWithContainsDirectly(a);
            } else {
                chemicalComponent = a;
            }
        }
        return chemicalComponent;
    }
    
    public String queryForChemicalComponentWithContainsDirectly(String IRIString) {
        SelectQuery query = Queries.SELECT();
        String result = null;
        Variable chemicalComponent = SparqlBuilder.var("chemicalComponent");
        TriplePattern queryPattern = iri(IRIString).has(containsDirectly, chemicalComponent);
        query.prefix(PREFIX_ONTOCAPE_SYSTEM).select(chemicalComponent).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient1.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info(kbClient1.executeQuery());
                result = kbClient1.executeQuery().getJSONObject(0).getString("chemicalComponent");
            } 
        } catch (Exception e) {
            throw new JPSRuntimeException(GETCHEMICALCOMPONENT_ERROR_MSG, e);
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

