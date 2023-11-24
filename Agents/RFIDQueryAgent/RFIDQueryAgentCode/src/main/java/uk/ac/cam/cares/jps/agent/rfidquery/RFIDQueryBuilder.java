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
 * Class to construct queries to retrieve IRIs and information from the triple store.
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
    private static final String GETTAGSTATE_ERROR_MSG = "Unable to query for tag state IRI!" ;
    private static final String GETTAGGEDOBJECT_ERROR_MSG = "Unable to query for tagged object IRI!" ;
    private static final String GETCHEMICALAMOUNT_ERROR_MSG = "Unable to query for chemical amount IRI!" ;
    private static final String GETCHEMICAL_ERROR_MSG = "Unable to query for chemical IRI!" ;
    private static final String GETPHASE_ERROR_MSG = "Unable to query for phase IRI!" ;
    private static final String GETPHASECOMPONENT_ERROR_MSG = "Unable to query for phase component IRI!" ;
    private static final String GETSPECIES_ERROR_MSG = "Unable to query for species IRI!" ;
    private static final String GETLABEL_ERROR_MSG = "Unable to query for label via rdfs:label!";
    private static final String GETCHEMICALCOMPONENT_ERROR_MSG = "Unable to query for chemical component IRI!";
    private static final String GETSTATEMENTLABELANDCOMMENT_ERROR_MSG = "Unable to query for GHS Hazard Statements labels and comments!";
    private static final String GETMOLECULARFORMULA_ERROR_MSG = "Unable to query for molecular formula!";
    private static final String GETMOLECULARWEIGHT_ERROR_MSG = "Unable to query for molecular weight!";
    private static final String GETMOLECULARWEIGHTUNIT_ERROR_MSG = "Unable to query for molecular weight unit!";
    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
	public static final String ONTOLAB_NS = "https://www.theworldavatar.com/kg/ontolab/";
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/ontology/ontocape/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#";
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
     * Object Properties
     */ 
	private static final Iri isAttachedTo = PREFIX_ONTODEVICE.iri("isAttachedTo");
	private static final Iri isFilledWith = PREFIX_ONTOLAB.iri("isFilledWith");
	private static final Iri refersToMaterial = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("refersToMaterial");
	private static final Iri thermodynamicBehavior = PREFIX_ONTOCAPE_MATERIAL.iri("thermodynamicBehavior");
    private static final Iri isComposedOfSubsystem = PREFIX_ONTOCAPE_SYSTEM.iri("isComposedOfSubsystem");
    private static final Iri representsOccurenceOf = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("representsOccurenceOf");
    private static final Iri hasState = PREFIX_SAREF.iri("hasState");
    private static final Iri hasGHSHazardStatement = PREFIX_ONTOSPECIES.iri("hasGHSHazardStatements");
    private static final Iri intrinsicCharacteristics = PREFIX_ONTOCAPE_MATERIAL.iri("intrinsicCharacteristics");
    private static final Iri containsDirectly = PREFIX_ONTOCAPE_SYSTEM.iri("containsDirectly");
    private static final Iri hasMolecularFormula = PREFIX_ONTOSPECIES.iri("hasMolecularFormula");
    private static final Iri hasMolecularWeight = PREFIX_ONTOSPECIES.iri("hasMolecularWeight");
    private static final Iri unit = PREFIX_ONTOSPECIES.iri("unit");

    /**
     * Data Properties
     */
    private static final Iri label = PREFIX_RDFS.iri("label");
    private static final Iri comment = PREFIX_RDFS.iri("comment");
    private static final Iri value = PREFIX_ONTOSPECIES.iri("value");

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
            if (prop.containsKey("sparql.username")) {
				this.kbClient1.setUser(prop.getProperty("sparql.username"));
			}
			if (prop.containsKey("sparql.password")) {
				this.kbClient1.setPassword(prop.getProperty("sparql.password"));
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
            if (prop.containsKey("sparql.username")) {
				this.kbClient2.setUser(prop.getProperty("sparql.username"));
			}
			if (prop.containsKey("sparql.password")) {
				this.kbClient2.setPassword(prop.getProperty("sparql.password"));
			}
        }
    }

    //SELECT ?tagStateIRI WHERE {  ?tag ontodevice:isAttachedTo <IRIString> .
    //                     ?tag saref:hasState ?tagStateIRI }
    /**
     * Query for tag state IRI from tagged object IRI
     * @param IRIString tagged object IRI
     * @return tag state IRI
     */
    public String queryForTagStateIRIFromTaggedObjectIRI(String IRIString) {
        String result = null;
        Variable tagStateIRI = SparqlBuilder.var("tagStateIRI");
        Variable tag = SparqlBuilder.var("tag");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = tag.has(isAttachedTo, iri(IRIString));
        TriplePattern queryPattern2 = tag.has(hasState, tagStateIRI);
        query.prefix(PREFIX_ONTODEVICE, PREFIX_SAREF).select(tagStateIRI).where(queryPattern, queryPattern2);
        kbClient1.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient1.executeQuery();
            if(!queryResult.isEmpty()){
                LOGGER.info("The tagged object IRI " + IRIString + " has the following tag state IRI: " + kbClient1.executeQuery().getJSONObject(0).getString("tagStateIRI"));
                result = kbClient1.executeQuery().getJSONObject(0).getString("tagStateIRI");
            } else {
                throw new JPSRuntimeException(GETTAGSTATE_ERROR_MSG);
            }
        } catch (Exception e){
            throw new JPSRuntimeException(GETTAGSTATE_ERROR_MSG, e);
        }
        return result;
    }
    
    //SELECT ?tag WHERE { ?tag saref:hasState <IRIString> }
    /**
     * Query for tag IRI via saref:hasState
     * @param IRIString state data IRI
     * @return tag IRI
     */
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
            } else {
                throw new JPSRuntimeException(GETTAG_ERROR_MSG);
            }
        } catch (Exception e){
            throw new JPSRuntimeException(GETTAG_ERROR_MSG, e);
        }
        return result;
    }
    
    //SELECT ?TaggedObject WHERE { <IRIString> ontodevice:isAttachedTo ?TaggedObject }
    /**
     * Query for tagged object IRI
     * @param IRIString tag IRI
     * @return tagged object IRI
     */
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
            } else {
                throw new JPSRuntimeException(GETTAGGEDOBJECT_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETTAGGEDOBJECT_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?chemicalAmount WHERE { <IRIString> ontolab:isFilledWith ?chemicalAmount }
    /**
     * Query for chemical amount IRI via ontolab:isFilledWith
     * @param IRIString chemical container IRI
     * @return chemical amount IRI
     */
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
            } else {
                result = "This tagged object does not contain any chemicals";
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETCHEMICALAMOUNT_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?chemical WHERE { <IRIString> ontoCAPE_CPS_Behavior:refersToMaterial ?chemical }
    /**
     * Query for chemical IRI via ontoCAPE_CPS_Behavior:refersToMaterial
     * @param IRIString chemical amount IRI
     * @return chemical IRI
     */
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
            } else {
                throw new JPSRuntimeException(GETCHEMICAL_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETCHEMICAL_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?phase WHERE { <IRIString> ontoCAPE_Material:thermodynamicBehavior ?phase }
    /**
     * Query for phase IRI via ontoCAPE_Material:thermodynamicBehavior
     * @param IRIString chemical or material IRI
     * @return phase IRI
     */
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
            } else {
                throw new JPSRuntimeException(GETPHASE_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETPHASE_ERROR_MSG, e);
        }
        return result;
    }



    //Check whether <IRIString> rdf:type ontoCAPE_System:MultiphaseSystem, if so
    //SELECT ?phaseComponent WHERE { <IRIString> ontoCAPE_System:isComposedOfSubsystem ?singlePhase .
    //                               ?singlePhase ontoCAPE_System:isComposedOfSubsystem ?phaseComponent . }
    // else
    //SELECT ?phaseComponent WHERE {<IRIString> ontoCAPE_System:isComposedOfSubsystem ?phaseComponent }
    /**
     * Query for phase component IRI via ontoCAPE_System:isComposedOfSubsystem
     * @param IRIString phase IRI
     * @return phase component IRI
     */
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
                    } else {
                        throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG);
                    }
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG, e);
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
                    } else {
                        throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG);
                    }
                } catch (Exception e) {
                    throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG, e);
                }
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETPHASECOMPONENT_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?Species WHERE { <IRIString> ontoCAPE_Phase_System:representsOccurenceOf ?Species }
    /**
     * Query for species IRI via ontoCAPE_Phase_System:representsOccurenceOf
     * @param IRIString phase component IRI
     * @return species IRI
     */
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
            } else {
                throw new JPSRuntimeException(GETSPECIES_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETSPECIES_ERROR_MSG,e);
        }
        return result;
    }

    //SELECT ?label WHERE { <IRIString> rdfs:label ?label }
    /**
     * Query for species label
     * @param IRIString species IRI
     * @return label of species
     */
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
            } else {
                throw new JPSRuntimeException(GETLABEL_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETLABEL_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?label WHERE { <IRIString> rdfs:label ?Label }
    /**
     * Query for tagged object label
     * @param IRIString tagged object IRI
     * @return label of tagged object
     */
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
            } else {
                throw new JPSRuntimeException(GETLABEL_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETLABEL_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?substance WHERE {<IRIString> ontoCAPE_Material:intrinsicCharacteristics ?substance}
    //if ?substance rdf:type is not a Mixture, query for chemicalComponent via ontoCAPE_System:containsDirectly
    //else ?substance == ?chemicalComponent
    /**
     * Query for chemical component IRI using chemical/material IRI
     * @param IRIString chemical or material IRI
     * @return chemical component IRI
     */
    public String queryForChemicalComponent(String IRIString) {
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
    
    //SELECT ?chemicalComponent WHERE <IRIString> ontoCAPE_System:containsDirectly ?chemicalComponent
    /**
     * Query for chemicalComponent via ontoCAPE_System:containsDirectly
     * @param IRIString substance IRI
     * @return chemical component IRI
     */
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
            } else {
                throw new JPSRuntimeException(GETCHEMICALCOMPONENT_ERROR_MSG);
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETCHEMICALCOMPONENT_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?GHSHazardStatements WHERE {<IRIString> ontospecies:hasGHSHazardStatements ?GHSHazardStatements}
    /**
     * Query for the GHSHazardStatements of a species IRI
     * @param IRIString species IRI
     * @return GHSHazardStatements IRIs of the species
     */
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
    /**
     * Query for the label and comments of each GHSHazardStatement in the JSONArray
     * @param IRIArray a JSONArray containing GHSHazardStatement IRIs
     * @return a map containing the label and comment for each GHSHazardStatement IRI
     */
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
                    } else {
                        throw new JPSRuntimeException(GETSTATEMENTLABELANDCOMMENT_ERROR_MSG);
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

    //SELECT ?molecularFormulaValue WHERE {<IRIString> ontospecies:hasMolecularFormula ?molecularFormula .
    //                                     ?molecularFormula ontospecies:value ?molecularFormulaValue .}
    /**
     * Query for molecular formula of a species IRI
     * @param IRIString species IRI
     * @return molecular formula of the species
     */
    public String queryForMolecularFormula(String IRIString) {
        String result = null;
        Variable molecularFormula = SparqlBuilder.var("molecularFormula");
        Variable molecularFormulaValue = SparqlBuilder.var("molecularFormulaValue");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasMolecularFormula, molecularFormula);
        TriplePattern queryPattern2 = molecularFormula.has(value, molecularFormulaValue);
        query.prefix(PREFIX_ONTOSPECIES).select(molecularFormulaValue).where(queryPattern, queryPattern2);
        kbClient2.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient2.executeQuery();
            if(!queryResult.isEmpty()){
                result = queryResult.getJSONObject(0).getString("molecularFormulaValue");
            }
            else {
                result = "Molecular Formula information not available";
            }
        } catch (Exception e) {
            throw new JPSRuntimeException(GETMOLECULARFORMULA_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?molecularWeightValue WHERE {<IRIString> ontospecies:hasMolecularWeight ?molecularWeight .
    //                                     ?molecularWeight ontospecies:value ?molecularWeightValue .}
    /**
     * Query for molecular weight of a species IRI
     * @param IRIString species IRI
     * @return molecular weight
     */
    public String queryForMolecularWeightValue(String IRIString) {
        String result = null;
        Variable molecularWeight = SparqlBuilder.var("molecularWeight");
        Variable molecularWeightValue = SparqlBuilder.var("molecularWeightValue");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasMolecularWeight, molecularWeight);
        TriplePattern queryPattern2 = molecularWeight.has(value, molecularWeightValue);
        query.prefix(PREFIX_ONTOSPECIES).select(molecularWeightValue).where(queryPattern, queryPattern2);
        kbClient2.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient2.executeQuery();
            if(!queryResult.isEmpty()){
                result = queryResult.getJSONObject(0).getString("molecularWeightValue");
            } else {
                result = "Molecular Weight information not available";
            }
         } catch (Exception e) {
            throw new JPSRuntimeException(GETMOLECULARWEIGHT_ERROR_MSG, e);
        }
        return result;
    }

    //SELECT ?molecularWeightUnit WHERE {<IRIString> ontospecies:hasMolecularWeight ?molecularWeight .
    //                                     ?molecularWeight ontospecies:unit ?molecularWeightUnit .}
    /**
     * Query for molecular weight unit of a species IRI
     * @param IRIString species IRI
     * @return molecular weight unit
     */
    public String queryForMolecularWeightUnit(String IRIString) {
        String result = null;
        Variable molecularWeight = SparqlBuilder.var("molecularWeight");
        Variable molecularWeightUnit = SparqlBuilder.var("molecularWeightUnit");
        Variable molecularWeightUnitLabel = SparqlBuilder.var("molecularWeightUnitLabel");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasMolecularWeight, molecularWeight);
        TriplePattern queryPattern2 = molecularWeight.has(unit, molecularWeightUnit);
        TriplePattern queryPattern3 = molecularWeightUnit.has(label, molecularWeightUnitLabel);
        query.prefix(PREFIX_ONTOSPECIES, PREFIX_RDFS).select(molecularWeightUnitLabel).where(queryPattern, queryPattern2, queryPattern3);
        kbClient2.setQuery(query.getQueryString());
        try {
            JSONArray queryResult = kbClient2.executeQuery();
            if(!queryResult.isEmpty()){
                result = queryResult.getJSONObject(0).getString("molecularWeightUnitLabel");
            } else {
                result = "Molecular Weight unit information not available";
            }
         } catch (Exception e) {
            throw new JPSRuntimeException(GETMOLECULARWEIGHTUNIT_ERROR_MSG, e);
        }
        return result;
    }
}

