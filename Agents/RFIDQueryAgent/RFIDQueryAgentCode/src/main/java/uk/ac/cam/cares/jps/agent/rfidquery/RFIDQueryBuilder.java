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
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.json.JSONArray;

import com.hp.hpl.jena.graph.Triple;

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
    private static final String GETQUALITY_ERROR_MSG = "Unable to query for quality IRI!" ;
    private static final String GETTAG_ERROR_MSG = "Unable to query for tag IRI!" ;
    private static final String GETBOTTLE_ERROR_MSG = "Unable to query for bottle IRI!" ;
    private static final String GETCHEMICAL_ERROR_MSG = "Unable to query for chemical IRI!" ;
    private static final String GETMATERIAL_ERROR_MSG = "Unable to query for material IRI!" ;
    private static final String GETPHASE_ERROR_MSG = "Unable to query for phase IRI!" ;
    private static final String GETPHASECOMPONENT_ERROR_MSG = "Unable to query for phase component IRI!" ;
    private static final String GETSPECIES_ERROR_MSG = "Unable to query for species IRI!" ;
    private static final String GETMOLECULARFORMULA_ERROR_MSG = "Unable to query for molecular formula IRI!" ;
    private static final String GETELEMENTNUMBER_ERROR_MSG = "Unable to query for element number IRI!" ;
    private static final String GETNUMBEROFELEMENT_ERROR_MSG = "Unable to query for number of elements!" ;
    
    /**
     * Namespaces for ontologies
     */
	public static final String ONTODEVICE_NS = "https://www.theworldavatar.com/kg/ontodevice/";
	public static final String ONTOLAB_NS = "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontolab/OntoLab.owl#";
    public static final String ONTOCAPE_CPS_BEHAVIOR_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#";
    public static final String ONTOCAPE_PHASE_SYSTEM_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#";
    public static final String ONTOCAPE_MATERIAL_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/material.owl#";
    public static final String ONTOCAPE_SYSTEM_NS = "http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#";
    public static final String ONTOSPECIES_NS = "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#";
    public static final String ONTOKIN_NS = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#";
    
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
    private static final Prefix PREFIX_ONTOKIN = SparqlBuilder.prefix("ontokin", iri(ONTOKIN_NS));
    
	/**
     * Relationships
     */ 
	private static final Iri hasQualitativeValue = PREFIX_ONTODEVICE.iri("hasQualitativeValue");
	private static final Iri isPropertyOf = PREFIX_ONTODEVICE.iri("isPropertyOf");
	private static final Iri isAttachedTo = PREFIX_ONTODEVICE.iri("isAttachedTo");
	private static final Iri isFilledWith = PREFIX_ONTOLAB.iri("isFilledWith");
	private static final Iri refersToMaterial = PREFIX_ONTOCAPE_CPS_BEHAVIOR.iri("refersToMaterial");
	private static final Iri thermodynamicBehavior = PREFIX_ONTOCAPE_MATERIAL.iri("thermodynamicBehavior");
    private static final Iri isComposedOfSubsystem = PREFIX_ONTOCAPE_SYSTEM.iri("isComposedOfSubsystem");
    private static final Iri representsOccurenceOf = PREFIX_ONTOCAPE_PHASE_SYSTEM.iri("representsOccurenceOf");
    private static final Iri hasMolecularFormula = PREFIX_ONTOSPECIES.iri("hasMolecularFormula");
    private static final Iri hasElementNumber = PREFIX_ONTOKIN.iri("hasElementNumber");
    private static final Iri hasNumberOfElement = PREFIX_ONTOKIN.iri("hasNumberOfElement");

    	/**
     * Classes
     */ 
	private static final Iri MolecularFormula = PREFIX_ONTOSPECIES.iri("MolecularFormula");

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

    //SELECT ?quality WHERE { ?quality ontodevice:hasQualitativeValue <data IRI or ontodevice:QualitativeValue> }
    public String queryForQualityWithHasQualitativeValue(String dataIRIString) {
        String result = null;
        Variable quality = SparqlBuilder.var("quality");
        iri(dataIRIString) ;
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = quality.has(hasQualitativeValue, iri(dataIRIString));
        //TriplePattern triple = GraphPatterns.tp(quality, hasQualitativeValue, iri(dataIRIString));
        query.prefix(PREFIX_ONTODEVICE).select(quality).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
		if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
			result = kbClient1.executeQuery().getJSONObject(0).getString("quality");
		}
    } catch (Exception e){
        throw new JPSRuntimeException(GETQUALITY_ERROR_MSG);
    }
		return result;
    }
    
    //SELECT ?tag WHERE { ?quality ontodevice:isPropertyOf ?tag }
    public String queryForTagWithQualityIRI(String IRIString) {
         String result = null;
         Variable tag = SparqlBuilder.var("tag");
         SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(isPropertyOf, tag);
        query.prefix(PREFIX_ONTODEVICE).select(tag).where(queryPattern);
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
    
    //SELECT ?Bottle WHERE { ?tag ontodevice:isAttachedTo ?Bottle }
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

    //SELECT ?Chemical WHERE { ?bottle ontodevice:isFilledWith ?Chemical }
    public String queryForChemicalWithIsFilledWith(String IRIString) {
        String result = null;
        Variable chemical = SparqlBuilder.var("chemical");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(isFilledWith, chemical);
        query.prefix(PREFIX_ONTOLAB).select(chemical).where(queryPattern);
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

    //SELECT ?Material WHERE { ?Chemical ontoCAPE_CPS_Behavior:refersToMaterial ?Material }
    public String queryForMaterialWithRefersToMaterial(String IRIString) {
        String result = null;
        Variable material = SparqlBuilder.var("material");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(refersToMaterial, material);
        query.prefix(PREFIX_ONTOCAPE_CPS_BEHAVIOR).select(material).where(queryPattern);
        kbClient1.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient1.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient1.executeQuery().getJSONObject(0));
            result = kbClient1.executeQuery().getJSONObject(0).getString("material");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETMATERIAL_ERROR_MSG);
    }
        return result;
    }

     //SELECT ?Phase WHERE { ?Material ontoCAPE_Material:thermodynamicBehavior ?Phase }
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

     //SELECT ?PhaseComponent WHERE { ?Phase ontoCAPE_System:isComposedOfSubsystem ?PhaseComponent }
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

     //SELECT ?MolecularFormula WHERE { ?Species ontospecies:hasMolecularFormula ?MolecularFormula 
     // ?MolecularFormula rdf:Type ontospecies:MolecularForumula }
     public String queryForMolecularFormulaWithHasMolecularFormula(String IRIString) {
        String result = null;
        Variable molecularFormula = SparqlBuilder.var("molecularFormula");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasMolecularFormula, molecularFormula);
        TriplePattern queryPattern2 = molecularFormula.isA(MolecularFormula);
        query.prefix(PREFIX_ONTOSPECIES).select(molecularFormula).where(queryPattern, queryPattern2);
        kbClient2.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient2.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info("There are " + kbClient2.executeQuery().getJSONObject(0).length() + " number of results returned from the query.");
            result = kbClient2.executeQuery().getJSONObject(0).getString("molecularFormula");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETMOLECULARFORMULA_ERROR_MSG);
    }
        return result;
    }

     //SELECT ?ElementNumber WHERE { ?MolecularFormula ontokin:hasElementNumber ?ElementNumber }
     public String queryForElementNumberViaHasElementNumber(String IRIString) {
        String result = null;
        Variable elementNumber = SparqlBuilder.var("elementNumber");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasElementNumber, elementNumber);
        query.prefix(PREFIX_ONTOKIN).select(elementNumber).where(queryPattern);
        kbClient2.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient2.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient2.executeQuery().getJSONObject(0));
            result = kbClient2.executeQuery().getJSONObject(0).getString("elementNumber");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETELEMENTNUMBER_ERROR_MSG);
    }
        return result;
    }

     //SELECT ?Number WHERE { ?ElementNumber ontokin:hasNumberOfElement ?Number }
     public String queryForNumberViaHasNumberOfElement(String IRIString) {
        String result = null;
        Variable number = SparqlBuilder.var("number");
        SelectQuery query = Queries.SELECT();
        //create triple pattern
        TriplePattern queryPattern = iri(IRIString).has(hasNumberOfElement, number);
        query.prefix(PREFIX_ONTOKIN).select(number).where(queryPattern);
        kbClient2.setQuery(query.getQueryString());
        try {
        JSONArray queryResult = kbClient2.executeQuery();
        if(!queryResult.isEmpty()){
            LOGGER.info(kbClient2.executeQuery().getJSONObject(0));
            result = kbClient2.executeQuery().getJSONObject(0).getString("number");
        }
    } catch (Exception e) {
        throw new JPSRuntimeException(GETNUMBEROFELEMENT_ERROR_MSG);
    }
        return result;
    }
 }

