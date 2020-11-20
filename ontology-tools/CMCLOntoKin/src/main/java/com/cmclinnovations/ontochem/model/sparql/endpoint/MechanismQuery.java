package com.cmclinnovations.ontochem.model.sparql.endpoint;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import com.cmclinnovations.ontochem.model.exception.OntoException;

import de.derivo.sparqldlapi.Query;
import de.derivo.sparqldlapi.QueryEngine;
import de.derivo.sparqldlapi.QueryResult;
import de.derivo.sparqldlapi.exceptions.QueryEngineException;
import de.derivo.sparqldlapi.exceptions.QueryParserException;

/**
 * This class implemented the method that belong to the IMechanism
 * interface.
 * 
 * @author msff2
 *
 */
public class MechanismQuery implements IMechanismQuery{
	private OWLOntology ontology;
	private IRI ontologyIRI;
	private QueryEngine engine;
	private OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	private StructuralReasonerFactory reasonerFactory = new StructuralReasonerFactory();
	private ArrayList<String> queryResult = new ArrayList<String>();
	private static final String EMPTY = "";
	private static final String HASH = "#";
	private static final String RDF = "rdf";
	private static final String RDF_TYPE = "type";
	private static final String RDF_URL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
	private static final String RDFS = "rdfs";
	private static final String RDFS_LABEL = "label";
	private static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";

	// OntoKin Knolwedge Base (KB) management properties
	private static final String ONTOKIN_TBOX_IRI = "http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl";
	private static final String ONTOKIN_ABOX_IRI = "https://como.cheng.cam.ac.uk/kb/ontokin/ontokinabox.txt";
	private static final String ONTOKIN_ABOX_FILE_NAME="ontokinabox.txt";
	private static final String ONTOKIN_KB_URL="http://www.theworldavatar.com/kb/ontokin/";
	private static final String ONTOKIN_KB_ROOT_DIRECTORY="kb/ontokin/";
	
	/**
	 * This method has been written to test the IRI and Name reader.
	 * 
	 * @param args
	 */
	public static void main(String[] args){
		
		try{
			for(int i =0; i<=10;i++){
			long timestampStart = System.currentTimeMillis();
			IMechanismQuery iMechanism = new MechanismQuery();
			Map<String, String> iriPlusMechanismName = iMechanism.queryIriPlusName();
//			List<String> list = new ArrayList<String>(iriPlusMechanismName.keySet());
//			Collections.sort(list);
			for(String iri:iriPlusMechanismName.keySet()){
				System.out.println("Mechanism  IRI:"+iri);
				System.out.println("Mechanism Name:"+iriPlusMechanismName.get(iri));
				System.out.println();
			}
			long timestampEnd = System.currentTimeMillis();
			System.out.println("It took in total:"+(timestampEnd-timestampStart)+" milliseconds.");
			System.out.println("It took in total:"+(timestampEnd-timestampStart)/1000+" second(s).");
			}
		}catch(OntoException e){
			e.printStackTrace();
		}catch(OWLOntologyCreationException e){
			e.printStackTrace();
		}catch(IOException e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Queries IRIs and names of all mechanisms available in the Ontokin KB.  
	 */
	public Map<String, String> queryIriPlusName() throws OntoException, OWLOntologyCreationException, IOException{
		Map<String, String> irisPlusNames = new HashMap<String, String>();
		BufferedReader br = openFileFromURL(ONTOKIN_ABOX_IRI);
		String line;
		while((line=br.readLine())!=null){
			if(!line.isEmpty()){
				queryABox(irisPlusNames,ONTOKIN_KB_URL.concat(line));
			}
		}
		return irisPlusNames;
	}
	
	/**
	 * 
	 * 
	 * @param irisPlusName
	 * @param aboxOwlFile
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private void queryABox(Map<String, String> irisPlusName, String aboxOwlFile)  throws OntoException, OWLOntologyCreationException{
		ontologyIRI = IRI.create(aboxOwlFile);
		loadOntology(ontologyIRI);
		if(engine == null){
			engine = createQueryEngine();
		}
		IRI iri = readOntologyIRI(ontology);
		queryABox(irisPlusName, iri);
	}
	
	/**
	 * Checks if the current ontology contains an IRI. If an IRI is available
	 * it is returned, otherwise the OntoException exception is thrown.
	 * 
	 * @param ontology
	 * @return IRI the IRI of the input ontology 
	 * @throws OntoException
	 */
	private IRI readOntologyIRI(OWLOntology ontology) throws OntoException{
		if(ontology.getOntologyID().getOntologyIRI().isPresent()){
			return ontology.getOntologyID().getOntologyIRI().get();
		} else{
			throw new OntoException("The OWL file does not contain an IRI.");
		}
	}

	
	/**
	 * 
	 * @param irisPlusName
	 * @param iri
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private void queryABox(Map<String, String> irisPlusName, IRI iri)  throws OntoException, OWLOntologyCreationException{
		queryInstance("ChemicalMechanism");
		if(queryResult!=null && !queryResult.isEmpty()){
			for(String result:queryResult){
				String label = queryLabel(result);
				if(label!=null && !label.isEmpty()){
					irisPlusName.put(iri.toString(), label);
					queryResult = new ArrayList<String>();
					engine = null;
					return;
				}
			}
		}
	}
	
	/**
	 * Loads a mechanism OWL ontology into memory.
	 * 
	 * @param ontologyIRI
	 * @return
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private void loadOntology(IRI ontologyIRI) throws OntoException, OWLOntologyCreationException{
		// Loads the ontology located at a IRI
		ontology = OWLManager.createOWLOntologyManager().loadOntology(ontologyIRI);
		if (ontology == null) {
//			logger.error("The requested ontology could not be loaded into memory.");
			throw new OntoException("The requested ontology could not be loaded into memory.");
		}
	}
	
	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @return the result of the SPARQL query
	 */
	public String performQuery(String q, int type){
		String filteredResult = "";
		// Create a query object from it's string representation
		try {
		Query query = Query.create(q);
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
			if(result.isEmpty()){
				return null;				
			}
			// Calls filterResult method to filter out the unwanted
			// part of the result
			filteredResult = filterResult(result.toString(), type);
		} catch (QueryParserException e) {
			// TODO Auto-generated catch block
			return null;
		} catch (QueryEngineException e) {
			// TODO Auto-generated catch block
			return null;
		}
		return filteredResult;
	}
	
	/**
	 * Separates the actual result of a query from the 
	 * SPARQLDLAPI part (e.g. ?de.derivo.sparqldlapi.Var@76 = ?),
	 * which appears at the beginning of the result and the
	 * datatype, which appears at the end.
	 * 
	 * @param result contains SPARQLDLAPI part + result + datatype
	 * (e.g. ^^xsd:string)
	 * @return only the result of a query
	 */
	private String filterResult(String result, int type){
		String onlyResult = "";
		if (type == 1) {
			return filterPropertyValue(result, result.length());
		} else if (type == 2) {
			return filterClassType(result, result.length());
		}
		return onlyResult;
	}
	
	/**
	 * Produces the expected result by filtering out the unwanted parts from
	 * the value of a property.
	 * 
	 * @param result
	 * @param resultLength
	 * @return
	 */
	private String filterPropertyValue(String result, int resultLength){
		try{
		if (resultLength > 48) {
			if(result.contains("^^xsd:string")){
				return result.toString().substring(34, resultLength - 14);
			} else if(result.contains("#")){
				String[] tokens = result.split("#");
				if(tokens.length>1){
					return tokens[1];
				}
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
		return EMPTY;
	}

	/**
	 * Produces the expected result by filtering out the unwanted parts from
	 * the class of an instance.
	 *  
	 * @param result
	 * @param resultLength
	 * @return
	 */
	private String filterClassType(String result, int resultLength){
		try{
		if (resultLength > 32) {
			if(result.substring(32)!=null){
				if(result.substring(32).contains(HASH)){
					String[] resultParts = result.substring(32).split(HASH);
					if(resultParts.length>1){
						return resultParts[1];
					}
				}
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
		return EMPTY;
	}

	/**
	 * Queries the instances of a class.
	 * 
	 * @param claz the class to which instances belong to
	 */
	/**
	 * 
	 * @param claz the class to which instances belong to
	 * @param type the type represents the 
	 */
	public void queryInstance(String claz){
		String q = formQueryWithAType(RDF, RDF_URL, RDF_TYPE, claz);
		// This method call will store result in a global variable called
		// queryResult, therefore, the line following the call will extract
		// results from it. 
		performMultilineAnswerQuery(q, 2);
	}

	/**
	 * Forms the SPARQL query to retrieve the instance belonging to a type 
	 * when the type is given.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithAType(String vocabulary, String vocabURL, String property, String type) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("Type(?v, ")
				.concat("base:").concat(type)
				.concat(")\n}");
		return q;
	}
	
	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @param type the type of value expected to return.
	 * It can be a class or property value
	 * @return the result of the SPARQL query
	 */
	public void performMultilineAnswerQuery(String q, int type){
		// Create a query object from it's string representation
		try {
			Query query = Query.create(q);
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
			// Calls filterResult method to filter out the unwanted
			// part of the result
			filterMultilineResult(result.toString(), type);
		} catch (QueryParserException e) {
			return;
		} catch (QueryEngineException e) {
			return;
		} catch(Exception e){
			return;
		}
	}
	
	/**
	 * Removes unwanted texts from a multi-line result.
	 * 
	 * @param result
	 * @param type
	 */
	private void filterMultilineResult(String result, int type){
		try{
		if(result.contains("\n")){
			for(String res:result.split("\n")){
				// To reduce the amount of message passing all the multiline
				// results query use queryResult global vairable. It helps
				// reduce the amount of message passing.
				String str = filterResult(res, type);
				if(str!=null && !str.isEmpty()){
					queryResult.add(str);
				}
				//queryResult.add(filterResult(res, type));
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Retrieves the name of a mechanism using its OWL instance id.
	 * 
	 * @param mechanismInstanceId
	 * @return
	 * @throws OntoException
	 */
	private String queryLabel(String mechanismInstanceId) throws OntoException{
		String label = readLabel(mechanismInstanceId);
		if(label==null || label.trim().isEmpty()){
//			logger.error("Mechanism OWL instance does not have a label (name).");
			throw new OntoException("Mechanism OWL instance does not have a label (name).");
		}
		return label;
	}

	/**
	 * Forms and then performs a SPARQL query to read the rdfs:label value of 
	 * any object. 
	 * 
	 * @param instanceOwlId the instance id in the OWL representation of 
	 * a mechanism
	 * @return
	 */
	public String readLabel(String instanceOwlId) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instanceOwlId, RDFS_LABEL);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithAStandardVocabulary(String vocabulary, String vocabURL, String object, String property) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat("#>\n")
				.concat("PREFIX ").concat(vocabulary).concat(": <").concat(vocabURL).concat(">\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(base:").concat(object)
				.concat(", ").concat(vocabulary).concat(":").concat(property).concat(", ?v)\n")
				.concat("}");
		return q;
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class.
	 * It takes 
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 */
	public static BufferedReader openFileFromURL(String fileURL)
			throws IOException {
		URL url = new URL(fileURL);
		return new BufferedReader(new InputStreamReader(url.openStream()));
	}
	
	/**
	 * Creates an instance of query engine and returns it.
	 * 
	 * @return
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	private QueryEngine createQueryEngine() throws OntoException, OWLOntologyCreationException{
		// Creates a reasoner
        OWLReasoner reasoner = reasonerFactory.createReasoner(ontology);
		// Creates a query engine
		return QueryEngine.create(manager, reasoner, true);
	}
}
