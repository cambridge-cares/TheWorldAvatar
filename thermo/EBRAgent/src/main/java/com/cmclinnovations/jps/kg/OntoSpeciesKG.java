package com.cmclinnovations.jps.kg;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import org.apache.log4j.Logger;
import org.eclipse.rdf4j.IsolationLevels;
import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.federated.FedXFactory;

import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import com.cmclinnovations.jps.agent.quantum.calculation.EBRAgentException;
import com.cmclinnovations.jps.agent.quantum.calculation.Property;
import com.cmclinnovations.jps.model.species.SpeciesBean;

/**
 * This class manages the download of species from the OntoSpecies repository</br>
 * available on any RDF4J triple store.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OntoSpeciesKG{
	Logger logger = Logger.getLogger(OntoSpeciesKG.class);	
	public static final String RDF = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> \n";
	public static final String RDFS = "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n";
	
	public static void main(String[] args) throws ServletException, EBRAgentException{
//		OntoSpeciesKG ontoSpeciesKG = new OntoSpeciesKG();
//		EBRAgent dftAgent = new EBRAgent();
//		dftAgent.init();
//		for(String speciesIRI:dftAgent.ontoSpeciesIRIs){
//			if(!speciesIRI.trim().startsWith("<") && !speciesIRI.trim().endsWith(">")){
//				speciesIRI = "<".concat(speciesIRI).concat(">");
//			}
//			String queryString = ontoSpeciesKG.formGeometryQuery(Property.PREFIX_BINDING_ONTOSPECIES.getPropertyName(), speciesIRI);
//			System.out.println("QueryString:"+queryString+"\n");
//			List<String> testResults = ontoSpeciesKG.queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), Property.RDF4J_ONTOSPECIES_REPOSITORY_ID.getPropertyName(), queryString);
//			for(String testResult: testResults){
//				System.out.println("Test Result:\n"+testResult);
//			}
//		}
		
		OntoSpeciesKG ontoSpeciesKG = new OntoSpeciesKG();
		ontoSpeciesKG.getAllSpecies();
	}
	
	/**
	 * Queries the geometry of species from the OntoSpecies knowledge graph.
	 * 
	 * @param speciesIRI
	 * @return
	 * @throws EBRAgentException
	 */
	public String querySpeciesGeometry(String speciesIRI) throws EBRAgentException{
		String speciesGeometry = null;
		if(!speciesIRI.trim().startsWith("<") && !speciesIRI.trim().endsWith(">")){
			speciesIRI = "<".concat(speciesIRI).concat(">");
		}
		String queryString = formGeometryQuery(Property.PREFIX_BINDING_ONTOSPECIES.getPropertyName(), speciesIRI);
		
		System.out.println("QueryString:"+queryString+"\n");
		
		List<String> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), Property.RDF4J_ONTOSPECIES_REPOSITORY_ID.getPropertyName(), queryString);
		
		for(String testResult: testResults){
			System.out.println("Test Result:\n"+testResult);
		
			speciesGeometry = testResult;
		}
		
		return speciesGeometry;
		
	}

	public List<String> querySpeciesBondGeometry(String speciesIRI) throws EBRAgentException{
		String speciesGeometry = null;
		if(!speciesIRI.trim().startsWith("<") && !speciesIRI.trim().endsWith(">")){
			speciesIRI = "<".concat(speciesIRI).concat(">");
		}
		String queryString = formGeometryQuery(Property.PREFIX_BINDING_ONTOSPECIES.getPropertyName(), speciesIRI);
		
		System.out.println("QueryString:"+queryString+"\n");
		
		List<String> testResults = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), Property.RDF4J_ONTOSPECIES_REPOSITORY_ID.getPropertyName(), queryString);
		
		for(String testResult: testResults){
			System.out.println("Test Result:\n"+testResult);
		
//			speciesGeometry = testResult;
		}
		
		return testResults;
		
	}
	
	/**
	 * Queries all species available in the OntoSpecies Knowledge Graph.
	 * 
	 * @return
	 * @throws EBRAgentException
	 */
	public List<String> getAllSpecies() throws EBRAgentException{
		String queryString = formAllSpeciesQuery(Property.PREFIX_BINDING_RDF.getPropertyName(), Property.PREFIX_BINDING_ONTOSPECIES.getPropertyName());
		System.out.println("QueryString:"+queryString+"\n");
		List<String> speciesIRIs = queryRepository(Property.RDF4J_SERVER_URL_FOR_LOCALHOST.getPropertyName(), Property.RDF4J_ONTOSPECIES_REPOSITORY_ID.getPropertyName(), queryString);
		int i = 0;
		for(String speciesIRI: speciesIRIs){
			System.out.println("Species ["+ ++i+"] IRI: "+speciesIRI);
		}
		return speciesIRIs;
	}
	
	/**
	 * Queries a given repository using SPARQL.
	 * 
	 * @param serverURL
	 * @param repositoryID
	 * @param queryString
	 * @return Set<String> 
	 */
	public List<String> queryRepository(String serverURL, String repositoryID, String queryString)
			throws EBRAgentException {
		List<String> processedResult = new ArrayList<>();
		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			
			/**
			 * Feroz line
			 */
//			repo.initialize();
			
			/**
			 * Added the first line below by @author NK510 (caresssd@hermes.cam.ac.uk)
			 * 
			 */
			repo.init();
				
			/**
			 * Feroz's code
			 */
			RepositoryConnection con = repo.getConnection();
			try {
				System.out.println("Query String:\n" + queryString);
				// Export all statements in the context to System.out, in
				// RDF/XML format
				TupleQuery queryResult = con.prepareTupleQuery(queryString);
				// A QueryResult is also an AutoCloseable resource, so make sure
				// it gets
				// closed when done.
				try (TupleQueryResult result = queryResult.evaluate()) {
					processResult(result, processedResult);
				} finally {
					// Before our program exits, make sure the database is
					// properly shut down.
					repo.shutDown();
				}
			} catch (Exception e) {
				logger.error("Exception occurred.");
				e.printStackTrace();
				throw new EBRAgentException("Exception occurred.");
			} finally {
				logger.info("Executed the command to close the connection to the repository");
				con.close();
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
			throw new EBRAgentException("RDF4JException occurred.");
		}
		return processedResult;
	}
	
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param localHostSparqlEndPoint the localhost sparql endpoint.
	 * @param claudiusServerSparqlEndPoint the remote repository sparql.
	 * @param query the query string.
	 * @return the set of sparql results.
	 * @throws Exception 
	 */
	
	public LinkedList<SpeciesBean> runFederatedQueryRepositories(String localHostSparqlEndPoint, String claudiusServerSparqlEndPoint, String query) throws Exception {
		
	LinkedList<SpeciesBean> nistSpeciesIdList = new LinkedList<SpeciesBean>();
		
    Repository repository = FedXFactory.newFederation()
    
    
    		/**
    		 * 
    		 * @author NK510 
    		 * a sparql endpoint on localhost
    		 * 
    		 */
	    .withSparqlEndpoint(localHostSparqlEndPoint)
	    
	    /**
	     * 
	     * @author NK510
	     * a sparql endpoint on Caludius server .
	     * 
	     */
        .withSparqlEndpoint(claudiusServerSparqlEndPoint)
		.create();
    
    try {
		

    	
	RepositoryConnection conn = repository.getConnection();
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * Returns a result of federated sparql query via ontospecieskb repositories stored on local host and on Claudius server.
	 * 
	 */
	
	TupleQuery tq = conn.prepareTupleQuery(query);
	
	try{
		
	TupleQueryResult tqRes = tq.evaluate();
	
	while (tqRes.hasNext()) {
				
				BindingSet bSet = tqRes.next();
				
				/**
				 * 
				 * @author NK510 (caresssd@hermes.cam.ac.uk)
				 * Stores query results into NISTSpeciesId bean: cas reg number, atomic bond, geometry, enthalpy of formation, scf energy, zero point energy.
				 * 
				 */		
//				SpeciesBean nistSpeciesId= new SpeciesBean(bSet.getValue("crid").stringValue());
				
				SpeciesBean nistSpeciesId = new SpeciesBean(
						bSet.getValue("crid").stringValue(), 
						bSet.getValue("atomicBond").stringValue(),
						bSet.getValue("geometry").stringValue(),
						bSet.getValue("enthalpyOfFormationValue").stringValue(),
						bSet.getValue("scfEnergyValue").stringValue(),
						bSet.getValue("zeroEnergyValue").stringValue());
				
				nistSpeciesIdList.add(nistSpeciesId);
	}
	
	}catch(TupleQueryResultHandlerException e) {
	
		e.printStackTrace();
		
	}
		conn.close();
		
	}catch(RepositoryException e) {
		
		e.printStackTrace();
	}
    
	repository.shutDown();	

	return nistSpeciesIdList;
	
	}	
	
	
	/**
	 * Builds a query to retrieve the geometry of species.</br> 
	 * 
	 * @param ONTOCHEM this contains ontochem prefix and IRI.
	 * @return String query
	 */
	public String formGeometryQuery(String prefixBindingOntoSpecies, String speciesIRI){
		String queryString = prefixBindingOntoSpecies;
		queryString = queryString.concat("SELECT ?geometry ?atomicBond \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(speciesIRI).concat(" OntoSpecies:hasGeometry ?geometry . \n");
		queryString = queryString.concat("  ").concat(speciesIRI).concat(" OntoSpecies:hasAtomicBond ?atomicBond . \n");
		queryString = queryString.concat("	}");
		return queryString;
	}
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param ontoSpeciesIRI onto species iri given is jos input file
	 * @param ontoCompChemIRI ontocompchem iri given in json input file
	 * @return sparql query string.
	 * 
	 */
	public String getSpeciesQueryFromJsonInput(String ontoSpeciesIRI, String ontoCompChemIRI){
		
			String query ="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
					+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
					+ "PREFIX gc: <http://purl.org/gc/> "
					+ "SELECT DISTINCT ?crid ?atomicBond ?geometry ?enthalpyOfFormationValue  ?scfEnergyValue ?zeroEnergyValue "
					+ "WHERE { "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:casRegistryID ?crid . "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:hasAtomicBond ?atomicBond . "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:hasGeometry ?geometry . "
					+ "<"+ontoSpeciesIRI+"> OntoSpecies:hasStandardEnthalpyOfFormation ?enthalpy . "
					+ "?enthalpy OntoSpecies:value ?enthalpyOfFormationValue ."
					+ "<"+ontoCompChemIRI+"> ontocompchem:hasUniqueSpecies <"+ontoSpeciesIRI+"> . "
					+ "<"+ontoCompChemIRI+"> gc:isCalculationOn ?scfEnergy . "
					+ "?scfEnergy a ontocompchem:ScfEnergy . "
					+ "?scfEnergy gc:hasElectronicEnergy ?scfElectronicEnergy . "
					+ "?scfElectronicEnergy gc:hasValue ?scfEnergyValue . "
					+ "<"+ontoCompChemIRI+"> gc:isCalculationOn ?zeroEnergy . "
					+ "?zeroEnergy a ontocompchem:ZeroPointEnergy . "
					+ "?zeroEnergy gc:hasElectronicEnergy ?zeroElectronicEnergy . "
					+ "?zeroElectronicEnergy gc:hasValue ?zeroEnergyValue . "
					+ "}";
			
			return query;
	}
	
	
	public static String getGaussianFileIRI(String ontoCompChemIRI) {
		
		
		String query="PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> "
				+ "PREFIX ontocompchem: <http://www.theworldavatar.com/ontology/ontocompchem/ontocompchem.owl#> "
				+ "PREFIX gc: <http://purl.org/gc/> "
				+ "SELECT DISTINCT ?outputFile "
				+ "WHERE { "
				+ "<"+ontoCompChemIRI+"> ontocompchem:hasEnvironment ?environment . "
				+ "?environment ontocompchem:hasProgram ?program . "
				+ "?environment gc:hasOutputFile ?outputFile . "
				+ "FILTER(REGEX(str(?outputFile),'.g09') || REGEX(str(?outputFile),'.log') || REGEX(str(?outputFile),'.g16')) ."
				+ "FILTER(str(?program)='Gaussian') ."
				+ "}";
		
		return query;
	}
	
	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param prefixBindingOntoSpecies
	 * @param uniqueSpeciesIRI
	 * @return String query  
	 */
	public String getCASRegistryID (String prefixBindingOntoSpecies, String uniqueSpeciesIRI){
		
		
		String queryString = prefixBindingOntoSpecies;
		
		queryString = queryString.concat("SELECT ?speciesIRI ?casRegID");
		queryString = queryString.concat("WHERE { \n ");
		queryString = queryString.concat("  ").concat(uniqueSpeciesIRI).concat(" OntoSpecies:casRegistryID ?casRegID . \n");
		
		return queryString; 
	}
	
	
	
	public String formAllSpeciesQuery(String prefixBindingRDF, String prefixBindingOntoSpecies){
		String queryString = prefixBindingRDF;
		queryString = queryString.concat(prefixBindingOntoSpecies);
		queryString = queryString.concat("SELECT ?speciesIRI \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ?speciesIRI rdf:type OntoSpecies:Species . \n");
		queryString = queryString.concat("	}");
		return queryString;		
	}
	
	/**
	 * Removes the OWL API dependent extra characters from the result.
	 * 
	 * @param result
	 * @param processedResult
	 */
	private void processResult(TupleQueryResult result, List<String> processedResult) {
		// we just iterate over all solutions in the result...
		while (result.hasNext()) {
			BindingSet solution = result.next();
			for (String bindingName : solution.getBindingNames()) {
				
				System.out.println("bindingName: " + bindingName);
				
				processedResult.add(removeDataType(solution.getValue(bindingName).toString()));
				
			}
		}
	}
	
	/**
	 * Removes the following XML Schema data types from a string:</br>
	 * 1. string</br>
	 * 2. integer</br>
	 * 3. float</br>
	 * 4. double.
	 * 
	 * @param value
	 * @return
	 */
	private String removeDataType(String value){
		String stringType = "^^<http://www.w3.org/2001/XMLSchema#string>";
		String integerType = "^^<http://www.w3.org/2001/XMLSchema#integer>";
		String floatType = "^^<http://www.w3.org/2001/XMLSchema#float>";
		String doubleType = "^^<http://www.w3.org/2001/XMLSchema#double>";
		if(value.contains(stringType)){
			value = value.replace(stringType, "");
		} else if(value.contains(integerType)){
			value = value.replace(integerType, "");
			value = replaceInvertedComma(value);
		} else if(value.contains(floatType)){
			value = value.replace(floatType, "");
			value = replaceInvertedComma(value);
		} else if(value.contains(doubleType)){
			value = value.replace(doubleType, "");
			value = replaceInvertedComma(value);
		} else if(value.startsWith("\"") || value.endsWith("\"")){
			value = value.replace("\"", "");
		}
		return value;
	}
	
	/**
	 * Removes inverted commas from a string.
	 * 
	 * @param value
	 * @return
	 */
	private String replaceInvertedComma(String value){
		if(value.contains("\"")){
			value = value.replace("\"", "");
		}
		return value;
	}
	
	/**
	 * 
	 * @param serverUrl the server url
	 * @param ontoCompChemIRI the ontocompchem species IRIs
	 * @return the list of 
	 */
	public LinkedList<String> queryOntoCompChemSpeciesRepository(String serverUrl, String ontoCompChemIRI) {

		LinkedList<String> outputGaussianFileIRIList = new LinkedList<String>();
		
		String queryString = getGaussianFileIRI(ontoCompChemIRI);
		
		System.out.println("queryString: " + queryString);
		
		Repository repository = new HTTPRepository(serverUrl);

		repository.init();

		RepositoryConnection connection = repository.getConnection();

		System.out.println("connection.isActive(): " + connection.isActive());
		
		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				System.out.println("result: " + result);
				
				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					String gaussianFile = bindingSet.getValue("outputFile").stringValue();
					
					System.out.println("gaussian File: " + gaussianFile);
					
					outputGaussianFileIRIList.add(gaussianFile);

				}

				connection.commit();

			} catch (Exception e) {

				System.out.println(e.getMessage());

			} finally {

				result.close();
			}

		} catch (RepositoryException e) {

			System.out.println(e.getMessage());

			connection.rollback();

		} finally {

			connection.close();

			repository.shutDown();
		}
		
		return outputGaussianFileIRIList;
	}
/**
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * @param referenceSpeciesList the List<Map> of reference species.
 * @return linked list of Gaussian files URL.
 */
public LinkedList<String> getGaussianSpeciesSPARQLResults(List<Map<String, Object>> referenceSpeciesList){
	
	LinkedList<String> ontoCompChemIRIList =null;
	
	for(Map<String, Object> map: referenceSpeciesList) {
		
		ontoCompChemIRIList = new LinkedList<String>();
		
		for(Map.Entry<String, Object> m : map.entrySet()) {
			
			if(m.getKey().matches("ontocompchemIRI")) {
			
			System.out.println("m.getKey(): " + m.getKey());
			System.out.println("m.getValue(): " + m.getValue());
			
			String speciesIRI =m.getValue().toString();
				
			ontoCompChemIRIList.add(speciesIRI);
			
			} else {
				
				continue;
			}
		}
		
		
	}
	
	return ontoCompChemIRIList;
	
}


}
