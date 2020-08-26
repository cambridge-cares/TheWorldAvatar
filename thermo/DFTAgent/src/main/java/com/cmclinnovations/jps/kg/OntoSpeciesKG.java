package com.cmclinnovations.jps.kg;

import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import org.apache.log4j.Logger;
import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import com.cmclinnovations.jps.agent.configuration.DFTAgentProperty;
import com.cmclinnovations.jps.agent.quantum.calculation.DFTAgent;
import com.cmclinnovations.jps.agent.quantum.calculation.DFTAgentException;
import com.cmclinnovations.jps.agent.quantum.calculation.Property;

import uk.ac.cam.cares.jps.base.slurm.job.configuration.SlurmJobProperty;


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
	
	public static void main(String[] args) throws ServletException, DFTAgentException{
//		OntoSpeciesKG ontoSpeciesKG = new OntoSpeciesKG();
//		DFTAgent dftAgent = new DFTAgent();
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
	 * @throws DFTAgentException
	 */
	public String querySpeciesGeometry(String speciesIRI, DFTAgentProperty dftAgentProperty) throws DFTAgentException{
		String speciesGeometry = null;
		if(!speciesIRI.trim().startsWith("<") && !speciesIRI.trim().endsWith(">")){
			speciesIRI = "<".concat(speciesIRI).concat(">");
		}
		String queryString = formGeometryQuery(Property.PREFIX_BINDING_ONTOSPECIES.getPropertyName(), speciesIRI);
		System.out.println("QueryString:"+queryString+"\n");
		List<String> testResults = queryRepository(dftAgentProperty.getRdf4jServerURL(), Property.RDF4J_ONTOSPECIES_REPOSITORY_ID.getPropertyName(), queryString);
		for(String testResult: testResults){
//			System.out.println("Test Result:\n"+testResult);
			speciesGeometry = testResult;
		}
		return speciesGeometry;
	}

	/**
	 * Queries all species available in the OntoSpecies Knowledge Graph.
	 * 
	 * @return
	 * @throws DFTAgentException
	 */
	public List<String> getAllSpecies() throws DFTAgentException{
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
			throws DFTAgentException {
		List<String> processedResult = new ArrayList<>();
		try {
			Repository repo = new HTTPRepository(serverURL, repositoryID);
			repo.initialize();
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
				throw new DFTAgentException("Exception occurred.");
			} finally {
				logger.info("Executed the command to close the connection to the repository");
				con.close();
			}
		} catch (RDF4JException e) {
			logger.error("RDF4JException occurred.");
			e.printStackTrace();
			throw new DFTAgentException("RDF4JException occurred.");
		}
		return processedResult;
	}
	
	/**
	 * Builds a query to retrieve the geometry of species.</br> 
	 * 
	 * @param ONTOCHEM this contains ontochem prefix and IRI.
	 * @return String query
	 */
	public String formGeometryQuery(String prefixBindingOntoSpecies, String speciesIRI){
		String queryString = prefixBindingOntoSpecies;
		queryString = queryString.concat("SELECT ?geometry \n");
		queryString = queryString.concat("WHERE { \n");
		queryString = queryString.concat("    ").concat(speciesIRI).concat(" OntoSpecies:hasGeometry ?geometry . \n");
		queryString = queryString.concat("	}");
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

}
