package uk.ac.cam.cares.jps.thermo.manager;

import java.io.FileOutputStream;
import java.io.OutputStream;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandler;
import org.eclipse.rdf4j.query.resultio.sparqljson.SPARQLResultsJSONWriter;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import uk.ac.cam.cares.jps.thermo.sparql.QueryString;

/**
 * 
 * @author NK510
 * 
 *
 */

public class SPARQLManager {

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(SPARQLManager.class.getName());
	
	
	/**
	 * @author NK510
	 * @param gaussian
	 * @param jsonInputFilePath
	 * @param serverUrl
	 */
	public void runCompChemSPARQL(String gaussian, String jsonInputFilePath,String serverUrl ) {
		
		/**
		 * 
		 * Connection to RDF4J triple store and query 'CompChem' repository.
		 * 
		 */			
				
		Repository repository =   new HTTPRepository(serverUrl);
				
				repository.initialize();
					
			    RepositoryConnection connection = repository.getConnection();
			    
				try {
				
				/**
				 * @author NK510
				 * SPARQL query encoded as an object of a String.
				 * 
				 */
	    		String queryString = QueryString.getAllTriplesForThermoCalculation(gaussian);
		
				TupleQuery query = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);				
				
				
				try {
					
					OutputStream outputJson = new FileOutputStream(jsonInputFilePath);
					
					/**
					 * @author NK510
					 * Serialization of SPARQL query result as JSON file.
					 */
					TupleQueryResultHandler writer = new SPARQLResultsJSONWriter(outputJson);
					
					query.evaluate(writer);
					
					outputJson.close();
				
				} catch (Exception e) {
				
					e.getMessage();
					
				}
				
				connection.commit();

			} catch (RepositoryException e) {

				logger.info(e.getMessage());
				
			}  finally {

				connection.close();
				
				repository.shutDown();

			}	
	}
	
	/**
	 * 
	 * @param serverUrl The ontocompchemkb server where generated ontocompchem ontologies are stored.
	 * @param gaussianIRI The IRI of Gaussian calculation. 
	 * @return The species URI that is related to Gaussian calculations.
	 *  
	 */
	public String getUniqueSpeciesUri(String serverUrl, String gaussianIRI) {
		
		
		String speciesUri ="";
		
		Repository repository =   new HTTPRepository(serverUrl);
		
		repository.initialize();
			
	    RepositoryConnection connection = repository.getConnection();
		
		try {
			
			String queryString = QueryString.getUniqueSpeciesUriSPARQL(gaussianIRI);
			
			/**
			 * Change TupleQueryResult	
			 */
			TupleQuery query = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);
				
			TupleQueryResult tqr = query.evaluate();		
			
			while (tqr.hasNext()) {
				
				speciesUri=tqr.next().getValue("speciesUri").stringValue();
				
				logger.info("species-uri: " + speciesUri);
				
			}
			
			connection.commit();
			
		} catch (RepositoryException e) {

			logger.info(e.getMessage());
			
		}  finally {

			connection.close();
			
			repository.shutDown();

		}
		
		logger.info("speciesUri inside getUniqueSpeciesUri: "+ speciesUri);
		
		return speciesUri;	
		
	}
	/**
	 * 
	 * @param The serverUrl The URL of species knowledge graph.
	 * @param The speciesUri The species URI that is used in SPARQL query in order to get enthalpy and temperature for given species URI.
	 * @return The temperature and enthalpy. 
	 */
	public String getEnthalpyOfFormation(String serverUrl, String speciesUri) {
		
		/**
		 * 
		 * Converts old species Uri into new species URI.
		 *  
		 */		
//		String ontoSpeciesUri = "http://www.theworldavatar.com/kb/ontospecies/ontospecies.owl#"+speciesUri.substring(speciesUri.lastIndexOf("#") + 1);
		
//		logger.info("ontoSpeciesUri: " + ontoSpeciesUri);
		
		String hrefEnthalpy = "";
		
		Repository repository = new HTTPRepository(serverUrl);
		
		repository.initialize();
		
		RepositoryConnection connection = repository.getConnection();
		
		try {
			
			String queryString = QueryString.getOntoSpeciesEnthalpyAndTemperatureSPARQL(speciesUri);
			
			TupleQuery query = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);
			
			TupleQueryResult tqr = query.evaluate();
			
			while (tqr.hasNext()) {
			
			String enthalpy = tqr.next().getValue("enthalpy_value").toString();

			String temperature = tqr.next().getValue("temp_value").toString();
			
			
			
			hrefEnthalpy = temperature + " , " + enthalpy; 
					
			}
			
			connection.commit();
			
		}catch(RepositoryException e) {
			
			logger.info(e.getMessage());
			
			
		}finally {
			
			connection.close();
			
			repository.shutDown();
			
		}
		
		logger.info("hrefEnthalpy: " + hrefEnthalpy );
		
		return hrefEnthalpy;
	}
}
