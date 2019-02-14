package uk.ac.cam.cares.jps.thermo.manager;

import java.io.FileOutputStream;
import java.io.OutputStream;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResultHandler;
import org.eclipse.rdf4j.query.resultio.sparqljson.SPARQLResultsJSONWriter;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import uk.ac.cam.cares.jps.thermo.sparql.QueryString;



public class SPARQLManager {

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(SPARQLManager.class.getName());
	
	
	
	public void runCompChemSPARQL(String gaussian, String jsonInputFilePath,String serverUrl ) {
		
		/**
		 * 
		 * Connection to RDF4J triple store and query 'compchem' repository.
		 * 
		 */			
				Repository repository =   new HTTPRepository(serverUrl);
				
				repository.initialize();
					
			    RepositoryConnection connection = repository.getConnection();
			    
				try {
				
				/**
				 * @author NK510
				 * SPARQL query encoded as an object of String.
				 */
	    		String queryString = QueryString.getAllTriplesForThermoCalculation(gaussian);
		
				TupleQuery query = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);				
				
				try {
					
					OutputStream outputJson = new FileOutputStream(jsonInputFilePath);
					
					/**
					 * @author NK510
					 * Serialization of sparql query result as Json file.
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
}
