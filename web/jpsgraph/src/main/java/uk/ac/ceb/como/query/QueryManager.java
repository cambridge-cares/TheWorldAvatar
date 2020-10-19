package uk.ac.ceb.como.query;

import java.io.IOException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Properties;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.IsolationLevels;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.QueryLanguage;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import uk.ac.ceb.como.properties.PropertiesManager;
import uk.ac.ceb.como.properties.Request;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 *         Implements methods for querying RDF4J repository and Fuseki
 *         repository. 
 *
 */
public class QueryManager {

	static Properties kbProperties = PropertiesManager
			.loadProperties(QueryManager.class.getClassLoader().getResourceAsStream("kb.properties"));

	private static String fusakiUrl = kbProperties.getProperty("fusaki.url.for.world.avatar");

	final static Logger logger = Logger.getLogger(QueryManager.class.getName());

	/**
	 * 
	 * @param repositoryUrl the repository URL
	 * @param queryString the query string.
	 * @return
	 */
	public String getQuery(String repositoryUrl, String queryString) {
		
		ArrayList<String> queryResult = new ArrayList<String>();
		
		System.out.println("getQuery(): repositoryUrl: " + repositoryUrl);
		
		Repository repository = new HTTPRepository(repositoryUrl);
		repository.init();
		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();
					
					queryResult.add(bindingSet.getValue("sum").stringValue());
				}

			} catch (Exception e) {

			System.out.println("Result exception: " + e.getMessage());

			} finally {

			result.close();
			
			}
			
		} catch (RepositoryException e) {

			System.out.println("Repository exception: " + e.getMessage());
			
			connection.rollback();	
			

		} finally {		
			connection.close();
			repository.shutDown();
		}
		
	return queryResult.get(0);

	}

	/**
	 * 
	 * @return the number of agents in OntoAgent. Queries data stored in Fuseki
	 *         server.
	 * @throws IOException
	 */
	public static String getNumberOfAgents() throws IOException {
		String query = QueryString.getNumberOfOntoAgents();
		System.out.println("Query:" + query);
		String httpURL = fusakiUrl.concat(URLEncoder.encode(query, "UTF-8"));
		return Request.get(httpURL);
	}

	/**
	 * 
	 * @param repositoryUrl the repository URL
	 * @param queryString the query String
	 * @return LinkedList of species IRI
	 * @throws IOException
	 */
	public LinkedList<String> getQueryDateStamp(String repositoryUrl, String queryString) throws IOException {

		LinkedList<String> speciesIRIList = new LinkedList<String>();

		System.out.println("getQueryDateStamp: repositoryUrl: " + repositoryUrl);
		
		Repository repository = new HTTPRepository(repositoryUrl);
		repository.init();
		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();

					String speicesIRI = new String();

					speicesIRI = bindingSet.getValue("s").stringValue();

				    System.out.println("speicesIRI: " + speicesIRI);

					speciesIRIList.add(speicesIRI);
				}
				

			} catch (Exception e) {

				System.out.println("Result exception: " +e.getMessage());

			} finally {

				result.close();

			}

		} catch (RepositoryException e) {

			System.out.println("Repository exception: " + e.getMessage());
			
			connection.rollback();
			

		} 
		finally {		
			
			connection.close();
			repository.shutDown();
		}

		connection.close();
		repository.shutDown();
		
		return speciesIRIList;

	}

}
