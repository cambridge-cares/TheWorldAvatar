package uk.ac.ceb.como.query;

import java.io.IOException;
import java.net.URLEncoder;
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

public class QueryManager {

	static Properties kbProperties = PropertiesManager.loadProperties(QueryManager.class.getClassLoader().getResourceAsStream("kb.properties"));
	
	private static String fusakiUrl = kbProperties.getProperty("fusaki.url.for.world.avatar");
	
	final static Logger logger = Logger.getLogger(QueryManager.class.getName());
	
	public  String getQuery(String repositoryUrl, String queryString) {
		
		String queryResult = new String();
		
		Repository repository = new HTTPRepository(repositoryUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();

		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();
					
					queryResult =bindingSet.getValue("sum").stringValue();
				}

			} catch (Exception e) {

				e.getMessage();

			} finally {

				result.close();
			}

			connection.commit();

		} catch (RepositoryException e) {

			e.printStackTrace();

			connection.rollback();

		} finally {

			connection.close();

			repository.shutDown();

		}

		return queryResult;

	}
	/**
	 * 
	 * @return the number of agents in OntoAgent. Queries data stored in Fuseki server. 
	 * @throws IOException
	 */
	public static String getNumberOfAgents() throws IOException{
		String query = QueryString.getNumberOfOntoAgents();
		System.out.println("Query:"+query);
		String httpURL = fusakiUrl.concat(URLEncoder.encode(query, "UTF-8"));		
		return Request.get(httpURL);
	}
	
	/**
	 * 
	 * @param repositoryUrl 
	 * @param queryString
	 * @return
	 */
public  LinkedList<String> getQueryDateStamp(String repositoryUrl, String queryString) {
		
		LinkedList<String> speciesIRIList = new LinkedList<String>();
		
		Repository repository = new HTTPRepository(repositoryUrl);

		repository.initialize();

		RepositoryConnection connection = repository.getConnection();
		
		try {

			connection.begin(IsolationLevels.SNAPSHOT_READ);

			TupleQuery tupleQuery = connection.prepareTupleQuery(QueryLanguage.SPARQL, queryString);

			TupleQueryResult result = tupleQuery.evaluate();

			try {

				while (result.hasNext()) {

					BindingSet bindingSet = result.next();
					
					String speicesIRI = new String();
					
					speicesIRI =bindingSet.getValue("s").stringValue();
					
					speciesIRIList.add(speicesIRI);
				}

			} catch (Exception e) {

				e.getMessage();

			} finally {

				result.close();
			}

			connection.commit();

		} catch (RepositoryException e) {

			e.printStackTrace();

			connection.rollback();

		} finally {

			connection.close();
			
			repository.shutDown();

		}

		return speciesIRIList;

	}
}
