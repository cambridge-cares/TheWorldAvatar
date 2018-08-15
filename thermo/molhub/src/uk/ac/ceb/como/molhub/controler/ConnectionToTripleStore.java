package uk.ac.ceb.como.molhub.controler;

import org.eclipse.rdf4j.repository.Repository;

import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.http.HTTPRepository;

import org.eclipse.rdf4j.repository.sparql.SPARQLRepository;

/**
 * The Class ConnectionToTripleStore.
 *
 * @author nk510
 */
public class ConnectionToTripleStore {
	
	/**
	 * Gets the repository connection.
	 *
	 * @param serverUrl remote sparql endpoint.
	 * @param graphName name of graph in rdf4j triple store where ontologies are saved.
	 * @return instance of class <p>org.eclipse.rdf4j.repository.RepositoryConnection</p>.
	 */
   public static RepositoryConnection getRepositoryConnection(String serverUrl, String graphName) {
		
		Repository  repository = new HTTPRepository(serverUrl, graphName);
		
		repository.initialize();
		
		RepositoryConnection repositoryConnection = repository.getConnection();
		
		return repositoryConnection;
	}

	/**
	 * Gets the SPARQL end point.
	 *
	 * @param serverUrl the server url
	 * @param graphName the graph name
	 * @return the SPARQL end point (instance of class RepositoryConnection) 
	 * 
	 */
	public static RepositoryConnection getSPARQLRepositoryConnection(String serverUrl, String graphName) {
		
		Repository repository = new SPARQLRepository(serverUrl);		
		repository.initialize();	
		
		RepositoryConnection repositoryConnection = repository.getConnection();
		
		return repositoryConnection;
	}
}