package uk.ac.cam.cares.ebr.manager;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.eclipse.rdf4j.RDF4JException;
import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.model.ValueFactory;
import org.eclipse.rdf4j.query.BindingSet;
import org.eclipse.rdf4j.query.TupleQuery;
import org.eclipse.rdf4j.query.TupleQueryResult;
import org.eclipse.rdf4j.query.TupleQueryResultHandlerException;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

import uk.ac.cam.cares.mapping.species.QueryString;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * Uploads generated owl file into RDF4J triple store (repository).
 * 
 *
 */
public class RepositoryManager {


	public static void uploadOwlFileOnRDF4JRepository(File owlFile, String serverUrl, String ontoCompChemUri, String ontoCompChemNs) { 
		
		ExecutorService executor = Executors.newSingleThreadExecutor();

		Thread threadTask = new Thread(new Runnable() {

			@Override
			public void run() {

				/**
				 * @author nk510 Gets the repository connection.
				 * @param serverUrl remote RDF4J sparql endpoint.
				 * 
				 */

				Repository repository = new HTTPRepository(serverUrl);

				repository.init();

				RepositoryConnection connection = repository.getConnection();

				try {

					/**
					 * @author nk510
					 *         <p>
					 *         Begins a new transaction. Requires commit() or rollback() to be
					 *         called to end of the transaction.
					 *         </p>
					 */

					connection.begin();

					try {

						Resource context =connection.getValueFactory().createIRI(ontoCompChemNs, owlFile.getName()) ;
						
						System.out.println("context: " + context);
						
						/**
						 * @author nk510
						 *         <p>
						 *         Each generated owl file will be stored in RDF4J triple store.
						 *         </p>
						 */
						connection.add(owlFile, ontoCompChemUri, RDFFormat.RDFXML,context);
						
						connection.commit();

					} catch (RepositoryException e) {

						e.printStackTrace();	

						connection.rollback();
					}

				} catch (Exception e) {

				e.getStackTrace();

				}

				connection.close();

				repository.shutDown();
			}

		});

		executor.submit(threadTask);

		ExecutorManager em = new ExecutorManager();

		em.shutdownExecutorService(executor);
		
	}

	/**
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * 
	 * @param serverUrl           the server url
	 * @param casRegId  the cas registiry id for each species            
	 * @return the hash map of cas registry id species and its relation species IRIs. The
	 *                            method queries ontospecieskb repository.
	 * 
	 */
	public static HashMap<String, LinkedList<String>> queryOntoSpeciesRepository(String serverUrl, String casRegId) {

		HashMap<String, LinkedList<String>> casRegIdMap = new HashMap<String, LinkedList<String>>();

		LinkedList<String> speciesIRIList = new LinkedList<String>();

		Repository repository = new HTTPRepository(serverUrl);

		repository.init();

		try {

			RepositoryConnection conn = repository.getConnection();

			/**
			 * 
			 * @author NK510 (caresssd@hermes.cam.ac.uk) Returns a result of sparql query
			 *         via ontospecieskb repositories stored on local host.
			 * 
			 */

			TupleQuery tq = conn.prepareTupleQuery(QueryString.getSpeciesIRI(casRegId));

			try {

				TupleQueryResult tqRes = tq.evaluate();

				while (tqRes.hasNext()) {

					BindingSet bSet = tqRes.next();

					/**
					 * 
					 * @author NK510 (caresssd@hermes.cam.ac.uk) Stores query results into
					 *         NISTSpeciesId bean: species IRIs.
					 * 
					 */

					speciesIRIList.add(bSet.getValue("species").stringValue());

				}

			} catch (TupleQueryResultHandlerException e) {

				e.printStackTrace();
			}

			
			conn.close();

		} catch (RepositoryException e) {

			e.printStackTrace();
		}
		
		repository.shutDown();

		casRegIdMap.put(casRegId, speciesIRIList);

		return casRegIdMap;
	}
	
	
	/**
	* Loads an ontology to the Ontokin KB repository. It also creates</br>
	* a context, which is a necessary feature to delete the mechanism</br>
	* if user wants.
	*
	* @param serverURL
	* @param owlFileName
	* @param owlFilePath
	* @param baseURI
	* @param repositoryID
	* @throws OntoException
	*/
	public static void loadOntology(String serverURL, String owlFileName, String owlFilePath, String repositoryID) throws Exception{
//	http://www.theworldavatar.com/kb/ontokin/
		
		try {
	Repository repo = new HTTPRepository(serverURL, repositoryID);
	repo.init();;
	RepositoryConnection con = repo.getConnection();
	ValueFactory f = repo.getValueFactory();
	org.eclipse.rdf4j.model.IRI context = f.createIRI("http://www.theworldavatar.com/kb/ontocompchem/".concat(owlFileName));
	try {
	URL url = new URL("file:/".concat(owlFilePath).concat(owlFileName));
	con.add(url, url.toString(), RDFFormat.RDFXML, context);
	} finally {
	con.close();
	}
	} catch (RDF4JException e) {
	System.out.println("RDF4JException occurred.");
	e.printStackTrace();
	} catch (IOException e) {
	System.out.println("IOException occurred.");
	e.printStackTrace();
	}
	}
	
	
}
