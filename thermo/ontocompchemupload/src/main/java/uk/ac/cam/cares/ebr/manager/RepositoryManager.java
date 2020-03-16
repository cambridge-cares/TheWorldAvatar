package uk.ac.cam.cares.ebr.manager;

import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.eclipse.rdf4j.model.Resource;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

import uk.ac.cam.cares.ebr.constant.Constants;



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
}
