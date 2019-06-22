package uk.ac.cam.cares.jps.thermo.manager;

import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.apache.log4j.Logger;
import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

public class UploadOntology {

	/** The Constant logger. */
	final static Logger logger = Logger.getLogger(UploadOntology.class.getName());	

	/**
	 * 
	 * @param owlFilePath The owl file that is uploaded in RDF4J triple store
	 * @param serverUrl The server url.
	 * @param ontologyUri The ontology uri. 
	 */
	public void uploadOntoKin(String owlFilePath, String serverUrl, String ontologyUri) {
		
		
		File owlFile = new File (owlFilePath);
		
		ExecutorService executor = Executors.newSingleThreadExecutor();

		Thread threadTask = new Thread(new Runnable() {
			
			@Override
			public void run() {
				// TODO Auto-generated method stub
				
				/**
				 * 
				 * @author nk510 Gets the repository connection.
				 * @param serverUrl remote ontokin sparql endpoint.
				 * 
				 */				
				
				Repository repository = new HTTPRepository(serverUrl);

				repository.initialize();

				RepositoryConnection connection = repository.getConnection();

				try {

					/**
					 * @author nk510
					 *         <p>
					 * 		Begins a new transaction. Requires commit() or rollback() to be
					 *         called to end of the transaction.
					 *         </p>
					 */

					connection.begin();

					try {

						/**
						 * @author nk510
						 *         <p>
						 * 		Each generated owl file will be stored in RDF4J triple store.
						 *         </p>
						 */
						
						connection.add(owlFile, ontologyUri, RDFFormat.RDFXML);

						connection.commit();

					} catch (RepositoryException e) {

						/**
						 * 
						 * @author nk510
						 *         
						 *         If something is wrong during the transaction, it will return a
						 *         message about it.
						 *         
						 * 
						 */

						logger.info("RepositoryException: " + e.getMessage());

						connection.rollback();
					}

				} catch (Exception e) {

					logger.info(e.getStackTrace());

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
