package uk.ac.cam.cares.ebr.repository;

import java.io.File;

import org.eclipse.rdf4j.repository.Repository;
import org.eclipse.rdf4j.repository.RepositoryConnection;
import org.eclipse.rdf4j.repository.RepositoryException;
import org.eclipse.rdf4j.repository.http.HTTPRepository;
import org.eclipse.rdf4j.rio.RDFFormat;

import org.eclipse.rdf4j.model.Resource;

/**
 * 
 * @author NK510 (caresssd@hermes.cam.ac.uk)
 * 
 * The repository manager that uploads owl file to remote or local host rdf4j repository.
 *
 */
public class RepositoryManager {
	
	/**
	 * 
	 * @author NK510 (caresssd@hermes.cam.ac.uk)
	 * @param repositoryUrl the repository url where owl files will be uploaded.
	 * @param folderPath the folder path where owl files are located.
	 * 
	 * Uploads generated (ontospecies) owl files into rdf4j triple store.
	 *  
	 */
	public void getUploadOwlFiles(String repositoryUrl, String folderPath, String nameSpace) {
	
		File[] owlFiles = new File(folderPath).listFiles();
			
		Repository repository = new HTTPRepository(repositoryUrl);
		
		repository.init();
		
		RepositoryConnection repositoryConnection = repository.getConnection();
		
		for(File  f: owlFiles){
			
		try {
			
		repositoryConnection.begin();
			
		try {					
					Resource context =repositoryConnection.getValueFactory().createIRI(nameSpace, f.getName()) ;
					
					repositoryConnection.add(f, repositoryUrl, RDFFormat.RDFXML, context);
					
					repositoryConnection.commit();

				
			}catch(RepositoryException e) {
				
				e.printStackTrace();
				
				repositoryConnection.rollback();
			}
		
	
		}catch (Exception e) {
			
			e.printStackTrace();
		}
		
		}
		repositoryConnection.close();

		repository.shutDown();
	}
}