package com.cmclinnovations.ontochem.model.kb.server;

import java.util.List;

import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * This is an interface to the methods that manage repositories of any RDF4J</br> 
 * triple store. The methods are meant to support the following operations:</br>
 * 1. uploading a mechanism to the triple store.</br>
 * 2. downloading a mechanism from the triple store.</br>
 * 3. deleting a mechanism from the triple store.</br>
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IRepositoryManager {
	public void loadOntology(String serverURL, String mechanismName, String mechanismFilePath, String baseURI, String repositoryID) throws OntoException;
	public void deleteOntology(String serverURL, String mechanismName, String contextURL, String repositoryID) throws OntoException;
	public void downloadOntology(String serverURL, String mechanismName, String contextURL, String repositoryID, String filePath) throws OntoException;
	public String queryRepository(String serverURL, String repositoryID, String queryString) throws OntoException;
	public List<String> queryRepositoryMechanism(String serverURL, String repositoryID, String queryString) throws OntoException;
	public String formReactionMechanismQuery(String ontochemPrefix);
	public String formInstanceValueQuery(String instanceURL, String property, String ontochemPrefix);
}
