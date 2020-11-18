package com.cmclinnovations.ontochem.model.tboxes;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontochem.model.exception.TBoxManagementException;

/**
 * This provides interface to the following methods:</br>
 * 1. createOWLClass.
 * 2. createOWLDataProperty.
 * 3. createOWLObjectProperty.
 * 
 * @author msff2
 *
 */
public interface ITBoxManagement {
	/**
	 * Initialises the ontology parameter and ontology model.
	 * 
	 * @throws TBoxManagementException
	 * @throws OWLOntologyCreationException
	 */
	public void init() throws TBoxManagementException, OWLOntologyCreationException;
	/**
	 * Saves the generated OWL file under the path species by user in the </br>
	 * kb.ontochem.management.properties.
	 * 
	 * @param owlFilePath
	 * @throws OWLOntologyStorageException
	 */
	public void saveOntology(String owlFilePath) throws OWLOntologyStorageException;
	/**
	 * Crates an ontology class.
	 * 
	 * @param className
	 * @param parentName
	 * @throws TBoxManagementException
	 * @throws TBoxManagementException
	 */
	public void createOWLClass(String className, String parentName) throws TBoxManagementException, TBoxManagementException;
	/**
	 * Creates an ontology data property.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws TBoxManagementException
	 */
	public void createOWLDataProperty(String propertyName, String domain, String range) throws TBoxManagementException;
	/**
	 * Creates an ontology object property.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws TBoxManagementException
	 */
	public void createOWLObjectProperty(String propertyName, String domain, String range) throws TBoxManagementException;
}
