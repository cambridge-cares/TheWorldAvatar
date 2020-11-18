package com.cmclinnovations.ontochem.model.tboxes;

import java.io.IOException;

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
	 * @param targetName
	 * @param relation
	 * @throws TBoxManagementException
	 * @throws TBoxManagementException
	 */
	public void createOWLClass(String className, String targetName, String relation) throws TBoxManagementException, TBoxManagementException;
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
	 * @param quantifier
	 * @throws TBoxManagementException
	 */
	public void createOWLObjectProperty(String propertyName, String domain, String range, String quantifier) throws TBoxManagementException;
	
	/**
	 * Adds the definition as a comment to the OWL class.
	 * 
	 * @param className
	 * @param definition
	 * @throws TBoxManagementException
	 */
	public void addDefinitionToOWLClass(String className, String definition) throws TBoxManagementException;
	
	/**
	 * Adds the definition of the current object property.
	 * 
	 * @param property
	 * @param definition
	 * @throws TBoxManagementException
	 */
	public void addDefinitionToObjectProperty(String property, String definition) throws TBoxManagementException;
	
	/**
	 * Adds the definition of the current data property.
	 * 
	 * @param property
	 * @param definition
	 * @throws TBoxManagementException
	 */
	public void addDefinitionToDataProperty(String property, String definition) throws TBoxManagementException;	

	/**
	 * Adds the definedBy annotation property to the current OWL class.
	 * 
	 * @param className
	 * @param url
	 * @throws TBoxManagementException
	 */
	public void addDefinedByToClass(String className, String url) throws TBoxManagementException;
	
	/**
	 * Adds the definedBy annotation property to the current object property.
	 * 
	 * @param property
	 * @param url
	 * @throws TBoxManagementException
	 */
	public void addDefinedByToObjectProperty(String property, String url) throws TBoxManagementException;
	
	/**
	 * Adds the definedBy annotation property to the current data property.
	 * 
	 * @param property
	 * @param url
	 * @throws TBoxManagementException
	 */
	public void addDefinedByToDataProperty(String property, String url) throws TBoxManagementException;
	
	/**
	 * 
	 * 
	 * @param property
	 * @param url
	 * @throws TBoxManagementException
	 */
	/**
	 * Adds a logical formula to the current object property.
	 * 
	 * @param property
	 * @param quantifier
	 * @param domain
	 * @param range
	 * @throws TBoxManagementException
	 */
	public void addLogicalFormulaToObjectProperty(String property, String quantifier, String domain, String range) throws TBoxManagementException;
}
