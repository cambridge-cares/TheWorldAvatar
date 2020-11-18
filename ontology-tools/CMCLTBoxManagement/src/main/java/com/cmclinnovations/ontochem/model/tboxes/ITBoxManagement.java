package com.cmclinnovations.ontochem.model.tboxes;

import java.io.IOException;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontochem.model.exception.OntoException;

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
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	public void init() throws OntoException, OWLOntologyCreationException;
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
	 * @throws OntoException
	 * @throws OntoException
	 */
	public void createOWLClass(String className, String targetName, String relation) throws OntoException, OntoException;
	/**
	 * Creates an ontology data property.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws OntoException
	 */
	public void createOWLDataProperty(String propertyName, String domain, String range) throws OntoException;
	/**
	 * Creates an ontology object property.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @param quantifier
	 * @throws OntoException
	 */
	public void createOWLObjectProperty(String propertyName, String domain, String range, String quantifier) throws OntoException;
	
	/**
	 * Adds the definition as a comment to the OWL class.
	 * 
	 * @param className
	 * @param definition
	 * @throws OntoException
	 */
	public void addDefinitionToOWLClass(String className, String definition) throws OntoException;
	
	/**
	 * Adds the definition of the current object property.
	 * 
	 * @param property
	 * @param definition
	 * @throws OntoException
	 */
	public void addDefinitionToObjectProperty(String property, String definition) throws OntoException;
	
	/**
	 * Adds the definition of the current data property.
	 * 
	 * @param property
	 * @param definition
	 * @throws OntoException
	 */
	public void addDefinitionToDataProperty(String property, String definition) throws OntoException;	

	/**
	 * Adds the definedBy annotation property to the current OWL class.
	 * 
	 * @param className
	 * @param url
	 * @throws OntoException
	 */
	public void addDefinedByToClass(String className, String url) throws OntoException;
	
	/**
	 * Adds the definedBy annotation property to the current object property.
	 * 
	 * @param property
	 * @param url
	 * @throws OntoException
	 */
	public void addDefinedByToObjectProperty(String property, String url) throws OntoException;
	
	/**
	 * Adds the definedBy annotation property to the current data property.
	 * 
	 * @param property
	 * @param url
	 * @throws OntoException
	 */
	public void addDefinedByToDataProperty(String property, String url) throws OntoException;
	
	/**
	 * 
	 * 
	 * @param property
	 * @param url
	 * @throws OntoException
	 */
	/**
	 * Adds a logical formula to the current object property.
	 * 
	 * @param property
	 * @param quantifier
	 * @param domain
	 * @param range
	 * @throws OntoException
	 */
	public void addLogicalFormulaToObjectProperty(String property, String quantifier, String domain, String range) throws OntoException;
}
