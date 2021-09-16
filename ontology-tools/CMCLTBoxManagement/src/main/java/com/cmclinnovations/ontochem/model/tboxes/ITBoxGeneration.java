package com.cmclinnovations.ontochem.model.tboxes;

import java.io.IOException;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontochem.model.exception.TBoxManagementException;

/**
 * This provides interface to the following methods:</br>
 * 1. generateClass</br>
 * 2. generateSubClass</br>
 * 3. generateDataProperty</br>
 * 4. generateObjectProperty</br>
 * 5. generateUnionOfRanges</br>
 * 6. generateUnionOfDomains</br>
 * 7. readTBoxTemplate</br>
 * 
 * @author msff2
 *
 */
public interface ITBoxGeneration {
	/**
	 * Reads a CSV file containing TBoxes (i.e. Classes, object properties and data properties).
	 * 
	 * @param csvFileNamePlusPath
	 * @throws IOException
	 * @throws TBoxManagementException
	 * @throws OWLOntologyCreationException
	 * @throws OWLOntologyStorageException
	 */
	public void readTBoxTemplate(String csvFileNamePlusPath) throws IOException, TBoxManagementException, OWLOntologyCreationException, OWLOntologyStorageException;
	/**
	 * Generates an ontology class.
	 * 
	 * @param className
	 * @param targetName
	 * @param relation
	 * @throws IOException
	 * @throws TBoxManagementException
	 */
	public void generateClass(String className, String targetName, String relation) throws IOException, TBoxManagementException;
	/**
	 * Generates an ontological data property.
	 * 
	 * @param propertyName
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 * @throws IOException
	 * @throws TBoxManagementException
	 */
	public void generateDataProperty(String propertyName, String targetName, String relation, String domain, String range) throws IOException, TBoxManagementException;
	/**
	 * Generates an ontological object property.
	 * 
	 * @param propertyName
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 * @param quantifier
	 * @throws IOException
	 * @throws TBoxManagementException
	 */
	public void generateObjectProperty(String propertyName, String targetName, String relation, String domain, String range, String quantifier) throws IOException, TBoxManagementException;
}