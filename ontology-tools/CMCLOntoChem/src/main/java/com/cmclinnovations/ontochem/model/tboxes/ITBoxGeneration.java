package com.cmclinnovations.ontochem.model.tboxes;

import java.io.IOException;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontochem.model.exception.OntoException;

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
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 * @throws OWLOntologyStorageException
	 */
	public void readTBoxTemplate(String csvFileNamePlusPath) throws IOException, OntoException, OWLOntologyCreationException, OWLOntologyStorageException;
	/**
	 * Generates an ontology class.
	 * 
	 * @param className
	 * @param parentName
	 * @throws IOException
	 * @throws OntoException
	 */
	public void generateClass(String className, String parentName) throws IOException, OntoException;
	/**
	 * Generates an ontology data property.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws IOException
	 * @throws OntoException
	 */
	public void generateDataProperty(String propertyName, String domain, String range) throws IOException, OntoException;
	/**
	 * Generates an ontology object property.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws IOException
	 * @throws OntoException
	 */
	public void generateObjectProperty(String propertyName, String domain, String range) throws IOException, OntoException;
}