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
public interface ITBoxGenerationDetailed {
	public void readTBoxTemplate(String csvFileNamePlusPath) throws IOException, OntoException, OWLOntologyCreationException, OWLOntologyStorageException;
	public void generateClass(String className, String classDefinition, String language, String parentName) throws IOException, OntoException;
	public void generateDataProperty(String propertyName, String domain, String range) throws IOException, OntoException;
	public void generateObjectProperty(String propertyName, String domain, String range) throws IOException, OntoException;
}
