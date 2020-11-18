package com.cmclinnovations.ontochem.model.tboxes;

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
public interface ITBoxManagementDetailed {
	public void init() throws OntoException, OWLOntologyCreationException;
	public void saveOntology(String owlFilePath) throws OWLOntologyStorageException;
	public void createOWLClass(String className, String classDefinition, String language, String parentName) throws OntoException, OntoException;
	public void createOWLDataProperty(String propertyName, String domain, String range) throws OntoException;
	public void createOWLObjectProperty(String propertyName, String domain, String range) throws OntoException;
}
