package com.cmclinnovations.ontokin.model.tboxes;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontokin.model.exception.OntoException;

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
	public void init() throws OntoException, OWLOntologyCreationException;
	public void saveOntology(String owlFilePath) throws OWLOntologyStorageException;
	public void createOWLClass(String className, String parentName) throws OntoException, OntoException;
	public void createOWLDataProperty(String propertyName, String domain, String range) throws OntoException;
	public void createOWLObjectProperty(String propertyName, String domain, String range) throws OntoException;
}
