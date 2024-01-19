package uk.ac.cam.cares.jps.base.converter;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

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
	 * Initialises the ontology parameter.
	 * 
	 * @throws JPSRuntimeException
	 * @throws OWLOntologyCreationException
	 */
	public void init() throws JPSRuntimeException, OWLOntologyCreationException;

	/**
	 * Instantiates the ontology model.
	 * 
	 * @throws OWLOntologyCreationException
	 */
	public void instantiateOntologyModel() throws OWLOntologyCreationException;
	
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
	 * @throws JPSRuntimeException
	 */
	public void createOWLClass(String className, String targetName, String relation) throws JPSRuntimeException;
	/**
	 * Creates an ontological data property.
	 * 
	 * @param propertyName
	 * @param type
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 * @throws JPSRuntimeException
	 */
	public void createOWLDataProperty(String propertyName, String type, String targetName, String relation, String domain, String range) throws JPSRuntimeException;
	/**
	 * Creates an ontological object property.
	 * 
	 * @param propertyName
	 * @param type
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 * @param quantifier
	 * @throws JPSRuntimeException
	 */
	public void createOWLObjectProperty(String propertyName, String type, String targetName, String relation, String domain, String range, String quantifier) throws JPSRuntimeException;
	
	/**
	 * Adds a label as rdfs:label to the OWL class.
	 * 
	 * @param className
	 * @param label
	 * @throws JPSRuntimeException
	 */
	public void addLabelToOWLClass(String className, String label) throws JPSRuntimeException;
	
	
	/**
	 * Adds the definition as a comment to the OWL class.
	 * 
	 * @param className
	 * @param definition
	 * @throws JPSRuntimeException
	 */
	public void addDefinitionToOWLClass(String className, String definition) throws JPSRuntimeException;
	
	/**
	 * Adds a label to the current object property.
	 * 
	 * @param property
	 * @param label
	 * @throws JPSRuntimeException
	 */
	public void addLabelToObjectProperty(String property, String label) throws JPSRuntimeException;
	
	/**
	 * Adds the definition of the current object property.
	 * 
	 * @param property
	 * @param definition
	 * @throws JPSRuntimeException
	 */
	public void addDefinitionToObjectProperty(String property, String definition) throws JPSRuntimeException;
	
	/**
	 * Adds a label to the current data property.
	 * 
	 * @param property
	 * @param label
	 * @throws JPSRuntimeException
	 */
	public void addLabelToDataProperty(String property, String label) throws JPSRuntimeException;
	
	/**
	 * Adds the definition of the current data property.
	 * 
	 * @param property
	 * @param definition
	 * @throws JPSRuntimeException
	 */
	public void addDefinitionToDataProperty(String property, String definition) throws JPSRuntimeException;	

	/**
	 * Adds the definedBy annotation property to the current OWL class.
	 * 
	 * @param className
	 * @param url
	 * @throws JPSRuntimeException
	 */
	public void addDefinedByToClass(String className, String url) throws JPSRuntimeException;
	
	/**
	 * Adds the definedBy annotation property to the current object property.
	 * 
	 * @param property
	 * @param url
	 * @throws JPSRuntimeException
	 */
	public void addDefinedByToObjectProperty(String property, String url) throws JPSRuntimeException;
	
	/**
	 * Adds the definedBy annotation property to the current data property.
	 * 
	 * @param property
	 * @param url
	 * @throws JPSRuntimeException
	 */
	public void addDefinedByToDataProperty(String property, String url) throws JPSRuntimeException;
	
	/**
	 * 
	 * 
	 * @param property
	 * @param url
	 * @throws JPSRuntimeException
	 */
	/**
	 * Adds a logical formula to the current object property.
	 * 
	 * @param property
	 * @param quantifier
	 * @param domain
	 * @param range
	 * @throws JPSRuntimeException
	 */
	public void addLogicalFormulaToObjectProperty(String property, String quantifier, String domain, String range) throws JPSRuntimeException;
}
