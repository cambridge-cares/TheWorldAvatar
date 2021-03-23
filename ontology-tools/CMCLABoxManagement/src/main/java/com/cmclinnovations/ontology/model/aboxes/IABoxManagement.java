package com.cmclinnovations.ontology.model.aboxes;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

import de.derivo.sparqldlapi.QueryEngine;
/**
 * Declares the methods that are needed to instantiate OWL classes, object</br>
 * properties and data properties.</br>
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IABoxManagement {

	/**
	 * Creates an instance of the class defined in the clasName parameter and<br>
	 * adds it to the given ontology.
	 * 
	 * @param ontology
	 * @param clasName
	 * @param basePathTBox
	 * @param instance
	 * @param basePathABox
	 * @return
	 * @throws ABoxManagementException
	 */
	public OWLIndividual createIndividual(OWLOntology ontology, String clasName, String basePathTBox, String instance, String basePathABox) throws ABoxManagementException;

	
	/**
	 * Creates an instance of a class based on the data provided.</p>
	 * If the instance is not available, it will create the 
	 * instance and add the property.</br>
	 *  
	 * @param clasName
	 * @param instance
	 * @return OWLIndividual
	 * @throws ABoxManagementException
	 */
	public OWLIndividual createIndividual(String clasName, String instance) throws ABoxManagementException;
	
	/**
	 * Add a data property to an instance.</p>
	 * If the instance is not available, it will create the 
	 * instance before adding the property.</br>
	 * 
	 * @param ontology
	 * @param instance
	 * @param basePathTBox
	 * @param dataPropertyIRI
	 * @param dataPropertyValue
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addDataProperty(OWLOntology ontology, String instance, String basePathTBox, IRI dataPropertyIRI, String dataPropertyValue, String propertyType) throws ABoxManagementException;
	
	/**
	 * Add a data property to an already created instance.</p>
	 * 
	 * @param instance
	 * @param dataPropertyIRI
	 * @param dataPropertyValue
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addDataProperty(OWLIndividual instance, IRI dataPropertyIRI, String dataPropertyValue, String propertyType) throws ABoxManagementException;
	/**
	 * Add a data property to an already created instance.</p>
	 * 
	 * @param instance an OWL instance
	 * @param dataPropertyName the name of data property
	 * @param dataPropertyValue the value of data property
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addDataProperty(OWLIndividual instance, String dataPropertyName, String dataPropertyValue, String propertyType) throws ABoxManagementException;
	/**
	 * Add a data property to an instance.</p>
	 * If the instance is not available, it will create the 
	 * instance before adding the property.</br>
	 *  
	 * @param instance
	 * @param dataPropertyIRI
	 * @param dataPropertyValue
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addProperty(String instance, IRI dataPropertyIRI, String dataPropertyValue, String propertyType) throws ABoxManagementException;
	/**
	 * Add a data property to an instance of a class.</p>
	 * If the instance is not available, it will create the 
	 * instance to add the property.</br> 
	 * 
	 * @param instance
	 * @param dataPropertyName
	 * @param dataPropertyValue
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addProperty(String instance, String dataPropertyName, String dataPropertyValue, String propertyType) throws ABoxManagementException;
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyName
	 * @param domainInstanceName
	 * @param rangeInstanceName
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(String objectPropertyName, String domainInstanceName, String rangeInstanceName) throws ABoxManagementException;
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyName
	 * @param domainInstance an instance of the OWLIndividual class
	 * @param rangeInstance an instance of the OWLIndividual class
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(String objectPropertyName, OWLIndividual domainInstance, OWLIndividual rangeInstance) throws ABoxManagementException;
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyIri
	 * @param domainInstanceName
	 * @param rangeInstanceName
	 * @throws ABoxManagementException
	 */

	public void addObjectProperty(IRI objectPropertyIri, String domainInstanceName, String rangeInstanceName) throws ABoxManagementException;
	
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param ontology
	 * @param basePathABox
	 * @param objectPropertyIri
	 * @param domainInstanceName
	 * @param rangeInstanceName
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(OWLOntology ontology, String basePathABox, IRI objectPropertyIri, String domainInstanceName, String rangeInstanceName) throws ABoxManagementException;

	
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyIri
	 * @param domainInstance an instance of the OWLIndividual class
	 * @param rangeInstance an instance of the OWLIndividual class
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(IRI objectPropertyIri, OWLIndividual domainInstance, OWLIndividual rangeInstance) throws ABoxManagementException;
	/**
	 * Saves an ABox ontology in the file system.
	 * 
	 * @param manager
	 * @param ontology
	 * @param tBoxIri
	 * @param ontologyIRI
	 * @throws OWLOntologyStorageException
	 *
	 */
	public void saveOntology(String tBoxIri) throws OWLOntologyStorageException;
	
	/**
	 * Loads an experiment OWL ontology into memory.
	 * 
	 * @param ontologyIRI
	 * @return
	 * @throws ABoxManagementException
	 * @throws OWLOntologyCreationException
	 */
	public OWLOntology loadOntology(IRI ontologyIRI) throws ABoxManagementException, OWLOntologyCreationException;
	/**
	 * Creates an instance of query engine and returns it.
	 * 
	 * @return
	 * @throws ABoxManagementException
	 * @throws OWLOntologyCreationException
	 */
	public QueryEngine createQueryEngine() throws ABoxManagementException, OWLOntologyCreationException;
	/**
	 * Forms and then performs a SPARQL query to read any comment that is
	 * codified using rdfs:comment vocabulary term. 
	 * 
	 * @return the comment about a species
	 */
	public String readComment(String instance);
	
	/**
	 * Forms the SPARQL query to retrieve the types of an instance.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formTypeQueryWithAnInstance(String instance);
	
	/**
	 * Forms the SPARQL query to retrieve the instance belonging to a type 
	 * when the type is given.
	 * 
	 * @param tBoxPrefix
	 * @param tBoxIri
	 * @param type
	 * @return
	 */
	public String formQueryWithAType(String tBoxPrefix, String tBoxIri, String type);
	
	/**
	 * Forms the SPARQL query to retrieve the property value of an instance.
	 * 
	 * @param tBoxPrefix the prefix of the ontology which contains the property being queried
	 * @param tBoxIri the IRI of the ontology which contains the property being queried
	 * @param object the instance whose property is being searched
	 * @param queryItem the property being searched
	 * 
	 * @return the value of the property being searched
	 */
	public String formQueryWithBaseURL(String tBoxPrefix, String tBoxIri, String object, String queryItem);
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formSubjectRetrievalQuery(String object, String property);
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithAStandardVocabulary(String vocabulary, String vocabURL, String object, String property);
	
	/**
	 * Forms and then performs a SPARQL query to read the rdfs:label value of 
	 * any object. 
	 * 
	 * @param instanceOwlId the instance id in the OWL representation of 
	 * a mechanism
	 * @return
	 */
	public String readLabel(String instanceOwlId);
	
	/**
	 * Queries the instances of a class.
	 * 
	 * @param tBoxPrefix
	 * @param tBoxIri
	 * @param claz
	 */
	public void queryInstance(String tBoxPrefix, String tBoxIri, String claz);
	
	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @return the result of the SPARQL query
	 */
	public String performQuery(String q, int type);
	
	/**
	 * Retrieves the name of a mechanism using its OWL instance id.
	 * 
	 * @param instanceId
	 * @return
	 * @throws ABoxManagementException
	 */
	public String queryLabel(String instanceId) throws ABoxManagementException;
	
}
