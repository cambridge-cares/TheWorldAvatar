package com.cmclinnovations.ontochem.model.tboxes;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.model.configuration.OntoKinKB;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.exception.OntoException;

/**
 * This class implemented the methods that were provided in the ITBoxmanagement</br> 
 * interface, namely following methods were implemented:</br>
 * 1. createOWLClass.
 * 2. createOWLDataProperty.
 * 3. createOWLObjectProperty.
 * 
 * @author msff2
 *
 */
public class TBoxManagementDetailed implements ITBoxManagementDetailed{
	private Logger logger = org.slf4j.LoggerFactory.getLogger(TBoxManagementDetailed.class);
	public OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	public OWLOntology ontology;
	public IRI ontologyIRI;
	public OntoKinKB ontoKinTBox;
	public static ApplicationContext applicationContext;
	
	/**
	 * Creates an OWL class using the name provided. If the name of the parent 
	 * class is also provided, it creates the subClassOf relation as well.
	 * 
	 * @param className
	 * @param parentName
	 * @throws OntoException
	 */
	public void createOWLClass(String className, String classDefinition, String language, String parentName) throws OntoException {
		// Checks if the class name is null or empty
		checkClassName(className);
		// Creates the child class.
		OWLClass child = createClass(className);
		dataFactory.getOWLAnnotation(dataFactory.getRDFSIsDefinedBy(), dataFactory.getOWLLiteral(classDefinition, language));
		OWLClass parent = null;
		if (parentName != null) {
			// Creates the parent class.
			parent = createClass(parentName);
			manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(child, parent)));
		} else {
			ontology.add(dataFactory.getOWLDeclarationAxiom(child));
		}
	}
	
	/**
	 * Creates a data property using the name provided. If the domain and range
	 * are provided, it creates them as well.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws OntoException
	 */
	public void createOWLDataProperty(String propertyName, String domain, String range) throws OntoException {
		checkPropertyName(propertyName);
		OWLDataProperty dataProperty = createDataProperty(propertyName);
		addDomain(dataProperty, domain);
		addRange(dataProperty, range);
	}
	
	
	/**
	 * Creates an object property using the name provided. If the domain and
	 * range are provided, it creates them as well.
	 * 
	 * @param propertyName
	 * @param domain
	 * @param range
	 * @throws OntoException
	 */
	public void createOWLObjectProperty(String propertyName, String domain, String range) throws OntoException {
		checkPropertyName(propertyName);
		OWLObjectProperty objectProperty = createObjectProperty(propertyName);
		addDomain(objectProperty, domain);
		addRange(objectProperty, range);
	}
	
	/**
	 * Adds the domain(s) to the current data property. 
	 * 
	 * @param dataProperty
	 * @param domain
	 * @throws OntoException
	 */
	private void addDomain(OWLDataProperty dataProperty, String domain) throws OntoException {
		if(domain==null || domain.isEmpty()){
			return;
		}
		if(domain.contains("UNION")){
			addUnionOfDomain(dataProperty, domain.split("UNION"));
		} else if(domain.contains("INTERSECTION")){
			addIntersectionOfDomain(dataProperty, domain.split("INTERSECTION"));
		}
		else{
			addSingleClassDomain(dataProperty, domain);
		}
	}
	
	/**
	 * Adds the domain(s) to the current object property. 
	 * 
	 * @param objectProperty
	 * @param domain
	 * @throws OntoException
	 */
	private void addDomain(OWLObjectProperty objectProperty, String domain) throws OntoException {
		if(domain==null || domain.isEmpty()){
			return;
		}
		if(domain.contains("UNION")){
			addUnionOfDomain(objectProperty, domain.split("UNION"));
		} else if(domain.contains("INTERSECTION")){
			addIntersectionOfDomain(objectProperty, domain.split("INTERSECTION"));
		}
		else{
			addSingleClassDomain(objectProperty, domain);
		}
	}
	
	/**
	 * Adds the range(s) to the current data property. 
	 * 
	 * @param dataProperty
	 * @param range
	 * @throws OntoException
	 */
	private void addRange(OWLDataProperty dataProperty, String range) throws OntoException {
		if(range==null || range.isEmpty()){
			return;
		}
		if(range.contains("UNION")){
		} else if(range.contains("INTERSECTION")){
		}
		else{
			addSingleDataTypeRange(dataProperty, range);
		}
	}
	
	/**
	 * Adds the range(s) to the current object property. 
	 * 
	 * @param objectProperty
	 * @param range
	 * @throws OntoException
	 */
	private void addRange(OWLObjectProperty objectProperty, String range) throws OntoException {
		if(range==null || range.isEmpty()){
			return;
		}
		if(range.contains("UNION")){
			addUnionOfRange(objectProperty, range.split("UNION"));
		} else if(range.contains("INTERSECTION")){
			addIntersectionOfRange(objectProperty, range.split("INTERSECTION"));
		}
		else{
			addSingleClassRange(objectProperty, range);
		}
	}
	
	/**
	 * Adds the domain to the current data property.
	 * 
	 * @param dataProperty
	 * @param domains
	 * @throws OntoException
	 */
	private void addSingleClassDomain(OWLDataProperty dataProperty, String domain) throws OntoException{
		OWLClass owlClass = createClass(domain);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyDomainAxiom(dataProperty, owlClass)));
	}
	
	/**
	 * Adds the domain, which is a single class, to the current object property.
	 * 
	 * @param objectProperty
	 * @param domain
	 * @throws OntoException
	 */
	private void addSingleClassDomain(OWLObjectProperty objectProperty, String domain) throws OntoException{
		OWLClass owlClass = createClass(domain);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, owlClass)));
	}
	
	/**
	 * Adds the ranges(s) to the current data property.
	 * 
	 * @param dataProperty
	 * @param ranges
	 * @throws OntoException
	 */
	private void addSingleDataTypeRange(OWLDataProperty dataProperty, String range) throws OntoException{
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLDataPropertyRangeAxiom(dataProperty, getRange(range))));
	}
	
	/**
	 * Adds the range(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param ranges
	 * @throws OntoException
	 */
	private void addSingleClassRange(OWLObjectProperty objectProperty, String range) throws OntoException{
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty, createClass(range))));
	}
	
	/**
	 * Adds the range(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param ranges
	 * @throws OntoException
	 */
	private void addUnionOfRange(OWLObjectProperty objectProperty, String[] ranges) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String range: ranges){
			owlClassExpressions.add(createClass(range));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty, dataFactory.getOWLObjectUnionOf(owlClassExpressions))));
	}
	
	/**
	 * Adds the range(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param ranges
	 * @throws OntoException
	 */
	private void addIntersectionOfRange(OWLObjectProperty objectProperty, String[] ranges) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String range: ranges){
			owlClassExpressions.add(createClass(range));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty, dataFactory.getOWLObjectIntersectionOf(owlClassExpressions))));
	}
	
	/**
	 * Adds the domain(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param domains
	 * @throws OntoException
	 */
	private void addUnionOfDomain(OWLObjectProperty objectProperty, String[] domains) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String domain: domains){
			owlClassExpressions.add(createClass(domain));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, dataFactory.getOWLObjectUnionOf(owlClassExpressions))));
	}
	
	/**
	 * Adds the domain(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param domains
	 * @throws OntoException
	 */
	private void addIntersectionOfDomain(OWLObjectProperty objectProperty, String[] domains) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String domain: domains){
			owlClassExpressions.add(createClass(domain));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, dataFactory.getOWLObjectIntersectionOf(owlClassExpressions))));
	}
	
	/**
	 * Adds the domain(s) to the current data property.
	 * 
	 * @param dataProperty
	 * @param domains
	 * @throws OntoException
	 */
	private void addUnionOfDomain(OWLDataProperty dataProperty, String[] domains) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String domain: domains){
			owlClassExpressions.add(createClass(domain));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLDataPropertyDomainAxiom(dataProperty, dataFactory.getOWLObjectUnionOf(owlClassExpressions))));
	}
	
	/**
	 * Adds the domain(s) to the current data property.
	 * 
	 * @param dataProperty
	 * @param domains
	 * @throws OntoException
	 */
	private void addIntersectionOfDomain(OWLDataProperty dataProperty, String[] domains) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String domain: domains){
			owlClassExpressions.add(createClass(domain));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLDataPropertyDomainAxiom(dataProperty, dataFactory.getOWLObjectIntersectionOf(owlClassExpressions))));
	}
	
	/**
	 * Performs a match between the user defined data type and OWL2Datatypes
	 * and returns the one that matches.
	 * 
	 * @param range
	 * @return
	 */
	private OWL2Datatype getRange(String range){
		if(range.equalsIgnoreCase("string")){
			return OWL2Datatype.XSD_STRING;
		} else if(range.equalsIgnoreCase("integer") || range.equalsIgnoreCase("int")){
			return OWL2Datatype.XSD_INTEGER;
		} else if(range.equalsIgnoreCase("float")){
			return OWL2Datatype.XSD_FLOAT;
		} else if(range.equalsIgnoreCase("double")){
			return OWL2Datatype.XSD_DOUBLE;
		} else if(range.equalsIgnoreCase("datetime")){
			return OWL2Datatype.XSD_DATE_TIME;
		} else if(range.equalsIgnoreCase("timestamp")){
			return OWL2Datatype.XSD_DATE_TIME_STAMP;
		}
		return OWL2Datatype.XSD_STRING;
	}
	
	/**
	 * Deals with comma separated class name. It is needed when user wants to
	 * create a class with a preferred label and one or more alternative
	 * labels. 
	 * 
	 * @param className
	 * @return
	 * @throws OntoException
	 */
	private OWLClass createClass(String className) throws OntoException{
		String[] classLabels;
		if(className.contains(",")){
			classLabels = className.split(",");			
		} else{
			classLabels = new String[]{className};
		}
		return createClass(classLabels);
	}
	
	/**
	 * Creates an OWL class with multiple labels, of which the first one is 
	 * created as rdfs:label and the rests are created as skos:altLabel(s).
	 * 
	 * @param classLabels
	 * @return
	 * @throws OntoException
	 */
	private OWLClass createClass(String[] classLabels) throws OntoException{
		OWLClass classInOwl = null;
		int labelSequence = 0;
		for (String classLabel : classLabels) {
			if (++labelSequence < 2) {
				checkClassName(classLabel);
				if (classLabel.contains("http://")) {
					classInOwl = dataFactory.getOWLClass(classLabel.replace(" ", ""));
				} else {
					classInOwl = dataFactory.getOWLClass(
							ontoKinTBox.getOntoKinKbTBoxIri().concat("#").concat(classLabel.replace(" ", "")));
				}
			}
		}
		return classInOwl;
	}
	
	/**
	 * Creates an OWL data property using the name. It also applies domain and
	 * range if they are available.
	 * 
	 * @param propertyLabel
	 * @return
	 * @throws OntoException
	 */
	private OWLDataProperty createDataProperty(String propertyLabel) throws OntoException {
		if(propertyLabel.contains("http://")){
			return dataFactory.getOWLDataProperty(propertyLabel.replace(" ", ""));
		}
		return dataFactory.getOWLDataProperty(
				ontoKinTBox.getOntoKinKbTBoxIri().concat("#").concat(propertyLabel.replace(" ", "")));
	}
	
	/**
	 * Creates an OWL object property using the name. It also applies domain and
	 * range if they are available.
	 * 
	 * @param propertyLabel
	 * @return
	 * @throws OntoException
	 */
	private OWLObjectProperty createObjectProperty(String propertyLabel) throws OntoException {
		if(propertyLabel.contains("http://")){
			return dataFactory.getOWLObjectProperty(propertyLabel.replace(" ", ""));
		}
		return dataFactory.getOWLObjectProperty(
				ontoKinTBox.getOntoKinKbTBoxIri().concat("#").concat(propertyLabel.replace(" ", "")));
	}
	
	/**
	 * Checks if the class name or label is null. It also checks if the 
	 * same is empty. 
	 * 
	 * @param className
	 * @throws OntoException
	 */
	private void checkClassName(String className) throws OntoException{
		if(className==null){
			logger.error("Class name is null.");
			throw new OntoException("Class name is null.");
		}
		if(className.isEmpty()){
			logger.error("Class name is empty.");
			throw new OntoException("Class name is empty.");
		}		
	}
 
	/**
	 * Checks if the class name or label is null. It also checks if the 
	 * same is empty. 
	 * 
	 * @param className
	 * @throws OntoException
	 */
	private void checkPropertyName(String propertyName) throws OntoException{
		if(propertyName==null){
			logger.error("Property name is null.");
			throw new OntoException("Property name is null.");
		}
		if(propertyName.isEmpty()){
			logger.error("Property name is empty.");
			throw new OntoException("Property name is empty.");
		}		
	}
	
	public void init() throws OntoException, OWLOntologyCreationException{
		if (applicationContext == null) {
			applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		}
		if (ontoKinTBox == null) {
			ontoKinTBox = applicationContext.getBean(OntoKinKB.class);
		}
		if(ontologyIRI==null){
			ontologyIRI = IRI.create(ontoKinTBox.getOntoKinKbTBoxIri());
		}
		ontology = manager.createOntology(ontologyIRI);
		if (ontology == null) {
			logger.error("The requested ontology could not be created.");
			throw new OntoException("Ontology could not be created.");
		}
	}
	
	/**
	 * Saves an ontology created for codifying a chemical mechanism.
	 */
	public void saveOntology(String owlFilePath) throws OWLOntologyStorageException {
		try {
			File file = new File(owlFilePath);
			manager.saveOntology(ontology, manager.getOntologyFormat(ontology), IRI.create(file.toURI()));
		} catch (OWLOntologyStorageException e) {
			logger.error("The ontology could not be saved.");
			e.printStackTrace();
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		}
	}
	
	/**
	 * Attaches a data property value to an instance.
	 * The IRI contains both the name space and the
	 * name of the property. 
	 * 
	 * @param iri
	 * @param propertyValue
	 * @param individialName
	 * @throws OntoException
	 */
	private void addDataProperty(IRI iri, String propertyValue, String individialName) throws OntoException {
		// Reads the data property from the OWL API data factory
		OWLDataProperty identifierProperty = dataFactory.getOWLDataProperty(iri);
		addDataProperty(identifierProperty, propertyValue, individialName);
	}
	
	/**
	 * Attaches a data property value to an instance.
	 * 
	 * @param identifierProperty
	 * @param propertyValue
	 * @param individialName
	 * @throws OntoException
	 */
	private void addDataProperty(OWLDataProperty identifierProperty, String propertyValue, String individialName) throws OntoException {
		OWLLiteral literal = createOWLLiteral(dataFactory, propertyValue);
		OWLIndividual individual = dataFactory
				.getOWLNamedIndividual(ontologyIRI.toString().concat("#").concat(individialName));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(identifierProperty, individual, literal)));
	}
	
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String literal){
		return ontoFactory.getOWLLiteral(literal);
	}
}
