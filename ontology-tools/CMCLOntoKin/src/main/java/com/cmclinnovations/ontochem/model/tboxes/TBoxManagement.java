package com.cmclinnovations.ontochem.model.tboxes;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectCardinalityRestriction;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import com.cmclinnovations.ontochem.model.configuration.OntoChemKB;
import com.cmclinnovations.ontochem.model.configuration.OntoKinVocabulary;
import com.cmclinnovations.ontochem.model.configuration.SpringConfiguration;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.utils.CtmlConverterUtils;

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
public class TBoxManagement implements ITBoxManagement{
	private Logger logger = org.slf4j.LoggerFactory.getLogger(TBoxManagement.class);
	public OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	public OWLOntology ontology;
	public IRI ontologyIRI;
	public OntoChemKB ontoChemKB;
	public static ApplicationContext applicationContext;
	public static OntoKinVocabulary appConfigOntoKin;
	
	/**
	 * Creates an OWL class using the name provided. If the name of the parent 
	 * class is also provided, it creates the subClassOf relation as well.
	 * 
	 * @param className
	 * @param targetName
	 * @param relation
	 * @throws OntoException
	 */
	public void createOWLClass(String className, String targetName, String relation) throws OntoException, OntoException{
		// Checks if the class name is null or empty
		checkClassName(className);
		// Creates the child class.
		OWLClass child = createClass(className);
		OWLClass parent = null;
		if (targetName != null && !targetName.isEmpty() && relation!=null && !relation.isEmpty()) {
			// Creates the target class.
			parent = createClass(targetName);
			if(relation.equalsIgnoreCase(appConfigOntoKin.getIsARelation())){
				manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(child, parent)));
			} else if(relation.equalsIgnoreCase(appConfigOntoKin.getEquivalentToRelation())){
				manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLEquivalentClassesAxiom(child, parent)));				
			}
		} else {
			ontology.add(dataFactory.getOWLDeclarationAxiom(child));
		}
	}

	/**
	 * Adds the definition as a comment to the OWL class.
	 * 
	 * @param className
	 * @param definition
	 * @throws OntoException
	 */
	public void addDefinitionToOWLClass(String className, String definition) throws OntoException{
		if(definition!=null && !definition.isEmpty()){
			// Reads the class from the ontology model. If not available, 
			// it creates the class.
			OWLClass clas = createClass(className);
			OWLAnnotationProperty comment = dataFactory.getRDFSComment();
			OWLAnnotation definitionLiteral = dataFactory.getOWLAnnotation(comment, dataFactory.getOWLLiteral(definition));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(clas.getIRI(), definitionLiteral)));
		}
	}
	
	/**
	 * Adds the definition of the current object property.
	 * 
	 * @param property
	 * @param definition
	 * @throws OntoException
	 */
	public void addDefinitionToObjectProperty(String property, String definition) throws OntoException{
		if(definition!=null && !definition.isEmpty()){
			// Reads the object property from the ontology model. If not available, 
			// it creates the property.
			OWLObjectProperty objectProperty = createObjectProperty(property);
			OWLAnnotationProperty comment = dataFactory.getRDFSComment();
			OWLAnnotation definitionLiteral = dataFactory.getOWLAnnotation(comment, dataFactory.getOWLLiteral(definition));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(objectProperty.getIRI(), definitionLiteral)));
		}
	}
	
	/**
	 * Adds the definition of the current data property.
	 * 
	 * @param property
	 * @param definition
	 * @throws OntoException
	 */
	public void addDefinitionToDataProperty(String property, String definition) throws OntoException{
		if(definition!=null && !definition.isEmpty()){
			// Reads the data property from the ontology model. If not available, 
			// it creates the property.
			OWLDataProperty dataProperty = createDataProperty(property);
			OWLAnnotationProperty comment = dataFactory.getRDFSComment();
			OWLAnnotation definitionLiteral = dataFactory.getOWLAnnotation(comment, dataFactory.getOWLLiteral(definition));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(dataProperty.getIRI(), definitionLiteral)));
		}
	}
	
	/**
	 * Adds the definedBy annotation property to the current OWL class.
	 * 
	 * @param className
	 * @param url
	 * @throws OntoException
	 */
	public void addDefinedByToClass(String className, String url) throws OntoException{
		if(url!=null && !url.isEmpty()){
			// Reads the class from the ontology model. If not available, 
			// it creates the class.
			OWLClass clas = createClass(className);
			OWLAnnotationProperty isDefinedBy = dataFactory.getRDFSIsDefinedBy();
			OWLAnnotation definedByLiteral = dataFactory.getOWLAnnotation(isDefinedBy, dataFactory.getOWLLiteral(url));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(clas.getIRI(), definedByLiteral)));
		}
	}
	
	/**
	 * Adds the definedBy annotation property to the current object property.
	 * 
	 * @param property
	 * @param url
	 * @throws OntoException
	 */
	public void addDefinedByToObjectProperty(String property, String url) throws OntoException{
		if(url!=null && !url.isEmpty()){
			// Reads the object property from the ontology model. If not available, 
			// it creates the property.
			OWLObjectProperty objectProperty = createObjectProperty(property);
			OWLAnnotationProperty isDefinedBy = dataFactory.getRDFSIsDefinedBy();
			OWLAnnotation definedByLiteral = dataFactory.getOWLAnnotation(isDefinedBy, dataFactory.getOWLLiteral(url));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(objectProperty.getIRI(), definedByLiteral)));
		}
	}
	
	/**
	 * Adds the definedBy annotation property to the current data property.
	 * 
	 * @param property
	 * @param url
	 * @throws OntoException
	 */
	public void addDefinedByToDataProperty(String property, String url) throws OntoException{
		if(url!=null && !url.isEmpty()){
			// Reads the object property from the ontology model. If not available, 
			// it creates the property.
			OWLDataProperty dataProperty = createDataProperty(property);
			OWLAnnotationProperty isDefinedBy = dataFactory.getRDFSIsDefinedBy();
			OWLAnnotation definedByLiteral = dataFactory.getOWLAnnotation(isDefinedBy, dataFactory.getOWLLiteral(url));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(dataProperty.getIRI(), definedByLiteral)));
		}
	}
	
	/**
	 * Adds the definedBy annotation property to the current object property.
	 * 
	 * @param property
	 * @param quantifier
	 * @param domain
	 * @param range
	 * @throws OntoException
	 */
	public void addLogicalFormulaToObjectProperty(String property, String quantifier, String domain, String range)
			throws OntoException {
		if (quantifier!=null && !quantifier.isEmpty() && domain != null && !domain.isEmpty() && range != null && !range.isEmpty()) {
			// Reads the object property from the ontology model. If not
			// available, it creates the property.
			OWLObjectProperty objectProperty = createObjectProperty(property);
			OWLClass rangeClass = null;
			OWLObjectUnionOf objectUnionOfRanges = null;
			processUnionOfRelationToAddTypeOfLogicalFormula(objectProperty, quantifier, rangeClass, objectUnionOfRanges, domain, range);
		}
	}
	
	/**
	 * Processes the domain and range values to understand if the term UNION appears</br> 
	 * in them. If UNION appears in the domain, it applies the logical formula to</br>
	 * each domain class separately. If UNION appears in the range, it applies the</br>
	 * the formula to the union of the range classes.
	 * 
	 * @param objectProperty
	 * @param quantifier
	 * @param rangeClass
	 * @param objectUnionOfRanges
	 * @param domain
	 * @param range
	 * @throws OntoException
	 */
	private void processUnionOfRelationToAddTypeOfLogicalFormula(OWLObjectProperty objectProperty, String quantifier, OWLClass rangeClass, OWLObjectUnionOf objectUnionOfRanges, String domain, String range) throws OntoException {
		if (range.contains("UNION")) {
			objectUnionOfRanges = getUnionOfRange(objectProperty, range.split("UNION"));
		}else{
			rangeClass = createClass(range);
		}
		for (String singleDomain : domain.split("UNION")) {
			decideToAddTypeOfLogicalFormula(objectProperty, rangeClass, objectUnionOfRanges, quantifier,
					singleDomain, range);
		}
	}
	
	/**
	 * Decides the type of logical formula needs to be created.</br>
	 * Currently, it supports formulas with the following two quantifiers:
	 * - for all (only)
	 * - exactly 1
	 * 
	 * @param objectProperty
	 * @param rangeClass
	 * @param objectUnionOfRanges
	 * @param quantifier
	 * @param singleDomain
	 * @param range
	 * @throws OntoException
	 */
	private void decideToAddTypeOfLogicalFormula(OWLObjectProperty objectProperty, OWLClass rangeClass,
			OWLObjectUnionOf objectUnionOfRanges, String quantifier, String singleDomain, String range)
			throws OntoException {
		OWLClass domainClass = createClass(singleDomain);
		if (quantifier != null && !quantifier.isEmpty() && quantifier.equalsIgnoreCase("only")) {
			addUniversalQuantification(objectProperty, domainClass, rangeClass, objectUnionOfRanges, range);
		} else if (quantifier != null && !quantifier.isEmpty()
				&& quantifier.equalsIgnoreCase("exactly 1")) {
			addExactlyOneQuantification(objectProperty, domainClass, rangeClass);
		}
	}
	
	/**
	 * Adds a logical formula with a universal quantifier.
	 * 
	 * @param objectProperty
	 * @param domainClass
	 * @param rangeClass
	 * @param objectUnionOfRanges
	 * @param range
	 */
	private void addUniversalQuantification(OWLObjectProperty objectProperty, OWLClass domainClass, OWLClass rangeClass, OWLObjectUnionOf objectUnionOfRanges, String range){
		OWLObjectAllValuesFrom restriction;
		if (range.contains("UNION")) {
			restriction = dataFactory.getOWLObjectAllValuesFrom(objectProperty, objectUnionOfRanges);
		} else {
			restriction = dataFactory.getOWLObjectAllValuesFrom(objectProperty, rangeClass);
		}
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(domainClass, restriction)));
	}
	
	/**
	 * Adds a logical formula with an exactly 1 quantifier.
	 * 
	 * @param objectProperty
	 * @param domainClass
	 * @param rangeClass
	 */
	private void addExactlyOneQuantification(OWLObjectProperty objectProperty, OWLClass domainClass, OWLClass rangeClass){
		OWLObjectCardinalityRestriction restriction;
		restriction = dataFactory.getOWLObjectExactCardinality(1, objectProperty, rangeClass);
		manager.applyChange(
				new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(domainClass, restriction)));
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
	 * @param quantifier
	 * @throws OntoException
	 */
	public void createOWLObjectProperty(String propertyName, String domain, String range, String quantifier) throws OntoException {
		checkPropertyName(propertyName);
		OWLObjectProperty objectProperty = createObjectProperty(propertyName);
		addDomain(objectProperty, domain, quantifier);
		addRange(objectProperty, range, quantifier);
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
	 * @param quantifier
	 * @throws OntoException
	 */
	private void addDomain(OWLObjectProperty objectProperty, String domain, String quantifier) throws OntoException {
		if(domain==null || domain.isEmpty()){
			return;
		}
		if(domain.contains("UNION")){
			addUnionOfDomain(objectProperty, domain.split("UNION"));
		} else if(domain.contains("INTERSECTION")){
			addIntersectionOfDomain(objectProperty, domain.split("INTERSECTION"));
		} else if(quantifier==null || quantifier.isEmpty()){
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
	 * @param quantifier
	 * @throws OntoException
	 */
	private void addRange(OWLObjectProperty objectProperty, String range, String quantifier) throws OntoException {
		if(range==null || range.isEmpty()){
			return;
		}
		if(range.contains("UNION") && (quantifier==null || quantifier.isEmpty())){
			addUnionOfRange(objectProperty, range.split("UNION"));
		} else if(range.contains("INTERSECTION") && (quantifier==null || quantifier.isEmpty())){
			addIntersectionOfRange(objectProperty, range.split("INTERSECTION"));
		}else if(quantifier==null || quantifier.isEmpty()){
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
	 * Adds the range(s) to the current object property and returns the union
	 * of classes, which form the range.
	 * 
	 * @param objectProperty
	 * @param ranges
	 * @throws OntoException
	 */
	private OWLObjectUnionOf getUnionOfRange(OWLObjectProperty objectProperty, String[] ranges) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String range: ranges){
			owlClassExpressions.add(createClass(range));
		}
		return dataFactory.getOWLObjectUnionOf(owlClassExpressions);
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
	 * Adds the domain(s) to the current data property and returns the union 
	 * of classes, which form the domain.
	 * 
	 * @param objectProperty
	 * @param domains
	 * @throws OntoException
	 */
	private OWLObjectUnionOf getUnionOfDomain(OWLObjectProperty objectProperty, String[] domains) throws OntoException{
		Set<OWLClassExpression> owlClassExpressions = new HashSet<>();
		for(String domain: domains){
			owlClassExpressions.add(createClass(domain));
		}
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, dataFactory.getOWLObjectUnionOf(owlClassExpressions))));
		return dataFactory.getOWLObjectUnionOf(owlClassExpressions);
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
							ontoChemKB.getOntoKinKbTBoxIri().concat("#").concat(classLabel.replace(" ", "")));
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
				ontoChemKB.getOntoKinKbTBoxIri().concat("#").concat(propertyLabel.replace(" ", "")));
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
				ontoChemKB.getOntoKinKbTBoxIri().concat("#").concat(propertyLabel.replace(" ", "")));
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
		if (appConfigOntoKin == null) {
			appConfigOntoKin = applicationContext.getBean(OntoKinVocabulary.class);
		}
		if (ontoChemKB == null) {
			ontoChemKB = applicationContext.getBean(OntoChemKB.class);
		}
		if(ontologyIRI==null){
			ontologyIRI = IRI.create(ontoChemKB.getOntoKinKbTBoxIri());
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
			File file = new File(
					ontoChemKB.getOntoChemOntolgyFilePath().concat(ontoChemKB.getOntoChemOntolgyFileName()));
			// Adding metadata to the ontology.
			representOntologyMetadata();
			manager.saveOntology(ontology, manager.getOntologyFormat(ontology), IRI.create(file.toURI()));
			logger.info("The OntoChem TBox has been saved under the path "
					+ ontoChemKB.getOntoChemOntolgyFilePath().concat(ontoChemKB.getOntoChemOntolgyFileName()));
		} catch (OWLOntologyStorageException e) {
			logger.error("The ontology could not be saved.");
			e.printStackTrace();
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		} catch (OntoException e){
			logger.error("The ontology-code commit hash could not be retrieved.");
			e.printStackTrace();
			throw new OWLOntologyStorageException("The ontology-code commit hash could not be retrieved.");			
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
	
	/**
	 * Represents metadata of the ontology.
	 * 
	 * @throws OntoException
	 */
	private void representOntologyMetadata() throws OntoException{
		representComment();
		representDateOfGeneration();
		representVersion();
		representCommitHash();
	}
	
	/**
	 * Represents a comment about the ontology.
	 * 
	 * @throws OntoException
	 */
	private void representComment() throws OntoException{
		String comment = appConfigOntoKin.getAnnotationPropertyValueOfComment();
		if (comment != null && !comment.isEmpty()) {
			OWLLiteral commentValue = dataFactory.getOWLLiteral(comment);
			OWLAnnotationProperty commentProperty = dataFactory.getRDFSComment();
			OWLAnnotation commentPropertyAttributeWithValue = dataFactory.getOWLAnnotation(commentProperty, commentValue);
			manager.applyChange(new AddOntologyAnnotation(ontology, commentPropertyAttributeWithValue));
		}
	}
	
	/**
	 * Represents the current commit hash using OWL.
	 * 
	 * @throws OntoException
	 */
	private void representCommitHash() throws OntoException{
		String commitHash = CtmlConverterUtils.gitCommitHash();
		if (commitHash != null && !commitHash.isEmpty()) {
			OWLLiteral commitHashValue = dataFactory.getOWLLiteral(commitHash);
			OWLAnnotationProperty commit = dataFactory.getOWLAnnotationProperty(IRI.create(ontoChemKB
					.getOntoKinKbTBoxIri().concat("#").concat(appConfigOntoKin.getCompChemGitCommitHash())));
			OWLAnnotation commitAttributeWithValue = dataFactory.getOWLAnnotation(commit, commitHashValue);
			manager.applyChange(new AddOntologyAnnotation(ontology, commitAttributeWithValue));
		}
	}
	
	/**
	 * Represents the current version of the ontology.
	 * 
	 * @throws OntoException
	 */
	private void representVersion() throws OntoException{
		String version = appConfigOntoKin.getAnnotationPropertyValueOfVersion();
		if (version != null && !version.isEmpty()) {
			OWLLiteral versionValue = dataFactory.getOWLLiteral(version);
			OWLAnnotationProperty versionProperty = dataFactory.getOWLAnnotationProperty(CtmlConverterUtils.OWL_URL.concat(CtmlConverterUtils.OWL_VERSIONINFO));
			OWLAnnotation versionAttributeWithValue = dataFactory.getOWLAnnotation(versionProperty, versionValue);
			manager.applyChange(new AddOntologyAnnotation(ontology, versionAttributeWithValue));
		}
	}
	
	/**
	 * Represents the date on which the ontological TBox is generated from the excel template.
	 * 
	 * @throws OntoException
	 */
	private void representDateOfGeneration() throws OntoException{
		String date = appConfigOntoKin.getAnnotationPropertyDate();
		if (date != null && !date.isEmpty()) {
			OWLLiteral dateValue = dataFactory.getOWLLiteral(getCurrentDate());
			OWLAnnotationProperty dateProperty = dataFactory.getOWLAnnotationProperty(date);
			OWLAnnotation versionAttributeWithValue = dataFactory.getOWLAnnotation(dateProperty, dateValue);
			manager.applyChange(new AddOntologyAnnotation(ontology, versionAttributeWithValue));
		}
	}
	
	/**
	 * Generates the current date based on the template dd MMMM yyyy.</br>
	 * Following this, it returns the date.
	 * @return
	 */
	private String getCurrentDate(){
		String pattern = "dd MMMM yyyy";
		SimpleDateFormat simpleDateFormat =new SimpleDateFormat(pattern, new Locale("en", "UK"));
		String date = simpleDateFormat.format(new Date());
		return date;
	}
}
