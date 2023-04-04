package uk.ac.cam.cares.jps.base.converter;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.validator.routines.UrlValidator;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
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
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.vocab.OWL2Datatype;
import org.slf4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

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
public class TBoxManagement extends TBoxGeneration implements ITBoxManagement{
	private Logger logger = org.slf4j.LoggerFactory.getLogger(TBoxManagement.class);
	public OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	public OWLOntology ontology;
	public IRI ontologyIRI;
	public static TBoxConfiguration tBoxConfig;
	public static ApplicationContext applicationContext;
	public String SLASH = "/";
	public String BACKSLASH = "\\";
	public String FILE_EXT_OWL = ".owl";
	public String FILE_EXT_RDF = ".rdf";
	
	public static final String HTTP_PROTOCOL="http://";
	public static final String HTTPS_PROTOCOL="https://";			
	
	public static final String OWL_VERSIONINFO = "versionInfo";
	public static final String OWL_URL = "http://www.w3.org/2002/07/owl#";
	
	/**
	 * Creates an OWL class using the name provided. If the name of the parent 
	 * class is also provided, it creates the subClassOf relation as well.
	 * 
	 * @param className
	 * @param targetName
	 * @param relation
	 * @throws JPSRuntimeException
	 */
	public void createOWLClass(String className, String targetName, String relation) throws JPSRuntimeException{
		// Checks if the class name is null or empty
		checkClassName(className);
		// Creates the child class.
		OWLClass child = createClass(className);
		OWLClass parent = null;
		if (targetName != null && !targetName.isEmpty() && relation!=null && !relation.isEmpty()) {
			// Creates the target class.
			parent = createClass(targetName);
			if(relation.equalsIgnoreCase(tBoxConfig.getIsARelation())){
				manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(child, parent)));
			} else if(relation.equalsIgnoreCase(tBoxConfig.getEquivalentToRelation())){
				manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLEquivalentClassesAxiom(child, parent)));				
			}
		} else {
			if(tBoxConfig.gettBoxIri()==null || tBoxConfig.gettBoxIri().isEmpty()){
				logger.error("TBox IRI is not provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
				throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			}
			if(ontology==null){
				logger.error("TBox IRI is missing in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
				throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));				
			}
			ontology.add(dataFactory.getOWLDeclarationAxiom(child));
		}
	}

	/**
	 * Adds a label as rdfs:label to the OWL class.
	 * 
	 * @param className
	 * @param label
	 * @throws JPSRuntimeException
	 */
	public void addLabelToOWLClass(String className, String label) throws JPSRuntimeException{
		if(label!=null && !label.isEmpty()){
			// Reads the class from the ontology model. If not available, 
			// it creates the class.
			OWLClass clas = createClass(className);
			OWLAnnotationProperty rdfsLabel = dataFactory.getRDFSLabel();
			OWLAnnotation labelAnnotation = dataFactory.getOWLAnnotation(rdfsLabel, dataFactory.getOWLLiteral(label));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(clas.getIRI(), labelAnnotation)));
		}
	}

	
	/**
	 * Adds the definition as a comment to the OWL class.
	 * 
	 * @param className
	 * @param definition
	 * @throws JPSRuntimeException
	 */
	public void addDefinitionToOWLClass(String className, String definition) throws JPSRuntimeException{
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
	 * Adds a label to the current object property.
	 * 
	 * @param property
	 * @param label
	 * @throws JPSRuntimeException
	 */
	public void addLabelToObjectProperty(String property, String label) throws JPSRuntimeException{
		if(label!=null && !label.isEmpty()){
			// Reads the object property from the ontology model. If not available, 
			// it creates the property.
			OWLObjectProperty objectProperty = createObjectProperty(property);
			OWLAnnotationProperty rdfsLabel = dataFactory.getRDFSLabel();
			OWLAnnotation labelAnnotation = dataFactory.getOWLAnnotation(rdfsLabel, dataFactory.getOWLLiteral(label));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(objectProperty.getIRI(), labelAnnotation)));
		}
	}

	
	/**
	 * Adds the definition of the current object property.
	 * 
	 * @param property
	 * @param definition
	 * @throws JPSRuntimeException
	 */
	public void addDefinitionToObjectProperty(String property, String definition) throws JPSRuntimeException{
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
	 * Adds a label to the current data property.
	 * 
	 * @param property
	 * @param label
	 * @throws JPSRuntimeException
	 */
	public void addLabelToDataProperty(String property, String label) throws JPSRuntimeException{
		if(label!=null && !label.isEmpty()){
			// Reads the data property from the ontology model. If not available, 
			// it creates the property.
			OWLDataProperty dataProperty = createDataProperty(property);
			OWLAnnotationProperty rdfsLabel = dataFactory.getRDFSLabel();
			OWLAnnotation labelAnnotation = dataFactory.getOWLAnnotation(rdfsLabel, dataFactory.getOWLLiteral(label));
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLAnnotationAssertionAxiom(dataProperty.getIRI(), labelAnnotation)));
		}
	}
	
	/**
	 * Adds the definition of the current data property.
	 * 
	 * @param property
	 * @param definition
	 * @throws JPSRuntimeException
	 */
	public void addDefinitionToDataProperty(String property, String definition) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	public void addDefinedByToClass(String className, String url) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	public void addDefinedByToObjectProperty(String property, String url) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	public void addDefinedByToDataProperty(String property, String url) throws JPSRuntimeException{
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
	 * Adds a logical formula to the current object property.
	 * 
	 * @param property
	 * @param quantifier
	 * @param domain
	 * @param range
	 * @throws JPSRuntimeException
	 */
	public void addLogicalFormulaToObjectProperty(String property, String quantifier, String domain, String range)
			throws JPSRuntimeException {
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
	 * @throws JPSRuntimeException
	 */
	private void processUnionOfRelationToAddTypeOfLogicalFormula(OWLObjectProperty objectProperty, String quantifier, OWLClass rangeClass, OWLObjectUnionOf objectUnionOfRanges, String domain, String range) throws JPSRuntimeException {
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
	 * Currently, it supports formulas with the following two quantifiers:<br>
	 * - for all (only)<br>
	 * - exactly 1<br>
	 * - maximum 1<br>
	 * - minimum 1<br>
	 * 
	 * @param objectProperty
	 * @param rangeClass
	 * @param objectUnionOfRanges
	 * @param quantifier
	 * @param singleDomain
	 * @param range
	 * @throws JPSRuntimeException
	 */
	private void decideToAddTypeOfLogicalFormula(OWLObjectProperty objectProperty, OWLClass rangeClass,
			OWLObjectUnionOf objectUnionOfRanges, String quantifier, String singleDomain, String range)
			throws JPSRuntimeException {
		OWLClass domainClass = createClass(singleDomain);
		if (quantifier != null && !quantifier.isEmpty() && quantifier.trim().equalsIgnoreCase("only")) {
			addUniversalQuantification(objectProperty, domainClass, rangeClass, objectUnionOfRanges, range);
		} else if (quantifier != null && !quantifier.isEmpty()
				&& quantifier.trim().equalsIgnoreCase("exactly 1")) {
			addExactlyOneQuantification(objectProperty, domainClass, rangeClass);
		} else if (quantifier !=null && !quantifier.isEmpty()
				&& quantifier.trim().equalsIgnoreCase("minimum 1")){
			addMinimumOneQuantification(objectProperty, domainClass, rangeClass);
		} else if(quantifier !=null && !quantifier.isEmpty()
				&& quantifier.trim().equalsIgnoreCase("maximum 1")){
			addMaximumOneQuantification(objectProperty, domainClass, rangeClass);
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
	 * Adds a logical formula with a minimum 1 quantifier.
	 * 
	 * @param objectProperty
	 * @param domainClass
	 * @param rangeClass
	 */
	private void addMinimumOneQuantification(OWLObjectProperty objectProperty, OWLClass domainClass, OWLClass rangeClass){
		OWLObjectCardinalityRestriction restriction = dataFactory.getOWLObjectMinCardinality(1, objectProperty, rangeClass);
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(domainClass, restriction)));
	}
	
	/**
	 * Adds a logical formula with a maximum 1 quantifier.
	 * 
	 * @param objectProperty
	 * @param domainClass
	 * @param rangeClass
	 */
	private void addMaximumOneQuantification(OWLObjectProperty objectProperty, OWLClass domainClass, OWLClass rangeClass){
		OWLObjectCardinalityRestriction restriction = dataFactory.getOWLObjectMaxCardinality(1, objectProperty, rangeClass);
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubClassOfAxiom(domainClass, restriction)));
	}

	
	/**
	 * Creates a data property using the name provided. If the domain and range
	 * are provided, it creates them as well.
	 * 
	 * @param propertyName
	 * @param type
	 * @param targetName
	 * @param relation
	 * @param domain
	 * @param range
	 * @throws JPSRuntimeException
	 */
	public void createOWLDataProperty(String propertyName, String type, String targetName, String relation, String domain, String range) throws JPSRuntimeException {
		checkPropertyName(propertyName);
			OWLDataProperty dataProperty = createDataProperty(propertyName);
			
			for (String singleType : type.split(",")) {
				switch (singleType.toLowerCase().trim()) {
				case "functional property":
					manager.applyChange(
							new AddAxiom(ontology, dataFactory.getOWLFunctionalDataPropertyAxiom(dataProperty)));
					break;
				}
			}
			
			addDomain(dataProperty, domain);
			addRange(dataProperty, range);
			OWLDataProperty parentProperty = null;
			if (targetName != null && !targetName.isEmpty() && relation!=null && !relation.isEmpty()) {
				// Creates the target property.
				parentProperty = createDataProperty(targetName);
				if(relation.equalsIgnoreCase(tBoxConfig.getIsARelation())){
					manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubDataPropertyOfAxiom(dataProperty, parentProperty)));
				} else if(relation.equalsIgnoreCase(tBoxConfig.getEquivalentToRelation())){
					manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLEquivalentDataPropertiesAxiom(dataProperty, parentProperty)));				
				}
			}
	}
	
	
	/**
	 * Creates an object property using the name provided. If the domain and
	 * range are provided, it creates them as well.
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
	public void createOWLObjectProperty(String propertyName, String type, String targetName, String relation, String domain, String range, String quantifier) throws JPSRuntimeException {
		checkPropertyName(propertyName);
		OWLObjectProperty objectProperty = createObjectProperty(propertyName);

		for (String singleType : type.split(",")) {
			switch (singleType.toLowerCase().trim()) {
			case "reflexive property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLReflexiveObjectPropertyAxiom(objectProperty)));
				break;
			case "transitive property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLTransitiveObjectPropertyAxiom(objectProperty)));
				break;
			case "symmetric property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLSymmetricObjectPropertyAxiom(objectProperty)));
				break;
			case "asymmetric property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLAsymmetricObjectPropertyAxiom(objectProperty)));
				break;
			case "irreflexive property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLIrreflexiveObjectPropertyAxiom(objectProperty)));
				break;
			case "functional property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLFunctionalObjectPropertyAxiom(objectProperty)));
				break;
			case "inverse functional property":
				manager.applyChange(
						new AddAxiom(ontology, dataFactory.getOWLInverseFunctionalObjectPropertyAxiom(objectProperty)));
				break;
			}
		}

		addDomain(objectProperty, domain);
		addRange(objectProperty, range, quantifier);
		OWLObjectProperty parentProperty = null;
		if (targetName != null && !targetName.isEmpty() && relation!=null && !relation.isEmpty()) {
			// Creates the target property.
			parentProperty = createObjectProperty(targetName);
			if(relation.equalsIgnoreCase(tBoxConfig.getIsARelation())){
				manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLSubObjectPropertyOfAxiom(objectProperty, parentProperty)));
			} else if(relation.equalsIgnoreCase(tBoxConfig.getEquivalentToRelation())){
				manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLEquivalentObjectPropertiesAxiom(objectProperty, parentProperty)));				
			} else if(relation.equalsIgnoreCase(tBoxConfig.getInverseOfRelation())){
			manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLInverseObjectPropertiesAxiom(objectProperty, parentProperty)));
			}
		}
	}
	
	/**
	 * Adds the domain(s) to the current data property. 
	 * 
	 * @param dataProperty
	 * @param domain
	 * @throws JPSRuntimeException
	 */
	private void addDomain(OWLDataProperty dataProperty, String domain) throws JPSRuntimeException {
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
	 * @throws JPSRuntimeException
	 */
	private void addDomain(OWLObjectProperty objectProperty, String domain) throws JPSRuntimeException {
		if(domain==null || domain.isEmpty()){
			return;
		}
		if(domain.contains("UNION")){
			addUnionOfDomain(objectProperty, domain.split("UNION"));
		} else if(domain.contains("INTERSECTION")){
			addIntersectionOfDomain(objectProperty, domain.split("INTERSECTION"));
		} else if(domain!=null || !domain.isEmpty()){
			addSingleClassDomain(objectProperty, domain);
		}
	}
	
	/**
	 * Adds the range(s) to the current data property. 
	 * 
	 * @param dataProperty
	 * @param range
	 * @throws JPSRuntimeException
	 */
	private void addRange(OWLDataProperty dataProperty, String range) throws JPSRuntimeException {
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
	 * @throws JPSRuntimeException
	 */
	private void addRange(OWLObjectProperty objectProperty, String range, String quantifier) throws JPSRuntimeException {
		if(range==null || range.isEmpty()){
			return;
		}
		if(range.contains("UNION") && (quantifier==null || quantifier.isEmpty())){
			addUnionOfRange(objectProperty, range.split("UNION"));
		} else if(range.contains("INTERSECTION") && (quantifier==null || quantifier.isEmpty())){
			addIntersectionOfRange(objectProperty, range.split("INTERSECTION"));
		} else if(range!=null || !range.isEmpty()){
			addSingleClassRange(objectProperty, range);
		}
	}
	
	/**
	 * Adds the domain to the current data property.
	 * 
	 * @param dataProperty
	 * @param domains
	 * @throws JPSRuntimeException
	 */
	private void addSingleClassDomain(OWLDataProperty dataProperty, String domain) throws JPSRuntimeException{
		OWLClass owlClass = createClass(domain);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyDomainAxiom(dataProperty, owlClass)));
	}
	
	/**
	 * Adds the domain, which is a single class, to the current object property.
	 * 
	 * @param objectProperty
	 * @param domain
	 * @throws JPSRuntimeException
	 */
	private void addSingleClassDomain(OWLObjectProperty objectProperty, String domain) throws JPSRuntimeException{
		OWLClass owlClass = createClass(domain);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyDomainAxiom(objectProperty, owlClass)));
	}
	
	/**
	 * Adds the ranges(s) to the current data property.
	 * 
	 * @param dataProperty
	 * @param ranges
	 * @throws JPSRuntimeException
	 */
	private void addSingleDataTypeRange(OWLDataProperty dataProperty, String range) throws JPSRuntimeException{
		if(range.trim().startsWith(HTTP_PROTOCOL) || range.trim().startsWith(HTTPS_PROTOCOL)){
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLDataPropertyRangeAxiom(dataProperty, dataFactory.getOWLDatatype(range))));
		}else{
			manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLDataPropertyRangeAxiom(dataProperty, getRange(range))));
		}
	}
	
	/**
	 * Adds the range(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param ranges
	 * @throws JPSRuntimeException
	 */
	private void addSingleClassRange(OWLObjectProperty objectProperty, String range) throws JPSRuntimeException{
		manager.applyChange(new AddAxiom(ontology,
					dataFactory.getOWLObjectPropertyRangeAxiom(objectProperty, createClass(range))));
	}
	
	/**
	 * Adds the range(s) to the current object property.
	 * 
	 * @param objectProperty
	 * @param ranges
	 * @throws JPSRuntimeException
	 */
	private void addUnionOfRange(OWLObjectProperty objectProperty, String[] ranges) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private OWLObjectUnionOf getUnionOfRange(OWLObjectProperty objectProperty, String[] ranges) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private void addIntersectionOfRange(OWLObjectProperty objectProperty, String[] ranges) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private void addUnionOfDomain(OWLObjectProperty objectProperty, String[] domains) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private OWLObjectUnionOf getUnionOfDomain(OWLObjectProperty objectProperty, String[] domains) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private void addIntersectionOfDomain(OWLObjectProperty objectProperty, String[] domains) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private void addUnionOfDomain(OWLDataProperty dataProperty, String[] domains) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private void addIntersectionOfDomain(OWLDataProperty dataProperty, String[] domains) throws JPSRuntimeException{
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
	private OWL2Datatype getRange(String range) {
		switch (range.toLowerCase()) {
		case "string":
			return OWL2Datatype.XSD_STRING;
		case "integer":
			return OWL2Datatype.XSD_INTEGER;
		case "int":
			return OWL2Datatype.XSD_INT;
		case "float":
			return OWL2Datatype.XSD_FLOAT;
		case "double":
			return OWL2Datatype.XSD_DOUBLE;
		case "date time":
			return OWL2Datatype.XSD_DATE_TIME;
		case "date time stamp":
			return OWL2Datatype.XSD_DATE_TIME_STAMP;
		case "boolean":
			return OWL2Datatype.XSD_BOOLEAN;
		case "long":
			return OWL2Datatype.XSD_LONG;
		case "decimal":
			return OWL2Datatype.XSD_DECIMAL;
		case "negative integer":
			return OWL2Datatype.XSD_NEGATIVE_INTEGER;
		case "non negative integer":
			return OWL2Datatype.XSD_NON_NEGATIVE_INTEGER;
		case "non positive integer":
			return OWL2Datatype.XSD_NON_POSITIVE_INTEGER;
		case "rational":
			return OWL2Datatype.OWL_RATIONAL;
		case "real":
			return OWL2Datatype.OWL_REAL;
		case "literal":
			return OWL2Datatype.RDFS_LITERAL;
		case "lang string":
			return OWL2Datatype.RDF_LANG_STRING;
		case "plain literal":
			return OWL2Datatype.RDF_PLAIN_LITERAL;
		case "xml literal":
			return OWL2Datatype.RDF_XML_LITERAL;
		case "any uri":
			return OWL2Datatype.XSD_ANY_URI;
		case "base 64 binary":
			return OWL2Datatype.XSD_BASE_64_BINARY;
		case "byte":
			return OWL2Datatype.XSD_BYTE;
		case "hex binary":
			return OWL2Datatype.XSD_HEX_BINARY;
		case "language":
			return OWL2Datatype.XSD_LANGUAGE;
		case "name":
			return OWL2Datatype.XSD_NAME;
		case "ncname":
			return OWL2Datatype.XSD_NCNAME;
		case "nmtoken":
			return OWL2Datatype.XSD_NMTOKEN;
		case "normalized string":
			return OWL2Datatype.XSD_NORMALIZED_STRING;
		case "positive integer":
			return OWL2Datatype.XSD_POSITIVE_INTEGER;
		case "short":
			return OWL2Datatype.XSD_SHORT;
		case "token":
			return OWL2Datatype.XSD_TOKEN;
		case "unsigned byte":
			return OWL2Datatype.XSD_UNSIGNED_BYTE;
		case "unsigned int":
			return OWL2Datatype.XSD_UNSIGNED_INT;
		case "unsigned long":
			return OWL2Datatype.XSD_UNSIGNED_LONG;
		case "unsigned short":
			return OWL2Datatype.XSD_UNSIGNED_SHORT;
		default:
			logger.warn("The following data type is not valid:" + range
					+ ". Now the converter will proceed with the string data type.");
			return OWL2Datatype.XSD_STRING;
		}
	}
	
	/**
	 * Deals with comma separated class name. It is needed when user wants to
	 * create a class with a preferred label and one or more alternative
	 * labels. 
	 * 
	 * @param className
	 * @return
	 * @throws JPSRuntimeException
	 */
	private OWLClass createClass(String className) throws JPSRuntimeException{
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
	 * @throws JPSRuntimeException
	 */
	private OWLClass createClass(String[] classLabels) throws JPSRuntimeException{
		if(tBoxConfig.gettBoxIri()==null || tBoxConfig.gettBoxIri().isEmpty()){
			logger.error("TBox IRI is not provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
		}
		if(ontology==null){
			logger.error("TBox IRI is missing in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));				
		}
		OWLClass classInOwl = null;
		int labelSequence = 0;
		for (String classLabel : classLabels) {
			if (++labelSequence < 2) {
				checkClassName(classLabel);
				if (classLabel.trim().startsWith(HTTP_PROTOCOL)||classLabel.trim().startsWith(HTTPS_PROTOCOL)) {
					classInOwl = dataFactory.getOWLClass(classLabel.replace(" ", ""));
				} else {
					if(tBoxConfig.gettBoxIri().endsWith(SLASH)) {
						classInOwl = dataFactory.getOWLClass(
								tBoxConfig.gettBoxIri().concat(classLabel.replace(" ", "")));						
					} else{
						classInOwl = dataFactory.getOWLClass(
							tBoxConfig.gettBoxIri().concat("#").concat(classLabel.replace(" ", "")));
					}
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
	 * @throws JPSRuntimeException
	 */
	private OWLDataProperty createDataProperty(String propertyLabel) throws JPSRuntimeException {
		if(tBoxConfig.gettBoxIri()==null || tBoxConfig.gettBoxIri().isEmpty()){
			logger.error("TBox IRI is not provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
		}
		if(ontology==null){
			logger.error("TBox IRI is missing in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));				
		}
		if(propertyLabel.trim().startsWith(HTTP_PROTOCOL)||propertyLabel.trim().startsWith(HTTPS_PROTOCOL)){
			return dataFactory.getOWLDataProperty(propertyLabel.replace(" ", ""));
		}
		if(tBoxConfig.gettBoxIri().endsWith(SLASH)) {
			return dataFactory.getOWLDataProperty(
					tBoxConfig.gettBoxIri().concat(propertyLabel.replace(" ", "")));
		} else {
			return dataFactory.getOWLDataProperty(
				tBoxConfig.gettBoxIri().concat("#").concat(propertyLabel.replace(" ", "")));
		}
	}
	
	/**
	 * Creates an OWL object property using the name. It also applies domain and
	 * range if they are available.
	 * 
	 * @param propertyLabel
	 * @return
	 * @throws JPSRuntimeException
	 */
	private OWLObjectProperty createObjectProperty(String propertyLabel) throws JPSRuntimeException {
		if(tBoxConfig.gettBoxIri()==null || tBoxConfig.gettBoxIri().isEmpty()){
			logger.error("TBox IRI is not provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
		}
		if(ontology==null){
			logger.error("TBox IRI is missing in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));
			throw new JPSRuntimeException("TBox IRI must be provided in the following CSV template file: "+owlFilePath.replace(".owl", ".csv"));				
		}
		if(propertyLabel.trim().startsWith(HTTP_PROTOCOL)||propertyLabel.trim().startsWith(HTTPS_PROTOCOL)){
			return dataFactory.getOWLObjectProperty(propertyLabel.replace(" ", ""));
		}
		if(tBoxConfig.gettBoxIri().endsWith(SLASH)) {
			return dataFactory.getOWLObjectProperty(
					tBoxConfig.gettBoxIri().concat(propertyLabel.replace(" ", "")));			
		} else {
			return dataFactory.getOWLObjectProperty(
				tBoxConfig.gettBoxIri().concat("#").concat(propertyLabel.replace(" ", "")));
		}
	}
	
	/**
	 * Checks if the class name or label is null. It also checks if the 
	 * same is empty. 
	 * 
	 * @param className
	 * @throws JPSRuntimeException
	 */
	private void checkClassName(String className) throws JPSRuntimeException{
		if(className==null){
			logger.error("Class name is null.");
			throw new JPSRuntimeException("Class name is null.");
		}
		if(className.isEmpty()){
			logger.error("Class name is empty.");
			throw new JPSRuntimeException("Class name is empty.");
		}		
	}
 
	/**
	 * Checks if the class name or label is null. It also checks if the 
	 * same is empty. 
	 * 
	 * @param className
	 * @throws JPSRuntimeException
	 */
	private void checkPropertyName(String propertyName) throws JPSRuntimeException{
		if(propertyName==null){
			logger.error("Property name is null.");
			throw new JPSRuntimeException("Property name is null.");
		}
		if(propertyName.isEmpty()){
			logger.error("Property name is empty.");
			throw new JPSRuntimeException("Property name is empty.");
		}		
	}
	
	/**
	 * Initialise variables for reading configuration properties.
	 */
	public void init() throws JPSRuntimeException, OWLOntologyCreationException{
		applicationContext = new AnnotationConfigApplicationContext(SpringConfiguration.class);
		tBoxConfig = applicationContext.getBean(TBoxConfiguration.class);
	}
	
	/**
	 * Instantiates the ontology model after reading the TBox IRI from the CSV template.
	 * 
	 * @throws OWLOntologyCreationException
	 */
	public void instantiateOntologyModel() throws OWLOntologyCreationException{
		if(ontologyIRI==null){
			if(tBoxConfig.gettBoxIri()==null || tBoxConfig.gettBoxIri().isEmpty()){
				logger.error("TBox IRI must be provided.");
				throw new JPSRuntimeException("TBox IRI must be provided.");				
			}
			if(!(new UrlValidator().isValid(tBoxConfig.gettBoxIri()))){
				logger.error("Provided TBox IRI is not a valid:"+tBoxConfig.gettBoxIri());
				throw new JPSRuntimeException("Provided TBox IRI is not a valid:"+tBoxConfig.gettBoxIri());				
			}
			ontologyIRI = IRI.create(tBoxConfig.gettBoxIri());
		}
		ontology = manager.createOntology(ontologyIRI);
		if (ontology == null) {
			logger.error("The requested ontology could not be created.");
			throw new JPSRuntimeException("Ontology could not be created.");
		}
	}
	
	/**
	 * Saves the generated ontology.
	 */
	public void saveOntology(String owlFilePath) throws OWLOntologyStorageException {
		try {
			if(getOntologyFileNameFromIri(tBoxConfig.gettBoxIri())==null 
					|| getOntologyFileNameFromIri(tBoxConfig.gettBoxIri()).isEmpty()){
				throw new OWLOntologyStorageException("Invalid TBox file name provided.");
			}
			File file = new File(owlFilePath);
			// Adding metadata to the ontology.
			representOntologyMetadata();
			// Adding import statements to the ontology.
			if(tBoxConfig.gettBoxImport()!=null && tBoxConfig.gettBoxImport().length()>HTTP_PROTOCOL.length()){
				for(String ontologyBeingImported:tBoxConfig.gettBoxImport().split(",")){
					if(ontologyBeingImported.trim().startsWith(HTTP_PROTOCOL) || ontologyBeingImported.trim().startsWith(HTTPS_PROTOCOL)){
						OWLImportsDeclaration importDeclarationABox = dataFactory.getOWLImportsDeclaration(IRI.create(ontologyBeingImported.trim()));
						manager.applyChange(new AddImport(ontology, importDeclarationABox));
					}
				}
			}
			manager.saveOntology(ontology, manager.getOntologyFormat(ontology), IRI.create(file.toURI()));
			logger.info("The TBox has been saved at the path of " + owlFilePath);
		} catch (OWLOntologyStorageException e) {
			logger.error("The ontology could not be saved.");
			e.printStackTrace();
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		} catch (JPSRuntimeException e){
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
	 * @throws JPSRuntimeException
	 */
	private void addDataProperty(IRI iri, String propertyValue, String individialName) throws JPSRuntimeException {
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
	 * @throws JPSRuntimeException
	 */
	private void addDataProperty(OWLDataProperty identifierProperty, String propertyValue, String individialName) throws JPSRuntimeException {
		OWLLiteral literal = createOWLLiteral(dataFactory, propertyValue);
		OWLIndividual individual;
		if(ontologyIRI.toString().endsWith(SLASH)) {
			individual = dataFactory
					.getOWLNamedIndividual(ontologyIRI.toString().concat(individialName));
		} else {
			individual = dataFactory
				.getOWLNamedIndividual(ontologyIRI.toString().concat("#").concat(individialName));
		}
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(identifierProperty, individual, literal)));
	}
	
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String literal){
		return ontoFactory.getOWLLiteral(literal);
	}
	
	/**
	 * Represents metadata of the ontology.
	 * 
	 * @throws JPSRuntimeException
	 */
	private void representOntologyMetadata() throws JPSRuntimeException{
		representComment();
		representDateOfGeneration();
		representVersion();
		representCommitHash();
	}
	
	/**
	 * Represents a comment about the ontology.
	 * 
	 * @throws JPSRuntimeException
	 */
	private void representComment() throws JPSRuntimeException{
		String comment = tBoxConfig.gettBoxComment();
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
	 * @throws JPSRuntimeException
	 */
	private void representCommitHash() throws JPSRuntimeException{
		//String commitHash = CtmlConverterUtils.gitCommitHash();
		String commitHash = tBoxConfig.getGitCommitHashValue();
		if (commitHash != null && !commitHash.isEmpty()) {
			OWLLiteral commitHashValue = dataFactory.getOWLLiteral(commitHash);
			OWLAnnotationProperty commit;
			if(tBoxConfig.gettBoxIri().endsWith(SLASH)) {
				commit = dataFactory.getOWLAnnotationProperty(IRI.create(tBoxConfig
					.gettBoxIri().concat(tBoxConfig.getCompChemGitCommitHash())));
			} else {
				commit = dataFactory.getOWLAnnotationProperty(IRI.create(tBoxConfig
					.gettBoxIri().concat("#").concat(tBoxConfig.getCompChemGitCommitHash())));
			}
			OWLAnnotation commitAttributeWithValue = dataFactory.getOWLAnnotation(commit, commitHashValue);
			manager.applyChange(new AddOntologyAnnotation(ontology, commitAttributeWithValue));
		}
	}
	
	/**
	 * Represents the current version of the ontology.
	 * 
	 * @throws JPSRuntimeException
	 */
	private void representVersion() throws JPSRuntimeException{
		String version = tBoxConfig.gettBoxVersion();
		if (version != null && !version.isEmpty()) {
			OWLLiteral versionValue = dataFactory.getOWLLiteral(version);
			OWLAnnotationProperty versionProperty = dataFactory.getOWLAnnotationProperty(OWL_URL.concat(OWL_VERSIONINFO));
			OWLAnnotation versionAttributeWithValue = dataFactory.getOWLAnnotation(versionProperty, versionValue);
			manager.applyChange(new AddOntologyAnnotation(ontology, versionAttributeWithValue));
		}
	}
	
	/**
	 * Represents the date on which the ontological TBox is generated from the excel template.
	 * 
	 * @throws JPSRuntimeException
	 */
	private void representDateOfGeneration() throws JPSRuntimeException{
		String date = tBoxConfig.getAnnotationPropertyDate();
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
	
	/**
	 * Extracts the name of the OWL file being created from the IRI of TBox.
	 * 
	 * @param iri
	 * @return
	 */
	private String getOntologyFileNameFromIri(String iri){
		String tokens[] = iri.split(SLASH);
		if(iri.endsWith(SLASH)){
			return tokens[tokens.length-2];
		} else if(iri.contains(SLASH)){
			return tokens[tokens.length-1];
		} else{
			return null;
		}
	}
}
