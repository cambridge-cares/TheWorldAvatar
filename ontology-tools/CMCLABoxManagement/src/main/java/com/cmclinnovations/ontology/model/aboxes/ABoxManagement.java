package com.cmclinnovations.ontology.model.aboxes;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory;
import org.slf4j.Logger;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.ontology.model.utils.ABoxManagementUtils;

import de.derivo.sparqldlapi.Query;
import de.derivo.sparqldlapi.QueryEngine;
import de.derivo.sparqldlapi.QueryResult;
import de.derivo.sparqldlapi.exceptions.QueryEngineException;
import de.derivo.sparqldlapi.exceptions.QueryParserException;

/**
 * This class holds all the methods needed to instantiate OWL classes, object</br>
 * properties and data properties. It also holds the following member variables</br>
 * to assist both the ABox ontology generation and Knowledge Base development:</br>
 * 1. dataFactory        : an instance of OWLDataFactory</br>
 * 2. manager            : an instance of OWLOntologyManager</br>
 * 3. ontology           : an instance of OWLOntology created to hold the</br>
 * ABox ontology currently being generated.</br> 
 * 4. kb                 : an instance of OWLOntology created to hold the</br>
 * Knowledge Base (KB) ontology currently being generated. The KB ontology</br>
 * holds links to the TBox and ABoxes using import statements.</br> 
 * 5. ontologyIRI        : an instance of IRI created to store the path</br>
 * where the current ontology will be deployed or reside.</br>
 * 6. kbIRI              : an instance of IRI created to store the path</br>
 * where the current KB ontology will be deployed or reside.</br>
 * 7. ontologyIRIFileSave: an instance of IRI created to store the file</br>
 * system path where the ABox ontology will be stored physically.
 * 8. basePathTBox       : the base path of a TBox ontology.</br>
 * 9. basePathABox       : the base path of an ABox ontology.</br>
 * </br>
 * In addition, this class also holds methods to allow users to</br>
 * perform query on an ontology.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class ABoxManagement implements IABoxManagement{
	private Logger logger = org.slf4j.LoggerFactory.getLogger(ABoxManagement.class);

	public static OWLDataFactory dataFactory = OWLManager.getOWLDataFactory();
	public static OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
	// Create an instance of an OWL API reasoner (we use the OWL API
	// built-in StructuralReasoner)
    public static StructuralReasonerFactory reasonerFactory;
    public static QueryEngine engine;
    public static ArrayList<String> queryResult;
    public static OWLOntology ontology;
	public static OWLOntology kb;
	public static IRI ontologyIRI;
	public static IRI kbIRI;
	public static IRI ontologyIRIFileSave;
	public static String basePathTBox;
	public static String basePathABox;
	
	public static final String EMPTY = "";
	public static final String HASH = "#";
	public static final String SPACE = " ";
	public static final String COLON = ":";
	public static final String UNDERSCORE = "_";
	public static final String BACKSLASH = "/";
	public static final String FRONTSLASH = "\\";
	public static final String RDFS = "rdfs";
	public static final String RDFS_LABEL = "label";
	public static final String RDFS_COMMENT = "comment";
	public static final String RDFS_URL = "http://www.w3.org/2000/01/rdf-schema#";
	public static final String RDF = "rdf";
	public static final String RDF_TYPE = "type";
	public static final String RDF_URL = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";	
	public static final String OWL = "owl";
	public static final String OWL_VERSIONINFO = "versionInfo";
	public static final String OWL_URL = "http://www.w3.org/2002/07/owl#";
	public static final String DUBLIN_CORE = "dc";
	public static final String DUBLIN_CORE_ID = "identifier";
	public static final String DUBLIN_CORE_URL = "http://purl.org/dc/elements/1.1/";

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
	public OWLIndividual createIndividual(String clasName, String instance) throws ABoxManagementException {
		// Checks the validity of the ABox ontology and method parameters
		validateParameter(clasName, instance);
		// Creates a class.
		OWLClass clas = getOWLClass(dataFactory, basePathTBox, clasName);
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Adds to the ontology the instance of the class
		manager.applyChange(new AddAxiom(ontology, dataFactory.getOWLClassAssertionAxiom(clas, individual)));
		return individual;
	}
	
	/**
	 * Add a data property to an already created instance.</p>
	 * 
	 * @param instance
	 * @param dataPropertyIRI
	 * @param dataPropertyValue
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addDataProperty(OWLIndividual instance, IRI dataPropertyIRI, String dataPropertyValue, String propertyType) throws ABoxManagementException {
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyValue, propertyType);
		// Reads the data property
		OWLDataProperty dataProperty = dataFactory
				.getOWLDataProperty(dataPropertyIRI);
		// Adds to the ontology the comment about a mechanism in
		// CTML
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, instance, literal)));
	}
	
	/**
	 * Add a data property to an already created instance.</p>
	 * 
	 * @param instance an OWL instance
	 * @param dataPropertyName the name of data property
	 * @param dataPropertyValue the value of data property
	 * @param propertyType
	 * @throws ABoxManagementException
	 */
	public void addDataProperty(OWLIndividual instance, String dataPropertyName, String dataPropertyValue, String propertyType) throws ABoxManagementException {
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyValue, propertyType);
		// Creates the data property
		OWLDataProperty dataPropertyCreated = createOWLDataProperty(dataFactory, basePathTBox,
				dataPropertyName, HASH);
		// Adds the data property and value to the ABox ontology
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataPropertyCreated, instance, literal)));
	}

	
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
	public void addProperty(String instance, IRI dataPropertyIRI, String dataPropertyValue, String propertyType) throws ABoxManagementException {
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyValue, propertyType);
		// Reads the data property
		OWLDataProperty dataProperty = dataFactory
				.getOWLDataProperty(dataPropertyIRI);
		// Adds the data property and value to the ABox ontology
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataProperty, individual, literal)));
	}
	
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
	public void addProperty(String instance, String dataPropertyName, String dataPropertyValue, String propertyType) throws ABoxManagementException {
		// Creates an instance.
		OWLIndividual individual = createOWLIndividual(dataFactory, basePathABox, instance);
		// Creates the value of the data property being created
		OWLLiteral literal = createOWLLiteral(dataFactory, dataPropertyValue, propertyType);
		// Creates the data property
		OWLDataProperty dataPropertyCreated = createOWLDataProperty(dataFactory, basePathTBox,
				dataPropertyName, HASH);
		// Adds the data property and value to the ABox ontology
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLDataPropertyAssertionAxiom(dataPropertyCreated, individual, literal)));
	}
	
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyName
	 * @param domainInstanceName
	 * @param rangeInstanceName
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(String objectPropertyName, String domainInstanceName, String rangeInstanceName) throws ABoxManagementException {
		// Creates the object property
		OWLObjectProperty objectProperty = dataFactory
				.getOWLObjectProperty(basePathTBox.concat(HASH).concat(objectPropertyName));
		// Creates the domain instance
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox,
				domainInstanceName);
		// Creates the range instance
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstanceName);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));
	}

	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyName
	 * @param domainInstance an instance of the OWLIndividual class
	 * @param rangeInstance an instance of the OWLIndividual class
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(String objectPropertyName, OWLIndividual domainInstance, OWLIndividual rangeInstance) throws ABoxManagementException {
		// Creates the object property
		OWLObjectProperty objectProperty = dataFactory
				.getOWLObjectProperty(basePathTBox.concat("#").concat(objectPropertyName));
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainInstance, rangeInstance)));
	}
	
	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyIri
	 * @param domainInstanceName
	 * @param rangeInstanceName
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(IRI objectPropertyIri, String domainInstanceName, String rangeInstanceName) throws ABoxManagementException {
		// Creates the object property
		OWLObjectProperty objectProperty = dataFactory
				.getOWLObjectProperty(objectPropertyIri);
		// Creates the domain instance
		OWLIndividual domainIndividual = createOWLIndividual(dataFactory, basePathABox,
				domainInstanceName);
		// Creates the range instance
		OWLIndividual rangeIndividual = createOWLIndividual(dataFactory, basePathABox, rangeInstanceName);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainIndividual, rangeIndividual)));
	}

	/**
	 * Add an object property to connect a domain object with a range object.
	 * 
	 * @param objectPropertyIri
	 * @param domainInstance an instance of the OWLIndividual class
	 * @param rangeInstance an instance of the OWLIndividual class
	 * @throws ABoxManagementException
	 */
	public void addObjectProperty(IRI objectPropertyIri, OWLIndividual domainInstance, OWLIndividual rangeInstance) throws ABoxManagementException {
		// Creates the object property
		OWLObjectProperty objectProperty = dataFactory
				.getOWLObjectProperty(objectPropertyIri);
		manager.applyChange(new AddAxiom(ontology,
				dataFactory.getOWLObjectPropertyAssertionAxiom(objectProperty, domainInstance, rangeInstance)));
	}
	
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
	public void saveOntology(String tBoxIri) throws OWLOntologyStorageException {
		try {
			if(tBoxIri==null){
				logger.error("The TBox ontology IRI is null.");
				return;
			}
			// Adds a statement to the ABox ontology to import the TBox.
			OWLImportsDeclaration importDeclaration=OWLManager.getOWLDataFactory().getOWLImportsDeclaration(IRI.create(tBoxIri));
			manager.applyChange(new AddImport(ontology, importDeclaration));
			if(ontologyIRI==null){
				logger.error("While saving the ABox ontology, it found that the ontology IRI is null.");
				return;
			}
			saveOntology(manager, ontologyIRIFileSave);
		} catch (OWLOntologyStorageException e1) {
			logger.error("The ontology could not be saved.");
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		}
	}
	
	/**
	 * Saves the ABox ontology in the file system.
	 * 
	 * @param manager
	 * @param ontologyIRIFileSave
	 * @throws OWLOntologyStorageException
	 */
	private void saveOntology(OWLOntologyManager manager, IRI ontologyIRIFileSave) throws OWLOntologyStorageException{
		// Saves the ontology in the file system
		manager.saveOntology(ontology, ontologyIRIFileSave);
		System.out.println("ontologyIRIFileSave:"+ontologyIRIFileSave.toString());
		logger.info("Started removing TBox from the mechanism OWL file.");
		removeTBox(ontologyIRIFileSave);
		logger.info("Finished removing TBox from the mechanism OWL file.");
	}
	
	/**
	 * Removes TBox elements from the generated ABox ontology.  
	 * 
	 * @param ontologyIRIFileSave
	 */
	private void removeTBox(IRI ontologyIRIFileSave){
		try{
			String owlFileNameTemp = ontologyIRIFileSave.toString().replace("file:/", "").replace(".owl", "_temp.owl");
			String owlFileNameOriginal = ontologyIRIFileSave.toString().replace("file:/", "");
			BufferedReader br = ABoxManagementUtils.openSourceFile(owlFileNameOriginal);
			BufferedWriter bw = new BufferedWriter(new OutputStreamWriter(
					new FileOutputStream(owlFileNameTemp), "UTF-8"));
			String line;
			while ((line = br.readLine()) != null) {
				if ((line.trim().startsWith("<!--") && line.endsWith("-->")) || ((line.contains("<owl:DatatypeProperty")
						|| line.contains("<owl:ObjectProperty") || line.contains("</owl:ObjectProperty")
						|| line.contains("<owl:Class") || line.contains("<rdfs:domain") || line.contains("<rdfs:range")
						|| line.trim().isEmpty()))) {
				} else {
					bw.write(line.concat("\n"));
				}
			}
			bw.close();
			br.close();
			delete(owlFileNameOriginal, owlFileNameTemp);
		}catch(IOException e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Deletes the original file and renames the temporary file to the 
	 * original file name.
	 * 
	 * @param owlFileNameOriginal
	 * @param owlFileNameTemp
	 */
	private void delete(String owlFileNameOriginal, String owlFileNameTemp){
		File fileOriginal = new File(owlFileNameOriginal);
		if(fileOriginal.delete()){
			fileOriginal = new File(owlFileNameOriginal);
			File fileTemp = new File(owlFileNameTemp);
			if(fileTemp.renameTo(fileOriginal)){
			}else{
				logger.error("The temporary ABox ontology file could not be renamed.");
			}
		}else{
			logger.error("The generated original ABox ontology file could not be deleted.");
		}
	}
	
	/**
	 * Checks if clasName and instance are empty. It also checks if the</br>
	 * current ABox ontology management parameters are null or empty.  
	 * 
	 * @param clasName
	 * @param instance
	 * @throws ABoxManagementException
	 */
	private void validateParameter(String clasName, String instance) throws ABoxManagementException{
		validateClasName(clasName);
		validateInstanceName(instance);
		validateOntologyParameters();
	}
	
	/**
	 * Checks if the current class name is empty.
	 * 
	 * @param clasName
	 * @throws ABoxManagementException
	 */
	private void validateClasName(String clasName) throws ABoxManagementException{
		if(clasName == null || clasName.isEmpty()){
			throw new ABoxManagementException("The class name provided to create an instance is empty.");
		}
	}
	
	/**
	 * Checks if the name of the provided instance is empty.
	 * 
	 * @param instanceName
	 * @throws ABoxManagementException
	 */
	private void validateInstanceName(String instanceName) throws ABoxManagementException{
		if(instanceName == null || instanceName.isEmpty()){
			throw new ABoxManagementException("The name of the given instance is empty.");
		}
	}
	/**
	 * Validates if all relevant ontology parameters have non null and </br> 
	 * non empty values.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateOntologyParameters() throws ABoxManagementException{
		validateDataFactory();
		validateOntologyManager();
		validateOntology();
		validateIRI();
		validateBasePath();
	}
	
	/**
	 * Checks if the data factory is null. If it is null, the tool throws</br> 
	 * an exception. If it is not null, the tool continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateDataFactory() throws ABoxManagementException{
		if(dataFactory == null){
			throw new ABoxManagementException("The data factory is null.");
		}
	}
	
	/**
	 * Checks if the ontology manager is null. If it is null, the tool throws</br> 
	 * an exception. If it is not null, the tool continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateOntologyManager() throws ABoxManagementException{
		if(dataFactory == null){
			throw new ABoxManagementException("The ontology manager is null.");
		}
	}
	
	/**
	 * Checks if the current ABox ontology is null. If it is null, the tool throws</br> 
	 * an exception. If it is not null, the tool continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateOntology() throws ABoxManagementException{
		if(ontology == null){
			throw new ABoxManagementException("The ABox ontology being created is null.");
		}
	}
	
	/**
	 * Checks if all relevant ontology IRIS have non empty values.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateIRI() throws ABoxManagementException{
		validateOntologyIRI();
		validateOntologyFileSaveIRI();
	}
	
	/**
	 * Checks if the current ABox ontology IRI is empty. If it is empty, the tool throws</br> 
	 * an exception. If it is not empty, the tool continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateOntologyIRI() throws ABoxManagementException{
		if(ontologyIRI == null){
			throw new ABoxManagementException("The ABox ontology IRI is empty.");
		}
	}
	
	/**
	 * Checks if the current ABox ontology IRI for saving the ontology file</br> 
	 * is empty. If it is empty, the tool throws an exception. If it is not</br> 
	 * empty, the tool continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateOntologyFileSaveIRI() throws ABoxManagementException{
		if(ontologyIRIFileSave == null){
			throw new ABoxManagementException("The ABox ontology IRI that is"
					+ "used for saving the ontology file is empty.");
		}
	}
	
	/**
	 * Validates if all relevant base paths have non empty values.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateBasePath() throws ABoxManagementException{
		validateBasePathTBox();
		validateBasePathABox();
	}
	
	/**
	 * Checks if the base path of the TBox ontology that is used to create</br> 
	 * the current ABox ontology is empty. If it is empty, the tool throws</br>
	 * an exception. If it is not empty, the tool continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateBasePathTBox() throws ABoxManagementException{
		if(basePathTBox == null){
			throw new ABoxManagementException("The base path of the current "
					+ "TBox ontology is empty.");
		}
	}
	
	/**
	 * Checks if the base path of the current ABox ontology is empty. If it</br> 
	 * is empty, the tool throws an exception. If it is not empty, the tool</br>
	 * continues its operation.
	 * 
	 * @throws ABoxManagementException
	 */
	private void validateBasePathABox() throws ABoxManagementException{
		if(basePathTBox == null){
			throw new ABoxManagementException("The base path of the current "
					+ "ABox ontology is empty.");
		}
	}
	
	/**
	 * Creates an OWL class using OWLDataFactory.
	 * </br>
	 * To enable the creation of the class, its name and URL forming
	 * path should be provided.
	 * 
	 * @param ontoFactory an instance of OWLDataFactory.
	 * @param owlFilePath the path for forming the URL. 
	 * @param className the name of the class.
	 * @return an OWL class.
	 * @see OWLDataFactory
	 */
	private OWLClass getOWLClass(OWLDataFactory ontoFactory, String owlFilePath, String className){
		return ontoFactory.getOWLClass(owlFilePath.concat("#").concat(className));
	}
	
	private OWLIndividual createOWLIndividual(OWLDataFactory ontoFactory, String owlFilePath, String individualName){
		return ontoFactory.getOWLNamedIndividual(owlFilePath.concat(BACKSLASH).concat(individualName));
	}
	
	private OWLDataProperty createOWLDataProperty(OWLDataFactory dataFactory, String iri, String propertyName, String separator){
		return dataFactory.getOWLDataProperty(iri.concat(separator).concat(propertyName));
	}
	
	/**
	 * Creates an OWL literal according to the data type.
	 * 
	 * @param ontoFactory
	 * @param literal
	 * @param propertyType
	 * @return
	 * @throws ABoxManagementException
	 */
	private OWLLiteral createOWLLiteral(OWLDataFactory ontoFactory, String literal, String propertyType) throws ABoxManagementException{
		if(propertyType.equalsIgnoreCase("string")){
			return ontoFactory.getOWLLiteral(literal);
		} else if(propertyType.equalsIgnoreCase("integer")){
			try{
				return ontoFactory.getOWLLiteral(Integer.parseInt(literal));
			}catch(NumberFormatException e){
				throw new ABoxManagementException("The following value is not an integer:"+literal);
			}
		} else if(propertyType.equalsIgnoreCase("float")){
			try{
				return ontoFactory.getOWLLiteral(Float.parseFloat(literal));
			}catch(NumberFormatException e){
				throw new ABoxManagementException("The following value is not a float:"+literal);
			}
		}
		return ontoFactory.getOWLLiteral(literal);
	}
	

	/**
	 * Retrieves the name of a mechanism using its OWL instance id.
	 * 
	 * @param instanceId
	 * @return
	 * @throws ABoxManagementException
	 */
	public String queryLabel(String instanceId) throws ABoxManagementException{
		String label = readLabel(instanceId);
		if(label==null || label.trim().isEmpty()){
			logger.error("The OWL instance does not have a label (name).");
			throw new ABoxManagementException("The OWL instance does not have a label (name).");
		}
		return label;
	}
	
	/**
	 * Loads an experiment OWL ontology into memory.
	 * 
	 * @param ontologyIRI
	 * @return
	 * @throws ABoxManagementException
	 * @throws OWLOntologyCreationException
	 */
	public OWLOntology loadOntology(IRI ontologyIRI) throws ABoxManagementException, OWLOntologyCreationException{
		// Loads the ontology located at a IRI
		ontology = manager.loadOntology(ontologyIRI);
		if (ontology == null) {
			logger.error("The requested ontology could not be loaded into memory.");
			throw new ABoxManagementException("The requested ontology could not be loaded into memory.");
		}
		return ontology;
	}
	
	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @return the result of the SPARQL query
	 */
	public String performQuery(String q, int type){
		String filteredResult = "";
		// Create a query object from it's string representation
		try {
		Query query = Query.create(q);
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
			if(result.isEmpty()){
				return null;				
			}
			// Calls filterResult method to filter out the unwanted
			// part of the result
			filteredResult = filterResult(result.toString(), type);
		} catch (QueryParserException e) {
			// TODO Auto-generated catch block
			return null;
		} catch (QueryEngineException e) {
			// TODO Auto-generated catch block
			return null;
		}
		return filteredResult;
	}

	/**
	 * Performs a SPARQL query
	 * 
	 * @param q a SPARQL query
	 * @param type the type of value expected to return.
	 * It can be a class or property value
	 * @return the result of the SPARQL query
	 */
	public void performMultilineAnswerQuery(String q, int type){
		// Create a query object from it's string representation
		try {
			Query query = Query.create(q);
			// Execute the query and generate the result set
			QueryResult result = engine.execute(query);
			// Calls filterResult method to filter out the unwanted
			// part of the result
			filterMultilineResult(result.toString(), type);
		} catch (QueryParserException e) {
			return;
		} catch (QueryEngineException e) {
			return;
		} catch(Exception e){
			return;
		}
	}

	/**
	 * Separates the actual result of a query from the 
	 * SPARQLDLAPI part (e.g. ?de.derivo.sparqldlapi.Var@76 = ?),
	 * which appears at the beginning of the result and the
	 * datatype, which appears at the end.
	 * 
	 * @param result contains SPARQLDLAPI part + result + datatype
	 * (e.g. ^^xsd:string)
	 * @return only the result of a query
	 */
	private String filterResult(String result, int type){
		String onlyResult = "";
		if (type == 1) {
			return filterPropertyValue(result, result.length());
		} else if (type == 2) {
			return filterClassType(result, result.length());
		}
		return onlyResult;
	}
	
	/**
	 * Removes unwanted texts from a multi-line result.
	 * 
	 * @param result
	 * @param type
	 */
	private void filterMultilineResult(String result, int type){
		try{
		if(result.contains("\n")){
			for(String res:result.split("\n")){
				// To reduce the amount of message passing all the multiline
				// results query use queryResult global vairable. It helps
				// reduce the amount of message passing.
				String str = filterResult(res, type);
				if(str!=null && !str.isEmpty()){
					queryResult.add(str);
				}
				//queryResult.add(filterResult(res, type));
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Produces the expected result by filtering out the unwanted parts from
	 * the value of a property.
	 * 
	 * @param result
	 * @param resultLength
	 * @return
	 */
	private String filterPropertyValue(String result, int resultLength){
		try{
		if (resultLength > 48) {
			if(result.contains("^^xsd:string")){
				return result.toString().substring(34, resultLength - 14);
			} else if(result.contains("#")){
				String[] tokens = result.split("#");
				if(tokens.length>1){
					return tokens[1];
				}
			} else if(result.contains(BACKSLASH)){
				String[] tokens = result.split(BACKSLASH);
				if(tokens.length>1){
					return tokens[tokens.length-1];
				}
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
		return EMPTY;
	}
	
	/**
	 * Produces the expected result by filtering out the unwanted parts from
	 * the class of an instance.
	 *  
	 * @param result
	 * @param resultLength
	 * @return
	 */
	private String filterClassType(String result, int resultLength){
		try{
		if (resultLength > 32) {
			if(result.substring(32)!=null){
				if(result.substring(32).contains(BACKSLASH)){
					String[] resultParts = result.substring(32).split(BACKSLASH);
					if(resultParts.length>1){
						return resultParts[resultParts.length-1];
					}
				}
			}
		}
		}catch(Exception e){
			e.printStackTrace();
		}
		return EMPTY;
	}
	
	/**
	 * Queries the instances of a class.
	 * 
	 * @param tBoxPrefix
	 * @param tBoxIri
	 * @param claz
	 */
	public void queryInstance(String tBoxPrefix, String tBoxIri, String claz){
		String q = formQueryWithAType(tBoxPrefix, tBoxIri, claz);
		// This method call will store result in a global variable called
		// queryResult, therefore, the line following the call will extract
		// results from it. 
		performMultilineAnswerQuery(q, 2);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the rdfs:label value of 
	 * any object. 
	 * 
	 * @param instanceOwlId the instance id in the OWL representation of 
	 * a mechanism
	 * @return
	 */
	public String readLabel(String instanceOwlId) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instanceOwlId, RDFS_LABEL);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formQueryWithAStandardVocabulary(String vocabulary, String vocabURL, String object, String property) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat(BACKSLASH+">\n")
				.concat("PREFIX ").concat(vocabulary).concat(": <").concat(vocabURL).concat(">\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(base:").concat(object)
				.concat(", ").concat(vocabulary).concat(":").concat(property).concat(", ?v)\n")
				.concat("}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the comment about a mechanism.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formSubjectRetrievalQuery(String object, String property) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat(BACKSLASH+">\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(?v, base:").concat(property).concat(", base:")
				.concat(object).concat(")\n")
				.concat("}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the requirement of reaction 
	 * data validation.
	 * 
	 * @param tBoxPrefix the prefix of the ontology which contains the property being queried
	 * @param tBoxIri the IRI of the ontology which contains the property being queried
	 * @param object the instance whose property is being searched
	 * @param queryItem the property being searched
	 * 
	 * @return the value of the property being searched
	 */
	public String formQueryWithBaseURL(String tBoxPrefix, String tBoxIri, String object, String queryItem){
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat(BACKSLASH+">\n")
				.concat("PREFIX ".concat(tBoxPrefix).concat(" <").concat(tBoxIri).concat("#>\n"))
				.concat("SELECT ?v WHERE {\n")
				.concat("PropertyValue(base:").concat(object)
				.concat(", ").concat(tBoxPrefix).concat(queryItem).concat(", ?v)\n").concat("}");
		return q;
	}

	/**
	 * Forms the SPARQL query to retrieve the instance belonging to a type 
	 * when the type is given.
	 * 
	 * @param tBoxPrefix
	 * @param tBoxIri
	 * @param type
	 * @return
	 */
	public String formQueryWithAType(String tBoxPrefix, String tBoxIri, String type) {
		String q = "PREFIX ".concat(tBoxPrefix).concat(" <").concat(tBoxIri).concat("#>\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("Type(?v, ")
				.concat(tBoxPrefix).concat(type)
				.concat(")\n}");
		return q;
	}
	
	/**
	 * Forms the SPARQL query to retrieve the types of an instance.
	 * 
	 * @param ontologyIRI ontologyIRI the IRI of the ontology is being queried
	 * @param mechanismName the name of a mechanism
	 * @return
	 */
	public String formTypeQueryWithAnInstance(String instance) {
		String q = "PREFIX base: <".concat(ontologyIRI.toString()).concat(BACKSLASH+">\n")
				.concat("SELECT ?v WHERE {\n")
				.concat("Type(base:").concat(instance).concat(", ")
				.concat("?v")
				.concat(")\n}");
		return q;
	}
	
	
	/**
	 * Forms and then performs a SPARQL query to read any comment that is
	 * codified using rdfs:comment vocabulary term. 
	 * 
	 * @return the comment about a species
	 */
	public String readComment(String instance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, instance, RDFS_COMMENT);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Creates an instance of query engine and returns it.
	 * 
	 * @return
	 * @throws ABoxManagementException
	 * @throws OWLOntologyCreationException
	 */
	public QueryEngine createQueryEngine() throws ABoxManagementException, OWLOntologyCreationException{
		// Creates a reasoner
        OWLReasoner reasoner = reasonerFactory.createReasoner(ontology);
		// Creates a query engine
		return QueryEngine.create(manager, reasoner, true);
	}


}
