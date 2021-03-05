package org.cam.ceb.como.nist.converter;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.time.LocalDate;
import java.util.Map;
import java.util.UUID;

import org.cam.ceb.como.nist.model.InitNISTConverter;
import org.cam.ceb.como.nist.model.NISTConverterState;
import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.cam.ceb.como.nist.model.utils.NISTConverterUtils;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;
import org.cam.ceb.como.nist.webbook.parser.NISTWebBookParser;
import org.cam.ceb.como.nist.webbook.thermochem.NISTEnthalpy;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.slf4j.Logger;

import gigadot.chom.model.cookie.EnthalpyOfFormation;

/**
 * Parses HTML, SDF and MOL files containing NIST species and convert them into 
 * ontologies represented using OWL.
 * 
 * @author msff2
 *
 */
public class NISTConverter extends NISTConverterState implements INISTConverter{
	// An instance of the Logger class, which implemented SLF4J, created
	// to log messages relevant for the NISTConverter class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(NISTConverter.class);

	public static final String NIST_WEB_LINK = "https://webbook.nist.gov";  
	
	public static final String CLS_SPECIES = "Species";
	public static final String CLS_WEBLINK = "Weblink";
	public static final String CLS_ENTHALPY = "StandardEnthalpyOfFormation";
	public static final String CLS_TEMPERATURE = "Temperature";
	
	public static final String PROP_OBJ_WEBLINK = "hasWeblink";
	public static final String PROP_DAT_VALUE = "value";
	public static final String PROP_DAT_DATE = "dateOfAccess";
	public static final String PROP_OBJ_ENTHALPY = "hasStandardEnthalpyOfFormation";
	public static final String PROP_DAT_UNITS = "units";
	public static final String PROP_DAT_CAS_REG_ID = "casRegistryID";
	public static final String PROP_DAT_GEOMETRY = "hasGeometry";
	public static final String PROP_DAT_ATOMIC_BONDS = "hasAtomicBond";
	public static final String PROP_OBJ_TEMPERATURE = "hasReferenceTemperature";
	public static final String VALUE_REF_TEMPERATURE = "298.15";
	public static final String VALUE_REF_TEMP_UNITS = "K";
	public static final String PROP_DAT_ALT_LABEL = "altLabel";
	public static final String PROP_DAT_ID = "identifier";	
	
	/**
	 * Takes the paths to the input HTML and structure (SDF and MOL) files</br>
	 * representing NIST species and the output OWL files. It then</br>
	 * converts input files into OWL. See below the list of mandatory</br>
	 * arguments and their sequence.</br>
	 * </br>
	 * - first argument: absolute path to the HTML files (e.g. C:/html/)
	 * - second argument: absolute path to the SDF and MOL files (e.g. C:/structure/) 
	 * - third argument: absolute path to the OWL files, which will be</br>
	 * generated (e.g. C:/owl/)
	 * 
	 * @param args
	 * @throws IOException
	 * @throws OntoSpeciesException
	 * @throws OWLOntologyCreationException
	 */
	public static void main(String[] args) {
		try {
			if (args.length >= 3) {
				if (new File(args[0]).exists() && new File(args[1]).exists() && new File(args[2]).exists()) {
					NISTConverter nistConverter = new NISTConverter();
					nistConverter.convert(args[0], args[1], args[2]);
				} else {
					logger.info("The paths provided in the argument do not exist.");
				}
			} else {
				logger.info("Three arguments have not been provided.");
			}
		} catch (OntoSpeciesException e) {
			e.printStackTrace();
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * It takes a set of NIST species as inputs and converts each species</br>
	 * into an OWL file.
	 * 
	 * @param sourceHTMLFilePath
	 * @param sourceStructureFilePath
	 * @param owlFilesPath
	 * @throws OntoSpeciesException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(String sourceHTMLFilePath, String sourceStructureFilePath, String owlFilesPath)
			throws OntoSpeciesException, OWLOntologyCreationException {
		// Checks if the path to the input HTML file(s) exists
		if (sourceHTMLFilePath == null || !new File(sourceHTMLFilePath).exists()) {
			logger.error("The HTML file path is null.");
			throw new OntoSpeciesException("The HTML file path is null.");
		}
		// Checks if the path to the input SDF and MOL file(s) exists
		if (sourceStructureFilePath == null || !new File(sourceStructureFilePath).exists()) {
			logger.error("The SDF and MOL files path is null.");
			throw new OntoSpeciesException("The SDF and MOL file path is null.");
		}
		// Checks if the path to the output OWL file(s) exists.
		if (owlFilesPath == null || !new File(owlFilesPath).exists()) {
			logger.error("The ontology file path is null.");
			throw new OntoSpeciesException("The ontology file path is null.");
		}
		logger.info("Started producing OntoSpecies from NIST data...");
		int iteration = 0;
		NISTWebBookParser nistWebBookParser = new NISTWebBookParser();
		try{
		Map<String, NISTSpeciesInfo> data = nistWebBookParser.parseNISTData(sourceHTMLFilePath, sourceStructureFilePath);
		int count = 0;
		// Each iteration converts a NIST species into an OWL file.
		for (String key:data.keySet()) {
			NISTSpeciesInfo speciesInfo = data.get(key);
			if(speciesInfo.getEnthalpyOfFormationInGas() == null || speciesInfo.getEnthalpyOfFormationInGas().size()<1){
				continue;
			}
			System.out.println(speciesInfo.getEnthalpyOfFormationInGas().size());
			if(speciesInfo.getSpeciesGeometry()==null || speciesInfo.getSpeciesGeometry().getString()==null || speciesInfo.getSpeciesGeometry().getString().trim().equals("")){
				continue;
			}
			count++;
			logger.info("Number of species found with all relevant info:"+count);
			System.out.println("Number of species found with all relevant info:"+count);
			// Also initialises the instances of the classes that read
			// configuration parameters using Spring framework annotations.
			initNISTConverter = new InitNISTConverter();
			initNISTConverter.init();
			// Populates the data type hash map if not done yet
			if(!dataTypePopulated){
				populateDataType(ontoSCSVFileName);
			}
			speciesId++;
			byte[] bytes = (""+speciesId).getBytes("UTF-8");
			UUID uuid = UUID.nameUUIDFromBytes(bytes);
			uniqueSpeciesId = uuid.toString();
			basePathABox = ontoSKBIRI.concat(uniqueSpeciesId).concat(".owl");			
			basePathTBox = ontoSKBTBoxIRI;
			// Creates an IRI for the OWL ontology that is
			// being created to codify a species
			ontologyIRI = IRI.create(basePathABox);
			if (ontologyIRI == null) {
				logger.error("An IRI for an ontology could not be created.");
				throw new OntoSpeciesException("An IRI for an ontology could not be created.");
			}
			kbIRI = IRI.create(basePathABox);
			// Creates an IRI to save the OWL ontology on the file system
			ontologyIRIFileSave = IRI.create("file:/".concat(owlFilesPath).concat(uniqueSpeciesId).concat(".owl"));
			ontology = manager.createOntology(IRI.create(basePathABox));
			if (ontology == null) {
				logger.error("The requested ontology could not be created.");
				throw new OntoSpeciesException("Ontology could not be created.");
			}
			convertNISTSpecies(speciesInfo);
			try{
				saveOntology();
			}catch(Exception e){
				e.printStackTrace();
			}
			iteration++;
			logger.info("["+iteration+"] Finished the conversion of Species:"+iteration);
		}
		}catch(IOException e){
			logger.error("Input file(s) (HTML, SDF and/or MOL) is/are not found. Detailed error message is here:" + e.getMessage()); 
			e.printStackTrace();
		}
		logger.info("Finished producing OntoSpecies from NIST.");
	}

	/**
	 * Populates a hash map with data properties and related data types.</br>  
	 * The map is used to create each OWL literal with the correct data type. 
	 * 
	 * @param fileName
	 */
	private void populateDataType(String fileName) {
		try {
			BufferedReader br = NISTConverterUtils.openResourceFile(fileName);
			String line;
			br.readLine();
			while ((line = br.readLine()) != null) {
				String[] tokens = line.split(COMMA);
				if ((tokens.length - 1) >= ontoSCSVPValueColumnNo) {
					if (!tokens[ontoSCSVTermTypeColumnNo].isEmpty()
							&& !tokens[ontoSCSVTermTypeColumnNo]
									.equalsIgnoreCase(ontoSCSVDataPropertyName)) {
						continue;
					}
					if (!tokens[ontoSCSVPKeyColumnNo].isEmpty()
							&& !tokens[ontoSCSVPValueColumnNo].isEmpty()) {
						dataPropertyNameVsTypeMap.put(tokens[ontoSCSVPKeyColumnNo].toLowerCase().replace(SPACE, EMPTY),
								tokens[ontoSCSVPValueColumnNo].toLowerCase());
					}
				}
			}
			dataTypePopulated = true;
		} catch (IOException e) {
			logger.error("Could not read the following file:" + fileName);
			e.printStackTrace();
		}
	}
	
	/**
	 * Represents NIST species using classes and properties of OntoSpecies</br>
	 * and other onotlogies such as OntoKin.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void convertNISTSpecies(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		createSpeciesInstance();
		addIdentifier();
		addName(speciesInfo);
		addWeblinkToSpecies(speciesInfo);
		addEnthalpyOfFormation(speciesInfo);
		addCASRegistryNumber(speciesInfo);
		addAlternativeNames(speciesInfo);
		addGeometryString(speciesInfo);
		addAtomicBondsString(speciesInfo);
	}
	
	/**
	 * Creates the corresponding OWL instance of the given NIST species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void createSpeciesInstance() throws OntoSpeciesException{
		individual = iNistOWLWriter.createInstance(uniqueSpeciesId, CLS_SPECIES);
	}
	
	/**
	 * Adds the identifier to the current species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addIdentifier() throws OntoSpeciesException{
		iNistOWLWriter.addDataPropertyToIndividual(individual, DUBLIN_CORE_URL, PROP_DAT_ID, BACKSLASH, uniqueSpeciesId);
	}
	
	/**
	 * Adds the weblink to the page where the current species is described.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addWeblinkToSpecies(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getPermanentLink()!=null && !speciesInfo.getPermanentLink().trim().isEmpty())
		{
			weblinkId++;
			OWLIndividual weblinkIndividual = iNistOWLWriter.createInstance(CLS_WEBLINK.concat(UNDERSCORE)+weblinkId, CLS_WEBLINK);
			iNISTOwlWriter.linkInstance(PROP_OBJ_WEBLINK, uniqueSpeciesId, CLS_WEBLINK.concat(UNDERSCORE)+weblinkId);
			iNistOWLWriter.addDataPropertyToIndividual(weblinkIndividual, PROP_DAT_VALUE, HASH, NIST_WEB_LINK.concat(speciesInfo.getPermanentLink()));
			LocalDate localDate = LocalDate.now(); 
			iNistOWLWriter.addDataPropertyToIndividual(weblinkIndividual, PROP_DAT_DATE, HASH, localDate.toString());
		}
	}
	
	/**
	 * Adds a set of enthalpies of formation to the current species.
	 * 
	 * @param speciesInfo
	 * @throws OntoSpeciesException
	 */
	private void addEnthalpyOfFormation(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getEnthalpyOfFormationInGas()!=null){
				for(NISTEnthalpy nistEnthalpy:speciesInfo.getEnthalpyOfFormationInGas()){
					addEnthalpyOfFormation(nistEnthalpy);
				break;
				}
				}
		}
	
	
	
	/**
	 * Adds a single enthalpy of formation to the current species.
	 * 
	 * @param nistEnthalpy
	 * @throws OntoSpeciesException
	 */
	private void addEnthalpyOfFormation(NISTEnthalpy nistEnthalpy)  throws OntoSpeciesException{
		enthalpyId++;
		OWLIndividual enthalpyIndividual = iNistOWLWriter.createInstance(CLS_ENTHALPY.concat(UNDERSCORE)+enthalpyId, CLS_ENTHALPY);
		iNISTOwlWriter.linkInstance(PROP_OBJ_ENTHALPY, uniqueSpeciesId, CLS_ENTHALPY.concat(UNDERSCORE)+enthalpyId);
		if(nistEnthalpy.getNegTolerance()!=0 && nistEnthalpy.getPosTolerance()!=0){
			iNistOWLWriter.addDataPropertyToIndividual(enthalpyIndividual, PROP_DAT_VALUE, HASH, nistEnthalpy.getValue()+"±"+nistEnthalpy.getPosTolerance());
		} else{
			iNistOWLWriter.addDataPropertyToIndividual(enthalpyIndividual, PROP_DAT_VALUE, HASH, EMPTY+nistEnthalpy.getValue());
		}
		if(nistEnthalpy.getUnits()!=null && !nistEnthalpy.getUnits().isEmpty()){
			iNistOWLWriter.addDataPropertyToIndividual(enthalpyIndividual, PROP_DAT_UNITS, HASH, nistEnthalpy.getUnits());
		}
		addReferenceTemperature(enthalpyId);
	}
	
	private void addReferenceTemperature(long enthalpyId)  throws OntoSpeciesException{
		OWLIndividual temperatureIndividual = iNistOWLWriter.createInstance(CLS_TEMPERATURE.concat(UNDERSCORE)+temperatureId, CLS_TEMPERATURE);
		iNISTOwlWriter.linkInstance(PROP_OBJ_TEMPERATURE, CLS_ENTHALPY.concat(UNDERSCORE)+enthalpyId, CLS_TEMPERATURE.concat(UNDERSCORE)+temperatureId);
		iNistOWLWriter.addDataPropertyToIndividual(temperatureIndividual, PROP_DAT_VALUE, HASH, VALUE_REF_TEMPERATURE);
		iNistOWLWriter.addDataPropertyToIndividual(temperatureIndividual, PROP_DAT_UNITS, HASH, VALUE_REF_TEMP_UNITS);
	}
	
	/**
	 * Adds the CAS Registry Number to the current species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addCASRegistryNumber(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getCASRegNr()!=null && !speciesInfo.getCASRegNr().trim().isEmpty())
		{
			iNistOWLWriter.addDataPropertyToIndividual(individual, PROP_DAT_CAS_REG_ID, HASH, speciesInfo.getCASRegNr());
		}
	}
	
	/**
	 * Adds the name to the current species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addName(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getName()!=null && !speciesInfo.getName().isEmpty())
		{
			dataPropertyNameVsTypeMap.put(RDFS_URL+RDFS_LABEL, "string");
			iNistOWLWriter.addDataPropertyToIndividual(individual, RDFS_URL, RDFS_LABEL, EMPTY, speciesInfo.getName());
		}
	}
	
	/**
	 * Adds one or more alternative names to the current species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addAlternativeNames(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getOtherNames()!=null)
		{
			for(String altName: speciesInfo.getOtherNames()){
				iNistOWLWriter.addDataPropertyToIndividual(individual, SKOS_URL, PROP_DAT_ALT_LABEL, HASH, altName);
			}
		}
	}

	/**
	 * Adds the geometry to the current species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addGeometryString(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getSpeciesGeometry()!=null && speciesInfo.getSpeciesGeometry().getString()!=null)
		{
			iNistOWLWriter.addDataPropertyToIndividual(individual, PROP_DAT_GEOMETRY, HASH, speciesInfo.getSpeciesGeometry().getString());
		}
	}
	
	/**
	 * Adds all atomic bonds as a string to the current species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addAtomicBondsString(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
		if(speciesInfo.getSpeciesGeometry()!=null && speciesInfo.getSpeciesGeometry().getAtomicBondsString()!=null)
		{
			iNistOWLWriter.addDataPropertyToIndividual(individual, PROP_DAT_ATOMIC_BONDS, HASH, speciesInfo.getSpeciesGeometry().getAtomicBondsString());
		}
	}
	
	/**	 * Saves the ontology created for codifying a single species chemical mechanism.
	 */
	public void saveOntology() throws OWLOntologyStorageException {
		try {
			// Adds the import clause to the OntoChem TBox
			OWLImportsDeclaration importDeclaration = OWLManager.getOWLDataFactory()
					.getOWLImportsDeclaration(IRI.create(ontoSKBTBoxIRI));
			manager.applyChange(new AddImport(ontology, importDeclaration));
			// Saves the ontology in the file system
			manager.saveOntology(ontology, ontologyIRIFileSave);
			logger.info("Started removing TBox from the mechanism OWL file.");
			iNISTOwlWriter.removeTBox(ontologyIRIFileSave);
			logger.info("Finished removing TBox from the mechanism OWL file.");
		} catch (OWLOntologyStorageException e1) {
			logger.error("The ontology could not be saved.");
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		}
	}

}
