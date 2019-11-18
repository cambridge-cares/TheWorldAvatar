package org.cam.ceb.como.nist.converter;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.UUID;

import org.cam.ceb.como.nist.model.InitNISTConverter;
import org.cam.ceb.como.nist.model.NISTConverterState;
import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.cam.ceb.como.nist.model.utils.NISTConverterUtils;
import org.cam.ceb.como.nist.webbook.info.NISTSpeciesInfo;
import org.cam.ceb.como.nist.webbook.parser.NISTWebBookParser;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.slf4j.Logger;

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
		// Each iteration converts a NIST species into an OWL file.
		for (String key:data.keySet()) {
			NISTSpeciesInfo speciesInfo = data.get(key);
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
		addWeblinkToSpecies(speciesInfo);
	}
	
	/**
	 * Creates the corresponding OWL instance of the given NIST species.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void createSpeciesInstance() throws OntoSpeciesException{
		individual = iNistOWLWriter.createInstance(ontoKinTBoxIRI, uniqueSpeciesId, "Species");
	}
	
	/**
	 * Adds the weblink to the page where the current species is described.
	 * 
	 * @throws OntoSpeciesException
	 */
	private void addWeblinkToSpecies(NISTSpeciesInfo speciesInfo) throws OntoSpeciesException{
//		if(speciesInfo.getPermanentLink()!=null && !speciesInfo.getPermanentLink().trim().isEmpty())
//		{
//			OWLIndividual weblink = nistOWLWriter.createInstance(ontoKinTBoxIRI, uniqueSpeciesId, "Weblink");
//			nistOWLWriter.addDataPropertyToIndividual(individual, "", propertyPathSeparator, dataPropertyValue);InstanceName(appConfigOntokin
//					.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)), mechanismName);
//		}
	}
	
	/**
	 * Saves the ontology created for codifying a single species chemical mechanism.
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
