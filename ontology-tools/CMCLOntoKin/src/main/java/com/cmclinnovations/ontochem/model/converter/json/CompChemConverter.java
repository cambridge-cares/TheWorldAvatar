package com.cmclinnovations.ontochem.model.converter.json;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonValue;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AddOntologyAnnotation;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.vocab.DublinCoreVocabulary;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.CtmlConverterState;
import com.cmclinnovations.ontochem.model.InitCtmlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.utils.CtmlConverterUtils;

/**
 * Parses JSON files containing CompChem data and convert them into 
 * OWL ontologies. 
 * 
 * @author msff2
 *
 */
public class CompChemConverter extends CtmlConverterState implements ICompChemConverter{
	// An instance of the Logger class (which implemented SLF4J) created
	// to log messages relevant for the CompChemConverter class.
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(CompChemConverter.class);

	String nasaFile;

	/**
	 * Takes a CompChem file in the json format as the input and produces</br> 
	 * the corresponding OntoKin file in the OWL format.
	 * 
	 * @param args
	 * @throws IOException
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	public static void main(String[] args) throws IOException, OntoException, OWLOntologyCreationException {
		ArrayList<String> compChemFiles = new ArrayList<>();
		if (args.length >= 1) {
			if (new File(args[0]).exists()) {
				if (args[0].endsWith(".json")) {
					if(!(new File(args[0]).isAbsolute())){
						args[0] = new File(args[0]).getAbsolutePath();
					}
					String splitter = null;
					if (args[0].contains("/")) {
						splitter = "/";
					} else if (args[0].contains("\\")) {
						splitter = "\\\\";
					}
					if (splitter != null) {
						String[] tokens = args[0].split(splitter);
						String owlFilePath = args[0].replace(tokens[tokens.length - 1], "");
						owlFilePath = owlFilePath.substring(0, owlFilePath.length() - 1);
						compChemFiles.add(args[0]);
						CompChemConverter compChemConverter = new CompChemConverter();
						compChemConverter.convert(compChemFiles, owlFilePath);
					}
				}else{
					logger.info("The file does not have the json extension.");
				}
			} else{
				logger.info("The file provided in the argument does not exist.");
			}
		}else{
			logger.info("No argument has been provided.");
			logger.info("An input json file must be provided as the first argument.");
		}
	}
	
	public static void main() {
		CompChemConverter ccc = new CompChemConverter();
		ArrayList<String> inputFiles = new ArrayList<>();
		inputFiles.add("C:/Users/msff2/Documents/c4e-jps-hydrogen/Data/compchem/input-json-h2-species/0a0df9ef-03a0-325a-b531-d9c57a1f84f8/0a0df9ef-03a0-325a-b531-d9c57a1f84f8_nasa.json");
		String owlFileDir = "C:/Users/msff2/Documents/c4e-jps-hydrogen/Codes/mechanism-viewer/CMCLOntoKin/kb";
		try{
			ccc.convert(inputFiles, owlFileDir);
		}catch(OWLOntologyCreationException e){
			e.printStackTrace();
		}catch(OntoException e){
			e.printStackTrace();
		}
	}
	
	/**
	 * Converts a CompChem file into an OWL file.</br>
	 * It also supports the conversion of multiple files.</br>
	 * The conversion process takes a CompChem file, uses the standard JSON
	 * parser to parse the CompChem data and metadata</br>.
	 * 
	 * @param compchemFiles
	 * @param owlFilesPath
	 * @param quantumCalcIRIs
	 * @param uniqueSpeciesIRIs
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> compchemFiles, String owlFilesPath)
			throws OntoException, OWLOntologyCreationException {
		// Checks if the user shown path to save the OWL file is valid
		if (owlFilesPath == null) {
			logger.error("The ontology file path is null.");
			throw new OntoException("The ontology file path is null.");
		}
		// Checks if the list that contains the path to the input CompChem files
		// exists
		if (compchemFiles == null) {
			logger.error("CompChem source files are empty.");
			throw new OntoException("CompChem source files are empty.");
		}
		logger.info("Started producing OntoKin from CompChem...");
		int iteration = 0;
		// Each iteration parses a CompChem file to extract the values
		// of CompChem properties
		for (String compchemFile : compchemFiles) {
			nasaFile = compchemFile;
			// Initialise the instances of the classes that hold CompChem metadata
			// and data as well as parsing status information thereof.
			// Also initialises the instances of the classes that read
			// configuration parameters using Spring framework annotations.
			initCtmlConverter = new InitCtmlConverter();
			initCtmlConverter.init();
			// Populates the data type hash map if not done yet
			if(!dataTypePopulated){
				populateDataType(opCtrl.gettBoxFileName());
			}
			// Replaces any space in the user shown OWL file path with %20
			// to form a valid URL.
			String owlFilePath = owlFilesPath;
			if (owlFilePath.contains(" ")) {
				owlFilePath = owlFilePath.replaceAll(" ", "%20");
			}
			try {
				// Calls the CompChem parser
				parseCompChemData(compchemFile);
			} catch (IOException e) {
				logger.error("IOException occurred.");
				e.printStackTrace();
			}
			mechanismName = ontoChemKB.getOntoChemKBSingleSpeciesMechPreamble().concat(UNDERSCORE).concat(compChem.getName()).concat(UNDERSCORE).concat(appConfigOntokin.getOntokinMechanism()).concat(UNDERSCORE).concat(Long.toString(++mechanismInstanceId));
			basePath = ontoChemKB.getOntoKinKbURL().concat(mechanismName).concat(opCtrl.getOwlFileExtension());
			basePathABox = basePath;
			
			basePathTBox = ontoChemKB.getOntoKinKbTBoxIri();
			// Creates an IRI for the OWL ontology that is
			// being created to codify a mechanism
			ontologyIRI = IRI.create(basePath);
			if (ontologyIRI == null) {
				logger.error("An IRI for an ontology could not be created.");
				throw new OntoException("An IRI for an ontology could not be created.");
			}
			kbIRI = IRI.create(ontoChemKB.getOntoKinKbURL().concat(ontoChemKB.getOntoChemOntolgyFileName()));
			// Replaces any space in the user shown OWL file path with %20
			// to form a valid URL.
			String owlFilePathForFileSave = formOwlFileSaveUrl(compchemFile, owlFilesPath);
			if (owlFilePathForFileSave.contains(" ")) {
				owlFilePathForFileSave = owlFilePathForFileSave.replaceAll(" ", "%20");
			}
			// Creates an IRI to save the OWL ontology on the file system
			ontologyIRIFileSave = IRI.create(owlFilePathForFileSave);
			ontology = manager.createOntology(IRI.create(basePath));
			if (ontology == null) {
				logger.error("The requested ontology could not be created.");
				throw new OntoException("Ontology could not be created.");
			}
			
			convertCompChem();

			kb = manager.createOntology(kbIRI);
			try{
			saveOntology();
			}catch(Exception e){
				
			}
			iteration++;
			logger.info("["+iteration+"] Finished the conversion of of the following CompChem file:\n\t\t\t\t\t\t"+compchemFile);
		}
		logger.info("Finished producing OntoKin from CompChem.");
	}

	/**
	 * Populates a hash map with data properties and related data types.</br>  
	 * The map is used to create each OWL literal with the correct data type. 
	 * 
	 * @param fileName
	 */
	private void populateDataType(String fileName) {
		try {
			BufferedReader br = CtmlConverterUtils.openResourceFile(fileName);
			String line;
			br.readLine();
			while ((line = br.readLine()) != null) {
				String[] tokens = line.split(COMMA);
				if ((tokens.length - 1) >= opCtrl.getDataTypeValueColumnIndex()) {
					if (!tokens[opCtrl.getSourceTypeColumnIndex()].isEmpty()
							&& !tokens[opCtrl.getSourceTypeColumnIndex()]
									.equalsIgnoreCase(opCtrl.getSourceTypeDataPropertyName())) {
						continue;
					}
					if (!tokens[opCtrl.getDataTypeKeyColumnIndex()].isEmpty()
							&& !tokens[opCtrl.getDataTypeValueColumnIndex()].isEmpty()) {
						dataPropertyNameVsTypeMap.put(tokens[opCtrl.getDataTypeKeyColumnIndex()].toLowerCase().replace(SPACE, EMPTY),
								tokens[opCtrl.getDataTypeValueColumnIndex()].toLowerCase());
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
	 * Parses a JSON file containing CompChem data and convert it
	 * into an OWL ontology.
	 * 
	 * @param compchemFile
	 * @throws IOException
	 * @throws OntoException
	 */
	public void parseCompChemData(String compchemFile) throws IOException, OntoException {
		JsonReader reader = Json.createReader(openSourceFile(compchemFile));
		JsonObject compChemObject = reader.readObject();
		reader.close();
		readMetaData(compChemObject);
		readData(compChemObject);
	}
	
	/**
	 * Reads metadata about the calculated thermo data.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readMetaData(JsonObject compChemObject) throws OntoException{
		readComment(compChemObject);
		readRunDate(compChemObject);
		readProgramName(compChemObject);
		readBasisSet(compChemObject);
		readProgramVersion(compChemObject);
		readPhase(compChemObject);
		readLevelOfTheory(compChemObject);
		CtmlConverterUtils.gitCommitHash();
	}

	/**
	 * Reads thermo data of the species currently being processed.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readData(JsonObject compChemObject) throws OntoException{
		readName(compChemObject);
		readAtomicMasses(compChemObject);
		readAtomicWeightUnits(compChemObject);
		readHighTCoefficient(compChemObject);
		readComposition(compChemObject);
		readLowTCoefficient(compChemObject);
		readTMin(compChemObject);
		readTMid(compChemObject);
		readTMax(compChemObject);
		readUniqueSpeciesIRI(compChemObject);
		readQuantumCalculationIRI(compChemObject);
		readThermoAgentIRI(compChemObject);
	}
	
	/**
	 * Creates and returns an instance of the BufferedReader class. It takes
	 * 
	 * @param filePathPlusName
	 *            the path plus name of the file being read
	 * @return
	 * @throws IOException
	 * @throws OntoException
	 */
	private InputStream openSourceFile(String filePathPlusName) throws IOException, OntoException {
		return new FileInputStream(filePathPlusName);
	}
	
	/**
	 * Reads the comment
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readComment(JsonObject compChemObject) throws OntoException{
		String comment = compChemObject.getString("Comment");
		compChem.setComment(comment);
	}
	
	/**
	 * Reads the date when a program was run to calculate thermo data.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readRunDate(JsonObject compChemObject) throws OntoException{
		String runDate = compChemObject.getString("runDate");
		compChem.setRunDate(runDate);
	}
	
	/**
	 * Reads the name of the program run to compute thermo data. 
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readProgramName(JsonObject compChemObject) throws OntoException{
		String programName = compChemObject.getString("programName");
		compChem.setProgramName(programName);
	}
	
	/**
	 * Reads the name of the basis set used in the calculation.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readBasisSet(JsonObject compChemObject) throws OntoException{
		String basisSetValue = compChemObject.getString("basisSetValue");
		compChem.setBasisSetValue(basisSetValue);
	}
	
	/**
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readProgramVersion(JsonObject compChemObject) throws OntoException{
		String programVersion = compChemObject.getString("programVersion");
		compChem.setProgramVersion(programVersion);
	}
	
	/**
	 * Reads the phase in which the current species belongs to.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readPhase(JsonObject compChemObject) throws OntoException{
		String phase = compChemObject.getString("Phase");
		compChem.setPhase(phase);
	}
	
	/**
	 * Reads the level of theory used to calculate thermo data.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readLevelOfTheory(JsonObject compChemObject) throws OntoException{
		String levelOfTheory = compChemObject.getString("levelOfTheory");
		compChem.setLevelOfTheory(levelOfTheory);
	}
	
	/**
	 * Reads the name of the species currently being parsed.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readName(JsonObject compChemObject) throws OntoException{
		String name = compChemObject.getString("Name");
		compChem.setName(name);
	}
	
	/**
	 * Reads the atomic mass of those atoms that participate in the species
	 * currently being processed. 
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readAtomicMasses(JsonObject compChemObject) throws OntoException{
		JsonArray atomicMassesArray = compChemObject.getJsonArray("atomicMasses");
		int i = 1;
		HashMap<String, String> atomicMasses = new HashMap<>();
		ArrayList<String> atomNames = new ArrayList<>();
		for (JsonValue jsonValue : atomicMassesArray) {
			if(i==1){
				JsonArray atoms = (JsonArray) jsonValue;
				for(JsonValue atom: atoms){
					String atomString = atom.toString();
					atomString = atomString.replace("\"", "");
					atomNames.add(atomString);
				}
			}
			if(i==2){
				int j = 0;
				JsonArray atoms = (JsonArray) jsonValue;
				for(JsonValue atom: atoms){
					double atomicMass = Double.parseDouble(atom.toString());
					if(!atomicMasses.containsKey(atomNames.get(j))){
						atomicMasses.put(atomNames.get(j++), Double.toString(atomicMass));
					}
				}
			}
			i++;
		}
		compChem.setAtomicMasses(atomicMasses);
	}
	
	/**
	 * Reads the name of the species currently being parsed.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readAtomicWeightUnits(JsonObject compChemObject) throws OntoException{
		String massUnit = null;
		JsonArray mUnit = compChemObject.getJsonArray("massUnit");
		if(mUnit==null){
			logger.info("Mass Unit is not provided in the compchem file");
		}
		for (JsonValue jsonValue : mUnit) {
			try {
				massUnit = jsonValue.toString();
			} catch (NullPointerException e) {
				logger.info("Mass Unit is not provided in the compchem file:" + nasaFile);
			} finally {
				if (massUnit == null || massUnit.isEmpty()) {
					logger.error("Mass Unit is not provided in the compchem file: " + nasaFile);
				} else {
					massUnit = massUnit.replace("\"", "");
					compChem.setAtomicWeightUnits(massUnit);
				}
			}
			break;
		}
	}

	/**
	 * Reads the high temperature coefficient of the species currently 
	 * being processed. 
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readHighTCoefficient(JsonObject compChemObject) throws OntoException{
		JsonArray highTCoeff = compChemObject.getJsonArray("highTcoeff");
		int nOfCoeffs = highTCoeff.size();
		compChem.setNumberOfHighTCoeff(nOfCoeffs);
		int countParsedCoeffs = 0;
		compChem.setHighTCoeff("");
		for (JsonValue jsonValue : highTCoeff) {
			double highTCoeffValue = Double.parseDouble(jsonValue.toString());
			if(++countParsedCoeffs<nOfCoeffs){
				if(countParsedCoeffs%4==0){
					compChem.setHighTCoeff(compChem.getHighTCoeff().concat(Double.toString(highTCoeffValue)).concat(COMMA).concat(NEWLINE));
				}else{
					compChem.setHighTCoeff(compChem.getHighTCoeff().concat(Double.toString(highTCoeffValue)).concat(COMMA).concat(SPACE_3));
				}
			} else {
				compChem.setHighTCoeff(compChem.getHighTCoeff().concat(Double.toString(highTCoeffValue)));
			}
		}
	}
	
	/**
	 * Reads the composition of the species currently being processed.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readComposition(JsonObject compChemObject) throws OntoException{
		HashMap<String, Integer> parsedComposition = new HashMap<>(); 
		JsonArray composition = compChemObject.getJsonArray("Composition");
		int i = 0;
		String key = "";
		String value = "";
		for (JsonValue jsonValue : composition) {
				String compositionValue = jsonValue.toString();
				if(++i%2==1){
					key = compositionValue;
					if(key.length()>=3){
						key = key.substring(1, key.length()-1);
					}
				}else{
					value = compositionValue;
					if(value.length()>=3){
						value = value.substring(1, value.length()-1);
					}
					parsedComposition.put(key, Integer.parseInt(value));
				}
		}
		compChem.setComposition(parsedComposition);
	}
	
	/**
	 * Reads the low temperature coefficient of the species currently 
	 * being processed.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readLowTCoefficient(JsonObject compChemObject) throws OntoException{
		JsonArray lowTcoeff = compChemObject.getJsonArray("LowTcoeff");
		int nOfCoeffs = lowTcoeff.size();
		compChem.setNumberOfLowTCoeff(nOfCoeffs);
		int countParsedCoeffs = 0;
		compChem.setLowTCoeff("");
		for (JsonValue jsonValue : lowTcoeff) {
			double lowTcoeffValue = Double.parseDouble(jsonValue.toString());
			if(++countParsedCoeffs<nOfCoeffs){
				if(countParsedCoeffs%4==0){
					compChem.setLowTCoeff(compChem.getLowTCoeff().concat(Double.toString(lowTcoeffValue)).concat(COMMA).concat(NEWLINE));
				} else{
					compChem.setLowTCoeff(compChem.getLowTCoeff().concat(Double.toString(lowTcoeffValue)).concat(COMMA).concat(SPACE_3));
				}
			} else {
				compChem.setLowTCoeff(compChem.getLowTCoeff().concat(Double.toString(lowTcoeffValue)));
			}
		}
	}
	
	/**
	 * Reads the temperature below which calculated thermo data is not valid.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readTMin(JsonObject compChemObject) throws OntoException{
		double tMin = compChemObject.getJsonNumber("Tmin").doubleValue();
		compChem.settMin(Double.toString(tMin));
	}
	
	/**
	 * Reads the medium temperature value for computing thermo data.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readTMid(JsonObject compChemObject) throws OntoException{
		int tMid = compChemObject.getJsonNumber("Tmid").intValue();
		compChem.settMid(Double.toString(tMid));
	}
	
	/**
	 * Reads the temperature above which calculated thermo data is not valid.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readTMax(JsonObject compChemObject) throws OntoException{
		int tMax = compChemObject.getJsonNumber("Tmax").intValue();
		compChem.settMax(Double.toString(tMax));
	}
	
	/**
	 * Reads the unique Species KB IRI of the current species.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readUniqueSpeciesIRI(JsonObject compChemObject) throws OntoException {
		String uniqueSpeciesIRI = null;
		JsonArray speciesIRI = compChemObject.getJsonArray("speciesIRI");
		if(speciesIRI==null){
			logger.info("Unique Species IRI is not provided in the compchem file");
		}
		for (JsonValue jsonValue : speciesIRI) {
			try {
				uniqueSpeciesIRI = jsonValue.toString();
			} catch (NullPointerException e) {
				logger.info("Unique Species IRI is not provided in the compchem file:" + nasaFile);
			} finally {
				if (uniqueSpeciesIRI == null) {
					logger.error("UniqueSpeciesIRI is not provided in the compchem file: " + nasaFile);
				} else {
					uniqueSpeciesIRI = uniqueSpeciesIRI.replace("\"", "");
					compChem.setUniqueSpeciesIRI(uniqueSpeciesIRI);
				}
			}
			break;
		}
	}

	/**
	 * Reads the unique IRI of the quantum calculation for the current species.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readQuantumCalculationIRI(JsonObject compChemObject) throws OntoException {
		String quantumCalculationIRI = null;
		JsonArray qCalculationIRIArray = compChemObject.getJsonArray("quantumCalculationIRI");
		if(qCalculationIRIArray==null){
			logger.error("Quantum Calculation IRI is not provided in the compchem file");
			throw new OntoException("Quantum Calculation IRI is not provided in the compchem file");
		}
		for (JsonValue jsonValue : qCalculationIRIArray) {
			try {
				quantumCalculationIRI = jsonValue.toString();
			} catch (NullPointerException e) {
				logger.info("Quantum Calculation IRI is not provided in the compchem file:" + nasaFile);
			} finally {
				if (quantumCalculationIRI == null) {
					logger.error("Quantum Calculation IRI is not provided in the compchem file: " + nasaFile);
				} else {
					quantumCalculationIRI = quantumCalculationIRI.replace("\"", "");
					compChem.setQuantumCalculationIRI(quantumCalculationIRI);
				}
			}
			break;
		}
	}
	
	/**
	 * Reads the IRI of the thermo-agent employed in the calculation of the 
	 * NASA Polynomial coefficients.
	 * 
	 * @param compChemObject
	 * @throws OntoException
	 */
	private void readThermoAgentIRI(JsonObject compChemObject) throws OntoException{
		String thermoAgentIRI = null;
		JsonArray thrmAgentIRI = compChemObject.getJsonArray("ThermoAgentIRI");
		if(thrmAgentIRI==null){
			logger.info("Thermo Agent IRI is not provided in the compchem file");
		}
		for (JsonValue jsonValue : thrmAgentIRI) {
			try {
				thermoAgentIRI = jsonValue.toString();
			} catch (NullPointerException e) {
				logger.info("Thermo Agent IRI is not provided in the compchem file:" + nasaFile);
			} finally {
				if (thermoAgentIRI == null) {
					logger.error("Thermo Agent IRI is not provided in the compchem file: " + nasaFile);
				} else {
					thermoAgentIRI = thermoAgentIRI.replace("\"", "");
					compChem.setThermoAgentIRI(thermoAgentIRI);
				}
			}
			break;
		}
	}
	
	/**
	 * Forms the URL of a mechanism based on where the file will be stored.
	 * 
	 * @param compchemFile The path to the CompChem file being processed.
	 * @param owlFilePath The path to the file being processed.
	 * @return a string representing a URL.
	 * @throws OntoException a specialised exception designed to deal with
	 * compchem to ontology generation related errors.
	 */
	public static String formOwlFileSaveUrl(String compchemFile, String owlFilePath) throws OntoException {
		if (compchemFile == null) {
			logger.error("Provided compchemFile path is null.");
			throw new OntoException("Provided compchemFile path is null.");
		}
		if (owlFilePath == null) {
			logger.error("Provided file path is null.");
			throw new OntoException("Provided file path is null.");
		}
		owlFilePath = owlFilePath.concat("/").concat(ontoChemKB.getOntoKinKbRootDirectory())
				.concat(mechanismName).concat(opCtrl.getOwlFileExtension());
		owlFilePath = CtmlConverterUtils.formatToURLSlash(owlFilePath);
		owlFilePath = CtmlConverterUtils.addFileProtocol(owlFilePath);
		return owlFilePath;
	}
	
	/**
	 * Represents classes and properties of CompChem using OWL.
	 * 
	 * @throws OntoException
	 */
	private void convertCompChem() throws OntoException{
		createMechanismInstance();
		representMechanismData();
		createPhaseInstance();
		representPhaseData();
		createSpeciesInstance();
		linkSpeciesToPhase();
		createSpeciesMetadataInstance();
		representSpeciesMetadata();
		representSpeciesData();
		createThermoModelInstances();
		representUniqueSpeciesIRI();
		representSpeciesElements();
		createReactionMetadataInstance();
		representReactionMetadata();
		representCommitHash();
	}
	
	/**
	 * Creates the corresponding OWL instance of the given single species mechanism.
	 * 
	 * @throws OntoException
	 */
	private void createMechanismInstance() throws OntoException{
		individual = compChemOwlWriter.createInstance(appConfigOntokin
				.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)), appConfigOntokin
				.getOntokinMechanism());
		createHeadComment();
	}
	
	/**
	 * Adds NASA Polynomial Coefficients to the OWL instance of the current thermo model.
	 * 
	 * @throws OntoException
	 */
	private void createHeadComment() throws OntoException{
		compChemOwlWriter.addDataPropertyToIndividual(individual, RDFS_URL, appConfigOntokin.getCtmlComment(), EMPTY, ontoChemKB.getOntoChemKBSingleSpeciesMechHeadComment());
	}
	
	/**
	 * Adds data to the OWL instance of the given single species mechanism.
	 * 
	 * @throws OntoException
	 */
	private void representMechanismData() throws OntoException{
		addMechanismName();
	}
	
	/**
	 * Adds the name of the given single species mechanism to the already 
	 * created OWL instance.
	 * 
	 * @throws OntoException
	 */
	private void addMechanismName() throws OntoException{
		if(compChem.getName()!=null && !compChem.getName().trim().isEmpty())
		{
			compChemOwlWriter.addInstanceName(appConfigOntokin
					.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)), mechanismName);
		}
	}
	
	/**
	 * Creates the corresponding OWL instance of the phase.
	 * 
	 * @throws OntoException
	 */
	private void createPhaseInstance() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().trim().isEmpty()){
			individual = compChemOwlWriter.createInstance(appConfigOntokin
					.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(++phaseInstanceId)), appConfigOntokin
					.getClassGasPhase());
		}
	}

	/**
	 * Adds data to the OWL instance of the current species.
	 * 
	 * @throws OntoException
	 */
	private void representPhaseData() throws OntoException{
		linkPhaseToMechanism();
		linkPhaseToReaction();
		linkPhaseToSpecies();
		linkPhaseToElements();
		linkPhaseToIdentifier();
		linkPhaseToDimension();
		linkPhaseToThermoModel();
		linkPhaseToTransportModel();
	}
	
	/**
	 * Creates the containedIn link between the phase and the mechanism.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToMechanism() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().trim().isEmpty()){
			compChemOwlWriter.linkInstance(appConfigOntokin.getObjectPropertyContainedIn(), appConfigOntokin
					.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)), appConfigOntokin
							.getOntokinMechanism().concat(UNDERSCORE).concat(Long.toString(mechanismInstanceId)));
		}
	}

	/**
	 * Creates the hasChemicalReactionArray link between the phase and the reaction.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToReaction() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().isEmpty()){
			if(compChem.getName()!=null && !compChem.getName().isEmpty()){
				compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getReactionDataSource(), HASH, HASH.concat(appConfigCtml.getGasPhaseReactionDataId()));
			}
		}
	}
	
	/**
	 * Creates the hasChemicalSpeciesArray link between the phase and the species.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToSpecies() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().isEmpty()){
			if(compChem.getName()!=null && !compChem.getName().isEmpty()){
				compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getPhaseSpeciesArray(), HASH, compChem.getName());
				compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getSpeciesDataSource(), HASH, HASH.concat(appConfigCtml.getGasSpeciesDataId()));
			}
		}
	}

	/**
	 * Creates the hasElement link between the phase and elements.</br>
	 * It also creates the value of hasChemicalElementArrayId. 
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToElements() throws OntoException{
		if(compChem.getComposition()!=null){
			OWLIndividual elementMDIndividual = createElementMetadataInstance();
			for (String element : compChem.getComposition().keySet()) {
				linkPhaseToElement(element);
				representElementMetadata(element, elementMDIndividual);
			}
			compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getElementDataSource(), HASH, HASH.concat(appConfigCtml.getGasElementDataId()));
		}
	}
	
	/**
	 * Creates the identifier link between the phase and the identifier of the gas phase.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToIdentifier() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(individual, DublinCoreVocabulary.IDENTIFIER.getIRI().toString(), EMPTY, "GAS");
		}
	}
	
	/**
	 * Creates the dimension data property of the phase.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToDimension() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(individual,  appConfigOntokin.getPhaseDimension(), EMPTY, "3");
		}
	}
	
	/**
	 * Creates the thermo model data property of the phase.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToThermoModel() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(individual,  appConfigOntokin.getThermoModel(), HASH, appConfigCtml.getGasPhaseThermoModelType());
		}
	}
	
	/**
	 * Creates the transport model data property of the phase.</br>
	 * As the compchem species specification does not include transport,</br>
	 * its value is set to 'None'
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToTransportModel() throws OntoException{
		if(compChem.getPhase()!=null && !compChem.getPhase().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(individual,  appConfigOntokin.getTransportModel(), HASH, appConfigCtml.getGasPhaseTxModelTypeNone());
		}
	}
	
	/**
	 * Creates the corresponding OWL instance of the current species.
	 * 
	 * @throws OntoException
	 */
	private void createSpeciesInstance() throws OntoException{
		
		individual = compChemOwlWriter.createInstance(appConfigOntokin
				.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(++speciesInstanceId)), appConfigOntokin
				.getClassSpecies());
	}

	/**
	 * Links the species to its phase.
	 * 
	 * @throws OntoException
	 */
	private void linkSpeciesToPhase() throws OntoException{
		compChemOwlWriter.linkInstance(appConfigOntokin.getOntokinBelongsToPhase(), appConfigOntokin
				.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(speciesInstanceId)), appConfigOntokin
						.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)));
	}
	
	/**
	 * Creates the corresponding OWL instance of the current element metadata.
	 * 
	 * @throws OntoException
	 */
	private OWLIndividual createElementMetadataInstance() throws OntoException{
		++elementMetaDataInstanceId;
		return compChemOwlWriter.createInstance(appConfigOntokin
				.getElementMetadata().concat(UNDERSCORE).concat(Long.toString(elementMetaDataInstanceId)), appConfigOntokin
				.getElementMetadata());
	}
	
	/**
	 * Creates the corresponding OWL instance of the current reaction metadata.
	 * 
	 * @throws OntoException
	 */
	private void createReactionMetadataInstance() throws OntoException{
		++reactionMetaDataInstanceId;
		individual = compChemOwlWriter.createInstance(appConfigOntokin
				.getReactionMetadata().concat(UNDERSCORE).concat(Long.toString(reactionMetaDataInstanceId)), appConfigOntokin
				.getReactionMetadata());
	}
	
	/**
	 * Creates the corresponding OWL instance of the current species metadata.
	 * 
	 * @throws OntoException
	 */
	private void createSpeciesMetadataInstance() throws OntoException{
		++speciesMetaDataInstanceId;
		individual = compChemOwlWriter.createInstance(appConfigOntokin
				.getSpeciesMetadata().concat(UNDERSCORE).concat(Long.toString(speciesMetaDataInstanceId)), appConfigOntokin
				.getSpeciesMetadata());
	}
	
	private void representReactionMetadata() throws OntoException{
		compChemOwlWriter.addDataPropertyToIndividual(individual,  appConfigOntokin.getReactionDataCaseSensitivity(), HASH, appConfigCtml.getIsReactionDataCaseSensitive());
		compChemOwlWriter.addDataPropertyToIndividual(individual,  DublinCoreVocabulary.IDENTIFIER.getIRI().toString(), EMPTY, appConfigCtml.getGasPhaseReactionDataId());		
	}
	
	private void representSpeciesMetadata() throws OntoException{
		compChemOwlWriter.linkInstance(appConfigOntokin.getSpeciesMetadataProperty(), appConfigOntokin
				.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(speciesInstanceId)), appConfigOntokin
						.getSpeciesMetadata().concat(UNDERSCORE).concat(Long.toString(speciesMetaDataInstanceId)));
		compChemOwlWriter.addDataPropertyToIndividual(individual,  appConfigOntokin.getSpeciesDataCaseSensitivity(), HASH, appConfigCtml.getIsSpeciesDataCaseSensitive());
		compChemOwlWriter.addDataPropertyToIndividual(individual,  DublinCoreVocabulary.IDENTIFIER.getIRI().toString(), EMPTY, appConfigCtml.getGasSpeciesDataId());		
	}
	
	private void representElementMetadata(String element, OWLIndividual individual) throws OntoException{
		compChemOwlWriter.linkInstance(appConfigOntokin.getElementMetadataProperty(), appConfigOntokin
				.getOntokinElement().concat(UNDERSCORE).concat(element), appConfigOntokin
						.getElementMetadata().concat(UNDERSCORE).concat(Long.toString(elementMetaDataInstanceId)));
		compChemOwlWriter.addDataPropertyToIndividual(individual,  appConfigOntokin.getElementDataCaseSensitivity(), HASH, appConfigCtml.getIsElementDataCaseSensitive());
		compChemOwlWriter.addDataPropertyToIndividual(individual,  DublinCoreVocabulary.IDENTIFIER.getIRI().toString(), EMPTY, appConfigCtml.getGasElementDataId());		
	}
	
	/**
	 * Adds data to the OWL instance of the current species.
	 * 
	 * @throws OntoException
	 */
	private void representSpeciesData() throws OntoException{
		addSpeciesName();
	}
	
	/**
	 * Adds the name of the current species to the already created OWL instance.
	 * 
	 * @throws OntoException
	 */
	private void addSpeciesName() throws OntoException{
		if(compChem.getName()!=null && !compChem.getName().trim().isEmpty())
		{
			compChemOwlWriter.addInstanceName(appConfigOntokin
					.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(speciesInstanceId)), compChem.getName());
		}
	}
	
	/**
	 * Creates the corresponding OWL instances of the (high-temperature and 
	 * low-temperature) Thermo Models of the current species.
	 * 
	 * @throws OntoException
	 */
	private void createThermoModelInstances() throws OntoException {
		createLowTemperatureInstance();
		createHighTemperatureInstance();
	}
	
	/**
	 * Creates an instance of Thermo Model represent NASA Polynomial
	 * Coefficients in High-Temperature region.
	 * 
	 * @throws OntoException
	 */
	private void createHighTemperatureInstance() throws OntoException {
		if (compChem.getHighTCoeff() != null && !compChem.getHighTCoeff().trim().isEmpty()) {
			++nasaPolyCoeffsInstanceId;
			individual = getCurrentThermoIndividual();
			linkToSpecies();
			representThermoData(individual, compChem.getHighTCoeff());
			representMaximumTemperature(individual, compChem.gettMax());
			representMinimumTemperature(individual, compChem.gettMid());
			representNumberOfCoefficients(compChem.getNumberOfHighTCoeff());
			representNameOfCoefficientArray();
			representQuantumCalculationIRI();
			representThermoAgentIRI();
		}
	}
	
	/**
	 * Creates an instance of Thermo Model represent NASA Polynomial
	 * Coefficients in Low-Temperature region.
	 * 
	 * @throws OntoException
	 */
	private void createLowTemperatureInstance() throws OntoException {
		if (compChem.getLowTCoeff() != null && !compChem.getLowTCoeff().trim().isEmpty()) {
			++nasaPolyCoeffsInstanceId;
			individual = getCurrentThermoIndividual();
			linkToSpecies();
			representThermoData(individual, compChem.getLowTCoeff());
			representMaximumTemperature(individual, compChem.gettMid());
			representMinimumTemperature(individual, compChem.gettMin());
			representNumberOfCoefficients(compChem.getNumberOfLowTCoeff());
			representNameOfCoefficientArray();
			representQuantumCalculationIRI();
			representThermoAgentIRI();
		}
	}
	
	/**
	 * Creates the hasThermoModel link between the species and the Thermo Model.
	 * 
	 * @throws OntoException
	 */
	private void linkToSpecies() throws OntoException{
			compChemOwlWriter.linkInstance(appConfigOntokin.getObjectPropertyHasThermoModel(), appConfigOntokin
					.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(speciesInstanceId)), appConfigOntokin
							.getClassThermoModel().concat(UNDERSCORE).concat(Long.toString(nasaPolyCoeffsInstanceId)));
	}
	
	/**
	 * Creates the hasElementNumber link between the Species and Element Number classes.
	 * Also creates the hasElement link between the Species and Element classes.
	 * 
	 * @throws OntoException
	 */
	private void linkSpeciesToElement(String element) throws OntoException{
			compChemOwlWriter.linkInstance(appConfigOntokin.getObjectPropertyHasElementNumber(), appConfigOntokin
					.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(speciesInstanceId)), appConfigOntokin
							.getOntokinElementNumber().concat(UNDERSCORE).concat(Long.toString(++elementNumberInstanceId)));
			compChemOwlWriter.linkInstance(appConfigOntokin.getObjectPropertyHasElement(), appConfigOntokin
					.getClassSpecies().concat(UNDERSCORE).concat(Long.toString(speciesInstanceId)), appConfigOntokin
							.getOntokinElement().concat(UNDERSCORE).concat(element));
			compChemOwlWriter.linkInstance(appConfigOntokin.getIndicatesNumberOf(), appConfigOntokin
							.getOntokinElementNumber().concat(UNDERSCORE).concat(Long.toString(elementNumberInstanceId)), appConfigOntokin
							.getOntokinElement().concat(UNDERSCORE).concat(element));
	}

	/**
	 * Creates the hasElement link between the Phase and Element classes.
	 * 
	 * @throws OntoException
	 */
	private void linkPhaseToElement(String element) throws OntoException{
			compChemOwlWriter.linkInstance(appConfigOntokin.getObjectPropertyHasElement(), appConfigOntokin
					.getOntokinPhase().concat(UNDERSCORE).concat(Long.toString(phaseInstanceId)), appConfigOntokin
							.getOntokinElement().concat(UNDERSCORE).concat(element));
	}
	
	/**
	 * Adds NASA Polynomial Coefficients to the OWL instance of the current thermo model.
	 * 
	 * @param individual
	 * @param thermoData
	 * @throws OntoException
	 */
	private void representThermoData(OWLIndividual individual, String thermoData) throws OntoException{
		compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getDataPropertyHasCoeffValues(), HASH, thermoData);
	}
	
	/**
	 * Adds NASA Polynomial Coefficients to the OWL instance of the current thermo model.
	 * 
	 * @param individual
	 * @param thermoData
	 * @throws OntoException
	 */
	private void representNumberOfElement(OWLIndividual individual, String numberOfElement) throws OntoException{
		compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getDataPropertyNumberOfElement(), HASH, numberOfElement);
	}

	/**
	 * Adds maximum temperature to the OWL instance of the current thermo model.
	 * 
	 * @param individual
	 * @param maxTemperature the maximum temperature above which the high-
	 * temperature NASA polynomial coefficients are invalid.
	 * @throws OntoException
	 */
	private void representMaximumTemperature(OWLIndividual individual, String maxTemperature) throws OntoException{
		if(compChem.gettMax() != null && !compChem.gettMax().trim().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getDataPropertyTempMax(), HASH, maxTemperature);
		}
	}

	/**
	 * Adds maximum temperature to the OWL instance of the current thermo model.
	 * 
	 * @param individual
	 * @param minTemperature the minimum temperature below which the low-
	 * temperature NASA polynomial coefficients are invalid.
	 * @throws OntoException
	 */
	private void representMinimumTemperature(OWLIndividual individual, String minTemperature) throws OntoException{
		if(compChem.gettMin() != null && !compChem.gettMin().trim().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getDataPropertyTempMin(), HASH, minTemperature);
		}
	}
	
	/**
	 * Adds number of coefficients to the thermo model at a temperature range.
	 * 
	 * @param numOfCoeffs the number of coefficients comes out at a temperature.
	 * @throws OntoException
	 */
	private void representNumberOfCoefficients(int numOfCoeffs) throws OntoException{
		if(numOfCoeffs!=0){	
			compChemOwlWriter.addDataPropertyToIndividual(individual, appConfigOntokin.getOntokinHasNumberOfCoefficients(), HASH, Integer.toString(numOfCoeffs));
		}
	}
	
	/**
	 * Adds the name of coefficients array to the thermo model.
	 * 
	 * @throws OntoException
	 */
	private void representNameOfCoefficientArray() throws OntoException{
		if(appConfigCtml.getSpeciesNasaFloatArrayName()!=null && !appConfigCtml.getSpeciesNasaFloatArrayName().isEmpty()){	
			compChemOwlWriter.addDataPropertyToIndividual(individual, OWLRDFVocabulary.RDFS_LABEL.toString(), EMPTY, appConfigCtml.getSpeciesNasaFloatArrayName().toString());
		}
	}
	
	/**
	 * Adds the unique species IRI to the given species.
	 * 
	 * @throws OntoException
	 */
	private void representUniqueSpeciesIRI() throws OntoException{	
		if(compChem.getUniqueSpeciesIRI() != null && !compChem.getUniqueSpeciesIRI().trim().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(getCurrentSpeciesIndividual(), appConfigOntokin.getDataPropertyUniqueSpeciesIRI(), HASH, compChem.getUniqueSpeciesIRI());
		}
	}
	
	/**
	 * If an individual of the current species has already been created, it
	 * returns that, otherwise it creates an individual and returns this.
	 * 
	 * @return
	 * @throws OntoException
	 */
	private OWLIndividual getCurrentSpeciesIndividual() throws OntoException{
		individual = compChemOwlWriter.createInstance(
				appConfigOntokin.getClassSpecies().concat(UNDERSCORE)
						.concat(Long.toString(speciesInstanceId)),
				appConfigOntokin.getClassSpecies());
		return individual;
	}

	/**
	 * If an individual of the current thermo model has already been created, it
	 * returns that, otherwise it creates an individual and returns this.
	 * 
	 * @return
	 * @throws OntoException
	 */
	private OWLIndividual getCurrentThermoIndividual() throws OntoException{
		individual = compChemOwlWriter.createInstance(
				appConfigOntokin.getClassThermoModel().concat(UNDERSCORE)
						.concat(Long.toString(nasaPolyCoeffsInstanceId)),
				appConfigOntokin.getOntokinNasaPolyCoefficient());
		return individual;
	}
	
	/**
	 * Adds the quantum calculation IRI to the given Thermo Model.
	 * 
	 * @throws OntoException
	 */
	private void representQuantumCalculationIRI() throws OntoException{	
		if(compChem.getQuantumCalculationIRI() != null && !compChem.getQuantumCalculationIRI().trim().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(getCurrentThermoIndividual(), appConfigOntokin.getDataPropertyQuantumCalculationIRI(), HASH, compChem.getQuantumCalculationIRI());
		}
	}
	
	/**
	 * Adds the thermo-agent IRI to the given thermo-model.
	 * 
	 * @throws OntoException
	 */
	private void representThermoAgentIRI() throws OntoException{	
		if(compChem.getThermoAgentIRI() != null && !compChem.getThermoAgentIRI().trim().isEmpty()){
			compChemOwlWriter.addDataPropertyToIndividual(getCurrentThermoIndividual(), appConfigOntokin.getDataPropertyThermoAgentIRI(), HASH, compChem.getThermoAgentIRI());
		}
	}
	
	/**
	 * Creates the relations between the current species and its elements.
	 * 
	 * @throws OntoException
	 */
	private void representSpeciesElements() throws OntoException {
		if (compChem.getComposition() != null) {
			for (String element : compChem.getComposition().keySet()) {
				linkSpeciesToElement(element);
				OWLIndividual elementNumberIndividual = compChemOwlWriter.createInstance(appConfigOntokin.getOntokinElementNumber()
						.concat(UNDERSCORE).concat(Long.toString(elementNumberInstanceId)),
						appConfigOntokin.getOntokinElementNumber());
				representNumberOfElement(elementNumberIndividual, Integer.toString(compChem.getComposition().get(element)));
				compChemOwlWriter.addInstanceName(appConfigOntokin.getOntokinElement()
						.concat(UNDERSCORE).concat(element), element);
				representAtomicMass(element);
			}
		}
	}
	
	/**
	 * Represents the atomic mass of the element currently being processed.
	 * 
	 * @param element
	 * @throws OntoException
	 */
	private void representAtomicMass(String element) throws OntoException{
		if (compChem.getAtomicMasses() != null && compChem.getAtomicMasses().containsKey(element)
				&& compChem.getAtomicMasses().get(element) != null
				&& !compChem.getAtomicMasses().get(element).trim().isEmpty()) {
			// Adds atomic weight and units to the elements.
			OWLIndividual elementIndividual = compChemOwlWriter.createInstance(
					appConfigOntokin.getOntokinElement().concat(UNDERSCORE).concat(element),
					appConfigOntokin.getOntokinElement());
			compChemOwlWriter.addDataPropertyToIndividual(elementIndividual,
					appConfigOntokin.getElementAtomicWt(), HASH, compChem.getAtomicMasses().get(element).trim());
			if(compChem.getAtomicWeightUnits()!=null && !compChem.getAtomicWeightUnits().isEmpty()){
				compChemOwlWriter.addDataPropertyToIndividual(elementIndividual,
						appConfigOntokin.getDataPropertyAtomicWtUnits(), HASH,
						getAtomicMassUnitInGPerMol(compChem.getAtomicWeightUnits()));
			}
		}
	}
	
	/**
	 * CompChem sends the atomic weight unit in Dalton while the Fortran</br>
	 * converter uses this in g/mol. Therefore, Dalton is converted to g/mol</br>
	 * to make it suitable for this converter.  
	 * 
	 * @param units
	 * @return
	 */
	private String getAtomicMassUnitInGPerMol(String units){
		if(units.equalsIgnoreCase("http://data.nasa.gov/qudt/owl/unit#Dalton") || units.equalsIgnoreCase("Dalton")){
			return "g/mol";
		}
		return units;
	}
	
	/**
	 * Represents the current commit hash using OWL.
	 * 
	 * @throws OntoException
	 */
	private void representCommitHash() throws OntoException{
		String commitHash = null;
		try{
			commitHash = CtmlConverterUtils.gitCommitHash();
		}catch(Exception e){}
		if (commitHash != null && !commitHash.isEmpty()) {
			OWLLiteral label = dataFactory.getOWLLiteral(commitHash);
			OWLAnnotationProperty commit = dataFactory.getOWLAnnotationProperty(IRI.create(ontoChemKB
					.getOntoKinKbTBoxIri().concat("#").concat(appConfigOntokin.getCompChemGitCommitHash())));
			OWLAnnotation commitAttribute = dataFactory.getOWLAnnotation(commit, label);
			manager.applyChange(new AddOntologyAnnotation(ontology, commitAttribute));
		}
	}
	
	/**
	 * Saves the ontology created for codifying a single species chemical mechanism.
	 */
	public void saveOntology() throws OWLOntologyStorageException {
		try {
			// Adds the import clause to the OntoChem TBox
			OWLImportsDeclaration importDeclaration = OWLManager.getOWLDataFactory()
					.getOWLImportsDeclaration(IRI.create(ontoChemKB.getOntoKinKbTBoxIri()));
			manager.applyChange(new AddImport(ontology, importDeclaration));
			// Saves the ontology in the file system
			manager.saveOntology(ontology, ontologyIRIFileSave);
			logger.info("Started removing TBox from the mechanism OWL file.");
			iOwlConstructWriter.removeTBox(ontologyIRIFileSave);
			logger.info("Finished removing TBox from the mechanism OWL file.");
		} catch (OWLOntologyStorageException e1) {
			logger.error("The ontology could not be saved.");
			throw new OWLOntologyStorageException("The ontology could not be saved.");
		}
	}

}
