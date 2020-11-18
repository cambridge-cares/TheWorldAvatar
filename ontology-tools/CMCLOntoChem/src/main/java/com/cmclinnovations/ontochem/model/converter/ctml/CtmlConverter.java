package com.cmclinnovations.ontochem.model.converter.ctml;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.LinkedHashMap;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.StreamDocumentTarget;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.slf4j.Logger;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import com.cmclinnovations.ontochem.model.CtmlConverterState;
import com.cmclinnovations.ontochem.model.InitCtmlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.utils.CtmlConverterUtils;

/**
 * A converter that can convert a set of CTML files, each containing a chemical
 * mechanism, into OWL files. When it receives a request for conversion, in each
 * iteration, it parses a CTML file to extract the CTML metadata i.e. CMCL
 * version number and comment. It also extracts phase, species and reaction
 * data. As soon as the value of a CTML element or data property is extracted, 
 * it represents this in OWL. Following the extraction and representation of 
 * a chemical in OWL, it saves the OWL content as a file to the disk.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class CtmlConverter extends CtmlConverterState implements ICtmlConverter, LexicalHandler, ContentHandler{

	// Stores a multiline value of an element.
	public StringBuffer multiLineValue = new StringBuffer();

	private static Logger logger = org.slf4j.LoggerFactory.getLogger(CtmlConverter.class);
	
	private BufferedWriter listOfOntoKinAboxes;
	
	OWLOntologyManager managerKB;

    OWLDataFactory factoryKB;
    
    OWLOntology ontologyKB;
    
    IRI ontologyKBIRI;
    
	public static String lastComment;

	public void startDTD(String name, String publicId, String systemId) throws SAXException {
	}

	public void endDTD() throws SAXException {
	}

	public void startEntity(String name) throws SAXException {
	}

	public void endEntity(String name) throws SAXException {
	}

	public void startCDATA() throws SAXException {
	}

	public void endCDATA() throws SAXException {
	}

	public void comment(char[] text, int start, int length) throws SAXException {
		lastComment = new String(text, start, length).trim();
	}

	/**
	 * This method manages all the read operations for
	 * extracting CTML meta data as well as phase, species and
	 * reactions data from CTML
	 */
	public void characters(char[] ch, int start, int length) throws SAXException {
		// Reads multiline values
		multiLineValue.append(ch, start, length);
		// Calls the method that forwards the call to
		// the metadata writer.
		iCtmlMetadataWriter.writer(ch, start, length);
		// Calls the method that forwards the call to
		// the metadata writer.
		iPhaseWriter.writer(ch, start, length);
		// Calls the method that forwards the call to
		// the element writer.
		iElementWriter.writer(ch, start, length);
		// Calls the method that forwards the call to
		// the species writer.
		iSpeciesWriter.writer(ch, start, length);
		// Calls the method that forwards the call to
		// the species writer.
		iReactionWriter.writer(ch, start, length);
	}

	public void endDocument() {
	}

	/**
	 * SAXParser calls endElement automatically when it reads a
	 * closing tag (or element) of the tag which is being
	 * processed.
	 * 
	 * SAXParser requires a special technique for extracting
	 * multiline values which are codified in between a start
	 * tag and an end tag. For example, comments and species
	 * array. To extract such values in their entireties,
	 * the StringBuffer.append() method keeps running until
	 * it fins the corresponding closing tag.
	 * 
	 * In CTML, there are some elements which do not always
	 * appear with the same set of sub-elements. Therefore, the
	 * end of parsing of a specific sub-element cannot be used
	 * to determine the end of parsing of such elements. For
	 * them, endElement trigger is used to detect the end of
	 * parsing precisely.
	 * 
	 */
	public void endElement(String uri, String localName, String qName) {
		// Detects the termination of comments that might
		// go beyond a single line.
		endComments(qName);
		// Detects the termination of some of phase elements.
		endSomePhaseElements(qName);
		// Detects the termination of a number of species
		// elements.
		endSomeSpeciesElements(qName);
		// Detects the termination of a number of reaction
		// elements.
		endSomeReactionElements(qName);
		// Marks the end of parsing of a number of CTML elements.
		markEnd(qName);

	}

	public void endPrefixMapping(String prefix) {
	}

	public void ignorableWhitespace(char[] ch, int start, int length) {
	}

	public void processingInstruction(String target, String data) {
	}

	public void setDocumentLocator(Locator locator) {
	}

	public void skippedEntity(String name) {
	}

	public void startDocument() {
	}

	public void startElement(String uri, String localName, String qName, Attributes attributes) {
		// Prepares the string buffer for reuse. It is used
		// to read multiline values i.e. the CTML (root level)
		// comment, the species arrays, species thermo
		// comments and NASA Polynomical coefficients.
		multiLineValue.setLength(0);
		// Calls the method that forwards the call to the
		// the CTML parser.
		convert(qName, attributes);
	}

	public void startPrefixMapping(String prefix, String uri) {
	}

	/**
	 * The main method that will help user to run the CTML to OWL converter from
	 * command prompt.
	 * 
	 * @param argv
	 * @throws OWLOntologyCreationException
	 * @throws OntoException
	 */
	public static void main(String argv[]) throws OWLOntologyCreationException, OntoException {
		String MECHANISM_FILE_NAME = "chemical_mechanism.xml";
		ArrayList<String> arrayList = new ArrayList<String>();
		arrayList.add("C:\\Users\\Zhou%20Li\\Feroz_s_Work\\Development\\Mechanisms\\CoMo\\H2\\" + MECHANISM_FILE_NAME);
		new CtmlConverter().convert(arrayList, "file:C:/Users/farazi/Test_CtmlToOwlConversion");
	}

	/**
	 * Converts a CTML file into an OWL file.</br>
	 * It also supports the conversion of multiple files.</br>
	 * The conversion process takes a CTML file, uses SAX parser to parse</br>
	 * the CTML metadata such as comment and version as well as phase data,</br>
	 * element data, species data and reaction data.
	 * 
	 * 
	 * @param ctmlFiles
	 * @param owlFilePath
	 * @throws OntoException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> ctmlFiles, String owlFilesPath)
			throws OntoException, OWLOntologyCreationException {
		// Checks if the user shown path to save the OWL file is valid
		if (owlFilesPath == null) {
			logger.error("The ontology file path is null.");
			throw new OntoException("The ontology file path is null.");
		}
		// Checks if the list that contains the path to the input CTML files
		// exists
		if (ctmlFiles == null) {
			logger.error("Ctml source files are empty.");
			throw new OntoException("Ctml source files are empty.");
		}
		// Each iteration parses a CTML file to extract the values
		// of CTML elements and properties
		for (String ctmlFile : ctmlFiles) {
			// Initialise the instances of the classes that hold CTML metadata
			// and data as well as parsing status information thereof.
			// Also initialises the instances of the classes that read
			// configuration parameters using Spring framework annotations.
			initCtmlConverter = new InitCtmlConverter();
			initCtmlConverter.init();
			// Replaces any space in the user shown OWL file path with %20
			// to form a valid URL.
			String owlFilePath = CtmlConverterUtils.formOwlUrl(ctmlFile, owlFilesPath);
			if (owlFilePath.contains(" ")) {
				owlFilePath = owlFilePath.replaceAll(" ", "%20");
			}
			// Extracts and saves the base URL of a mechanism OWL ontology.
			basePath = CtmlConverterUtils.formBaseURL(ctmlFile);
			basePathTBox = ontoChemKB.getOntoKinKbTBoxIri();
			// Creates an IRI for the OWL ontology that is
			// being created to codify a mechanism
			ontologyIRI = IRI.create(owlFilePath);
			if (ontologyIRI == null) {
				logger.error("An IRI for an ontology could not be created.");
				throw new OntoException("An IRI for an ontology could not be created.");
			}
			kbIRI = IRI.create(ontoChemKB.getOntoKinKbURL().concat(ontoChemKB.getOntoChemOntolgyFileName()));
			// Replaces any space in the user shown OWL file path with %20
			// to form a valid URL.
			String owlFilePathForFileSave = CtmlConverterUtils.formOwlFileSaveUrl(ctmlFile, owlFilesPath);
			if (owlFilePathForFileSave.contains(" ")) {
				owlFilePathForFileSave = owlFilePathForFileSave.replaceAll(" ", "%20");
			}
			// Creates an IRI to save the OWL ontology on the file system
			ontologyIRIFileSave = IRI.create(owlFilePathForFileSave);
			ontology = manager.createOntology(IRI.create(basePathABox));
			if (ontology == null) {
				logger.error("The requested ontology could not be created.");
				throw new OntoException("Ontology could not be created.");
			}
			kb = manager.createOntology(kbIRI);
			mechanismName = ctmlFile;
			// Checks if the mechanism name or ctml file name exists
			if (mechanismName == null) {
				logger.error("Ctml source files are empty.");
				throw new OntoException("Ctml source files are empty.");
			}
			basePathABox = ontoChemKB.getOntoKinKbURL().concat(CtmlConverterUtils.extractMechanismName(ctmlFile)).concat(opCtrl.getOwlFileExtension());
			CtmlConverter ctmlConverter = new CtmlConverter();
		    XMLReader parser;

		    try {
		      parser = XMLReaderFactory.createXMLReader();
		    } catch (SAXException ex1) {
		      try {
		        parser = XMLReaderFactory.createXMLReader("org.apache.xerces.parsers.SAXParser");
		      } catch (SAXException ex2) {
		        return;
		      }
		    }

		    try {
		      parser.setProperty("http://xml.org/sax/properties/lexical-handler", ctmlConverter);
		    } catch (SAXNotRecognizedException e) {
		      return;
		    } catch (SAXNotSupportedException e) {
		      return;
		    }

		    parser.setContentHandler(ctmlConverter);
			// Before it starts parsing, checks if the file exists.
		    File f = new File(ctmlFile);
			if (f.exists()) {
			} else {
				logger.info("There is no mechanism file in the following path:" + ctmlFile);
			}

		    try {
	    	      File file = new File(ctmlFile);
	    	      InputStream inputStream= new FileInputStream(file);
	    	      Reader reader = new InputStreamReader(inputStream,"UTF-8");
	    	      InputSource is = new InputSource(reader);
	    	      is.setEncoding("UTF-8");
		    	parser.parse(is);
		      iOwlConstructWriter.saveOntology();
		    } catch (SAXParseException e) {
		    	logger.error("SAXParseException occurred.");
		    	e.printStackTrace();
		    } catch (SAXException e) { 
		    	logger.error("SAXException occurred.");
		    	e.printStackTrace();
		    } catch (IOException e) {
		    	logger.error("IOException occurred.");
		    	e.printStackTrace();
		    } catch (OWLOntologyStorageException e) {
			    logger.error("OWLOntologyStorageException occurred.");  
		    	e.printStackTrace();
			}
			// Saves the URL of the OntoKin KB's ABox OWL file URL
			saveOntoKinABox(owlFilesPath, CtmlConverterUtils.extractMechanismName(ctmlFile).concat(opCtrl.getOwlFileExtension()));
		}
		// Closes the the file that saves the list of OntoKin KB ABox
		// OWL files.
		try {
			if (listOfOntoKinAboxes != null) {
				listOfOntoKinAboxes.close();
			}
		} catch (IOException e) {
			logger.error("File containing a list of OntoKin ABox OWL ontology could not be closed.");
			e.printStackTrace();
		}
		// Saves the KB in the file system
		try{
			System.out.println("file iri that does not work:"+"file:/".concat(ontoChemKB.getOntoChemKBFilePath().concat(ontoChemKB.getOntoChemKBFileName())));
			// Saves the kb in the file system
			managerKB.saveOntology(ontologyKB, IRI.create(
					"file:/".concat(ontoChemKB.getOntoChemKBFilePath().concat(ontoChemKB.getOntoChemKBFileName()))));
		}catch(OWLOntologyStorageException e){
			logger.error("OWLOntologyStorageException occurred.");
		}
	}
	
	/**
	 * Detects the termination of reaction rate coefficients and process them
	 * to codify in the OWL ontology being created.
	 *  
	 * @param qName
	 */
	private void endRateCoefficients(String qName){
		endArrheniusCoefficientA(qName);
		endArrheniusCoefficientb(qName);
		endArrheniusCoefficientE(qName);
		endArrheniusCoefficientP(qName);
		endCoverageCoefficienta(qName);
		endCoverageCoefficientm(qName);
		endCoverageCoefficiente(qName);
		endLandauTellerCoefficientB(qName);
		endLandauTellerCoefficientC(qName);
		endFallOffParameters(qName);
		endTMin(qName);
		endTMax(qName);
		endPMin(qName);
		endPMax(qName);		
	}
	
	/**
	 * Detects the termination of the Arrhenius coefficient A and process this
	 * to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endArrheniusCoefficientA(String qName) {
		// Deals with comments that might go beyond a single line.
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusA())) {
			String A = multiLineValue.toString();
			if (arrheniusParseStatus.isArrhenius()) {
				if (!CtmlConverterState.createdArrhenius) {
					iReactionWriter.decideArrheniusOrSticking();
					CtmlConverterState.createdArrhenius = true;
				}
				addToEitherArrheniusOrSticking(A);
			}
		}
	}
	
	/**
	 * Based on the decision taken in the method called iReactionWriter
	 * .decideArrheniusOrSticking(), this method adds the pre-exponential
	 * coefficient either to Arrhenius or to Sticking.
	 * 
	 * @param A the value of the pre-exponetial factor Arrhenius coefficient
	 */
	private void addToEitherArrheniusOrSticking(String A){
		if (createdArrheniusArrhenius) {
			iReactionWriter.writeArrheniusProperties(appConfigOntokin.getClassArrheniusCoefficient(),
					rateCoeffArrheniusInstanceId);
			iReactionWriter.writeCoefficientA(A, appConfigOntokin.getClassArrheniusCoefficient(),
					rateCoeffArrheniusInstanceId);
		} else if (createdArrheniusSticking) {
			iReactionWriter.writeArrheniusProperties(appConfigOntokin.getClassStickingCoefficient(),
					rateCoeffStickingInstanceId);
			iReactionWriter.writeCoefficientA(A, appConfigOntokin.getClassStickingCoefficient(),
					rateCoeffStickingInstanceId);
		}
	}
	
	/**
	 * Detects the termination of the Arrhenius coefficient b and process this
	 * to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endArrheniusCoefficientb(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusB())) {
			String b = multiLineValue.toString();
			if (createdArrheniusArrhenius) {
				iReactionWriter.writeCoefficientb(b, appConfigOntokin.getClassArrheniusCoefficient(),
						rateCoeffArrheniusInstanceId);
			} 
			if (createdArrheniusSticking){
				iReactionWriter.writeCoefficientb(b, appConfigOntokin.getClassStickingCoefficient(),
						rateCoeffStickingInstanceId);
			}
		}
	}

	/**
	 * Detects the termination of the Arrhenius coefficient E and process this
	 * to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endArrheniusCoefficientE(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusE())) {
			String E = multiLineValue.toString();
			if (createdArrheniusArrhenius) {
				iReactionWriter.writeCoefficientE(E, appConfigOntokin.getClassArrheniusCoefficient(),
						rateCoeffArrheniusInstanceId);
			} 
			if (createdArrheniusSticking){
				iReactionWriter.writeCoefficientE(E, appConfigOntokin.getClassStickingCoefficient(),
						rateCoeffStickingInstanceId);
			}
		}
	}
	
	/**
	 * Detects the termination of the reference pressure P for Arrhenius 
	 * reaction rate computation and process this to codify in the OWL 
	 * ontology being created.
	 * 
	 * @param qName
	 */
	private void endArrheniusCoefficientP(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusP())) {
			String P = multiLineValue.toString();
			if (createdArrheniusArrhenius) {
				iReactionWriter.writeCoefficientP(P, appConfigOntokin.getClassArrheniusCoefficient(),
						rateCoeffArrheniusInstanceId);
			} 
			if (createdArrheniusSticking){
				iReactionWriter.writeCoefficientP(P, appConfigOntokin.getClassStickingCoefficient(),
						rateCoeffStickingInstanceId);
			}
		}
	}
	
	/**
	 * Detects the termination of the coverage dependency parameter a for a 
	 * reaction rate computation and process this to codify in the OWL ontology
	 * being created.
	 * 
	 * @param qName
	 */
	private void endCoverageCoefficienta(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageA())) {
			String a = multiLineValue.toString();
			iReactionWriter.writeCoverageDependecies();
			iReactionWriter.writeCoefficienta(a);
		}
	}
	
	/**
	 * Detects the termination of the coverage dependency parameter m for a 
	 * reaction rate computation and process this to codify in the OWL ontology
	 * being created.
	 * 
	 * @param qName
	 */
	private void endCoverageCoefficientm(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageM())) {
			String m = multiLineValue.toString();
			iReactionWriter.writeCoefficientm(m);
		}
	}
	
	/**
	 * Detects the termination of the coverage dependency parameter e for a 
	 * reaction rate computation and process this to codify in the OWL ontology
	 * being created.
	 * 
	 * @param qName
	 */
	private void endCoverageCoefficiente(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffArrheniusCoverageE())) {
			String e = multiLineValue.toString();
			iReactionWriter.writeCoefficiente(e);
		}
	}
	
	/**
	 * Detects the termination of the Landau-Teller coefficient B for the 
	 * Landau-Teller reaction rate computation and process this to codify in 
	 * the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endLandauTellerCoefficientB(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffLandauTellerB())) {
			String B = multiLineValue.toString();
			iReactionWriter.writeLandauTellerCoeffB(B);
		}
	}
	
	/**
	 * Detects the termination of the Landau-Teller coefficient C for the 
	 * Landau-Teller reaction rate computation and process this to codify in 
	 * the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endLandauTellerCoefficientC(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffLandauTellerC())) {
			String C = multiLineValue.toString();
			iReactionWriter.writeLandauTellerCoeffC(C);
		}
	}
	
	/**
	 * Detects the termination of the Fall-off model parameters for the 
	 * reaction rate computation and process them to codify in the OWL 
	 * ontology being created.
	 * 
	 * @param qName
	 */
	private void endFallOffParameters(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffFallOff())) {
			String fallOffModelData = multiLineValue.toString();
			iReactionWriter.writeFallOffModelData(fallOffModelData);
		}
	}
	
	/**
	 * Detects the termination of the minimum temperature for the CHEB reaction 
	 * rate computation and process them to codify in the OWL ontology being 
	 * created.
	 * 
	 * @param qName
	 */
	private void endTMin(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffTMin())) {
			String tmin = multiLineValue.toString();
			iReactionWriter.writeTminProperty(tmin);
		}
	}
	
	/**
	 * Detects the termination of the maximum temperature for the CHEB reaction 
	 * rate computation and process them to codify in the OWL ontology being 
	 * created.
	 * 
	 * @param qName
	 */
	private void endTMax(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffTMax())) {
			String tmax = multiLineValue.toString();
			iReactionWriter.writeTmaxProperty(tmax);
		}
	}

	/**
	 * Detects the termination of the minimum pressure for the CHEB reaction 
	 * rate computation and process them to codify in the OWL ontology being 
	 * created.
	 * 
	 * @param qName
	 */
	private void endPMin(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffPMin())) {
			String pmin = multiLineValue.toString();
			iReactionWriter.writePminProperty(pmin);
		}
	}
	
	/**
	 * Detects the termination of the maximum pressure for the CHEB reaction 
	 * rate computation and process them to codify in the OWL ontology being 
	 * created.
	 * 
	 * @param qName
	 */
	private void endPMax(String qName){
		if (qName.equals(appConfigCtml.getRateCoeffPMax())) {
			String pmax = multiLineValue.toString();
			iReactionWriter.writePmaxProperty(pmax);
		}
	}

	
	/**
	 * Detects the termination of a multiline comment and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endComments(String qName){
		// Deals with comments that might go beyond a single line.
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlComment())) {
			String comment = multiLineValue.toString();
			endComment(comment);
		}
	}
	
	/**
	 * Detects the termination of the following phase elements:</br>
	 * 1. elementArray;</br>
	 * 2. speciesArray;</br>
	 * 3. site_density; and</br>
	 * 4. phaseArray.
	 * 
	 * @param qName
	 */
	private void endSomePhaseElements(String qName){
		// Detects the termination of elementArray data of 
		// a phase.
		endElementArrayData(qName);
		// Parses species array.
		parseSpeciesArray(qName);
		// Detects the termination of the site_density 
		// (the number of sites) on a surface.
		endSiteDensityValue(qName);
		// Detects the termination of the phaseArray data 
		// of a phase.
		endPhaseArrayData(qName);
	}
	
	/**
	 * Detects the termination of a number of species elements.
	 * 
	 * @param qName
	 */
	private void endSomeSpeciesElements(String qName){
		// Detects the termination of note for a species.
		endNote(qName);
		// Detects the termination of the atom array of a species that might 
		// contain characters requiring UTF-8 encoding.
		endAtomArray(qName);
		// Detects the termination of size of a species.
		endSize(qName);
		// Detects the termination of density of a species.
		endDensity(qName);
		// Parses NASA Polynomial Coefficients.
		parseNasaPolynomialCoefficients(qName);
		// Detects the termination of transport data elements.
		endTransportParameters(qName);
	}
	
	/**
	 * Detects the termination of the transport elements.
	 * 
	 * @param qName
	 */
	private void endTransportParameters(String qName){
		// Detects the termination of the geometry type of a species.
		endString(qName);
		// Detects the termination of the well-depth of a species.
		endLJWellDepth(qName);
		// Detects the termination of the Lennard-Jones 
		// collision diameter of a species.
		endLJDiameter(qName);
		// Detects the termination of the dipole moment 
		// of a species.
		endDipoleMoment(qName);
		// Detects the termination of the polarizability 
		// of a species.
		endPolarizability(qName);
		// Detects the termination of the rotational
		// relaxation collision number of a species. 
		endRotRelax(qName);	
	}
	
	/**
	 * Detects the termination of a number of reaction elements.
	 * 
	 * @param qName
	 */
	private void endSomeReactionElements(String qName){
		// Detects the termination of the reaction order
		// of a species.
		endReactionOrder(qName);
		// Detects the termination of species efficiencies that might 
		// contain characters requiring UTF-8 encoding.
		endEfficiencies(qName);
		// Detects the termination of reactants of a reaction that might 
		// contain characters requiring UTF-8 encoding.
		endReactants(qName);
		// Detects the termination of products of a reaction that might 
		// contain characters requiring UTF-8 encoding.
		endProducts(qName);
		// Detects the termination of an equation to read the
		// the complete equation. SAXParser reads upto
		// the equal symbol otherwise.
		endEquation(qName);
		// Detects the termination of the Arrhenius coefficients.
		endRateCoefficients(qName);
		// Parses Chebyshev Rate Coefficient parameters.
		parseChebyshevRateCoefficientParameters(qName);
	}
	
	/**
	 * Detects the termination of the site density value of the current phase.
	 * 
	 * @param qName
	 */
	private void endSiteDensityValue(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseSiteDensity())) {
			String siteDensityValue = multiLineValue.toString();
			iPhaseWriter.readSiteDensity(siteDensityValue);
		}
	}
	
	/**
	 * Detects the termination of phase array data of the current phase.
	 * 
	 * @param qName
	 */
	private void endPhaseArrayData(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseArray())) {
			String phaseArrayData = multiLineValue.toString();
			iPhaseWriter.readPhaseArray(phaseArrayData);
		}
	}
	
	/**
	 * Detects the termination of elementArrayData of the current phase.
	 * 
	 * @param qName
	 */
	private void endElementArrayData(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseElementArray())) {
			String elementArrayData = multiLineValue.toString();
			iPhaseWriter.readElementArray(elementArrayData);
		}
	}
	
	/**
	 * Detects the termination of a note attached to the current species.
	 * 
	 * @param qName
	 */
	private void endNote(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpeciesNote())) {
			String note = multiLineValue.toString();
			iSpeciesWriter.writeNote(note);
		}
	}
	
	/**
	 * Detects the termination of size of the current species.
	 * 
	 * @param qName
	 */
	private void endSize(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesSize())) {
			String size = multiLineValue.toString();
			iSpeciesWriter.writeSizeInfo(size);
		}
	}
	
	/**
	 * Detects the termination of density of the current species.
	 * 
	 * @param qName
	 */
	private void endDensity(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDensity())) {
			String density = multiLineValue.toString();
			iSpeciesWriter.writeDensityInfo(density);
		}
	}
	
	/**
	 * Detects the termination of string of the current species.
	 * 
	 * @param qName
	 */
	private void endString(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransportString())) {
			String string = multiLineValue.toString();
			iSpeciesWriter.writeStringInfo(string);
		}
	}
	
	/**
	 * Detects the termination of the Lennard-Jones potential well depth of 
	 * the current species.
	 * 
	 * @param qName
	 */
	private void endLJWellDepth(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportLJWelldepth())) {
			String wellDepth = multiLineValue.toString();
			iSpeciesWriter.writeLJWellDepthInfo(wellDepth);
		}
	}
	
	/**
	 * Detects the termination of the Lennard-Jones collision diamter of 
	 * the current species.
	 * 
	 * @param qName
	 */
	private void endLJDiameter(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportLJDiameter())) {
			String diameter = multiLineValue.toString();
			iSpeciesWriter.writeLJDiameterInfo(diameter);
		}
	}
	
	/**
	 * Detects the termination of the dipole moment of the current species. 
	 * 
	 * @param qName
	 */
	private void endDipoleMoment(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportDipoleMoment())) {
			String dipoleMoment = multiLineValue.toString();
			iSpeciesWriter.writeDipoleMomentInfo(dipoleMoment);
		}
	}

	/**
	 * Detects the termination of the polarizability of the current species. 
	 * 
	 * @param qName
	 */
	private void endPolarizability(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportPolarizability())) {
			String polarizability = multiLineValue.toString();
			iSpeciesWriter.writePolarizabilityInfo(polarizability);
		}
	}

	/**
	 * Detects the termination of the rotational relaxation number of a species. 
	 * 
	 * @param qName
	 */
	private void endRotRelax(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlTransportRotRelax())) {
			String rotRelax = multiLineValue.toString();
			iSpeciesWriter.writeRotRelaxInfo(rotRelax);
		}
	}
	
	/**
	 * Detects the termination of the reaction order of a species. 
	 * 
	 * @param qName
	 */
	private void endReactionOrder(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionOrder())) {
			String order = multiLineValue.toString();
			iReactionWriter.writeReactionOrder(order);
		}
	}
	
	/**
	 * Detects the termination of a multiline comment and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endComment(String comment){
		if (speciesThermoParseStatus.isSpeciesThermo()) {
			iOwlConstructWriter.createThermoCommentInOntology(comment);
			speciesThermoParseStatus.setThermoComment(false);
		} else if (speciesParseStatus.isSpeciesComment()) {
			iOwlConstructWriter.readSpeciesComment(comment);
				speciesParseStatus.setSpeciesComment(false);
		} else if (reactionParseStatus.isComment()) {
			iOwlConstructWriter.createReactionComment(comment);
				reactionParseStatus.setComment(false);	
		} else if (elementParseStatus.isElementComment()) {
			iOwlConstructWriter.createElementComment(comment);
			elementParseStatus.setElementComment(false);
			elementParseStatus.setElementDataElement(false);
		} else if (speciesTransportParseStatus.isTransport()) {
			iOwlConstructWriter.createTransportCommentInOntology(comment);
			speciesTransportParseStatus.setComment(false);
		} else if (ctmlCommentParseStatus.isComment()) {
			iOwlConstructWriter.createMechanismComment(comment);
			ctmlCommentParseStatus.setComment(false);
		} else if (phaseParseStatus.isPhase()) {
			phaseMD.setComment(comment);
		}
	}
	
	/**
	 * Detects the termination of an equation and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endEquation(String qName){
		// Deals with the equation that might not be read completely using 
		// the regular element value retrieval operation.
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionEquation())) {
			String equation = multiLineValue.toString();
			reaction.setEquation(equation);
			iOwlConstructWriter.writeEquation(equation);
			reactionParseStatus.setEquation(false);
		}
	}
	
	/**
	 * Detects the termination of the products of a reaction and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endProducts(String qName){
		// Deals with the products of a reaction that might not be read 
		// completely using the regular element value retrieval operation.
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionProducts())) {
			String products = multiLineValue.toString();
			try {
				iOwlConstructWriter.addProducts(basePath, products);
			} catch (OntoException e) {
				logger.error("Products of a reaction could not be created.");
			}
			reactionParseStatus.setProducts(false);
		}	
	}

	/**
	 * Detects the termination of the reactants of a reaction and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endReactants(String qName){
		// Deals with the products of a reaction that might not be read 
		// completely using the regular element value retrieval operation.
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionReactants())) {
			String reactants = multiLineValue.toString();
			try {
				iOwlConstructWriter.addReactants(basePath, reactants);
			} catch (OntoException e) {
				logger.error("Reactants of a reaction could not be created.");
			}
			reactionParseStatus.setReactants(false);
		}	
	}
	
	/**
	 * Detects the termination of the atom array of a species and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endAtomArray(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpeciesAtomArray())) {
			String atomArray = multiLineValue.toString();
			try {
				iOwlConstructWriter.addAtomArray(basePath, atomArray);
			} catch (OntoException e) {
				logger.error("Atom array of a species could not be created.");
			}
			speciesParseStatus.setSpeciesAtomArray(false);
		}
	}
	
	/**
	 * Detects the termination of the species efficienies of a reaction and processes
	 * it to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void endEfficiencies(String qName) {
		// Deals with the species efficiencies in a reaction that might not be read 
		// completely using the regular element value retrieval operation.
		if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffEfficiencies())) {
			String efficiencies = multiLineValue.toString();
			try {
				writeSpeciesEfficiencies(CtmlConverterUtils.createKeyValuePair(efficiencies));
			} catch (OntoException e) {
				logger.error("The efficiencies are not in the right "
						+ "format(i.e. key1:value1 key2:value2).");
			}
			efficiencyParseStatus.setEfficiencies(false);
		}
	}

	/**
	 * Codifies in OWL the species which has the thirdbody efficiency in </br>
	 * a pressure dependent reaction. Also codifies the value of efficiency.
	 * 
	 * @param hashMap
	 */
	private void writeSpeciesEfficiencies(LinkedHashMap<String, String> hashMap) {
		for (String species : hashMap.keySet()) {
			// Creates an instance of third body efficiency once it finds the
			// appearance of the efficiencies element within a reaction.
			writeEfficiencyInstance();
			try {
				iOwlConstructWriter.addSpeciesEfficiency(
						species.concat(UNDERSCORE).concat(appConfigCtml.
								getGasSpeciesDataId()), hashMap.get(species));
			} catch (OntoException e) {
				logger.error("The reaction efficiency for a species could " 
			+ "not be created.");
			}
		}
	}

	/**
	 * Creates an instance of third body efficiency of species in pressure 
	 * dependent reactions.
	 */
	private void writeEfficiencyInstance() {
		try {
			iOwlConstructWriter.addEfficiencyInstance(basePath);
		} catch (OntoException e) {
			logger.error("An OWL ontology instance for third body efficiency "
					+ " of species could not be created.");
		}
	}
	
	/**
	 * Parses species array to codify in the OWL ontology being created.
	 * 
	 * @param qName
	 */
	private void parseSpeciesArray(String qName){
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhaseSpeciesArray())) {
			speciesArray.setValue(multiLineValue.toString());
			speciesArrayParseStatus.setSpeciesArray(false);
		}
	}
	
	/**
	 * Parses NASA Polynomial Coefficients to codify this in the OWL 
	 * ontology being created.
	 * 
	 * @param qName
	 */
	private void parseNasaPolynomialCoefficients(String qName){
		// Parses NASA Polynomial Coefficients.
		if (speciesThermoParseStatus.isSpeciesThermo()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getCtmlNasaPolCoeffArray())) {
				String nasaPolyCoeffs = multiLineValue.toString();
				createNasaPCoeffsInOntology(nasaPolyCoeffs);
				coeffArrayParseStatus.setFloatArray(false);
			}
		}
	}
	
	/**
	 * Parses CHEB rate coefficient parameters to encode them in the OWL
	 * ontology being created.
	 * 
	 * @param qName
	 */
	private void parseChebyshevRateCoefficientParameters(String qName){
		// Parses Chebyshev Rate Coefficient parameters.
		if (rateCoeffParseStatus.isRateCoeff()) {
			if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffFloatArray())) {
				String chebRateCoeffs = multiLineValue.toString();
				createChebyshevRateCoeffs(chebRateCoeffs);
				rateCoeffParseStatus.setFloatArray(false);
			}
		}
	}
	/**
	 * In CTML, there are some elements which do not always appear with the
	 * same set of sub-elements. Therefore, the end of parsing of a specific
	 * sub-element cannot be used to determine the end of parsing of such
	 * elements. For them, endElement trigger is used to detect the end of
	 * parsing precisely.
	 * 
	 * @param qName
	 */
	private void markEnd(String qName){
		// Sets the flags to false to mark the fact that element 
		// data have been parsed.
		markEndElement(qName);
		// Sets the flags to false to mark the fact that a species 
		// and its thermo data have been parsed.
		markEndSpeciesThermo(qName);
		// Sets a flag to false to mark the fact that a phase 
		// has been parsed.
		markEndPhase(qName);
		// Sets a flag to false to mark the fact that the transport
		// properties of a phase has been parsed.
		markEndTransportProperties(qName);
		// Sets the flags to false to mark the fact that a reaction 
		// and its reaction have been parsed.
		markEndReactionNOrder(qName);
		// Sets the flags to false to mark the fact that the rate coefficients
		// of the types Arrhenius, Coverage and Landau-Teller have been parsed.
		markEndRateCoefficients(qName);
	}

	/**
	 * Marks the end of parsing of the current element data.
	 * 
	 * @param qName
	 */
	private void markEndElement(String qName){
		// Marks the end of the current element parsing. 
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlElementData())) {
			elementParseStatus.setElementDataElement(false);
			elementDataParseStatus.setElementData(false);
		}
	}
	
	/**
	 * Marks the end of parsing of the following two elements:
	 * </br>
	 * 1. Species thermo data.
	 * </br>
	 * 2. Species.
	 * 
	 * @param qName
	 */
	private void markEndSpeciesThermo(String qName){
		// Sets a flag to false to mark the fact that thermo data
		// of a species has been parsed.
		markEndSpeciesThermoData(qName);
		// Sets a flag to false to mark the fact that a species 
		// has been parsed.
		markEndSpecies(qName);
	}
	
	/**
	 * Marks the end of parsing of the following two elements:
	 * </br>
	 * 1. Reaction
	 * </br>
	 * 2. Reaction order.
	 * 
	 * @param qName
	 */
	private void markEndReactionNOrder(String qName){
		// Sets a flag to false to mark the fact that a reaction belonging
		// to a gas or material has been parsed.
		markEndReaction(qName);
		// Sets a flag to false to mark the fact that a reaction order
		// relevant for a species has been parsed.
		markEndReactionOrder(qName);
	}
	
	/**
	 * Marks the end of parsing of the elements representing the following
	 * three rate coefficients:
	 * </br>
	 * 1. Arrhenius rate coefficients.
	 * </br>
	 * 2. Coverage rate coefficients.
	 * </br>
	 * 3. Landau-Teller rate ceofficients.
	 * 
	 * @param qName
	 */
	private void markEndRateCoefficients(String qName){
		// Sets a flag to false to mark the fact that the Arrhenius rate
		// coefficients for a reaction have already been parsed.
		markEndArrheniusRateCoefficients(qName);
		// Sets a flag to false to mark the fact that the Arrhenius coverage
		// rate coefficients for a reaction have already been parsed.		
		markEndCoverageRateCoefficients(qName);
		// Sets a flag to false to mark the fact that the Landau-Teller
		// rate coefficients for a reaction have already been parsed.				
		markEndLandauTellerRateCeofficients(qName);
	}
	
	/**
	 * Marks the end of parsing of thermo data specific to a species.
	 * 
	 * @param qName
	 */
	private void markEndSpeciesThermoData(String qName){
		// Marks the end of the thermo data parsing for a species. 
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesThermo())) {
			if(speciesThermoParseStatus.isSpeciesThermo()){
				speciesThermoParseStatus.setSpeciesThermo(false);
			}
		}
	}
	
	/**
	 * Marks the end of parsing of a species.
	 * 
	 * @param qName
	 */
	private void markEndSpecies(String qName){
		// Marks the end of parsing of a species.
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesDataSpecies())) {
			speciesParseStatus.setSpeciesDataSpecies(false);
		}
	}
	
	/**
	 * Marks the end of parsing of a phase.
	 * 
	 * @param qName
	 */
	private void markEndPhase(String qName){
		// Marks the end of parsing of a phase. 
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlPhase())) {
			try{
			iOwlConstructWriter.addPhase(basePath);
			}catch(OntoException e){
				e.printStackTrace();
				logger.error("A phase could not be created.");
			}
			CtmlConverterState.createdPhase = false;
			phaseParseStatus.setPhase(false);
		}
	}
	
	/**
	 * Marks the end of parsing of transport properties.
	 * 
	 * @param qName
	 */
	private void markEndTransportProperties(String qName){
		// Marks the end of parsing of transport properties of a species.
		if (qName.equalsIgnoreCase(appConfigCtml.getCtmlSpeciesTransport())) {
			if(speciesParseStatus.isSpeciesDataSpecies()){
				idTransportParameter++;
				speciesTransportParseStatus.setTransport(false);
			}
		}		
	}
	
	/**
	 * Marks the end of parsing of a gas phase or material reaction.
	 * 
	 * @param qName
	 */
	private void markEndReaction(String qName){
		// Marks the end of parsing of a reaction.
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionDataReaction())) {
			CtmlConverterState.createdReaction = false;
			reactionParseStatus.setReaction(false);
		}
	}
	
	/**
	 * Marks the end of parsing of the reaction order for a species.
	 * 
	 * @param qName
	 */
	private void markEndReactionOrder(String qName){
		// Marks the end of parsing of a reaction order.
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionOrder())) {
			CtmlConverterState.createdReactionOrder = false;
			reactionOrderParseStatus.setOrder(false);
		}
	}
	
	/**
	 * Marks the end of parsing of Arrhenius rate coefficients.
	 * 
	 * @param qName
	 */
	private void markEndArrheniusRateCoefficients(String qName){
		// Marks the end of parsing of the Tmin parameter 
		// in the CHEB parameterisation.
		if (qName.equalsIgnoreCase(appConfigCtml.getReactionRateCoeffArrhenius())) {
			CtmlConverterState.createdArrhenius = false;
			arrheniusParseStatus.setArrhenius(false);
		}
	}
	
	/**
	 * Marks the end of parsing of Arrhenius Coverage rate coefficients.
	 * 
	 * @param qName
	 */
	private void markEndCoverageRateCoefficients(String qName){
		// Marks the end of parsing of the coverage parameters. 
		if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffArrheniusCoverage())) {
			CtmlConverterState.createdCoverageDependency = false;
			coverageParseStatus.setCoverage(false);
		}
	}
	
	/**
	 * Marks the end of parsing of Ladau-Teller rate coefficients.
	 * 
	 * @param qName
	 */
	private void markEndLandauTellerRateCeofficients(String qName){
		// Marks the end of parsing of the Landau-Teller 
		// rate coefficients. 
		if (qName.equalsIgnoreCase(appConfigCtml.getRateCoeffLandauTeller())) {
			CtmlConverterState.createdLandauTeller = false;
			landauTellerParseStatus.setLandauTeller(false);
		}
	}
	
	/**
	 * Calls the methods that forward the calls to the following parsers:</p>
	 * - CTML metadata</br>
	 * - Phase</br>
	 * - Element</br>
	 * - Species</br>
	 * - Reaction</br
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void convert(String qName, Attributes attributes){
		// Calls the method that forwards the call to
		// the metadata parser.
		iMetaDataConverter.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the phase parser.
		iPhaseConverter.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the element parser.
		iElementConverter.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the species parser.
		iSpeciesConverter.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the species parser.
		iReactionConverter.parse(qName, attributes);
	}
	
	/**
	 * Forwards the call to the method that creates the NASA
	 * Polynomial coefficients of a species in the
	 * ontology being created.
	 * 
	 * @param nasaPCoeffs
	 */
	private void createNasaPCoeffsInOntology(String nasaPCoeffs) {
		iOwlConstructWriter.createNasaPCoeffsInOntology(nasaPCoeffs);
	}

	/**
	 * Forwards the call to the method that creates the
	 * Chebyshev Rate Coefficients of a reaction species in the
	 * ontology being created.
	 * 
	 * @param nasaPCoeffs
	 */
	private void createChebyshevRateCoeffs(String chebRateCoeffs) {
		iOwlConstructWriter.createChebyshevRateCoeffs(chebRateCoeffs);
	}
	
	/**
	 * Saves the list of OntoKin ABox OWL files. 
	 * 
	 * @param owlFilePath
	 * @param aBoxOwlFileURL
	 */
	private void saveOntoKinABox(String aBoxOwlFilePath, String aBoxOwlFileURL) {
		try {
			if (listOfOntoKinAboxes == null) {
				listOfOntoKinAboxes = new BufferedWriter(new OutputStreamWriter(
						new FileOutputStream(aBoxOwlFilePath.concat("/").concat(
								ontoChemKB.getOntoKinKbRootDirectory().concat(ontoChemKB.getOntoKinKbAboxFileName()))),
						"UTF8"));
			}
			listOfOntoKinAboxes.write(aBoxOwlFileURL.concat("\n"));
		    if(managerKB == null){
		    	managerKB = OWLManager.createOWLOntologyManager();
		    }
			if(ontologyKBIRI == null){
		    	ontologyKBIRI = IRI.create(ontoChemKB.getOntoKinKbURL().concat(ontoChemKB.getOntoChemKBFileName()));
		    }
		    if(ontologyKB == null){
		    	ontologyKB = managerKB.createOntology(ontologyKBIRI);
		    }
		    if(factoryKB == null){
		    	factoryKB = managerKB.getOWLDataFactory();
		    }
			// Adds the import clause to the OntoChem ABox
			OWLImportsDeclaration importDeclarationABox = factoryKB.getOWLImportsDeclaration(IRI.create(ontoChemKB
					.getOntoKinKbURL().concat(CtmlConverterUtils.extractMechanismName(mechanismName)).concat(".owl")));
			managerKB.applyChange(new AddImport(ontologyKB, importDeclarationABox));
			
		} catch (IOException e) {
			logger.error("File containing a list of OntoKin ABox OWL ontology could not be created.");
			e.printStackTrace();
		} catch(OWLOntologyCreationException e){
			logger.error("OWLOntologyCreationException occurred.");
		} catch(OntoException e){
			logger.error("OntoException occurred.");
		}
	}
}