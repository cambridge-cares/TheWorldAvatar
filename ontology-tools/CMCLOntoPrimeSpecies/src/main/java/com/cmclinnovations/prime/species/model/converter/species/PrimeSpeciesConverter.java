package com.cmclinnovations.prime.species.model.converter.species;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.output.FileWriterWithEncoding;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLImportsDeclaration;
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

import com.cmclinnovations.ontology.model.utils.ABoxManagementUtils;
import com.cmclinnovations.prime.species.model.InitPrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.PrimeSpeciesConverterState;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.ChemicalSpecies;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;
import com.cmclinnovations.prime.species.model.utils.PrimeSpeciesConverterUtils;

public class PrimeSpeciesConverter extends PrimeSpeciesConverterState implements IPrimeSpeciesConverter, LexicalHandler, ContentHandler {
	public static StringBuffer multiLineValue = new StringBuffer();
	
	private static Logger logger = org.slf4j.LoggerFactory.getLogger(PrimeSpeciesConverter.class);
	
	OWLOntologyManager managerKB;

	OWLDataFactory factoryKB;

	OWLOntology ontologyKB;

	IRI ontologyKBIRI;
	
	public String tmpValue;
	
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
	 * This method manages all read operations for extracting PrIMe data and meta
	 * data.
	 */
	public void characters(char[] ch, int start, int length) throws SAXException {
		multiLineValue.append(ch, start, length);
		// iZZZWtiter.writer(ch, start, length);
		iChemicalSpeciesWriter.writer(ch, start, length);
		iPreferredKeyWriter.writer(ch, start, length);
		iChemicalIdentifierWriter.writer(ch, start, length);
		iChemicalCompositionWriter.writer(ch, start, length);
		
		
		tmpValue = new String(ch, start, length);
	}
	
	public void endDocument() {
	}
	
	/**
	 * SAXParser calls endElement automatically when it reads a closing tag (or
	 * element) of the tag which is being processed.
	 * 
	 * SAXParser requires a special technique for extracting multiline values which
	 * are codified in between a start tag and an end tag. For example, comments and
	 * species array. To extract such values in their entireties, the
	 * StringBuffer.append() method keeps running until it fins the corresponding
	 * closing tag.
	 * 
	 * In PrIMe, there are some elements which do not always appear with the same
	 * set of sub-elements. Therefore, the end of parsing of a specific sub-element
	 * cannot be used to determine the end of parsing of such elements. For them,
	 * endElement trigger is used to detect the end of parsing precisely.
	 * 
	 */
	
	public void endElement(String uri, String localName, String qName) {
		///////////////////////////////markEnd
		endSomeName(qName);
	}
	
	private void endSomeName(String qName) {
		if (nameParseStatus.isName() && qName.equalsIgnoreCase(primeSpeciesVocabulary.getElemName())) {
			String value = multiLineValue.toString();
			name.setValue(value);
			iChemicalIdentifierWriter.writeName();
		}
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
		// to read multiline values.
		multiLineValue.setLength(0);

		convert(qName, attributes);
	}
	
	public void startPrefixMapping(String prefix, String uri) {
	}

	/**
	 * Converts a PrIMe file into an OWL file.</br>
	 * It also supports the conversion of multiple files.</br>
	 * The conversion process takes a PrIMe file, uses SAX parser to parse</br>
	 * experiment data and result.
	 * 
	 * 
	 * @param primeFiles
	 * @param owlFilePath
	 * @throws OntoChemExpException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> primeFiles, String owlFilesPath, long instanceSerialID) throws OntoPrimeSpeciesException, OWLOntologyCreationException {
		// Calls the method that checks if the user provided path parameters
		// are valid.
		checkPathParameters(primeFiles, owlFilesPath);
		// Each iteration parses a PrIMe file to extract the values
		// of experiment elements and properties.
		for (String primeFile : primeFiles) {
			// Initialises parameters related to ABox generation
			init(primeFile, owlFilesPath, instanceSerialID);
			PrimeSpeciesConverter primeSpeciesConverter = new PrimeSpeciesConverter();
			XMLReader parser = null;
			parser = createXMLReader(parser);
			if (parser == null) {
				throw new OntoPrimeSpeciesException("Parser could not be initialised with SAXParser.");
			}
			
			parser = setParserProperty(parser, primeSpeciesConverter);
			if (parser == null) {
				throw new OntoPrimeSpeciesException("Parser lexical property could not be set.");
			}
			
			parser.setContentHandler(primeSpeciesConverter);
			// Before it starts parsing, checks if the file exists.
			File f = new File(primeFile);
			if (f.exists()) {
			} else {
				logger.info("There is no experiment file in the following path:" + primeFile);
			}
			
			try {
				InputSource is = new InputSource(primeFile);
				parser.parse(is);
				iABoxManagement.saveOntology(basePathTBox);
				String ontologyFileName = owlFilesPath.concat("\\").concat("kb").concat("\\")
						.concat(generateOwlFileName(primeFile));
				removeCommentedOut(ontologyFileName);
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
			// Saves the generated ABox OWL file and the conversion report.
			saveOntoKinABox(primeFile, owlFilesPath, PrimeSpeciesConverterUtils.extractChemicalSpeciesName(primeFile).concat(opCtrl.getOwlFileExtension()));
		}
	}
	
	/**
	 * Produces the name of the current OWL file from its path.
	 * 
	 * @param owlFilesPath
	 * @return
	 */
	private String generateOwlFileName(String primeFilePathPlusName) {
		if (primeFilePathPlusName.contains("\\")) {
			return primeFilePathPlusName.substring(primeFilePathPlusName.lastIndexOf("\\") + 1).replace(".xml", ".owl");
		}
		return primeFilePathPlusName;
	}
	
	/**
	 * Removes commented out TBox elements from the generated ABox ontology.
	 * 
	 * @param ontologyFileName
	 */
	private void removeCommentedOut(String ontologyFileName) {
		try {
			String owlFileNameTemp = ontologyFileName.replace("file:/", "").replace(".owl", "_temp.owl");
			String owlFileNameOriginal = ontologyFileName.replace("file:/", "");
			BufferedReader br = ABoxManagementUtils.openSourceFile(owlFileNameOriginal);
			BufferedWriter bw = new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(owlFileNameTemp), "UTF-8"));
			String line;
			while ((line = br.readLine()) != null) {
				if ((line.trim().startsWith("<!--")) || (line.trim().endsWith("-->"))
						|| (line.trim().startsWith("//"))) {
				} else {
					bw.write(line.concat("\n"));
				}
			}
			bw.close();
			br.close();
			delete(owlFileNameOriginal, owlFileNameTemp);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Deletes the original file and renames the temporary file to the original file
	 * name.
	 * 
	 * @param owlFileNameOriginal
	 * @param owlFileNameTemp
	 */
	private void delete(String owlFileNameOriginal, String owlFileNameTemp) {
		File fileOriginal = new File(owlFileNameOriginal);
		if (fileOriginal.delete()) {
			fileOriginal = new File(owlFileNameOriginal);
			File fileTemp = new File(owlFileNameTemp);
			if (fileTemp.renameTo(fileOriginal)) {
			} else {
				logger.error("The temporary ABox ontology file could not be renamed.");
			}
		} else {
			logger.error("The generated original ABox ontology file could not be deleted.");
		}
	}
	
	/**
	 * Creates an XML reader using SAXParser.
	 * 
	 * @param parser
	 */
	private XMLReader createXMLReader(XMLReader parser) {
		try {
			if (parser == null) {
				parser = XMLReaderFactory.createXMLReader();
			}
		} catch (SAXException ex1) {
			try {
				parser = XMLReaderFactory.createXMLReader("org.apache.xerces.parsers.SAXParser");
			} catch (SAXException ex2) {
				return null;
			}
		}
		return parser;
	}
	
	/**
	 * Sets the lexical handler property of the parser.
	 * 
	 * @param parser
	 * @param primeConverter
	 * @return
	 */
	private XMLReader setParserProperty(XMLReader parser, PrimeSpeciesConverter primeSpeciesConverter) {
		try {
			parser.setProperty("http://xml.org/sax/properties/lexical-handler", primeSpeciesConverter);
		} catch (SAXNotRecognizedException e) {
			return null;
		} catch (SAXNotSupportedException e) {
			return null;
		}
		return parser;
	}
	
	/**
	 * Initialise the instances of the classes that hold PrIMe metadata and data as
	 * well as parse status information thereof. Also initialises the instances of
	 * the classes that read configuration parameters using Spring framework
	 * annotations.
	 * 
	 * @param primeFile
	 * @param owlFilesPath
	 * @throws OntoChemExpException
	 * @throws OWLOntologyCreationException
	 */
	private void init(String primeFile, String owlFilesPath, long instanceSerialID) throws OntoPrimeSpeciesException, OWLOntologyCreationException {
		// Initialises the ontology parameteres.
		init();
		initPrimeSpeciesConverter = new InitPrimeSpeciesConverter();
		initPrimeSpeciesConverter.init(instanceSerialID);
		// Extracts the name of the experiment.
		chemicalSpeciesName = PrimeSpeciesConverterUtils.extractChemicalSpeciesName(primeFile);
		// Replaces any space in the user shown OWL file path with %20
		// to form a valid URL.
		String owlFilePath = PrimeSpeciesConverterUtils.formOwlUrl(primeFile, owlFilesPath);
		if (owlFilePath.contains(" ")) {
			owlFilePath = owlFilePath.replaceAll(" ", "%20");
		}
		basePathTBox = ontoPrimeSpeciesKB.getOntoSpeciesKbTBoxIri();
		// Creates an IRI for the OWL ontology that is
		// being created to codify an experiment
		ontologyIRI = IRI.create(owlFilePath);
		if (ontologyIRI == null) {
			logger.error("An IRI for an ontology could not be created.");
			throw new OntoPrimeSpeciesException("An IRI for an ontology could not be created.");
		}
		kbIRI = IRI.create(ontoPrimeSpeciesKB.getOntoSpeciesKbURL().concat(ontoPrimeSpeciesKB.getOntoSpeciesOntolgyFileName()));
		// Replaces any space in the user shown OWL file path with %20
		// to form a valid URL.
		String owlFilePathForFileSave = PrimeSpeciesConverterUtils.formOwlFileSaveUrl(primeFile, owlFilesPath);
		if (owlFilePathForFileSave.contains(" ")) {
			owlFilePathForFileSave = owlFilePathForFileSave.replaceAll(" ", "%20");
		}
		// Creates an IRI to save the OWL ontology on the file system
		ontologyIRIFileSave = IRI.create(owlFilePathForFileSave);
		basePathABox = ontoPrimeSpeciesKB.getOntoSpeciesKbURL().concat(chemicalSpeciesName).concat(opCtrl.getOwlFileExtension());
		ontology = manager.createOntology(IRI.create(basePathABox));
		if (ontology == null) {
			logger.error("The requested ontology could not be created.");
			throw new OntoPrimeSpeciesException("Ontology could not be created.");
		}
		if (kb == null) {
			kb = manager.createOntology(kbIRI);
		}
		// Checks if the experiment name or PrIMe file name exists
		if (chemicalSpeciesName == null) {
			logger.error("PrIMe source files are empty.");
			throw new OntoPrimeSpeciesException("PrIMe source files are empty.");
		}
	}
	
	/**
	 * Initialises the following parameters: 1. ontology 2. ontologyIRI 3.
	 * ontologyIRI for saving the being generated ABox OWL file 4. base URL of TBox
	 * 5. base URL of ABox
	 */
	public void init() {
		ontology = null;
		ontologyIRI = null;
		ontologyIRIFileSave = null;
		basePathTBox = EMPTY;
		basePathABox = EMPTY;
	}
	
	/**
	 * Checks the validity of the path parameters.
	 * 
	 * @param primeFiles
	 * @param owlFilesPath
	 * @throws OntoChemExpException
	 */
	private void checkPathParameters(ArrayList<String> primeFiles, String owlFilesPath) throws OntoPrimeSpeciesException {
		// Checks if the list that contains the path to the input PrIMe files
		// exists
		if (primeFiles == null) {
			logger.error("PrIMe source files are empty.");
			throw new OntoPrimeSpeciesException("PrIMe source files are empty.");
		}
		// Checks if the user shown path to save the OWL file is valid
		if (owlFilesPath == null) {
			logger.error("The ontology file path is null.");
			throw new OntoPrimeSpeciesException("The ontology file path is null.");
		}
	}
	
	/**
	 * Calls the methods that forward the calls to the following PrIMe parsers:
	 * </p>
	 * - Experiment</br>
	 * - Apparatus</br>
	 * 
	 * @param qName
	 * @param attributes
	 */
	private void convert(String qName, Attributes attributes) {
		// Calls the method that forwards the call to
		// the experiment parser class.
		iChemicalSpeciesConverter.parse(qName, attributes);
		iPreferredKeyConverter.parse(qName, attributes);
		iChemicalIdentifierConverter.parse(qName, attributes);
		iChemicalCompositionConverter.parse(qName, attributes);
		
	}
	
	/**
	 * Saves the generated ABox OWL file and the conversion report describing the
	 * difference between the source file and the generated one.
	 * 
	 * @param primeFile
	 * @param aBoxOwlFilePath
	 * @param aBoxOwlFileURL
	 */
	private void saveOntoKinABox(String primeFile, String aBoxOwlFilePath, String aBoxOwlFileURL) {
		System.out.println("aBoxOwlFilePath:" + aBoxOwlFilePath);
		logger.info("aBoxOwlFilePath:" + aBoxOwlFilePath);
		try {
			if (managerKB == null) {
				managerKB = OWLManager.createOWLOntologyManager();
			}
			if (ontologyKBIRI == null) {
				ontologyKBIRI = IRI
						.create(ontoPrimeSpeciesKB.getOntoSpeciesKbURL().concat(ontoPrimeSpeciesKB.getOntoSpeciesKBFileName()));
			}
			if (ontologyKB == null) {
				ontologyKB = managerKB.createOntology(ontologyKBIRI);
			}
			if (factoryKB == null) {
				factoryKB = managerKB.getOWLDataFactory();
			}
			// Adds the import clause to the OntoChem ABox
			OWLImportsDeclaration importDeclarationABox = factoryKB
					.getOWLImportsDeclaration(IRI.create(ontoPrimeSpeciesKB.getOntoSpeciesKbURL()
							.concat(PrimeSpeciesConverterUtils.extractChemicalSpeciesName(chemicalSpeciesName)).concat(".owl")));
			managerKB.applyChange(new AddImport(ontologyKB, importDeclarationABox));
			// Produces the absolute file path including the file name of
			// the current conversion report
			String reportFilePath = aBoxOwlFilePath.concat(BACKSLASH).concat(chemicalSpeciesName).concat(HYPHEN)
					.concat(opCtrl.getConversionReportName());
			// Produces the absolute path including the file name of the
			// generated temporary file
			String generatedTempFilePath = aBoxOwlFilePath.concat(FRONTSLASH).concat(chemicalSpeciesName)
					.concat(opCtrl.getDataStructureSavingFileName());
			// Saves in-memory experiment data and metadata extracted from a
			// PrIMe source file and stored in the PrIMe data structure into a
			// temporary file to check the completeness of the conversion.
			saveInMemorySourceContent(generatedTempFilePath);
			// The following method reports the incompleteness of the conversion
			iCompleteness.reportDifference(reportFilePath, primeFile, generatedTempFilePath);
		} catch (OWLOntologyCreationException e) {
			logger.error("OWLOntologyCreationException occurred.");
		} catch (OntoPrimeSpeciesException e) {
			logger.error("OntoException occurred.");
		}
	}

	/**
	 * Saves in-memory data and metadata of the PrIMe experiment file that is being
	 * converted into the disk as an XML file.
	 * 
	 * @param filePath
	 * @throws OntoChemExpException
	 */
	private void saveInMemorySourceContent(String filePath) throws OntoPrimeSpeciesException {
		try {
			FileWriterWithEncoding file = new FileWriterWithEncoding(filePath, "UTF-8");
			JAXBContext jaxbContext = JAXBContext.newInstance(ChemicalSpecies.class);
			Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			jaxbMarshaller.marshal(chemicalSpecies, file);
			file.close();
		} catch (JAXBException e) {
			logger.error("JAXBException occurred.");
			e.printStackTrace();
		} catch (IOException e) {
			logger.error("IOException occurred.");
			e.printStackTrace();
		}
	}
	
}
