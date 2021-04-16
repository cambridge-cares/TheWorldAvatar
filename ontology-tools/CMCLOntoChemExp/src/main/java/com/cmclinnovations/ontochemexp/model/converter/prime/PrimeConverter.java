package com.cmclinnovations.ontochemexp.model.converter.prime;

import static com.cmclinnovations.ontochemexp.model.owl.IDataGroupWriter.logger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

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

import com.cmclinnovations.ontochemexp.model.PrimeConverterState;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.Experiment;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.ApparatusProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Kind;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Mode;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.BibliographyLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.common_properties.CommonProperties;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroup;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupDataPoint;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupPropertyComponent;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.DataGroupDataPointX;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X1;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.X2;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.SpeciesLink;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.InitPrimeConverter;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.utils.PrimeConverterUtils;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.ontology.model.utils.ABoxManagementUtils;

import uk.ac.cam.cares.jps.base.query.SparqlOverHttpService.RDFStoreType;
import uk.ac.cam.cares.jps.blazegraph.KnowledgeRepository;

import org.apache.commons.lang3.text.WordUtils;

/**
 * A converter that can convert a set of PrIMe files, each containing a chemical
 * experiment, into OWL files. When it receives a request for conversion, in
 * each iteration, it parses a PrIMe file to extract experiment data and
 * results. Following the extraction and representation of a chemical experiment
 * in OWL, it saves the OWL content as a file to the disk.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class PrimeConverter extends PrimeConverterState implements IPrimeConverter, LexicalHandler, ContentHandler {

	// Stores a multiline value of an element.
	public static StringBuffer multiLineValue = new StringBuffer();

	private static Logger logger = org.slf4j.LoggerFactory.getLogger(PrimeConverter.class);

	OWLOntologyManager managerKB;

	OWLDataFactory factoryKB;

	OWLOntology ontologyKB;

	IRI ontologyKBIRI;

	//////////////////////////
	public String tmpValue;
	String saveValue;
	String saveValueForApparatus;
	String subClassname;
	String Unit;
	String apparatusDescription;
	String apparatusName;
	//////////////////////////

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
		// Reads multiline values
		multiLineValue.append(ch, start, length);
		// Calls the method that forwards the call to
		// the experiment writer class.
		iExperimentWriter.writer(ch, start, length);
		// Calls the method that forwards the call to
		// the apparatus writer class.
		iApparatusWriter.writer(ch, start, length);
//		iAdditionalDataItemWriter.writer(ch, start, length);
//		iBibliographyLinkWriter.writer(ch, start, length);
		iCommonPropertiesWriter.writer(ch, start, length);
		iCopyrightWriter.writer(ch, start, length);
		iDataGroupWriter.writer(ch, start, length);
		iPreferredKeyWriter.writer(ch, start, length);

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
		try {
			iAdditionalDataItemWriter.writer(qName);
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			iBibliographyLinkWriter.writer(qName);
		} catch (SAXException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		endSomeAdditionalDataItem(qName);
		endSomeBibliographyLink(qName);
		endSomeCopyright(qName);
		endSomeDataGroup(qName);
		endSomePreferredKey(qName);

		markEnd(qName);
	}

	private void endSomeAdditionalDataItem(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAdditionalDataItem())) {
			if (additionalDataItemParseStatus.isAdditionalDataItem()) {
				String value = multiLineValue.toString();
				additionalDataItem.setValue(value);

				iAdditionalDataItemWriter.writeValue();
				iAdditionalDataItemWriter.setUP();
			}
		}
	}

	private void endSomeBibliographyLink(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemBibliographyLink())) {
			if (bibliographyLinkParseStatus.isBibliographyLink()) {
				String value = multiLineValue.toString();
				bibliographyLink.setValue(value);

				iBibliographyLinkWriter.writeValue();
				iBibliographyLinkWriter.setUP();
				bibliographyLinkParseStatus.setBibliographyLink(false);
			}
		}
	}

	private void endSomeCopyright(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemCopyright())) {
			if (copyrightParseStatus.isCopyright()) {
				String value = multiLineValue.toString();
				copyright.setValue(value);

				iCopyrightWriter.writeValue();
				iCopyrightWriter.setUP();
			}
		}
	}

	private void endSomeDataGroup(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty())) {
			if (dataGroupPropertyUncertaintyParseStatus.isUncertainty()) {
				String value = multiLineValue.toString();
				dataGroupPropertyUncertainty.setUncertaintyValue(value);

				iDataGroupWriter.writeUncertaintyValue();
				iDataGroupWriter.setUPUncertainty();
			}
		}
		
		if (qName.equalsIgnoreCase(primeVocabulary.getElemComponent())) {
			if (dataGroupPropertyComponentParseStatus.isComponent() && inDataGroup && dataGroupPropertyComponentParseStatus.hasComponentValue()) {
				List<Object> items = new ArrayList<Object>();
				String value = multiLineValue.toString().trim();
				if (value != null && !value.isEmpty()) {
					items.add(value);
				}
				
				if (value != null && !value.trim().isEmpty()) {
					try {
						iABoxManagement.addProperty(
								"Component" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
								ontoChemExpVocabulary.getDataPropertyhasValue(), dataGroupPropertyComponent.getItems().get(0).toString().trim(),
								STRING);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
//					IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//					iABoxManagement.addProperty("Component"+UNDERSCORE+(dataGroupID+dataGroupCount)+UNDERSCORE+dataGroupPropertyCount, 
//							dataPropertyIRI, dataGroupPropertyComponent.getComponentValue(), STRING);
				}
				
				dataGroupPropertyComponentParseStatus.setComponent(false);
				dataGroupProperty.setPropertyComponent(dataGroupPropertyComponent);
				dataGroupPropertyComponent = new DataGroupPropertyComponent();
			}
		}
		
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink())) {
			if (dataGroupPropertyComponentParseStatus.isComponentSpeciesLink() && dataGroupPropertyComponentParseStatus.isComponent() && inDataGroup) {
				String value = multiLineValue.toString().trim();
				dataGroupPropertyComponentSpeciesLink.setSpeciesLinkValue(value);
				
				if (dataGroupPropertyComponentSpeciesLink.getSpeciesLinkValue() != null 
						&& !dataGroupPropertyComponentSpeciesLink.getSpeciesLinkValue().trim().isEmpty()) {
					try {
						iABoxManagement.addProperty(
								"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
								ontoChemExpVocabulary.getDataPropertyhasValue(),
								dataGroupPropertyComponentSpeciesLink.getSpeciesLinkValue(), STRING);
					} catch (ABoxManagementException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				
				dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(false);
				// maybe add later: set it to higher level
				dataGroupPropertyComponentSpeciesLink = new SpeciesLink();
			}
		}
		
		if (qName.equalsIgnoreCase(primeVocabulary.getElemUncertainty()) 
				&& xUncertaintyParseStatus.isUncertainty() 
				&& (x1ParseStatus.isX() || x2ParseStatus.isX() || x3ParseStatus.isX() || x4ParseStatus.isX()
						|| x5ParseStatus.isX() || x6ParseStatus.isX() || x7ParseStatus.isX() || x8ParseStatus.isX()
						|| x9ParseStatus.isX() || x10ParseStatus.isX() || x11ParseStatus.isX())) {
			String value = multiLineValue.toString();
			xUncertainty.setUncertaintyValue(value);
			
			try {
				iDataGroupWriter.readDataGroupDataPointXUncertainty(qName);
			} catch (SAXException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
			
			try {
				iABoxManagement.addProperty(
						"Uncertainty" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupDataPointCount
						+ UNDERSCORE + xUncertaintyCount,
						ontoChemExpVocabulary.getDataPropertyhasValue(), xUncertainty.getUncertaintyValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			xUncertaintyParseStatus.setUncertainty(false);
			// set xUncertainty to x
			xUncertainty = new Uncertainty();
		}

		if (qName.toLowerCase().equalsIgnoreCase("x1")) {
			if (x1ParseStatus.isX() && !x1ParseStatus.isParsed()) {
				iDataGroupWriter.writeX1ToOwl();
			}
		}

		if (qName.toLowerCase().equalsIgnoreCase("x2")) {
			if (x2ParseStatus.isX() && !x2ParseStatus.isParsed()) {
				iDataGroupWriter.writeX2ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x3")) {
			if (x3ParseStatus.isX() && !x3ParseStatus.isParsed()) {
				iDataGroupWriter.writeX3ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x4")) {
			if (x4ParseStatus.isX() && !x4ParseStatus.isParsed()) {
				iDataGroupWriter.writeX4ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x5")) {
			if (x5ParseStatus.isX() && !x5ParseStatus.isParsed()) {
				iDataGroupWriter.writeX5ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x6")) {
			if (x6ParseStatus.isX() && !x6ParseStatus.isParsed()) {
				iDataGroupWriter.writeX6ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x7")) {
			if (x7ParseStatus.isX() && !x7ParseStatus.isParsed()) {
				iDataGroupWriter.writeX7ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x8")) {
			if (x8ParseStatus.isX() && !x8ParseStatus.isParsed()) {
				iDataGroupWriter.writeX8ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x9")) {
			if (x9ParseStatus.isX() && !x9ParseStatus.isParsed()) {
				iDataGroupWriter.writeX9ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x10")) {
			if (x10ParseStatus.isX() && !x10ParseStatus.isParsed()) {
				iDataGroupWriter.writeX10ToOwl();
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x11")) {
			if (x11ParseStatus.isX() && !x11ParseStatus.isParsed()) {
				iDataGroupWriter.writeX11ToOwl();
			}
		}
	}

	private void endSomePreferredKey(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemPreferredKey())) {
			if (preferredKeyParseStatus.isPreferredKey()) {
				String value = multiLineValue.toString();
				preferredKey.setValue(value);

				iPreferredKeyWriter.writeValue();
				iPreferredKeyWriter.setUP();
			}
		}
	}

	private void markEnd(String qName) {
		markEndAdditionalDataItem(qName);
		markEndApparatus(qName);
//		markEndBibliographyLink(qName);
		markEndCommonProperties(qName);
		markEndCopyright(qName);
		markEndDataGroup(qName);
		markEndPreferredKey(qName);
	}

	private void markEndAdditionalDataItem(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemAdditionalDataItem())) {
			if (additionalDataItemParseStatus.isAdditionalDataItem()) {
				additionalDataItemParseStatus.setAdditionalDataItem(false);
			}
		}
	}

	private void markEndApparatus(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemApparatus())) {
			inApparatus = false;
		}
	}

//	private void markEndBibliographyLink(String qName) {
//		if (qName.equalsIgnoreCase(primeVocabulary.getElemBibliographyLink())) {
//			if (bibliographyLinkParseStatus.isBibliographyLink()) {
//				bibliographyLinkParseStatus.setBibliographyLink(false);
//			}
//		}
//	}

	private void markEndCommonProperties(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inCommonProperties) {
			componentCount = 0;
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemCommonProperties())) {
			inCommonProperties = false;
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemComponent()) && inCommonPropertiesPropertyComponent) {
			inCommonPropertiesPropertyComponent = false;
		}
	}

	private void markEndCopyright(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemCopyright())) {
			if (copyrightParseStatus.isCopyright()) {
				copyrightParseStatus.setCopyright(false);
			}
		}
	}

	private void markEndDataGroup(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup()) && inDataGroup) {
			dataGroupPropertyCount = 1000;
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemFeature()) && inDataGroup) {
			indicatorCount = 0;
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup()) && inDataGroup) {
			dataGroupDataPointCount = 10000;
			dataGroupDataPointXCount = 0;
			xUncertaintyCount = 1000;
		}

		if (qName.equalsIgnoreCase(primeVocabulary.getElemDataGroup())) {
			inDataGroup = false;
		}

		if (qName.toLowerCase().equalsIgnoreCase("x1")) {
			if (x1ParseStatus.isX()) {
				x1ParseStatus.setX(false);
			}
		}

		if (qName.toLowerCase().equalsIgnoreCase("x2")) {
			if (x2ParseStatus.isX()) {
				x2ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x3")) {
			if (x3ParseStatus.isX()) {
				x3ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x4")) {
			if (x4ParseStatus.isX()) {
				x4ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x5")) {
			if (x5ParseStatus.isX()) {
				x5ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x6")) {
			if (x6ParseStatus.isX()) {
				x6ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x7")) {
			if (x7ParseStatus.isX()) {
				x7ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x8")) {
			if (x8ParseStatus.isX()) {
				x8ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x9")) {
			if (x9ParseStatus.isX()) {
				x9ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x10")) {
			if (x10ParseStatus.isX()) {
				x10ParseStatus.setX(false);
			}
		}
		if (qName.toLowerCase().equalsIgnoreCase("x11")) {
			if (x11ParseStatus.isX()) {
				x11ParseStatus.setX(false);
			}
		}
	}

	private void markEndPreferredKey(String qName) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemPreferredKey())) {
			if (preferredKeyParseStatus.isPreferredKey()) {
				preferredKeyParseStatus.setPreferredKey(false);
			}
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
		convert(qName, attributes);
		// Prepares the string buffer for reuse. It is used
		// to read multiline values.
		multiLineValue.setLength(0);
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
	public void convert(ArrayList<String> primeFiles, String owlFilesPath, long instanceSerialID)
			throws OntoChemExpException, OWLOntologyCreationException {
		// Calls the method that checks if the user provided path parameters
		// are valid.
		checkPathParameters(primeFiles, owlFilesPath);
		// Each iteration parses a PrIMe file to extract the values
		// of experiment elements and properties.
		for (String primeFile : primeFiles) {
			// Initialises parameters related to ABox generation
			init(primeFile, owlFilesPath, instanceSerialID);
			PrimeConverter primeConverter = new PrimeConverter();
			XMLReader parser = null;
			parser = createXMLReader(parser);
			if (parser == null) {
				throw new OntoChemExpException("Parser could not be initialised with SAXParser");
			}

			parser = setParserProperty(parser, primeConverter);
			if (parser == null) {
				throw new OntoChemExpException("Parser lexical property could not be set");
			}
			parser.setContentHandler(primeConverter);
			// Before it starts parsing, checks if the file exists.
			File f = new File(primeFile);
			if (f.exists()) {
			} else {
				logger.info("There is no experiment file in the following path:" + primeFile);
			}
			try {
				InputSource is = new InputSource(primeFile);
				// is.setEncoding("UTF-8");
				parser.parse(is);
				iABoxManagement.saveOntology(basePathTBox);
				String ontologyFileName = owlFilesPath.concat("\\").concat(ontoChemExpKB.getOntoChemExpKbRootDirectory())
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
			saveOntoKinABox(primeFile, owlFilesPath,
					PrimeConverterUtils.extractExperimentName(primeFile).concat(opCtrl.getOwlFileExtension()));
			if (ontoChemExpKB.getFilesGeneration().equalsIgnoreCase("server")) {
				if (ontoChemExpKB.getUploadTripleStoreServerURL().toLowerCase().contains("blazegraph")) {
					KnowledgeRepository kr = new KnowledgeRepository();
					try {
						kr.uploadOntology(ontoChemExpKB.getUploadTripleStoreServerURL(), 
								ontoChemExpKB.getUploadTripleStoreRepositoryOntoChemExp(), 
								owlFilesPath.concat("\\").concat(ontoChemExpKB.getOntoChemExpKbRootDirectory()).concat(generateOwlFileName(primeFile)));
						logger.info("OWL file uploaded to blazegraph server: " + ontoChemExpKB.getUploadTripleStoreServerURL() + ", under namespece: " + ontoChemExpKB.getUploadTripleStoreRepositoryOntoChemExp());
					} catch (Exception e) {
						e.printStackTrace();
					}
				} else if (ontoChemExpKB.getUploadTripleStoreServerURL().toLowerCase().contains("rdf4j-server")) {
					PrimeConverterUtils.uploadExperiment(generateOwlFileName(primeFile), 
							owlFilesPath.concat("\\").concat(ontoChemExpKB.getOntoChemExpKbRootDirectory()));
					logger.info("OWL file uploaded to rdf4j-server: " + ontoChemExpKB.getUploadTripleStoreServerURL() + ", under namespece: " + ontoChemExpKB.getUploadTripleStoreRepositoryOntoChemExp());
				} else {
					System.out.println("Please provide an endpoint URL for either rdf4j-server or blazegraph server.");
				}
			}
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
	private XMLReader setParserProperty(XMLReader parser, PrimeConverter primeConverter) {
		try {
			parser.setProperty("http://xml.org/sax/properties/lexical-handler", primeConverter);
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
	private void init(String primeFile, String owlFilesPath, long instanceSerialID)
			throws OntoChemExpException, OWLOntologyCreationException {
		// Initialises the ontology parameteres.
		init();
		initPrimeConverter = new InitPrimeConverter();
		initPrimeConverter.init(instanceSerialID);
		// Extracts the name of the experiment.
		experimentName = PrimeConverterUtils.extractExperimentName(primeFile);
		// Replaces any space in the user shown OWL file path with %20
		// to form a valid URL.
		String owlFilePath = PrimeConverterUtils.formOwlUrl(primeFile, owlFilesPath);
		if (owlFilePath.contains(" ")) {
			owlFilePath = owlFilePath.replaceAll(" ", "%20");
		}
		basePathTBox = ontoChemExpKB.getOntoChemExpKbTBoxIri();
		// Creates an IRI for the OWL ontology that is
		// being created to codify an experiment
		ontologyIRI = IRI.create(owlFilePath);
		if (ontologyIRI == null) {
			logger.error("An IRI for an ontology could not be created.");
			throw new OntoChemExpException("An IRI for an ontology could not be created.");
		}
		kbIRI = IRI.create(ontoChemExpKB.getOntoChemExpKbURL().concat(ontoChemExpKB.getOntoChemExpOntolgyFileName()));
		// Replaces any space in the user shown OWL file path with %20
		// to form a valid URL.
		String owlFilePathForFileSave = PrimeConverterUtils.formOwlFileSaveUrl(primeFile, owlFilesPath);
		if (owlFilePathForFileSave.contains(" ")) {
			owlFilePathForFileSave = owlFilePathForFileSave.replaceAll(" ", "%20");
		}
		// Creates an IRI to save the OWL ontology on the file system
		ontologyIRIFileSave = IRI.create(owlFilePathForFileSave);
		basePathABox = ontoChemExpKB.getOntoChemExpKbURL().concat(experimentName).concat(opCtrl.getOwlFileExtension());
		ontology = manager.createOntology(IRI.create(basePathABox));
		if (ontology == null) {
			logger.error("The requested ontology could not be created.");
			throw new OntoChemExpException("Ontology could not be created.");
		}
		if (kb == null) {
			kb = manager.createOntology(kbIRI);
		}
		// Checks if the experiment name or PrIMe file name exists
		if (experimentName == null) {
			logger.error("PrIMe source files are empty.");
			throw new OntoChemExpException("PrIMe source files are empty.");
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
	private void checkPathParameters(ArrayList<String> primeFiles, String owlFilesPath) throws OntoChemExpException {
		// Checks if the list that contains the path to the input PrIMe files
		// exists
		if (primeFiles == null) {
			logger.error("PrIMe source files are empty.");
			throw new OntoChemExpException("PrIMe source files are empty.");
		}
		// Checks if the user shown path to save the OWL file is valid
		if (owlFilesPath == null) {
			logger.error("The ontology file path is null.");
			throw new OntoChemExpException("The ontology file path is null.");
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
		iExperimentConverter.parse(qName, attributes);
		// Calls the method that forwards the call to
		// the apparatus parser class.
		iApparatusConverter.parse(qName, attributes);
		iAdditionalDataItemConverter.parse(qName, attributes);
//////////////////////////////////////////////		
		iDataGroupConverter.parse(qName, attributes);
		iCopyrightConverter.parse(qName, attributes);
		iBibliographyLinkConverter.parse(qName, attributes); // All elements' names are checked
		iCommonPropertiesConverter.parse(qName, attributes);
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
						.create(ontoChemExpKB.getOntoChemExpKbURL().concat(ontoChemExpKB.getOntoChemExpKBFileName()));
			}
			if (ontologyKB == null) {
				ontologyKB = managerKB.createOntology(ontologyKBIRI);
			}
			if (factoryKB == null) {
				factoryKB = managerKB.getOWLDataFactory();
			}
			// Adds the import clause to the OntoChem ABox
			OWLImportsDeclaration importDeclarationABox = factoryKB
					.getOWLImportsDeclaration(IRI.create(ontoChemExpKB.getOntoChemExpKbURL()
							.concat(PrimeConverterUtils.extractExperimentName(experimentName)).concat(".owl")));
			managerKB.applyChange(new AddImport(ontologyKB, importDeclarationABox));
			// Produces the absolute file path including the file name of
			// the current conversion report
			String reportFilePath = aBoxOwlFilePath.concat(BACKSLASH).concat(experimentName).concat(HYPHEN)
					.concat(opCtrl.getConversionReportName());
			// Produces the absolute path including the file name of the
			// generated temporary file
			String generatedTempFilePath = aBoxOwlFilePath.concat(FRONTSLASH).concat(experimentName)
					.concat(opCtrl.getDataStructureSavingFileName());
			// Saves in-memory experiment data and metadata extracted from a
			// PrIMe source file and stored in the PrIMe data structure into a
			// temporary file to check the completeness of the conversion.
			saveInMemorySourceContent(generatedTempFilePath);
			// The following method reports the incompleteness of the conversion
			iCompleteness.reportDifference(reportFilePath, primeFile, generatedTempFilePath);
		} catch (OWLOntologyCreationException e) {
			logger.error("OWLOntologyCreationException occurred.");
		} catch (OntoChemExpException e) {
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
	private void saveInMemorySourceContent(String filePath) throws OntoChemExpException {
		try {
			FileWriterWithEncoding file = new FileWriterWithEncoding(filePath, "UTF-8");
			JAXBContext jaxbContext = JAXBContext.newInstance(Experiment.class);
			Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			jaxbMarshaller.marshal(experiment, file);
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