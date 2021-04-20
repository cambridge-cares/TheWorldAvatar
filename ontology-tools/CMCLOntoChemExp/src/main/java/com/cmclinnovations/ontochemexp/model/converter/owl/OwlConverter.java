package com.cmclinnovations.ontochemexp.model.converter.owl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.output.FileWriterWithEncoding;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.controller.OntoChemExp;
import com.cmclinnovations.ontochemexp.model.IInitPrimeConverter;
import com.cmclinnovations.ontochemexp.model.InitPrimeConverter;
import com.cmclinnovations.ontochemexp.model.PrimeConverterState;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.Experiment;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.utils.PrimeConverterUtils;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.ontology.model.utils.ABoxManagementUtils;

/**
 * A converter that can convert a set of OWL files, each containing an
 * experiment, into PrIMe. When it receives a request for conversion, in each
 * iteration, it loads an OWL file into memory as an ontology model, which is
 * then processed to retrieve the Experiment instance and its properties and
 * related objects such as Apparatus. As soon as it extracts an OWL class, an
 * instance or a property it codifies this in PrIMe. Finally, in the last step
 * it serializes the CTML content and saves this to the disk.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class OwlConverter extends PrimeConverterState implements IOwlConverter {

	static Logger logger = org.slf4j.LoggerFactory.getLogger(OwlConverter.class);

	public static void main(String[] args)
			throws OWLOntologyCreationException, OntoChemExpException, ABoxManagementException {
		ArrayList<String> owlFiles = new ArrayList<String>();
		String primeFilePath = "file:\\C:\\Users\\jb2197\\TestAdditional\\xml";
		owlFiles.add("file:/C:/Users/jb2197/TestAdditional/kb/x00000252.owl"); //x00000003.owl");
//		new OwlConverter().convert(owlFiles, primeFilePath);
	}

	/**
	 * Converts a set of experiment OWL ontologies into PrIMe.
	 * 
	 * @param owlFiles      absolute paths including names of the OWL ontology files
	 * @param primeFilePath the path to the PrIMe files being generated
	 * @throws OntoChemExpException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> owlFiles, String primeFilePath, long instanceSerialID)
			throws OntoChemExpException, OWLOntologyCreationException, ABoxManagementException {
		if (primeFilePath == null) {
			logger.error("The PrIMe file path is null.");
			throw new OntoChemExpException("The PrIMe file path is null.");
		}
		for (String owlFile : owlFiles) {
			// Assigns the name of an OWL file to the global variable
			// owlFileName.
			owlFileName = owlFile;
			// Initialise the instances of the classes that hold PrIMe metadata
			// and data as well as parsing status information thereof.
			// Also initialises the instances of the classes that read
			// configuration parameters using Spring framework annotations.
			IInitPrimeConverter iCOConverter = new InitPrimeConverter();
			iCOConverter.init(instanceSerialID);
			System.out.println(reasonerFactory);
			convertOwlFile(primeFilePath, owlFile);
		}
	}

	/**
	 * Converts an experiment OWL ontology file into PrIMe.
	 * 
	 * @param primeFilePath the path (without the file name) where a PrIMe file
	 *                      being stored
	 * @param owlFile       the path plus file name of an OWL file
	 * @throws OntoChemExpException
	 * @throws OWLOntologyCreationException
	 */
	private void convertOwlFile(String primeFilePath, String owlFile)
			throws OntoChemExpException, OWLOntologyCreationException, ABoxManagementException {
		// OWL file URL needs to create the ontology IRI
		String owlFileURL = PrimeConverterUtils.formOwlUrl(owlFile);
		ontologyIRI = IRI.create(owlFileURL);
		if (ontologyIRI == null) {
			logger.error("An IRI for the following OWL ontology could not be created:" + owlFile);
			throw new OntoChemExpException("An IRI for the following OWL ontology could not be created:" + owlFile);
		}
		
		
		///////////////////////////////////////////////////////////////
		manager.clearOntologies();///// not sure if can add this line /
		///////////////////////////////////////////////////////////////
		
		
		// Loads the ontology located at a IRI
		ontology = iABoxManagement.loadOntology(ontologyIRI);
		// Replaces the previous ontology IRI with the one retrieved
		// from the OWL file
		ontologyIRI = PrimeConverterUtils.readOntologyIRI(ontology);
		// Creates an instance of query engine and assigns it to the
		// member variable engine
		engine = createQueryEngine();
		// Creates an absolute path of the PrIMe file being created
		primeFilePath = ABoxManagementUtils.formSourceFileAbsoultePath(owlFile, primeFilePath);
		// Converts an experiment OWL ontology into PrIMe
		convertOwlFile(primeFilePath);
	}

	/**
	 * Converts an OWL ontology into PrIMe. The conversion process reads the OWL
	 * ontology and looks for data and metadata of the experiment being represented
	 * in PrIMe.
	 * 
	 * @param primeFilePath
	 * @throws OntoChemExpException
	 */
	private void convertOwlFile(String primeFilePath) throws OntoChemExpException {
		iExperimentQuery.query();
		iAdditionalDataItemQuery.query();
		iBibliographyLinkQuery.query();
		iCopyrightQuery.query();
		iPreferredKeyQuery.query();
		iApparatusQuery.query();
		iCommonPropertiesQuery.query();
		iDataGroupQuery.query();
		try {
			String renamedPrimeFilePath = primeFilePath.replace(".xml", "_generated.xml");
			FileWriterWithEncoding file = new FileWriterWithEncoding(renamedPrimeFilePath, "UTF-8");
			JAXBContext jaxbContext = JAXBContext.newInstance(Experiment.class);
			Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
			// output pretty printed
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			jaxbMarshaller.marshal(experiment, file);
//			jaxbMarshaller.marshal(experiment, System.out);
		} catch (JAXBException e) {
			logger.error("JAXBException occurred.");
			e.printStackTrace();
		} catch (IOException e) {
			logger.error("IOException occurred.");
			e.printStackTrace();
		}
	}

	/**
	 * Compose and then performs a SPARQL query to read the ObjectProperty.
	 * 
	 * @param interestedInstance
	 * @return
	 */

	public ArrayList<String> readObjPropertyApparatus(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasApparatus());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyAdditionalDataItem(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasAdditionalDataItem());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyCommonProperties(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasCommonProperties());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyCopyright(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasCopyright());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyBibliographyLink(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasBibliographyLink());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyDataGroup(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasDataGroup());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyPreferredKey(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getOntoChemExpExpSpecs());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyKind(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasKind());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyMode(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasMode());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyProperty(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasProperty());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

//	public ArrayList<String> readObjPropertyValue(String interestedInstance) throws OntoChemExpException {
//		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
//				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
//				ontoChemExpVocabulary.getObjPropertyhasValue());
//		performMultilineAnswerQuery(q, 1);
//		Collections.sort(queryResult);
//		ArrayList<String> ObjPropertyInstances = queryResult;
//		queryResult = new ArrayList<String>();
//		return ObjPropertyInstances;
//	}

	public ArrayList<String> readObjPropertyUncertainty(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasUncertainty());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyComponent(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasComponent());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyDataGroupLink(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasDataGroupLink());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertySpeciesLink(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasSpeciesLink());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyDerivedProperty(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasDerivedProperty());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyDataPoint(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasDataPoint());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyAmount(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasAmount());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyFeature(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasFeature());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyIndicator(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasIndicator());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyObservable(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasObservable());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyPropertyLink(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasPropertyLink());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyDataAttributeLink(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasDataAttributeLink());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	public ArrayList<String> readObjPropertyDataPointX(String interestedInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getObjPropertyhasDataPointX());
		
//		System.out.println(q);
		
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> ObjPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		return ObjPropertyInstances;
	}

	/**
	 * Compose and then performs a SPARQL query to read the DataProperty.
	 * 
	 * @param interestedInstance
	 * @return
	 */

	public String readDataPropertyExpType(String interestedInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getOntoChemExpExpSpecshasExpType());
		return performQuery(q, 1);
	}
	
	public String readDataPropertyValue(String interestedInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), interestedInstance,
				ontoChemExpVocabulary.getDataPropertyhasValue());
		return performQuery(q, 1);
	}

	public String readDataPropertyName(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasName());
		return performQuery(q, 1);
	}

	public String readDataPropertyID(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasID());
		return performQuery(q, 1);
	}

	public String readDataPropertyLabel(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasLabel());
		return performQuery(q, 1);
	}

	public String readDataPropertyUnits(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasUnits());
		return performQuery(q, 1);
	}

	public String readDataPropertyDescription(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasDescription());
		return performQuery(q, 1);
	}

	public String readDataPropertyBound(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasBound());
		return performQuery(q, 1);
	}

	public String readDataPropertyKind(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasKind());
		return performQuery(q, 1);
	}

	public String readDataPropertyTransformation(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasTransformation());
		return performQuery(q, 1);
	}

	public String readDataPropertyType(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasType());
		return performQuery(q, 1);
	}

	public String readDataPropertyMIME(String additionalDataItemInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), additionalDataItemInstance,
				ontoChemExpVocabulary.getDataPropertyhasMIME());
		return performQuery(q, 1);
	}

	public String readDataPropertyItemType(String additionalDataItemInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), additionalDataItemInstance,
				ontoChemExpVocabulary.getDataPropertyhasItemType());
		return performQuery(q, 1);
	}

	public String readDataPropertyPreferredKey(String bibliographyLinkInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), bibliographyLinkInstance,
				ontoChemExpVocabulary.getDataPropertyhasPreferredKey());
		return performQuery(q, 1);
	}

	public String readDataPropertyPrimeID(String bibliographyLinkInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), bibliographyLinkInstance,
				ontoChemExpVocabulary.getDataPropertyhasPrimeID());
		return performQuery(q, 1);
	}
	
	public String readDataPropertyDOI(String bibliographyLinkInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), bibliographyLinkInstance,
				ontoChemExpVocabulary.getDataPropertyhasDOI());
		return performQuery(q, 1);
	}

//	public String readDataPropertyXmlns(String experimentInstance) {
//		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
//				ontoChemExpKB.getOntoChemExpKbTBoxIri(), experimentInstance,
//				ontoChemExpVocabulary.getDataPropertyhasXmlns());
//		return performQuery(q, 1);
//	}
//
//	public String readDataPropertyXmlnsXsi(String experimentInstance) {
//		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
//				ontoChemExpKB.getOntoChemExpKbTBoxIri(), experimentInstance,
//				ontoChemExpVocabulary.getDataPropertyhasXmlnsXsi());
//		return performQuery(q, 1);
//	}
//
//	public String readDataPropertyXsiSchemaLocation(String experimentInstance) {
//		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
//				ontoChemExpKB.getOntoChemExpKbTBoxIri(), experimentInstance,
//				ontoChemExpVocabulary.getDataPropertyhasXsiSchemaLocation());
//		return performQuery(q, 1);
//	}

	public String readDataPropertyDataGroupID(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasDataGroupID());
		return performQuery(q, 1);
	}

	public String readDataPropertyDataPointForm(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasDataPointForm());
		return performQuery(q, 1);
	}

	public String readDataPropertyDataPointID(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasDataPointID());
		return performQuery(q, 1);
	}

	public String readDataPropertyDerivedPropertyExists(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasDerivedPropertyExists());
		return performQuery(q, 1);
	}

	public String readDataPropertyVariableID(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasVariableID());
		return performQuery(q, 1);
	}
	
	public String readDataPropertyPropertyID(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasPropertyID());
		return performQuery(q, 1);
	}
	
	public String readDataPropertyCAS(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasCAS());
		return performQuery(q, 1);
	}
	
	public String readDataPropertyInChI(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasInChI());
		return performQuery(q, 1);
	}
	
	public String readDataPropertySMILES(String propertyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getDataPropertyhasSMILES());
		return performQuery(q, 1);
	}
}
