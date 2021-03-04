package com.cmclinnovations.prime.species.model.converter.owl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.io.output.FileWriterWithEncoding;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.EntityType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.ontology.model.utils.ABoxManagementUtils;
import com.cmclinnovations.prime.species.model.IInitPrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.InitPrimeSpeciesConverter;
import com.cmclinnovations.prime.species.model.PrimeSpeciesConverterState;
import com.cmclinnovations.prime.species.model.data.structure.chemicalspecies.ChemicalSpecies;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;
import com.cmclinnovations.prime.species.model.utils.PrimeSpeciesConverterUtils;

public class OwlConverter extends PrimeSpeciesConverterState implements IOwlConverter {

	static Logger logger = LoggerFactory.getLogger(OwlConverter.class);

	public static void main(String[] args)
			throws OWLOntologyCreationException, OntoPrimeSpeciesException, ABoxManagementException {
		ArrayList<String> owlFiles = new ArrayList<String>();
		String primeFilePath = "file:\\C:\\Users\\jb2197\\Documents\\c4e-jb2197-PrIMeSpeciesOntology\\Test\\s00000074\\xml";
		owlFiles.add("file:/C:/Users/jb2197/Documents/c4e-jb2197-PrIMeSpeciesOntology/Test/kb/s00000074.owl");
//		owlFiles.add("file:/C:/Users/jb2197/Documents/c4e-jb2197-PrIMeSpeciesOntology/Data/Test/kb/POLIMI_H2CO_1412.owl");
//		new OwlConverter().convert(owlFiles, primeFilePath);
	}

	public void convert(ArrayList<String> owlFiles, String primeSpeciesFilePath, long instanceSerialID)
			throws OntoPrimeSpeciesException, OWLOntologyCreationException, ABoxManagementException {
		if (primeSpeciesFilePath == null) {
			logger.error("The PrIMeSpecies file path is null.");
			throw new OntoPrimeSpeciesException("The PrIMeSpecies file path is null.");
		}
		for (String owlFile : owlFiles) {
			owlFileName = owlFile;
			IInitPrimeSpeciesConverter iCOConverter = new InitPrimeSpeciesConverter();
			iCOConverter.init(instanceSerialID);
			System.out.println(reasonerFactory);
			convertOwlFile(primeSpeciesFilePath, owlFile);
		}
	}

	private void convertOwlFile(String primeSpeciesFilePath, String owlFile)
			throws OntoPrimeSpeciesException, OWLOntologyCreationException, ABoxManagementException {
		String owlFileURL = PrimeSpeciesConverterUtils.formOwlUrl(owlFile);
		ontologyIRI = IRI.create(owlFileURL);
		if (ontologyIRI == null) {
			logger.error("An IRI for the following OWL ontology could not be created:" + owlFile);
			throw new OntoPrimeSpeciesException("An IRI for the following OWL ontology could not be created:" + owlFile);
		}
		
		manager.clearOntologies();
		ontology = iABoxManagement.loadOntology(ontologyIRI);
		ontologyIRI = PrimeSpeciesConverterUtils.readOntologyIRI(ontology);
		engine = createQueryEngine();
		primeSpeciesFilePath = ABoxManagementUtils.formSourceFileAbsoultePath(owlFile, primeSpeciesFilePath);
		convertOwlFile(primeSpeciesFilePath);
	}
	
	private void convertOwlFile(String primeSpeciesFilePath) throws OntoPrimeSpeciesException {
		iChemicalSpeciesQuery.query();
//		queryTest();
		
		try {
			String renamedPrimeSpeciesFilePath = primeSpeciesFilePath.replace(".xml", "_generated.xml");
			FileWriterWithEncoding file = new FileWriterWithEncoding(renamedPrimeSpeciesFilePath, "UTF-8");
			JAXBContext jaxbContext;
			jaxbContext = JAXBContext.newInstance(ChemicalSpecies.class);
			Marshaller jaxbMarshaller = jaxbContext.createMarshaller();
			jaxbMarshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
			jaxbMarshaller.marshal(chemicalSpecies, file);
		} catch (JAXBException e) {
			logger.error("JAXBException occurred.");
			e.printStackTrace();
		} catch (IOException e) {
			logger.error("IOException occurred.");
			e.printStackTrace();
		}
	}
	
	private void queryTest() {
//		String q = "PREFIX base: <http://www.theworldavatar.com/kb/ontokin/POLIMI_H2CO_1412.owl#>\r\n" + 
//				"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\r\n" + 
//				"SELECT ?v WHERE {\r\n" + 
//				"PropertyValue(base:Element_AR"+", rdfs:label, ?v)\r\n" + 
//				"}";
		
		String q = "PREFIX base: <http://www.theworldavatar.com/kb/ontospecies/s00006480.owl/>\r\n" + 
				"PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\r\n" + 
				"PREFIX OntoKin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl/>\r\n" + 
				"PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\r\n" + 
				"PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl/>\r\n" + 
				"SELECT ?v WHERE {\r\n" + 
//				"PropertyValue(base:Species_2404722298400900"+", skos:altLabel, ?v)\r\n" + 
//				"PropertyValue(base:Species_2404722298400900"+", rdfs:label, ?v)\r\n" + 
//				"PropertyValue(base:Species_2404722298400900"+", OntoSpecies:hasEmpiricalFormula, ?v)\r\n" + 
//				"PropertyValue(base:EmpiricalFormula_2656725278302900"+", ontokin:hasElement, ?v)\r\n" + 
				"PropertyValue(base:EmpiricalFormula_2656725278302900"+", rdfs:label, ?v)\r\n" + 
				"}";
		
		System.out.println(q);
//		

//		System.out.println(performQuery(q, 1));
		
		performMultilineAnswerQuery(q, 1);
		System.out.println(queryResult);
		queryResult = new ArrayList<String>();
	}
	
	public ArrayList<String> readRDFSLabel(String instance) {
		String q = formQueryWithAStandardVocabulary(RDFS, 
				RDFS_URL, instance, 
				RDFS_LABEL);
		
		String labels = performQuery(q, 1);
		if (labels != null && !labels.isEmpty()) {
			labels = labels.replace("?de.derivo.sparqldlapi.Var@76 = ?\"", "").replace("\"^^xsd:string", "///").replace("\n", "");
			String str[] = labels.split("///");
			List<String> rl = new ArrayList<String>();
			rl = Arrays.asList(str);
			Collections.sort(rl);
			ArrayList<String> rdfsLabelInstances = new ArrayList<String>();
			for (String s : rl) {
//				System.out.println(s);
				rdfsLabelInstances.add(s);
			}
			rl = new ArrayList<String>();
			return rdfsLabelInstances;
		} else {
			return null;
		}
	}
	
	public String readDataProperty(String tBoxPrefix, String tBoxIri, String instance, String dataProperty) {
		String q = formQueryWithBaseURL(tBoxPrefix.concat(COLON), tBoxIri, instance, dataProperty);
		
		return performQuery(q, 1);
	}
	
	public ArrayList<String> readDataPropertyCasRegistryID(String instance) {
		String q = formQueryWithBaseURL(ontoPrimeSpeciesKB.getOntoSpeciesNamespace().concat(COLON), 
				ontoPrimeSpeciesKB.getOntoSpeciesKbTBoxIri(), 
				instance, ontoPrimeSpeciesVocabulary.getOSDataPropertyhasCasRegistryID());
		
		String casNumbers = performQuery(q, 1);
		if (casNumbers != null && !casNumbers.isEmpty()) {
			casNumbers = casNumbers.replace("?de.derivo.sparqldlapi.Var@76 = ?\"", "").replace("\"^^xsd:string", "；").replace("\n", "");
			String str[] = casNumbers.split("；");
			List<String> cas = new ArrayList<String>();
			cas = Arrays.asList(str);
			Collections.sort(cas);
			ArrayList<String> dataPropertyInstances = new ArrayList<String>();
			for (String s : cas) {
				dataPropertyInstances.add(s);
			}
			cas = new ArrayList<String>();
			return dataPropertyInstances;
		} else {
			return null;
		}
	}
	
	public ArrayList<String> readDataPropertyAltLabel(String instance) {
		String q = formQueryWithAStandardVocabulary(SKOS, 
				SKOS_URL, instance, 
				ontoPrimeSpeciesVocabulary.getOSDataPropertyhasAltLabel());

		String altNames = performQuery(q, 1);
		if (altNames != null && !altNames.isEmpty()) {
			altNames = altNames.replace("?de.derivo.sparqldlapi.Var@76 = ?\"", "").replace("\"^^xsd:string", "；").replace("\\\"", "\"").replace("\n", "");
			String str[] = altNames.split("；");
			List<String> alt = new ArrayList<String>();
			alt = Arrays.asList(str);
			Collections.sort(alt);
			ArrayList<String> dataPropertyInstances = new ArrayList<String>();
			for (String s : alt) {
				dataPropertyInstances.add(s);
			}
			alt = new ArrayList<String>();
			
			return dataPropertyInstances;
		} else {
			return null;
		}
	}
	
	public ArrayList<String> readObjProperty(String tBoxPrefix, String tBoxIri, String instance, String objProperty) {
		String q = formQueryWithBaseURL(tBoxPrefix.concat(COLON), tBoxIri, instance, objProperty);
		performMultilineAnswerQuery(q, 1);
		
		Collections.sort(queryResult);
		ArrayList<String> objPropertyInstances = queryResult;
		queryResult = new ArrayList<String>();
		
		return objPropertyInstances;
	}	
}
