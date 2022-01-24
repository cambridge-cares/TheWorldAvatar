package uk.ac.cam.cares.jps.base.converter.test;

import java.net.URL;
import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import static org.junit.Assert.*;

import uk.ac.cam.cares.jps.base.converter.ITBoxGeneration;
import uk.ac.cam.cares.jps.base.converter.TBoxGeneration;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * Unit tests provided in this class cover the creation of following aspects of ontology.
 *  - Class, class hierarchy and equivalent relationship between classes
 *  - Object property, object property hierarchy and inverse
 *    relation and characteristics of object property
 *  - Data property, data property hierarchy and characteristics of data property
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public class TBoxGenerationTest {
	
	private final String SAMPLE_TBOX_CSV_TEMPLATE_FILE_1 = "sample-tbox-template-input-1.csv";
	private final String SAMPLE_TBOX_CSV_TEMPLATE_FILE_2 = "sample-tbox-template-input-2.csv";
	private final String SAMPLE_TBOX_CSV_TEMPLATE_FILE_3 = "sample-tbox-template-input-3.csv";
	private final String SAMPLE_TBOX_CSV_TEMPLATE_FILE_4 = "sample-tbox-template-input-4.csv";
	private final String SAMPLE_TBOX_CSV_TEMPLATE_FILE_5 = "sample-tbox-template-input-5.csv";

	private final String REFERENCE_OWL_FILE_1 = "sample-tbox-template-input-1.owl";
	private final String REFERENCE_OWL_FILE_2 = "sample-tbox-template-input-2.owl";
	private final String REFERENCE_OWL_FILE_3 = "sample-tbox-template-input-3.owl";
	private final String REFERENCE_OWL_FILE_4 = "sample-tbox-template-input-4.owl";
	private final String REFERENCE_OWL_FILE_5 = "sample-tbox-template-input-5.owl";

	private final String FILE_EXTENSION_CSV = ".csv";
	private final String FILE_EXTENSION_OWL = ".owl";
	
	private final String SAMPLE_CSV_FILE_PATH = "TBoxManagerTest/csv/";
	private final String REFERENCE_OWL_FILE_PATH = "TBoxManagerTest/owl/";
	
	static ITBoxGeneration iTBoxGeneration = new TBoxGeneration();

	/**
	 * Tests the creation and hierarchy of classes
	 */
	@Test
	public void testClass(){
		OWLOntology generatedOntology = generateOntology(SAMPLE_CSV_FILE_PATH+SAMPLE_TBOX_CSV_TEMPLATE_FILE_1);
		if (generatedOntology == null) {
			throw new JPSRuntimeException("The requested CSV file could not be converted into an ontology.");
		}

		OWLOntology referenceOntology = readReferenceOntology(REFERENCE_OWL_FILE_PATH+REFERENCE_OWL_FILE_1);
		if (referenceOntology == null) {
			throw new JPSRuntimeException("The requested reference ontology could not be read from the provided path.");
		}
		
		assertEquals(generatedOntology.getAxiomCount(), referenceOntology.getAxiomCount());
		assertEquals(generatedOntology.getAxioms(), referenceOntology.getAxioms());
	}
	
	/**
	 * Tests the creation, hierarchy and inverse relation of object properties.
	 */
	@Test
	public void testObjectProperty(){
		OWLOntology generatedOntology = generateOntology(SAMPLE_CSV_FILE_PATH+SAMPLE_TBOX_CSV_TEMPLATE_FILE_2);
		if (generatedOntology == null) {
			throw new JPSRuntimeException("The requested CSV file could not be converted into an ontology.");
		}

		OWLOntology referenceOntology = readReferenceOntology(REFERENCE_OWL_FILE_PATH+REFERENCE_OWL_FILE_2);
		if (referenceOntology == null) {
			throw new JPSRuntimeException("The requested reference ontology could not be read from the provided path.");
		}
		
		assertEquals(generatedOntology.getAxiomCount(), referenceOntology.getAxiomCount());
		assertEquals(generatedOntology.getAxioms(), referenceOntology.getAxioms());		
	}

	/**
	 * Tests some characteristics of object property.
	 */
	@Test
	public void testObjectPropertyCharacteristics(){
		OWLOntology generatedOntology = generateOntology(SAMPLE_CSV_FILE_PATH+SAMPLE_TBOX_CSV_TEMPLATE_FILE_3);
		if (generatedOntology == null) {
			throw new JPSRuntimeException("The requested CSV file could not be converted into an ontology.");
		}

		OWLOntology referenceOntology = readReferenceOntology(REFERENCE_OWL_FILE_PATH+REFERENCE_OWL_FILE_3);
		if (referenceOntology == null) {
			throw new JPSRuntimeException("The requested reference ontology could not be read from the provided path.");
		}
		
		assertEquals(generatedOntology.getAxiomCount(), referenceOntology.getAxiomCount());
		assertEquals(generatedOntology.getAxioms(), referenceOntology.getAxioms());
	}
	
	/**
	 * Tests the creation, hierarchy and characteristics of data properties.
	 */
	@Test
	public void testDataProperty(){
		OWLOntology generatedOntology = generateOntology(SAMPLE_CSV_FILE_PATH+SAMPLE_TBOX_CSV_TEMPLATE_FILE_4);
		if (generatedOntology == null) {
			throw new JPSRuntimeException("The requested CSV file could not be converted into an ontology.");
		}

		OWLOntology referenceOntology = readReferenceOntology(REFERENCE_OWL_FILE_PATH+REFERENCE_OWL_FILE_4);
		if (referenceOntology == null) {
			throw new JPSRuntimeException("The requested reference ontology could not be read from the provided path.");
		}
		
		assertEquals(generatedOntology.getAxiomCount(), referenceOntology.getAxiomCount());
		assertEquals(generatedOntology.getAxioms(), referenceOntology.getAxioms());
	}

	/**
	 * Tests the creation of equivalent classes.
	 */
	@Test
	public void testClassEquivalentRelation(){
		OWLOntology generatedOntology = generateOntology(SAMPLE_CSV_FILE_PATH+SAMPLE_TBOX_CSV_TEMPLATE_FILE_5);
		if (generatedOntology == null) {
			throw new JPSRuntimeException("The requested CSV file could not be converted into an ontology.");
		}

		OWLOntology referenceOntology = readReferenceOntology(REFERENCE_OWL_FILE_PATH+REFERENCE_OWL_FILE_5);
		if (referenceOntology == null) {
			throw new JPSRuntimeException("The requested reference ontology could not be read from the provided path.");
		}
		
		assertEquals(generatedOntology.getAxiomCount(), referenceOntology.getAxiomCount());
		assertEquals(generatedOntology.getAxioms(), referenceOntology.getAxioms());		
	}
	
	/**
	 * Reads an ontology from a file located in the file system.
	 * 
	 * @param referenceOntologyFilePath the absolute path of the ontology file.
	 * @return
	 */
	private OWLOntology readReferenceOntology(String referenceOntologyFile){
		URL referenceOWLFile = getClass().getClassLoader().getResource(referenceOntologyFile);
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLOntology referenceOntology = null;
		try {
			referenceOntology = manager.loadOntology(IRI.create(addFileProtocol(referenceOWLFile.getPath())));
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (JPSRuntimeException e) {
			e.printStackTrace();
		}
		return referenceOntology;
	}

	/**
	 * Converts a TBox CSV Template formatted file into OWL. 
	 * 
	 * @param tBoxCSVTemplateFilePath the absolute path of the TBox CSV file.
	 * @return
	 */
	private OWLOntology generateOntology(String tBoxCSVTemplateFilePath){
		URL tBoxCSVTemplateFileURL = getClass().getClassLoader().getResource(tBoxCSVTemplateFilePath);
		iTBoxGeneration.generateTBox(tBoxCSVTemplateFileURL.getPath());
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		OWLOntology generatedOntology = null;
		try {
			generatedOntology = manager.loadOntology(IRI.create(addFileProtocol(tBoxCSVTemplateFileURL.getPath().replace(FILE_EXTENSION_CSV, FILE_EXTENSION_OWL))));
		} catch (OWLOntologyCreationException e) {
			e.printStackTrace();
		} catch (JPSRuntimeException e) {
			e.printStackTrace();
		}
		return generatedOntology;
	}
	
	/**
	 * Adds the protocol 'file:' at the beginning of a file path
	 * to form a URL that can be used in an OWL file as a URL.
	 * 
	 * @param path an absolute file path that needs to be converted
	 * to a URL that can be used in an OWL file.
	 * @return an OWL file formatted URL.
	 * @throws JPSRuntimeException a specialised exception designed to deal with
	 * errors at runtime in JPS libraries.
	 */
	public static String addFileProtocol(String path) throws JPSRuntimeException{
		if(path==null){
			throw new JPSRuntimeException("A null input path has been provided.");
		}
		if(!path.contains("file:")){
			path = "file:"+path;
		}
		return path;
	}
}
