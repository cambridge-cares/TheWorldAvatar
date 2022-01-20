package uk.ac.cam.cares.jps.base.converter.test;

import java.net.URL;

import org.junit.jupiter.api.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import uk.ac.cam.cares.jps.base.converter.ITBoxGeneration;
import uk.ac.cam.cares.jps.base.converter.TBoxGeneration;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class TBoxGenerationTest {
	
	private final String FILE_EXTENSION_CSV = ".csv";
	private final String FILE_EXTENSION_OWL = ".owl";
	
	static ITBoxGeneration iTBoxGeneration = new TBoxGeneration();
	
	@Test
	public void testClassHierachy(){
		
	}
	
	@Test
	public void testObjectPropertyHierachy(){
		
	}

	@Test
	public void testObjectPropertyInverseOfRelation(){
		
	}

	@Test
	public void testObjectPropertyCharacteristics(){
		
	}
	
	@Test
	public void testDataPropertyHierachy(){
		
	}

	@Test
	public void testDataPropertyCharacteristics(){
		
	}

	@Test
	public void testEquivalentRelation(){
		
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
