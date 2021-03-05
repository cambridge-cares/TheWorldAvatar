/*
 * 
 */
package uk.ac.ceb.como.gc.ontology.test;

import java.io.File;

import org.junit.Test;
import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

// TODO: Auto-generated Javadoc
/**
 * 
 * The Class ConsistencyTest.
 *
 * @author nk510
 * 
 *      <p>Tests for consistency Compchem ontology that extends Gainesville Core Ontology (GNVC) ver. 0.7.
 *         Hermit reasoner 1.3.8.4 is used for testing consistency of a
 *         Gainesville Core Ontology (GNVC) ver 0.7. The ontology is taken from:
 *         http://ontologies.makolab.com/gc06/gc.html#sec-crossref Resolved
 *         conflict with loading Basic Formal Ontology(BFO):
 *         https://github.com/BFO-ontology/BFO
 * 
 *         Ontology is available in folder:
 *         /CoMoOntology/src/test/resources/ontology/ </p>
 *         
 */

public class ConsistencyTest {

	/** The model. */
	static OWLOntologyManager model = OWLManager.createOWLOntologyManager();

	/**
	 * 
	 * Check consistency.
	 *
	 * @author nk510
	 * @throws OWLOntologyCreationException <p>This file path shows that ontologies are stored in
	 *         CoMoOnotology project folder: '/src/test/resources/ontology/compchem_ontology/'.</p>
	 *         
	 */

	@Test
    public void checkConsistency() throws OWLOntologyCreationException {

		/**
		 * @author nk510 
		 * <p>Line below creates HermiT's object variable.It creates an
		 *         instance of the Reasoner class in the package 'org.semanticweb.HermiT'.</p>
		 */

		Reasoner hermit = new Reasoner(getOntology(new File("src/test/resources/ontology/compchem_ontology/ontocompchem.rdf")));
		
		System.out.println("OntoCompchem ontology consistecy: " + hermit.isConsistent());

	}
	
	/**
	 * Gets the ontology.
	 *
	 * @author nk510
	 * <p>Load single ontology from local file. </p>
	 * @param file the file
	 * @return the ontology
	 * @throws OWLOntologyCreationException the OWL ontology creation exception
	 */

	public static OWLOntology getOntology(File file) throws OWLOntologyCreationException {

		OWLOntology gc_ontology = model.loadOntologyFromOntologyDocument(file);

		return gc_ontology;

	}
}