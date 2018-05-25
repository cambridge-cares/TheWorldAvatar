package uk.ac.ceb.como.gc.ontology.test;

import java.io.File;

import org.junit.Test;
import org.semanticweb.HermiT.Reasoner;
import org.semanticweb.owlapi.apibinding.OWLManager;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

/**
 * 
 * @author nk510
 *
 *         Tests Gainesville Core Ontology (GNVC) ver. 0.7, for consistency.
 *         Hermit reasoner 1.3.8.4 is used for testing consistency of a
 *         Gainesville Core Ontology (GNVC) ver 0.7. The ontology is taken from:
 *         http://ontologies.makolab.com/gc06/gc.html#sec-crossref Resolved
 *         conflict with loading Basic Formal Ontology(BFO):
 *         https://github.com/BFO-ontology/BFO
 * 
 *         Ontology is available in folder:
 *         /CoMoOntology/src/test/resources/ontology/
 * 
 */

public class ConsistencyTest {

	static OWLOntologyManager model = OWLManager.createOWLOntologyManager();

	/**
	 * @author nk510 This file path shows that ontologies are stored in
	 *         CoMoOnotology's folder: '/src/test/resources/ontology/'.
	 * 
	 */

	@Test
	public void checkConsistency() throws OWLOntologyCreationException {

		/**
		 * @author nk510 Line below creates HermiT's object variable.It creates an
		 *         instance of the Reasoner class in the package 'org.semanticweb.HermiT'.
		 */

		Reasoner hermit = new Reasoner(getOntology(new File("src/test/resources/ontology/gc07.owl")));

		System.out.println("GNVC ontology consistecy: " + hermit.isConsistent());

	}

	/**
	 * 
	 * @author nk510 Load single ontology from local file
	 * 
	 */

	public static OWLOntology getOntology(File file) throws OWLOntologyCreationException {

		OWLOntology gc_ontology = model.loadOntologyFromOntologyDocument(file);

		return gc_ontology;

	}
}