package org.cam.ceb.como.nist.converter;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLIndividual;

public interface INISTOWLWriter {
	public OWLIndividual createInstance(String instance, String type);
	public OWLIndividual createInstance(String externalOntologyIRI, String instance, String type);
	public void removeTBox(IRI ontologyIRIFileSave);
}
