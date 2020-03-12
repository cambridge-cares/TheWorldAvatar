package org.cam.ceb.como.nist.converter;

import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLIndividual;

public interface INISTOWLWriter {
	public void addDataPropertyToIndividual(OWLIndividual individual, String dataPropertyName, String propertyPathSeparator, String dataPropertyValue) throws OntoSpeciesException;
	public void addDataPropertyToIndividual(OWLIndividual individual, String dataPropertyIRI, String dataPropertyName, String propertyPathSeparator, String dataPropertyValue) throws OntoSpeciesException;
	public OWLIndividual createInstance(String instance, String type);
	public OWLIndividual createInstance(String externalOntologyIRI, String instance, String type);
	public void linkInstance(String objectPropertyName, String domainInstance, String rangeInstance) throws OntoSpeciesException;
	public void removeTBox(IRI ontologyIRIFileSave);
}
