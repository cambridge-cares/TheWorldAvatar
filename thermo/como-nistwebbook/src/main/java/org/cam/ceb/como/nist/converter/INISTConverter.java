package org.cam.ceb.como.nist.converter;

import org.cam.ceb.como.nist.model.exception.OntoSpeciesException;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

public interface INISTConverter {
	public void convert(String sourceHTMLFile, String sourceStructureFile, String owlFilesPath)
			throws OntoSpeciesException, OWLOntologyCreationException;
}
