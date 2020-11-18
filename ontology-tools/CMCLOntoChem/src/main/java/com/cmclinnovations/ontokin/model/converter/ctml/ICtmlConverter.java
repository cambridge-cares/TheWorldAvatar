package com.cmclinnovations.ontokin.model.converter.ctml;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import com.cmclinnovations.ontokin.model.exception.OntoException;

public interface ICtmlConverter {
	public void convert(ArrayList<String> ctmlFiles, String owlFilesPath)
			throws OntoException, OWLOntologyCreationException;
}
