package com.cmclinnovations.ontokin.model.converter.owl;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import com.cmclinnovations.ontokin.model.exception.OntoException;

public interface IOwlConverter {
	public void convert(ArrayList<String> ctmlFiles, String owlFilesPath)
			throws OntoException, OWLOntologyCreationException;
}