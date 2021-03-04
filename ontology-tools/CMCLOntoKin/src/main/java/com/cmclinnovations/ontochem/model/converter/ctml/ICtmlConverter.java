package com.cmclinnovations.ontochem.model.converter.ctml;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import com.cmclinnovations.ontochem.model.exception.OntoException;

public interface ICtmlConverter {
	public void convert(ArrayList<String> ctmlFiles, String owlFilesPath)
			throws OntoException, OWLOntologyCreationException;
}
