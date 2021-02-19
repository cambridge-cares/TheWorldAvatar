package com.cmclinnovations.ontochemexp.model.converter.owl;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public interface IOwlConverter {
	public void convert(ArrayList<String> ctmlFiles, String owlFilesPath, long instanceSerialID)
			throws OntoChemExpException, OWLOntologyCreationException, ABoxManagementException;
}