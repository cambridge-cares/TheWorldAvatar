package com.cmclinnovations.prime.species.model.converter.owl;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public interface IOwlConverter {
	public void convert(ArrayList<String> owlFiles, String primeSpeciesFilePath, long instanceSerialID)
			throws OntoPrimeSpeciesException, OWLOntologyCreationException, ABoxManagementException;
}
