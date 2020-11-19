package com.cmclinnovations.prime.species.model.converter.species;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import com.cmclinnovations.prime.species.model.exception.OntoPrimeSpeciesException;

public interface IPrimeSpeciesConverter {
	/**
	 * Converts a set of PrIMe files into OWL ontologies.
	 * 
	 * @param primeFiles
	 * @param owlFilesPath
	 * @throws OntoPrimeSpeciesException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> primeFiles, String owlFilesPath, long instanceSerialID) throws OWLOntologyCreationException, OntoPrimeSpeciesException;
}
