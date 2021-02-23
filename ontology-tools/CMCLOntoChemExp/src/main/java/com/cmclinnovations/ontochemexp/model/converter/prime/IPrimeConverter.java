package com.cmclinnovations.ontochemexp.model.converter.prime;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

/**
 * An interface to the converter that can convert a set of PrIMe files, 
 * each containing an experiment data as well as results, into OWL files. 
 * When it receives a request for conversion, in each iteration, it parses 
 * a PrIMe file to extract data and metadata. It then represents this in OWL. 
 * Following the extraction and representation of an experiment in OWL, it 
 * saves the OWL content as a file to the disk.
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */
public interface IPrimeConverter {
	/**
	 * Converts a set of PrIMe files into OWL ontologies.
	 * 
	 * @param primeFiles
	 * @param owlFilesPath
	 * @throws OntoChemExpException
	 * @throws OWLOntologyCreationException
	 */
	public void convert(ArrayList<String> primeFiles, String owlFilesPath, long instanceSerialID)
			throws OntoChemExpException, OWLOntologyCreationException;
}
