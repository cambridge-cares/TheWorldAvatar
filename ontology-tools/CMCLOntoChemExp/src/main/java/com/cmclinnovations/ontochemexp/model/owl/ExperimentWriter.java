package com.cmclinnovations.ontochemexp.model.owl;

import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;
import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * Implements the method that forwards a call to those methods that
 * read a PrIMe experiment's data and metadata from an in-memory temporary 
 * storage to pass them to the corresponding PrIMe to OWL conversion methods.
 * 
 * 
 * @author Feroz Farazi (msff2@cam.ac.uk)
 *
 */

public class ExperimentWriter extends PrimeConverter implements IExperimentWriter{
	public void writer(char ch[], int start, int length) throws SAXException{
		readExperiment(ch, start, length);
	}
	
	/**
	 * Forwards the call to the methods that first read and then write 
	 * experiment data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readExperiment(char ch[], int start, int length) throws SAXException {
		if (experimentParseStatus.isExperiment()) {
			// Verifies if there is a need to create an experiment. This has
			// been implemented to avoid creating another instance of the same
			// experiment in the next iteration of the parser.
			if(needsToCreateExperiment) {
				// Calls this method to create an OWL instance of the chemical 
				// experiment being parsed.
				createExperiment();
				addAllAttributes();
				addHeadComment();
				needsToCreateExperiment = false;
			}
			experimentParseStatus.setExperiment(false);
		}
	}

	/**
	 * Following the detection of the experiment tag, it creates 
	 * an instance of Experiment in OWL.
	 * 
	 */
	private void createExperiment() {
		try {
			currentExperimentInstance = "Experiment"+UNDERSCORE+experimentInstanceId;
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassExperiment(), currentExperimentInstance);
			
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of experiment could not be created.");
		}
	}
	
	private void addAllAttributes() {
	}
	
	private void addHeadComment() {
		try {
			IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_COMMENT));
			iABoxManagement.addProperty("Experiment"+UNDERSCORE+experimentInstanceId, 
					dataPropertyIRI, ontoChemExpKB.getOntoChemExpHeadComment(), STRING);
		} catch (ABoxManagementException e) {
			e.printStackTrace();
		}
	}
}
