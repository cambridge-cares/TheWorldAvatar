package com.cmclinnovations.prime.species.model.owl;

import org.xml.sax.SAXException;

import com.cmclinnovations.ontology.model.exception.ABoxManagementException;
import com.cmclinnovations.prime.species.model.converter.species.PrimeSpeciesConverter;

public class ChemicalSpeciesWriter extends PrimeSpeciesConverter implements IChemicalSpeciesWriter {
	public void writer(char ch[], int start, int length) throws SAXException{
		readChemicalSpecies(ch, start, length);
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
	private void readChemicalSpecies(char ch[], int start, int length) throws SAXException {
		if (chemicalSpeciesParseStatus.isChemicalSpecies()) {
			if (needsToCreateChemicalSpecies) {
				createChemicalSpecies();
				needsToCreateChemicalSpecies = false;
			}
			chemicalSpeciesParseStatus.setChemicalSpecies(false);
		}
	}
	
	private void createChemicalSpecies() {
		try {
			iABoxManagement.createIndividual(ontoPrimeSpeciesVocabulary.getOSClassSpecies(), 
					"Species"+UNDERSCORE+chemicalSpeciesInstanceID);
			
//			if (chemicalSpecies.getPrimeID() != null && !chemicalSpecies.getPrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
//						ontoPrimeSpeciesVocabulary.getDataPropertyhasPrimeID(), chemicalSpecies.getPrimeID(), STRING);
//			}
//			
//			if (chemicalSpecies.getXmlns() != null && !chemicalSpecies.getXmlns().trim().isEmpty()) {
//				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
//						ontoPrimeSpeciesVocabulary.getDataPropertyhasXmlns(), chemicalSpecies.getXmlns(), STRING);
//			}
//			
//			if (chemicalSpecies.getXmlnsXsi() != null && !chemicalSpecies.getXmlnsXsi().trim().isEmpty()) {
//				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
//						ontoPrimeSpeciesVocabulary.getDataPropertyhasXmlnsXsi(), chemicalSpecies.getXmlnsXsi(), STRING);
//			}
//			
//			if (chemicalSpecies.getXsiSchemaLocation() != null && !chemicalSpecies.getXsiSchemaLocation().trim().isEmpty()) {
//				iABoxManagement.addProperty("Species"+UNDERSCORE+chemicalSpeciesInstanceID, 
//						ontoPrimeSpeciesVocabulary.getDataPropertyhasXsiSchemaLocation(), chemicalSpecies.getXsiSchemaLocation(), STRING);
//			}
		
		} catch (ABoxManagementException e) {
			logger.error(
					"An individual of chemicalSpecies could not be created.");
		}
	}
}
