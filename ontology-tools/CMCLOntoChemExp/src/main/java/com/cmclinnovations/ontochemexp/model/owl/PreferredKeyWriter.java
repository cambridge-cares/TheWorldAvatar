package com.cmclinnovations.ontochemexp.model.owl;

import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.preferred_key.PreferredKey;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class PreferredKeyWriter extends PrimeConverter implements IPreferredKeyWriter {

	@Override
	public void writer(char[] ch, int start, int length) throws SAXException {
		// TODO Auto-generated method stub
		readPreferredKey(ch, start, length);
	}

	private void readPreferredKey(char ch[], int start, int length) throws SAXException {
		if (preferredKeyParseStatus.isPreferredKey()) {
			createExpSpecs();
			linkExpSpecsToExperiment();
		}
	}

	private void createExpSpecs() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getOntoChemExpExpSpecs(),
					ontoChemExpVocabulary.getOntoChemExpExpSpecs() + UNDERSCORE + preferredKeyID);
		} catch (ABoxManagementException e) {
			logger.error("");
		}
	}

	private void linkExpSpecsToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getOntoChemExphasExpSpecs(),
					currentExperimentInstance, ontoChemExpVocabulary.getOntoChemExpExpSpecs() + UNDERSCORE + preferredKeyID);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between an expSpecs and an experiment.");
		}
	}

	public void writeValue() {
		if (preferredKey.getValue() != null && !preferredKey.getValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(ontoChemExpVocabulary.getOntoChemExpExpSpecs() + UNDERSCORE + preferredKeyID,
						ontoChemExpVocabulary.getOntoChemExpExpSpecshasExpType(), preferredKey.getValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("PreferredKey"+UNDERSCORE+preferredKeyID, dataPropertyIRI, preferredKey.getValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public void setUP() {
		experiment.setPreferredKey(preferredKey);
		preferredKey = new PreferredKey();
	}
}
