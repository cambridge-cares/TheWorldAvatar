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
			createPreferredKey();
			addAllAttributes();
			linkPreferredKeyToExperiment();
		}
	}

	private void createPreferredKey() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassPreferredKey(),
					"PreferredKey" + UNDERSCORE + preferredKeyID);
		} catch (ABoxManagementException e) {
			logger.error("");
		}
	}

	private void addAllAttributes() {
		if (preferredKey.getType() != null && !preferredKey.getType().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty("PreferredKey" + UNDERSCORE + preferredKeyID,
						ontoChemExpVocabulary.getDataPropertyhasType(), preferredKey.getType(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void linkPreferredKeyToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasPreferredKey(),
					"Experiment" + UNDERSCORE + experimentInstanceId, "PreferredKey" + UNDERSCORE + preferredKeyID);
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between a preferredKey and an experiment conducted using it.");
		}
	}

	public void writeValue() {
		if (preferredKey.getValue() != null && !preferredKey.getValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty("PreferredKey" + UNDERSCORE + preferredKeyID,
						ontoChemExpVocabulary.getDataPropertyhasValue(), preferredKey.getValue(), STRING);
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
