
package com.cmclinnovations.ontochemexp.model.owl;

import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.copyright.Copyright;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.CopyrightParseStatus;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class CopyrightWriter extends PrimeConverter implements ICopyrightWriter {
	public void writer(char ch[], int start, int length) throws SAXException {

		readCopyright(ch, start, length);
	}

	/**
	 * Forwards the call to the methods that first read and then write Copyright
	 * data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readCopyright(char ch[], int start, int length) throws SAXException {
		if (copyrightParseStatus.isCopyright()) {
//			String value = new String(ch, start, length);
//			copyright.setValue(value);
			createCopyright();
			linkCopyrightToExperiment();
//			copyrightParseStatus.setCopyright(false); // Set to false otherwise it will read every element
//			experiment.setCopyright(copyright);
//			copyright = new Copyright();
		}
	}

	private void createCopyright() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassCopyright(),
					"Copyright" + UNDERSCORE + copyrightID);
		} catch (ABoxManagementException e) {
			logger.error("An individual of copyright could not be created.");
		}
	}

	private void linkCopyrightToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasCopyright(),
					"Experiment" + UNDERSCORE + experimentInstanceId, "Copyright" + UNDERSCORE + copyrightID);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a copyrigh and an experiment conducted using it.");
		}
	}

	public void writeValue() {
		if (copyright.getValue() != null && !copyright.getValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty("Copyright" + UNDERSCORE + copyrightID,
						ontoChemExpVocabulary.getDataPropertyhasValue(), copyright.getValue(), STRING);
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("Copyright"+UNDERSCORE+copyrightID, dataPropertyIRI, copyright.getValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	public void setUP() {
		experiment.setCopyright(copyright);
		copyright = new Copyright();
	}
}
