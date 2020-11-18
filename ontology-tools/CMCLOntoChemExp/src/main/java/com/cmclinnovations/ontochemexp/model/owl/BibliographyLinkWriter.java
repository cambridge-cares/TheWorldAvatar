
package com.cmclinnovations.ontochemexp.model.owl;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.IRI;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.BibliographyLink;
import com.cmclinnovations.ontochemexp.model.parse.status.prime.BibliographyLinkParseStatus;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class BibliographyLinkWriter extends PrimeConverter implements IBibliographyLinkWriter {
	public void writer(String qName) throws SAXException {
		readBibliographyLink(qName);
	}

	/**
	 * Forwards the call to the methods that first read and then write
	 * BibliographyLink data and metadata.
	 * 
	 * @param ch
	 * @param start
	 * @param length
	 * @throws SAXException
	 */
	private void readBibliographyLink(String qName) throws SAXException {
		if (bibliographyLinkParseStatus.isBibliographyLink()) {
			createBibliographyLink();
			addAllAttributes();
			linkBibliographyLinkToExperiment();
		}
	}

	private void createBibliographyLink() {
		bibliographyLinkCount += 1;

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassBibliographyLink(),
					"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount));
		} catch (ABoxManagementException e) {
			logger.error("An individual of bibliographyLink could not be created.");
		}
	}

	private void addAllAttributes() {
		if (bibliographyLink.getPreferredKey() != null && !bibliographyLink.getPreferredKey().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
						ontoChemExpVocabulary.getDataPropertyhasPreferredKey(), bibliographyLink.getPreferredKey(),
						STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		if (bibliographyLink.getPrimeID() != null && !bibliographyLink.getPrimeID().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
						ontoChemExpVocabulary.getDataPropertyhasPrimeID(), bibliographyLink.getPrimeID(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	/**
	 * Links the equipment to the experiment.
	 * 
	 */
	private void linkBibliographyLinkToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasBibliographyLink(),
					"Experiment" + UNDERSCORE + experimentInstanceId,
					"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount));
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between a copyrigh and an experiment conducted using it.");
		}
	}

	public void writeValue() {
		if (bibliographyLink.getValue() != null && !bibliographyLink.getValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"BibliographyLink" + UNDERSCORE + (bibliographyLinkID + bibliographyLinkCount),
						ontoChemExpVocabulary.getDataPropertyhasValue(), bibliographyLink.getValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
//     		IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//     		iABoxManagement.addProperty("BibliographyLink"+UNDERSCORE+(bibliographyLinkID+bibliographyLinkCount), 
//     				dataPropertyIRI, bibliographyLink.getValue(), STRING);
		}
	}

	public void setUP() {
		bibliographyLinkList.add(bibliographyLink);
		bibliographyLink = new BibliographyLink();
		experiment.setBibliographyLink(bibliographyLinkList);
		bibliographyLinkList = new ArrayList<BibliographyLink>();
	}
}
