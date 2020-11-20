package com.cmclinnovations.ontochemexp.model.owl;

import static com.cmclinnovations.ontochemexp.model.owl.IBibliographyLinkWriter.logger;

import java.util.ArrayList;

import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Value;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class AdditionalDataItemWriter extends PrimeConverter implements IAdditionalDataItemWriter {
	public void writer(String qName) throws SAXException {
		readAdditionalDataItem(qName);
	}
	
	private void readAdditionalDataItem(String qName) throws SAXException {
		if (additionalDataItemParseStatus.isAdditionalDataItem()) {
			createAdditionalDataItem();
			addAllAttributes();
			linkAdditionalDataItemToExperiment();
		}
	}

	private void createAdditionalDataItem() {
		additionalDataItemCount += 1;

		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassAdditionalDataItem(),
					"AdditionalDataItem" + UNDERSCORE + (additionalDataItemID + additionalDataItemCount));			
			
		} catch (ABoxManagementException e) {
			logger.error("An individual of additionalDataItem could not be created.");
		}
		
	}

	private void addAllAttributes() {
		if (additionalDataItem.getMIME() != null && !additionalDataItem.getMIME().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"AdditionalDataItem" + UNDERSCORE + (additionalDataItemID + additionalDataItemCount),
						ontoChemExpVocabulary.getDataPropertyhasMIME(), additionalDataItem.getMIME(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		if (additionalDataItem.getItemType() != null && !additionalDataItem.getItemType().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"AdditionalDataItem" + UNDERSCORE + (additionalDataItemID + additionalDataItemCount),
						ontoChemExpVocabulary.getDataPropertyhasItemType(), additionalDataItem.getItemType(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}

		if (additionalDataItem.getDescription() != null && !additionalDataItem.getDescription().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"AdditionalDataItem" + UNDERSCORE + (additionalDataItemID + additionalDataItemCount),
						ontoChemExpVocabulary.getDataPropertyhasDescription(), additionalDataItem.getDescription(),
						STRING);
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
	private void linkAdditionalDataItemToExperiment() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasAdditionalDataItem(),
					"Experiment" + UNDERSCORE + experimentInstanceId,
					"AdditionalDataItem" + UNDERSCORE + (additionalDataItemID + additionalDataItemCount));
		} catch (ABoxManagementException e) {
			logger.error(
					"A link could not be established between an additionalDataItem and an experiment conducted using it.");
		}
	}
	
	public void writeValue() {
		if (additionalDataItem.getValue() != null && !additionalDataItem.getValue().trim().isEmpty()) {
			try {
				iABoxManagement.addProperty(
						"AdditionalDataItem" + UNDERSCORE + (additionalDataItemID + additionalDataItemCount),
						ontoChemExpVocabulary.getDataPropertyhasValue(), additionalDataItem.getValue(), STRING);
				
//				IRI dataPropertyIRI = IRI.create(RDFS_URL.concat(RDFS_LABEL));
//				iABoxManagement.addProperty("AdditionalDataItem"+UNDERSCORE+(additionalDataItemID+additionalDataItemCount), 
//	     				dataPropertyIRI, additionalDataItem.getValue(), STRING);
			} catch (ABoxManagementException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	public void setUP() {
		additionalDataItemList.add(additionalDataItem);
		additionalDataItem = new AdditionalDataItem();
		experiment.setAdditionalDataItem(additionalDataItemList);
		additionalDataItemList = new ArrayList<AdditionalDataItem>();
	}
}
