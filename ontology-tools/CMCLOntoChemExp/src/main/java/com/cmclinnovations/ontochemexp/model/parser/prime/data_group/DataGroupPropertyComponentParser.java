package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import static com.cmclinnovations.ontochemexp.model.owl.IDataGroupWriter.logger;

import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class DataGroupPropertyComponentParser extends PrimeConverter implements IDataGroupPropertyComponentParser {
	public void parse(String qName, Attributes attributes) {
		// Element Component
		parseDataGroupPropertyComponent(qName, attributes);
		parseDataGroupPropertyComponentSpeciesLink(qName, attributes);
	}
	
	private void parseDataGroupPropertyComponent(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemComponent()) && inDataGroup) {
			dataGroupPropertyComponentParseStatus.setComponent(true);
		}
	}
	
	private void parseDataGroupPropertyComponentSpeciesLink(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemSpeciesLink()) && inDataGroup && dataGroupPropertyComponentParseStatus.isComponent()) {
			dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(true);
			
			String preferredKey = attributes.getValue(primeVocabulary.getAttribPreferredKey());
			if (preferredKey != null && !preferredKey.isEmpty()) {
				dataGroupPropertyComponentSpeciesLink.setSpeciesLinkPreferredKey(preferredKey);
				dataGroupPropertyComponentSpeciesLinkParseStatus.setPreferredKey(true);
				dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(true);
			}
			
			String primeID = attributes.getValue(primeVocabulary.getAttribPrimeID());
			if (primeID != null) {
				dataGroupPropertyComponentSpeciesLink.setSpeciesLinkPrimeID(primeID);
				dataGroupPropertyComponentSpeciesLinkParseStatus.setPrimeID(true);
				dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(true);
			}
			
			checkComponentValue();
			createPropertyComponentSpeciesLink();
			linkPropertyComponentSpeciesLinkToComponent();
		}
	}
	
	private void checkComponentValue() {
		String value = multiLineValue.toString().trim();
		if ((value == null) || (value.isEmpty())) {
			dataGroupPropertyComponentParseStatus.setComponentValue(false);
		}
	}
	
	private void createPropertyComponentSpeciesLink() {
		try {
			iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassSpeciesLink(),
					"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount);
			
			if (dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey() != null 
					&& !dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasPreferredKey(),
						dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPreferredKey(), STRING);
			}
			
//			if (dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID() != null 
//					&& !dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty(
//						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(),
//						dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID(), STRING);
//			}
		} catch (ABoxManagementException e) {
			logger.error("An individual of PropertyComponentSpeciesLink could not be created.");
		}
	}

	private void linkPropertyComponentSpeciesLinkToComponent() {
		try {
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasSpeciesLink(),
					"Component" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount,
					"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount);
		} catch (ABoxManagementException e) {
			logger.error("A link could not be established between the dataGroup propertyComponent and its speciesLink.");
		}
	}
}
