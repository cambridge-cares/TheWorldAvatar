package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import static com.cmclinnovations.ontochemexp.model.owl.IDataGroupWriter.logger;

import java.util.ArrayList;
import java.util.List;

import org.xml.sax.Attributes;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontochemexp.model.utils.PrimeConverterUtils;
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
			
			String cas = attributes.getValue(primeVocabulary.getAttribCAS());
			if (cas != null) {
				dataGroupPropertyComponentSpeciesLink.setCas(cas);
				dataGroupPropertyComponentSpeciesLinkParseStatus.setCas(true);
				dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(true);
			}
			
			String inchi = attributes.getValue(primeVocabulary.getAttribInChI());
			if (inchi != null) {
				dataGroupPropertyComponentSpeciesLink.setInchi(inchi);
				dataGroupPropertyComponentSpeciesLinkParseStatus.setInchi(true);
				dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(true);
			}
			
			String smiles = attributes.getValue(primeVocabulary.getAttribSMILES());
			if (smiles != null) {
				dataGroupPropertyComponentSpeciesLink.setSmiles(smiles);
				dataGroupPropertyComponentSpeciesLinkParseStatus.setSmiles(true);
				dataGroupPropertyComponentParseStatus.setComponentSpeciesLink(true);
			}
			
			String chemName = attributes.getValue(primeVocabulary.getAttribChemName());
			if (chemName != null) {
				dataGroupPropertyComponentSpeciesLink.setChemName(chemName);
				dataGroupPropertyComponentSpeciesLinkParseStatus.setChemName(true);
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
			
			if (dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID() != null 
					&& !dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID().trim().isEmpty()) {
//				iABoxManagement.addProperty(
//						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
//						ontoChemExpVocabulary.getDataPropertyhasPrimeID(),
//						dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID(), STRING);
				
				String uniqueSpeciesIRI;
				try {
					uniqueSpeciesIRI = PrimeConverterUtils.retrieveSpeciesIRI(ontoChemExpKB.getOntoSpeciesUniqueSpeciesIRIKBAboxIRI()
							.concat(dataGroupPropertyComponentSpeciesLink.getSpeciesLinkPrimeID()));
					if (uniqueSpeciesIRI.trim() != null && !uniqueSpeciesIRI.trim().isEmpty()) {
						iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasUniqueSpecies(),
								"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
								uniqueSpeciesIRI);
					}
				} catch (OntoChemExpException e) {
					logger.error("The uniqueSpeciesIRI could not be retrieved.");
					e.printStackTrace();
				}
			}
			
			if (dataGroupPropertyComponentSpeciesLink.getCas() != null 
					&& !dataGroupPropertyComponentSpeciesLink.getCas().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasCAS(),
						dataGroupPropertyComponentSpeciesLink.getCas(), STRING);
			}
			
			if (dataGroupPropertyComponentSpeciesLink.getInchi() != null 
					&& !dataGroupPropertyComponentSpeciesLink.getInchi().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasInChI(),
						dataGroupPropertyComponentSpeciesLink.getInchi(), STRING);
			}
			
			if (dataGroupPropertyComponentSpeciesLink.getSmiles() != null 
					&& !dataGroupPropertyComponentSpeciesLink.getSmiles().trim().isEmpty()) {
				iABoxManagement.addProperty(
						"SpeciesLink" + UNDERSCORE + (dataGroupID + dataGroupCount) + UNDERSCORE + dataGroupPropertyCount + UNDERSCORE + dataGroupPropertyCount,
						ontoChemExpVocabulary.getDataPropertyhasSMILES(),
						dataGroupPropertyComponentSpeciesLink.getSmiles(), STRING);
			}
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
