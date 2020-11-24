package com.cmclinnovations.ontochemexp.model.parser.prime.data_group;

import static com.cmclinnovations.ontochemexp.model.owl.IDataGroupWriter.logger;

import java.util.ArrayList;

import org.apache.commons.lang3.text.WordUtils;
import org.xml.sax.Attributes;
import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroupProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.data_point.DataGroupDataPointX;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.data_group.DataGroup;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

public class DataGroupPropertyParser extends PrimeConverter implements IDataGroupPropertyParser {
	public void parse(String qName, Attributes attributes) {
		// Itself
		parseProperty(qName, attributes);
		parsePropertyName(qName, attributes);
		parsePropertyID(qName, attributes);
		parsePropertyLabel(qName, attributes);
		parsePropertyUnits(qName, attributes);
		parsePropertyDescription(qName, attributes);
		parsePropertyDerivedPropertyExists(qName, attributes);
	}
	
	private void parseProperty(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			dataGroupPropertyParseStatus.setProperty(true);
		}
	}

	private void parsePropertyName(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			String name = attributes.getValue(primeVocabulary.getAttribName());
			if (name != null) {
				dataGroupProperty.setPropertyName(name);
				dataGroupPropertyParseStatus.setName(true);
				dataGroupPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyID(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			String id = attributes.getValue(primeVocabulary.getAttribID());
			if (id != null) {
				dataGroupProperty.setPropertyId(id);
				dataGroupPropertyParseStatus.setID(true);
				dataGroupPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyLabel(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			String label = attributes.getValue(primeVocabulary.getAttribLabel());
			if (label != null) {
				dataGroupProperty.setPropertyLabel(label);
				dataGroupPropertyParseStatus.setLabel(true);
				dataGroupPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyUnits(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			String units = attributes.getValue(primeVocabulary.getAttribUnits());
			if (units != null) {
				dataGroupProperty.setPropertyUnits(units);
				dataGroupPropertyParseStatus.setUnits(true);
				dataGroupPropertyParseStatus.setProperty(true);
			}
		}
	}

	private void parsePropertyDescription(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			String description = attributes.getValue(primeVocabulary.getAttribDescription());
			if (description != null) {
				dataGroupProperty.setPropertyDescription(description);
				dataGroupPropertyParseStatus.setDescription(true);
				dataGroupPropertyParseStatus.setProperty(true);
			}
		}
	}
	
	private void parsePropertyDerivedPropertyExists(String qName, Attributes attributes) {
		if (qName.equalsIgnoreCase(primeVocabulary.getElemProperty()) && inDataGroup) {
			String derivedPropertyExists = attributes.getValue(primeVocabulary.getAttribDerivedPropertyExists());
			if (derivedPropertyExists != null) {
				dataGroupProperty.setPropertyDerivedPropertyExists(derivedPropertyExists);
				dataGroupPropertyParseStatus.setDerivedPropertyExists(true);
				dataGroupPropertyParseStatus.setProperty(true);
			}
		}
	}

	/**
	
	
	String description;

	
	
	public void parse(String qName, Attributes attributes) {
		// Calls the method that checks the appearance of
		// the dataGroup tag
		parseDescription(qName, attributes);
		// parseDescription(qName, attributes);
	}
	
	

	/**
	 * Checks the appearance of the dataGroup tag
	 * 
	 * @param qName
	 * @param attributes
	 */
	/**
	private void parseDescription(String qName, Attributes attributes) {
//		if (attributes.getValue("name") != null)
//		System.out.println("+++1####++++++#######++++++name:" + attributes.getValue("name"));
		if (attributes.getValue(primeVocabulary.getDataGroupPropertyDescription()) == null && attributes.getValue(primeVocabulary.getUnit()) != null
				&& attributes.getValue("id") != null && attributes.getValue("name") != null && attributes.getValue("label") != null) {
			dataGroupProperty.setId(attributes.getValue("id"));
			dataGroupProperty.setName(attributes.getValue("name"));
			dataGroupProperty.setLabel(attributes.getValue("label"));
			dataGroupProperty.setUnits(attributes.getValue("units"));
		}
		
		if (dataGroupPropertyParseStatus.isProperty()
				&& attributes.getValue(primeVocabulary.getDataGroupPropertyDescription()) != null) {

			description = WordUtils.capitalize(attributes.getValue(primeVocabulary.getDataGroupPropertyDescription()))
					.replace(SPACE, "");
			String lable = WordUtils.capitalize(attributes.getValue("label")).replace("X_", "");
			String name = WordUtils.capitalize(attributes.getValue("name")).replace("X_", "");
			// System.out.println("+++1####++++++#######++++++description:"
			// + description);
			// dataGroupProperty.setDescription(description);
			
			// System.out.println("+++1####++++++#######++++++name:" + name);
			
			if (description != null) {
				dataGroupProperty.setDescription(description);
				dataGroupProperty.setDescriptionOrig(attributes.getValue("description"));
//				System.out.println("+++1####++++++#######++++++" + dataGroupProperty.getDescriptionOrig());
//				System.out.println("+++1####++++++#######++++++" + dataGroup.getLabel());
		//		try {
				//	iABoxManagement.addProperty(dataGroup.getLabel()+"_1",  "hasDescription", dataGroupProperty.getDescriptionOrig(),STRING);
					dataGroupProperty.setDescriptionOrig(null);
			//	} catch (ABoxManagementException e) {
					// TODO Auto-generated catch block
			//		e.printStackTrace();
			//	}
 
//				System.out.println("+++2####++++++#######++++++dataGroupProperty.getDescription();:"
//						+ dataGroupProperty.getDescription());
			//	System.out.println("+++2####++++++#######++++++x.getValue():"
		//				+ x.getValue());
//


				// dataGroupPropertyUncertainty = new DataGroupPropertyUncertainty();

				// dataGroupPropertyUncertainty = new DataGroupPropertyUncertainty();
				// dataGroupPropertyUncertainty = new DataGroupPropertyUncertainty();
				// System.out.println("+++#####++++++#######++++++dataGroupPropertyUncertainty.getBound().:"
				// + dataGroupPropertyUncertainty.getBound());
				// x.setUncertaintyName(dataGroupProperty.getDescription());
				// x.setUncertaintyName(description);

			}

			if (dataGroupProperty.getDescription().contains("Speed")) {
				createLaminarFlameSpeed();
			}
			// System.out.println("+++####++++++#######++++++attributes.getValue(primeVocabulary.getUnit()):"
			// + attributes.getValue(primeVocabulary.getUnit()));
			if (attributes.getValue(primeVocabulary.getUnit()) != null)
				dataGroupProperty.setUnits(attributes.getValue(primeVocabulary.getUnit()));
			dataGroupProperty.setName(name);
			
//			 System.out.println("+++####++++++#######++++++dataGroupProperty.getName():"
//			 + dataGroupProperty.getName());
			
			// x.setNameProperty(dataGroupProperty.getName());
			if (attributes.getValue(primeVocabulary.getUnit()) != null)
				dataGroupProperty
						.setLabel(lable + WordUtils.capitalize(dataGroupProperty.getUnits()).replace(SPACE, ""));

			if (dataGroupProperty.getDescription().equalsIgnoreCase("IgnitionTimeDelay")
					|| dataGroupProperty.getDescription().equalsIgnoreCase("IgnitionDelay")) {

			//	createIgnitionDelayObjectProperties();
				dataGroup.setLabel("IgnitionDelay");

			} else if (dataGroupProperty.getLabel() != null && dataGroupProperty.getLabel().contains("]/[")) {
				// System.out.println("+++#####+++++++++++++++++++dataGroup.getLabel().:" +
				// dataGroupProperty.getLabel());
			//	createCharacteristicTime();
			} else if (dataGroupProperty.getDescription().contains("Concentration")) {
			//	createSpeciesProfile();
			} else if (dataGroup.getLabel() != null && dataGroup.getLabel().equalsIgnoreCase("TProfile")) {
			//	createTemperatureProfile();
			} else if (dataGroup.getLabel() != null && dataGroup.getLabel().contains("Max")
					|| (dataGroup.getLabel() != null && dataGroup.getLabel().equalsIgnoreCase("NO"))) {
			//	createSpeciesMaximum();
			} else if (dataGroup.getLabel() != null && dataGroup.getLabel().contains("Calibration")) {
			//	createCalibration();
			} else if (dataGroup.getLabel() != null && dataGroup.getLabel() != "") {
			//	createAnythingElse();
			}

			// dataGroup.setUnits(attributes.getValue(primeVocabulary.getUnit()));
			dataGroupProperty.setId(attributes.getValue(primeVocabulary.getId()));
			// id.setValue(attributes.getValue(primeVocabulary.getId()));

			if (dataGroupProperty.getId() != null) {
				dataGroupPropertyParseStatus.setDescription(false);

//				dataGroupPropertyList.add(dataGroupProperty);				
// 				dataGroup.setProperty(dataGroupPropertyList);
 				
			//	System.out.println("+++####++++++#######++++++attributes:"+ dataGroupProperty.getDescription());
			//	dataGroupProperty = new DataGroupProperty(); 
				// dataGroup = new DataGroup();
				// idList.add(id);
				// id = new Id();
				// dataPoint.setId(idList);
			}

			if (dataGroup.getLabel() != null && dataGroup.getLabel().contains("Calibration")) {
				createCalibration();
			}

			dataGroupPropertyParseStatus.setProperty(true);
		}
		// System.out.println("+++#####+++++++++++++++++++dataGroupProperty.getDescription():"
		// + dataGroupProperty.getDescription());
		if (dataGroupPropertyUncertaintyParseStatus.isUncertainty()) {

//			 System.out.println("+++###Check##+++++++++++++++++++dataGroupProperty.getDescription():"
//			 + dataGroupProperty.getDescription());
			 
			 
			dataGroupPropertyUncertainty.setBound(attributes.getValue(primeVocabulary.getUncertaintyBound()));
			dataGroupPropertyUncertainty.setKind(attributes.getValue(primeVocabulary.getUncertaintyKind()));
			dataGroupPropertyUncertainty
					.setTransformation(attributes.getValue(primeVocabulary.getUncertaintyTransformation()));
			dataGroupPropertyUncertainty.setPropertyDescription(dataGroupProperty.getDescription());

			// dataGroupPropertyUncertainty.setBound(attributes.getValue("bound"));
			// dataGroupPropertyUncertainty.setKind(attributes.getValue("kind"));
			// dataGroupPropertyUncertainty.setValue(attributes.getValue("value"));

			// dataGroupProperty = new DataGroupProperty();

			// System.out.println("+++#####+++++++++++++++++++" +
			// attributes.getValue(primeVocabulary.getUncertaintyBound()));
			if (attributes.getValue(primeVocabulary.getUncertaintyBound()) != null) {
				x.setBound(attributes.getValue(primeVocabulary.getUncertaintyBound()));
				x.setKind(attributes.getValue(primeVocabulary.getUncertaintyKind()));
				x.setTransformation(attributes.getValue(primeVocabulary.getUncertaintyTransformation()));

			}

			// System.out.println("+++''''++++++#######++++++x.getBound:" + x.getBound());
			// System.out.println("+++''''++++++#######++++++description:" + description);
			if (x.getBound() != null) {
				// System.out.println("+++#####++++++#######++++++dataGroupProperty.getDescription():"
				// + dataGroupProperty.getDescription());
				x.setUncertaintyName(description);
				// x.setBound(null);
			}
			// System.out.println("+++#####++++++#######++++++x.getUncertaintyName():" +
			// x.getUncertaintyName());
			// dataGroupPropertyUncertaintyParseStatus.setUncertainty(true);

			// System.out.println("~~attributes.getValue(\"uncertainty\")//////////////////"
			// + primeVocabulary.getUncertaintyBound());

//			 System.out.println("~~attributes.getValue(\"uncertainty\")//////////////////"
//			 + dataGroupPropertyUncertainty.getBound());
//			 System.out.println("~~x.getBound()//////////////////"
//			 + x.getBound());
//			 System.out.println("~~x.uncertaintyName()//////////////////"
//			 + x.getUncertaintyName());
//			 System.out.println("~~dataGroup.label()//////////////////"
//			 + dataGroup.getLabel()); 
//			 try {
//				iABoxManagement.addProperty(dataGroup.getLabel()+"_1", "hasUncertaintyBound"+"For"+x.getUncertaintyName(), x.getBound(), STRING);
//				iABoxManagement.addProperty(dataGroup.getLabel()+"_1", "hasUncertaintyKind"+"For"+x.getUncertaintyName(), x.getKind(), STRING);
//				iABoxManagement.addProperty(dataGroup.getLabel()+"_1", "hasUncertaintyTransformation"+"For"+x.getUncertaintyName(), x.getTransformation(), FLOAT);
//			//	iABoxManagement.addProperty(dataGroup.getLabel()+"_1", "hasUncertainty"+"For"+x.getUncertaintyName(), x.getUncertainty(), FLOAT);
//			} catch (ABoxManagementException e) {
//				// TODO Auto-generated catch block
//				e.printStackTrace();
//			}
		}

	}

	private void createIgnitionDelayObjectProperties() {
		try {
			iABoxManagement.addObjectProperty("hasIgnitionDelaySpecification", "IgnitionDelay",
					"IgnitionDelaySpecification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	private void createSpeciesProfile() {
		try {
			iABoxManagement.addObjectProperty("hasSpeciesProfileSpecification", "SpeciesProfile",
					dataGroupProperty.getDescription().replaceAll("Concentration", "") + "SpeciesProfileSpecification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	private void createLaminarFlameSpeed() {
		try {
			iABoxManagement.addObjectProperty("hasLaminarFlameSpeedSpecification", "LaminarFlameSpeed",
					"LaminarFlameSpeedSpecification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	private void createCharacteristicTime() {
		try {
			iABoxManagement.addObjectProperty("hasCharacteristicTimeSpecification", "CharacteristicTime",
					"CharacteristicTimeSpecification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	private void createTemperatureProfile() {
		try {
			iABoxManagement.addObjectProperty("hasTemperatureProfileSpecification", "TemperatureProfile",
					"TemperatureProfileSpecification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	private void createSpeciesMaximum() {
		try {
			iABoxManagement.addObjectProperty("hasSpeciesMaximumSpecification", "SpeciesMaximum",
					"SpeciesMaximumSpecification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}

	}

	private void createCalibration() {
		try {
			iABoxManagement.addObjectProperty("has" + dataGroup.getLabel() + "Specification", dataGroup.getLabel(),
					dataGroup.getLabel() + "Specification");
		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}

	private void createAnythingElse() {
		try {
			iABoxManagement.addObjectProperty(
					"has" + dataGroup.getLabel().replace("Case", "").replaceAll("[0-9]", "").replaceAll("_", "")
							+ "Specification",
					dataGroup.getLabel(),
					dataGroup.getLabel().replaceAll("[0-9]", "").replaceAll("_", "") + "Specification");

			// System.out.println("+++#####++++++#######++++++dataGroup.getLabel() +  \"Specification\":" + dataGroup.getLabel().replace("Case",
			// "").replaceAll("[0-9]", "") + "Specification");

		} catch (ABoxManagementException e) {
			logger.error("An individual of dataGroup could not be created.");
		}
	}
*/	
}
