package com.cmclinnovations.ontochemexp.model.owl;

import org.slf4j.Logger;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Property;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * This is a utility class that process all the calls to read Property 
 * data from an in-memory temporary storage to pass them to the
 * corresponding PrIMe to OWL conversion methods.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
public class DimensionalQuantityWriter extends PrimeConverter {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(DimensionalQuantityWriter.class);
	private long id;
	private int count;
	private Property property;
	private String subject;
	private boolean inDataGroup = false;
	
	public DimensionalQuantityWriter(long id, int count, String subject, Property property) {
		this.id = id;
		this.count = count;
		this.subject = subject;
		this.property = property;
	}
	
	public DimensionalQuantityWriter(long id, int count, boolean inDataGroup, String subject, Property property) {
		this.id = id;
		this.count = count;
		this.inDataGroup = inDataGroup;
		this.subject = subject;
		this.property = property;
	}
	
	
	public void createDimensionalQuantityInOWL() throws OntoChemExpException {
		count += 1;
		
		String name = mapName(property.getPropertyName());
		String object = name + UNDERSCORE + id + UNDERSCORE + count;
		
		try {
			if (property.getPropertyName() != null && !property.getPropertyName().trim().isEmpty()) {
				iABoxManagement.createIndividual(ontoChemExpVocabulary.getClassProperty(), object);
			}
			
			if (property.getPropertyLabel() != null && !property.getPropertyLabel().trim().isEmpty()) {
				iABoxManagement.addProperty(object, ontoChemExpVocabulary.getDataPropertyhasLabel(), property.getPropertyLabel(), STRING);
			}

			if (property.getPropertyUnits() != null && !property.getPropertyUnits().trim().isEmpty()) {
				iABoxManagement.addProperty(object, ontoChemExpVocabulary.getDataPropertyhasUnits(), property.getPropertyUnits(), STRING);
			}

			if (property.getPropertyDescription() != null && !property.getPropertyDescription().trim().isEmpty()) {
				iABoxManagement.addProperty(object, ontoChemExpVocabulary.getDataPropertyhasDescription(),
						property.getPropertyDescription().replace("\r", " ").replace("\n", ""), STRING);
			}
			
			if (property.getPropertyDerivedPropertyExists() != null && !property.getPropertyDerivedPropertyExists().trim().isEmpty()) {
				iABoxManagement.addProperty(object, ontoChemExpVocabulary.getDataPropertyhasDerivedPropertyExists(),
						property.getPropertyDerivedPropertyExists(), STRING);
			}
			
			if (property.getSourceType() != null && !property.getSourceType().trim().isEmpty()) {
				iABoxManagement.addProperty(object, ontoChemExpVocabulary.getDataPropertyhasSourceType(), property.getSourceType(), STRING);
			}
			
			// Property (DimensionalQuantity) ID is non-handled in Apparatus and CommonProperties
			if (inDataGroup) {
				if (property.getPropertyId() != null && !property.getPropertyId().trim().isEmpty()) {
					xDQMap.put(property.getPropertyId().toLowerCase(), object);
				}
			}
			
			iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasProperty(),
					subject, object);
			
//			add rdfs:label
			
//			deal with id
//			deal with reference
			
//			report if the name is not found
			
//			concentration, composition, initial composition, name not found
			
		} catch (ABoxManagementException e) {
			logger.error("An individual of Property could not be created.");
		}
	}
	
	public String mapName(String key) throws OntoChemExpException {
		return dimensionalQuantityMapping.getOntoChemExpDimensionalQuantityEntry(key);
	}
}
