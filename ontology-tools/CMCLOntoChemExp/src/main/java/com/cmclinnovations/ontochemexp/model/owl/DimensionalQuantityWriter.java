package com.cmclinnovations.ontochemexp.model.owl;

import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.xml.sax.SAXException;

import com.cmclinnovations.ontochemexp.model.converter.prime.PrimeConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Property;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;
import com.cmclinnovations.ontology.model.exception.ABoxManagementException;

/**
 * This is a utility class that process all the calls to read Property data from
 * an in-memory temporary storage to pass them to the corresponding PrIMe to OWL
 * conversion methods.
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
		if (property.getPropertyName().toLowerCase().equalsIgnoreCase(primeVocabulary.getElemUncertainty().toLowerCase())) {
			// this function is for supporting ReSpecTh.hu data
			// here we check if the property is actually Uncertainty, different treatment needed for different parent element (i.e., Apparatus, CommonProperties, and DataGroup)
			// we only create Uncertainty OWL individual here, the linking is done at the end of each parent element
			// four types of uncertainties: (1) uncertainty with value node reference to another property which is not in dataGroup; 
			// (2) uncertainty in a data group that with its own x and reference to another x in the dataGroup, it should be noted that the reference is possible to be either the name of the property or the id
			// (3) uncertainty in commonProperties, and refers to concentration/composition, then a child node speciesLink is mandatory, and should only be used for locating the uncertainty, but not added to OWL
			// (4) uncertainty in dataGroup, and refers to concentration/composition, again speciesLink should be used to locate the link in the OWL, and shouldn't be added to the OWL
			
		} else if (property.getPropertyName().toLowerCase().equalsIgnoreCase("evaluated standard deviation")) {
			// evaluated standard deviation: (1) inside dataGroup, with id, essentially type (2) of uncertainty; 
			// (2) inside commonProperties, reference to composition, also pretty much a type of uncertainty
			// (3) inside commonProperties, reference to other DQ, essentially a type of uncertainty
			
		} else {
			count += 1;
			String name = mapName(property.getPropertyName());
			currentDQInstance = name + UNDERSCORE + id + UNDERSCORE + count;
			
			try {
				if (property.getPropertyName() != null && !property.getPropertyName().trim().isEmpty()) {
					iABoxManagement.createIndividual(name, currentDQInstance);
					iABoxManagement.addProperty(currentDQInstance, OWLRDFVocabulary.RDFS_LABEL.getIRI(), property.getPropertyName(), STRING);
				}
				
				if (property.getPropertyLabel() != null && !property.getPropertyLabel().trim().isEmpty()) {
					iABoxManagement.addProperty(currentDQInstance, ontoChemExpVocabulary.getDataPropertyhasLabel(), property.getPropertyLabel(), STRING);
				}
	
				if (property.getPropertyUnits() != null && !property.getPropertyUnits().trim().isEmpty()) {
					iABoxManagement.addProperty(currentDQInstance, ontoChemExpVocabulary.getDataPropertyhasUnits(), property.getPropertyUnits(), STRING);
				}
	
				if (property.getPropertyDescription() != null && !property.getPropertyDescription().trim().isEmpty()) {
					iABoxManagement.addProperty(currentDQInstance, ontoChemExpVocabulary.getDataPropertyhasDescription(),
							property.getPropertyDescription().replace("\r", " ").replace("\n", ""), STRING);
				}
				
				if (property.getPropertyDerivedPropertyExists() != null && !property.getPropertyDerivedPropertyExists().trim().isEmpty()) {
					iABoxManagement.addProperty(currentDQInstance, ontoChemExpVocabulary.getDataPropertyhasDerivedPropertyExists(),
							property.getPropertyDerivedPropertyExists(), STRING);
				}
				
				if (property.getSourceType() != null && !property.getSourceType().trim().isEmpty()) {
					iABoxManagement.addProperty(currentDQInstance, ontoChemExpVocabulary.getDataPropertyhasSourceType(), property.getSourceType(), STRING);
				}
				
				// Property (DimensionalQuantity) ID is non-handled in Apparatus and CommonProperties
				if (inDataGroup) {
					if (property.getPropertyId() != null && !property.getPropertyId().trim().isEmpty()) {
						xDQMap.put(property.getPropertyId().toLowerCase(), currentDQInstance);
					}
				}
				
				iABoxManagement.addObjectProperty(ontoChemExpVocabulary.getObjPropertyhasProperty(),
						subject, currentDQInstance);
				} catch (ABoxManagementException e) {
					logger.error("An individual of Property could not be created.");
				}
			}
//			add rdfs:label - done
			
//			deal with id - done
//			deal with reference - ReSpecTh specifically
//			deal with uncertainty - ReSpecTh specifically
//			deal with estimated standard deviation - ReSpecTh specifically
			
//			report if the name is not found - ReSpecTh specifically
			
//			concentration, composition, initial composition, name not found - ReSpecTh specifically
	}

	public String mapName(String key) throws OntoChemExpException {
		return dimensionalQuantityMapping.getOntoChemExpDimensionalQuantityEntry(key);
	}
}
