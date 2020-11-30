package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Apparatus;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.ApparatusProperty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.apparatus.Mode;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.copyright.Copyright;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Uncertainty;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.property.Value;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class ApparatusQuery extends OwlConverter implements IApparatusQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);

	public void query() throws OntoChemExpException {
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), ontoChemExpKB.getOntoChemExpKbTBoxIri(),
				ontoChemExpVocabulary.getClassApparatus());
		Collections.sort(queryResult);
		ArrayList<String> apparatusInstance = queryResult;
		queryResult = new ArrayList<>();
		if (apparatusInstance.size() > 0 && apparatusInstance.size() < 2) {
			queryAllElement(apparatusInstance.get(0));
		} else if (apparatusInstance.size() > 1) {
			logger.error("There should not be more than one instance of apparatus in an experiment.");
		} else {
			logger.error("No instance of apparatus found in an experiment.");
		}
		experiment.setApparatus(apparatus);
		apparatus = new Apparatus();
	}

	private void queryAllElement(String apparatusInstance) throws OntoChemExpException {
		queryKind(apparatusInstance);
		apparatus.setKind(kind);
		queryMode(apparatusInstance);
		apparatus.setMode(modeList);
		queryApparatusProperty(apparatusInstance);
		apparatus.setProperty(apparatusPropertyList);
	}

	private void queryKind(String apparatusInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), apparatusInstance,
				ontoChemExpVocabulary.getObjPropertyhasKind());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> kindInstance = queryResult;
		queryResult = new ArrayList<String>();

		String kindValue = readDataPropertyValue(kindInstance.get(0));
		if (kindValue != null && !kindValue.isEmpty()) {
			kind.setValue(kindValue);
		}
	}

	private void queryMode(String apparatusInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), apparatusInstance,
				ontoChemExpVocabulary.getObjPropertyhasMode());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> modeInstances = queryResult;
		queryResult = new ArrayList<String>();

		for (String modeInstance : modeInstances) {
			String modeValue = readDataPropertyValue(modeInstance);
			if (modeValue != null && !modeValue.isEmpty()) {
				mode.setValue(modeValue);
			}
			modeList.add(mode);
			mode = new Mode();
		}
	}

	private void queryApparatusProperty(String apparatusInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), apparatusInstance,
				ontoChemExpVocabulary.getObjPropertyhasProperty());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> propertyInstances = queryResult;
		queryResult = new ArrayList<String>();

		for (String propertyInstance : propertyInstances) {
			queryPropertyAllAttributes(propertyInstance);
			queryPropertyValue(propertyInstance);
			queryPropertyUncertainty(propertyInstance);
			apparatusPropertyList.add(apparatusProperty);
			apparatusProperty = new ApparatusProperty();
		}
	}

	private void queryPropertyAllAttributes(String propertyInstance) throws OntoChemExpException {
		String name = readDataPropertyName(propertyInstance);
		if (name != null && !name.isEmpty()) {
			apparatusProperty.setPropertyName(name);
		}

		String id = readDataPropertyID(propertyInstance);
		if (id != null && !id.isEmpty()) {
			apparatusProperty.setPropertyId(id);
		}

		String label = readDataPropertyLabel(propertyInstance);
		if (label != null && !label.isEmpty()) {
			apparatusProperty.setPropertyLabel(label);
		}

		String units = readDataPropertyUnits(propertyInstance);
		if (units != null && !units.isEmpty()) {
			apparatusProperty.setPropertyUnits(units);
		}

		String description = readDataPropertyDescription(propertyInstance);
		if (description != null && !description.isEmpty()) {
			apparatusProperty.setPropertyDescription(description);
		}
	}

	private void queryPropertyValue(String propertyInstance) throws OntoChemExpException {
		ArrayList<String> valueInstance = readObjPropertyValue(propertyInstance);

		if (valueInstance.size() > 0 && valueInstance.size() < 2) {
			String propertyValue = readDataPropertyValue(valueInstance.get(0));
			if (propertyValue != null && !propertyValue.isEmpty()) {
				apparatusPropertyValue.setValueValue(propertyValue);
				apparatusProperty.setPropertyValue(apparatusPropertyValue);
				apparatusPropertyValue = new Value();
			}
		} else if (valueInstance.size() > 1) {
			logger.error("There should not be more than one instance of property value of apparatusProperty.");
		}
		
	}
	
	private void queryPropertyUncertainty(String propertyInstance) throws OntoChemExpException {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON),
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), propertyInstance,
				ontoChemExpVocabulary.getObjPropertyhasUncertainty());
		performMultilineAnswerQuery(q, 1);
		Collections.sort(queryResult);
		ArrayList<String> uncertaintyInstance = queryResult;
		queryResult = new ArrayList<String>();

		if (uncertaintyInstance.size() > 0 && uncertaintyInstance.size() < 2) {
			queryPropertyUncertaintyAllAttributes(uncertaintyInstance.get(0));
			quaryPropertyUncertaintyValue(uncertaintyInstance.get(0));
			apparatusProperty.setPropertyUncertainty(apparatusPropertyUncertainty);
			apparatusPropertyUncertainty = new Uncertainty();
		} else if (uncertaintyInstance.size() > 1) {
			logger.error("There should not be more than one instance of property uncertainty of apparatusProperty.");
		}
	}
	
	private void queryPropertyUncertaintyAllAttributes(String uncertaintyInstance) {
		String bound = readDataPropertyBound(uncertaintyInstance);
		if (bound != null && !bound.isEmpty()) {
			apparatusPropertyUncertainty.setUncertaintyBound(bound);
		}
		String kind = readDataPropertyKind(uncertaintyInstance);
		if (kind != null && !kind.isEmpty()) {
			apparatusPropertyUncertainty.setUncertaintyKind(kind);
		}
		String transformation = readDataPropertyTransformation(uncertaintyInstance);
		if (transformation != null && !transformation.isEmpty()) {
			apparatusPropertyUncertainty.setUncertaintyTransformation(transformation);
		}
		String type = readDataPropertyType(uncertaintyInstance);
		if (type != null && !type.isEmpty()) {
			apparatusPropertyUncertainty.setUncertaintyType(type);
		}
	}

	private void quaryPropertyUncertaintyValue(String uncertaintyInstance) {
		String value = readDataPropertyValue(uncertaintyInstance);
		if (value != null && !value.isEmpty()) {
			apparatusPropertyUncertainty.setUncertaintyValue(value);
		}
	}
}
