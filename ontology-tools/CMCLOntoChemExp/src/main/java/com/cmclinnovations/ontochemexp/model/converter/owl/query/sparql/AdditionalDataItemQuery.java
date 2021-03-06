package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.additional_data_item.AdditionalDataItem;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class AdditionalDataItemQuery extends OwlConverter implements IAdditionalDataItemQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);
	
	public void query() throws OntoChemExpException{
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
				ontoChemExpVocabulary.getClassAdditionalDataItem());
		Collections.sort(queryResult);
		List<String> additionalDataItemInstances = queryResult;
		queryResult = new ArrayList<>();
		for (String additionalDataItemInstance : additionalDataItemInstances) {
			queryAllAttributes(additionalDataItemInstance);
			queryValue(additionalDataItemInstance);
			additionalDataItemList.add(additionalDataItem);
			additionalDataItem = new AdditionalDataItem();
		}
		experiment.setAdditionalDataItem(additionalDataItemList);
		additionalDataItemList = new ArrayList<AdditionalDataItem>();
	}
	
	private void queryAllAttributes(String additionalDataItemInstance) throws OntoChemExpException{
		String MIME = readDataPropertyMIME(additionalDataItemInstance);
		if (MIME != null && !MIME.isEmpty()) {
			additionalDataItem.setMIME(MIME);
		}
		
		String itemType = readDataPropertyItemType(additionalDataItemInstance);
		if (itemType != null && !itemType.isEmpty()) {
			additionalDataItem.setItemType(itemType);
		}
		
		String description = readDataPropertyDescription(additionalDataItemInstance);
		if (description != null && !description.isEmpty()) {
			additionalDataItem.setDescription(description);
		}
	}
	
	private void queryValue(String additionalDataItemInstance) throws OntoChemExpException{
		String value = readDataPropertyValue(additionalDataItemInstance);
		if (value != null && !value.isEmpty()) {
			additionalDataItem.setValue(value);
		}
	}
}
