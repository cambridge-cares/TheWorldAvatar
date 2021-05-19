package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.preferred_key.PreferredKey;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class PreferredKeyQuery extends OwlConverter implements IPreferredKeyQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);
	
	public void query() throws OntoChemExpException{
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
				ontoChemExpVocabulary.getOntoChemExpExpSpecs());
		ArrayList<String> preferredKeyInstance = queryResult;
		queryResult = new ArrayList<>();
		
		if (preferredKeyInstance.size()>0 && preferredKeyInstance.size()<2) {
			queryValue(preferredKeyInstance.get(0));
			experiment.setPreferredKey(preferredKey);
		}
		preferredKey = new PreferredKey();
	}
	
	private void queryValue(String preferredKeyInstance) throws OntoChemExpException{
		String value = readDataPropertyExpType(preferredKeyInstance);
		if (value != null && !value.isEmpty()) {
			preferredKey.setValue(value);
		}
	}
	
	private String readType(String preferredKeyInstance) {
		String q = formQueryWithBaseURL(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
				preferredKeyInstance, 
				ontoChemExpVocabulary.getDataPropertyhasType());
		return performQuery(q, 1);
	}
}
