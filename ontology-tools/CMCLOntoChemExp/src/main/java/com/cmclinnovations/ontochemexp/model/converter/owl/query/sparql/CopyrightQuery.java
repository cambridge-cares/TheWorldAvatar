package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.copyright.Copyright;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class CopyrightQuery extends OwlConverter implements ICopyrightQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);
	
	public void query() throws OntoChemExpException{
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
				ontoChemExpVocabulary.getClassCopyright());
		ArrayList<String> copyrightInstance = queryResult; 
		queryResult = new ArrayList<>();
		if(copyrightInstance.size()>0 && copyrightInstance.size()<2){
			// Queries all attributes of the experiment being processed 
			queryValue(copyrightInstance.get(0));
			experiment.setCopyright(copyright);
		}
		copyright = new Copyright();
	}
	
	private void queryValue(String copyrightInstance) throws OntoChemExpException{
		String value = readDataPropertyValue(copyrightInstance);
		if (value != null && !value.isEmpty()) {
			copyright.setValue(value);
		}
	}
}
