package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.data.structure.prime.bibliography.BibliographyLink;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

public class BibliographyLinkQuery extends OwlConverter implements IBibliographyLinkQuery {
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);
	
	public void query() throws OntoChemExpException{
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
				ontoChemExpVocabulary.getClassBibliographyLink());
		Collections.sort(queryResult);
		List<String> bibliographyLinkInstances = queryResult;
		queryResult = new ArrayList<>();
		for (String bibliographyLinkInstance : bibliographyLinkInstances) {
			queryAllAttributes(bibliographyLinkInstance);
			queryValue(bibliographyLinkInstance);
			bibliographyLinkList.add(bibliographyLink);
			bibliographyLink = new BibliographyLink();
		}
		experiment.setBibliographyLink(bibliographyLinkList);
		bibliographyLinkList = new ArrayList<BibliographyLink>();
	}
	
	private void queryAllAttributes(String bibliographyLinkInstance) throws OntoChemExpException{
		String preferredKey = readDataPropertyPreferredKey(bibliographyLinkInstance);
		if (preferredKey != null && !preferredKey.isEmpty()) {
			bibliographyLink.setPreferredKey(preferredKey);
		}
		
		String primeID = readDataPropertyPrimeID(bibliographyLinkInstance);
		if (primeID != null && !primeID.isEmpty()) {
			bibliographyLink.setPrimeID(primeID);
		}
	}
	
	private void queryValue(String bibliographyLinkInstance) throws OntoChemExpException{
		String value = readDataPropertyValue(bibliographyLinkInstance);
		if (value != null && !value.isEmpty()) {
			bibliographyLink.setValue(value);
		}
	}
}
