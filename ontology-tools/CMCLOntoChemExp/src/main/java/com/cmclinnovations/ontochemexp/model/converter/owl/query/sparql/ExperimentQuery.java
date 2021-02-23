package com.cmclinnovations.ontochemexp.model.converter.owl.query.sparql;

import java.util.ArrayList;
import org.slf4j.Logger;

import com.cmclinnovations.ontochemexp.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochemexp.model.exception.OntoChemExpException;

/**
 * Implements the methods that are used to perform SPARQL queries on top of
 * the OWL representation of a PrIMe experiment ontology to extract
 * data and metadata about the apparatus used in the corresponding
 * chemical experiment.  
 * 
 * @author msff2
 *
 */
public class ExperimentQuery extends OwlConverter implements IExperimentQuery{
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ExperimentQuery.class);
	/**
	 * Queries the instance of the Experiment class and its data and metadata.
	 */
	
	public void query() throws OntoChemExpException{
		//query Instance first
		queryInstance(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
				ontoChemExpVocabulary.getClassExperiment());
//		System.out.println(formQueryWithAType(ontoChemExpKB.getOntoChemNamespace().concat(COLON), 
//				ontoChemExpKB.getOntoChemExpKbTBoxIri(), 
//				ontoChemExpVocabulary.getClassExperiment()));
		
		//assign queryResult to experimentInstance
		ArrayList<String> experimentInstance = queryResult; 
		//release queryResult memory
		queryResult = new ArrayList<>();
		//check the size of queryResult
		if(experimentInstance.size()>0 && experimentInstance.size()<2){
			// Queries all attributes of the experiment being processed 
			queryAllAttributes(experimentInstance.get(0));
			
		}else if(experimentInstance.size() > 1){
			logger.error("There should not be more than one instance of experiment in an OWL file.");
		} else {
			logger.error("No instance of experiment found in an OWL file.");
		}
	}

	/**
	 * Retrieves all attributes of the current experiment including the</br>
	 * PrIMe ID using the experiment instance.
	 * 
	 * @param experimentInstance
	 * @return
	 * @throws OntoChemExpException
	 */
	private void queryAllAttributes(String experimentInstance) throws OntoChemExpException{
		String primeID = readDataPropertyPrimeID(experimentInstance);
		if (primeID != null && !primeID.isEmpty()) {
			experiment.setPrimeID(primeID);
		}
		
		String xmlns = readDataPropertyXmlns(experimentInstance);
		if (xmlns != null && !xmlns.isEmpty()) {
			experiment.setXmlns(xmlns);
		}
		
		String xmlnsXsi = readDataPropertyXmlnsXsi(experimentInstance);
		if (xmlnsXsi != null && !xmlnsXsi.isEmpty()) {
			experiment.setXmlnsXsi(xmlnsXsi);
		}
		
		String xsiSchemaLocation = readDataPropertyXsiSchemaLocation(experimentInstance);
		if (xsiSchemaLocation != null && !xsiSchemaLocation.isEmpty()) {
			experiment.setXsiSchemaLocation(xsiSchemaLocation);
		}
	}
}
