package com.cmclinnovations.ontochem.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontokin.model.data.structure.ctml.element.Element;
import com.cmclinnovations.ontokin.model.data.structure.ctml.element.ElementData;

/**
 * Implements the methods that are used to perform SPARQL queries on top of
 * the OWL representation of a chemical mechanism ontology to extract
 * data and metadata about a chemical element to codify them in CTML.  
 * 
 * @author msff2
 *
 */
public class ElementQuery extends OwlConverter implements IElementQuery{
	static Logger logger = org.slf4j.LoggerFactory.getLogger(ElementQuery.class);
	/**
	 * Queries the instances of the Element class and their metadata.</br>
	 * It then sorts the instances and collects all the details including</br>
	 * name, phase they belong to, constituent elements, thermo data and</br>
	 * transport data (if available).
	 */
	public void query(){
		queryInstance(appConfigOntokin.getElementMetadata());
		Collections.sort(queryResult);
		List<String> elementMetadataInstances = queryResult;
		queryResult = new ArrayList<String>();
		if(elementMetadataInstances.size()<2){
			for (String elementMetadataInstance : elementMetadataInstances) {
				ElementData elementData = new ElementData();
				ctmlMD.setElementData(elementData);
				queryElementMetadata(elementMetadataInstance, elementData);
				elementData.setElement(queriedElementList);
				// Sets the properties of all the elements of a mechanism
				queryAllProperties(elementMetadataInstances.get(0));
			}
		}else{
			if(elementMetadataInstances==null){
				logger.error("ElementData should not be null.");
			}else{
				logger.error("The number of ElementData is more than 1 which is unexpected.");
			}
		}
	}
		
	/**
	 * Queries element metadata. 
	 * 
	 * @param elementInstances
	 * @param elementData
	 */
	private void queryElementMetadata(String elementMetadataInstance, ElementData elementData){
		// Forms query to retrieve meta data id
		String q = formQueryWithAStandardVocabulary(DUBLIN_CORE, DUBLIN_CORE_URL, elementMetadataInstance, DUBLIN_CORE_ID);
		// The query variable q is reused to store the result. The reason of
		// this choice is to reduce the amount of local variable declaration
		// that leads to increased workload on the garbage collection component.
		q = performQuery(q, 1);
		// Sets the id of element data to codify in CTML
		if(q!=null){
			elementData.setId(q);
		}
		readElementMetaData(elementMetadataInstance, elementData);
	}
	
	/**
	 * Queries all the properties of an element.
	 * 
	 * @param elementMetadataInstance
	 */
	private void queryAllProperties(String elementMetadataInstance){
		queryElementInstances(elementMetadataInstance);
		// As the above query will populate the global variable queryResult
		// with the instances of the class Element, we parse queryResult.
		if (queryResult != null) {
			ArrayList<String> elementInstances = queryResult;
			queryResult = new ArrayList<String>();
			for (String elementInstance : elementInstances) {
				queryElmentInstance(elementInstance);
			}
		}
	}
	
	/**
	 * Queries all the properties of an element.
	 * 
	 * @param instance the OWL instance id of the element
	 */
	private void queryElmentInstance(String instance){
		// Creates a phase instance for all the properties of a phase.
		Element element = new Element();
		queriedElementList.add(element);
		// Passes the already created phase instance to all the methods
		// in order to assign the values of the corresponding
		// properties.
		queryName(instance, element);
		queryAtomicWt(instance, element);
		queryAtomicWtUnits(instance, element);
		queryComment(instance, element);
		String sourceComment = performQuery(formQueryWithBaseURL(instance, appConfigOntokin.getSourceComment()),
				1);
		if (sourceComment != null) {
			objectVsSourceComment.put("element".concat(Integer.toString(++elementSequence)), sourceComment);
		}
	}
	
	/**
	 * Queries the element instances using the metadata instance id they 
	 * are attached to.
	 * 
	 * @param elementMetadaId the instance id of element metadata in the OWL ontology 
	 * being processed
	 */
	private void queryElementInstances(String elementMetadaId){
		String q = formSubjectRetrievalQuery(elementMetadaId, appConfigOntokin.getElementMetadataProperty());
		// This method call will store result in a global variable called
		// queryResult. 
		performMultilineAnswerQuery(q, 2);
	}
	
	/**
	 * Queries metadata from OWL and then sets them in an element meta data 
	 * instance to codify them in CTML.
	 * 
	 * @param metaDataInstance a metadata instance used to perform SPARQL query
	 * to retreive metadata
	 * @param elementData an element data instance used to set the 
	 * metadata values 
	 */
	private void readElementMetaData(String metaDataInstance, ElementData elementData){
		// Forms query to retrieve the case sensitivity info about element data 
		String q = formQueryWithBaseURL(metaDataInstance, appConfigOntokin.getElementDataCaseSensitivity());
		q = performQuery(q, 1);
		// Sets the case sensitivity info about elment data to codify in CTML
		if(q!=null){
			elementData.setCaseSensitive(q);
		}
		String sourceComment = performQuery(formQueryWithBaseURL(metaDataInstance, appConfigOntokin.getSourceComment()), 1);
		if (sourceComment != null) {
			objectVsSourceComment.put("elementData".concat(Integer.toString(++elementDataSequence)), sourceComment);
		}
	}
	
	/**
	 * Queries and then sets the name attached to the instance of
	 * an element in the mechanism OWL file being processed.
	 */
	private void queryName(String instance, Element element){
		String name = readName(instance);
		if(name!=null){
			element.setName(name);
		}
	}
	
	/**
	 * Queries and then sets the atomic mass attached to the instance of
	 * an element in the mechanism OWL file being processed.
	 */
	private void queryAtomicWt(String instance, Element element){
		String atomicWt = readAtomicWt(instance);
		if(atomicWt!=null){
			element.setAtomicWt(atomicWt);
		}
	}
	
	/**
	 * Queries and then sets the atomic weight units if they are not the 
	 * default units g/mol.
	 */
	private void queryAtomicWtUnits(String instance, Element element){
		String units = readAtomicWtUnits(instance);
		if(!units.equalsIgnoreCase("g/mol")){
			element.setUnits(units);
		}
	}
	
	/**
	 * Queries and then sets the comment attached to the instance of
	 * an element in the mechanism OWL file being processed.
	 */
	private void queryComment(String instance, Element element){
		String comment = readComment(instance);
		if(comment==null){	
		} else{
			if(comment.contains("\\\\")){
				element.setComment(comment.replace("\\\\", "\\"));
			} else if(comment.contains("\\\"")){
				element.setComment(comment.replace("\\\"", "\""));
			} else{
				element.setComment(comment);
			}
		}
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the name of an element. 
	 * 
	 * @return the dimension of a element
	 */
	private String readName(String elementInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, 
				RDFS_URL, elementInstance,
				RDFS_LABEL);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the name of an element. 
	 * 
	 * @return the dimension of a element
	 */
	private String readAtomicWt(String elementInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(elementInstance, appConfigOntokin.getElementAtomicWt());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	/**
	 * Forms and then performs a SPARQL query to read the name of an element. 
	 * 
	 * @return the name of an element
	 */
	private String readAtomicWtUnits(String elementInstance) {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(elementInstance, appConfigOntokin.getDataPropertyAtomicWtUnits());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
}
