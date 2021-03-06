package com.cmclinnovations.ontochem.model.converter.owl.query.sparql;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;

import com.cmclinnovations.ontochem.model.converter.owl.OwlConverter;
import com.cmclinnovations.ontochem.model.exception.OntoException;
import com.cmclinnovations.ontochem.model.reference.IDoiParser;
import com.cmclinnovations.ontochem.model.utils.OwlConverterUtils;
import com.cmclinnovations.ontokin.model.data.structure.ctml.CtmlComment;
import com.cmclinnovations.ontokin.model.data.structure.ctml.Validate;

/**
 * Implements the methods that are used to perform SPARQL queries on top of
 * the OWL representation of a chemical mechanism ontology to extract
 * metadata of the mechanism to codify them in CTML.  
 * 
 * @author msff2
 *
 */
public class MetadataQuery extends OwlConverter implements IMetadataQuery{
	
	static Logger logger = org.slf4j.LoggerFactory.getLogger(MetadataQuery.class);
	
	public void query(){
		// Queries the name of the mechanism being processed 
		queryMechanism();
		// Initialises ctmlComments
		ctmlComments = new ArrayList<CtmlComment>();
		// Assigns ctmlComments
		ctmlMD.setComment(ctmlComments);
		// Queries the comment attached to a mechanism
		queryComment();
		// Queries the comment about a material
		queryMaterialComment();
		// Queries the reaction and species validation requirements 
		queryValidationData();
		// Queries the CTML version maintained at CMCL
		queryCtmlCmclVersion();
		// Queries the commit info originally generated by the CMCL converter
		queryCtmlCmclCommit();
	}

	/**
	 * Queries the instance id of the mechanism attached to the instance
	 *  of the Reaction Mechanism class.
	 */
	private void queryMechanism() {
		// The following query stores result in a global variable
		// called queryResult
		queryInstance(appConfigOntokin.getOntokinMechanism());
		if (queryResult != null && !queryResult.isEmpty()) {
			if (queryResult.size() >= 1) {
				mechanismName = queryResult.get(0);
				queryResult = new ArrayList<String>();
			}
		} else{
			logger.error(
					"The file system path may contain folder(s) with characters, "
					+ "e.g. '(' or ')' which are not allowed in SPARQL queries.");
			logger.error("Therefore, the conversion will lead to empty CTML file.");
		}
	}
	
	/**
	 * Queries and then sets the CTML comment attached to the instance of
	 * a mechanism in the mechanism OWL file being processed.
	 */
	private void queryComment() {
		String comment = readComment();
		if (comment == null) {
		} else if (comment.isEmpty()) {
			setCtmlComment(comment, null);
		} else {
			if (comment.contains("\\\\")) {
				setCtmlComment(comment.replace("\\\\", "\\"), null);
			} else if (comment.contains("\\\"")) {
				setCtmlComment(comment.replace("\\\"", "\""), null);
			} else {
				setCtmlComment(comment, null);
			}
		}
	}
	
	/**
	 * Forms and then performs a query to retrieve those materials that
	 * have comments.
	 */
	private void queryMaterialComment(){
		String q = formQueryWithAType("", "", "", appConfigOntokin.getClassMaterial());
		performMultilineAnswerQuery(q, 2);
		if(queryResult!=null && !queryResult.isEmpty()){
			ArrayList<String> materialInstances = queryResult;
			queryResult = new ArrayList<String>();
			for(String materialInstance:materialInstances){
				queryMaterialComment(materialInstance);
			}
		}
	}
	
	private void queryMaterialComment(String materialInstance){
		String q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, materialInstance, RDFS_LABEL);
		// Here q is reused for storing query result as well.
		// The reason of doing so is to avoid the creation additional local
		// variable, which in turn will reduce the workload of the garbage collector
		String material = performQuery(q, 1);
		if(q!=null && !q.isEmpty()){
			q = formQueryWithAStandardVocabulary(RDFS, RDFS_URL, materialInstance, RDFS_COMMENT);
			q = performQuery(q, 1);
			if(q!=null && !q.isEmpty()){
				setCtmlComment(q, material);
				
			}
		}
	}
	
	/**
	 * Adds a comment to the CTML root object. 
	 * 
	 * @param commentValue
	 * @param material
	 */
	private void setCtmlComment(String commentValue, String material){
		CtmlComment ctmlComment = new CtmlComment();
		commentValue = OwlConverterUtils.cTMLifyComment(commentValue);
		ctmlComment.setValue(commentValue);
		if(material!=null){
			ctmlComment.setMaterial(material);
		}
		ctmlComments.add(ctmlComment);
	}
	
	/**
	 * Queries and then sets the reaction validation requirement data.
	 */
	private void queryValidationData(){
		Validate validate = new Validate();
		validate.setReactions(readReactionValidations());
		validate.setSpecies(readSpeciesValidations());
		ctmlMD.setValidate(validate);
	}
	
	/**
	 * Queries and then sets the CTML CMCL version.
	 */
	private void queryCtmlCmclVersion(){
		ctmlMD.setCmclVersion(readCtmlCmclVersion());
	}
	
	/**
	 * Queries and then sets the CTML CMCL commit info.
	 */
	private void queryCtmlCmclCommit(){
		ctmlMD.setCommit(readCtmlCmclCommit());
	}
	
	/**
	 * Reads the comment about a chemical mechanism from the mechanism OWL file 
	 * being converted.
	 * 
	 * @return String
	 */
	private String readComment(){
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary(RDFS, 
				RDFS_URL, mechanismName,
				RDFS_COMMENT);
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and them performs a query to read information about the 
	 * necessity of reaction data validation.
	 * 
	 * @return "yes" or "no". It returns "yes", if it requires the validation of
	 *         reaction data, otherwise it returns "no".
	 */
	private String readReactionValidations() {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(mechanismName, appConfigOntokin.getCtmlValidateReactions());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}

	/**
	 * Forms and then performs a query to read information about the 
	 * necessity of species data validation.
	 *  
	 * @return "yes" or "no". It returns "yes", if it requires the validation
	 * of species data, otherwise it returns "no".
	 */
	private String readSpeciesValidations() {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(mechanismName, 
				appConfigOntokin.getCtmlValidateSpecies());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the CTML CMCL 
	 * version number. 
	 * 
	 * @return the CTML CMLCL version number
	 */
	private String readCtmlCmclVersion() {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithAStandardVocabulary("owl", 
				"http://www.w3.org/2002/07/owl#", appConfigOntokin.getCtml(),
				appConfigOntokin.getCtmlVersion());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}
	
	/**
	 * Forms and then performs a SPARQL query to read the CTML CMCL
	 * commit info.
	 * 
	 * @return the CTML CMCL commit info. 
	 */
	private String readCtmlCmclCommit() {
		// Calls the method that forms a SPARQL query
		String q = formQueryWithBaseURL(appConfigOntokin.getCtml(), 
				appConfigOntokin.getCtmlCommit());
		// Performs the query q and returns the result
		return performQuery(q, 1);
	}	
}
